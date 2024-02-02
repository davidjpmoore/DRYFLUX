##DETERMINE DAYLENGTH FOR EACH SITE LOCATION AND CRU DATE
##PRE PROCESS DATA FOR DOWNSTREAM ANALYSIS
##CREATE DAYLENGTH RASTER
##CHECK THE RASTER DATA FOR ACCURACY
##GET DAYLENGTHS FOR OXFLUX SITES


library(geosphere)
library(tidyr)

getwd()
setwd("C:/Users/farellam/Documents/DryFlux/")

#load data with site coordinates
data <- read.csv("./data/site_locs.csv")
#consense data to a singe observation/tower
#sites <- aggregate(cbind(lat,long) ~ site, data = data, FUN = mean, na.rm=TRUE )
sites <- data[,c(3,12,13)]
#make a list of unique latitudes
lats <- (unique(sites$lat))

#get the dates of CRU observations
dl_t <- read.csv("./data/dates.csv")
dates <- paste(dl_t$vals_year, dl_t$mo, dl_t$day, sep="-")

#create an empty matrix to store outputs
output <- matrix(nrow=length(dates), ncol=length(lats))
rownames(output) <- dates
colnames(output) <- as.character(lats)

#get daylength for various dates based on site location
for (j in lats) {
  for (i in dates) {
    dl <- daylength(j, i)
    output[i,as.character(j)] <- dl
    }
}

output_t <- as.data.frame(t(output))
output_t$lat <- rownames(output_t)
daylength <- merge(sites, output_t, by="lat")
rownames(daylength) <- daylength$site
daylength <- daylength[,-c(1:3)]
daylength_t <- as.data.frame(t(daylength))

daylength_t$date <- rownames(daylength_t)


#format data for RF where each row= single obervation per site/date
data.long <- pivot_longer(daylength_t, cols=1:24, names_to = "site", values_to= "vals")
data.long$year <- substring(data.long$date, 1,4)
data.long$mo <- substring(data.long$date, 6,7)
data.long$month[data.long$mo == "1-"] <- "Jan"
data.long$month[data.long$mo == "2-"] <- "Feb"
data.long$month[data.long$mo == "3-"] <- "Mar"
data.long$month[data.long$mo == "4-"] <- "Apr"
data.long$month[data.long$mo == "5-"] <- "May"
data.long$month[data.long$mo == "6-"] <- "Jun"
data.long$month[data.long$mo == "7-"] <- "Jul"
data.long$month[data.long$mo == "8-"] <- "Aug"
data.long$month[data.long$mo == "9-"] <- "Sep"
data.long$month[data.long$mo == "10"] <- "Oct"
data.long$month[data.long$mo == "11"] <- "Nov"
data.long$month[data.long$mo == "12"] <- "Dec"
#make month/year variable
data.long$mo_yr <- paste(data.long$month, data.long$year, sep = "-")

data.long$sitedate <- paste(data.long$site, data.long$mo_yr, sep="-")

data.long <- data.long[,-c(1,2,4,5,6,7)]

names(data.long)[names(data.long) == "vals"] <- "daylength"

write.csv(data.long, "./data/daylength.csv")


########################################################
###############create daylength raster##################
########################################################

#create a an empty raster to store output
#CRS = CRS of the CRU data products (see bottom of 'SPEIextract.R')
r1 <- raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90, 
             crs=crs(recent), resolution = c(0.5,0.5), vals=NULL)
#create an empty raster stack (length of 'dates') to store output
dlstack <- stack(r1, layers= 348)

#get the latitude coordinate
lat <- coordinates(r1, row=1)
longs <- unique(lat[,1])
lats <- unique(lat[,2])

#create an empty matrix you will use to populate the raster
rastvals <- matrix(nrow=360, ncol=720)

#do the loop
for(i in dates) { #select a single date
  for(j in lats){ #select a single latitude/row
    dl <- daylength(j, i) #determine the daylength for that latitude/date
    dlras <- rep(dl, 720) #repeat the daylength value the number of columns 
    rowras <- match(j, lats) #determine which row the latitude occurred at
    rastvals[rowras,] <- dlras #put the calculated daylength into the appropriate row of the data matrix
  }
  values(r1) <- rastvals #populate the raster with the daylength for each date
  stackno <- match(i,dates) #determine which raster stack layer the date occurred at
  dlstack[[stackno]] <- r1 #put the raster layer for a single date into the raster stack
}

writeRaster(dlstack, "z:/SRER/Martha/DryFlux/data/daylength.tif")



############################################################################
#check to see if the raster extracted values match up with the other values#
############################################################################
output2 <- raster::extract(dlstack, sites)
colnames(output2) <- dates
rownames(output2) <- sites$site

dl_t <- t(output2)
dl_t <- cbind(dates, dl_t)
dl_t <- as.data.frame(dl_t)
dl_t$mo <- substring(dl_t$dates, 6,7)
dl_t$month[dl_t$mo == "1-"] <- "Jan"
dl_t$month[dl_t$mo == "2-"] <- "Feb"
dl_t$month[dl_t$mo == "3-"] <- "Mar"
dl_t$month[dl_t$mo == "4-"] <- "Apr"
dl_t$month[dl_t$mo == "5-"] <- "May"
dl_t$month[dl_t$mo == "6-"] <- "Jun"
dl_t$month[dl_t$mo == "7-"] <- "Jul"
dl_t$month[dl_t$mo == "8-"] <- "Aug"
dl_t$month[dl_t$mo == "9-"] <- "Sep"
dl_t$month[dl_t$mo == "10"] <- "Oct"
dl_t$month[dl_t$mo == "11"] <- "Nov"
dl_t$month[dl_t$mo == "12"] <- "Dec"

dl_t$year <- substring(dl_t$dates, 1,4)

#make month/year variable
dl_t$mo_yr <- paste(dl_t$month, dl_t$year, sep = "-")

data.long <- pivot_longer(dl_t, cols=2:24, names_to = "site", values_to= "vals")


data.long$sitedate <- paste(data.long$site, data.long$mo_yr, sep="-")
data.long <- data.long[,c(8,7)]
colnames(data.long) <- c("sitedate", "rastvals")

#check to see if the raster extracted values match up with the otehrs values
others <- read.csv("./data/daylength.csv")

check <- merge(others,data.long, by="sitedate")
check$rastvals <- as.numeric(check$rastvals)
check$diff <- check$rastvals - check$daylength
str(check)



#####################################################################
#####################OzFlux site daylength values####################
#####################################################################
setwd("c:/Users/farellam/Documents/DryFlux/")

#load data with site coordinates
sites <- read.csv("./data/global_locs.csv")
#make a list of unique latitudes
lats <- (unique(sites$lat))

#get the dates of CRU observations
dl_t <- read.csv("./data/dates.csv")
dates <- paste(dl_t$vals_year, dl_t$mo, dl_t$day, sep="-")

#create an empty matrix to store outputs
output <- matrix(nrow=length(dates), ncol=length(lats))
rownames(output) <- dates
colnames(output) <- as.character(lats)


for (j in lats) {
  for (i in dates) {
    dl <- daylength(j, i)
    output[i,as.character(j)] <- dl
  }
}

output_t <- as.data.frame(t(output))
output_t$lat <- rownames(output_t)
daylength <- merge(sites, output_t, by="lat")
rownames(daylength) <- daylength$SITE_ID
daylength <- daylength[,-c(1,2,4:9)]
daylength_t <- as.data.frame(t(daylength))
colnames(daylength_t) <- daylength_t[1,]
daylength_t <- daylength_t[-1,]

daylength_t$date <- rownames(daylength_t)


#format data for RF where each row= single obervation per site/date
data.long <- pivot_longer(daylength_t, cols=1:23, names_to = "SITE_ID", values_to= "vals")
data.long$year <- substring(data.long$date, 1,4)
data.long$mo <- substring(data.long$date, 6,7)
data.long$month[data.long$mo == "1-"] <- "Jan"
data.long$month[data.long$mo == "2-"] <- "Feb"
data.long$month[data.long$mo == "3-"] <- "Mar"
data.long$month[data.long$mo == "4-"] <- "Apr"
data.long$month[data.long$mo == "5-"] <- "May"
data.long$month[data.long$mo == "6-"] <- "Jun"
data.long$month[data.long$mo == "7-"] <- "Jul"
data.long$month[data.long$mo == "8-"] <- "Aug"
data.long$month[data.long$mo == "9-"] <- "Sep"
data.long$month[data.long$mo == "10"] <- "Oct"
data.long$month[data.long$mo == "11"] <- "Nov"
data.long$month[data.long$mo == "12"] <- "Dec"
#make month/year variable
data.long$mo_yr <- paste(data.long$month, data.long$year, sep = "-")

data.long$sitedate <- paste(data.long$SITE_ID, data.long$mo_yr, sep="-")

data.long <- data.long[,c(8,3)]

names(data.long)[names(data.long) == "vals"] <- "daylength"

write.csv(data.long, "C:/Users/farellam/Documents/DryFlux/data/aus_daylength.csv")
