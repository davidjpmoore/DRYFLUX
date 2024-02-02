###CODE USED TO EXTRACT CRU DATA VALUES FOR FLUX TOWER LOCATIONS AND PRE-PROCESS FOR DOWNSTREAM ANALYSIS###
#raw CRU data downloaded from https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.04/cruts.2004151855.v4.04/ 
#meterological variables downloaded included: precipitation (pre), tmin (tmn), tmax (tmx), vapor pressure (vap), potential evapotranspiration (pet), and Tavg (tmp) for time periods: 1991-2000; 2001-2010; 2011-2019
#citation: Harris et al. (2020) doi:10.1038/s41597-020-0453-3
  #raw CRU data stored in DryFlux/data/CRU

library(rgdal)
library(raster)
library(GSIF)
library(tidyr)


#####################################
####FOR THE SPEI CRU DATA PRODUCTS###
#####################################

#Set the working directory 
setwd("c:/Users/farellam/Documents/DryFlux/data/")
#load data with site coordinates
data <- read.csv("C:/Users/farellam/Documents/DryFlux/data/site_locs.csv")
#consense data to a singe observation/tower
#sites <- aggregate(cbind(lat,long) ~ site, data = data, FUN = mean, na.rm=TRUE )
sites <- data
#convert site locations into a spatial points df
coordinates(sites)= ~ long + lat

#create a list of all of the SPEI files created in 'computeSPEI.R'
ff <- list.files('./outputNcdf/', 'speiALL.*\\.nc' ,full.names=TRUE)

#convert each element in the list to a raster stack
rasts <- lapply(ff, stack)
#flip the raster images so they are in the correct orientation
f_rasts <- lapply(rasts, function(i) flip(i, 'y'))

#stack all of these raster stacks into a single stack
stackr <- stack(f_rasts)

#extract SPEI values for flux tower locations for the raster stack 
spei <- raster::extract(stackr, sites)

#combine spei values with siteIDs
speiloc <- cbind.data.frame(sites$site, spei)

#####Format SPEI output for merge with other data where each row= 1 site/date
speiloc <- t(speiloc)
colnames(speiloc) <- speiloc[1,]
speiloc <- speiloc[-1,]

#get the SPEI date/time duration info
speiID <- as.data.frame(rownames(speiloc))
colnames(speiID) <- "speiID"
speiID$spei_year <- substring(speiID$speiID, 2,5)
speiID$mo <- substring(speiID$speiID, 7,8)
speiID$month[speiID$mo == "01"] <- "Jan"
speiID$month[speiID$mo == "02"] <- "Feb"
speiID$month[speiID$mo == "03"] <- "Mar"
speiID$month[speiID$mo == "04"] <- "Apr"
speiID$month[speiID$mo == "05"] <- "May"
speiID$month[speiID$mo == "06"] <- "Jun"
speiID$month[speiID$mo == "07"] <- "Jul"
speiID$month[speiID$mo == "08"] <- "Aug"
speiID$month[speiID$mo == "09"] <- "Sep"
speiID$month[speiID$mo == "10"] <- "Oct"
speiID$month[speiID$mo == "11"] <- "Nov"
speiID$month[speiID$mo == "12"] <- "Dec"
#make month/year variable
speiID$mo_yr <- paste(speiID$month, speiID$spei_year, sep = "-")

speiID$day <- substring(speiID$speiID, 10,11)
#SPEI time duration info
speiID$spei_date <- paste(speiID$mo, speiID$day, speiID$spei_year, sep="/")
speiID$time <- substr(speiID$speiID, 13,13)
speiID$time1[speiID$time == 1] <- 1
speiID$time1[speiID$time == 2] <- 3
speiID$time1[speiID$time == 3] <- 6
speiID$time1[speiID$time == 4] <- 9
speiID$time1[speiID$time == 5] <- 12
speiID$spei_time <- paste0("spei", speiID$time1)

data <- cbind(speiID[,c(2,5,7,10)], speiloc)

#change data from wide to long format and back again
data.long <- pivot_longer(data, cols=5:28, names_to = "site", values_to= "spei")
data.long$sitedate <- paste(data.long$site, data.long$mo_yr, sep="-")
data.long <- data.long[,c(4,6,7)]

data.wide <- pivot_wider(data.long, names_from= spei_time, values_from=spei)


write.csv(data.wide, "./data/swusa_SPEI.csv")

#####################################
###FOR THE OTHER CRU DATA PRODUCTS###
#####################################
#create a list of all of the individual CRU files **(will need to change for each variable)**
ff <- list.files('c:/Users/farellam/Documents/DryFlux/data/CRU/','*tmp.dat.nc',full.names=TRUE) #change the file name for each of the CRU data products

#convert each element in the list to a raster stack
rasts <- lapply(ff, stack)
#stack all of these raster stacks into a single stack
stackr <- stack(rasts)
#extract climate variables values for flux tower locations for the raster stack 
vals <- raster::extract(stackr, sites)

#combine meterological values with siteIDs
valsloc <- cbind.data.frame(sites$site, vals)

#####Format SPEI output for merge with other data where each row= 1 site/date
valsloc <- t(valsloc)
colnames(valsloc) <- valsloc[1,]
valsloc <- valsloc[-1,]

#get the SPEI date/time duration info
valsID <- as.data.frame(rownames(valsloc))
colnames(valsID) <- "valsID"
valsID$vals_year <- substring(valsID$valsID, 2,5)
valsID$mo <- substring(valsID$valsID, 7,8)
valsID$month[valsID$mo == "01"] <- "Jan"
valsID$month[valsID$mo == "02"] <- "Feb"
valsID$month[valsID$mo == "03"] <- "Mar"
valsID$month[valsID$mo == "04"] <- "Apr"
valsID$month[valsID$mo == "05"] <- "May"
valsID$month[valsID$mo == "06"] <- "Jun"
valsID$month[valsID$mo == "07"] <- "Jul"
valsID$month[valsID$mo == "08"] <- "Aug"
valsID$month[valsID$mo == "09"] <- "Sep"
valsID$month[valsID$mo == "10"] <- "Oct"
valsID$month[valsID$mo == "11"] <- "Nov"
valsID$month[valsID$mo == "12"] <- "Dec"
#make month/year variable
valsID$mo_yr <- paste(valsID$month, valsID$vals_year, sep = "-")

valsID$day <- substring(valsID$valsID, 10,11)

data <- cbind(valsID[,c(2,5)], valsloc)

#change data from wide to long format and back again
data.long <- pivot_longer(data, cols=3:26, names_to = "site", values_to= "vals")
data.long$sitedate <- paste(data.long$site, data.long$mo_yr, sep="-")
data.long <- data.long[,c(5,2,4)]

names(data.long)[names(data.long) == "vals"] <- "Tavg"

write.csv(data.long, "./swusa_Tavg.csv")

###########################################################
#########Offsetting precip and temp vars by a month########
###########################################################
setwd("c:/Users/farellam/Documents/DryFlux/data/re-do")

data <- read.csv("aus_precip.csv")
data$month <- substr(data$mo_yr, 1,3)
data$month[data$month == "Jan"] <- "FebL"
data$month[data$month == "Feb"] <- "MarL"
data$month[data$month == "Mar"] <- "AprL"
data$month[data$month == "Apr"] <- "MayL"
data$month[data$month == "May"] <- "JunL"
data$month[data$month == "Jun"] <- "JulL"
data$month[data$month == "Jul"] <- "AugL"
data$month[data$month == "Aug"] <- "SepL"
data$month[data$month == "Sep"] <- "OctL"
data$month[data$month == "Oct"] <- "NovL"
data$month[data$month == "Nov"] <- "DecL"
data$month[data$month == "Dec"] <- "JanL"

#if last months value is 'Dec' replace the year by subtracting 1
data$year=ifelse(data$month =="JanL",as.numeric(substr(data$mo_yr, 5,9))+1, as.numeric(substr(data$mo_yr, 5,9)))

data$site <- substr(data$sitedate, 1, 6)

lastmo <- data[,c(4:7)]
names(lastmo)[1] <- "lastmo_precip"
lastmo$sitedate <- paste(lastmo$site, substr(lastmo$month, 1,3), lastmo$year, sep="-")

lastmo <- lastmo[,c(1,5)]

write.csv(lastmo, "aus_lastmo_precip.csv")

###########################################################
#################values for OzFlux Sites###################
###########################################################

#####SPEI#####
setwd("C:/Users/farellam/Documents/DryFlux/")
#load data with site coordinates
sites <- read.csv("./data/aus_drylands.csv")
#convert site locations into a spatial points df
coordinates(sites)= ~ long + lat

#create a list of all of the SPEI files created in 'computeSPEI.R'
ff <- list.files('C:/Users/farellam/Documents/DryFlux/data/outputNcdf/', 'speiALL.*\\.nc' ,full.names=TRUE)
#convert each element in the list to a raster stack
rasts <- lapply(ff, stack)
#flip the raster images so they are in the correct orientation
f_rasts <- lapply(rasts, function(i) flip(i, 'y'))

#stack all of these raster stacks into a single stack
stackr <- stack(f_rasts)

#extract SPEI values for flux tower locations for the raster stack 
spei <- raster::extract(stackr, sites)
#combine spei values with siteIDs
speiloc <- cbind.data.frame(sites$SITE_ID, spei)

#Format SPEI output for merge with other data where each row= 1 site/date
speiloc <- t(speiloc)
colnames(speiloc) <- speiloc[1,]
speiloc <- speiloc[-1,]

#get the SPEI date/time duration info
speiID <- as.data.frame(rownames(speiloc))
colnames(speiID) <- "speiID"
speiID$spei_year <- substring(speiID$speiID, 2,5)
speiID$mo <- substring(speiID$speiID, 7,8)
speiID$month[speiID$mo == "01"] <- "Jan"
speiID$month[speiID$mo == "02"] <- "Feb"
speiID$month[speiID$mo == "03"] <- "Mar"
speiID$month[speiID$mo == "04"] <- "Apr"
speiID$month[speiID$mo == "05"] <- "May"
speiID$month[speiID$mo == "06"] <- "Jun"
speiID$month[speiID$mo == "07"] <- "Jul"
speiID$month[speiID$mo == "08"] <- "Aug"
speiID$month[speiID$mo == "09"] <- "Sep"
speiID$month[speiID$mo == "10"] <- "Oct"
speiID$month[speiID$mo == "11"] <- "Nov"
speiID$month[speiID$mo == "12"] <- "Dec"
#make month/year variable
speiID$mo_yr <- paste(speiID$month, speiID$spei_year, sep = "-")

speiID$day <- substring(speiID$speiID, 10,11)
#SPEI time duration info
speiID$spei_date <- paste(speiID$mo, speiID$day, speiID$spei_year, sep="/")
speiID$time <- substr(speiID$speiID, 13,13)
speiID$time1[speiID$time == 1] <- 1
speiID$time1[speiID$time == 2] <- 3
speiID$time1[speiID$time == 3] <- 6
speiID$time1[speiID$time == 4] <- 9
speiID$time1[speiID$time == 5] <- 12
speiID$spei_time <- paste0("spei", speiID$time1)

data <- cbind(speiID[,c(2,5,7,10)], speiloc)

#change data from wide to long format and back again
data.long <- pivot_longer(data, cols=5:27, names_to = "site", values_to= "spei")
data.long$sitedate <- paste(data.long$site, data.long$mo_yr, sep="-")
data.long <- data.long[,c(4,6,7)]

data.wide <- pivot_wider(data.long, names_from= spei_time, values_from=spei)
write.csv(data.wide, "./data/aus_SPEI.csv")



#####other CRU data products#####
#create a list of all of the individual CRU files **(will need to change for each variable)**
ff <- list.files('c:/Users/farellam/Documents/DryFlux/data/CRU/','*pre.dat.nc',full.names=TRUE) #change the file name for each of the CRU data products

#convert each element in the list to a raster stack
rasts <- lapply(ff, stack)
#stack all of these raster stacks into a single stack
stackr <- stack(rasts)
#extract climate variables values for flux tower locations for the raster stack 
vals <- raster::extract(stackr, sites)

#combine meterological values with siteIDs
valsloc <- cbind.data.frame(sites$SITE_ID, vals)

#####Format climate variable output for merge with other data where each row= 1 site/date
valsloc <- t(valsloc)
colnames(valsloc) <- valsloc[1,]
valsloc <- valsloc[-1,]

#get the climate variable date/time duration info
valsID <- as.data.frame(rownames(valsloc))
colnames(valsID) <- "valsID"
valsID$vals_year <- substring(valsID$valsID, 2,5)
valsID$mo <- substring(valsID$valsID, 7,8)
valsID$month[valsID$mo == "01"] <- "Jan"
valsID$month[valsID$mo == "02"] <- "Feb"
valsID$month[valsID$mo == "03"] <- "Mar"
valsID$month[valsID$mo == "04"] <- "Apr"
valsID$month[valsID$mo == "05"] <- "May"
valsID$month[valsID$mo == "06"] <- "Jun"
valsID$month[valsID$mo == "07"] <- "Jul"
valsID$month[valsID$mo == "08"] <- "Aug"
valsID$month[valsID$mo == "09"] <- "Sep"
valsID$month[valsID$mo == "10"] <- "Oct"
valsID$month[valsID$mo == "11"] <- "Nov"
valsID$month[valsID$mo == "12"] <- "Dec"
#make month/year variable
valsID$mo_yr <- paste(valsID$month, valsID$vals_year, sep = "-")

valsID$day <- substring(valsID$valsID, 10,11)

data <- cbind(valsID[,c(2,5)], valsloc)

#change data from wide to long format and back again
data.long <- pivot_longer(data, cols=3:25, names_to = "site", values_to= "vals")
data.long$sitedate <- paste(data.long$site, data.long$mo_yr, sep="-")
data.long <- data.long[,c(5,2,4)]

names(data.long)[names(data.long) == "vals"] <- "precip"

write.csv(data.long, "./data/aus_precip.csv")


####OzFlux lastmo Tavg and precip
setwd("C:/Users/farellam/Documents/DryFlux/data")

data <- read.csv("aus_precip.csv")
data$month <- substr(data$mo_yr, 1,3)
data$month[data$month == "Jan"] <- "FebL"
data$month[data$month == "Feb"] <- "MarL"
data$month[data$month == "Mar"] <- "AprL"
data$month[data$month == "Apr"] <- "MayL"
data$month[data$month == "May"] <- "JunL"
data$month[data$month == "Jun"] <- "JulL"
data$month[data$month == "Jul"] <- "AugL"
data$month[data$month == "Aug"] <- "SepL"
data$month[data$month == "Sep"] <- "OctL"
data$month[data$month == "Oct"] <- "NovL"
data$month[data$month == "Nov"] <- "DecL"
data$month[data$month == "Dec"] <- "JanL"

#only include data from 2000 - 2019
data00 <- subset(data, substr(mo_yr, 7, 8) < 20) 

#add year column
data00$yr <- as.numeric(paste("20", substr(data00$mo_yr, 7,8), sep="")) #for the temp data frame

#if last months value is 'Dec' replace the year by adding 1
data00$year=ifelse(data00$month =="JanL",as.numeric(data00$yr)+1, as.numeric(data00$yr))

#subset to sitedate vars and lastmo_temp/precip
lastmo <- data00[,c(2,4,5,7)]
names(lastmo)[2] <- "lastmo_precip"
lastmo$sitedate <- paste(substr(lastmo$sitedate, 1,6), substr(lastmo$month, 1,3), lastmo$year, sep="-")

lastmo <- lastmo[,c(1,2)]

write.csv(lastmo, "aus_lastmo_precip.csv")




###################################################
######CREATE RASTERS OF THE CRU DATA PRODUCTS######
###################################################
#####SPEI RASTERS#####

#create a list of all of the SPEI files created in 'computeSPEI.R'
ff <- list.files('Z:/SRER/Martha/DryFlux/data/outputNcdf/', 'speiALL.*\\.nc' ,full.names=TRUE)

#convert each element in the list to a raster stack
rasts <- lapply(ff, stack)
#flip the raster images so they are in the correct orientation -- #take 20-30mins
f_rasts <- lapply(rasts, function(i) flip(i, 'y'))

#stack all of these raster stacks into a single stack
stackr <- stack(f_rasts)
names(stackr)

#subset spei time frames and select only 2000-2015
spei1 <- subset(stackr, c(109:312))
spei3 <- subset(stackr, c(457:660))
spei6 <- subset(stackr, c(805:1008))
spei9 <- subset(stackr, c(1153:1356))
spei12 <- subset(stackr, c(1501:1704))



#rename raster layers
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
year <- rep("2000", 12)
yr0 <- paste(year, months, sep="")
year <- rep("2001", 12)
yr1 <- paste(year, months, sep="")
year <- rep("2002", 12)
yr2 <- paste(year, months, sep="")
year <- rep("2003", 12)
yr3 <- paste(year, months, sep="")
year <- rep("2004", 12)
yr4 <- paste(year, months, sep="")
year <- rep("2005", 12)
yr5 <- paste(year, months, sep="")
year <- rep("2006", 12)
yr6 <- paste(year, months, sep="")
year <- rep("2007", 12)
yr7 <- paste(year, months, sep="")
year <- rep("2008", 12)
yr8 <- paste(year, months, sep="")
year <- rep("2009", 12)
yr9 <- paste(year, months, sep="")
year <- rep("2010", 12)
yr10 <- paste(year, months, sep="")
year <- rep("2011", 12)
yr11 <- paste(year, months, sep="")
year <- rep("2012", 12)
yr12 <- paste(year, months, sep="")
year <- rep("2013", 12)
yr13 <- paste(year, months, sep="")
year <- rep("2014", 12)
yr14 <- paste(year, months, sep="")
year <- rep("2015", 12)
yr15 <- paste(year, months, sep="")
year <- rep("2016", 12)
yr16 <- paste(year, months, sep="")
moyr <- c(yr0, yr1, yr2, yr3, yr4, yr5, yr6, yr7, yr8, yr9, yr10, yr11, yr12, yr13, yr14, yr15, yr16)

names(spei1) <- moyr
names(spei3) <- moyr
names(spei6) <- moyr
names(spei9) <- moyr
names(spei12) <- moyr

writeRaster(spei1, "Z:/SRER/Martha/DryFlux/data/CRU/tifs/spei1NEW.grd", format="raster", overwrite=TRUE)
writeRaster(spei3, "Z:/SRER/Martha/DryFlux/data/CRU/tifs/spei3NEW.grd", format="raster", overwrite=TRUE)
writeRaster(spei6, "Z:/SRER/Martha/DryFlux/data/CRU/tifs/spei6NEW.grd", format="raster", overwrite=TRUE)
writeRaster(spei9, "Z:/SRER/Martha/DryFlux/data/CRU/tifs/spei9NEW.grd", format="raster", overwrite=TRUE)
writeRaster(spei12, "Z:/SRER/Martha/DryFlux/data/CRU/tifs/spei12NEW.grd", format="raster", overwrite=TRUE)

#####RASTERS FOR THE OTHER CRU DATA PRODUCTS####
#create a list of all of the individual CRU files **(will need to change for each variable)**
var = "pet"

ff <- list.files('Z:/SRER/Martha/DryFlux/data/CRU/', paste('*', var, '.dat.nc', sep=""), full.names=TRUE) #change the file name for each of the CRU data products

#convert each element in the list to a raster stack
rasts <- lapply(ff, stack)
#stack all of these raster stacks into a single stack
stackr <- stack(rasts)

#subset to just 2000 - 2015
recent <- subset(stackr, 109:312)
names(recent)
names(recent) <- moyr
outname <- paste("Z:/SRER/Martha/DryFlux/data/CRU/tifs/", var, ".grd", sep="")
print(outname)
writeRaster(recent, outname, format="raster", overwrite=TRUE)

