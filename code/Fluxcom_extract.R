####EXTRACT FLUXCOM GPP PREDICTIONS FOR FLUX TOWER SITES####


library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(plyr)
library(tidyr)

setwd("C:/Users/farellam/Documents/DryFlux/data/")

#load the site coordinates
data <- read.csv("Z:/SRER/Martha/DryFlux/data/site_locs.csv", stringsAsFactors = FALSE)
data <- read.csv("C:/Users/farellam/Documents/DryFlux/data/global_locs.csv") #OzFluxsites
#convert the site coords into a spatial points df
sites <- data
coordinates(sites)= ~ long + lat

#list the Fluxcom .nc files
ff <- list.files('D:/martha/DryFlux/data/Fluxcom/', '*.nc', full.names = TRUE)

#convert each element in the list to a raster stack
rasts <- lapply(ff, stack)
#stack all of these raster stacks into a single raster stack
stackr <- stack(rasts)
#extract the GPP values from the raster stack for the flux tower locations
beginCluster()
gpp <- raster::extract(stackr, sites)
endCluster()

#combine the GPP values with site IDs
gpploc <- cbind.data.frame(sites$SITE_ID, gpp)
names(gpploc)[names(gpploc)=='sites$site'] <- 'site'
rownames(gpploc) <- gpploc$`sites$SITE_ID`
gpploc <- gpploc[,-1]
#transpose the dataframe
tgpp <- as.data.frame(t(gpploc))
tgpp$year <- substr(rownames(tgpp), 2,5)
tgpp$mo <- substr(rownames(tgpp), 7,8)
tgpp$day <- substr(rownames(tgpp), 10,11)
tgpp$date <- strptime(paste(tgpp$year, tgpp$mo, tgpp$day, sep="-"), format="%Y-%m-%d")
tgpp$date <- as.Date(tgpp$date)

##Define the months for each date range of observations
Jan <- 1
Feb <- 2
Mar <- 3
Apr <- 4
May <- 5
Jun <- 6
Jul <- 7
Aug <- 8
Sep <- 9
Oct <- 10
Nov <- 11
Dec <- 12
day_num <- 1
bound <- 15
jandates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), Jan, day_num))
febdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), Feb, day_num))
mardates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), Mar, day_num))
aprdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), Apr, day_num))
maydates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), May, day_num))
jundates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), Jun, day_num))
juldates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), Jul, day_num))
augdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), Aug, day_num))
sepdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), Sep, day_num))
octdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), Oct, day_num))
novdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), Nov, day_num))
decdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(tgpp$date, "%Y"))))), Dec, day_num))

#subset data set based on month delinations
Jan <- tgpp[tgpp$date %in% outer(jandates, -bound:bound, `+`),]
Jan$month <- "Jan"
Jan$year2 <- ifelse(substr(Jan$date, 9,10) >= 17, as.numeric(substr(Jan$date, 1,4)) +1, substr(Jan$date, 1,4))

Feb <- tgpp[tgpp$date %in% outer(febdates, -bound:bound, `+`),]
Feb$month <- "Feb"
Feb$year2 <- substr(Feb$date, 1,4)

Mar <- tgpp[tgpp$date %in% outer(mardates, -bound:bound, `+`),]
Mar$month <- "Mar"
Mar$year2 <- substr(Mar$date, 1,4)

Apr <- tgpp[tgpp$date %in% outer(aprdates, -bound:bound, `+`),]
Apr$month <- "Apr"
Apr$year2 <- substr(Apr$date, 1,4)

May <- tgpp[tgpp$date %in% outer(maydates, -bound:bound, `+`),]
May$month <- "May"
May$year2 <- substr(May$date, 1,4)

Jun <- tgpp[tgpp$date %in% outer(jundates, -bound:bound, `+`),]
Jun$month <- "Jun"
Jun$year2 <- substr(Jun$date, 1,4)

Jul <- tgpp[tgpp$date %in% outer(juldates, -bound:bound, `+`),]
Jul$month <- "Jul"
Jul$year2 <- substr(Jul$date, 1,4)

Aug <- tgpp[tgpp$date %in% outer(augdates, -bound:bound, `+`),]
Aug$month <- "Aug"
Aug$year2 <- substr(Aug$date, 1,4)

Sep <- tgpp[tgpp$date %in% outer(sepdates, -bound:bound, `+`),]
Sep$month <- "Sep"
Sep$year2 <- substr(Sep$date, 1,4)

Oct <- tgpp[tgpp$date %in% outer(octdates, -bound:bound, `+`),]
Oct$month <- "Oct"
Oct$year2 <- substr(Oct$date, 1,4)

Nov <- tgpp[tgpp$date %in% outer(novdates, -bound:bound, `+`),]
Nov$month <- "Nov"
Nov$year2 <- substr(Nov$date, 1,4)

Dec <- tgpp[tgpp$date %in% outer(decdates, -bound:bound, `+`),]
Dec$month <- "Dec"
Dec$year2 <- substr(Dec$date, 1,4)

tgpp <- rbind.data.frame(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

#define month year column
tgpp$monthyear <- paste(tgpp$month, substr(tgpp$year2, 1, 4), sep="-")

#Get Monthly GPP (mean)
cleaned_tgpp <- ddply(tgpp, .(monthyear), numcolwise(mean, na.rm=TRUE))
cleaned_tgpp$date <- as.Date(paste("01-", cleaned_tgpp$monthyear, sep=""), format= "%d-%b-%Y")
data.long <- pivot_longer(cleaned_tgpp, cols=2:24, names_to = "site", values_to = "vals" )
data.long <- arrange(data.long, date)
data.long$sitedate <- paste(data.long$site, data.long$monthyear, sep="-")
final <- data.long[,c(5,4)]
colnames(final) <- c("sitedate", "FluxcomGPP")

write.csv(final, "C:/Users/farellam/Documents/DryFlux/data/aus_FluxcomGPP.csv")
