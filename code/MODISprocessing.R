#CODE TO EXTRACT MODIS 16-DAY (MOD13C1) EVI AND NDVI VALUES FOR FLUX TOWER LOCATIONS AND CREATE .TIF NDVI AND EVI DATA PRODUCTS#
#.hdf data products downloaded from: https://e4ftl01.cr.usgs.gov/MOLT/MOD13C1.006/ 
  #16th day global 0.5deg CMG MOD13C1
  #Date downloaded is the date closest to the 15th/16th of each month 
  #citation: Didan, K. (2015). MOD13C1 MODIS/Terra Vegetation Indices 16-Day L3 Global 0.05Deg CMG V006 [Data set]. NASA EOSDIS Land Processes DAAC. Accessed 2020-10-13 from https://doi.org/10.5067/MODIS/MOD13C1.006

#load required libraries 
library(MODIS)
library(sp)
library(rgdal)
library(sf)
library(tidyr)

getwd()
setwd("c:/Users/farellam/Documents/DryFlux/data/")

#load data with site coordinates
train <- read.csv("site_locs.csv")
train <- train[,c(3,13,14)]
train$type <- rep("train", nrow=nrow(train))
test <- read.csv("global_locs.csv")
test <- test[,c(2,4,5)]
test$type <- rep("test", nrow=nrow(test))
colnames(test) <- c("site", "lat", "long", "type")
sites <- rbind.data.frame(train,test)
#condense data to a singe observation/tower
#sites <- aggregate(cbind(lat,long) ~ site, data = data, FUN = mean, na.rm=TRUE )

#convert site locations into a spatial points df
coordinates(sites)= ~ long + lat

latlon = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
crs(sites) <- latlon

#transform the flux tower locations to the crs of the MODIS data product
modiscrs <- CRS('+proj=longlat +ellps=clrk66 +no_defs')
sites_t <- spTransform(sites, modiscrs)

#function to extract VI values for each flux tower location
extract_vi <- function(x){
  #open the hdf file
  sds <- getSds(x)
  ndvi <- raster(readGDAL(sds$SDS4gdal[1], as.is=TRUE))
  evi <- raster(readGDAL(sds$SDS4gdal[2], as.is=TRUE))
  # extract data
  ndvi_vals <- raster::extract(ndvi,sites_t)
  evi_vals <- raster::extract(evi, sites_t)
  VIs <- cbind.data.frame(sites$site, ndvi_vals/10000, evi_vals/10000)
  date <- substring(x, 9, 15)
  colnames(VIs) <- c("site", paste0(date, "ndvi"), paste0(date,"evi"))
  return(VIs)
  }


setwd("D:/martha/DryFlux/data/MOD13C1/2016")

#make a list of the MODIS files in each folder
h5files = list.files(pattern = "*.hdf")

#lapply "extract_vi" fxn over all of the site reflectance files : takes about 1 minute for 2000 with 11 h5 files
indices_tmp <- lapply(h5files, extract_vi)

#merge columns together
indices <- Reduce(function(x,y) merge(x,y, by="site"), indices_tmp)


indices_t <- t(indices)
colnames(indices_t) <- indices_t[1,]
indices_t <- indices_t[-1,]
date <- as.data.frame(substring(rownames(indices_t), 1,7))
colnames(date) <- "date"
date$vi <- (substring(rownames(indices_t), 8,11))
date$mo <- substring(date$date, 6,7)
date$month[date$mo == "01"] <- "Jan"
date$month[date$mo == "02"] <- "Feb"
date$month[date$mo == "03"] <- "Mar"
date$month[date$mo == "04"] <- "Apr"
date$month[date$mo == "05"] <- "May"
date$month[date$mo == "06"] <- "Jun"
date$month[date$mo == "07"] <- "Jul"
date$month[date$mo == "08"] <- "Aug"
date$month[date$mo == "09"] <- "Sep"
date$month[date$mo == "10"] <- "Oct"
date$month[date$mo == "11"] <- "Nov"
date$month[date$mo == "12"] <- "Dec"
date$year <- substring(date$date, 1,4)
date$mo_yr <- paste(date$month, date$year, sep = "-")

vidata <- cbind.data.frame(date[,c(2,6)], indices_t)


#change data from wide to long format and back again
data.long <- pivot_longer(vidata, cols=3:49, names_to = "site", values_to= "vals")
data.long$sitedate <- paste(data.long$site, data.long$mo_yr, sep="-")
data.long <- data.long[,c(5,2,1,4)]
#data.long$vi[data.long$vi =="evi."] <- "evi"

data.wide <- pivot_wider(data.long, names_from= vi, values_from=vals)

write.csv(data.wide, "C:/Users/farellam/Documents/DryFlux/data/re-do/VIs/VIs2016.csv")


#######################################################
####################OzFlux Sites#######################
#######################################################
#load data with site coordinates
sites <- read.csv("C:/Users/farellam/Documents/DryFlux/data/global_locs.csv")
#convert site locations into a spatial points df
coordinates(sites)= ~ long + lat
latlon = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
crs(sites) <- latlon
#transform the flux tower locations to the crs of the MODIS data product
modiscrs <- CRS('+proj=longlat +ellps=clrk66 +no_defs')
sites_t <- spTransform(sites, modiscrs)

#function to extract VI values for each flux tower location
extract_vi <- function(x){
  #open the hdf file
  sds <- getSds(x)
  ndvi <- raster(readGDAL(sds$SDS4gdal[1], as.is=TRUE))
  evi <- raster(readGDAL(sds$SDS4gdal[2], as.is=TRUE))
  # extract data
  ndvi_vals <- raster::extract(ndvi,sites_t)
  evi_vals <- raster::extract(evi, sites_t)
  VIs <- cbind.data.frame(sites$SITE_ID, ndvi_vals/10000, evi_vals/10000)
  date <- substring(x, 9, 15)
  colnames(VIs) <- c("site", paste0(date, "ndvi"), paste0(date,"evi"))
  return(VIs)
}

#have fluxtower GPP values for OzFlux sites from 2000-2014
setwd("D:/martha/DryFlux/data/MOD13C1/2014")
#make a list of the MODIS files in each folder
h5files = list.files(pattern = "*.hdf")
#lapply "extract_vi" fxn over all of the site reflectance files : takes about 1 minute for 2000 with 11 h5 files
indices_tmp <- lapply(h5files, extract_vi)
#merge columns together
indices <- Reduce(function(x,y) merge(x,y, by="site"), indices_tmp)

indices_t <- t(indices)
colnames(indices_t) <- indices_t[1,]
indices_t <- indices_t[-1,]
date <- as.data.frame(substring(rownames(indices_t), 1,7))
colnames(date) <- "date"
date$vi <- (substring(rownames(indices_t), 8,11))
date$mo <- substring(date$date, 6,7)
date$month[date$mo == "01"] <- "Jan"
date$month[date$mo == "02"] <- "Feb"
date$month[date$mo == "03"] <- "Mar"
date$month[date$mo == "04"] <- "Apr"
date$month[date$mo == "05"] <- "May"
date$month[date$mo == "06"] <- "Jun"
date$month[date$mo == "07"] <- "Jul"
date$month[date$mo == "08"] <- "Aug"
date$month[date$mo == "09"] <- "Sep"
date$month[date$mo == "10"] <- "Oct"
date$month[date$mo == "11"] <- "Nov"
date$month[date$mo == "12"] <- "Dec"
date$year <- substring(date$date, 1,4)
date$mo_yr <- paste(date$month, date$year, sep = "-")

vidata <- cbind.data.frame(date[,c(2,6)], indices_t)

#change data from wide to long format and back again
data.long <- pivot_longer(vidata, cols=3:25, names_to = "site", values_to= "vals")
data.long$sitedate <- paste(data.long$site, data.long$mo_yr, sep="-")
data.long <- data.long[,c(5,2,1,4)]
#data.long$vi[data.long$vi =="evi."] <- "evi"

data.wide <- pivot_wider(data.long, names_from= vi, values_from=vals)

write.csv(data.wide, "C:/Users/farellam/Documents/DryFlux/data/aus_VIs/VIs2014.csv")


###############################################################################
########Creating .tif for each EVI and NDVI MODIS file x date#################
###############################################################################
#will need to change last part for each year (folder)
setwd("Z:/SRER/Martha/DryFlux/data/MOD13C1/2016")

files <- list.files(pattern=".hdf")
#define the directory you want to write the rasters to
writedir <- "Z:/SRER/Martha/DryFlux/data/MOD13C1/tifs/"

write.rast <- function(x){
  #open the hdf file
  sds <- getSds(x)
  #create raster of the NDVI and EVI bands
  ndvi <- raster(readGDAL(sds$SDS4gdal[1], as.is=TRUE))
  evi <- raster(readGDAL(sds$SDS4gdal[2], as.is=TRUE))
  #get output file name for ndvi and evi rasters
  ndvi_out <- paste(writedir, "ndvi_", substr(x,9,18), ".tif", sep="")
  evi_out <- paste(writedir, "evi_", substr(x,9,18), ".tif", sep="")
  writeRaster(ndvi, ndvi_out)
  writeRaster(evi, evi_out)
  }

#apply the 'write.rast' function over the '.hdf' files in the folder 
  ##takes about 5 minutes to go through 11 '.hdf' files
lapply(files, write.rast)


