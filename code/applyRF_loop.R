###APPLY THE RF MODELS TO RASTER DATA TO PREDICT GPP ACROSS THE STUDY AREA###

library(testthat)
library(raster)
library(rgdal)
library(rangeBuilder)
library(dplyr)
library(plyr)

################################################################################################################################################################################
####COARSE RESOLUTION DATA UPSCALING############################################################################################################################################
################################################################################################################################################################################
#load raster stacks
pet <- stack("D:/martha/DryFlux/data/CRU/tifs/pet.grd")
pre <- stack("D:/martha/DryFlux/data/CRU/tifs/pre.grd")
rvap <- stack("D:/martha/DryFlux/data/CRU/tifs/vap.grd")
tmx <- stack("D:/martha/DryFlux/data/CRU/tifs/tmx.grd")
tmn <- stack("D:/martha/DryFlux/data/CRU/tifs/tmn.grd")
tmp <- stack("D:/martha/DryFlux/data/CRU/tifs/tmp.grd")
spei1 <- stack("D:/martha/DryFlux/data/CRU/tifs/spei1NEW.grd")
spei3 <- stack("D:/martha/DryFlux/data/CRU/tifs/spei3NEW.grd")
spei6 <- stack("D:/martha/DryFlux/data/CRU/tifs/spei6NEW.grd")
spei9 <- stack("D:/martha/DryFlux/data/CRU/tifs/spei9NEW.grd")
spei12 <- stack("D:/martha/DryFlux/data/CRU/tifs/spei12NEW.grd")
dayln <- stack("D:/martha/DryFlux/data/daylength.tif")
MAT <- raster("D:/martha/DryFlux/data/spatial/globalMAT.tif")
MAP <- raster("D:/martha/DryFlux/data/spatial/globalMAP.tif")
elev <- raster("D:/martha/DryFlux/data/spatial/global_elev.tif")

#subset one month of the precip raster
pre_onemo <- subset(pre, "X2000Dec")

#reproject MAT, MAP, and elevation rasters so that resolution and crs are matching
#use the CRU raster as the 'master'
r17 <-projectRaster(MAT, pre_onemo)
r18 <- projectRaster(MAP, pre_onemo)
r19 <- projectRaster(elev, pre_onemo)

#give date names to the daylength raster
#get the dates of CRU observations
dl_t <- read.csv("D:/martha/DryFlux/data/dates.csv")
dates <- paste(dl_t$vals_year, dl_t$mo, sep=".")
names(dayln) <- dates

#load the RFmodels
RFmodel <- readRDS("C:/Users/farellam/Documents/DryFlux/RFmodels/coarse_new.rds")
RFmodel_noeco <- readRDS("C:/Users/farellam/Documents/DryFlux/RFmodels/coarse_noecohydro.rds")

#load Flux tower site locs
#load site locations
sites <- read.csv("C:/Users/farellam/Documents/DryFlux/data/site_locs.csv")
#sites$site <- tolower(sites$site)
#convert site locations into a spatial points df
coordinates(sites)= ~ long + lat
latlon = CRS('+proj=longlat +datum=WGS84 +no_defs')
crs(sites) <- latlon
#global drylands
aus_sites <- read.csv("C:/Users/farellam/Documents/DryFlux/data/global_locs.csv")
#sites$site <- tolower(sites$site)
#convert site locations into a spatial points df
coordinates(aus_sites)= ~ long + lat
crs(aus_sites) <- latlon


#######################################################################################
#function to select the correct raters, re-project NDVI and EVI to CRU raster resolution/CRS, stack all rasters, then use the RF models to predict GPP, then extracts GPP at Fluxtower locs
get_gpp <- function(i) {
  date1 <- thismo[i]
  lastmo <- last_mos[i]
  print(paste("now processing ", substr(date1, 6, 8)," ", substr(date1, 2,5), " (last month ", substr(lastmo, 6,8)," ", substr(lastmo, 2,5), ")", sep=""))
  
  if(i < 10){
    j <- paste("0",i,sep="")
  } else {
    j <- i
  }
  #first we need to get all of the raster layers to align with eachother. 
  #Select rasters for the appropriate month/year
  ndvi_file <- paste("ndvi_", year, ".", j, sep="")
  ndvi <- list.files(path="D:/martha/DryFlux/data/MOD13C1/tifs/", pattern=ndvi_file, full.names = TRUE)
  r1 <- raster(ndvi)
  r1 <- r1 * 0.0001
 
  evi_file <- paste("evi_", year, ".", j, sep="")
  evi <- list.files(path="D:/martha/DryFlux/data/MOD13C1/tifs/", pattern=evi_file, full.names = TRUE)
  r2 <- raster(evi)
  r2 <- r2 * 0.0001
  
  #select the desired date with the CRU data
  r3 <- subset(pet, date1) 
  r4 <- subset(pre, date1) 
  rvap_mo <- subset(rvap, date1) 
  r6 <- subset(tmx, date1) 
  r7 <- subset(tmn, date1) 
  r8 <- subset(tmp, date1)
  r9 <- subset(pre, lastmo) 
  r10 <- subset(tmp, lastmo)
  r11 <- subset(spei1, date1) 
  r12 <- subset(spei3, date1) 
  r13 <- subset(spei6, date1)
  r14 <- subset(spei9, date1) 
  r15 <- subset(spei12, date1) 
  
  #Calculating VPD from Tavg and vap raster
  #calculate VPsat
  VPsat <- 610.78 * exp((17.269*r8)/(237.3+r8))
  #calculate VPD raster
  r5 <- VPsat - rvap_mo*100
  
  #subset the daylength raster
  # if subsetting oct, nov, or dec, change 2nd substr to 11,12; otherwise 12, 12
  if(substr(date1, 6,8) %in% c("Oct", "Nov", "Dec")){
    dayln_mo <- 11 
  } else {
    dayln_mo <- 12
  }
  
  dayl_date <- paste("X", substr(names(r1), 6, 10), substr(names(r1), dayln_mo, 12),sep="") 
  r16 <- subset(dayln, dayl_date)
  
  #reproject the NDVI and EVI rasters so that resolution and crs are matching
  #use the CRU raster as the 'master'
  r1_p <-projectRaster(r1, r4)
  r2_p <- projectRaster(r2, r4)
  
  #make a list of all the rasters
  files <- list(r1_p, r2_p, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19)
  #stack all of the raster layers together
  env_data <- stack(files)
  names(env_data) <- c("ndvi", "evi", "pet", "precip", "vpd", "Tmax", "Tmin", "Tavg", "lastmo_precip", "lastmo_Tavg", "spei1", "spei3", "spei6", "spei9", "spei12", "daylength", "MAT", "MAP", "elev")

  #use the RF model to predict GPP on the raster stack
  GPP <- predict(env_data, RFmodel)
  #write the output
  DFoutname <- paste("C:/Users/farellam/Documents/DryFlux/RFmodels/spatial/DF_", date1, year, ".tif", sep="")
  writeRaster(GPP, DFoutname, format="GTiff", overwrite=TRUE)
  
  #make a list of all the rasters
  files_noeco <- list(r1_p, r2_p, r6, r7, r8, r10, r16, r17, r18, r19)
  #stack all of the raster layers together
  data_noeco <- stack(files_noeco)
  names(data_noeco) <- c("ndvi", "evi", "Tmax", "Tmin", "Tavg", "lastmo_Tavg", "daylength", "MAT", "MAP", "elev")
  
  #use the RF model to predict GPP on the raster stack
  GPP_noeco <- predict(data_noeco, RFmodel_noeco)
  #write the output
  DFnoeco_outname <- paste("C:/Users/farellam/Documents/DryFlux/RFmodels/spatial/DFnoeco_", date1, year, ".tif", sep="")
  writeRaster(GPP_noeco, DFnoeco_outname, format="GTiff", overwrite=TRUE)
  
  
  #extract DryFlux model predictions for site locs
  #DF <- raster::extract(GPP,sites)
  #DFnoeco <- raster::extract(GPP_noeco, sites)
  #final <- cbind.data.frame(sites$site, DF, DFnoeco)
  #colnames(final) <- c("site", "DryFlux", "DryFlux_noeco")
  #final$monthyr <- paste(substr(date1, 6, 8), substr(date1, 2,5), sep="")
  #return(final)
  
  
  #DF_aus <- raster::extract(GPP, aus_sites)
  #DFnoeco_aus <- raster::extract(GPP_noeco, aus_sites)
  #aus_final <- cbind.data.frame(aus_sites$SITE_ID, DF_aus, DFnoeco_aus)
  #colnames(aus_final) <- c("site", "DryFlux", "DryFlux_noeco")
  #aus_final$monthyr <- paste(substr(date1, 6, 8), substr(date1, 2,5), sep="")
  #return(aus_final)
}

num_mos <- c(1,2,3,4,5,6,7,8,9,10,11)
  
##define the year and then run through the loop
year <- 2000
months <- c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
thismo <- paste("X", year, months, sep="")

lastyr <- year - 1
first_mo <- paste("X", lastyr, "Dec", sep="")
other_mo <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
last_mos <- paste("X", year, other_mo, sep="")
last_mos <- c(first_mo, last_mos)


#apply the function over all months 
final_gpp <- lapply(num_mos, get_gpp) %>% bind_rows()

#rbind extracted GPP from each year together
#all_results <- final_gpp # for the first year
all_results <- rbind.data.frame(all_results, final_gpp) #for all other years after the first

#write.csv(all_results, "C:/Users/farellam/Documents/DryFlux/aus_DryFlux_raster_extraction.csv")




############################################################################
########compare spatial predictions to observed GPP#########################
############################################################################
results <- read.csv("C:/Users/farellam/Documents/DryFlux/aus_DryFlux_raster_extraction.csv")
results$sitedate <- paste(results$site, substr(results$monthyr, 1, 3), substr(results$monthyr, 4,7), sep="-")
results <- results[,-c(1,2,5)]

data <- read.csv("aus_RFdata.csv")

all <- merge(results, data[,c(2,5)], by="sitedate")

all$site <- substr(all$sitedate, 1,6)

summ <- ddply(all, .(site), summarize, DFcor=cor(DryFlux, GPP, use="pairwise.complete.obs"), DFnoeco_cor = cor(DryFlux_noeco, GPP, use="pairwise.complete.obs"))

write.csv(summ, "C:/Users/farellam/Documents/DryFlux/aus_DryFlux_spatial_cor.csv")
