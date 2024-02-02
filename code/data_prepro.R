#COMBINE ALL DATA TOGETHER FOR RANDOM FOREST ANALYSIS#

setwd("D:/martha/DryFlux/data/")

#load in the flux tower data where dates are offset to represent mid-month vals-- e.g. Oct-03 = values from Sept 15 - Oct 14 in 2003
data <- read.csv("Flux_GPPprecip.csv") #Joel's GPP estimates for mid-month timeframe generated from 'CleanFlux.R'
data <- data[,-1]
data$site <- substr(data$sitedate, 1, 6)
data$monthyear <- substr(data$sitedate, 8,15)
#change mexico sites back to mexico
data$site[data$site=="us-ray"] <- "mx-ray"
data$site[data$site=="us-lpa"] <- "mx-lpa"
data$site[data$site=="us-tes"] <- "mx-tes"
#change other sites Joel had unique names for back to regular names
data$site[data$site=="us-sob"] <- "us-so2"
data$site[data$site=="us-soo"] <- "us-so2"
data$site[data$site=="us-son"] <- "us-so4"
data$site[data$site=="us-soy"] <- "us-so3"

data$sitedate <- paste(data$site, data$monthyear, sep="-")

###################################################################
##############COMBINE MODIS VIs WITH FLUX TOWER DATA###############
###################################################################
#make a list of the VI MODIS files
files <- list.files("C:/Users/farellam/Documents/DryFlux/data/re-do/VIs", ".csv", full.names = TRUE)


#define function
VI_vals <- function(x){
  modis <- read.csv(x)
  modis <- modis[,-c(1,3)]
  return(modis)
}

#apply the "VI_vals" function over the csv files
VI_tmp <- lapply(files, VI_vals)
#Bind rows together
VIs <- do.call(rbind, VI_tmp)

# look at the sitedate observations that are not in the data df
nomatch <- subset(VIs, !(sitedate %in% data$sitedate))

#merge the modis VIs with fluxtower data
data2 <- merge(data, VIs, by="sitedate" ,all.x=TRUE)

###################################################################
#####COMBINE CRU meterological variables WITH FLUX TOWER DATA######
###################################################################
setwd("C:/Users/farellam/Documents/DryFlux/data/re-do/")

pet <- read.csv("swusa_pet.csv")
pet <- pet[,c(2,4)]

precip <- read.csv("swusa_precip.csv")
precip <- precip[c(2,4)]

vap <- read.csv("swusa_vap.csv")
vap <- vap[,c(2,4)]

tmax <- read.csv("swusa_Tmax.csv")
tmax <- tmax[,c(2,4)]

tmin <- read.csv("swusa_Tmin.csv")
tmin <- tmin[,c(2,4)]

tavg <- read.csv("swusa_Tavg.csv")
tavg <- tavg[,c(2,4)]

#make a list of the meteorological variable 
meter <- list(pet, precip, vap, tmax, tmin, tavg)
#merge all of the meteorological variables together
cru <- Reduce(merge, meter)

#combine current month meteorological vars with last month vars
lastmo_precip <- read.csv("swusa_lastmo_precip.csv")
lastmo_precip <- lastmo_precip[,-1]

lastmo_tavg <- read.csv("swusa_lastmo_Tavg.csv")
lastmo_tavg <- lastmo_tavg[,-1]

lastmo <- merge(lastmo_precip, lastmo_tavg, by="sitedate")
cru <- merge(cru, lastmo, by="sitedate" ,all.x=TRUE)


#merge the meterological variables with fluxtower and MODIS data
data3 <- merge(data2, cru, by="sitedate" ,all.x=TRUE)


###################################################################
###########COMBINE SPEI  variables WITH FLUX TOWER DATA############
###################################################################
spei <- read.csv("swusa_SPEI.csv")
spei <- spei[,-1]

#merge spei with fluxtower, MODIS, and meterological data
data4 <- merge(data3, spei, by="sitedate" ,all.x=TRUE)


###################################################################
##############COMBINE daylength WITH FLUX TOWER DATA###############
###################################################################
dayln <- read.csv("C:/Users/farellam/Documents/DryFlux/data/daylength.csv")
dayln <- dayln[,-1]

#merge daylength with fluxtower data
data5 <- merge(data4, dayln, by="sitedate" ,all.x=TRUE)


####################################################################################################
##############COMBINE elevation, MAT and MAP values extracted with the raster package###############
####################################################################################################
meta <- read.csv('site_meta.csv')
#meta <- meta[,c(2, 16, 15, 17)]
meta <- meta[,-1]

data6 <- base::merge(data5, meta, by="site", all.x=TRUE)

####################################################################################################
#######################COMBINE MODIS GPP estimates with the RF data dataset#########################
####################################################################################################
gpp <- read.csv("C:/Users/farellam/Documents/DryFlux/data/re-do/swusa_MODIS_GPP.csv")
gpp <- gpp[,-1]

#merge MODIS GPP estimates with the other data
data7 <- merge(data6, gpp, by="sitedate" ,all.x=TRUE)

####################################################################################################
######################COMBINE Fluxcom GPP estimates with the RF data dataset########################
####################################################################################################
fluxcom <- read.csv("D:/martha/DryFlux/data/FluxcomGPP.csv")
fluxcom <- fluxcom[,-1]
data8 <- merge(data7, fluxcom, by="sitedate")

write.csv(data8, "C:/Users/farellam/Documents/DryFlux/data/re-do/RFdata.csv")




####################################################################################################
####################################Global Dryland sites############################################
####################################################################################################
##1.load in the flux tower data where dates are offset to represent mid-month vals-- e.g. Oct-03 = values from Sept 15 - Oct 14 in 2003
setwd("C:/Users/farellam/Documents/DryFlux/data/re-do/")
data <- read.csv("C:/Users/farellam/Documents/DryFlux/data/OzFlux_GPP.csv")
data <- data[,-1]
data$year <- paste("20", substr(data$monthyear, 5,6), sep="")
data$moyear <- paste(substr(data$monthyear, 1,3), data$year, sep="-")
data$sitedate <- paste(data$site, data$moyear, sep="-")
data <- data[,c(6,2)]

##2.Combine MODIS VIs with Fluxtower data
#make a list of the VI MODIS files
files <- list.files("./VIs", ".csv", full.names = TRUE)


#define function
VI_vals <- function(x){
  modis <- read.csv(x)
  #modis$sitedate <- paste(modis$site, modis$mo_yr, sep="-")
  modis <- modis[,-c(1,3)]
  return(modis)
}

#apply the "VI_vals" function over the csv files
VI_tmp <- lapply(files, VI_vals)
#Bind rows together
VIs <- do.call(rbind, VI_tmp)
# look at the sitedate observations that are not in the data df
nomatch <- subset(VIs, !(sitedate %in% data$sitedate))
#merge the modis VIs with fluxtower data
data2 <- merge(data, VIs, by="sitedate" ,all.x=TRUE)

##3. Combine CRU meterological variables 
pet <- read.csv("aus_pet.csv")
#pet$sitedate <- paste(pet$site, pet$mo_yr, sep="-")
pet <- pet[,c(2,4)]
precip <- read.csv("aus_precip.csv")
#precip$sitedate <- paste(precip$site, precip$mo_yr, sep="-")
precip <- precip[c(2,4)]
vap <- read.csv("aus_vap.csv")
#vap$sitedate <- paste(vap$site, vap$mo_yr, sep="-")
vap <- vap[,c(2,4)]
tmax <- read.csv("aus_Tmax.csv")
#tmax$sitedate <- paste(tmax$site, tmax$mo_yr, sep="-")
tmax <- tmax[,c(2,4)]
tmin <- read.csv("aus_Tmin.csv")
#tmin$sitedate <- paste(tmin$site, tmin$mo_yr, sep="-")
tmin <- tmin[,c(2,4)]
tavg <- read.csv("aus_Tavg.csv")
#tavg$sitedate <- paste(tavg$site, tavg$mo_yr, sep="-")
tavg <- tavg[,c(2,4)]
lastmo_precip <- read.csv("aus_lastmo_precip.csv")
lastmo_precip <- lastmo_precip[,c(2,3)]
lastmo_Tavg <- read.csv("aus_lastmo_Tavg.csv")
lastmo_Tavg <- lastmo_Tavg[,c(2,3)]

#merge all of the meterological variables together
cru <- merge(pet, precip, by="sitedate")
cru <- merge(cru, vap, by="sitedate")
cru <- merge(cru, tmax, by="sitedate")
cru <- merge(cru, tmin, by="sitedate")
cru <- merge(cru, tavg, by="sitedate")
cru <- merge(cru, lastmo_precip, by="sitedate")
cru <- merge(cru, lastmo_Tavg, by="sitedate")
#merge the meterological variables with fluxtower and MODIS data
data3 <- merge(data2, cru, by="sitedate" ,all.x=TRUE)

##4.Combine SPEI with Fluxtower data
spei <- read.csv("aus_SPEI.csv")
spei <- spei[,-1]
#merge spei with fluxtower, MODIS, and meterological data
data4 <- merge(data3, spei, by="sitedate" ,all.x=TRUE)

##5. Combine daylength with Fluxtower data
dayln <- read.csv("C:/Users/farellam/Documents/DryFlux/data/aus_daylength.csv")
dayln <- dayln[,-1]
#merge daylength with fluxtower data
data5 <- merge(data4, dayln, by="sitedate" ,all.x=TRUE)

##6. Combine elevation, MAP, and MAT with Fluxtower data
exp1 <- read.csv("C:/Users/farellam/Documents/DryFlux/data/aus_meta.csv")
exp1 <- exp1[-c(24:26),]
exp1 <- exp1[,c(4:7)]

exp2 <- read.csv("C:/Users/farellam/Documents/DryFlux/data/africa_meta.csv")
exp2 <- exp2[,c(3:6)]

exp <- rbind.data.frame(exp1, exp2)

#merge with fluxtower data
data5$site <- substr(data5$sitedate, 1,6)
data6 <- merge(data5, exp, by="site" ,all.x=TRUE)
data6 <- data6[,-1]


#get MODIS and Fluxcom GPP predictions
modis <- read.csv("aus_MODIS_GPP.csv")
modis <- modis[,-1]
  #fix modis site names for the non-Australian sites
  #modis$site <- substr(modis$sitedate, 1,6)
  #modis$site[modis$site== "AU-AMO"] <- "ES-Amo"
  #modis$site[modis$site== "AU-DEM"] <- "SD-Dem"
  #modis$site[modis$site== "AU-DHR"] <- "SN-Dhr"
  
  #modis$date <- substr(modis$sitedate, 8,15)
  #modis <- modis[,-1]
  #modis$sitedate <- paste(modis$site, modis$date, sep="-")
  #modis <- modis[,c(4,1)]

fluxcom <- read.csv("C:/Users/farellam/Documents/DryFlux/data/aus_FluxcomGPP.csv")
fluxcom <- fluxcom[,-1]
othergpp <- merge(modis, fluxcom, by="sitedate")

data7 <- merge(data6, othergpp, by="sitedate")

write.csv(data7, "global_RFdata.csv")
