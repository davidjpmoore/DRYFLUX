library(raster)
library(sp)
library(rgdal)

main_directory <- "C:/Users/lindseybell/OneDrive - University of Arizona/Desktop/DRYFLUX/"
setwd(main_directory)

##############################################################################
######################### meta data for tower sites###########################
##############################################################################
#load site locations
sites <- read.csv("./data/site_locs.csv")
#sites$site <- tolower(sites$site)

#convert site locations into a spatial points df
coordinates(sites)= ~ long + lat
latlon = CRS('+proj=longlat +datum=WGS84 +no_defs')
crs(sites) <- latlon

####ELEVATIONS FOR US/MEX SITES####
setwd("./data/SRTM")

elev1 <- getData('SRTM', lon=-116, lat=33)
#plot(elev1)
e1 <- raster::extract(elev1,sites)
e1 <- cbind.data.frame(sites$site,e1)
e1 <- e1[complete.cases(e1), ]

elev2 <- getData('SRTM', lon=-111, lat=35)
e2 <- raster::extract(elev2,sites)
e2 <- cbind.data.frame(sites$site,e2)
e2 <- e2[complete.cases(e2), ]

elev3 <- getData('SRTM', lon=-111, lat=36)
e3 <- raster::extract(elev3,sites)
e3 <- cbind.data.frame(sites$site,e3)
e3 <- e3[complete.cases(e3), ]

elev4 <- getData('SRTM', lon=-111, lat=25)
e4 <- raster::extract(elev4,sites)
e4 <- cbind.data.frame(sites$site,e4)
e4 <- e4[complete.cases(e4), ]

elev5 <- getData('SRTM', lon=-111, lat=26)
#plot(elev5)
e5 <- raster::extract(elev5,sites)
e5 <- cbind.data.frame(sites$site,e5)
e5 <- e5[complete.cases(e5), ]

elev6 <- getData('SRTM', lon=-109, lat=31)
e6 <- raster::extract(elev6,sites)
e6 <- cbind.data.frame(sites$site,e6)
e6 <- e6[complete.cases(e6), ]

elev7 <- getData('SRTM', lon=-109, lat=38)
e7 <- raster::extract(elev7,sites)
e7 <- cbind.data.frame(sites$site,e7)
e7 <- e7[complete.cases(e7), ]

elev8 <- getData('SRTM', lon=-109, lat=27)
e8 <- raster::extract(elev8,sites)
e8 <- cbind.data.frame(sites$site,e8)
e8 <- e8[complete.cases(e8), ]

elev9 <- getData('SRTM', lon=-117, lat=38)
elev10 <- getData('SRTM', lon=-117, lat=27)
elev11 <- getData('SRTM', lon=-107, lat=23)


#mosaic all of the rasters together, write out .tif file
allelev <- c(elev1, elev2, elev3, elev4, elev5, elev6, elev7, elev8)
allelev$fun <- mean

setwd(main_directory)
setwd("./data/spatial")
mos <- do.call(mosaic, allelev)
writeRaster(mos, "./SoAzelev.tif", format="GTiff")


#rename the column names for all of the elevation datasets
renameFunction<-function(x,someNames){
  names(x) <- someNames
  return(x)
}

all <- list(e1, e2, e3, e4, e5, e6, e7, e8)

names_elev <- c("site", "elev")
all <- lapply(all, renameFunction, names_elev) 
elevation <- do.call(rbind, all)


########MAT/MAP for sites########
r <- getData("worldclim",var="bio",res=0.5, lon=-110, lat=20)
#Bio  and Bio12 are mean annual temperature and annual precipitation
r <- r[[c(1,12)]]
names(r) <- c("MAT","MAP")

#extract values for site locations
values <- raster::extract(r,sites)
df <- cbind.data.frame(sites$site,values)
df <- df[complete.cases(df), ]

#load an alternate tile of data (if needed, for Australia sites)
r2 <- getData("worldclim",var="bio",res=0.5, lon=-110, lat=31)
#Bio  and Bio12 are mean annual temperature and annual precipitation
r2 <- r2[[c(1,12)]]
names(r2) <- c("MAT","MAP")
values2 <- raster::extract(r2,sites)
df2 <- cbind.data.frame(sites$site,values2)
df2 <- df2[complete.cases(df2), ]
final <- rbind.data.frame(df, df2)

#combine the MAP/MAT dataframe with the elevation dataframe
colnames(final) <- c("site", "MAT", "MAP")
  #divide MAT by 10
  final$MAT <- final$MAT/10
exp_var <- merge(final, elevation, by="site")


colnames(exp_var) <- c("site", "WorldClimMAT", "WorldClimMAP", "SRTMelev")
exp_var$site <- tolower(exp_var$site)
setwd(main_directory)
sites <- read.csv("./data/site_locs.csv")
sites$site <- tolower(sites$site)
sites <- sites[,-1]
data <- merge(sites, exp_var, by="site")
write.csv(data, "./data/site_meta.csv")


##############################################################################
##########################Elevation for OzFlux sites##########################
##############################################################################
#load site locations
sites <- read.csv("./data/global_locs.csv")
#sites$site <- tolower(sites$site)
#convert site locations into a spatial points df
coordinates(sites)= ~ long + lat
latlon = CRS('+proj=longlat +datum=WGS84 +no_defs')
crs(sites) <- latlon


setwd("./data/SRTM")

elev <- getData('SRTM', lon=-16, lat=16)
e1 <- raster::extract(elev,sites)
e1 <- cbind.data.frame(sites$SITE_ID,e1)
e1 <- e1[complete.cases(e1), ]
#plot(elev)
#plot(sites, add=TRUE)

elev <- getData('SRTM', lon=-2, lat=36)
e2 <- raster::extract(elev,sites)
e2 <- cbind.data.frame(sites$SITE_ID,e2)
e2 <- e2[complete.cases(e2), ]

elev <- getData('SRTM', lon=30, lat=13)
e3 <- raster::extract(elev,sites)
e3 <- cbind.data.frame(sites$SITE_ID,e3)
e3 <- e3[complete.cases(e3), ]

elev <- getData('SRTM', lon=148, lat=-24)
e4 <- raster::extract(elev,sites)
e4 <- cbind.data.frame(sites$SITE_ID,e4)
e4 <- e4[complete.cases(e4), ]

elev <- getData('SRTM', lon=120, lat=-30)
e5 <- raster::extract(elev,sites)
e5 <- cbind.data.frame(sites$SITE_ID,e5)
e5 <- e5[complete.cases(e5), ]

elev <- getData('SRTM', lon=115, lat=-31)
#plot(elev)
e6 <- raster::extract(elev,sites)
e6 <- cbind.data.frame(sites$SITE_ID,e6)
e6 <- e6[complete.cases(e6), ]

elev <- getData('SRTM', lon=140, lat=-34)
#plot(elev)
e7 <- raster::extract(elev,sites)
e7 <- cbind.data.frame(sites$SITE_ID,e7)
e7 <- e7[complete.cases(e7), ]

elev <- getData('SRTM', lon=146, lat=-34)
#plot(elev)
e8 <- raster::extract(elev,sites)
e8 <- cbind.data.frame(sites$SITE_ID,e8)
e8 <- e8[complete.cases(e8), ]

elev <- getData('SRTM', lon=145, lat=-36)
#plot(elev)
e9 <- raster::extract(elev,sites)
e9 <- cbind.data.frame(sites$SITE_ID,e9)
e9 <- e9[complete.cases(e9), ]

elev <- getData('SRTM', lon=150, lat=-33)
#plot(elev)
e10 <- raster::extract(elev,sites)
e10 <- cbind.data.frame(sites$SITE_ID,e10)
e10 <- e10[complete.cases(e10), ]

elev <- getData('SRTM', lon=147, lat=-17)
e11 <- raster::extract(elev,sites)
e11 <- cbind.data.frame(sites$SITE_ID,e11)
e11 <- e11[complete.cases(e11), ]

elev <- getData('SRTM', lon=142, lat=-37)
e12 <- raster::extract(elev,sites)
e12 <- cbind.data.frame(sites$SITE_ID,e12)
e12 <- e12[complete.cases(e12), ]



all <- list(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12)

#rename the column names for all of the elevation datasets
renameFunction<-function(x,someNames){
  names(x) <- someNames
  return(x)
}
someNames <- c("site", "elev")
all <- lapply(all, renameFunction, someNames) 
elevation <- do.call(rbind, all)



#combine the MAP/MAT dataframe with the elevation dataframe
exp_var <- merge(final, elevation, by="site")

setwd(main_directory)
write.csv(exp_var, "C./data/global_meta.csv")


##############################################################################
#########################OzFlux dryland sites#################################
##############################################################################
######Determine Australia OzFlux sites that fall within the drylands areas######
library(rgdal)
library(sf)
library(sp)


getwd()
setwd("C:/Users/farellam/Documents/DryFlux/data")
#load data with site coordinates
pts <- read.csv("./OzFlux/Ozsites.csv")
data <- pts
#convert site locations into a spatial points df
coordinates(data)= ~ long + lat
data

#load the drylands dataset
drylands <- readOGR("./Drylands_dataset_2007/Drylands_latest_July2014", layer="drylands_UNCCD_CBD_july2014")

plot(drylands)
plot(data, add=TRUE)

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(data) <- proj4string(drylands)

#determine points that fall within the australia dryland polygons
aus_dry <- sp::over(data, drylands, fn=NULL)
aus_dry <- aus_dry[complete.cases(aus_dry), ]
rn <- rownames(aus_dry)
aus_pts <- pts[rn,]

write.csv(aus_pts, "aus_drylands.csv")
aus_pts <- read.csv("C:/Users/farellam/Documents/DryFlux/data/aus_drylands.csv")
coordinates(aus_pts)= ~ long + lat
# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(aus_pts) <- proj4string(drylands)
writeOGR(aus_pts, "C:/Users/farellam/Documents/DryFlux/data/spatial", "AUS_sites", driver="ESRI Shapefile", overwrite_layer = TRUE)



##############################################################################
#########################Global adm boundaries################################
##############################################################################
us <- getData('GADM', country='USA', level=1)
plot(us)

mx <-  getData('GADM', country='MEX', level=1)
plot(mx)

au <- getData('GADM', country='AUS', level=1)
plot(au)


writeOGR(us, "Z:/SRER/Martha/DryFlux/data/spatial", "USbounds", driver="ESRI Shapefile")  # create to a shapefile 
writeOGR(mx, "Z:/SRER/Martha/DryFlux/data/spatial", "MXbounds", driver="ESRI Shapefile")  # create to a shapefile 
writeOGR(au, "Z:/SRER/Martha/DryFlux/data/spatial", "AUSbounds", driver="ESRI Shapefile")  # create to a shapefile 

