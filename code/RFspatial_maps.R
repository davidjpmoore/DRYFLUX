#SPATIAL DATA ANAYLSIS AND MAP CREATION FOR DRYFLUX
#1. create mid-monthly tif composites (avg vals) for the Fluxcom GPP estimates 
#2. calculate yearly GPP estimates for DryFlux and Fluxcom
#3. create maps of differences in yearly GPP estimates
#4. calculate yearly GPP z-scores for DryFlux and plot results

library(rgdal)
library(raster)
library(extrafont)
library(ggplot2)
library(rangeBuilder)
library("rnaturalearth")
library("rnaturalearthdata")

setwd("C:/Users/farellam/Documents/DryFlux/data/")


################################################################################################################################################################
####################1. create mid-monthly tif composites (avg vals) for the Fluxcom GPP estimates###############################################################
################################################################################################################################################################

mymonths <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


#list the Fluxcom .nc files
ff <- list.files('D:/martha/DryFlux/data/Fluxcom/', '*.nc', full.names = TRUE)
ff
#select the desired year (yr2) and the year before (yr1) (for Jan vals)
yr1 <- ff[[16]]
yr2 <- ff[[17]]
yr1
yr2

#convert the yearly Fluxcom GPP into a raster stack
y1 <- stack(yr1)
y2 <- stack(yr2)
rasts <- stack(y1, y2)

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
jandates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), Jan, day_num))
febdates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), Feb, day_num))
mardates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), Mar, day_num))
aprdates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), Apr, day_num))
maydates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), May, day_num))
jundates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), Jun, day_num))
juldates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), Jul, day_num))
augdates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), Aug, day_num))
sepdates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), Sep, day_num))
octdates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), Oct, day_num))
novdates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), Nov, day_num))
decdates <- as.Date(sprintf("%d-%02d-%02d", as.numeric(substr(names(y2)[1], 2, 5)), Dec, day_num))

#subset data set based on month delinations
jannames <- paste("X", format(outer(jandates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
Jan <- subset(rasts, jannames)
Janx <- mean(Jan, na.rm=TRUE)
filename <- paste(substr(jandates, 1,4), mymonths[as.numeric(substr(jandates, 6,7))], sep="")
names(Janx) <- filename
writeRaster(Janx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)


febnames <- paste("X", format(outer(febdates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
Feb <- subset(rasts, febnames)
Febx <- mean(Feb, na.rm=TRUE)
filename <- paste(substr(febdates, 1,4), mymonths[as.numeric(substr(febdates, 6,7))], sep="")
names(Febx) <- filename
writeRaster(Febx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)


marnames <- paste("X", format(outer(mardates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
Mar <- subset(rasts, marnames)
Marx <- mean(Mar, na.rm=TRUE)
filename <- paste(substr(mardates, 1,4), mymonths[as.numeric(substr(mardates, 6,7))], sep="")
names(Marx) <- filename
writeRaster(Marx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)


aprnames <- paste("X", format(outer(aprdates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
Apr <- subset(rasts, aprnames)
Aprx <- mean(Apr, na.rm=TRUE)
filename <- paste(substr(aprdates, 1,4), mymonths[as.numeric(substr(aprdates, 6,7))], sep="")
names(Aprx) <- filename
writeRaster(Aprx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)


maynames <- paste("X", format(outer(maydates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
May <- subset(rasts, maynames)
Mayx <- mean(May, na.rm=TRUE)
filename <- paste(substr(maydates, 1,4), mymonths[as.numeric(substr(maydates, 6,7))], sep="")
names(Mayx) <- filename
writeRaster(Mayx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)


junnames <- paste("X", format(outer(jundates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
Jun <- subset(rasts, junnames)
Junx <- mean(Jun, na.rm=TRUE)
filename <- paste(substr(jundates, 1,4), mymonths[as.numeric(substr(jundates, 6,7))], sep="")
names(Junx) <- filename
writeRaster(Junx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)


julnames <- paste("X", format(outer(juldates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
Jul <- subset(rasts, julnames)
Julx <- mean(Jul, na.rm=TRUE)
filename <- paste(substr(juldates, 1,4), mymonths[as.numeric(substr(juldates, 6,7))], sep="")
names(Julx) <- filename
writeRaster(Julx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)


augnames <- paste("X", format(outer(augdates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
Aug <- subset(rasts, augnames)
Augx <- mean(Aug, na.rm=TRUE)
filename <- paste(substr(augdates, 1,4), mymonths[as.numeric(substr(augdates, 6,7))], sep="")
names(Augx) <- filename
writeRaster(Augx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)


sepnames <- paste("X", format(outer(sepdates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
Sep <- subset(rasts, sepnames)
Sepx <- mean(Sep, na.rm=TRUE)
filename <- paste(substr(sepdates, 1,4), mymonths[as.numeric(substr(sepdates, 6,7))], sep="")
names(Sepx) <- filename
writeRaster(Sepx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)

octnames <- paste("X", format(outer(octdates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
Oct <- subset(rasts, octnames)
Octx <- mean(Oct, na.rm=TRUE)
filename <- paste(substr(octdates, 1,4), mymonths[as.numeric(substr(octdates, 6,7))], sep="")
names(Octx) <- filename
writeRaster(Octx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)

novnames <- paste("X", format(outer(novdates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
Nov <- subset(rasts, novnames)
Novx <- mean(Nov, na.rm=TRUE)
filename <- paste(substr(novdates, 1,4), mymonths[as.numeric(substr(novdates, 6,7))], sep="")
names(Novx) <- filename
writeRaster(Novx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)


decnames <- paste("X", format(outer(decdates, -bound:bound, `+`), "%Y.%m.%d"), sep="")
Dec <- subset(rasts, decnames)
Decx <- mean(Dec, na.rm=TRUE)
filename <- paste(substr(decdates, 1,4), mymonths[as.numeric(substr(decdates, 6,7))], sep="")
names(Decx) <- filename
writeRaster(Decx, paste("Z:/SRER/Martha/DryFlux/data/Fluxcom/", filename, ".tif", sep=""), format="GTiff", overwrite=TRUE)

################################################################################################################################################################
############################2. calculate yearly GPP estimates (and diff between mult years) for DryFlux and Fluxcom#############################################
################################################################################################################################################################
####DRYFLUX####
#list the DryFlux predictions for the desired year. (This is the output from 'applyRF.R')
  #define the desired year; **will need to chage for each year of interest**
  year <- 2015
ff <- list.files('C:/Users/farellam/Documents/DryFlux/RFmodels/spatial/', pattern=glob2rx(paste('DFnoeco_X*', year, '.tif', sep="")), full.names = TRUE)
ff

#multiple the monthly predictions by the number of days per month
Jan <- raster(ff[[5]]) * 31
Feb <- raster(ff[[4]]) * 28 # don't change for leap years so # of days per year is consistent across years
Mar <- raster(ff[[8]]) * 31
Apr <- raster(ff[[1]]) * 30
May <- raster(ff[[9]]) * 31
Jun <- raster(ff[[7]]) * 30
Jul <- raster(ff[[6]]) * 31
Aug <- raster(ff[[2]]) * 31
Sep <- raster(ff[[12]]) * 30
Oct <- raster(ff[[11]]) * 31
Nov <- raster(ff[[10]]) * 30
Dec <- raster(ff[[3]]) * 31

#convert each element in the list to a raster stack
rasts <- stack(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

#calculate the sum of these rasters; **will need to chage for each year of interest**
dfgpp2015 <- sum(rasts, na.rm=TRUE)

#after you have the sum of annual GPP for more than one year, you can calculate the difference.
dfdiff <- dfgpp2011 - dfgpp2015



####FLUXCOM####
#list the Fluxcom predictions for the desired year (output from part # of this code)
  #define the desired year; **will need to chage for each year of interest**
  year <- 2015
ff <- list.files('D:/martha/DryFlux/data/Fluxcom/', pattern=glob2rx(paste(year, '*.tif', sep="")), full.names = TRUE)
ff
#multiple the monthly predictions by the number of days per month
Jan <- raster(ff[[5]]) * 31
Feb <- raster(ff[[4]]) * 28 # don't change for leap years so # of days per year is consistent across years
Mar <- raster(ff[[8]]) * 31
Apr <- raster(ff[[1]]) * 30
May <- raster(ff[[9]]) * 31
Jun <- raster(ff[[7]]) * 30
Jul <- raster(ff[[6]]) * 31
Aug <- raster(ff[[2]]) * 31
Sep <- raster(ff[[12]]) * 30
Oct <- raster(ff[[11]]) * 31
Nov <- raster(ff[[10]]) * 30
Dec <- raster(ff[[3]]) * 31

#convert each element in the list to a raster stack
rasts <- stack(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)


#calculate the sum of these rasters; **will need to chage for each year of interest**
fcgpp2015 <- sum(rasts, na.rm=TRUE)

#after you have the sum of annual GPP for more than one year, you can calculate the difference.
fcdiff <- fcgpp2011 - fcgpp2015

################################################################################################################################################################
############################3. create maps of yearly differences################################################################################################
################################################################################################################################################################
#crop differences to dryland regions
#load drylands spatial data
drylands <- readOGR("D:/martha/DryFlux/data/Drylands_dataset_2007/Drylands_latest_July2014", layer="drylands_UNCCD_CBD_july2014")
#select arid and semi-arid regions
drylands2 <- subset(drylands, HIX_DESC %in% c("Arid", "Hyperarid", "Semiarid"))

#mask the dryflux and fluxcom annual differences to the drylands area
dfgpp <- raster::mask(dfdiff, drylands2)
fcgpp <- raster::mask(fcdiff, drylands2)

#crop to the SWUSA
SWUSA <- extent(-121, -99, 24, 46)
df_SWUSA <- crop(dfgpp, SWUSA)
fc_SWUSA <- crop(fcgpp, SWUSA)
#crop to Australia
AUS <- extent(108, 157, -47, 2)
df_AUS <- crop(dfgpp, AUS)
fc_AUS <- crop(fcgpp, AUS)

#load SWUSA and Australia sites

#load Flux tower site locs
#load site locations
sites <- read.csv("C:/Users/farellam/Documents/DryFlux/data/site_locs.csv")
#sites$site <- tolower(sites$site)
#convert site locations into a spatial points df
coordinates(sites)= ~ long + lat
latlon = CRS('+proj=longlat +datum=WGS84 +no_defs')
crs(sites) <- latlon
#global drylands
aus_sites <- read.csv("C:/Users/farellam/Documents/DryFlux/data/aus_drylands.csv")
#sites$site <- tolower(sites$site)
#convert site locations into a spatial points df
coordinates(aus_sites)= ~ long + lat
crs(aus_sites) <- latlon

#####SWUSA PLOT####
#load the US and mexico boundaries
usbounds <- readOGR("D:/martha/DryFlux/data/spatial", "USbounds")
mxbounds <- readOGR("D:/martha/DryFlux/data/spatial", "MXbounds")
#set the plot extent
plot_extent <- extent(-121,-99,24,46)
#crop us/mex boundaries to the plot extent
usbounds <- crop(usbounds, plot_extent)
mxbounds <- crop(mxbounds, plot_extent)

#create the plot window extent
my_window <- extent(-120, -100, 25, 45)

#create axis labels 
xlabs <- c("120°W", "", "", "114°W", "", "", "108°W", "", "", "102°W","")
ylabs <- c("25°N", "", "", "31°N", "","","37°N","","","43°N","" )

#set the cuts and color ramp parameters
cuts <- seq(-700, 700, by=20)
pal <- colorRampPalette(c("#DB4325","#EDA247", "#E6E1Bc", "#57C4AD", "#006164"))

#make the plot
tiff("C:/Users/farellam/Documents/DryFlux/plots/DFnoeco_2011_2015_diffSWUSA.tif",units="in", width = 4.5, height = 4.5, res = 300)
par(mar=c(3,3,1.2,0), bg="white", family="Calibri Light") #2x2 plot 'mai' set the plot margins in inches, 'oma' sets the outer margins in inches
plot(my_window, col=NA, xlab="", ylab="", xaxt="n", yaxt="n")
axis(side=2, at=seq(25,45, by=2), labels=ylabs, las=1)
axis(side=1, at=seq(-120,-100, by=2), labels=xlabs, las=1)
plot(usbounds, col="white", add=TRUE)
plot(mxbounds, col="white", add=TRUE)
plot(df_SWUSA, breaks=cuts, col=pal(length(cuts)-1), add=TRUE, legend=FALSE) #change depending on if you want to plot dryflux or fluxcom difference
plot(usbounds, add=TRUE)
plot(mxbounds, add=TRUE)
box(which="plot", lty="solid")
#add legend
plot(df_SWUSA, breaks=cuts, col=pal(length(cuts)-1), legend.only=TRUE, legend.width=1, legend.shrink=0.75, smallplot=c(.15,.19,.15,.3),
     axis.args = list(at=seq(-700, 700, 350),
                      labels=seq(-700, 700, 350),
                      cex.axis= 0.8),
     legend.args=list(text = '', side = 2, cex=0.8))
plot(sites, add=TRUE)
title(main = "Difference in DryFlux noecohydro GPP 2011 - 2015", cex.main = 1, adj=0, font.main=1) #change title depending on what you're plotting
dev.off()

#####AUSTRALIA PLOTS#####
#load the australia boundaries
aus <- readOGR("D:/martha/DryFlux/data/spatial", layer="AUSbounds")

#set the plot extent
my_window <- extent(110, 155, -45, 0)

#create axis labels 
ylabs <- c("45°S", "", "25°S", "", "5°S")
xlabs <- c("110°E", "", "130°E", "", "150°E")

#set the cuts and color ramp parameters
cuts <- seq(-700, 700, by=20)
pal <- colorRampPalette(c("#DB4325","#EDA247", "#E6E1Bc", "#57C4AD", "#006164"))

#make the plot
tiff("C:/Users/farellam/Documents/DryFlux/plots/DFnoeco_2011_2015_diffAUS.tif",units="in", width =4, height =4, res = 300)
par(mar=c(3,3,1.2,0), bg="white", family="Calibri Light") #2x2 plot 'mai' set the plot margins in inches, 'oma' sets the outer margins in inches
plot(my_window, col=NA, xlab="", ylab="", xaxt="n", yaxt="n")
axis(side=2, at=seq(-45,0, by=10), labels=ylabs, las=1)
axis(side=1, at=seq(110,155, by=10), labels=xlabs, las=1)
box(which="plot", lty="solid")
plot(df_AUS, breaks=cuts, col=pal(length(cuts)-1),legend=FALSE, axes=FALSE, add=TRUE) #change depending on if you want to plot dryflux or fluxcom difference
plot(aus, add=TRUE, col=NA, border="light gray")
#add legend
plot(df_AUS, breaks=cuts, col=pal(length(cuts)-1), legend.only=TRUE, legend.width=1, legend.shrink=0.75, smallplot=c(.23,.27,.7,.85),
     axis.args = list(at=seq(-700, 700, 350),
                      labels=seq(-700, 700, 350),
                      cex.axis= 0.8),
     legend.args=list(text = '', side = 2, cex=0.8))
title(main = "Difference in DryFlux noecohydro GPP 2011 - 2015", cex.main = 1, adj=0, font.main=1) #change title depending on what you're plotting

dev.off()
################################################################################################################################################################
############################4. calcualte yearly GPP z-scores and plot results###################################################################################
################################################################################################################################################################
#list the DryFlux monthly predictions, stack all years together, multiply average GPP val by # of days per month (output from 'applyRF.R')
ff <- list.files('C:/Users/farellam/Documents/DryFlux/RFmodels/spatial', pattern=glob2rx('DF_X*.tif'), full.names = TRUE)
#xx <- subset(ff, substr(ff, 62,64) == "Jan")#for DF with ecohydrological variables
xx <- subset(ff, substr(ff, 67,69) == "Jan")#for DF with NO ecohydrological variables
xx
Jans <- stack(xx)
Jan <- Jans * 31 

xx <- subset(ff, substr(ff, 62,64) == "Feb")
xx
Febs <- stack(xx)
Feb <- Febs * 28

xx <- subset(ff, substr(ff, 62,64) == "Mar")
xx
Mars <- stack(xx)
Mar <- Mars * 31

xx <- subset(ff, substr(ff, 62,64) == "Apr")
xx
Aprs <- stack(xx)
Apr <- Aprs * 30

xx <- subset(ff, substr(ff, 62,64) == "May")
xx
Mays <- stack(xx)
May <- Mays * 31 

xx <- subset(ff, substr(ff, 62,64) == "Jun")
xx
Juns <- stack(xx)
Jun <- Juns * 30

xx <- subset(ff, substr(ff, 62,64) == "Jul")
xx
Juls <- stack(xx)
Jul <- Juls * 31 

xx <- subset(ff, substr(ff, 62,64) == "Aug")
xx
Augs <- stack(xx)
Aug <- Augs * 31

xx <- subset(ff, substr(ff, 62,64) == "Sep")
xx
Seps <- stack(xx)
Sep <- Seps * 30

xx <- subset(ff, substr(ff, 62,64) == "Oct")
xx
Octs <- stack(xx)
Oct <- Octs * 31 

xx <- subset(ff, substr(ff, 62,64) == "Nov")
Novs <- stack(xx)
Nov <- Novs * 30

xx <- subset(ff, substr(ff, 62,64) == "Dec")
Decs <- stack(xx)
Dec <- Decs * 31 

#combine all month x years into a single raster stack
all <- stack(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

#create average and sd rasters for the total time period
totalavg <- calc(all, fun=mean, na.rm=TRUE)
totalsd <- calc(all, fun=sd, na.rm=TRUE)

#list the DryFlux  predictions for a single year of interest
  #define year of interest
  year <- 2015
ff <- list.files('C:/Users/farellam/Documents/DryFlux/RFmodels/spatial/', pattern=glob2rx(paste('DF_X*', year, '.tif', sep='')), full.names = TRUE)
ff

#multiple the monthly predictions by the number of days per month
Jan <- raster(ff[[5]]) * 31
Feb <- raster(ff[[4]]) * 28 # don't change for leap years so # of days per year is consistent across years
Mar <- raster(ff[[8]]) * 31
Apr <- raster(ff[[1]]) * 30
May <- raster(ff[[9]]) * 31
Jun <- raster(ff[[7]]) * 30
Jul <- raster(ff[[6]]) * 31
Aug <- raster(ff[[2]]) * 31
Sep <- raster(ff[[12]]) * 30
Oct <- raster(ff[[11]]) * 31
Nov <- raster(ff[[10]]) * 30
Dec <- raster(ff[[3]]) * 31

#convert each element in the list to a raster stack
rasts <- stack(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)

#calcualte the average 2011 GPP *change for each year*
avg2011 <- calc(rasts, fun=mean, na.rm=TRUE)

#calculate the z-score
z2011 <- (avg2011-totalavg)/totalsd

####create the map####
#crop to dryland regions
drylands <- readOGR("D:/martha/DryFlux/data/Drylands_dataset_2007/Drylands_latest_July2014", layer="drylands_UNCCD_CBD_july2014")
drylands2 <- subset(drylands, HIX_DESC %in% c("Arid", "Hyperarid", "Semiarid"))
dfgpp <- raster::mask(z2011, drylands2)

#load world country data
world <- ne_countries(scale = "medium", returnclass = "sf")

#create the plot extent
my_window <- extent(-150, 150, -60, 60)

#create axis labels 
xlabs <- c("", "100°W", "", "0°", "", "100°E", "")
ylabs <- c("50°S", "0°", "50°N" )

#legend/symbology parameters
cuts <- seq(-2.6,2.6, by=0.2)
pal <- colorRampPalette(c("#DB4325","#EDA247", "#E6E1Bc", "#57C4AD", "#006164"))

#make the plot
jpeg("C:/Users/farellam/Documents/DryFlux/plots/zscore2015_DF.tif",units="in", width = 7.75, height = 3.75, res = 300)
par(mar=c(2.1,2.8,1.2,0), bg="white", family="Calibri Light") #2x2 plot 'mai' set the plot margins in inches, 'oma' sets the outer margins in inches
plot(my_window, col=NA, xlab="", ylab="", xaxt="n", yaxt="n")
axis(side=1, at=seq(-150,150, by=50), labels=xlabs, las=1)
axis(side=2, at=seq(-50,50, by=50), labels=ylabs, las=1)

plot(dfgpp, breaks=cuts, col=pal(length(cuts)-1),legend=FALSE, axes=FALSE, add=TRUE)
plot(dfgpp, breaks=cuts, col=pal(length(cuts)-1), legend.only=TRUE, legend.width=1, legend.shrink=0.75,
     legend.args=list(text='z-score', side=2, line=0.1),
     axis.args = list(at=seq(-2.6, 2.6, 1.3),
                      labels=seq(-2.6, 2.6, 1.3),
                      cex.axis= 0.8),
     smallplot=c(.15,.2,.28,.48))

plot(world, add=TRUE, col=NA, border="light gray")
title(main = "2015 DryFlux GPP z-scores", cex.main = 1, adj=0.08, font.main=1)

dev.off()
