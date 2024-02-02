###Look at RF model seasonal performance x site and at OzFlux sites###

library(dplyr)
library(plyr)
library(ggvis)
library(caret)


setwd("C:/Users/farellam/Documents/DryFlux/data/")
#load the data
graphdata <- read.csv("coarseDFPred.csv")

meta <- read.csv("C:/Users/farellam/Documents/DryFlux/data/site_meta.csv")


graphdata$site <- substr(graphdata$sitedate, 1,6)
graphdata$month <- substr(graphdata$sitedate, 8,10)

#add veg class
graphdata <- merge(graphdata, meta[,c(2,5)], by="site")


#Split Apply Combine
out2 <- split(graphdata, graphdata$site)


######PLOT SEASONAL TREND BY SITE#######

plot_seasonal <- function(y){
  require("ggplot2")
  require("ggthemes")
  require("scales")
  require("psych")
  x <- ddply(y, .(month), summarize, coarse_se=sd(coarse_GPP, na.rm=TRUE)/sqrt(length(coarse_GPP[!is.na(coarse_GPP)])) , coarse_GPP=mean(coarse_GPP, na.rm=TRUE),
             GPP_se=sd(ObservedGPP, na.rm=TRUE)/sqrt(length(ObservedGPP[!is.na(ObservedGPP)])), GPP=mean(ObservedGPP, na.rm=TRUE),
             MODIS_se=sd(MODIS_GPP, na.rm=TRUE)/sqrt(length(MODIS_GPP[!is.na(MODIS_GPP)])), MODIS_GPP=mean(MODIS_GPP, na.rm=TRUE),
             Fluxcom_se=sd(FluxcomGPP, na.rm=TRUE)/sqrt(length(FluxcomGPP[!is.na(FluxcomGPP)])), FluxcomGPP=mean(FluxcomGPP, na.rm=TRUE))
  x$month <- factor(x$month, levels=month.abb)
  
  r_coarse <- as.character(round(cor(x$GPP, x$coarse_GPP), 3))
  r_modis <- as.character(round(cor(x$GPP, x$MODIS_GPP), 3))
  r_fluxcom <- as.character(round(cor(x$GPP, x$FluxcomGPP), 3))
  
  rmssdGPP <- as.character(round(rmssd(x$GPP), 3))
  rmssdcoarse_GPP <- as.character(round(rmssd(x$coarse_GPP), 3))
  rmssdMODIS_GPP <- as.character(round(rmssd(x$MODIS_GPP), 3))
  rmssdFluxcomGPP <- as.character(round(rmssd(x$FluxcomGPP), 3))
  
  rmsecoarse_GPP <- as.character(round(RMSE(x$coarse_GPP, x$GPP), 3))
  rmseMODIS_GPP <- as.character(round(RMSE(x$MODIS_GPP, x$GPP), 3))
  rmseFluxcomGPP <- as.character(round(RMSE(x$FluxcomGPP, x$GPP), 3))
  
  lbl1 <- paste("Observed: RMSSD =", rmssdGPP)
  lbl2 <- paste("DryFlux: rmse =", rmsecoarse_GPP, "; r =", r_coarse, "; RMSSD =", rmssdcoarse_GPP)
  lbl4 <- paste("MODIS: rmse =", rmseMODIS_GPP, "; r =", r_modis, "; RMSSD =", rmssdMODIS_GPP)
  lbl5 <- paste("Fluxcom: rmse =", rmseFluxcomGPP, "; r =", r_fluxcom, "; RMSSD =", rmssdFluxcomGPP)
  
  lbl6 <- y$VegClass[1]
  site <- y$site[1]
  
  filename <- paste(y$site[1], "seasonal_comparison.png", sep="_")
  print(filename)
  q <- ggplot() +
    ggtitle(x$site)+
    ylim(0,7.5)+
    geom_line(data = x, aes(x = month, group=1, y = GPP, color =I("#C57B57")), size=2) +
    geom_line(data = x, aes(x = month, group=1, y = coarse_GPP, color = I("#5D576B")), size=2) +
    geom_line(data = x, aes(x = month, group=1, y = MODIS_GPP, color = I("#70AE6E")), size=2) +
    geom_line(data = x, aes(x = month, group=1, y = FluxcomGPP, color = I("#639FAB")), size=2) +
    
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="#C57B57")+
    geom_errorbar(data=x,aes(x=month, ymin=coarse_GPP-coarse_se,ymax=coarse_GPP+coarse_se),colour="#5D576B")+
    geom_errorbar(data=x,aes(x=month, ymin=MODIS_GPP-MODIS_se,ymax=MODIS_GPP+MODIS_se),colour="#70AE6E")+
    geom_errorbar(data=x,aes(x=month, ymin=FluxcomGPP-Fluxcom_se,ymax=FluxcomGPP+Fluxcom_se),colour="#639FAB")+
    
    annotate("text", label = lbl1, parse=FALSE, x = 3.6, y = 7.5, size = 3.5, colour = "#C57B57")+
    annotate("text", label = lbl2, parse=FALSE, x = 6.2, y = 7.0, size = 3.5, colour = "#5D576B")+
    annotate("text", label = lbl4, parse=FALSE, x = 6.2, y = 6.5, size = 3.5, colour = "#70AE6E")+
    annotate("text", label = lbl5, parse=FALSE, x = 6.2, y = 6.0, size = 3.5, colour = "#639FAB")+
    
    
    annotate("text", label = lbl6, parse=FALSE, x = 11, y = 7.5, size = 4, colour = "Black")+
    ggtitle(paste(site, "(Testing Site)", sep=""))+
    scale_x_discrete(limits = month.abb)+
    xlab('month')+
    ylab('GPP')+
    theme_classic()+
    theme(legend.position = c(0, 0))
    #theme(panel.background = element_rect(fill = "gray85"))+ #change backgrounds for testing sites
    #theme(plot.background = element_rect(fill = "gray85")) #change background for testing sites
  plot(q)
  ggsave(filename, device='png', width=10, height=10, plot=q, dpi = 300, units="cm")
}

setwd("C:/Users/farellam/Documents/DryFlux/plots/seasonal/again")
lapply(out2, plot_seasonal)
getwd()

######################################################
############SEASONAL TREND AT OZFLUX SITES############
######################################################

###SEASONAL CYCLE FOR OzFLUX SITES###
ausTest <- read.csv("aus_RFdata.csv")
ausTest <- ausTest[,-1]

#calculate VPsat
ausTest$VPsat <- 610.78 * exp((17.269*ausTest$Tavg)/(237.3+ausTest$Tavg))
ausTest$vpd <- ausTest$VPsat - ausTest$vap # for fine res data
ausTest$vpd <- ausTest$VPsat - ausTest$vap*100 #for coarse res CRU data

ausTest$site <- substr(ausTest$sitedate, 1,7)

ausTest <- ausTest[,c(1,2,4:7,27,8:25)]
ausTest <- ausTest[complete.cases(ausTest), ]

#load the RFmodel
RFmodel <- readRDS("C:/Users/farellam/Documents/DryFlux/RFmodels/coarse_new.rds")

#define the explanatory and response variable columns
expvar <- colnames(ausTest[c(4:23)])
resvar <- colnames(ausTest[3])

#make a dataset with only the predicted GPP values 
Predicted <- ausTest[,c(1,3,24,25)]
colnames(Predicted) 
colnames(Predicted) <- c("sitedate", "ObservedGPP", "MODIS_GPP", "FluxcomGPP")
Predicted$coarse_GPP <- predict(RFmodel, ausTest[,expvar])

#replace -9999 with NA
Predicted$ObservedGPP[Predicted$ObservedGPP == -9999] <- NA
Predicted$ObservedGPP[Predicted$ObservedGPP < -1000] <- NA

Predicted <- Predicted[complete.cases(Predicted), ]


write.csv(Predicted, "ausDFPred.csv")

#load the graphdata
graphdata <- read.csv("ausDFPred.csv")

graphdata$site <- substr(graphdata$sitedate, 1,6)
graphdata$month <- substr(graphdata$sitedate, 8,10)


#Split Apply Combine
out2 <- split(graphdata, graphdata$site)

#make the plots

######PLOT SEASONAL TREND BY SITE#######

plot_seasonal <- function(y){
  require("ggplot2")
  require("ggthemes")
  require("scales")
  require("psych")
  x <- ddply(y, .(month), summarize, coarse_se=sd(coarse_GPP, na.rm=TRUE)/sqrt(length(coarse_GPP[!is.na(coarse_GPP)])) , coarse_GPP=mean(coarse_GPP, na.rm=TRUE),
             GPP_se=sd(ObservedGPP, na.rm=TRUE)/sqrt(length(ObservedGPP[!is.na(ObservedGPP)])), GPP=mean(ObservedGPP, na.rm=TRUE),
             MODIS_se=sd(MODIS_GPP, na.rm=TRUE)/sqrt(length(MODIS_GPP[!is.na(MODIS_GPP)])), MODIS_GPP=mean(MODIS_GPP, na.rm=TRUE),
             Fluxcom_se=sd(FluxcomGPP, na.rm=TRUE)/sqrt(length(FluxcomGPP[!is.na(FluxcomGPP)])), FluxcomGPP=mean(FluxcomGPP, na.rm=TRUE))
  x$month <- factor(x$month, levels=month.abb)
  
  r_coarse <- as.character(round(cor(x$GPP, x$coarse_GPP), 3))
  r_modis <- as.character(round(cor(x$GPP, x$MODIS_GPP), 3))
  r_fluxcom <- as.character(round(cor(x$GPP, x$FluxcomGPP), 3))
  
  rmssdGPP <- as.character(round(rmssd(x$GPP), 3))
  rmssdcoarse_GPP <- as.character(round(rmssd(x$coarse_GPP), 3))
  rmssdMODIS_GPP <- as.character(round(rmssd(x$MODIS_GPP), 3))
  rmssdFluxcomGPP <- as.character(round(rmssd(x$FluxcomGPP), 3))
  
  rmsecoarse_GPP <- as.character(round(RMSE(x$coarse_GPP, x$GPP), 3))
  rmseMODIS_GPP <- as.character(round(RMSE(x$MODIS_GPP, x$GPP), 3))
  rmseFluxcomGPP <- as.character(round(RMSE(x$FluxcomGPP, x$GPP), 3))
  
  lbl1 <- paste("Observed: RMSSD =", rmssdGPP)
  lbl2 <- paste("DryFlux: rmse =", rmsecoarse_GPP, "; r =", r_coarse, "; RMSSD =", rmssdcoarse_GPP)
  lbl4 <- paste("MODIS: rmse =", rmseMODIS_GPP, "; r =", r_modis, "; RMSSD =", rmssdMODIS_GPP)
  lbl5 <- paste("Fluxcom: rmse =", rmseFluxcomGPP, "; r =", r_fluxcom, "; RMSSD =", rmssdFluxcomGPP)
  
  lbl6 <- y$VegClass[1]
  site <- y$site[1]
  
  filename <- paste(y$site[1], "seasonal_comparison.png", sep="_")
  print(filename)
  q <- ggplot() +
    ggtitle(x$site)+
    ylim(-2,12)+
    geom_line(data = x, aes(x = month, group=1, y = GPP, color =I("#C57B57")), size=2) +
    geom_line(data = x, aes(x = month, group=1, y = coarse_GPP, color = I("#5D576B")), size=2) +
    geom_line(data = x, aes(x = month, group=1, y = MODIS_GPP, color = I("#70AE6E")), size=2) +
    geom_line(data = x, aes(x = month, group=1, y = FluxcomGPP, color = I("#639FAB")), size=2) +
    
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="#C57B57")+
    geom_errorbar(data=x,aes(x=month, ymin=coarse_GPP-coarse_se,ymax=coarse_GPP+coarse_se),colour="#5D576B")+
    geom_errorbar(data=x,aes(x=month, ymin=MODIS_GPP-MODIS_se,ymax=MODIS_GPP+MODIS_se),colour="#70AE6E")+
    geom_errorbar(data=x,aes(x=month, ymin=FluxcomGPP-Fluxcom_se,ymax=FluxcomGPP+Fluxcom_se),colour="#639FAB")+
    
    annotate("text", label = lbl1, parse=FALSE, x = 3.6, y = 12, size = 3.5, colour = "#C57B57")+
    annotate("text", label = lbl2, parse=FALSE, x = 6.2, y = 11, size = 3.5, colour = "#5D576B")+
    annotate("text", label = lbl4, parse=FALSE, x = 6.2, y = 10, size = 3.5, colour = "#70AE6E")+
    annotate("text", label = lbl5, parse=FALSE, x = 6.4, y = 9, size = 3.5, colour = "#639FAB")+
    
    
    annotate("text", label = lbl6, parse=FALSE, x = 11, y = 7.5, size = 4, colour = "Black")+
    ggtitle(site)+
    scale_x_discrete(limits = month.abb)+
    xlab('month')+
    ylab('GPP')+
    theme_classic()+
    theme(legend.position = c(0, 0))
  
  plot(q)
  ggsave(filename, device='png', width=10, height=10, plot=q, dpi = 300, units="cm")
}

setwd("C:/Users/farellam/Documents/DryFlux/plots/OzFlux/")
lapply(out2, plot_seasonal)
