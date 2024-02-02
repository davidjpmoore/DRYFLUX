#1. use the Random Forest model to predict GPP for Fluxtower sites 
#2. Compare DryFlux and Fluxcom performance to observed GPP -- create bar charts
#3. Compare DryFlux, Fluxcom, MODIS performance to observed GPP -- plot results
#4. Compare DryFlux, Fluxcom, MODIS, and observed GPP variance -- create bar chart
#5. Compare interannual variability in GPP predictions -- line graph
#6. Compare total GPP predictions x veg class and x site -- create bar chart
#7. evaluate anomalous GPP and precip values at each site. -- create line graphs
 

#############################################################################################################
######################1. use the Random Forest model to predict GPP for Fluxtower sites######################
#############################################################################################################
setwd("C:/Users/farellam/Documents/DryFlux/data/")
#####LOAD THE COARSE RES DATA#####
data <- read.csv("RFdata.csv")
data$site <- as.factor(data$site)
summary(data$site)
#add month column
#data$month <- substr(data$monthyear, 1,3)
#data$month <- as.factor(data$month)

#calculate VPsat
data$VPsat <- 610.78 * exp((17.269*data$Tavg)/(237.3+data$Tavg))
data$vpd <- data$VPsat - data$vap*100 #for coarse res CRU data

names(data)
#select columns wanted for RF
All_sites <- data[,c(2,3,5,6:9,28,11:26)] #for SWUSA sites

#remove samples with incomplete observations
All_sites <- All_sites[complete.cases(All_sites),]
#FOR NO NORMALIZATION
All <- All_sites

#define the explanatory and response variable columns
expvar <- colnames(All[c(4:22)])
resvar <- colnames(All[3])
coarsedata <- All[,expvar]

#Load the coarse res RF model
RFmodel <- readRDS("C:/Users/farellam/Documents/DryFlux/RFmodels/coarse_new.rds")
Predicted <- All[,c(1,3,23,24)]
colnames(Predicted) 
colnames(Predicted) <- c("sitedate", "ObservedGPP", "MODIS_GPP", "FluxcomGPP")
Predicted$coarse_GPP <- predict(RFmodel, coarsedata)

write.csv(Predicted, "coarseDFPred.csv")

#############################################################################################################
#########################2. Compare DryFlux and Fluxcom performance to observed GPP (Fig 1a-b)###############
#############################################################################################################

library(plyr)
library(ggplot2)
library(ggpattern)

setwd("C:/Users/farellam/Documents/DryFlux/data/")
coarsePred <- read.csv("coarseDFPred.csv")
#finePred <- read.csv("fineDFPred.csv")

final <- coarsePred

final$site <- substr(final$sitedate, 1,6)
final$year <- substr(final$sitedate, 12,16)


#define function to get correlation values
corfunc <- function(xx){
  return(data.frame(COR = cor(xx$ObservedGPP, xx$FluxcomGPP, use="pairwise.complete.obs")))
}

sites <- unique(as.factor(final$site))
vals <- matrix(nrow=length(sites), ncol=2)

row <- 0
for(i in sites){
  site_i <- print(i)
  y <- subset(final, site == site_i)
  y$year <- as.factor(y$year)
  corr_year <- ddply(y, .(year), corfunc)
  corr_year$R2 <- corr_year$COR * corr_year$COR
  vals[row+1,1] <- site_i
  vals[row+1,2] <- mean(corr_year$R2, na.rm=TRUE)
  row <- row+1
}

#load site meta data
meta <- read.csv("site_meta.csv")
meta2 <- read.csv("site_meta2.csv")
meta <- merge(meta[,c(2,4:15)], meta2, by="site")

vegclass <- meta[,c(1,3,16)]
#add veg class to R2 vals
vals <- as.data.frame(vals)
colnames(vals) <- c("site", "r")
vals <- merge(vegclass, vals, by="site")
vals$siteid <- substr(vals$site, 4,6)
vals$r <- as.numeric(vals$r)


#ID training/testing sites
vals$traintest <- rep("train", length=nrow(vals))
#reassigning testing site values 
vals$traintest[vals$siteid %in% c("fuf", "scw", "so3", "srg", "vcm")] <- "test"
vals$pattern[vals$traintes == "train"] <- "none"
vals$pattern[vals$traintes == "test"] <- "stripe"
vals$angle[vals$traintes == "train"] <- 0
vals$angle[vals$traintes == "test"] <- 45

#####MAKE THE PLOT#####
#Our transformation function to limit the axis labels to 2 decimal places
scaleFUN <- function(x) sprintf("%.2f", x)

#plot for correct bars in graph
#for the correct legend see "Z:/SRER/Martha/DryFlux/R2graphlegend.ppt"
lbl = "Fluxcom:Observed GPP" 

p <- ggplot(data=vals, aes(x=reorder(siteid, MAP.y), y=r, fill=VegClass)) + #label the x-axis according to site but order according to MAP from low to high
  geom_bar_pattern(stat="identity",
                   pattern= vals$pattern, 
                   pattern_angle = vals$angle,
                   pattern_density = .07,
                   pattern_spacing = .01,
                   pattern_fill = 'black') + 
  scale_fill_manual(values=c("#FF6666", "#FFD25A", "#57737A")) +
  theme_classic() +
  labs(x = "Site", y = expression("R"^2)) +
  scale_y_continuous(labels=scaleFUN) + 
  geom_hline(yintercept=0.9, linetype="dashed") + #add horizontal dashed line at 0.9
  labs(fill = "Veg Class") + #change legend title+
  theme(legend.position ="none")+
  annotate("text", label = lbl, parse=FALSE, x = 6, y = 1.1, size = 6, colour = "Black")

plot(p)
ggsave("C:/Users/farellam/Documents/DryFlux/plots/Observed_Fluxcom_r2.png", device='png', width=17, height=10, units="cm", plot=p, dpi = 300)

#############################################################################################################
################3. Compare DryFlux, Fluxcom, MODIS performance to observed GPP REGRESSIONS (Fig 1c)###################
#############################################################################################################

setwd("C:/Users/farellam/Documents/DryFlux/data")
#load the data
graphdata <- read.csv("coarseDFPred.csv")


graphdata$site <- substr(graphdata$sitedate, 1,6)

#define equation to calculate linear equation
lm_eqn = function(m) {
  l <- list(a = as.numeric(format(coef(m)[1], digits = 2)),
            b = as.numeric(format(abs(coef(m)[2]), digits = 2)),
            r2 = as.numeric(format(summary(m)$r.squared, digits = 2)));
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(R)^2~"="~r2,l)    
  }
  as.character(as.expression(eq));                 
}

#reformat data for plotting wtih ggplot
plotdat <- graphdata[,c(2,3,4)]
names(plotdat)
colnames(plotdat) <- c("sitedate", "GPP", "other")
plotdat$type <- rep("MODIS", nrow(plotdat))
plotdat2 <- graphdata[,c(2,3,6)]
names(plotdat2)
colnames(plotdat2) <- c("sitedate", "GPP", "other")
plotdat2$type <- rep("DryFlux", nrow(plotdat))

plotdata <- rbind(plotdat, plotdat2)

plotdat2 <- graphdata[,c(2,3,5)]
names(plotdat2)
colnames(plotdat2) <- c("sitedate", "GPP", "other")
plotdat2$type <- rep("Fluxcom", nrow(plotdat))

plotdata <- rbind(plotdata, plotdat2)

plotdata$type <- as.factor(plotdata$type)
summary(plotdata$type)

#make the plot
plot1 <- ggplot(plotdata, aes(x=GPP, y=other, col=type))+
  xlim(0,10)+ylim(0,10)+
  
  geom_point()+
  geom_smooth(method=lm,se=FALSE) +
  scale_color_manual(values=c('#5D576B','#639FAB', '#70AE6E'))+
  theme(legend.position = c(0.9, 0.88),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  geom_abline(intercept=0,slope=1)+
  xlab("observed GPP")+
  ylab("modeled GPP")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  #geom_rect(aes(xmin = -0.5, xmax = 7, ymin = 8.5, ymax = 10.5), fill="white", alpha = 0.2)+
  annotate("text", x = 2.5, y = 10, colour = "#5D576B", size = 3.5,
           label = lm_eqn(lm(coarse_GPP ~ ObservedGPP, graphdata)), parse = TRUE)+
  annotate("text", x = 2.5, y = 9.5, colour = "#639FAB", size = 3.5,
           label = lm_eqn(lm(FluxcomGPP ~ ObservedGPP, graphdata)), parse = TRUE)+
  annotate("text", x = 2.5, y = 9, colour = "#70AE6E", size = 3.5,
           label = lm_eqn(lm(MODIS_GPP ~ ObservedGPP, graphdata)), parse = TRUE)
#theme_few(base_size=14)
plot1
getwd()
ggsave("C:/Users/farellam/Documents/DryFlux/plots/RegressionFig1c.png", device='png', width=4, height=4, plot=plot1, dpi = 300, units="in")


#############################################################################################################
######################4. Compare DryFlux, Fluxcom, MODIS, and observed GPP variance (Fig 2a)#########################
#############################################################################################################
setwd("c:/Users/farellam/Documents/DryFlux/data")
#load the data
data2 <- read.csv("coarseDFPred.csv")

data2$site <- substr(data2$sitedate, 1,6)
data2$month <- substr(data2$sitedate, 8,10)

##!!!#DEPENDING ON TIME  FRAME WILL NEED TO SUBSET BY MONTH HERE, then change graph title and graph output name
data2 <- subset(data2, month %in% c("Jun", "Jul", "Aug"))
#graph title parameter
lbl <- "Summer Months (Jun, Jul, Aug)"
#graph output name
outputname <- "c:/Users/farellam/Documents/DryFlux/plots/sd_SummerMos_noecohyrdo.png"

#define function to get sdvals for each GPP estimate
sdfxn <- function(xx){
  modis.sd = sd(xx$MODIS_GPP)
  fluxcom.sd = sd(xx$FluxcomGPP)
  dryflux.sd = sd(xx$coarse_GPP)
  obs.sd = sd(xx$ObservedGPP)
  return(data.frame(modissd = modis.sd, fluxcomsd = fluxcom.sd, dryfluxsd = dryflux.sd, obssd = obs.sd))
}

sites <- unique(as.factor(data2$site))

#create empty matrix to store output
sdvals <- matrix(nrow=length(sites), ncol=5)

#apply std dev function to each site; calculate mean average std vals vals
row <- 0
for(i in sites){
  site_i <- print(i)
  y <- subset(data2, site == site_i)
  sd_site <- sdfxn(y)
  sdvals[row+1,1] <- site_i
  sdvals[row+1,2] <- mean(sd_site$modissd, na.rm=TRUE)
  sdvals[row+1,3] <- mean(sd_site$fluxcomsd, na.rm=TRUE)
  sdvals[row+1,4] <- mean(sd_site$dryfluxsd, na.rm=TRUE)
  sdvals[row+1,5] <- mean(sd_site$obssd, na.rm=TRUE)
  row <- row+1
}

#restructure dataframe
sdvals <- as.data.frame(sdvals)
colnames(sdvals) <- c("site", "modissd", "fluxcomsd", "dryfluxsd", "obssd")

#add veg class
#load site meta data
meta <- read.csv("site_meta.csv")
vegclass <- meta[,c(2,5)]
#add veg class to GPP vals
final <- merge(vegclass, sdvals, by="site")

#restructure data for grouped barchart
graphdata <- final[,c(1,2,6)]
colnames(graphdata) <- c("site", "vegclass", "sd")
graphdata$sd_type <- rep("Observed", length(nrow(graphdata)))
#dryflux std dev vals
temp <- final[,c(1,2,5)]
colnames(temp) <- c("site", "vegclass", "sd")
temp$sd_type <- rep("DryFlux", length(nrow(temp)))
graphdata <- rbind(graphdata, temp)
#modis std dev vals
temp <- final[,c(1,2,3)]
colnames(temp) <- c("site", "vegclass", "sd")
temp$sd_type <- rep("MODIS", length(nrow(temp)))
graphdata <- rbind(graphdata, temp)
#Fluxcom std dev vals
temp <- final[,c(1,2,4)]
colnames(temp) <- c("site", "vegclass", "sd")
temp$sd_type <- rep("Fluxcom", length(nrow(temp)))
graphdata <- rbind(graphdata, temp)

graphdata$sd <- as.numeric(graphdata$sd)
gdata <- aggregate(sd~ vegclass + sd_type, data=graphdata, mean)


# Change ordering manually
gdata$sd_type <- factor(gdata$sd_type,                                    
                        levels = c("Observed", "DryFlux", "MODIS", "Fluxcom"))


#graphing parameters
xlabs <- c("Forest", "Grasslands", "Savanna/Shrublands")
#plot
q <- ggplot(data=gdata, aes(x=vegclass, y=sd, fill=sd_type)) +
  geom_bar(position='dodge', stat='identity')+
  scale_fill_manual(values=c("#C57b57", "#5D576B", "#70AE6E", "#639FAB"),
                    name="GPP Source")+
  theme_classic()+
  scale_x_discrete(labels= labs)+
  labs(x = "Land Cover", y = "Std Dev")+
  theme(legend.position = c(0.85,0.90))   +
  theme(legend.title = element_blank()) +
  #theme(legend.background = element_rect(size=.5, linetype=1, color=1))+
  #theme(legend.position = "none")   +
  ggtitle(lbl) +
  theme(plot.title = element_text(size=11, face="bold"))

plot(q)

ggsave(outputname, device='png', width=15, height=10, plot=q, dpi = 300, units="cm")


#############################################################################################################
########################################5. Interannual Variability (Fig 2b)###########################################
#############################################################################################################
library(ggplot2)
library(ggthemes)
library(scales)
library(psych)
library(caret)


setwd("C:/Users/farellam/Documents/DryFlux/data/")
#load the data
graphdata <- read.csv("coarseDFPred.csv")


graphdata$site <- substr(graphdata$sitedate, 1,6)
graphdata$month <- substr(graphdata$sitedate, 8,10)
graphdata$year <- substr(graphdata$sitedate, 12,15)

y <- graphdata
#exclude 2015 since it only has observations for Jan
y$yearnum <- as.numeric(y$year)
y <- subset(y, yearnum < 2015)

  x <- ddply(y, .(year), summarize, coarse_se=sd(coarse_GPP, na.rm=TRUE)/sqrt(length(coarse_GPP[!is.na(coarse_GPP)])) , coarse_GPP=mean(coarse_GPP, na.rm=TRUE),
             GPP_se=sd(ObservedGPP, na.rm=TRUE)/sqrt(length(ObservedGPP[!is.na(ObservedGPP)])), GPP=mean(ObservedGPP, na.rm=TRUE),
             MODIS_se=sd(MODIS_GPP, na.rm=TRUE)/sqrt(length(MODIS_GPP[!is.na(MODIS_GPP)])), MODIS_GPP=mean(MODIS_GPP, na.rm=TRUE),
             #fine_se=sd(fine_GPP, na.rm=TRUE)/sqrt(length(fine_GPP[!is.na(fine_GPP)])), fine_GPP=mean(fine_GPP, na.rm=TRUE),
             Fluxcom_se=sd(FluxcomGPP, na.rm=TRUE)/sqrt(length(FluxcomGPP[!is.na(FluxcomGPP)])), FluxcomGPP=mean(FluxcomGPP, na.rm=TRUE),
             lngth= length(ObservedGPP))
  
  r_coarse <- as.character(round(cor(y$ObservedGPP, y$coarse_GPP), 3))
  r_modis <- as.character(round(cor(y$ObservedGPP, y$MODIS_GPP), 3))
  #r_fine <- as.character(round(cor(x$GPP, x$fine_GPP), 3))
  r_fluxcom <- as.character(round(cor(y$ObservedGPP, y$FluxcomGPP), 3))
  
  rmssdGPP <- as.character(round(rmssd(y$ObservedGPP), 3))
  rmssdcoarse_GPP <- as.character(round(rmssd(y$coarse_GPP), 3))
  #rmssdfine_GPP <- as.character(round(rmssd(x$fine_GPP), 3))
  rmssdMODIS_GPP <- as.character(round(rmssd(y$MODIS_GPP), 3))
  rmssdFluxcom_GPP <- as.character(round(rmssd(y$FluxcomGPP), 3))
  
  rmsecoarse_GPP <- as.character(round(RMSE(y$coarse_GPP, y$ObservedGPP), 3))
  rmseMODIS_GPP <- as.character(round(RMSE(y$MODIS_GPP, y$ObservedGPP), 3))
  #rmsefine_GPP <- as.character(round(RMSE(x$fine_GPP, x$GPP), 3))
  rmseFluxcom_GPP <- as.character(round(RMSE(y$FluxcomGPP, y$ObservedGPP), 3))
  
  lbl1 <- paste("Observed: RMSSD =", rmssdGPP)
  lbl2 <- paste("DryFlux: rmse =", rmsecoarse_GPP, "; r =", r_coarse, "; RMSSD =", rmssdcoarse_GPP)
  #lbl3 <- paste("Fine DF: rmse =", rmsefine_GPP, "; r =", r_fine, "; RMSSD =", rmssdfine_GPP)
  lbl4 <- paste("MODIS: rmse =", rmseMODIS_GPP, "; r =", r_modis, "; RMSSD =", rmssdMODIS_GPP)
  lbl5 <- paste("Fluxcom: rmse=", rmseFluxcom_GPP, "; r =", r_fluxcom, ";RMSSD =", rmssdFluxcom_GPP)
  
  lbl6 <- "All Sites"
  
  filename <- paste(lbl6, "interannual_comparison.png", sep="_")
  print(filename)
  q <- ggplot() +
    ylim(0,2)+
    
    geom_line(data = x, aes(x = year, group=1, y = MODIS_GPP, color = I("#70AE6E")), size=2) +
    geom_line(data = x, aes(x = year, group=1, y = FluxcomGPP, color = I("#639FAB")), size=2) +
    geom_line(data = x, aes(x = year, group=1, y = GPP, color =I("#C57B57")), size=2) +
    geom_line(data = x, aes(x = year, group=1, y = coarse_GPP, color = I("#5D576B")), size=2) +
    #geom_line(data = x, aes(x = year, group=1, y = fine_GPP, color = I("#FFD447")), size=2) +
    
    
    geom_errorbar(data=x,aes(x=year, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="#C57B57")+
    geom_errorbar(data=x,aes(x=year, ymin=coarse_GPP-coarse_se,ymax=coarse_GPP+coarse_se),colour="#5D576B")+
    #geom_errorbar(data=x,aes(x=year, ymin=fine_GPP-fine_se,ymax=fine_GPP+fine_se),colour="#FFD447")+
    geom_errorbar(data=x,aes(x=year, ymin=MODIS_GPP-MODIS_se,ymax=MODIS_GPP+MODIS_se),colour="#70AE6E")+
    geom_errorbar(data=x,aes(x=year, ymin=FluxcomGPP-Fluxcom_se,ymax=FluxcomGPP+Fluxcom_se),colour="#639FAB")+
    
    annotate("text", label = lbl1, parse=FALSE, x = 12.1, y = 2, size = 3.5, colour = "#C57B57")+
    annotate("text", label = lbl2, parse=FALSE, x = 10, y = 1.9, size = 3.5, colour = "#5D576B")+
    #annotate("text", label = lbl3, parse=FALSE, x = 5.3, y = 1.8, size = 3.5, colour = "#FFD447")+
    annotate("text", label = lbl4, parse=FALSE, x = 10.1, y = 1.8, size = 3.5, colour = "#70AE6E")+
    annotate("text", label = lbl5, parse=FALSE, x = 10.1, y = 1.7, size = 3.5, colour = "#639FAB")+
    
    ggtitle(lbl6)+
    scale_x_discrete(breaks=seq(2000,2014, by=1), labels=c("2000", "","2002","","2004","","2006","","2008","","2010","","2012","","2014"))+
    xlab('year')+
    ylab(expression('Average Annual GPP (gC/m'^2 ' /day)'))+
    theme_classic()+
    theme(legend.position = c(0, 0))
  plot(q)
  ggsave(paste("C:/Users/farellam/Documents/DryFlux/plots/interannual_avg_",filename,sep=""), device='png', width=15, height=10, plot=q, dpi = 300, units="cm")
  
#############################################################################################################
###############################################6. Total GPP (Fig S2 & S5)##################################################
#############################################################################################################
setwd("C:/Users/farellam/Documents/DryFlux/data")
#load the data
graphdata <- read.csv("coarseDFPred.csv")
  
graphdata$site <- substr(graphdata$sitedate, 1,6)
graphdata$month <- substr(graphdata$sitedate, 8,10)
graphdata$year <- substr(graphdata$sitedate, 12,15)

#load site meta data
meta <- read.csv("C:/Users/farellam/Documents/DryFlux/data/site_meta.csv")
veg <- meta[,c(2,5)]

final <- merge(graphdata, veg, by="site")
final$VegClass[final$VegClass =="Savanna/Shrublands"] <- "Savanna"

#split the data frame and create the charts
out2 <- split(final, final$site)#split by site
out2 <- split(final, final$VegClass) #split by veg class


plot_total <- function(y){
  sumdata=data.frame(value=apply(y[,c(4:7)],2,sum))
  sumdata$key=rownames(sumdata)
  sumdata$key <- factor(sumdata$key,                                    # Change ordering manually
                        levels = c("ObservedGPP", "coarse_GPP", "MODIS_GPP", "FluxcomGPP"))
  filename <- paste(y$site[1], "totalGPP.png", sep="_") #output name based on siteID
  #filename <- paste(y$VegClass[1], "totalGPP.png", sep="_")#output name based on Veg Class
  #filename <- "AllSites_totalGPP.png"#output name for all sites
  print(filename)
  labs <- c("Observed", "DryFlux", "MODIS", "Fluxcom")
  lbl1 <- y$site[1] #title based on site
  #lbl1 <- y$VegClass[1] #title based on Veg Class
  q <- ggplot(data=sumdata, aes(x=key, y=value, fill=key)) +
    geom_bar(stat="identity")+
    scale_fill_manual(values=c("#C57b57", "#5D576B", "#70AE6E", "#639FAB"))+
    theme_classic()+
    scale_x_discrete(labels= labs)+
    labs(x = "Source", y = paste("Total Monthly GPP ", min(y$year), " - ", max(y$year), sep=""))+
    theme(legend.position = "none")   +
    ggtitle(lbl1) +
    #annotate("text", label = lbl2, parse=FALSE, x = 4, y = max(sumdata$value)*1.25, size = 3.5, colour = "Black")+
    theme(plot.title = element_text(size=11, face="bold"))
  plot(q)
  ggsave(filename, device='png', width=10, height=10, plot=q, dpi = 300, units="cm")
  
}

setwd("C:/Users/farellam/Documents/DryFlux/plots/totalGPP")
lapply(out2, plot_total)

#############################################################################################################
#########################################7. GPP & precip z-scores (Fig S6)###################################
#############################################################################################################
setwd("C:/Users/farellam/Documents/DryFlux/data/")
#load the data
#get flux tower data and format correctly
data <- read.csv("RFdatanew.csv")

#subset only sitedate, GPP, & precip vals
graphdata <- data[,c(2,4,9)]

#make site, year, and moyr column
graphdata$site <- substr(graphdata$sitedate, 1,6)
graphdata$year <- substr(graphdata$sitedate, 12,15)
graphdata$moyr <- substr(graphdata$sitedate, 8, 15)

#split the data by site
out2 <- split(graphdata, graphdata$site)

####Z-score by site by year
plot_zscore <- function(y){
  require("ggplot2")
  require("ggthemes")
  require("scales")
  require("psych")
  x <- ddply(y, .(year), summarize, GPPz = (GPP- mean(GPP))/sd(GPP),
             coarse_z = (precip-mean(precip))/sd(precip),
             moyr = moyr)
  
  x$moyr <- as.Date(paste0("01-", x$moyr), format='%d-%b-%Y')           
  x$coarse_z[is.nan(x$coarse_z)]<-0
  lbl <- y$site[1]
  lastyr <- as.Date(max(x$moyr))-4
  filename <- paste(lbl, "zscore.png", sep="_") 
  print(filename)
  q <- ggplot() +
    ylim(-3.2,3.2)+
    geom_line(data = x, aes(x = moyr, group=1, y = GPPz, color ="a"), size=2) +
    #geom_line(data = x, aes(x = moyr, group=1, y = fine_z, color ="b"), size=2) +
    geom_line(data = x, aes(x = moyr, group=1, y = coarse_z, color ="c"), size=2) +
    scale_color_manual(name="colors", values = c("a"="#C57B57", "c"="#035E7B"), labels=c("GPP", "precip"))+
    #geom_line(data = x, aes(x = moyr, group=1, y = GPPz, color =I("#C57B57")), size=2) +
    #geom_line(data = x, aes(x = moyr, group=1, y = fine_z, color =I("#7DBBC3")), size=2) +
    #geom_line(data = x, aes(x = moyr, group=1, y = coarse_z, color =I("#035E7B")), size=2) +
    
    
    theme_classic()+
    xlab('year')+
    ylab('Annual z-score')+
    geom_hline(yintercept=0, linetype="dashed")+
    theme(legend.position = "none")+
    annotate("text", label = lbl, parse=FALSE, x = lastyr, y = 3, size = 4, colour = "Black")
  
  plot(q)
  ggsave(filename, device='png', width=15, height=10, plot=q, dpi = 300, units="cm")
}

setwd("C:/Users/farellam/Documents/DryFlux/plots/zscore/monthly")
lapply(out2, plot_zscore)



#############################################################################################################
##########8. Compare DryFlux, MODIS, and Fluxcom performance to observed GPP (Table S2 and S5)###############
#############################################################################################################

library(plyr)

setwd("C:/Users/farellam/Documents/DryFlux/data/re-do")
coarsePred <- read.csv("ausDFPred.csv")
#finePred <- read.csv("fineDFPred.csv")

final <- coarsePred
final$ObservedGPP[final$ObservedGPP == -9999] <- NA
final$ObservedGPP[final$ObservedGPP < -1000] <- NA
final <- final[complete.cases(final), ]



final$site <- substr(final$sitedate, 1,6)
final$year <- substr(final$sitedate, 12,16)


#define function to get correlation values
corfunc <- function(xx){
  return(data.frame(DryFluxR = cor(xx$coarse_GPP, xx$ObservedGPP),
                    DryFluxRMSE = sqrt(mean((xx$coarse_GPP - xx$ObservedGPP)^2)),
                    
                    MODISR = cor(xx$MODIS_GPP, xx$ObservedGPP),
                    MODISRMSE = sqrt(mean((xx$MODIS_GPP - xx$ObservedGPP)^2)),
                    
                    FluxcomR = cor(xx$FluxcomGPP, xx$ObservedGP),
                    FluxcomRMSE = sqrt(mean((xx$FluxcomGPP - xx$ObservedGPP)^2)),
                    
                    N = nrow(xx)
                    ))
}

vals <- ddply(final, .(site), corfunc)

write.csv(vals, "C:/Users/farellam/Documents/DryFlux/aus_performance.csv")
