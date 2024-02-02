###PART 1: take all of the Australia OzFlux site data (downloaded from https://fluxnet.org/data/download-data/), subset just the timestamp and desired GPP variable (GPP_NT_VUT_MEAN), determine day of year, write output for each site
###PART 2: Clean up the GPP data so that there's a single (mean) observation for each monthyear at each site; define the monthyear based on mid-month values; combine all sites into a single data set


library(lubridate)
library(dplyr)
library(plyr)

############################################################################
###################################PART 1###################################
############################################################################
setwd("C:/Users/farellam/Documents/DryFlux/data/OzFlux/")
###Get GPP and date values for each site###
oz_files <- list.files(pattern= "*FULLSET_DD.*\\.csv")
oz_files

format_GPP <- function(x){
  filename<- substr(x, 5,10)
  filename <- paste(filename, ".csv", sep="")
  oz <- read.csv(x)
  gpp <- oz[,c("TIMESTAMP", "GPP_NT_VUT_MEAN")]
  gpp$year <- substr(gpp$TIMESTAMP, 1,4)
  gpp$mo <- substr(gpp$TIMESTAMP, 5,6)
  gpp$day <- substr(gpp$TIMESTAMP, 7,8)
  gpp$date <- paste(gpp$year, gpp$mo, gpp$day, sep="-")
  gpp$dohy <- yday(gpp$date)
  final <- gpp[,c(3,7,2)]
  colnames(final) <- c("year", "dohy", "GPP")
  write.csv(final, file= paste("./site_GPP/",filename, sep=""))
  return(final)
}

#CAREFUL this overwrites everything
lapply(oz_files, format_GPP)


      #need to do BW_Nxr separately because it's FLUXNET-CH4 data
      y <- "FLX_BW-Nxr_FLUXNET-CH4_DD_2018-2018_1-1.csv"
      filename<- substr(y, 5,10)
      filename <- paste(filename, ".csv", sep="")
      oz <- read.csv(y)
      gpp <- oz[,c("TIMESTAMP", "GPP_NT")]
      gpp$year <- substr(gpp$TIMESTAMP, 1,4)
      gpp$mo <- substr(gpp$TIMESTAMP, 5,6)
      gpp$day <- substr(gpp$TIMESTAMP, 7,8)
      gpp$date <- paste(gpp$year, gpp$mo, gpp$day, sep="-")
      gpp$dohy <- yday(gpp$date)
      final <- gpp[,c(3,7,2)]
      colnames(final) <- c("year", "dohy", "GPP")
      write.csv(final, file= paste("./site_GPP/",filename, sep=""))

############################################################################
###################################PART 2###################################
############################################################################
###Clean up the GPP data so that there's a single (mean) observations for each monthyear at each site

setwd("C:/Users/farellam/Documents/DryFlux/data/OzFlux/site_GPP/")
gpp_files <- list.files(pattern=".csv")

###
format_flux <- function(file){
  
  df <- read.csv(file)
  print(file)
  df$datewy <- strptime(paste(df$dohy, df$year, sep="-"), format="%j-%Y")
  df$datewy <- as.Date(df$datewy)
  df$month <- rep(NA, nrow=nrow(df))
  df$year2 <- rep(NA, nrow=nrow(df))
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
  jandates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), Jan, day_num))
  febdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), Feb, day_num))
  mardates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), Mar, day_num))
  aprdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), Apr, day_num))
  maydates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), May, day_num))
  jundates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), Jun, day_num))
  juldates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), Jul, day_num))
  augdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), Aug, day_num))
  sepdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), Sep, day_num))
  octdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), Oct, day_num))
  novdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), Nov, day_num))
  decdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$datewy, "%Y"))))), Dec, day_num))
  
  #subset data set based on month delinations
  Jan <- df[df$datewy %in% outer(jandates, -bound:bound, `+`),]
  Jan$month <- "Jan"
  Jan$year2 <- ifelse(Jan$dohy > 350, Jan$year+1, Jan$year)
  
  Feb <- df[df$datewy %in% outer(febdates, -bound:bound, `+`),]
  Feb$month <- "Feb"
  Feb$year2 <- Feb$year
  
  Mar <- df[df$datewy %in% outer(mardates, -bound:bound, `+`),]
  Mar$month <- "Mar"
  Mar$year2 <- Mar$year
  
  Apr <- df[df$datewy %in% outer(aprdates, -bound:bound, `+`),]
  Apr$month <- "Apr"
  Apr$year2 <- Apr$year
  
  May <- df[df$datewy %in% outer(maydates, -bound:bound, `+`),]
  May$month <- "May"
  May$year2 <- May$year
  
  Jun <- df[df$datewy %in% outer(jundates, -bound:bound, `+`),]
  Jun$month <- "Jun"
  Jun$year2 <- Jun$year
  
  Jul <- df[df$datewy %in% outer(juldates, -bound:bound, `+`),]
  Jul$month <- "Jul"
  Jul$year2 <- Jul$year
  
  Aug <- df[df$datewy %in% outer(augdates, -bound:bound, `+`),]
  Aug$month <- "Aug"
  Aug$year2 <- Aug$year
  
  Sep <- df[df$datewy %in% outer(sepdates, -bound:bound, `+`),]
  Sep$month <- "Sep"
  Sep$year2 <- Sep$year
  
  Oct <- df[df$datewy %in% outer(octdates, -bound:bound, `+`),]
  Oct$month <- "Oct"
  Oct$year2 <- Oct$year
  
  Nov <- df[df$datewy %in% outer(novdates, -bound:bound, `+`),]
  Nov$month <- "Nov"
  Nov$year2 <- Nov$year
  
  Dec <- df[df$datewy %in% outer(decdates, -bound:bound, `+`),]
  Dec$month <- "Dec"
  Dec$year2 <- Dec$year
  
  df <- rbind.data.frame(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
  
  #define month year column
  df$monthyear <- paste(df$month, substr(df$year2, 3, 4), sep="-")
  
  #Get Monthly GPP (mean)
  cleaned_df <- ddply(df, .(monthyear), summarize, GPP=mean(GPP, na.rm=TRUE))
  
  #create 'site' column
  cleaned_df$site <- substr(file, 1,6)
  
  cleaned_df$date <- as.Date(paste("01-", cleaned_df$monthyear, sep=""), format= "%d-%b-%Y")
  cleaned_df <- arrange(cleaned_df, date)
  cleaned_df <- cleaned_df[,-4]
  
  return(cleaned_df)}

#apply the format_flux function to all of the OzFlux GPP datasets
final_gpp <- lapply(gpp_files, format_flux) %>% bind_rows()

#write.csv(final_gpp, "C:/Users/farellam/Documents/DryFlux/data/OzFlux_GPP.csv")

