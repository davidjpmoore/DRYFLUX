##TAKE DAILY FLUXTOWER VALUES FROM MATLAB FILES, CONVERT WATER YEAR INTO A NORMAL DATE, DEFINE MID-MONTH DURATIONS, CALCULATE MID-MONTHLY GPP VALUES


library(R.matlab)
library(lubridate)
library(plyr)
library(dplyr)

getwd()
setwd("C:/Users/farellam/Documents/DryFlux/data/Flux_raw/")
fluxlist <-list.files(pattern= "^.*\\.(mat)$")

#define function to take the flux MatLab file, separate data into mid-month delineations and calculate GPP for each month
format_flux <- function(file){
  filename<-paste("us", substr(file, 7,9), sep="-")
  filename <- paste(filename, ".csv", sep="")
  file <- readMat(file)
  print(filename)
  data = lapply(file, unlist, use.names=FALSE)
  df <- as.data.frame(data)
  names(df) <- c("year", "dohy", "NEP", "GEP", "Reco", "ET", "Precip", "Tair", "VPD", "Rnet", "Rsolar")
  
  #for cop need to subtract 3 from 'dohy' to get the correct value
  #df$dohy <- df$dohy-3
  #for src need to remove the NA rows at the bottom of the dataframe
  #df <- df[-c(1923:2192),]
  
  #deal with 'water year' delinations to get normal date 
  df$datewy <- strptime(paste(df$dohy, df$year, sep="-"), format="%j-%Y")
  #Substract 5256000 because that's the number of seconds in 2 months (water year - Nov 1) 
  df$datewy <- as.Date(df$datewy - 5256000)
  df$date <- as.Date(df$datewy)
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
  jandates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), Jan, day_num))
  febdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), Feb, day_num))
  mardates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), Mar, day_num))
  aprdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), Apr, day_num))
  maydates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), May, day_num))
  jundates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), Jun, day_num))
  juldates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), Jul, day_num))
  augdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), Aug, day_num))
  sepdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), Sep, day_num))
  octdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), Oct, day_num))
  novdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), Nov, day_num))
  decdates <- as.Date(sprintf("%d-%02d-%02d", do.call(seq, as.list(as.numeric(range(format(df$date, "%Y"))))), Dec, day_num))
  
  #subset data set based on month delinations
  Jan <- df[df$date %in% outer(jandates, -bound:bound, `+`),]
  Jan$month <- "Jan"
  Jan$year2 <- ifelse(substr(Jan$date, 9,10) >= 17, as.numeric(substr(Jan$date, 1,4)) +1, substr(Jan$date, 1,4))

  Feb <- df[df$date %in% outer(febdates, -bound:bound, `+`),]
  Feb$month <- "Feb"
  Feb$year2 <- substr(Feb$date, 1,4)
  
  Mar <- df[df$date %in% outer(mardates, -bound:bound, `+`),]
  Mar$month <- "Mar"
  Mar$year2 <- substr(Mar$date, 1,4)
  
  Apr <- df[df$date %in% outer(aprdates, -bound:bound, `+`),]
  Apr$month <- "Apr"
  Apr$year2 <- substr(Apr$date, 1,4)
  
  May <- df[df$date %in% outer(maydates, -bound:bound, `+`),]
  May$month <- "May"
  May$year2 <- substr(May$date, 1,4)
  
  Jun <- df[df$date %in% outer(jundates, -bound:bound, `+`),]
  Jun$month <- "Jun"
  Jun$year2 <- substr(Jun$date, 1,4)
  
  Jul <- df[df$date %in% outer(juldates, -bound:bound, `+`),]
  Jul$month <- "Jul"
  Jul$year2 <- substr(Jul$date, 1,4)
  
  Aug <- df[df$date %in% outer(augdates, -bound:bound, `+`),]
  Aug$month <- "Aug"
  Aug$year2 <- substr(Aug$date, 1,4)
  
  Sep <- df[df$date %in% outer(sepdates, -bound:bound, `+`),]
  Sep$month <- "Sep"
  Sep$year2 <- substr(Sep$date, 1,4)
  
  Oct <- df[df$date %in% outer(octdates, -bound:bound, `+`),]
  Oct$month <- "Oct"
  Oct$year2 <- substr(Oct$date, 1,4)
  
  Nov <- df[df$date %in% outer(novdates, -bound:bound, `+`),]
  Nov$month <- "Nov"
  Nov$year2 <- substr(Nov$date, 1,4)
  
  Dec <- df[df$date %in% outer(decdates, -bound:bound, `+`),]
  Dec$month <- "Dec"
  Dec$year2 <- substr(Dec$date, 1,4)
  
  df <- rbind.data.frame(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
  
  #define month year column
  df$monthyear <- paste(df$month, substr(df$year2, 1, 4), sep="-")
  
  #Get Monthly GPP (mean)
  cleaned_df <- ddply(df, .(monthyear), summarize, GPP=mean(GEP, na.rm=TRUE), 
                      Obsprecip=sum(Precip, na.rm=TRUE))
  cleaned_df$GPP[cleaned_df$GPP == 0] <- NA
  cleaned_df <- cleaned_df[complete.cases(cleaned_df),]
  cleaned_df$date <- as.Date(paste("01-", cleaned_df$monthyear, sep=""), format= "%d-%b-%Y")
  cleaned_df$site <- substr(filename, 1,6)
  cleaned_df <- arrange(cleaned_df, date)
  cleaned_df$sitedate <- paste(cleaned_df$site, cleaned_df$monthyear, sep="-")
  cleaned_df <- cleaned_df[,c(6,2,3)]
  return(cleaned_df)}

#cop and src produce errors so remove them from the list of sites.
cop <- fluxlist[2]
src <- fluxlist[17]
fluxlist <- fluxlist[-c(2,17)]
fluxlist

#apply the format_flux function to all of the flux matlab files
final_gpp <- lapply(fluxlist, format_flux) %>% bind_rows()

#after code is run individually for cop and src
final_gpp <- rbind.data.frame(final_gpp, cop)
final_gpp <- rbind.data.frame(final_gpp, src)

write.csv(final_gpp, "C:/Users/farellam/Documents/DryFlux/data/Flux_GPPprecip.csv")
