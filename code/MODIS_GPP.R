##TAKE GPP OUTPUTS FROM GOOGLE EARTH ENGINE, SELECT GPP OBSERVATION FROM DATE THAT MATCHES UP WITH NDVI/EVI DATES, FORMAT DATA FOR DOWNSTREAM ANALYSIS

library(lubridate)
setwd("C:/Users/farellam/Documents/DryFlux/data/MOD17A2H/Ozsites")
#setwd("D:/martha/DryFlux/data/MOD17A2H")

files <- list.files(pattern="*.csv")

#define function to subset GPP dates, format dates, and add site column
GPP_format <- function(x){
  data <- read.csv(x)
  ######
  #Subset by dates the other MODIS (EVI and NDVI) data prodcuts were collected
  dates <- subset(data, system.time_start %in% c(
    "Feb 18, 2000", "Mar 21, 2000", "Apr 22, 2000",  "May 8, 2000", "Jun 9, 2000",  
  "Jul 11, 2000", "Aug 12, 2000", "Sep 13, 2000", "Oct 15, 2000", "Nov 16, 2000", "Dec 18, 2000",
  "Jan 17, 2001", "Feb 18, 2001", "Mar 22, 2001", "Apr 23, 2001",  "May 9, 2001", "Jun 10, 2001",  
  "Jul 12, 2001", "Aug 13, 2001", "Sep 14, 2001", "Oct 16, 2001", "Nov 17, 2001", "Dec 19, 2001",
  "Jan 17, 2002", "Feb 18, 2002", "Mar 22, 2002", "Apr 23, 2002",  "May 9, 2002", "Jun 10, 2002",  
  "Jul 12, 2002", "Aug 13, 2002", "Sep 14, 2002", "Oct 16, 2002", "Nov 17, 2002", "Dec 19, 2002",
  "Jan 17, 2003", "Feb 18, 2003", "Mar 22, 2003", "Apr 23, 2003",  "May 9, 2003", "Jun 10, 2003",  
  "Jul 12, 2003", "Aug 13, 2003", "Sep 14, 2003", "Oct 16, 2003", "Nov 17, 2003", "Dec 19, 2003",
  "Jan 17, 2004", "Feb 18, 2004", "Mar 21, 2004", "Apr 22, 2004",  "May 8, 2004", "Jun 9, 2004",  
  "Jul 11, 2004", "Aug 12, 2004", "Sep 13, 2004", "Oct 15, 2004", "Nov 16, 2004", "Dec 18, 2004",
  "Jan 17, 2005", "Feb 18, 2005", "Mar 22, 2005", "Apr 23, 2005",  "May 9, 2005", "Jun 10, 2005",  
  "Jul 12, 2005", "Aug 13, 2005", "Sep 14, 2005", "Oct 16, 2005", "Nov 17, 2005", "Dec 19, 2005",
  "Jan 17, 2006", "Feb 18, 2006", "Mar 22, 2006", "Apr 23, 2006",  "May 9, 2006", "Jun 10, 2006",  
  "Jul 12, 2006", "Aug 13, 2006", "Sep 14, 2006", "Oct 16, 2006", "Nov 17, 2006", "Dec 19, 2006",
  "Jan 17, 2007", "Feb 18, 2007", "Mar 22, 2007", "Apr 23, 2007",  "May 9, 2007", "Jun 10, 2007",  
  "Jul 12, 2007", "Aug 13, 2007", "Sep 14, 2007", "Oct 16, 2007", "Nov 17, 2007", "Dec 19, 2007",
  "Jan 17, 2008", "Feb 18, 2008", "Mar 21, 2008", "Apr 22, 2008",  "May 8, 2008", "Jun 9, 2008",  
  "Jul 11, 2008", "Aug 12, 2008", "Sep 13, 2008", "Oct 15, 2008", "Nov 16, 2008", "Dec 18, 2008",
  "Jan 17, 2009", "Feb 18, 2009", "Mar 22, 2009", "Apr 23, 2009",  "May 9, 2009", "Jun 10, 2009",  
  "Jul 12, 2009", "Aug 13, 2009", "Sep 14, 2009", "Oct 16, 2009", "Nov 17, 2009", "Dec 19, 2009",
  "Jan 17, 2010", "Feb 18, 2010", "Mar 22, 2010", "Apr 23, 2010",  "May 9, 2010", "Jun 10, 2010",  
  "Jul 12, 2010", "Aug 13, 2010", "Sep 14, 2010", "Oct 16, 2010", "Nov 17, 2010", "Dec 19, 2010",
  "Jan 17, 2011", "Feb 18, 2011", "Mar 22, 2011", "Apr 23, 2011",  "May 9, 2011", "Jun 10, 2011",  
  "Jul 12, 2011", "Aug 13, 2011", "Sep 14, 2011", "Oct 16, 2011", "Nov 17, 2011", "Dec 19, 2011",
  "Jan 17, 2012", "Feb 18, 2012", "Mar 21, 2012", "Apr 22, 2012",  "May 8, 2012", "Jun 9, 2012",  
  "Jul 11, 2012", "Aug 12, 2012", "Sep 13, 2012", "Oct 15, 2012", "Nov 16, 2012", "Dec 18, 2012",
  "Jan 17, 2013", "Feb 18, 2013", "Mar 22, 2013", "Apr 23, 2013",  "May 9, 2013", "Jun 10, 2013",  
  "Jul 12, 2013", "Aug 13, 2013", "Sep 14, 2013", "Oct 16, 2013", "Nov 17, 2013", "Dec 19, 2013",
  "Jan 17, 2014", "Feb 18, 2014", "Mar 22, 2014", "Apr 23, 2014",  "May 9, 2014", "Jun 10, 2014",  
  "Jul 12, 2014", "Aug 13, 2014", "Sep 14, 2014", "Oct 16, 2014", "Nov 17, 2014", "Dec 19, 2014",
  "Jan 17, 2015", "Feb 18, 2015", "Mar 22, 2015", "Apr 23, 2015",  "May 9, 2015", "Jun 10, 2015",  
  "Jul 12, 2015", "Aug 13, 2015", "Sep 14, 2015", "Oct 16, 2015", "Nov 17, 2015", "Dec 19, 2015"))
  
  ######
  colnames(dates) <- c("date", "GPP")
  dates$mdy <- mdy(dates$date)
  dates$mdy <- as.character(dates$mdy)
  dates$mdy <- substr(dates$mdy, 1, 7)
  site <- substr(x, 1,3)
  dates$site <- rep(site, length=nrow(dates))
  return(dates)
}

#lapply "GPP_format" fxn over all of the site GPP files
indices_tmp <- lapply(files, GPP_format)

#merge columns together
indices <- do.call(rbind, indices_tmp)

id <- as.data.frame(indices$mdy)
id$mo <- substr(id$`indices$mdy`, 6,7)
id$month[id$mo == "01"] <- "Jan"
id$month[id$mo == "02"] <- "Feb"
id$month[id$mo == "03"] <- "Mar"
id$month[id$mo == "04"] <- "Apr"
id$month[id$mo == "05"] <- "May"
id$month[id$mo == "06"] <- "Jun"
id$month[id$mo == "07"] <- "Jul"
id$month[id$mo == "08"] <- "Aug"
id$month[id$mo == "09"] <- "Sep"
id$month[id$mo == "10"] <- "Oct"
id$month[id$mo == "11"] <- "Nov"
id$month[id$mo == "12"] <- "Dec"
id$year <- substr(id$`indices$mdy`, 1,4)
id$mo_yr <- paste(id$month, id$year, sep = "-")
id$site <- indices$site
id$id <- paste(id$site, id$mo_yr, sep="-")
id$site <- paste("AU", id$site, sep="-")

      ###NEED TO FIX NON-AUSTRALIA SITE NAMES###
      #something like this: 
      #fix modis site names
      #id$site <- substr(id$id, 1,6)
      id$site[id$site== "AU-AMO"] <- "ES-Amo"
      id$site[id$site== "AU-DEM"] <- "SD-Dem"
      id$site[id$site== "AU-DHR"] <- "SN-Dhr"
      
      id$site[id$site== "AU-Kru"] <- "ZA-Kru"
      id$site[id$site== "AU-Noe"] <- "IT-Noe"
      id$site[id$site== "AU-LJu"] <- "ES-LJu"
      id$site[id$site== "AU-Nxr"] <- "BW-Nxr"
      id$site[id$site== "AU-SLu"] <- "AR-SLu"
      
      id$site[id$site== "us-lpa"] <- "mx-lpa"
      id$site[id$site== "us-ray"] <- "mx-ray"
      id$site[id$site== "us-tes"] <- "mx-tes"
      
      id$id <- paste(id$site, id$mo_yr, sep="-")
      


final <- cbind.data.frame(id$id, indices$GPP)
colnames(final) <- c("sitedate", "MODIS_GPP")
write.csv(final, "C:/Users/farellam/Documents/DryFlux/data/aus_MODIS_GPP.csv")
