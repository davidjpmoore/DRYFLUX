# code adapted from: https://github.com/sbegueria/SPEIbase/tree/master/R
#If you use this code or parts of it, it'd be nice if you cite the repo as:
#Beguería S. (2017) SPEIbase: R code used in generating the SPEI global database, doi:10.5281/zenodo.834462.

#If you use the SPEI dataset in your resarch, please cite the following papers:
#Beguería, S., Vicente-Serrano, S.M. y Angulo, M., (2010): A multi-scalar global drought data set: the SPEIbase: A new gridded product for the analysis of drought variability and impacts. Bulletin of the American Meteorological Society. 91, 1351-1354
#Vicente-Serrano, S.M., Beguería, S., López-Moreno, J.I., Angulo, M., El Kenawy, A. (2010): A new global 0.5° gridded dataset (1901-2006) of a multiscalar drought index: comparison with current drought index datasets based on the Palmer Drought Severity Index. Journal of Hydrometeorology. 11: 1033-1043
############################################################################################
#Description: This script computes the global SPEI dataset at different time scales. 
#One netCDF file covering the whole globe and time period is generated for each time scale, 
#e.g. spei01.nc for a time scale of 1 month, etc. Output files are stored on /outputNcdf. 
#These are the global files that can be downloaded from http://spei.csic.es/database.html.
############################################################################################

# Compute SPEI at various time scales from very large netCDF files,
# using parallel computing.
getwd()
setwd("Z:/SRER/Martha/DryFlux")

source('./code/SPEI/functions.R')
library(SPEI)
library(ncdf4)
library(snowfall)
library(utils)

sfInit(parallel=TRUE, cpus=31)
sfExport(list='spei', namespace='SPEI')

# create progress bar
pb <- txtProgressBar(min = 0, max = 12, style = 3)

###takes about an hour for one time step of the CRU data frames using parallel 
###processing, 31 core processors, and calculating SPEI for 5 time steps.

# Compute all time scales between 1 and 48
for (i in c(3,6,9,12)) {
  #for (i in c(1,3,6,12)) {
  # update progress bar
  setTxtProgressBar(pb, i)
  
  spei.nc(
    sca=i,
    inPre='Z:/SRER/Martha/DryFlux/data/CRU/precip1991.2019.nc',
    inEtp='Z:/SRER/Martha/DryFlux/data/CRU/pet1991.2019.nc',
    outFile=paste('Z:/SRER/Martha/DryFlux/data/outputNcdf/speiALL_',formatC(i, width=2, format='d', flag='0'),'.nc',sep=''),
    title=paste('Global ',i,'-month',ifelse(i==1,'','s'),' SPEI, z-values, 0.5 degree',sep=''),
    comment='Using CRU TS 4.00 precipitation and potential evapotranspiration data',
    block=24,
    inMask=NA,
    tlapse=NA
  )
  gc()
}

sfStop()
close(pb)
