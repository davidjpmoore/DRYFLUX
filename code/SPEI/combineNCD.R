##COMBINE THE SEPERATE CRU .NC FILES INTO A SINGLE FILE THAT CONTAINS OBERSERVATIONS FROM 1991 - 2019 FOR EACH VARIABLE NEEDED TO CALCULATE SPEI (PET AND PRECIP)##

library(ncdf4)
library(raster)

# set path and filenames
ncpath <- "Z:/SRER/Martha/DryFlux/data/CRU/"
pre1 <- "cru_ts4.04.1991.2000.pet.dat"
pre2 <- "cru_ts4.04.2001.2010.pet.dat"
pre3 <- "cru_ts4.04.2011.2019.pet.dat"
ncfname1 <- paste(ncpath, pre1, ".nc", sep="")
ncfname2 <- paste(ncpath, pre2, ".nc", sep="")
ncfname3 <- paste(ncpath, pre3, ".nc", sep="")


# open a netCDF file
ncin1 <- nc_open(ncfname1)
print(ncin1)
ncin2 <- nc_open(ncfname2)
ncin3 <- nc_open(ncfname3)

# get longitude and latitude
lon <- ncvar_get(ncin1,"lon")
nlon <- dim(lon)
head(lon)
lat <- ncvar_get(ncin1,"lat")
nlat <- dim(lat)
head(lat)
print(c(nlon,nlat))

# get time
time1 <- ncvar_get(ncin1,"time")
time1
time2 <- ncvar_get(ncin2,"time")
time2
time3 <- ncvar_get(ncin3,"time")
time3
time <- c(time1, time2, time3)
ntime <- length(time)

tunits <- ncatt_get(ncin1,"time","units")

#load netCD files as rasters, stack together
brick1 <- brick(ncfname1)
brick2 <- brick(ncfname2)
brick3 <- brick(ncfname3)
pre_stack <- stack(brick1, brick2, brick3)
allbrick <- brick(pre_stack)


# create and write the netCDF file -- ncdf4 version
# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
timedim <- ncdim_def("time",tunits$value,as.double(time))

# define variables
fillvalue <- 9.96920996838687e+36
missingval <- 9.96920996838687e+36
dlname <- "precipitation"
tmp_def <- ncvar_def("pet","mm/day",list(londim,latdim,timedim),
                     dlname,
                     fillvalue,
                     missval=missingval,
                     prec="single")


# create netCDF file and put arrays
ncfname <- "Z:/SRER/Martha/DryFlux/data/CRU/pet1991.2019.nc"
ncout <- nc_create(ncfname,list(tmp_def),force_v4=TRUE)


# put variable values in the file
for (i in 1:nlayers(allbrick)) { 
  ncvar_put(nc = ncout,
            varid = tmp_def,
            vals = values(allbrick[[i]]),
            start = c(1, 1, i),
            count = c(-1, -1, 1))
  }

# close the file, writing data to disk
nc_close(ncout)




