## Description ##
' Load the long term average of the data between 1999 and 2010'

## Input
'
tavg_year_avg.nc/ pre_year_avg.nc - long term average calculate from the yearly average 1999 -2010 with CDO timmean
tavg_year_sum.nc/ pre_year_sum.nc - year average calculate from the data of the years 1000 -2010 with CDO yearsum
'

#### Packages ####

library(lattice)
library(ncdf4)


#############################################################################################################################################################################################
#############################################################################################################################################################################################


###############################
#### Laden des Tavg NetCDF ####
###############################


## long term average Temperature
tavg.nc <- nc_open("/home/peichl/Documents/projects/correlation/data/data_raw/tavg_year_avg.nc")
tavg.nc

tavg <- ncvar_get(tavg.nc, varid = "tavg")
tavg
str(tavg)

levelplot(tavg[,ncol(tavg):1])

tavg1 <- as.matrix()
str(tavg1)


tavg1 <- as.matrix(tavg[,ncol(tavg):1 , i])


tavg2 <-as.matrix(tavg[,ncol(tavg):1 ,2])
levelplot(tavg2)

## long term average Pre
pre.nc <- nc_open("/home/peichl/Documents/projects/correlation/data/data_raw/pre_year_avg.nc")
pre.nc

pre <- ncvar_get(pre.nc, varid = "pre")
pre
str(pre)

levelplot(pre[,ncol(pre):1])

