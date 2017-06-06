#### File Description ####

# Einlesen von Corine Land Use Land Cover Maps 
# Jahr 2006
# Maske erstellen für non-irrigated arable land
# Maske Herstellen: Aus CLC map nur landwirtschaftliche Gebiete auswählen ->  ascii in data.frame, alle Values ungleich 12 gelöscht, und dann wieder in raster transformiert
# From CLC_Legend: GridCode: 12 CLC_Code: 211	Label1: Agricultural areas Label2:	Arable land Label3:	Non-irrigated arable land	RGB: 255-255-168
# Extrahieren des SMI und hochrechnen wurde dann von Stephan Thober in Fortran Script gemacht, denoch sind hier erste Ansätze aufgeführt, wie das in R umgesetzt werden kann. 
# Für die Ansätze in R siehe ExtractSMI.R
# 

#### Input and Dependencies ####
# Corine Land Cove ("data/data_raw/4_michael/clc_2006.asc")r <- Extern

#### Output ####
# CLCrasterMask,"data/data_raw/clc_2006_nonirgarabland.asc

### Libraries ###
library(sp)
library(rgdal)
library(raster)
library(lattice)
library(rasterVis)
library(maptools)
library(RColorBrewer)
library(classInt)


################################################################################################
#### Raster: Einlesen der Corine Landcover Landuse Layer, welches im ASCII Format vorliegt #####
ASCFileMatze <- "data/data_raw/4_michael/CLC/clc_2006.asc"

# #### maptools::readAsciiGrid ####
# CLCasc<- readAsciiGrid(ASCFileMatze)
## save(CLCasc,file="data/data_raw/CLCasc.rda" )
# str(CLCasc)

# Read in with raster::raster
CLCraster<-raster(ASCFileMatze)
CLCraster # kein Projectionswert
plot(CLCraster)

####################
#### Projection ####
proj<-CRS("+init=epsg:31468") 
# "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +datum=potsdam +units=m +no_defs +ellps=bessel
# +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"
projection(CLCraster) <- proj 

# das scheint der richtige Projektionsstring zu sein
# Diese gilt sowohl im mHM als auch für die Landkreise

#### 
# coordinates(CLCraster) # Die Einheit scheint in Metern zu sein
# bbox(CLCraster)
# summary(CLCraster)
# CLCrasterplt<-spplot(CLCraster)

# ##########################################
# #### Nicht funktionierende Projection ####
# # +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
# # Das ist die Projection, welche mir beim Einlesen des SMI mit Raster vorgegeben wird. Das ist der EPSG Code für die Weltweite Projection.
# 
# proj1<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# CLCrasterWrgPrj<-CLCraster
# projection(CLCrasterWrgPrj)<-proj1
# # 
# # 
# # res<-project(xy, proj1)
# # head(res)
# # 
# # res <- project(CLCraster, proj1, inv=TRUE)
# 
# # Fehler in validityMethod(as(object, superClass)) : 
# # Geographical CRS given to non-conformant data: 4738000 6120000
# spplot(CLCrasterWrgPrj) ###ACHTUNG: FEHLERMELDUNG, wenn ich der File eine Projektion gebe, aber für Input Daten aus mHM nicht notwendig, 
# # da diese die gleiche Bounding Box haben
# # Generell gilt, das R die PROJ.4 Library exposed by rgdal benutzt, besonders wenn ich projection() benutze

#####################################################
#### Herstellen der Maske ####
### Raster as Matrix ###

CLCm<-as.matrix(CLCraster) # File sehr groß

# fix(CLCm)
# CLCm
# levelplot(CLCm)

### Bearbeiten der Daten in der Matrix ###
## Alle Werte, die nicht 12, also nicht Landwirtschaft sind, werden zu NAs
## 12 = Non-irrigated arable land
u<-CLCm!="12"
CLCm[u] <- NA 
# bad<- is.na(CLCm)
# CLCm[!bad]
# 
# good<-complete.cases(CLCm)
# good
# CLCm[good]

########################
### Matrix zu Raster ###
CLCrasterMask<-raster(CLCm, xmn=4038000, xmx=4738000, ymn=5220000, ymx=6120000)
# CLCrasterMaskNull<-raster(CLCm)
str(CLCrasterMask)
names(CLCrasterMask)<-names(CLCraster)
projection(CLCrasterMask)<-proj
CLCrasterMaskspplt<-spplot(CLCrasterMask)
# 
#### Show NA- Value
NAvalue(CLCrasterMask) 

#### Write as ascii ####
writeRaster(CLCrasterMask,"data/data_raw/clc_2006_nonirgarabland.asc", format="ascii", overwrite=T)

#############################################################
#### Herstellen der Maske ist beendet ####


