### File Description ###
' Dieses Script dient dazu, die file KreismSMiExAvgMask herzustellen. Dabei ist unklar, welcher layer hier genau bearbeitet wird. Wahrscheinlich ist es layer 720.
  Da diese file nicht mehr aktuell ist verschiebe ich es in old_scripts.

'

#### Output
## Files
'
    
'
## Plots
'

'


#### Input and Dependencies
'
mSMi1mask.asc <- ?

'


### Libraries ###
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)

setwd( "/gpfs0/home/peichl/projects/correlation")

mSMi1mask<- raster("data/data_raw/mSMi1mask.asc") 
plot(mSMi1maskSmall)

KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/","vg2500_krs")
proj4string(KreisPolOGR) 

projection(mSMi1maskSmall)<-proj4string(KreisPolOGR)

#### Extrahieren ####
mSMiExAvgMask<-extract(mSMi1maskSmall, KreisPolOGR, na.rm=T, fun=mean)


length(mSMiExAvgMask)
names(mSMiExAvgMask)<-0:411

write.csv(mSMiExAvgMask,"./data/data_raw/mSMiExAvgMask") 

mSMiExAvgMask<-read.csv("./data/data_raw/mSMiExAvgMask")
rownames(mSMiExAvgMask)<-0:411
mSMiExAvgMask[,1]<-NULL

KreismSMiExAvgMask<-spCbind(KreisPolOGR,mSMiExAvgMask)

writePolyShape(KreismSMiExAvgMask, "./data/data_raw/KreismSMiExAvgMask.shp")

KreismSMiExAvgMask2<-readShapePoly("./data/data_raw/KreismSMiExAvgMask.shp")

proj4string(KreismSMiExAvgMask2)<-proj4string(KreisPolOGR)

##############
#### Plot this layer ####
at <- c(seq(0,1, 0.01)) # gibt die breaks für die Legende und die Farbstufen an
mSMiExAvgMaskPlt <- spplot(KreismSMiExAvgMask2, zcol="x",at=at, ccol.regions= colorRampPalette(c("red3", "yellow","chartreuse4", "dodgerblue4"))(1e3),main="SMI Germany, Kreise, aus 100*100m masked raster")

