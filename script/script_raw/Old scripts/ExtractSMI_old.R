#### File Description ####
' - This file is based on a non successful approach to extract the SMI for the non-irrigated arable land generally presented in the Corine Land Cover Maps. Therefore, a mask was created
    based on that Corine Land Cover. Then, the SMI on the masked cells should be aggregated for the communes.
  - The approach was not succesfull within a certain time span. 
  - Therefore, Stephan Thober wrote a script in Fortran which does the extraction.
  - Main problem for the extraction of the SMI were, that the resolution of the mask (100*100 m) is much higher than the SMI (4000*4000m).
    Several problems occur because of that. 
        - First produces the raster::disaagregate very large files. Therefore, very large ram is need. 
        - Second, when I tried to solve this problem by looping over smaller parts (years) whilst disaggregating and masking each month of this year,  
          files where put in the /tmp folder. Those files add up to memory usage of 9 GB. This lead to memory problems on the root partition.
        - Also the extract function is very demanding. But multicore approaches can be used.
    
  - Further, I do some plots with spplot (lattice) -> this is also for eyploring purposes. For more information see output -> plot
    
  - There are some examples on what to do with projections  
  - mutlticore approaches are introduced

'

#### Ursprünglicher Plan: Vorgehen und Probleme ####
'
- CLCraster laden und richtige Projection geben. Dabei nehme ich EPSG: 34168 und nicht EPSG:
- Maske laden
- Disaggregieren des SMi auf 100*100 m -> disaggregate oder resample? -> disaggregate funxt gut
- Landwirtschaftliche Gebiete als Maske benutzen, sodass nur noch Landwirtschaftsflächen übrig sind -> mask()
- 100*100 m masked SMI Raster-Layer und Spatial Polygones in die gleiche Projektionsebene bringen, um diese mit ssplot zu plotten.
  Mit spplot liegen die beiden layer mit der richtigen Projection dann auch richtig übereinander.
  Scheinbar liest in meinem Fall hier der raster::brick Befehl eine Falsche Projection ein. Darüber hinaus plottet der einfach plot Befehl nicht richtig.
- SMI in landwirtschaftlichen Gebieten extrahieren, dabei verzichte ich vorerst auf weighting, da meine Auflösung sehr mit 100*100 schon sehr klein ist
- Restlichen Felder sind NA, was bedeutet das -> werden nicht in die Berechnung des Durchschnitts einbezogen 
- Plotten mit Aussagekräftigen Farbschema
'
#### Output
## Files
'
## Grundlagen der Plots ##
Mai2003
- Das Procedure soll für layer Mai von hinten bis vorne durchgeführt werden
- mSMiMai2003 <- raster layer
- mSMiMai2003Dis <- disaggregate
- mSMiMai2003DisMask <- masked
- mSMiMai2003Ext <- extracted (kein SpatialDataFrame mehr)
- KreismSMiMai2003Ext <- SpatialPolygonsDataFrame aus KreisPolygon und extrahierten SMI

Jahr 2003
- mSMiSub2003 <- subset des mSMI für das Jahr 2003
- mSMiSubDis2003 <- dieses Subset dann disaggregiert
- mSMiSubDisMask2003 <- das disaggregierte Subset dann maskiert
'
## Achtung, einige Files wurde komprimiert, da diese sehr groß sind ##
'
- clc_2006_nonirgarabland.asc
- mSMi1mask.asc
- mSMi720dis.gri
- mSMiMai2003Dis.gri
- mSMiMai2003DisMask.gri
- mSMiRas.gri
- mSMiRasDis.gri
- mSMiSubDisMaskStack2003.nc
- mSMiSubDisStack2003.nc
' 

## Plots
'- Plot der Maske CLCMask
 - Plot der Kreise
 - Plot SMI, 4*4 km, Layer 720 with Greenish colours

 - Plot des Mai 2003 nicht disaggregiert, also auf 4000*4000 m mit Kreispolygonen
 - Plot des Mai 2003  disaggregiert auf 100*100 m mit Kreispolygonen
 - Plot des Mai 2003, disaggregiert auf 100*100m, maskiert mit Kreispolygonen, (nicht extrahiert)
 - Plot eines Layers(Mai 2003), welcher mit der raster::extract function erzeugt wurde 

 - Plot des Jahres 2003 nicht disaggregiert, also auf 4000*4000 m mit Kreispolygonen
 - Plot des Jahres 2003  disaggregiert auf 100*100 m mit Kreispolygonen
 - Plot des Jahres 2003, disaggregiert auf 100*100m, maskiert mit Kreispolygonen, (nicht extrahiert)

'


#### Input and Dependencies
# clc_2006_nonirgarabland.asc <- CLC2006 -> Mask
# mSMI.cs <- exogen aus mHM

#### Packages ####
library(sp)
library(rgdal)
library(raster)
library(lattice)
library(rasterVis)
library(maptools)
library(RColorBrewer)
library(classInt)
library(ncdf)
library(zoo)
library(foreign)
library(rgeos)
library(parallel)
library(snow)
# library(ggplot2)

#################################################################################################################
##################### Disaggregate mSMI by factor 40 and then mask the values ###################################
#################################################################################################################

############################
#### Load CLCMask ####
## File ist komprimiert, muss erst entpackt werden
CLCMask<-raster("data/data_raw/clc_2006_nonirgarabland.asc") ### dadurch wird die file viel kleiner:12 kb statt 240 MB; Aber raster setzt nur pointer
projection(CLCMask) <-"+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +datum=potsdam +units=m +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"

#########################################################################################
#### Laden des SMI (ursprüngliche Version, also keine Maskierung und Disaggregation) ####
mSMi<-brick("./data/data_raw/4_michael/mSMI.nc")
# plot(mSMi)

######################################
#### Korrektur der Projectionen ####
projection(mSMi) # Falsche Projection, deswegen geb ich der file die richtige Projection aus mHM -> # EPSG:31468
projection(mSMi)<-"+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +datum=potsdam +units=m +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"
# bin mir nicht genau sicher, inwiefern die +ellps appendix die Projection ändert und wo diese her ist.
# link mit mehr Informationen ## http://spatialreference.org/ref/epsg/dhdn-gauss-kruger-zone-4/
# projInfo(type = "proj")
# projInfo(type = "datum")
# projInfo(type = "ellps")
# EPSG <- make_EPSG()
# EPSG[  grep  ("31468",   EPSG$code  ),]

# tmp<-  EPSG[(    grep    ("+proj=utm +zone=40 +datum=WGS84 +units=m +no_defs", EPSG$prj4)), ]

#################################################################
#### Verändern der Namen der Layer im netcdf mSMi RasterStack ###
names(mSMi)
idx <-seq(from=as.Date('1951-1-1'), to=as.Date('2010-12-1'), 'month')
idx <-as.yearmon(idx)
idx_str<-paste("SM",idx, sep="")
idx_names<-make.names(c(idx_str), unique = TRUE)
idx_gsub<-gsub(" ","",idx_str)
idx_names<-make.names(c(idx_gsub), unique = TRUE)
idx_names
length(idx_names)==length(names(mSMi))

names(mSMi)<-idx_names

# plot(mSMi)

############################################
#### First procedure for a single layer: Mai 2003 ####
############################################
#### Achtung: Es gab Probleme die files als netCDFs abszuspeichern, da dort entweder die Values nicht übergeben wurden oder es Probleme mit der CRS gab.
#### Daher nehme ich hier das Standard Raster Format

##########################################################
#### Einen einzelnen Layer des SMI Raster Extrahieren ####
## Hier 629: Mai 200
mSMiMai2003<-raster(mSMi, layer=629)
main<-names(mSMiMai2003)

##############################################
#### Diaggregieren des SMI720 auf 100*100 ####
mSMiMai2003Dis <- disaggregate(mSMiMai2003, fact=40)

#### Write disaggregated SMI of Layer 720
writeRaster(mSMiMai2003Dis,"./data/data_raw/mSMiMai2003Dis.grd", overwrite=T) 

#### Read disaggregated SMI of Layer 720
mSMiMai2003Dis2 <- raster("./data/data_raw/mSMiMai2003Dis.grd")

## Plot mit base:plot
par(mfrow=c(1,2))
plot(mSMiMai2003, main="SMI Germany, 4000*4000")
plot(mSMiMai2003Dis, main="SMI Germany, 100*100")
# Die Plots sehen sich sehr ähnlich und scheinen Sinn zu machen

####################################################
#### Maskieren des SMI für den Mai 2003 auf 100*100 Auflösung ####
### Überprüfen, um bounding boxes übereinstimmen
# extent(mSMiMai2003Dis)==extent(CLCMask) #->check

### Mask: computational intensive, therefore just load file downwards
mSMiMai2003DisMask <- mask(mSMiMai2003Dis , CLCMask) # durch Maskieren wächst die file wieder auf 240 MB
par(mfrow=c(1,1))


#### Write Masked SMI of Layer 720 ####
writeRaster(mSMiMai2003DisMask,"data/data_raw/mSMiMai2003DisMask.grd",  overwrite=T)

#### Read Masked SMI of Layer 720
mSMiMai2003DisMask <- raster("data/data_raw/mSMiMai2003DisMask.grd")

#### Plot zum überprüfen ob alles geklappt hat
plot(mSMiMai2003DisMask, main="Mai 2003, disaggregated, masked")
################################################################################
#### Extrahieren auf LK Level der disagg. und mask. SMI Werte des Mais 2003 ####
################################################################################

#####################
#### Preparation ####

###################################
#### Read KreisShape via rgdal ####
## Achtung, diese file war beschädigt und lies sich aufgrund dessen bis zum 18.09 nicht öffnen
KreisPolOGR <- readOGR(dsn = "./data/data_raw/4_michael/adminitrative_borders_Ger/","vg2500_krs")
 proj4string(KreisPolOGR) #
# gleiche Projection wie mHM Output: EPSG:31468

## Explorative Analysis of KreisPolOGR ##
# str(KreisPolOGR)
# par(mfrow=c(1,1))
plot(KreisPolOGR)
# identify(coordinates(KreisPolOGR)) ### Identify() und locator() ####


### Angleichen der Projectionen für mSMiMai2003Dis und KreisPolOGR ####
projection(mSMiMai2003DisMask)==projection(KreisPolOGR) # Die Projection ist die gleiche
extent(mSMiMai2003DisMask)
extent(KreisPolOGR)

#### Extract ####
beginCluster(detectCores()-1)
ptm <- proc.time() # Start Clock
mSMiMai2003Ext<-extract(mSMiMai2003DisMask, KreisPolOGR, na.rm=T, fun=mean)
timeextract <- proc.time() - ptm # Stop Clock and Take Time: dauert etwa elf Minuten Multicore
endCluster()
## Write the extracted SMI data
write.csv(mSMiMai2003Ext,"./data/data_raw/mSMiMai2003Ext.csv")  

## Read the extracted SMI data
mSMiMai2003Ext<-read.csv("./data/data_raw/mSMiMai2003Ext.csv")

# Funktion sp::over funktioniert mit den hier verwendeten Datenformaten nicht

###############################################################################
#### Erstellen eines SpatialPolygonesDataframe mit den extrahierten Werten ####
###############################################################################

############################################
### SpatialCbind mit LandkreisPolygonen ####

#### Load File with the extraction of the diasaggregated and masked values of SMI ####
mSMiMai2003Ext<-read.csv("./data/data_raw/mSMiMai2003Ext.csv")
dim(mSMiMai2003Ext)

mSMiMai2003Ext<-as.matrix(mSMiMai2003Ext[,2])

v<-length(mSMiMai2003Ext)

#### Change rownames so that they can be binded with the SpatialPolygones
rownames(mSMiMai2003Ext)<-0:411
dim(mSMiMai2003Ext)
names(mSMiMai2003Ext)
mSMiMai2003Ext<-mSMiMai2003Ext[,-1]
mSMiMai2003Ext<-as.data.frame(mSMiMai2003Ext)
rownames(mSMiMai2003Ext)<-0:411

#### Bind SpatialPolygones of Commnunities with extracted SMI  by same rownames ####
KreismSMiMai2003Ext<-spCbind(KreisPolOGR,mSMiMai2003Ext)
spplot(KreismSMiMai2003Ext)

## Write SpatialPolygonsDataframe ##
writePolyShape(KreismSMiMai2003Ext, "./data/data_raw/KreismSMiMai2003Ext.shp")

## Read SpatialPolygonsDataframe ##
KreismSMiMai2003Ext<-readOGR("./data/data_raw/", "KreismSMiMai2003Ext")
proj4string(KreismSMiMai2003Ext)<-proj4string(KreisPolOGR)


###########################################################################
#### Versuch, dass disaggregieren oben mit einem Subset zu replizieren ####
###########################################################################
#### Subset des SMI auswählen 
# year 2003
mSMiSub <-subset(mSMi,625:636)
names(mSMiSub) # Januar 1999 bis Dezember 2010
# 720-577


### Explorative Steps
# plot(mSMiSub)
# summary(mSMiSub)

#### Write Subset of SMI on 4000*4000 resolution
# writeRaster(mSMiSub,"data/data_raw/mSMiSub.grd" , format="raster", overwrite=T)

#### Read subset: raster uses pointer
# mSMiSub <- brick("data/data_raw/mSMiSub.grd") # wieder nur 12 KB
# # showMemoryUse()
# 
# #####################################
# #### Disaggreggieren des Subsets ####
# ## dauert lange, scheit aber mit nun größeren Swap zu funktionieren
# mSMiSubDis <- disaggregate(mSMiSub, fact=40)
# writeRaster(mSMiSubDis, "data/data_raw/mSMiSub2000Dis.nc")
# names(mSMiSubDis)
# plot(mSMiSubDis)
## Commment: File was getting very large and errors were reported during transformation procedures 
# 
# #### Write Disaggregiertes Subsets
# writeRaster(mSMiSubDis,"data/data_raw/mSMiSubDis.grd" , format="raster", overwrite=T)
# 
# #### Read Disaggregated Subset 
# mSMiSubDis <- brick("data/data_raw/mSMiSubDis")
# 
# # #######################################
# #### Maskieren des SMI des Subsets ####
# ### Überprüfen, um bounding boxes übereinstimmen
# # extent(mSMiMai2003Dis)==extent(CLCMask) #->check
# 
# ### Mask: computational intensive, therefore just load file downwards
# mSMiSubMask <- mask(mSMiSubDis, CLCMask) # durch Maskieren wächst die file wieder auf 240 MB
# plot(mSMiSubMask)
# 
# #### Write Masked SMI of Layer 720 ####
# writeRaster(mSMiSubMask,"data/data_raw/mSMiSubMask.asc", format="ascii", overwrite=T)
# 
# #### Read Masked SMI of Layer 720
# mSMiSubMask <- raster("data/data_raw/mSMiSubMask.asc")


########################################################################
#### Loop over all relevant layers to disaggregate and mask the SMI ####
########################################################################
## Achtung
'Es gibt Probleme mit der raster::disaggregate Funktion bei zu großen Datensätzen, da diese Auslagerungsdatein in die directory "../../../../../tmp/R_raster_peichl" legt. Wenn mein
Speicher auf dieser Partition dann voll ist bricht R in den Vorgang mit Problemem ab. Daher versuch ich diese Datei nun nach jedem Loop vorgang zu löschen'


## Layer definieren, über welche gelooped werden soll
names(mSMi)[577] # Definition des relevanten Startpunkts für die loop
seq(577,720,1)

layer<-c(seq(577,720,1))
length(layer)

#### Loop
# Start the clock!
# ptm <- proc.time()

# # Year 1999
# 
# mSMiSub1999<-list()
# mSMiSubDis1999<-list()
# mSMiSubDisMask1999<-list()
# 
# for (i in 1:12)
#   {
#   # Load Layer
#   mSMiSub1999[i]<-raster(mSMi, layer[i])
#   print(mSMiSub1999[i])
#   
#   # Disaggregate Layer
#   mSMiSubDis1999[i] <- disaggregate(mSMiSub1999[[i]], fact=40)
#   print(mSMiSubDis1999[i])
#   
#   # Mask Layer
#   mSMiSubDisMask1999[i] <- mask(mSMiSubDis1999[[i]], CLCMask)
#   print(mSMiSubDisMask1999[i])
#   }
# # file.remove("../../../../../tmp/R_raster_peichl/.", recursive=T)
# timeloop1999 <- proc.time() - ptm
# 
# mSMiSubDisMaskStack1999<- stack(mSMiSubDisMask1999)
# 
# names(mSMiSubDisMaskStack1999)<-names(mSMi)[577:588]
# 
# writeRaster(mSMiSubDisMaskStack1999, "data/data_raw/mSMiSubDisMaskStack1999.nc", overwrite=T)
# 
# 
# #Year 2000
#   
# mSMiSub2000<-list()
# mSMiSubDis2000<-list()
# mSMiSubDisMask2000<-list()
# 
# for (i in 13:24)
# {
#   # Load Layer
#   mSMiSub2000[i]<-raster(mSMi, layer[i])
#   print(mSMiSub2000[i])
#   
#   # Disaggregate Layer
#   mSMiSubDis2000[i] <- disaggregate(mSMiSub2000[[i]], fact=40)
#   print(mSMiSubDis2000[i])
#   
#   # Mask Layer
#   mSMiSubDisMask2000[i] <- mask(mSMiSubDis2000[[i]], CLCMask)
#   print(mSMiSubDisMask2000[i])
# }
# 
# test<- brick(mSMiSubDisMask2000[[1]])
# 
# names(mSMiSubDisMaskStack1999)<-names(mSMi)[589:600]
# 
# writeRaster(mSMiSubDisMaskStack2000, "data/data_raw/mSMiSubDisMaskStack2000.nc", overwrite=T)
# 
# #Year 2001
# 
# mSMiSub2001<-list()
# mSMiSubDis2001<-list()
# mSMiSubDisMask2001<-list()
# 
# for (i in 25:36)
# {
#   # Load Layer
#   mSMiSub2001[i]<-raster(mSMi, layer[i])
#   print(mSMiSub2001[i])
#   
#   # Disaggregate Layer
#   mSMiSubDis2001[i] <- disaggregate(mSMiSub2001[[i]], fact=40)
#   print(mSMiSubDis2001[i])
#   
#   # Mask Layer
#   mSMiSubDisMask2001[i] <- mask(mSMiSubDis2001[[i]], CLCMask)
#   print(mSMiSubDisMask2001[i])
# }
# 
# mSMiSubDisMaskStack2001<- stack(mSMiSubDisMask2001)
# writeRaster(mSMiSubDisMaskStack2001, "data/data_raw/mSMiSubDisMaskStack2001", format="raster")

# Year 2002
# mSMiSub2002<-list()
# mSMiSubDis2002<-list()
# mSMiSubDisMask2002<-list()
# 
# for (i in 37:48)
# {
#   # Load Layer
#   mSMiSub2002[i]<-raster(mSMi, layer[i])
#   print(mSMiSub2002[i])
#   
#   # Disaggregate Layer
#   mSMiSubDis2002[i] <- disaggregate(mSMiSub2002[[i]], fact=40)
#   print(mSMiSubDis2002[i])
#   
#   # Mask Layer
#   mSMiSubDisMask2002[i] <- mask(mSMiSubDis2002[[i]], CLCMask)
#   print(mSMiSubDisMask2002[i])
# }
# mSMiSubDisMaskStack2002<- stack(mSMiSubDisMask2002)
# writeRaster(mSMiSubDisMaskStack2002, "data/data_raw/mSMiSubDisMaskStack2002.nc")

########################################################
################ Year 2003 ############################# 
########################################################

mSMiSub2003<-list()
mSMiSubDis2003<-list()
mSMiSubDisMask2003<-list()

for (i in 53)
{
  # Load Layer
  mSMiSub2003[i]<-raster(mSMi, layer[i])
  print(mSMiSub2003[i])
  
  # Disaggregate Layer
  mSMiSubDis2003[i] <- disaggregate(mSMiSub2003[[i]], fact=40)
  print(mSMiSubDis2003[i])
  
  # Mask Layer
  mSMiSubDisMask2003[i] <- mask(mSMiSubDis2003[[i]], CLCMask)
  print(mSMiSubDisMask2003[i])
}

## Subset and stack lists produced by loops
# I guess that empty arrays are carried through the loop for those layers, which are not considered before the layers, that are considered
# Therefore, subsetting is necessary for being able to stack the files to produce raster stacks
## Masked and disaggregated year 2003
mSMiSubDisMaskStack2003<- stack(mSMiSubDisMask2003[c(49:60)])
writeRaster(mSMiSubDisMaskStack2003, "data/data_raw/mSMiSubDisMaskStack2003.nc")
mSMiSubDisMaskStack2003<-brick("data/data_raw/mSMiSubDisMaskStack2003.nc")
names(mSMiSubDisMaskStack2003)<-names(mSMi)[625:636]
plot(mSMiSubDisMaskStack2003)
## Disaggregated year 2003
mSMiSubDisStack2003 <- stack(mSMiSubDis2003[c(49:60)])
writeRaster(mSMiSubDisStack2003, "data/data_raw/mSMiSubDisStack2003.nc")
## Year 2003 of SMI
mSMiSubStack2003 <- stack(mSMiSub2003[c(49:60)])
writeRaster(mSMiSubStack2003, "data/data_raw/mSMiSubStack2003.nc")
test <- raster("data/data_raw/mSMiSubStack2003.nc")
test

############################################################################################################################################################################################
###############################################################################################################
############################### Extract #######################################################################
###############################################################################################################
## Generell lässt sich anmerken, dass das extrahieren sich als schwierig darstellt oder 
# gar gescheitert ist aufgrund des großen Rechenaufwandes ##



###########################################################
#### Extrahieren auf Polygonebene des einzelnen Layers ####

#### Extrahieren mit multicore functionalität der extract function ####
# # only subset, i.e. year 2003 considered for testing purposes
# ptm <- proc.time() # Start Clock
# beginCluster( detectCores())
# extent(mSMiSubDisMaskStack2003)
# extent(KreisPolOGR)
# projection(KreisPolOGR)==projection(mSMiSubDisMaskStack2003)
# mSMiExAvgMask<-extract(mSMiSubDisMaskStack2003, KreisPolOGR, na.rm=T, fun=mean)
# endCluster()
# timeextract <- proc.time() - ptm # Stop Clock and Take Time
# 
# ## Write the extracted SMI data
# write.csv(mSMiExAvgMask,"./data/data_raw/mSMiExAvgMask", overwrite=TRUE)  
# dput(mSMiExAvgMask,file="./data/data_raw/mSMiExAvgMask")
# 
# ## Read the extracted SMI data
# mSMiExAvgMask<-read.csv("./data/data_raw/mSMiExAvgMask")

## Funktion sp::over funktioniert mit den hier verwendeten Datenformaten nicht

###############################################################################
#### Erstellen eines SpatialPolygonesDataframe mit den extrahierten Werten ####
###############################################################################

#############################################
#### SpatialCbind mit LandkreisPolygonen ####

# #### Load File with the extraction of the diasaggregated and masked values of SMI ####
# mSMiExAvgMask<-read.csv("./data/data_raw/mSMiExAvgMask")
# 
# v<-length(mSMiExAvgMask)
# 
# #### Change rownames so that they can be binded with the SpatialPolygones
# rownames(mSMiExAvgMask)<-0:411
# 
# #### Bind SpatialPolygones of Commnunities with extracted SMI  by same rownames ####
# KreismSMiExAvgMask<-spCbind(KreisPolOGR,mSMiExAvgMask)
# 
# ## Write SpatialPolygonsDataframe ##
# writePolyShape(KreismSMiExAvgMask, "./data/data_raw/KreismSMiExAvgMask.shp")
# 
# ## Read SpatialPolygonsDataframe ##
# KreismSMiExAvgMask<-readOGR("./data/data_raw/", "KreismSMiExAvgMask")
# proj4string(KreismSMiExAvgMask)<-proj4string(KreisPolOGR)
#############################################################################################################################################################################################

#############################################################################################################################################################################################
#################################################################### Plotten ################################################################################################################
#############################################################################################################################################################################################

################################
#### Load Data for plotting ####

## not mamipulated data ##
## Read Data of Year 2003 ####
mSMi2003 <- brick("data/data_raw/mSMiSubStack2003.nc")
mSMi2003
## Read Data for Mai 2003 ##
mSMiMai2003 <- raster(mSMI2003, layer=5)


### Angleichen der Projectionen für mSMiMai2003Dis und KreisPolOGR ####
projection(mSMiMai2003)==projection(KreisPolOGR)

## masked data ##
## Year 2003 ##
mSMiSubDisMaskStack2003 <- brick("data/data_raw/mSMiSubDisMaskStack2003.nc")

mSMiMai2003DisMask  <- raster(mSMiSubDisMaskStack2003, layer=5)
#################################################################
#### Verändern der Namen der Layer im netcdf mSMi RasterStack ###
names(mSMi2003)
idx <-seq(from=as.Date('2003-1-1'), to=as.Date('2003-12-1'), 'month')
idx <-as.yearmon(idx)
idx_str<-paste("SM",idx, sep="")
idx_names<-make.names(c(idx_str), unique = TRUE)
idx_gsub<-gsub(" ","",idx_str)
idx_names<-make.names(c(idx_gsub), unique = TRUE)
idx_names
length(idx_names)==length(names(mSMi))

names(mSMi2003)<-idx_names
names(mSMiSubDisMaskStack2003) <- idx_names

#################
#### Plotten ####


### Hier lege ich die Schritte für welche es unterschiedliche Farbstufen geben soll fest
at=(seq(0,1,0.05))

## Modify trellis theme ##
my.theme = trellis.par.get()
names(my.theme)
my.theme$panel.background
trellis.par.set("background", list(col = "white")) 
trellis.par.set("panel.background", list(col = "white")) 
trellis.par.set("strip.background", list(col = "white")) 

show.settings()

## Setup Months considered ##
ztdrei<-c( "SMSep2003"  ,"SMOkt2003" , "SMNov2003" , "SMDez2003",  "SMMai2003" , "SMJun2003" ,"SMJul2003" , "SMAug2003","SMJan2003",  "SMFeb2003" , "SMMär2003", "SMApr2003" )

## Setuo Colourscheme ##
cs <-colorRampPalette(c("tomato4","tan4", "chartreuse4","cornsilk","chartreuse4","dodgerblue4", "darkblue"))(1e3)


###########################
#### Plot mSMi Mai 2003#### 
###########################


####################
## Plot der Maske ##
names(CLCMask)<-"NonIrAgL"
projection(CLCMask)<-"+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +datum=potsdam +units=m +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"
plot(CLCMask, main="CLC2006, masked for non-irrigated arable land")

## Plott der Maske mit Kreis ##
plot(KreisPolOGR)

# #######################################################
# ## Plot SMI, 4*4 km, Mai 2003 with Greenish colours ##
# colors <- brewer.pal(9, "YlGn")
# pal1 <- colorRampPalette(colors)(1e3)
# mSMiMai2003Spplot <- spplot(mSMiMai2003, main="SMMai2003, 4*4 km",at=at, col.regions = pal1) 
# # Könnte für Darstellung in Frage kommmen, daher exportiert

##########################################################################################
## Plotten des Mai 2003 nicht disaggregiert, also auf 4000*4000 m mit Kreispolygonen ##
mSMi2003Plt<-spplot(mSMiMai2003 ,at=at, col.regions= cs)
mSMi2003Plt
mSMi2003Plt + layer(sp.polygons(KreisPolOGR, col="black", lwd=1))

## Exported als png im format 700 and height 643

###########################################################################
## Plotten des Mai 2003 disaggregiert auf 100*100 m mit Kreispolygonen ##
mSMi2003DisPlt<-spplot(mSMiMai2003Dis,at=at, col.regions= cs, main="SMI Germany, Mai 2003, 100*100")
mSMi2003DisPlt
mSMi2003DisPlt + layer(sp.polygons(KreisPolOGR, col="black", lwd=1))

##########################################################################################################
## Plotten des Mai 2003, disaggregiert auf 100*100m, maskiert mit Kreispolygonen, (nicht extrahiert) ## 
mSMi2003DisPltmaskPlt <- spplot(mSMiMai2003DisMask, at=at,col.regions= cs, main="SMI Germany, Mai 2003, 100*100, masked")
mSMi2003DisPltmaskPlt
mSMi2003DisPltmaskPlt + layer(sp.polygons(KreisPolOGR, col="black", lwd=1))
## Exported als png im format 700 and height 643


############################
#### Plot mSMi Year 2003#### 
############################


##########################################################################################
## Plotten des Jahres 2003 nicht disaggregiert, also auf 4000*4000 m mit Kreispolygonen ##
mSMi2003Plt<-spplot(mSMiSubStack2003 ,at=at, col.regions= cs, main="SMI Germany, Year 2003, 4000*4000")
mSMi2003Plt
mSMi2003Plt + layer(sp.polygons(KreisPolOGR, col="black", lwd=1))

###########################################################################
## Plotten des Jahres 2003 disaggregiert auf 100*100 m mit Kreispolygonen ##
mSMi2003DisPlt<-spplot(mSMiSubDisStack2003,at=at, col.regions= cs, main="SMI Germany, Year 2003, 100*100")
mSMi2003DisPlt
mSMi2003DisPlt + layer(sp.polygons(KreisPolOGR, col="black", lwd=1))

##########################################################################################################
## Plotten des  Jahres 2003, disaggregiert auf 100*100m, maskiert mit Kreispolygonen, (nicht extrahiert) ## 
# Hier plotte ich das ganze Jahr 2003
mSMi2003DisPltmaskPlt <- spplot(mSMiSubDisMaskStack2003, at=at,col.regions= cs, main="SMI Germany, Year 2003, 100*100, masked")
mSMi2003DisPltmaskPlt
mSMi2003DisPltmaskPlt + layer(sp.polygons(KreisPolOGR, col="black", lwd=1))

#############################################
#### Für extrahierten Plot siehe 4km_SMI ####
#############################################

