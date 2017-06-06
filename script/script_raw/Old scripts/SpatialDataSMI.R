#### File Description

#### Laden notwendiger Paketet ####
library(maptools)
library(ncdf)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(zoo)
library(rgdal)
library(foreign)
library(maps)
library(colorspace)

#test
#### Load and plot the SMI layers ####
### read in the mSMI.ncd via the raster brick function
mSMiRas<-brick("./data/data_raw/4_michael/mSMI.nc")

mSMiRas # file is of class Raster Brick

### Plot the mSMIRas 

plot(mSMiRas, c(640,652)) #ohne Angabe der Layers würde hier automatisch die erste 16 geplottet werden

### Extract a Particular Layer
mSMI1<-raster(mSMiRas,layer=652) # extract particular layers

### Plot that layer
plot(mSMI1, main="SMI for the 1st month")

#### LATTICE: RasterVis Plotting #### Kann ich eventuell rauslassen
levelplot(mSMiRas, layers=c(640,652)) #Darstellung zweier Jahre

### Make a montly sequence, starting 1950
idx <-seq(from=as.Date('1950-1-1'), to=as.Date('2009-12-1'), 'month')
idx <-as.yearmon(idx)
idx
idx[540:720]
length(idx)

### Set and Get Z-Values, here time ###
mSMiRasM<-setZ(mSMiRas, idx, name='Zeit') #hier füge ich eine besser lesbare Zeitdimension hinzu

names(mSMiRasM)<-idx #hier werden die einzelen Slides nach der neuen Reihe benannt

levelplot(mSMiRasM, layers=c(640,652), par.settings=RdBuTheme, main="SMI", region=T ) # blau-rote Farbstruktur

#### Shape mit Kreisen einlesen ####
#Als Polygon
KreisPol<-readShapePoly(fn="./data/data_raw/4_michael/adminitrative_borders_Ger/vg2500_krs") 
KreisPol

# Give KreisPol a projection
projection=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")
projection(KreisPol)= projection

#Plot
plot(KreisPol, main="Kreise")



########################Von hier an mit Extrahierten SMI, also auf Kreisebene ###################

#### EXTRACT der mSMI Data to Landkreis Niveau von layer 540 an ####
# mSMiExAvg<-extract(mSMiRas, KreisPol,na.rm=T, fun=mean, weights=T, layer=540) #kommt als Large materix format
#dauert sehr lange wenn nicht auf dem Cluster, daher file gespeichert
# write.csv(mSMiExAvg,"./data/data_raw/mSMiExAvg")  


#### Laden der gespeichertet Extrahierten mSMI ####
mSMiAVG<-as.matrix(read.csv("./data/data_raw/mSMiExAvg"))
mSMiExAvg<-mSMiAVG[,c(2:ncol(mSMiAVG))]
str(mSMiExAvg)

#### Jetzt müssen die Daten wieder eine räumliche Zuordnung bekommen ####
nrow(KreisPol) #Kreispol hat 412 rows
row.names(KreisPol) # 0:411
nrow(mSMiExAvg)
row.names(mSMiExAvg) #Kreispol und mSMiExAvg benötigen die gleichen row names

#### Give mSMiExAvg_df same row names as Kreispol ####
mSMiExAvg_df<-as.data.frame(mSMiExAvg)
str(mSMiExAvg_df)

FIPS<-row.names(KreisPol)
FIPS 
length(FIPS)

row.names(mSMiExAvg_df)<-FIPS #Achtung, row.names gehen mit 0 los.

mSMiExAvg_df

KreismSMiExAvg<-spCbind(KreisPol, mSMiExAvg_df)
row.names(KreismSMiExAvg)
str(KreismSMiExAvg)

#### Geb meinem Datensatz die richtigen Namen für Plotting #####
idx[540:720] ## idx wurde weiter oben erzeugt
idx.names<-make.names(idx)
idx.names
names(KreismSMiExAvg)[c(6:length(names(KreismSMiExAvg)))]
names(KreismSMiExAvg)[c(6:length(names(KreismSMiExAvg)))] <- idx.names[540:720]

#### plot(KreismSMiExAvg)
