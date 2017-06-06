
#### File Description ####
# Einlesen der mSMI NetCDF File mit Hilfe des Raster Packets
# Funktion, um einzelnen layers mit Hilfe einer Loops ausgeben zu lassen <- mittlerweile nicht mehr so relevant
# Einlesen der Kreise Shape als Polygon mit dem maptools package
# Extract mSMI auf Landkreisniveau mit dem Raster Paket
# RasterVis Plotting
# Einlesen der Kreise
# EXTRACT der mSMI Data to Landkreis Niveau von layer 540 an
# Laden der gespeichertet Extrahierten mSMI
# Extrahierten mSMI ein räumliche Zuordnung zuweisen
#########################################################

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
library(lattice)
library(sp)
##########################################################

#### Einlesen von mSMI.nc mit Hilfe des Raster-Pakets ####
# x<-raster()
# x
# res(x)
# > # With other parameters 
# x <- raster(ncol=36, nrow=18, xmn=-1000, xmx=1000, ymn=-100, ymx=900)  # that can be changed > res(x)

### read in the mSMI.ncd via the raster brick function
mSMiRas<-brick("./data/data_raw/4_michael/mSMI.nc")
mSMiRas
str(mSMiRas)

#str(mSMiRas[layer=1])
#plot(mSMiRas[layer=1]) #weiß nicht genau, was ich hier gema

#image(mSMiRas) #keine gute Darstellung, da verzogen
plot(mSMiRas, c(700:702)) #ohne Angabe der Layers würde hier automatisch die erste 16 geplottet werden
nlayers(mSMiRas)
##############################################################

#### Extract a Particular Layer ####
# mSMI1<-raster(mSMiRas,layer=560) 
# mSMI1
# str(mSMI1)
# mSMI1@data@band # says its band 560
# 
# print(raster(mSMiRas, layer=1))
# 
# 
# plot(mSMI1, main="SMI for the 1st month")
# 
# 

## Frage, wie hier hier die Daten auslesen kann?
# hasValues(mSMI1)

# values(mSMI1) # eventuell hilft mir diese Funktion um values einlesen zu können in die Raster file
# inMemory(mSMI1)
# fromDisk(mSMI1) #data source is a file on disk
##################################################################

#### Setting a Z-Value: Defining the time sequence ####
#### Make a montly sequence, starting 1950
idx <-seq(from=as.Date('1951-1-1'), to=as.Date('2010-12-1'), 'month')
idx <-as.yearmon(idx)
idx
idx[540:720]
length(idx)

### Set and Get Z-Values, here time ###
mSMiRasM<-setZ(mSMiRas, idx, name='time')#hier füge ich eine besser lesbare Zeitdimension hinzu


mSMiRasM
str(mSMiRasM, max.level="2")

mSMiRasM@data@names
getZ(mSMiRas)

names(mSMiRasM)<-idx #hier werden die einzelen Slides nach der neuen Reihe benannt
names(mSMiRasM)

plot(mSMiRasM, c(640,652))

at<-c(0,0.2,0.4,0.6,0.8,1)
pal <- (brewer.pal(6, "RdBu"))

plot(mSMiRasM, c(640,652), at=at, col=pal) # funxt nicht so richtig
View(mSMiRasM)
###############################################################


#### LATTICE: RasterVis Plotting ####
### spplot beruht auf levelplot
levelplot(mSMiRas, layers=c(540,544)) #Darstellung zweier Jahre

### ab hier mit neuem Z-Value
#levelplot(mSMiRasM, layers=c(640,652), par.settings=GrTheme) # Graue Farbstruktur
#levelplot(mSMiRasM, layers=c(640,652), par.settings=BTCTheme) # blaue Farbstruktur
# levelplot(mSMiRasM, layers=c(640,652), par.settings=RdBuTheme ) # blau-rote Farbstruktur
# levelplot(mSMiRasM, layers=c(640,652),at=at, par.settings=RdBuTheme ) #mit diskreten Schritten
# levelplot(mSMiRasM, layers=c(640,652),at=at, par.settings=RdBuTheme)

#myTheme=rasterTheme(region=sequential_hcl(n=100, h = 260, c. = c(80, 0), l = c(30, 90), power = 1.5,  gamma = NULL, fixup = TRUE, alpha = 1)) #eigenes Farbtheme using sequential palette fronm colorspace
#levelplot(mSMiRasM, layers=c(640,652), par.settings=myTheme ) 
#levelplot(mSMiRasM, layers=c(640,652), FUN.margin=median, contour=TRUE) #this method displays a marginal plot of a function across each coordinate
######################################################################

#### Funktion zum Ausgeben einzelener Layer ####
# for(i in 1:10){paste("i =", i)<-raster(mSMI, layer=i)}
# smiList<-vector("list",10)
# smiList
# 
# hm<-function(x,y)for(i in x:y){
#       str<-paste("smi",i, sep="")
#       as.matrix(print(str))
#       x<-raster(mSMI, layer=i)
#       assign(str, x,envir=.GlobalEnv )
# }
### Mit dieser Function kann ich seperate layers ausgeben lassen, so dass ich sie danach auch bearbeiten kann!
#######################################################################

#### Einlessen von Shapefiles ####

#### Festlegen der Projektion ####
projection=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")

#### Maptools-Package: ####

#### SpatialLinesDataFrame ####
KreisLine <- readShapeLines("./data/data_raw/4_michael/adminitrative_borders_Ger/vg2500_krs")
KreisLine
plot(KreisLine)

# Explore structure of the shape file #
str(KreisLine, max.level=3)

KreisLine@data # d.h hier sind die Daten ensprechend als Dataframe abgespeichert

#die comID kann ich dann nutzen, um Daten beruhend auf den Kreisen mit data der Ertragszahlen zu verschneiden
plot(KreisLine, col="black", lwd=1, add= T)
#######################

#### Lese Shape file als Polygon ein ####

#### SpatialPolygonDataFrame ####

KreisPol<-readShapePoly(fn="./data/data_raw/4_michael/adminitrative_borders_Ger/vg2500_krs") 
KreisPol
KreisPol@data
names(KreisPol)

plot(mSMiRas, c(702:702))
plot(KreisPol, add=T) # würde man den Befehl hier genause wie beim SpatialLinesDataFrame ausführen, dann sind die Landkreise mit Farbe aufgefüllt

data(KreisPol)
projection(KreisPol)= projection #hier verpasse ich dem ganzen nochmals die gleiche Projection

coordinates(mSMiRas)
coordinates(KreisPol) #gibt die coordinaten der Mittelpunkte der Polygone wieder

extent(KreisPol)
extent(mSMiRas)

plot(mSMiRas,690)
plot(KreisPol, add=T)

#### RGDAL Package: Einlesen der Kreise mit Hilfe des rgdal Pakets ####
KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/","vg2500_krs", verbose=T)
names(KreisPolOGR)
KreisPolOGR # auch ein SpatialPolygonsDataFrame
projection(KreisPolOGR)= projection
plot(KreisPolOGR)
###############################################################################

#### Plot mit Trellis:Lattice ####
#### Add adminstrative borders using the spatia lines
#### Hier vergleich ich die Aprils verschiedener Jahre
# april <-   levelplot(mSMiRasM, layers=c(580,592,604,616,628,640,652,664,676,688,700,712), par.settings=RdBuTheme)
# Alternative Auswahl der Layers
april <-   levelplot(mSMiRasM, layers=c("Apr.2004", "Apr.2005"), par.settings=RdBuTheme)
april
aprilKrs<-april + layer(sp.lines(KreisLine, lwd=0.8, col='darkgray'))
aprilKrs

#### Geht das auch mit SpatielPolygons? ####
# aprilKrsPol<-april + layer(sp.polygons(KreisPol))
# aprilKrsPol
# NEIN!!!

#### 
# year200304<-levelplot(mSMiRasM, layers=seq(652,676,by=1), par.settings=RdBuTheme)
# year200304
# year200304Kr<-year200304 + layer(sp.lines(KreisLine, lwd=0.8, col='darkgray'))
# year200304Kr

#### Scatterplots and Histograms ####
#### xyplot 
# names(mSMiRasM)
# xyplot(Apr.2003~Mai.2003|cut(y,4), data=mSMiRasM, auto.key=list(space='right'))
# Das macht sicherlich bei der Analyse später noch Sinn!
# xyplot(data=aprilKrs)
##############################################################################

#### EXTRACT der mSMI Data to Landkreis Niveau von layer 540 an ####

# mSMiExAvg<-extract(mSMiRas, KreisPol,na.rm=T, fun=mean, weights=T, layer=540) #kommt als Large materix format
# dauert sehr lange wenn nicht auf dem Cluster, daher file gespeichert
# write.csv(mSMiExAvg,"./data/data_raw/mSMiExAvg")  

#### Laden der gespeichertet Extrahierten mSMI ####
###load
# mSMiAVG<-as.matrix(read.csv("./data/data_raw/mSMiExAvg"))
# row.names(mSMiAVG)

mSMiExAvg <-read.csv("./data/data_raw/mSMiExAvg") # Große Frage, warum ist das vorher als Matrix eingelesen habe?
str(mSMiExAvg, max.level=1)
row.names(mSMiExAvg) <- 0:411
mSMiExAvg$X <- NULL
row.names(mSMiExAvg)
names(mSMiExAvg)[1] <- "row.names"

names(mSMiExAvg) <- idx[540:720]

str(mSMiExAvg, max.level=2)

# mSMiExAvg <-mSMiAVG[,c(2:ncol(mSMiAVG))]

# Auslesen und speichern einzelner Layer
# mSMiExAvg
# mSMiExAvg[,"X540"]
# mSMiExAvg540<-mSMiExAvg[,"X540"]
# mSMiExAvg540 #nur noch eine numerische Liste
# plot(mSMiExAvg540) 

# levelplot(mSMiExAvg540)   #geht nicht, da nur numerische Liste, keine Info mehr zu Coordinaten oder Ähnlichen

View(mSMiExAvg[,"X540"])


### Was fehlt dem Datensatz?

coordinates(mSMiExAvg)#scheinbar haben sich die Koordinaten verändert, irgendwie ist hier eine Null davor
coordinates(KreisPolOGR) 


#### SpCbind ####
KreisPolOGR
nrow(KreisPolOGR) #Kreispol hat 412 rows
row.names(KreisPolOGR) # 0:411
# spChFIDs(KreisPol) <- 0:411

View(KreisPol)
FIPS<-row.names(KreisPol)
FIPS
length(FIPS)

nrow(mSMiExAvg)
# mSMiExAvg_df<-as.data.frame(mSMiExAvg)
row.names(mSMiExAvg) <- FIPS
row.names(mSMiExAvg)

## das bedeutet, meine Data Frame benötigt die gleiche row names
# mSMiExAvg_df<-as.data.frame(mSMiExAvg)
# str(mSMiExAvg_df)
# mSMiExAvg_df
# FIPS<-row.names(KreisPol)
# FIPS
# length(FIPS)

# eventuell kann es problematisch sein, dass die row.names bei Null beginnen, da R scheibar die Indezierung bei eins beginnnt
#aber: row.names(KreisPol) lassen sich nicht so einfach ändern

# 
# row.names(mSMiExAvg_df)<-FIPS
# row.names(mSMiExAvg_df)
# mSMiExAvg_df

#######################################################################################
########################################################################################
###spCbind um die Polygon mit data frame zu verbinden

KreismSMiExAvg<-spCbind(KreisPolOGR, mSMiExAvg)
str(KreismSMiExAvg)
View(KreismSMiExAvg)
coordinates(KreismSMiExAvg)
attributes(KreismSMiExAvg)[,"X640"]

########################################################
###### Make Names ########

# idx[540:720] ## idx wurde weiter oben erzeugt
# idx.names<-make.names(idx)
# idx.names
# names(KreismSMiExAvg)[c(6:length(names(KreismSMiExAvg)))]
# names(KreismSMiExAvg)[c(6:length(names(KreismSMiExAvg)))] <- idx.names[540:720]
# View(KreismSMiExAvg)
# ?make.names

# Datumsangaben als Jahreszahlen
# 
# idx[540:720] ## idx wurde weiter oben erzeugt
# names(KreismSMiExAvg_df)[c(6:length(names(KreismSMiExAvg_df)))] <- as.character(idx[540:720])

##############################
#### Plotten von KreisSMiExAvg #####

plot(KreismSMiExAvg)
plot(KreismSMiExAvg@data) #wahrscheinlich zu viele Dimensionen
plot(KreismSMiExAvg@data$X540)
KreismSMiExAvg@data$SHAPE_AREA

str(KreismSMiExAvg, max.level=2)
str(as(KreismSMiExAvg, "data.frame"))
# str(model.frame()) # man kann sich die Struktur eines Models anzeigen lassen und funktioniert mit fitting Funktionen, zum Beispiel GLM
# olinda$Expected <- olinda$POP * sum(olinda$CASES, na.rm = TRUE)/sum(olinda$POP,
# +  na.rm = TRUE)
# # number of rows and columns adapts

KreismSMiExAvg[,c("USE","RS","GEN","SHAPE_LENG", "SHAPE_AREA","X640")]
names(KreismSMiExAvg)

### April 1996 - 2010 ###
spplot(KreismSMiExAvg,zcol=c("Apr.1996", "Apr.1996", "Apr.1997","Apr.1998","Apr.1999","Apr.2000","Apr.2001","Apr.2002","Apr.2003","Apr.2004","Apr.2005", "Apr.2006","Apr.2007","Apr.2008","Apr.2009", "Apr.2010") ,
       at=seq(0, 1, length=1000) , col.regions= colorRampPalette(c("red", "yellow", "green", "blue"))(1e3) , main="Soil Moisture in Germany, April 1996-2010")

### Year 2003 ###
spplot(KreismSMiExAvg,zcol=c("Jan.2003","Feb.2003","Mär.2003","Apr.2003","Mai.2003","Jun.2003","Jul.2003","Aug.2003","Sep.2003","Okt.2003","Nov.2003","Dez.2003") ,
       at=seq(0, 1, length=1000) , col.regions= colorRampPalette(c("red", "yellow", "green", "blue"))(1e3) , main="Soil Moisture in Germany, Year=2003")

?spplot
###############################################################

####Schreiben der erzeugten Datei ####
#Scheinbar habe ich es geschafft, die Bodenfeuchte auf entsprechender Kreislicher Ebende darzustelllen
summary(KreismSMiExAvg@data)
getwd()
writePolyShape(KreismSMiExAvg,"./data/data_raw/KreismSMiExAvg")

##########################################################################################





##########################################################################################

######################################################

# Examine Structur and content of shapefile
summary(KreismSMiExAvg)
str(as.data.frame(KreismSMiExAvg))
attributes(KreismSMiExAvg)
attributes(KreismSMiExAvg$data)
attributes(KreismSMiExAvg$data$X541)
KreismSMiExAvg@data$X540
# KreisShp@data
# KreisPol@data
# merge<-merge(KreisPol, mSMiExAvg)
# str(merge)
# mergeShape<- merge(KreisShp, mSMiExAvg)
# mergePol<- cbind(KreisPol@data, mSMiExAvg)
# plot(mergeShape)
# 

#### Some simple maps

### Oslo SS Lecture 1, Olinda Example
names(KreismSMiExAvg)
str(KreismSMiExAvg)
plot(KreismSMiExAvg@data$X540,col.regions=grey.colors(20, 0.9, 0.3 ) )

#vll gibt es auch ein Problem mit den IDS, sodass diese nicht matchen
nrow(KreismSMiExAvg)
is.na(KreismSMiExAvg$X541) 
str(as(KreismSMiExAvg, "data.frame"))

# Mal die dbf file seperat anschauen
KreismSMiExAvg.dbf<-read.dbf(file.choose(), as.is = FALSE)

#KreisSmiExAvg als Dataframe
KreismSMiExAvg_df<-as.data.frame(KreismSMiExAvg)

####Aus dem Oregon Tutorial
plotvar <- KreismSMiExAvg@data$X540 # gets data from shapefile .dbf

plotvar <- rank(KreismSMiExAvg$X540)
nclr <- 8
plotclr <- brewer.pal(nclr,"PuOr")
colornum <- cut(plotvar, nclr, labels=FALSE)
colorcode <- plotclr[colornum]
plot(KreismSMiExAvg, colorcode <- plotclr[colornum], xlab="Latitude", ylab="Longitude")
####################################





#############################################################################################################
####Rasterize Kreis Shape
#Kreis<-rasterize(Kreis.shp, nrow=225, ncol=175, res=4000, xmin=4038000, xmax = 4738000, ymin = 5220000,ymax = 6120000, projection )


ext <- extent(4038000, 4738000, 5220000, 6120000)
KreisRaster<- raster(ext, nrow=225, ncol=175)
KreisRaster


KreisShpRas<-rasterize(KreisShp, KreisRaster)
KreisShpRas
View(KreisShpRas)
writeRaster(KreisShpRas, "/home/micha/Documents/frontend_share/projects/correlation/correlation/data/data_raw/KreisShpRas")
#################################################

# getwd()
str(Kreis)
Kreis
smi690
res(Kreis)<- 4000


writeRaster(Kreis, "testRaster2")
getwd()

Kreis_pol<-readShapeSpatial(Kreis)
extract<- extract(smi690, Kreis)
str(Kreis)
image(Kreis)

SMI <- raster(SMI.slice1)
str(SMI)
print(SMI)
print(Kreis.shp)
SMI
SMI.data<-as.data.frame(SMI)
filename(SMI)
hasValues(SMI)
inMemory(SMI)
cellStats(SMI, stat="min")
cellStats(SMI, stat="max")

#recode values < 0
SMI[SMI<0]<-0
#basic plot
levelplot(SMI)

# plot with outlines, a better color scale, no marginal plots
mapTheme <- rasterTheme(region=brewer.pal(8,"Greens"))
plt <- levelplot(SMI, margin=F,  par.settings=mapTheme)
warnings()
plt + layer(sp.lines(Kreis.shp, col="gray", lwd=0.5))

# plot with outlines, a better color scale, marginal plots
mapTheme <- rasterTheme(region=brewer.pal(8,"Reds"))
plt <- levelplot(SMI, margin=T, par.settings=mapTheme)

plt + layer(sp.lines(Kreis.shp, col="blue", lwd=0.9))

#The rasterVis package provides a couple of interesting Lattice-type 
#plots that can be used to visualize 3-D data  (usuallya function of  
#latitude, longitude and time).  The HovmÃ¶ller plot is a 2-D time/space
#diagram, where, for example, zonal or meridional averages are plotted 
#relative to time.  The horizon plot plots multiple time series 
#(here average values for individual latitudinal zones) in a way that 
#allows common trends to be visualized. c