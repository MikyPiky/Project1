#### File Description ####
'
Diese File hier ist der Vorgänger der 4km_SMI. Hier sind die SMI Werte direct extrahiert worden durch Stephan Thober. In der neuen file 4km_SMI werden die Bodenfeuchte Werte
disaggregiert, maskiert, und dann extrahiert. Danach erst wird der SMI berechnet. Da es sich bei SMI um ein Quantil, also relative Werte handelt, sollte man nach Meinung von Stephan
erst mit der Bodenfeuchte arbeiten, da dieses die Variation weniger beeinflusst.

'


library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(plyr)

############################################################
#### Laden des Weights NetCDF ####
weights<-raster("data/data_raw/weights.nc", varname="smi_lk")


#### Falsche Projection löschen ####
projection(weights)<-NA

#### Explorative Stuff ####
View(weights)
levelplot(weights)

names(weights)
dim(weights)

############################################
#### Datensatz transpose und Spaltenreihenfolge umkehren ####

weights_m<-as.matrix(weights)
dim(weights_m)
View(weights_m)

#### Transponieren 
weights_m_t<-t(weights_m)
dim(weights_m_t)
levelplot(weights_m_t, main="matrix transponiert")
head(weights_m_t)[,720]

#### Spaltenreihenfolge umkehren
weights_m_t_r<-weights_m_t[,ncol(weights_m_t):1]
dim(weights_m_t_r)
levelplot(weights_m_t_r, main="matrix transponiert und Spaltenreihenfolge umgekehrt")
tail(weights_m_t_r)[,720]


################################################
#### Datensatz an Spatialpolygondataframe anhängen ####

#### Matrix in Dataframe
weights_df_t_r<-as.data.frame(weights_m_t_r)
dim(weights_df_t_r)
#### Rownames anpassen
rownames(weights_df_t_r)<-0:411

#### Laden des SpatialPolygonDataFrames
KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/","vg2500_krs")
dim(KreisPolOGR)

#### Maptools::spCbind
KreisWeights_df_t<-spCbind(KreisPolOGR,weights_df_t_r)

#### Export der erstellten Datei
writePolyShape(KreisWeights_df_t, "./data/data_raw/KreisWeights_df_t.shp")

KreismSMiExAvgMask2<-readShapePoly("./data/data_raw/KreisWeights_df_t.shp")

#### Projection des ursrünglichen SpatialPolygonsDataFrame übertragen
proj4string(KreisWeights_df_t)<-proj4string(KreisPolOGR)

###################################################################
#### Plotten der erstellen SpatialPolygonsDataFrame ####

names(KreisWeights_df_t)

at=(seq(0,1,0.01))

KreisWeights_df_t_Plt1 <- spplot(KreisWeights_df_t, zcol="V720",at=at,col.regions= colorRampPalette(c("red", "yellow", "green", "blue"))(1e3) , main="SMI Germany, Kreise, aus 100*100m masked raster")
plot(CLCrasterMaskSmall, add=T)

KreisWeights_df_t_Plt2 <- spplot(KreisWeights_df_t, zcol="V720",at=at,col.regions= colorRampPalette(c("red", "yellow", "green", "blue"))(1e3) , main="SMI Germany, Kreise, aus 100*100m masked raster")

KreisWeights_df_t_Plt3 <- spplot(KreisWeights_df_t, zcol=c(paste("V", (715:720), sep="")),at=at,col.regions= colorRampPalette(c("red", "yellow", "green", "blue"))(1e3) , main="SMI Germany, Kreise, aus 100*100m masked raster")

# Pattern stimmen mit orginal Daten überein 


