#### File Description ####
' - Transformation der NetCDF Datei mit den SMI Werten in time series für die einzelnen Landkreise.
  - Stephans Fortran Program
    - Bodenfeuchte (4*4 km) maskiert für non- irrigated agricultural land (CLC06, class 12, 100*100 m)  
    - und danach wieder hochgerechnet auf Polygone der administrative districts(vg2500_krs).
  - Zusammenführen dieses Datensatzes mit SpatialPolygonDataframe (vg2500_krs) um räumlichen Bezug herzustellen.
  - Plotten mit ssplot{sp}
  - Plot der Spatial Transformation (ursprünglicher SMI -> SMI disaggregiert und maskiert -> SMI auf Polynome hochgerechnet)
'
#### Output ####
##Files
' - SpatialPolygonsDataFrame: KreisSMI_spdf (ganzer Zeitraum), SMI_months.csv
  - geht in MergeSMI_Yield ein
'
## Plots
' - Plots der Season von Maize für alle Jahre.
  - Vergleichende Plots des Spatial Processing. 
'

#### Dependencies and Input ####
'mSMI.nc (SMI als Polynome, bereitgestellt von Stephan Thober)
 vg2500_krs (extern bereitgestellt)
 ExtractSMI.R: raster der SMI May 2003, einmal in 4km² und einmal 100m² mit Maske für Landwirtschaft.'

#### Packages ####
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(plyr)
library(ncdf4)
library(zoo)
library(foreign)
library(maps)
library(colorspace)
library(lattice)
library(latticeExtra)
library(stringr)

########################################################################################################################################################################################
########################################################################################################################################################################################
getwd()

##############################
#### Laden des SMI NetCDF ####
##############################
# SMI<-brick("/home/peichl/Documents/projects/correlation/data/data_raw/4km_SMI.nc", varname="SMI")
# ## Funktioniert mit dieser File nicht



#### Alternatives Laden: ncdf4 ####
SMI_open <- nc_open("/Storage/ownCloud/Home/Klimabuero/Proj1/data/data_raw/mSMI.nc")
print(SMI_open)
# Hier habe ich 780 Monate oder 65 Jahre seit einschließlich Januar 1951 bis Dezember 2015

SMI <- ncvar_get(SMI_open, varid="SMI")
nc_close(SMI_open)
SMI

dim(SMI)
class(SMI)
# levelplot(SMI)
str(SMI)

####################################################################################
#### Laden der shape mit den Polygonen der Kreise und deren räumliche Zuordnung ####
####################################################################################
KreisPolOGR <- readOGR("/Storage/ownCloud/Home/Klimabuero/Proj1/data//data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")


##################################################################
#### Datensatz als Matrix um 0 Werte in NAs zu transformieren ####

SMI_m <- as.matrix(SMI)
dim(SMI_m)
head(SMI_m)
any(SMI_m == -9999)
any(SMI_m == 9999)

#### 0 Werte in NA ####
any(SMI_m == 0)
u <- SMI_m == 0
SMI_m[u] <- NA
# levelplot(SMI_m)



###########################################################################################################
#### Datensatz an Spatialpolygondataframe der Landkreise anhängen, um räumliche Beziehung herzustellen ####
###########################################################################################################

#### Matrix in Dataframe ###
SMI_df <- as.data.frame(SMI_m)
dim(SMI_df)
remove(SMI_m)
remove(SMI)

#### Verändern der Namen ####
names(SMI_df)
idx <-seq(from=as.Date('1951-1-1'), to=as.Date('2015-12-1'), 'month')
idx <-as.yearmon(idx)
idx_str<-paste("SM",idx, sep="")
idx_names<-make.names(c(idx_str), unique = TRUE)
idx_gsub<-gsub(" ","",idx_str)
idx_names<-make.names(c(idx_gsub), unique = TRUE)
idx_names
length(idx_names)
length(names(SMI_df))
names(SMI_df)<-idx_names

#### Rownames anpassen ####
rownames(SMI_df) <-0:411


#############################################
## Delete German Expression in Columnnames ##
head(SMI_df)
names(SMI_df)
names(SMI_df) <- chartr("ä","a",names(SMI_df))
names(SMI_df) <- chartr("z","c",names(SMI_df))
names(SMI_df) <- chartr("Mai","May",names(SMI_df))
names(SMI_df) <- chartr("Okt","Oct",names(SMI_df))
names(SMI_df)



# ######################################
# #### Data.frame: 1995 produzieren ####
length(SMI_df)
length(SMI_df) / 12
# 
# # Die letzten 21 Jahre sollen ausgelesen werden. Also 2015 -1995.
# # Bei längeren Zeitperioden muss das entsprechend angepasst werden. 
# 21*12
# 
names(SMI_df)[(length(SMI_df)-(21*12-1)) : length(SMI_df)]
SMI_df_1995 <- SMI_df[,(length(SMI_df)-(21*12-1)):length(SMI_df)]
names(SMI_df_1995)
rownames(SMI_df_1995) <- 0:411
# 
# #### Combine with Spatial Information
KreisSMI_spdf <- spCbind(KreisPolOGR,SMI_df)
KreisSMI_spdf_1995 <- spCbind(KreisPolOGR,SMI_df_1995)

# ######################################
# #### Data.frame: 1999 produzieren ####
names(SMI_df)[(length(SMI_df)-(17*12-1)):length(SMI_df)]
SMI_df_1999<-SMI_df[,(length(SMI_df)-(17*12-1)):length(SMI_df)]
names(SMI_df_1999)

#### Combine with Spatial Information
KreisSMI_spdf_1999 <- spCbind(KreisPolOGR,SMI_df_1999)

# ###################################################################
# #### Export der erstellten Dateien (SpatialPolygoneDataFrames) ####
# ## ... für den gesamten Zeitraum
writePolyShape(KreisSMI_spdf, "./data/data_raw/KreisSMI_spdf.shp")
# ## ... ab 1995
writePolyShape(KreisSMI_spdf_1995, "./data/data_raw/KreisSMI_spdf_1995.shp")
# ## ... ab 1999
writePolyShape(KreisSMI_spdf_1999, "./data/data_raw/KreisSMI_spdf_1999.shp")

###################################################################
#### Import der erstellten Dateien (SpatialPolygoneDataFrames) ####

KreisSMI_spdf<-readShapePoly("./data/data_raw/KreisSMI_spdf.shp")
# KreisSMI_spdf_1995 <-readShapePoly("./data/data_raw/KreisSMI_spdf_1995.shp")
KreisSMI_spdf_1999 <-readShapePoly("./data/data_raw/KreisSMI_spdf_1999.shp")
# KreisPolOGR <- readOGR("/Storage/ownCloud/Home/Klimabuero/Proj1/data//data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")
# names(KreisSMI_spdf)
# names(KreisSMI_spdf_1995)
 names(KreisSMI_spdf_1999)
# head(KreisSMI_spdf_1995)[1:7]

'Wenn man neu einlädt dan bildet sich die SP_ID'

#### Projection des ursrünglichen SpatialPolygonsDataFrame übertragen
 proj4string(KreisSMI_spdf_1999)  <-proj4string(KreisPolOGR)

####################################################################################################################################################################
####################################################################################################################################################################

########################################################
#### Plotten der erstellen SpatialPolygonsDataFrame ####
########################################################

#########################################S
## Make intervalls for coloring scheme ## 
at1=(seq(0,1,0.01))
at2=c(0.0, 0.10,0.20, 0.30,0.70, 0.80, 0.90, 1.00) # SMI Anomalie nach Drought Monitor

########################
## Setup Colourscheme ##
cs1 <- colorRampPalette(c("tomato4","tan4","tan2","tan1", "chartreuse4","darkgreen","chartreuse4","dodgerblue1","dodgerblue2","dodgerblue4", "darkblue"))(1e3)
## Used for publications 
# cs2 <- colorRampPalette(c("#543005","#8c510a", "#bf812d","#e8edd7","#35978f","#01665e", "#003c30"))(7)
#
cs2 <- colorRampPalette(brewer.pal(11,"RdYlBu"))(7)

##########################S
## Modify trellis theme ##
my.theme = trellis.par.get()
names(my.theme)
my.theme$panel.background

trellis.par.set("background", list(col = "white")) 
trellis.par.set("panel.background", list(col = "white")) 
trellis.par.set("strip.background", list(col = "white")) 
trellis.par.set("fontsize", list(text=30, cex=1)) 

# trellis.par.set("strip.border", list(lty=c(1,1,1,1,1,1,1))) 
# trellis.par.set("axis.line", list(col="#000000")) 

my.theme$strip.background
my.theme$axis.line
my.theme$strip.border
# my.theme$strip.border$col <- c("#000000", "#000000","#000000", "#000000", "#000000", "#000000","#000000")

show.settings()

trellis.par.get("fontsize")

#####################################################################################################################################################################
#####################################################################################################################################################################

###############
#### 1999 ####
## Setup Months considered ##
# zteins <- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)-48]
zt1999 <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)-49]
## Exported als png im format width 800 and height 800
KreisSMI_spdf_Plt2.99 <- spplot(KreisSMI_spdf_1999, zcol=zt1999,at=at2,col.regions= cs2)
# KreisSMI_spdf_Plt2.99

png("./figures/figures_exploratory/SMI/SMI_Anomaly_1999_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.99)
dev.off()

###############
#### 2000 ####
## Setup Months considered ##
zt2000<- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)-36]
zt2000<- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)-36]

KreisSMI_spdf_Plt2.2000 <- spplot(KreisSMI_spdf_1999, zcol=zt2000,at=at2,col.regions= cs2)
# KreisSMI_spdf_Plt2.2000

png(filename = "./figures/figures_exploratory/SMI/SMI_Anomaly_2000_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2000)
dev.off()


###############
#### 2001 ####
## Setup Months considered ##
zt2001 <- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)-24]
zt2001 <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)-24]

KreisSMI_spdf_Plt2.2001 <- spplot(KreisSMI_spdf_1999, zcol=zt2001,at=at2,col.regions= cs2)
# KreisSMI_spdf_Plt2.2001

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2001_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2001)
dev.off()


###############
#### 2002 ####
## Setup Months considered ##
zt2002 <- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)-12]
zt2002 <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)-12]

KreisSMI_spdf_Plt2.2002 <- spplot(KreisSMI_spdf_1999, zcol=zt2002,at=at2,col.regions= cs2)
# KreisSMI_spdf_Plt2.2

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2002_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2002)
dev.off()


###############
#### 2003 ####
## Setup Months considered ##
# ztdrei<- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)]
zt2003<- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)]

KreisSMI_spdf_Plt2.2003 <- spplot(KreisSMI_spdf_1999, zcol=zt2003,at=at2,col.regions= cs2)
# KreisSMI_spdf_Plt2.2003

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2003_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2003)
dev.off()

# ## Plot des Mai 2003 der extrahierten SMI Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
names(KreisSMI_spdf_1999)
KreisSMI_spdf_Plt3 <- spplot(KreisSMI_spdf_1999, zcol="SMMay2003",at=at2, col.regions= cs2)


###############
#### 2004 ####
## Setup Months considered ##
# ztvier<- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+12]
zt2004<- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)+12]

KreisSMI_spdf_Plt2.2004 <- spplot(KreisSMI_spdf_1999, zcol=zt2004,at=at2,col.regions= cs2)
# KreisSMI_spdf_Plt2.4 

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2004_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2004)
dev.off() 



###############
#### 2005 ####
## Setup Months considered ##
zt2005<- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+24]
zt2005<- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)+24]

KreisSMI_spdf_Plt2.2005 <- spplot(KreisSMI_spdf_1999, zcol=zt2005,at=at2,col.regions= cs2)
# KreisSMI_spdf_Plt2.5 

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2005_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2005)
dev.off()



###############
#### 2006 ####
## Setup Months considered ##
# ztsechs<- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+36]
zt2006<- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)+36]

KreisSMI_spdf_Plt2.2006 <- spplot(KreisSMI_spdf_1999, zcol=zt2006,at=at2,col.regions= cs2)
# KreisSMI_spdf_Plt2.6

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2006_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2006)
dev.off()



###############
#### 2007 ####
## Setup Months considered ##
# ztsieben<- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+48]
zt2007 <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)+48]

KreisSMI_spdf_Plt2.2007 <- spplot(KreisSMI_spdf_1999, zcol=zts2007,at=at2,col.regions= cs2)
# KreisSMI_spdf_Plt2.7

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2007_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2007)
dev.off()


###############
#### 2008 ####
## Setup Months considered ##
zt2008 <- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+60]
zt2008 <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)+60]

KreisSMI_spdf_Plt2.2008 <- spplot(KreisSMI_spdf_1999, zcol=zt2008 ,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2008_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2008)
dev.off()


###############
#### 2009 ####
## Setup Months considered ##
# ztsieben<- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+72]
zt2009 <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)+72]

KreisSMI_spdf_Plt2.2009 <- spplot(KreisSMI_spdf_1999, zcol=zt2009,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2009_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2009)
dev.off()


###############
#### 2010 ####
## Setup Months considered ##
zt2010 <- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
zt2010 <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)+84]

KreisSMI_spdf_Plt2.2010 <- spplot(KreisSMI_spdf_1999, zcol=zt2010,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2010_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2010)
dev.off()

###############
#### 2011 ####
## Setup Months considered ##
zt2011 <- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
zt2011 <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)+96]

KreisSMI_spdf_Plt2.2011 <- spplot(KreisSMI_spdf_1999, zcol=zt2011,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2011_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2011)
dev.off()


###############
#### 2012 ####
## Setup Months considered ##
# zt2012  <- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
zt2012  <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)+108]

KreisSMI_spdf_Plt2.2012 <- spplot(KreisSMI_spdf_1999, zcol=zt2012 ,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2012_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2012)
dev.off()


###############
#### 2013 ####
## Setup Months considered ##
# zt2013 <- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
zt2013 <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)+120]


KreisSMI_spdf_Plt2.2013 <- spplot(KreisSMI_spdf_1999, zcol=zt2013,at=at2,col.regions= cs2)
png("./figures/figures_exploratory/SMI/SMI_Anomaly_2013_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2013)
dev.off()

###############
#### 2014 ####
## Setup Months considered ##
# zt2014 <- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
zt2014 <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61)+132]

KreisSMI_spdf_Plt2.2014 <- spplot(KreisSMI_spdf_1999, zcol=zt2014,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/SMI/SMI_Anomaly_2014_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2014)
dev.off()


###############
#### 2015 ####
## Setup Months considered ##
# zt2015 <- names(KreisSMI_spdf_1999)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
zt2015 <- names(KreisSMI_spdf_1999)[c(62,63,64,59,60,61) + 144]

KreisSMI_spdf_Plt2.2015 <- spplot(KreisSMI_spdf_1999, zcol=zt2015,at=at2,col.regions= cs2)
png("./figures/figures_exploratory/SMI/SMI_Anomaly_2015_short.png", width = 800, height = 800, units = "px",)
plot(KreisSMI_spdf_Plt2.2015)
dev.off()

############################################################################################################################################################################################
############################################################################################################################################################################################
###############################################
#### Make Plots to show spatial processing ####
###############################################

######################################
## Latlon Projection der Kreisdaten ##
## Polynomial Data  (extracted)
KreisSMI_spdf_1999_ll <- spTransform(KreisSMI_spdf_1999, CRS("+proj=longlat +datum=WGS84"))
' This data are generated in this file beforehand. '
names(KreisSMI_spdf_1999_ll)

################################
#### Load Data for plotting ####
' Those data are generated in the file ExtractSMI '

#######################################
## not mamipulated data of year 2003 ##

# mSMi2003 <- brick("data/data_raw/mSMiMai2003DisMask.gri")
mSMi2003 <- raster("./data/data_raw/mSMiMai2003")
mSMi2003
str(mSMi2003)
crs(mSMi2003)
spplot(mSMiMai2003)

# ## Read Data for Mai 2003 ##
# mSMiMai2003 <- raster(mSMi2003, layer=5)

## Change projection to lat lon for figures
mSMiMai2003_ll <- projectRaster(mSMiMai2003, crs=CRS("+proj=longlat +datum=WGS84"))
mSMiMai2003_ll
spplot(mSMiMai2003_ll)

## Crop Plot ##
mSMiMai2003_ll_crop <- crop(mSMiMai2003_ll, extent(KreisSMI_spdf_1999_ll) )

spplot(mSMiMai2003_ll_crop)

#### 0 Werte in NA ####
any(values(mSMiMai2003_ll_crop) == 0.0000000)
u <- (values(mSMiMai2003_ll_crop) == 0.0000000)
values(mSMiMai2003_ll_crop)[u] <- NA

spplot(mSMiMai2003_ll_crop)
'Achtung, die Daten in der 100*100 Auflösung  sind alle sehr groß, wenn sie entpackt sind. Daher sind nur die tars gespeichert'

###################################
## disaggregated and masked data ##
## Year 2003 ##
mSMiMai2003DisMask <- raster("data/data_raw/mSMiMai2003DisMask")
mSMiMai2003DisMask

# mSMiMai2003DisMask <- brick("data/data_raw/mSMiMai2003DisMask.nc")

## May 2003 ##
# mSMiMai2003DisMask  <- raster(mSMiMai2003DisMask, layer=5)
mSMiMai2003DisMask_ll <- projectRaster(mSMiMai2003DisMask, crs=CRS("+proj=longlat +datum=WGS84"))

mSMiMai2003DisMask_ll_crop <- crop(mSMiMai2003DisMask_ll, extent(KreisSMI_spdf_1999_ll) )
spplot(mSMiMai2003DisMask_ll_crop)

########################################################################
## Change Projection of Kreis Polynomials to be able to include those ##
KreisPolOGR_ll <- spTransform(KreisPolOGR, CRS("+proj=longlat +datum=WGS84"))


###############
#### Plots ####
###############
at2=c(0.0000, 0.10,0.20, 0.30,0.70, 0.80, 0.90, 1.00) # SMI Anomalie nach Drought Monitor

extent(mSMiMai2003_ll_crop) <-   extent(mSMiMai2003DisMask_ll_crop) 
extent(mSMiMai2003_ll_crop) 
extent(mSMiMai2003DisMask_ll_crop) 
extent(KreisSMI_spdf_1999_ll) <- extent(mSMiMai2003DisMask_ll_crop) 
#######################################################################################
## Plotten des Mai 2003 nicht disaggregiert, also auf 4000*4000 m mit Kreispolygonen ##
mSMi2003Plt <- spplot(mSMiMai2003_ll_crop ,at=at2, col.regions= cs2,   scales=list(x=list(at=c(7,9,11,13), labels=c("7°E","9°E","11°E","13°E")), y=list(at=c(48,50,52,54), labels=c("48°E","50°E","52°E","54°E"))))
# scales=list(draw=T) ,
mSMi2003Plt
mSMi2003Plt <- mSMi2003Plt + layer(sp.polygons(KreisPolOGR_ll, col="black", lwd=1))
mSMi2003Plt

###################################################################################
## Plotten des Mai 2003, disaggregiert auf 100*100m, maskiert mit Kreispolygonen ## 
mSMi2003DisPltmaskPlt <- spplot(mSMiMai2003DisMask_ll_crop , at=at2,col.regions= cs2, scales=list(x=list(at=c(7,9,11,13), labels=c("7°E","9°E","11°E","13°E")), y=list(at=c(48,50,52,54), labels=c("48°E","50°E","52°E","54°E"))))
mSMi2003DisPltmaskPlt
mSMi2003DisPltmaskPlt <- mSMi2003DisPltmaskPlt + layer(sp.polygons(KreisPolOGR_ll, col="black", lwd=1))
mSMi2003DisPltmaskPlt


## Plot des Mai 2003 der extrahierten SMI Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
KreisSMI_spdf_Plt3 <- spplot(KreisSMI_spdf_1999_ll, zcol="SMMay2003",at=at2, col.regions= cs2, scales=list(x=list(at=c(7,9,11,13), labels=c("7°E","9°E","11°E","13°E")), y=list(at=c(48,50,52,54), labels=c("48°E","50°E","52°E","54°E"))))
KreisSMI_spdf_Plt3 
str(KreisSMI_spdf_1999, 2)

###################
## Combine Plots ##
c(mSMi2003Plt, mSMi2003DisPltmaskPlt, KreisSMI_spdf_Plt3, x.same=T,  y.same=T, layout=c(3,1))




####################################################################################################################################################################
################################################################### Kippen des Datensatzes #########################################################################
####################################################################################################################################################################

###################################################################
#### Jetzt muss ich noch den SMI-Datensatz entsprechend kippen ####
###################################################################
head(KreisSMI_spdf_1995)
##########################

##########################
#### reshape::reshape ####

#### SpatialPolygonsDataFrame in normalen DataFrame ####
KreisSMI_df_1995 <- as.data.frame(KreisSMI_spdf_1995)
head(KreisSMI_spdf_1995)
names(KreisSMI_spdf_1995)

#### Checken, ob Namen sich nicht verändert haben
all(names(KreisSMI_df_1995)==names(KreisSMI_spdf_1995)) # check positive

length(names(KreisSMI_df_1995))
names(KreisSMI_df_1995)
dim(KreisSMI_df_1995)
21*12
## Beschreibung des Vorganges ##
'Ziel: Ich habe 412 Beobachtungen pro Zeitschritt (Jahr). 
Da mein Datensatz tidy ist, also die Jahre untereinander fortlaufend angeordnet sind, müssen meine Variablen entsprechend angepasst werden.
Soll heißen aus den SMI Variablen mit Monats und Jahres zuordnung wird nun ein SMI mit Monats Zuordnung und die Jahre verschmelzen alle in 
eine Spalte. 
Damit habe ich 12 Spalten für den SMI, also eine pro Monat, und 16 (Jahre) mal 412 Beobachtungen, also 6592 Beobachtungen.
Für dieses Vorgehen reshape ich den Kreis_SMI_dataframe von wide nach long mit Hilfe der stats::reshape function. 
Dazu muss ich eine Liste mit den SMI mit Monat und Jahres Zuordnung erstellen, welche dann jeweils in die SMI mit Monats zuordnung
im long Format übergeht. -> listMonthYearNames[[i]]
Anschließend erstelle ich die Namen der SMI mit reiner Monats Zurdnung -> listMonthNames
Mit Hilfe der Year Variablen werden dann die Beeobachtungen innerhalb der SMI mit Monats Zuordnungen nach den Jahren differenziert.
'

#### Make a list of vectors of months ####
' Diese list dient der Indizierung der jeweiligen Monate. Sie startet bei 7, da die 6 columns davor die räumlichen Einheiten definieren.'
length(names(KreisSMI_df_1995))


list0<-list(c(7,length(names(KreisSMI_df_1995))-11),
            c(8,length(names(KreisSMI_df_1995))-10),
            c(9,length(names(KreisSMI_df_1995))-9),
            c(10,length(names(KreisSMI_df_1995))-8), 
            c(11,length(names(KreisSMI_df_1995))-7),
            c(12,length(names(KreisSMI_df_1995))-6),
            c(13,length(names(KreisSMI_df_1995))-5),
            c(14,length(names(KreisSMI_df_1995))-4),
            c(15,length(names(KreisSMI_df_1995))-3), 
            c(16,length(names(KreisSMI_df_1995))-2), 
            c(17,length(names(KreisSMI_df_1995))-1), 
            c(18,length(names(KreisSMI_df_1995))))
# those list define the starting and ending point for the steps, 12 each
list0 


## Container 
listMonthYearNames <- list()

## Lenght of considered time period ##
i=1
length(seq(list0[[i]][1],list0[[i]][2],12))

#### Loop zum erstellen der Liste listMonthYearNames mit den IDs, welche jeweils den gleichen Monatesnamen, über alle Jahre hinweg, haben ####
for (i in 01:12){
  listMonthYearNames[i] <- list(c(names(KreisSMI_df_1995)[c(seq(list0[[i]][1],list0[[i]][2],12))]))
}
# class(listMonthYearNames[1])
# class(listMonthYearNames)
listMonthYearNames # list including a list of the listMonthYearNames across the years. This is the basis for the new variables, i.e the SMI in each month
listMonthYearNames[[3]] # list of all marches across the 21 years considered here

#### Make the names for the new variables
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
listMonthNames<-list(c("SMI_Jan"), c( "SMI_Feb"), c("SMI_Mar"), c("SMI_Apr"), c("SMI_May"), c("SMI_Jun"),c("SMI_Jul"),c("SMI_Aug"),c("SMI_Sep"), c("SMI_Oct"), c("SMI_Nov"), c("SMI_Dec"))
listMonthNames


### Reshape from wide to long für jeden Monat ####
# KreisSMI_df_19953 <- reshape(KreisSMI_df_1995, 
#              varying = listMonthYearNames[[1]],
#              v.names = listMonthNames[[1]],
#              timevar = "YearofJan", 
#              times = listMonthYearNames[[1]],
#              direction = "long")
# 
# names(KreisSMI_df_19953)[184]
# names(KreisSMI_df_19953)
# head(KreisSMI_df_19953)
# rownames(KreisSMI_df_19953)<-NULL
# rownames(KreisSMI_df_19953)

#### Loop, welche mir die SMI pro Monat jeweils in einer Column ausgibt ####

8652/21

y <- data.frame(1:8652) # set container
x <- data.frame() # set container

##############################################################################################################
### loop over the twelve listMonthYearNames, split up, an then filled up with the values across the years ####
for(i in 1:12) {
  x <- reshape (KreisSMI_df_1995, 
                varying = listMonthYearNames[[i]],
                v.names = listMonthNames[[i]],
                timevar = "Year", 
                idvar = "RS",
                times = listMonthYearNames[[i]],
                direction = "long")
  print(names(x[listMonthNames[[i]]])) # here I print the name of the newly created variable
  y <- cbind(y,x[listMonthNames[[i]]]) # here I make a dataframe with the twelfe newly created variables
  #               print(names(y)[i + 2])
  #               names(y)[i + 2]<-names(x)[184]
  #               print(c(names(x)[184],"FIN"))
}

## Explore x ##
head(x)
names(x)
dim(x)
names(x)[185]
head(x)[185]

## Explore the newly build dataframe y ##
names(y)
rownames(y)
## Get rid of non necessary variables
y$X1.8652<-NULL
head(y)


#### Erstellen eines data.frames mit ID angaben #### 
' x delievers spatial and time information, y the newly created tidy data'
dim(x)
dim(y)
names(x)[1:6]
names(x)[length(x) - 1]
names(y)
length(x) - 1

## Combine data.frames
SMI_months <- cbind(x[,c(1:4, length(x) - 1)],y)

names(SMI_months)
head(SMI_months)
rownames(SMI_months) <- NULL

## Explore whether comIds and years match ##
head(SMI_months, 15) # check
tail(SMI_months, 15) # check
# In den rownames steht der Monat Dezember, da dies der letzte Monat ist, der gemelted, also in eine Spalte gefasst wurde. 
# Sieht aber sonst recht ordentlich aus.

####################################################
## Change strings which are necessary for merging ##

## Befreien der year Variable von den Monatsangaben, hier Dezember
SMI_months$Year <- as.integer(str_sub(SMI_months$Year,6,9 ))

#### Verkürzen des ID-Strings, also KreisSMI_spdf_1995$RS  ####
str_length(SMI_months$RS) #12
SMI_months$RS <- as.factor(str_sub(SMI_months$RS,1,5))
str_length(SMI_months$RS) #5

unique(SMI_months$RS)


## Anpassen der Namen fürs mergen
colnames(SMI_months)[colnames(SMI_months)=="Year"] <- "year"
colnames(SMI_months)[colnames(SMI_months)=="RS"] <- "comId"
head(SMI_months)


##################################################################################
#### Abgleichen der neu erstellten Daten mit KreisSMI_df_1995 (orginal Daten) ####
head(KreisSMI_df_1995$SMJan1995)
head(SMI_months$SMI_Jan) 

tail(KreisSMI_df_1995$SMDec2015)
tail(SMI_months$SMI_Dec) 

## Erstellen und überprüfen der beiden Vektoren, die jeweils den SMI des Jahres 1999 im Monat Januar wiedergebeb


## Vector in SMI_months des SMI Januar 1999
SMI_months$SMI_Jan[SMI_months$year == 1999]
length(SMI_months$SMI_Jan[SMI_months$year==1999])

## Vector in KreisSMI_df_1995 des SMI Januar 1999
KreisSMI_df_1995$SMJan1999
length(KreisSMI_df_1995$SMJan1999)

match(KreisSMI_df_1995$SMJan1999,SMI_months$SMI_Jan[SMI_months$year==1999]) # check
all(SMI_months$SMI_Jan[SMI_months$year==1999]%in%KreisSMI_df_1995$SMJan1999) # check
# Daten sehen gut aus
names(KreisSMI_df_1995)
match(KreisSMI_df_1995$SMDez2015,SMI_months$SMI_Dec[SMI_months$year==2015]) # check
all(SMI_months$SMI_Dez[SMI_months$year==1999]%in%KreisSMI_df_1995$SMJan1999) # check

##########################################################################
#### Produce lagged variabled of SMI for the months after the harvest ####
##########################################################################
## SMI ##
names(SMI_months)
"Das erstellen der lags muss vor dem mergen passsieren, da yield nur bis 1999 geht."


SMI_months <- slide(SMI_months, Var ="SMI_Aug", GroupVar= "comId", slideBy = -1)
SMI_months <- slide(SMI_months, Var ="SMI_Sep", GroupVar= "comId", slideBy = -1)
SMI_months <- slide(SMI_months, Var ="SMI_Oct", GroupVar= "comId", slideBy = -1)
SMI_months <- slide(SMI_months, Var ="SMI_Nov", GroupVar= "comId", slideBy = -1)
SMI_months <- slide(SMI_months, Var ="SMI_Dec", GroupVar= "comId", slideBy = -1)

head(SMI_months)                                                

## Check for validity
head(SMI_months[SMI_months$year==1999,])[13:17] %in% head(SMI_months[SMI_months$year==2000,])[18:22]  
' Da zum Beispiel der August des Jahre 1999 mit dem lag des Jahres 2000 übereinstimmt, scheint alles zu passen'

table(SMI_months$comId)  

## Umbennen der lagged Variablen ##
names(SMI_months)
names(SMI_months)[names(SMI_months)==c("SMI_Aug-1" )]  <- c("SMI_Aug_lag")
names(SMI_months)[names(SMI_months)==c("SMI_Sep-1" )]  <- c("SMI_Sep_lag")
names(SMI_months)[names(SMI_months)==c("SMI_Oct-1" )]  <- c("SMI_Oct_lag")
names(SMI_months)[names(SMI_months)==c("SMI_Nov-1" )]  <- c("SMI_Nov_lag")
names(SMI_months)[names(SMI_months)==c("SMI_Dec-1" )]  <- c("SMI_Dec_lag")

head(SMI_months)
dim
###########################################
#### Write newly created SMI_months ####

write.csv(SMI_months, "data/data_processed/SMI_months.csv")


