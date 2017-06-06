#### File Description ####
' - Transformation der NetCDF Datei mit den Tavg Werten in time series für die einzelnen Landkreise.
  - Stephans Fortran Program
    - Bodenfeuchte (4*4 km) maskiert für non- irrigated agricultural land (CLC06, class 12, 100*100 m)  
    - und danach wieder hochgerechnet auf Polygone der administrative districts(vg2500_krs).
  - Zusammenführen dieses Datensatzes mit SpatialPolygonDataframe (vg2500_krs) um räumlichen Bezug herzustellen.
  - Plotten mit ssplot{sp}
'
#### Output ####
##Files
' - SpatialPolygonsDataFrame: KreisTavg_spdf (ganzer Zeitraum), Tavg_months.csv
  - geht in MergeTavg_Yield ein
'
## Plots
' Plots der Season von Maize für alle Jahre.
'

#### Dependencies and Input ####
'm Tavg.nc (Stephan Thober)
 vg2500_krs (extern bereitgestellt)'

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
library(stringr)

########################################################################################################################################################################################
########################################################################################################################################################################################
getwd()

##############################
#### Laden des Tavg NetCDF ####
##############################
# Tavg<-brick("/home/peichl/Documents/projects/correlation/data/data_raw/4km_Tavg.nc", varname="Tavg")
# ## Funktioniert mit dieser File nicht



#### Alternatives Laden: ncdf4 ####
Tavg_open <- nc_open("/Storage/ownCloud/Home/Klimabuero/Proj1/data/data_raw/Tavg_lk.nc")
print(Tavg_open)
# Hier habe ich 780 Monate oder 65 Jahre seit einschließlich Januar 1951

Tavg <- ncvar_get(Tavg_open, varid="tavg_lk")
nc_close(Tavg_open)


dim(Tavg) 
class(Tavg)
levelplot(Tavg)
str(Tavg)

####################################################################################
#### Laden der shape mit den Polygonen der Kreise und deren räumliche Zuordnung ####
####################################################################################
KreisPolOGR <- readOGR("/Storage/ownCloud/Home/Klimabuero/Proj1/data//data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")


##################################################################
#### Datensatz als Matrix um 0 Werte in NAs zu transformieren ####

Tavg_m <- as.matrix(Tavg)
dim(Tavg_m)
head(Tavg_m)
any(Tavg_m == -9999)
any(Tavg_m == 9999)
any(Tavg_m == 0)

#### 0 Werte in NA ####
any(Tavg_m == 0)
u <- Tavg_m == 0
Tavg_m[u] <- NA
levelplot(Tavg_m)

###########################################################################################################
#### Datensatz an Spatialpolygondataframe der Landkreise anhängen, um räumliche Beziehung herzustellen ####
###########################################################################################################

#### Matrix in Dataframe ###
Tavg_df <- as.data.frame(Tavg_m)
dim(Tavg_df)


#### Verändern der Namen ####
' Tavg starts one year earlier as SMI'
names(Tavg_df)
idx <-seq(from=as.Date('1950-1-1'), to=as.Date('2015-12-1'), 'month')
idx <-as.yearmon(idx)
idx_str<-paste("Tav",idx, sep="")
idx_names<-make.names(c(idx_str), unique = TRUE)
idx_gsub<-gsub(" ","",idx_str)
idx_names<-make.names(c(idx_gsub), unique = TRUE)
idx_names
length(idx_names)
length(names(Tavg_df))
names(Tavg_df)<-idx_names

#### Rownames anpassen ####
rownames(Tavg_df) <-0:411


#############################################
## Delete German ExTavssion in Columnnames ##
head(Tavg_df)
names(Tavg_df)
names(Tavg_df) <- chartr("ä","a",names(Tavg_df))
names(Tavg_df) <- chartr("z","c",names(Tavg_df))
names(Tavg_df) <- chartr("Mai","May",names(Tavg_df))
names(Tavg_df) <- chartr("Okt","Oct",names(Tavg_df))
names(Tavg_df)

######################################
#### Data.frame: 1995 produzieren ####
length(Tavg_df) 
length(Tavg_df) / 12

# Die letzten 21 Jahre sollen ausgelesen werden. Also 2015 -1995.
# Bei längeren Zeitperioden muss das entsTavghend angepasst werden. 
21*12

names(Tavg_df)[(length(Tavg_df)-(21*12-1)) : length(Tavg_df)]
Tavg_df_1995 <- Tavg_df[,(length(Tavg_df)-(21*12-1)):length(Tavg_df)]
names(Tavg_df_1995)
rownames(Tavg_df_1995) <- 0:411

#### Combine with Spatial Information
KreisTavg_spdf <- spCbind(KreisPolOGR,Tavg_df)
KreisTavg_spdf_1995 <- spCbind(KreisPolOGR,Tavg_df_1995)

######################################
#### Data.frame: 1999 produzieren ####
names(Tavg_df)[(length(Tavg_df)-(17*12-1)):length(Tavg_df)]
Tavg_df_1999<-Tavg_df[,(length(Tavg_df)-(17*12-1)):length(Tavg_df)]
names(Tavg_df_1999)

#### Combine with Spatial Information
KreisTavg_spdf<-spCbind(KreisPolOGR,Tavg_df)
KreisTavg_spdf_1999 <- spCbind(KreisPolOGR,Tavg_df_1999)

###################################################################
#### Export der erstellten Dateien (SpatialPolygoneDataFrames) ####
## ... für den gesamten Zeitraum
writePolyShape(KreisTavg_spdf, "./data/data_raw/KreisTavg_spdf.shp")
## ... ab 1995
writePolyShape(KreisTavg_spdf_1995, "./data/data_raw/KreisTavg_spdf_1995.shp")
## ... ab 1999
writePolyShape(KreisTavg_spdf_1999, "./data/data_raw/KreisTavg_spdf_1999.shp")

###################################################################
#### Import der erstellten Dateien (SpatialPolygoneDataFrames) ####

KreisTavg_spdf<-readShapePoly("./data/data_raw/KreisTavg_spdf.shp")
KreisTavg_spdf_1995 <-readShapePoly("./data/data_raw/KreisTavg_spdf_1995.shp")
KreisTavg_spdf_1999 <-readShapePoly("./data/data_raw/KreisTavg_spdf_1999.shp")
names(KreisTavg_spdf)
names(KreisTavg_spdf_1995)
names(KreisTavg_spdf_1999)

head(KreisTavg_spdf_1995)[1:7]

'Wenn man neu einlädt dan bildet sich die SP_ID'

#### Projection des ursrünglichen SpatialPolygonsDataFrame übertragen
# proj4string(KreisTavg_spdf)<-proj4string(KreisPolOGR)

####################################################################################################################################################################
####################################################################################################################################################################

########################################################
#### Plotten der erstellen SpatialPolygonsDataFrame ####
########################################################
at=(seq(0,30,1)) #
colors = colorRampPalette(c("skyblue3", "cornsilk",  "orangered3"))(1e3)

names(KreisTavg_spdf)

## Modify trellis theme ##
my.theme = trellis.par.get()
names(my.theme)
my.theme$panel.background

trellis.par.set("background", list(col = "white")) 
trellis.par.set("panel.background", list(col = "white")) 
trellis.par.set("strip.background", list(col = "white")) 
trellis.par.set("fontsize", list(text=20, points=10)) 

# trellis.par.set("strip.border", list(lty=c(1,1,1,1,1,1,1))) 
# trellis.par.set("axis.line", list(col="#000000")) 

my.theme$strip.background
my.theme$axis.line
my.theme$strip.border
# my.theme$strip.border$col <- c("#000000", "#000000","#000000", "#000000", "#000000", "#000000","#000000")

show.settings()


KreisTavg_spdf_1999_s <- KreisTavg_spdf_1999



## Setup Colourscheme ##
cs1 <-colors
## Used for publications 
cs2 <- colors
#
at2 <- at

###############
#### 1999 ####
## Setup Months considered ##
# zteins <- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-48]
zteins <- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)-49]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.99 <- spplot(KreisTavg_spdf_1999_s, zcol=zteins,at=at2,col.regions= cs2)
# KreisTavg_spdf_Plt2.99

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_1999_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.99)
dev.off()
# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2000 ####
## Setup Months considered ##
zteins<- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-36]
zteins<- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)-36]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2000 <- spplot(KreisTavg_spdf_1999_s, zcol=zteins,at=at2,col.regions= cs2)
# KreisTavg_spdf_Plt2.2000

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2000_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2000)
dev.off()
# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 


###############
#### 2001 ####
## Setup Months considered ##
zteins<- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-24]
zteins<- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)-24]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2001 <- spplot(KreisTavg_spdf_1999_s, zcol=zteins,at=at2,col.regions= cs2)
# KreisTavg_spdf_Plt2.2001

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2001_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2001)
dev.off()
# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 


###############
#### 2002 ####
## Setup Months considered ##
ztzwei<- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-12]
ztzwei<- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)-12]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztzwei,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2002 <- spplot(KreisTavg_spdf_1999_s, zcol=ztzwei,at=at2,col.regions= cs2)
# KreisTavg_spdf_Plt2.2

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2002_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2002)
dev.off()

# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 






###############
#### 2003 ####
## Setup Months considered ##
# ztdrei<- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)]
ztdrei<- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2003 <- spplot(KreisTavg_spdf_1999_s, zcol=ztdrei,at=at2,col.regions= cs2)
# KreisTavg_spdf_Plt2.2003

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2003_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2003)
dev.off()
# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 



###############
#### 2004 ####
## Setup Months considered ##
# ztvier<- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+12]
ztvier<- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)+12]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt2.1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztvier,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2004 <- spplot(KreisTavg_spdf_1999_s, zcol=ztvier,at=at2,col.regions= cs2)
# KreisTavg_spdf_Plt2.4 

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2004_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2004)
dev.off() 

# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt2.3 <- spplot(KreisTavg_spdf_1999_s, zcol="May2004",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt2.4 <- spplot(KreisTavg_spdf_1999_s, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 


###############
#### 2005 ####
## Setup Months considered ##
ztfünf<- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+24]
ztfünf<- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)+24]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsechs,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2005 <- spplot(KreisTavg_spdf_1999_s, zcol=ztfünf,at=at2,col.regions= cs2)
# KreisTavg_spdf_Plt2.5 

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2005_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2005)
dev.off()

# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2006 ####
## Setup Months considered ##
# ztsechs<- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+36]
ztsechs<- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)+36]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsechs,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2006 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsechs,at=at2,col.regions= cs2)
# KreisTavg_spdf_Plt2.6

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2006_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2006)
dev.off()

# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 


###############
#### 2007 ####
## Setup Months considered ##
# ztsieben<- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+48]
ztsieben<- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)+48]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2007 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
# KreisTavg_spdf_Plt2.7

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2007_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2007)
dev.off()

# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2008 ####
## Setup Months considered ##
ztsieben<- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+60]
ztsieben<- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)+60]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2008 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2008_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2008)
dev.off()

# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2009 ####
## Setup Months considered ##
# ztsieben<- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+72]
ztsieben<- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)+72]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2009 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2009_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2009)
dev.off()
# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2010 ####
## Setup Months considered ##
ztsieben <- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)+84]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2010 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2010_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2010)
dev.off()

# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2011 ####
## Setup Months considered ##
ztsieben <- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)+96]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2011 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2011_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2011)
dev.off()
# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2012 ####
## Setup Months considered ##
# ztsieben <- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)+108]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2012 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2012_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2012)
dev.off()
# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2013 ####
## Setup Months considered ##
# ztsieben <- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)+120]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800

KreisTavg_spdf_Plt2.2013 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2013_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2013)
dev.off()
# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2014 ####
## Setup Months considered ##
# ztsieben <- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61)+132]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2014 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2014_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2014)
dev.off()
# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2015 ####
## Setup Months considered ##
# ztsieben <- names(KreisTavg_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisTavg_spdf_1999_s)[c(62,63,64,59,60,61) + 144]
## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisTavg_spdf_Plt2.2015 <- spplot(KreisTavg_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
png("./figures/figures_exploratory/Tavg/Tavg_Anomaly_2015_short.png", width = 800, height = 800, units = "px",)
plot(KreisTavg_spdf_Plt2.2015)
dev.off()
# ## Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisTavg_spdf_Plt3 <- spplot(KreisTavg_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisTavg_spdf_Plt4 <- spplot(KreisTavg_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 


####################################################################################################################################################################
################################################################### Kippen des Datensatzes #########################################################################
####################################################################################################################################################################

###################################################################
#### Jetzt muss ich noch den Tavg-Datensatz entsTavghend kippen ####
###################################################################
head(KreisTavg_spdf_1995)
##########################

##########################
#### reshape::reshape ####

#### SpatialPolygonsDataFrame in normalen DataFrame ####
KreisTavg_df_1995 <- as.data.frame(KreisTavg_spdf_1995)
head(KreisTavg_spdf_1995)
names(KreisTavg_spdf_1995)

#### Checken, ob Namen sich nicht verändert haben
all(names(KreisTavg_df_1995)==names(KreisTavg_spdf_1995)) # check positive

length(names(KreisTavg_df_1995))
names(KreisTavg_df_1995)
dim(KreisTavg_df_1995)
21*12
21*412
## Beschreibung des Vorganges ##
'Ziel: Ich habe 412 Beobachtungen pro Zeitschritt (Jahr). 
Da mein Datensatz tidy ist, also die Jahre untereinander fortlaufend angeordnet sind, müssen meine Variablen entsTavghend angepasst werden.
Soll heißen aus den Tavg Variablen mit Monats und Jahres zuordnung wird nun ein Tavg mit Monats Zuordnung und die Jahre verschmelzen alle in 
eine Spalte. 
Damit habe ich 12 Spalten für den Tavg, also eine pro Monat, und 16 (Jahre) mal 412 Beobachtungen, also 6592 Beobachtungen.
Für dieses Vorgehen reshape ich den Kreis_Tavg_dataframe von wide nach long mit Hilfe der stats::reshape function. 
Dazu muss ich eine Liste mit den Tavg mit Monat und Jahres Zuordnung erstellen, welche dann jeweils in die Tavg mit Monats zuordnung
im long Format übergeht. -> listMonthYearNames[[i]]
Anschließend erstelle ich die Namen der Tavg mit reiner Monats Zurdnung -> listMonthNames
Mit Hilfe der Year Variablen werden dann die Beeobachtungen innerhalb der Tavg mit Monats Zuordnungen nach den Jahren differenziert.
'

#### Make a list of vectors of months ####
' Diese list dient der Indizierung der jeweiligen Monate. Sie startet bei 7, da die 6 columns davor die räumlichen Einheiten definieren.'
length(names(KreisTavg_df_1995))


list0<-list(c(7,length(names(KreisTavg_df_1995))-11),
            c(8,length(names(KreisTavg_df_1995))-10),
            c(9,length(names(KreisTavg_df_1995))-9),
            c(10,length(names(KreisTavg_df_1995))-8), 
            c(11,length(names(KreisTavg_df_1995))-7),
            c(12,length(names(KreisTavg_df_1995))-6),
            c(13,length(names(KreisTavg_df_1995))-5),
            c(14,length(names(KreisTavg_df_1995))-4),
            c(15,length(names(KreisTavg_df_1995))-3), 
            c(16,length(names(KreisTavg_df_1995))-2), 
            c(17,length(names(KreisTavg_df_1995))-1), 
            c(18,length(names(KreisTavg_df_1995))))
# those list define the starting and ending point for the steps, 12 each
list0 


## Container 
listMonthYearNames <- list()

## Lenght of considered time period ##
i=1
length(seq(list0[[i]][1],list0[[i]][2],12))

#### Loop zum erstellen der Liste listMonthYearNames mit den IDs, welche jeweils den gleichen Monatesnamen, über alle Jahre hinweg, haben ####
for (i in 01:12){
  listMonthYearNames[i] <- list(c(names(KreisTavg_df_1995)[c(seq(list0[[i]][1],list0[[i]][2],12))]))
}
# class(listMonthYearNames[1])
# class(listMonthYearNames)
listMonthYearNames # list including a list of the listMonthYearNames across the years. This is the basis for the new variables, i.e the Tavg in each month
listMonthYearNames[[3]] # list of all marches across the 21 years considered here

#### Make the names for the new variables
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
listMonthNames<-list(c("Tavg_Jan"), c( "Tavg_Feb"), c("Tavg_Mar"), c("Tavg_Apr"), c("Tavg_May"), c("Tavg_Jun"),c("Tavg_Jul"),c("Tavg_Aug"),c("Tavg_Sep"), c("Tavg_Oct"), c("Tavg_Nov"), c("Tavg_Dec"))
listMonthNames


### Reshape from wide to long für jeden Monat ####
# KreisTavg_df_19953 <- reshape(KreisTavg_df_1995, 
#              varying = listMonthYearNames[[1]],
#              v.names = listMonthNames[[1]],
#              timevar = "YearofJan", 
#              times = listMonthYearNames[[1]],
#              direction = "long")
# 
# names(KreisTavg_df_19953)[184]
# names(KreisTavg_df_19953)
# head(KreisTavg_df_19953)
# rownames(KreisTavg_df_19953)<-NULL
# rownames(KreisTavg_df_19953)

#### Loop, welche mir die Tavg pro Monat jeweils in einer Column ausgibt ####

8652/21

y <- data.frame(1:412*21) # set container
x <- data.frame() # set container

##############################################################################################################
### loop over the twelve listMonthYearNames, split up, an then filled up with the values across the years ####
for(i in 1:12) {
  x <- reshape (KreisTavg_df_1995, 
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
y$X1.412...21<-NULL
head(y)


#### Erstellen eines data.frames mit ID angaben #### 
' x delievers spatial and time information, y the newly created tidy data'
dim(x)
dim(y)
names(x)[1:6]
names(x)[length(x) - 1]
head(x)[1:6]
names(y)
length(x) - 1

## Combine data.frames
head(x[,c(1:4, length(x) - 1)])
head(y)
Tavg_months <- cbind(x[,c(1:4, length(x) - 1)],y)

names(Tavg_months)
head(Tavg_months)
rownames(Tavg_months) <- NULL

## Explore whether comIds and years match ##
head(Tavg_months, 15) # check
tail(Tavg_months, 15) # check
# In den rownames steht der Monat Dezember, da dies der letzte Monat ist, der gemelted, also in eine Spalte gefasst wurde. 
# Sieht aber sonst recht ordentlich aus.

####################################################
## Change strings which are necessary for merging ##

## Befreien der year Variable von den Monatsangaben, hier Dezember
Tavg_months$Year <- as.integer(str_sub(Tavg_months$Year,7,10 ))

#### Verkürzen des ID-Strings, also KreisTavg_spdf_1995$RS  ####
str_length(Tavg_months$RS) #12
Tavg_months$RS <- as.factor(str_sub(Tavg_months$RS,1,5))
str_length(Tavg_months$RS) #5

unique(Tavg_months$RS)


## Anpassen der Namen fürs mergen
colnames(Tavg_months)[colnames(Tavg_months)=="Year"] <- "year"
colnames(Tavg_months)[colnames(Tavg_months)=="RS"] <- "comId"
head(Tavg_months)


##################################################################################
#### Abgleichen der neu erstellten Daten mit KreisTavg_df_1995 (orginal Daten) ####
head(KreisTavg_df_1995$TavJan1995)%in%head(Tavg_months$Tavg_Jan) 

tail(KreisTavg_df_1995$TavDec2015)%in%tail(Tavg_months$Tavg_Dec) 

## Erstellen und überprüfen der beiden Vektoren, die jeweils den Tavg des Jahres 1999 im Monat Januar wiedergebeb


## Vector in Tavg_months des Tavg Januar 1999
Tavg_months$Tavg_Jan[Tavg_months$year == 1999]
length(Tavg_months$Tavg_Jan[Tavg_months$year==1999])

## Vector in KreisTavg_df_1995 des Tavg Januar 1999
KreisTavg_df_1995$TavJan1999
length(KreisTavg_df_1995$TavJan1999)

all(Tavg_months$Tavg_Jan[Tavg_months$year==1999]%in%KreisTavg_df_1995$TavJan1999) # check
# Daten sehen gut aus
names(KreisTavg_df_1995)
all(Tavg_months$Tav_Dec[Tavg_months$year==1999]%in%KreisTavg_df_1995$PrJan1999) # check

##########################################################################
#### Produce lagged variabled of Tavg for the months after the harvest ####
##########################################################################
## Tavg ##
names(Tavg_months)
"Das erstellen der lags muss vor dem mergen passsieren, da yield nur bis 1999 geht."


Tavg_months <- slide(Tavg_months, Var ="Tavg_Aug", GroupVar= "comId", slideBy = -1)
Tavg_months <- slide(Tavg_months, Var ="Tavg_Sep", GroupVar= "comId", slideBy = -1)
Tavg_months <- slide(Tavg_months, Var ="Tavg_Oct", GroupVar= "comId", slideBy = -1)
Tavg_months <- slide(Tavg_months, Var ="Tavg_Nov", GroupVar= "comId", slideBy = -1)
Tavg_months <- slide(Tavg_months, Var ="Tavg_Dec", GroupVar= "comId", slideBy = -1)

head(Tavg_months)                                                

## Check for validity
head(Tavg_months[Tavg_months$year==1999,])[13:17] %in% head(Tavg_months[Tavg_months$year==2000,])[18:22]  
' Da zum Beispiel der August des Jahre 1999 mit dem lag des Jahres 2000 übereinstimmt, scheint alles zu passen'

table(Tavg_months$comId)  

## Umbennen der lagged Variablen ##
names(Tavg_months)
names(Tavg_months)[names(Tavg_months)==c("Tavg_Aug-1" )]  <- c("Tavg_Aug_lag")
names(Tavg_months)[names(Tavg_months)==c("Tavg_Sep-1" )]  <- c("Tavg_Sep_lag")
names(Tavg_months)[names(Tavg_months)==c("Tavg_Oct-1" )]  <- c("Tavg_Oct_lag")
names(Tavg_months)[names(Tavg_months)==c("Tavg_Nov-1" )]  <- c("Tavg_Nov_lag")
names(Tavg_months)[names(Tavg_months)==c("Tavg_Dec-1" )]  <- c("Tavg_Dec_lag")

head(Tavg_months)
dim(Tavg_months)
###########################################
#### Write newly created Tavg_months ####

write.csv(Tavg_months, "data/data_processed/Tavg_months.csv")


