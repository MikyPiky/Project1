#### File Description ####
' - Transformation der NetCDF Datei mit den Prec Werten in time series für die einzelnen Landkreise.
  - Stephans Fortran Program
    - Bodenfeuchte (4*4 km) maskiert für non- irrigated agricultural land (CLC06, class 12, 100*100 m)  
    - und danach wieder hochgerechnet auf Polygone der administrative districts(vg2500_krs).
  - Zusammenführen dieses Datensatzes mit SpatialPolygonDataframe (vg2500_krs) um räumlichen Bezug herzustellen.
  - Plotten mit ssplot{sp}
'
#### Output ####
##Files
' - SpatialPolygonsDataFrame: KreisPrec_spdf (ganzer Zeitraum), Prec_months.csv
  - geht in MergePrec_Yield ein
'
## Plots
' Plots der Season von Maize für alle Jahre.
'

#### Dependencies and Input ####
'm Prec.nc (Stephan Thober)
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
#### Laden des Prec NetCDF ####
##############################
# Prec<-brick("/home/peichl/Documents/projects/correlation/data/data_raw/4km_Prec.nc", varname="Prec")
# ## Funktioniert mit dieser File nicht



#### Alternatives Laden: ncdf4 ####
Prec_open <- nc_open("/Storage/ownCloud/Home/Klimabuero/Proj1/data/data_raw/Prec_lk.nc")
print(Prec_open)
# Hier habe ich 780 Monate oder 65 Jahre seit einschließlich Januar 1951

Prec <- ncvar_get(Prec_open, varid="pre_lk")
nc_close(Prec_open)


dim(Prec) 
class(Prec)
levelplot(Prec)
str(Prec)

####################################################################################
#### Laden der shape mit den Polygonen der Kreise und deren räumliche Zuordnung ####
####################################################################################
KreisPolOGR <- readOGR("/Storage/ownCloud/Home/Klimabuero/Proj1/data//data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")


##################################################################
#### Datensatz als Matrix um 0 Werte in NAs zu transformieren ####

Prec_m <- as.matrix(Prec)
dim(Prec_m)
head(Prec_m)
any(Prec_m == -9999)
any(Prec_m == 9999)
any(Prec_m == 0)

#### 0 Werte in NA ####
any(Prec_m == 0)
u <- Prec_m == 0
Prec_m[u] <- NA
levelplot(Prec_m)

###########################################################################################################
#### Datensatz an Spatialpolygondataframe der Landkreise anhängen, um räumliche Beziehung herzustellen ####
###########################################################################################################

#### Matrix in Dataframe ###
Prec_df <- as.data.frame(Prec_m)
dim(Prec_df)


#### Verändern der Namen ####
' Prec starts one year earlier as SMI'
names(Prec_df)
idx <-seq(from=as.Date('1950-1-1'), to=as.Date('2015-12-1'), 'month')
idx <-as.yearmon(idx)
idx_str<-paste("Pre",idx, sep="")
idx_names<-make.names(c(idx_str), unique = TRUE)
idx_gsub<-gsub(" ","",idx_str)
idx_names<-make.names(c(idx_gsub), unique = TRUE)
idx_names
length(idx_names)
length(names(Prec_df))
names(Prec_df)<-idx_names

#### Rownames anpassen ####
rownames(Prec_df) <-0:411


#############################################
## Delete German Expression in Columnnames ##
head(Prec_df)
names(Prec_df)
names(Prec_df) <- chartr("ä","a",names(Prec_df))
names(Prec_df) <- chartr("z","c",names(Prec_df))
names(Prec_df) <- chartr("Mai","May",names(Prec_df))
names(Prec_df) <- chartr("Okt","Oct",names(Prec_df))
names(Prec_df)

######################################
#### Data.frame: 1995 produzieren ####
length(Prec_df) 
length(Prec_df) / 12

# Die letzten 21 Jahre sollen ausgelesen werden. Also 2015 -1995.
# Bei längeren Zeitperioden muss das entsprechend angepasst werden. 
21*12

names(Prec_df)[(length(Prec_df)-(21*12-1)) : length(Prec_df)]
Prec_df_1995 <- Prec_df[,(length(Prec_df)-(21*12-1)):length(Prec_df)]
names(Prec_df_1995)
rownames(Prec_df_1995) <- 0:411

#### Combine with Spatial Information
KreisPrec_spdf <- spCbind(KreisPolOGR,Prec_df)
KreisPrec_spdf_1995 <- spCbind(KreisPolOGR,Prec_df_1995)

######################################
#### Data.frame: 1999 produzieren ####
names(Prec_df)[(length(Prec_df)-(17*12-1)):length(Prec_df)]
Prec_df_1999<-Prec_df[,(length(Prec_df)-(17*12-1)):length(Prec_df)]
names(Prec_df_1999)

#### Combine with Spatial Information
KreisPrec_spdf<-spCbind(KreisPolOGR,Prec_df)
KreisPrec_spdf_1999 <- spCbind(KreisPolOGR,Prec_df_1999)

###################################################################
#### Export der erstellten Dateien (SpatialPolygoneDataFrames) ####
## ... für den gesamten Zeitraum
writePolyShape(KreisPrec_spdf, "./data/data_raw/KreisPrec_spdf.shp")
## ... ab 1995
writePolyShape(KreisPrec_spdf_1995, "./data/data_raw/KreisPrec_spdf_1995.shp")
## ... ab 1999
writePolyShape(KreisPrec_spdf_1999, "./data/data_raw/KreisPrec_spdf_1999.shp")

###################################################################
#### Import der erstellten Dateien (SpatialPolygoneDataFrames) ####

KreisPrec_spdf<-readShapePoly("./data/data_raw/KreisPrec_spdf.shp")
KreisPrec_spdf_1995 <-readShapePoly("./data/data_raw/KreisPrec_spdf_1995.shp")
KreisPrec_spdf_1999 <-readShapePoly("./data/data_raw/KreisPrec_spdf_1999.shp")
names(KreisPrec_spdf)
names(KreisPrec_spdf_1995)
names(KreisPrec_spdf_1999)

head(KreisPrec_spdf_1995)[1:7]

'Wenn man neu einlädt dan bildet sich die SP_ID'

#### Projection des ursrünglichen SpatialPolygonsDataFrame übertragen
# proj4string(KreisPrec_spdf)<-proj4string(KreisPolOGR)

####################################################################################################################################################################
####################################################################################################################################################################

########################################################
#### Plotten der erstellen SpatialPolygonsDataFrame ####
########################################################

at=(seq(0,300,1)) #
colors = colorRampPalette(c("orangered3", "cornsilk", "skyblue3"))(1e3)

names(KreisPrec_spdf)

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


KreisPrec_spdf_1999_s <- KreisPrec_spdf_1999



## Setup Colourscheme ##
cs1 <-colors
## Used for publications 
cs2 <- colors
#
at2 <- at

###############
#### 1999 ####
## Setup Months considered ##
# zteins <- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-48]
zteins <- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)-49]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.99 <- spplot(KreisPrec_spdf_1999_s, zcol=zteins,at=at2,col.regions= cs2)
# KreisPrec_spdf_Plt2.99

png("./figures/figures_exploratory/Prec/Prec_Anomaly_1999_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.99)
dev.off()
# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2000 ####
## Setup Months considered ##
zteins<- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-36]
zteins<- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)-36]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2000 <- spplot(KreisPrec_spdf_1999_s, zcol=zteins,at=at2,col.regions= cs2)
# KreisPrec_spdf_Plt2.2000

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2000_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2000)
dev.off()
# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 


###############
#### 2001 ####
## Setup Months considered ##
zteins<- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-24]
zteins<- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)-24]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2001 <- spplot(KreisPrec_spdf_1999_s, zcol=zteins,at=at2,col.regions= cs2)
# KreisPrec_spdf_Plt2.2001

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2001_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2001)
dev.off()
# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 


###############
#### 2002 ####
## Setup Months considered ##
ztzwei<- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-12]
ztzwei<- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)-12]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztzwei,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2002 <- spplot(KreisPrec_spdf_1999_s, zcol=ztzwei,at=at2,col.regions= cs2)
# KreisPrec_spdf_Plt2.2

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2002_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2002)
dev.off()

# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 






###############
#### 2003 ####
## Setup Months considered ##
# ztdrei<- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)]
ztdrei<- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2003 <- spplot(KreisPrec_spdf_1999_s, zcol=ztdrei,at=at2,col.regions= cs2)
# KreisPrec_spdf_Plt2.2003

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2003_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2003)
dev.off()
# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 



###############
#### 2004 ####
## Setup Months considered ##
# ztvier<- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+12]
ztvier<- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)+12]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt2.1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztvier,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2004 <- spplot(KreisPrec_spdf_1999_s, zcol=ztvier,at=at2,col.regions= cs2)
# KreisPrec_spdf_Plt2.4 

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2004_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2004)
dev.off() 

# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt2.3 <- spplot(KreisPrec_spdf_1999_s, zcol="May2004",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt2.4 <- spplot(KreisPrec_spdf_1999_s, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 


###############
#### 2005 ####
## Setup Months considered ##
ztfünf<- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+24]
ztfünf<- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)+24]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsechs,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2005 <- spplot(KreisPrec_spdf_1999_s, zcol=ztfünf,at=at2,col.regions= cs2)
# KreisPrec_spdf_Plt2.5 

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2005_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2005)
dev.off()

# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2006 ####
## Setup Months considered ##
# ztsechs<- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+36]
ztsechs<- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)+36]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsechs,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2006 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsechs,at=at2,col.regions= cs2)
# KreisPrec_spdf_Plt2.6

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2006_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2006)
dev.off()

# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 


###############
#### 2007 ####
## Setup Months considered ##
# ztsieben<- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+48]
ztsieben<- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)+48]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2007 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
# KreisPrec_spdf_Plt2.7

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2007_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2007)
dev.off()

# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2008 ####
## Setup Months considered ##
ztsieben<- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+60]
ztsieben<- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)+60]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2008 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2008_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2008)
dev.off()

# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2009 ####
## Setup Months considered ##
# ztsieben<- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+72]
ztsieben<- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)+72]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2009 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2009_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2009)
dev.off()
# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2010 ####
## Setup Months considered ##
ztsieben <- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)+84]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2010 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2010_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2010)
dev.off()

# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2011 ####
## Setup Months considered ##
ztsieben <- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)+96]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2011 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2011_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2011)
dev.off()
# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2012 ####
## Setup Months considered ##
# ztsieben <- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)+108]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2012 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2012_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2012)
dev.off()
# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2013 ####
## Setup Months considered ##
# ztsieben <- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)+120]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800

KreisPrec_spdf_Plt2.2013 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
png("./figures/figures_exploratory/Prec/Prec_Anomaly_2013_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2013)
dev.off()
# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2014 ####
## Setup Months considered ##
# ztsieben <- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61)+132]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2014 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)

png("./figures/figures_exploratory/Prec/Prec_Anomaly_2014_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2014)
dev.off()
# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 

###############
#### 2015 ####
## Setup Months considered ##
# ztsieben <- names(KreisPrec_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
ztsieben <- names(KreisPrec_spdf_1999_s)[c(62,63,64,59,60,61) + 144]
## Plot des Jahres 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
## Exported als png im format width 800 and height 800
KreisPrec_spdf_Plt2.2015 <- spplot(KreisPrec_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
png("./figures/figures_exploratory/Prec/Prec_Anomaly_2015_short.png", width = 800, height = 800, units = "px",)
plot(KreisPrec_spdf_Plt2.2015)
dev.off()
# ## Plot des Mai 2003 der extrahierten Prec Werte auf Kreisniveau für Deutschland mit farbwahl für Tavsentation
# KreisPrec_spdf_Plt3 <- spplot(KreisPrec_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# ## Exported als png im format 800 and height 800
# KreisPrec_spdf_Plt4 <- spplot(KreisPrec_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# ## Comment: Pattern stimmen mit orginal Daten überein 


####################################################################################################################################################################
################################################################### Kippen des Datensatzes #########################################################################
####################################################################################################################################################################

###################################################################
#### Jetzt muss ich noch den Prec-Datensatz entsprechend kippen ####
###################################################################
head(KreisPrec_spdf_1995)
##########################

##########################
#### reshape::reshape ####

#### SpatialPolygonsDataFrame in normalen DataFrame ####
KreisPrec_df_1995 <- as.data.frame(KreisPrec_spdf_1995)
head(KreisPrec_spdf_1995)
names(KreisPrec_spdf_1995)

#### Checken, ob Namen sich nicht verändert haben
all(names(KreisPrec_df_1995)==names(KreisPrec_spdf_1995)) # check positive

length(names(KreisPrec_df_1995))
names(KreisPrec_df_1995)
dim(KreisPrec_df_1995)
21*12
21*412
## Beschreibung des Vorganges ##
'Ziel: Ich habe 412 Beobachtungen pro Zeitschritt (Jahr). 
Da mein Datensatz tidy ist, also die Jahre untereinander fortlaufend angeordnet sind, müssen meine Variablen entsprechend angepasst werden.
Soll heißen aus den Prec Variablen mit Monats und Jahres zuordnung wird nun ein Prec mit Monats Zuordnung und die Jahre verschmelzen alle in 
eine Spalte. 
Damit habe ich 12 Spalten für den Prec, also eine pro Monat, und 16 (Jahre) mal 412 Beobachtungen, also 6592 Beobachtungen.
Für dieses Vorgehen reshape ich den Kreis_Prec_dataframe von wide nach long mit Hilfe der stats::reshape function. 
Dazu muss ich eine Liste mit den Prec mit Monat und Jahres Zuordnung erstellen, welche dann jeweils in die Prec mit Monats zuordnung
im long Format übergeht. -> listMonthYearNames[[i]]
Anschließend erstelle ich die Namen der Prec mit reiner Monats Zurdnung -> listMonthNames
Mit Hilfe der Year Variablen werden dann die Beeobachtungen innerhalb der Prec mit Monats Zuordnungen nach den Jahren differenziert.
'

#### Make a list of vectors of months ####
' Diese list dient der Indizierung der jeweiligen Monate. Sie startet bei 7, da die 6 columns davor die räumlichen Einheiten definieren.'
length(names(KreisPrec_df_1995))


list0<-list(c(7,length(names(KreisPrec_df_1995))-11),
            c(8,length(names(KreisPrec_df_1995))-10),
            c(9,length(names(KreisPrec_df_1995))-9),
            c(10,length(names(KreisPrec_df_1995))-8), 
            c(11,length(names(KreisPrec_df_1995))-7),
            c(12,length(names(KreisPrec_df_1995))-6),
            c(13,length(names(KreisPrec_df_1995))-5),
            c(14,length(names(KreisPrec_df_1995))-4),
            c(15,length(names(KreisPrec_df_1995))-3), 
            c(16,length(names(KreisPrec_df_1995))-2), 
            c(17,length(names(KreisPrec_df_1995))-1), 
            c(18,length(names(KreisPrec_df_1995))))
# those list define the starting and ending point for the steps, 12 each
list0 


## Container 
listMonthYearNames <- list()

## Lenght of considered time period ##
i=1
length(seq(list0[[i]][1],list0[[i]][2],12))

#### Loop zum erstellen der Liste listMonthYearNames mit den IDs, welche jeweils den gleichen Monatesnamen, über alle Jahre hinweg, haben ####
for (i in 01:12){
  listMonthYearNames[i] <- list(c(names(KreisPrec_df_1995)[c(seq(list0[[i]][1],list0[[i]][2],12))]))
}
# class(listMonthYearNames[1])
# class(listMonthYearNames)
listMonthYearNames # list including a list of the listMonthYearNames across the years. This is the basis for the new variables, i.e the Prec in each month
listMonthYearNames[[3]] # list of all marches across the 21 years considered here

#### Make the names for the new variables
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
listMonthNames<-list(c("Prec_Jan"), c( "Prec_Feb"), c("Prec_Mar"), c("Prec_Apr"), c("Prec_May"), c("Prec_Jun"),c("Prec_Jul"),c("Prec_Aug"),c("Prec_Sep"), c("Prec_Oct"), c("Prec_Nov"), c("Prec_Dec"))
listMonthNames


### Reshape from wide to long für jeden Monat ####
# KreisPrec_df_19953 <- reshape(KreisPrec_df_1995, 
#              varying = listMonthYearNames[[1]],
#              v.names = listMonthNames[[1]],
#              timevar = "YearofJan", 
#              times = listMonthYearNames[[1]],
#              direction = "long")
# 
# names(KreisPrec_df_19953)[184]
# names(KreisPrec_df_19953)
# head(KreisPrec_df_19953)
# rownames(KreisPrec_df_19953)<-NULL
# rownames(KreisPrec_df_19953)

#### Loop, welche mir die Prec pro Monat jeweils in einer Column ausgibt ####

8652/21

y <- data.frame(1:412*21) # set container
x <- data.frame() # set container

##############################################################################################################
### loop over the twelve listMonthYearNames, split up, an then filled up with the values across the years ####
for(i in 1:12) {
  x <- reshape (KreisPrec_df_1995, 
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
Prec_months <- cbind(x[,c(1:4, length(x) - 1)],y)

names(Prec_months)
head(Prec_months)
rownames(Prec_months) <- NULL

## Explore whether comIds and years match ##
head(Prec_months, 15) # check
tail(Prec_months, 15) # check
# In den rownames steht der Monat Dezember, da dies der letzte Monat ist, der gemelted, also in eine Spalte gefasst wurde. 
# Sieht aber sonst recht ordentlich aus.

####################################################
## Change strings which are necessary for merging ##

## Befreien der year Variable von den Monatsangaben, hier Dezember
Prec_months$Year <- as.integer(str_sub(Prec_months$Year,7,10 ))

#### Verkürzen des ID-Strings, also KreisPrec_spdf_1995$RS  ####
str_length(Prec_months$RS) #12
Prec_months$RS <- as.factor(str_sub(Prec_months$RS,1,5))
str_length(Prec_months$RS) #5

unique(Prec_months$RS)


## Anpassen der Namen fürs mergen
colnames(Prec_months)[colnames(Prec_months)=="Year"] <- "year"
colnames(Prec_months)[colnames(Prec_months)=="RS"] <- "comId"
head(Prec_months)


##################################################################################
#### Abgleichen der neu erstellten Daten mit KreisPrec_df_1995 (orginal Daten) ####
head(KreisPrec_df_1995$PreJan1995)
head(Prec_months$Prec_Jan) 

tail(KreisPrec_df_1995$PreDec2015)
tail(Prec_months$Prec_Dec) 

## Erstellen und überprüfen der beiden Vektoren, die jeweils den Prec des Jahres 1999 im Monat Januar wiedergebeb


## Vector in Prec_months des Prec Januar 1999
Prec_months$Prec_Jan[Prec_months$year == 1999]
length(Prec_months$Prec_Jan[Prec_months$year==1999])

## Vector in KreisPrec_df_1995 des Prec Januar 1999
KreisPrec_df_1995$PreJan1999
length(KreisPrec_df_1995$PreJan1999)

match(KreisPrec_df_1995$PrJan1999,Prec_months$Prec_Jan[Prec_months$year==1999]) # check
all(Prec_months$Prec_Jan[Prec_months$year==1999]%in%KreisPrec_df_1995$PreJan1999) # check
# Daten sehen gut aus
names(KreisPrec_df_1995)
match(KreisPrec_df_1995$PreDec2015,Prec_months$Prec_Dec[Prec_months$year==2015]) # check
all(Prec_months$Pre_Dec[Prec_months$year==1999]%in%KreisPrec_df_1995$PrJan1999) # check

##########################################################################
#### Produce lagged variabled of Prec for the months after the harvest ####
##########################################################################
## Prec ##
names(Prec_months)
"Das erstellen der lags muss vor dem mergen passsieren, da yield nur bis 1999 geht."


Prec_months <- slide(Prec_months, Var ="Prec_Aug", GroupVar= "comId", slideBy = -1)
Prec_months <- slide(Prec_months, Var ="Prec_Sep", GroupVar= "comId", slideBy = -1)
Prec_months <- slide(Prec_months, Var ="Prec_Oct", GroupVar= "comId", slideBy = -1)
Prec_months <- slide(Prec_months, Var ="Prec_Nov", GroupVar= "comId", slideBy = -1)
Prec_months <- slide(Prec_months, Var ="Prec_Dec", GroupVar= "comId", slideBy = -1)

head(Prec_months)                                                

## Check for validity
head(Prec_months[Prec_months$year==1999,])[13:17] %in% head(Prec_months[Prec_months$year==2000,])[18:22]  
' Da zum Beispiel der August des Jahre 1999 mit dem lag des Jahres 2000 übereinstimmt, scheint alles zu passen'

table(Prec_months$comId)  

## Umbennen der lagged Variablen ##
names(Prec_months)
names(Prec_months)[names(Prec_months)==c("Prec_Aug-1" )]  <- c("Prec_Aug_lag")
names(Prec_months)[names(Prec_months)==c("Prec_Sep-1" )]  <- c("Prec_Sep_lag")
names(Prec_months)[names(Prec_months)==c("Prec_Oct-1" )]  <- c("Prec_Oct_lag")
names(Prec_months)[names(Prec_months)==c("Prec_Nov-1" )]  <- c("Prec_Nov_lag")
names(Prec_months)[names(Prec_months)==c("Prec_Dec-1" )]  <- c("Prec_Dec_lag")

head(Prec_months)
dim(Prec_months)
###########################################
#### Write newly created Prec_months ####

write.csv(Prec_months, "data/data_processed/Prec_months.csv")


