#### File Description ####
' - Transformation der NetCDF Datei mit den PET Werten in time series für die einzelnen Landkreise.
  - Stephans Fortran Program
    - Bodenfeuchte (4*4 km) maskiert für non- irrigated agricultural land (CLC06, class 12, 100*100 m)  
    - und danach wieder hochgerechnet auf Polygone der administrative districts(vg2500_krs).
  - Zusammenführen dieses Datensatzes mit SpatialPolygonDataframe (vg2500_krs) um räumlichen Bezug herzustellen.
  - Plotten mit ssplot{sp}
'
#### Output ####
##Files
' - SpatialPolygonsDataFrame: KreisPET_spdf (ganzer Zeitraum), PET_months.csv
  - geht in MergePET_Yield ein
'
## Plots
' Plots der Season von Maize für alle Jahre.
'

#### Dependencies and Input ####
'm PET.nc (Stephan Thober)
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
#### Laden des PET NetCDF ####
##############################
# PET<-brick("/home/peichl/Documents/projects/correlation/data/data_raw/4km_PET.nc", varname="PET")
# ## Funktioniert mit dieser File nicht



#### Alternatives Laden: ncdf4 ####
PET_open <- nc_open("/Storage/ownCloud/Home/Klimabuero/Proj1/data/data_raw/PET_lk.nc")
print(PET_open)
# Hier habe ich 780 Monate oder 65 Jahre seit einschließlich Januar 1951

PET <- ncvar_get(PET_open, varid="pet_lk")
nc_close(PET_open)


dim(PET) 
class(PET)
levelplot(PET)
str(PET)

####################################################################################
#### Laden der shape mit den Polygonen der Kreise und deren räumliche Zuordnung ####
####################################################################################
KreisPolOGR <- readOGR("/Storage/ownCloud/Home/Klimabuero/Proj1/data//data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")


##################################################################
#### Datensatz als Matrix um 0 Werte in NAs zu transformieren ####

PET_m <- as.matrix(PET)
dim(PET_m)
head(PET_m)
any(PET_m == -9999)
any(PET_m == 9999)
any(PET_m == 0)

#### 0 Werte in NA ####
any(PET_m == 0)
u <- PET_m == 0
PET_m[u] <- NA
levelplot(PET_m)

###########################################################################################################
#### Datensatz an Spatialpolygondataframe der Landkreise anhängen, um räumliche Beziehung herzustellen ####
###########################################################################################################

#### Matrix in Dataframe ###
PET_df <- as.data.frame(PET_m)
dim(PET_df)


#### Verändern der Namen ####
' PET starts one year earlier as SMI'
names(PET_df)
idx <-seq(from=as.Date('1950-1-1'), to=as.Date('2015-12-1'), 'month')
idx <-as.yearmon(idx)
idx_str<-paste("PET",idx, sep="")
idx_names<-make.names(c(idx_str), unique = TRUE)
idx_gsub<-gsub(" ","",idx_str)
idx_names<-make.names(c(idx_gsub), unique = TRUE)
idx_names
length(idx_names)
length(names(PET_df))
names(PET_df)<-idx_names

#### Rownames anpassen ####
rownames(PET_df) <-0:411


#############################################
## Delete German ExPETssion in Columnnames ##
head(PET_df)
names(PET_df)
names(PET_df) <- chartr("ä","a",names(PET_df))
names(PET_df) <- chartr("z","c",names(PET_df))
names(PET_df) <- chartr("Mai","May",names(PET_df))
names(PET_df) <- chartr("Okt","Oct",names(PET_df))
names(PET_df)

######################################
#### Data.frame: 1995 produzieren ####
length(PET_df) 
length(PET_df) / 12

# Die letzten 21 Jahre sollen ausgelesen werden. Also 2015 -1995.
# Bei längeren Zeitperioden muss das entsPEThend angepasst werden. 
21*12

names(PET_df)[(length(PET_df)-(21*12-1)) : length(PET_df)]
PET_df_1995 <- PET_df[,(length(PET_df)-(21*12-1)):length(PET_df)]
names(PET_df_1995)
rownames(PET_df_1995) <- 0:411

#### Combine with Spatial Information
KreisPET_spdf <- spCbind(KreisPolOGR,PET_df)
KreisPET_spdf_1995 <- spCbind(KreisPolOGR,PET_df_1995)

######################################
#### Data.frame: 1999 produzieren ####
names(PET_df)[(length(PET_df)-(17*12-1)):length(PET_df)]
PET_df_1999<-PET_df[,(length(PET_df)-(17*12-1)):length(PET_df)]
names(PET_df_1999)

#### Combine with Spatial Information
KreisPET_spdf<-spCbind(KreisPolOGR,PET_df)
KreisPET_spdf_1999 <- spCbind(KreisPolOGR,PET_df_1999)

###################################################################
#### Export der erstellten Dateien (SpatialPolygoneDataFrames) ####
## ... für den gesamten Zeitraum
writePolyShape(KreisPET_spdf, "./data/data_raw/KreisPET_spdf.shp")
## ... ab 1995
writePolyShape(KreisPET_spdf_1995, "./data/data_raw/KreisPET_spdf_1995.shp")
## ... ab 1999
writePolyShape(KreisPET_spdf_1999, "./data/data_raw/KreisPET_spdf_1999.shp")

###################################################################
#### Import der erstellten Dateien (SpatialPolygoneDataFrames) ####

KreisPET_spdf<-readShapePoly("./data/data_raw/KreisPET_spdf.shp")
KreisPET_spdf_1995 <-readShapePoly("./data/data_raw/KreisPET_spdf_1995.shp")
KreisPET_spdf_1999 <-readShapePoly("./data/data_raw/KreisPET_spdf_1999.shp")
names(KreisPET_spdf)
names(KreisPET_spdf_1995)
names(KreisPET_spdf_1999)

head(KreisPET_spdf_1995)[1:7]

'Wenn man neu einlädt dan bildet sich die SP_ID'

#### Projection des ursrünglichen SpatialPolygonsDataFrame übertragen
# proj4string(KreisPET_spdf)<-proj4string(KreisPolOGR)

####################################################################################################################################################################
####################################################################################################################################################################

########################################################
#### Plotten der erstellen SpatialPolygonsDataFrame ####
########################################################
summary(KreisPET_spdf_1995)
at=(seq(0,200,1)) #
colors = colorRampPalette(c("skyblue3", "cornsilk",  "orangered3"))(1e3)

names(KreisPET_spdf)

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


KreisPET_spdf_1999_s <- KreisPET_spdf_1999



## Setup Colourscheme ##
cs1 <-colors
## Used for publications 
cs2 <- colors
#
at2 <- at

# ###############
# #### 1999 ####
# ## Setup Months considered ##
# # zteins <- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-48]
# zteins <- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)-49]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.99 <- spplot(KreisPET_spdf_1999_s, zcol=zteins,at=at2,col.regions= cs2)
# # KreisPET_spdf_Plt2.99
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_1999_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.99)
# dev.off()
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# ###############
# #### 2000 ####
# ## Setup Months considered ##
# zteins<- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-36]
# zteins<- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)-36]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2000 <- spplot(KreisPET_spdf_1999_s, zcol=zteins,at=at2,col.regions= cs2)
# # KreisPET_spdf_Plt2.2000
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2000_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2000)
# dev.off()
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# 
# ###############
# #### 2001 ####
# ## Setup Months considered ##
# zteins<- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-24]
# zteins<- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)-24]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2001 <- spplot(KreisPET_spdf_1999_s, zcol=zteins,at=at2,col.regions= cs2)
# # KreisPET_spdf_Plt2.2001
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2001_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2001)
# dev.off()
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# 
# ###############
# #### 2002 ####
# ## Setup Months considered ##
# ztzwei<- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)-12]
# ztzwei<- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)-12]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztzwei,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2002 <- spplot(KreisPET_spdf_1999_s, zcol=ztzwei,at=at2,col.regions= cs2)
# # KreisPET_spdf_Plt2.2
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2002_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2002)
# dev.off()
# 
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# 
# ###############
# #### 2003 ####
# ## Setup Months considered ##
# # ztdrei<- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)]
# ztdrei<- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztdrei,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2003 <- spplot(KreisPET_spdf_1999_s, zcol=ztdrei,at=at2,col.regions= cs2)
# # KreisPET_spdf_Plt2.2003
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2003_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2003)
# dev.off()
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# 
# 
# ###############
# #### 2004 ####
# ## Setup Months considered ##
# # ztvier<- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+12]
# ztvier<- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)+12]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt2.1 <- spplot(KreisPET_spdf_1999_s, zcol=ztvier,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2004 <- spplot(KreisPET_spdf_1999_s, zcol=ztvier,at=at2,col.regions= cs2)
# # KreisPET_spdf_Plt2.4 
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2004_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2004)
# dev.off() 
# 
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt2.3 <- spplot(KreisPET_spdf_1999_s, zcol="May2004",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt2.4 <- spplot(KreisPET_spdf_1999_s, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# 
# ###############
# #### 2005 ####
# ## Setup Months considered ##
# ztfünf<- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+24]
# ztfünf<- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)+24]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztsechs,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2005 <- spplot(KreisPET_spdf_1999_s, zcol=ztfünf,at=at2,col.regions= cs2)
# # KreisPET_spdf_Plt2.5 
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2005_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2005)
# dev.off()
# 
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# ###############
# #### 2006 ####
# ## Setup Months considered ##
# # ztsechs<- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+36]
# ztsechs<- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)+36]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztsechs,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2006 <- spplot(KreisPET_spdf_1999_s, zcol=ztsechs,at=at2,col.regions= cs2)
# # KreisPET_spdf_Plt2.6
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2006_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2006)
# dev.off()
# 
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# 
# ###############
# #### 2007 ####
# ## Setup Months considered ##
# # ztsieben<- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+48]
# ztsieben<- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)+48]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2007 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
# # KreisPET_spdf_Plt2.7
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2007_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2007)
# dev.off()
# 
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# ###############
# #### 2008 ####
# ## Setup Months considered ##
# ztsieben<- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+60]
# ztsieben<- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)+60]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2008 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2008_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2008)
# dev.off()
# 
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# ###############
# #### 2009 ####
# ## Setup Months considered ##
# # ztsieben<- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+72]
# ztsieben<- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)+72]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2009 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2009_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2009)
# dev.off()
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# ###############
# #### 2010 ####
# ## Setup Months considered ##
# ztsieben <- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
# ztsieben <- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)+84]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2010 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2010_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2010)
# dev.off()
# 
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# ###############
# #### 2011 ####
# ## Setup Months considered ##
# ztsieben <- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
# ztsieben <- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)+96]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2011 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2011_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2011)
# dev.off()
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# ###############
# #### 2012 ####
# ## Setup Months considered ##
# # ztsieben <- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
# ztsieben <- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)+108]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2012 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2012_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2012)
# dev.off()
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# ###############
# #### 2013 ####
# ## Setup Months considered ##
# # ztsieben <- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
# ztsieben <- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)+120]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# 
# KreisPET_spdf_Plt2.2013 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
# png("./figures/figures_exploratory/PET/PET_Anomaly_2013_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2013)
# dev.off()
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# ###############
# #### 2014 ####
# ## Setup Months considered ##
# # ztsieben <- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
# ztsieben <- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61)+132]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2014 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
# 
# png("./figures/figures_exploratory/PET/PET_Anomaly_2014_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2014)
# dev.off()
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 
# ###############
# #### 2015 ####
# ## Setup Months considered ##
# # ztsieben <- names(KreisPET_spdf_1999_s)[c(63,64,65,66,59,60,61,62,55,56,57,58)+84]
# ztsieben <- names(KreisPET_spdf_1999_s)[c(62,63,64,59,60,61) + 144]
# ## Plot des Jahres 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt1 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at1,col.regions= cs1)
# ## Exported als png im format width 800 and height 800
# KreisPET_spdf_Plt2.2015 <- spplot(KreisPET_spdf_1999_s, zcol=ztsieben,at=at2,col.regions= cs2)
# png("./figures/figures_exploratory/PET/PET_Anomaly_2015_short.png", width = 800, height = 800, units = "px",)
# plot(KreisPET_spdf_Plt2.2015)
# dev.off()
# # ## Plot des Mai 2003 der extrahierten PET Werte auf Kreisniveau für Deutschland mit farbwahl für PETsentation
# # KreisPET_spdf_Plt3 <- spplot(KreisPET_spdf_1999, zcol="May2003",at=at1, col.regions= cs1)
# # ## Exported als png im format 800 and height 800
# # KreisPET_spdf_Plt4 <- spplot(KreisPET_spdf_1999, zcol="Apr2003",at=at2, col.regions= cs2)
# # ## Comment: Pattern stimmen mit orginal Daten überein 
# 

####################################################################################################################################################################
################################################################### Kippen des Datensatzes #########################################################################
####################################################################################################################################################################

###################################################################
#### Jetzt muss ich noch den PET-Datensatz entsPEThend kippen ####
###################################################################
head(KreisPET_spdf_1995)
##########################

##########################
#### reshape::reshape ####

#### SpatialPolygonsDataFrame in normalen DataFrame ####
KreisPET_df_1995 <- as.data.frame(KreisPET_spdf_1995)
head(KreisPET_spdf_1995)
names(KreisPET_spdf_1995)

#### Checken, ob Namen sich nicht verändert haben
all(names(KreisPET_df_1995)==names(KreisPET_spdf_1995)) # check positive

length(names(KreisPET_df_1995))
names(KreisPET_df_1995)
dim(KreisPET_df_1995)
21*12
21*412
## Beschreibung des Vorganges ##
'Ziel: Ich habe 412 Beobachtungen pro Zeitschritt (Jahr). 
Da mein Datensatz tidy ist, also die Jahre untereinander fortlaufend angeordnet sind, müssen meine Variablen entsPEThend angepasst werden.
Soll heißen aus den PET Variablen mit Monats und Jahres zuordnung wird nun ein PET mit Monats Zuordnung und die Jahre verschmelzen alle in 
eine Spalte. 
Damit habe ich 12 Spalten für den PET, also eine pro Monat, und 16 (Jahre) mal 412 Beobachtungen, also 6592 Beobachtungen.
Für dieses Vorgehen reshape ich den Kreis_PET_dataframe von wide nach long mit Hilfe der stats::reshape function. 
Dazu muss ich eine Liste mit den PET mit Monat und Jahres Zuordnung erstellen, welche dann jeweils in die PET mit Monats zuordnung
im long Format übergeht. -> listMonthYearNames[[i]]
Anschließend erstelle ich die Namen der PET mit reiner Monats Zurdnung -> listMonthNames
Mit Hilfe der Year Variablen werden dann die Beeobachtungen innerhalb der PET mit Monats Zuordnungen nach den Jahren differenziert.
'

#### Make a list of vectors of months ####
' Diese list dient der Indizierung der jeweiligen Monate. Sie startet bei 7, da die 6 columns davor die räumlichen Einheiten definieren.'
length(names(KreisPET_df_1995))


list0<-list(c(7,length(names(KreisPET_df_1995))-11),
            c(8,length(names(KreisPET_df_1995))-10),
            c(9,length(names(KreisPET_df_1995))-9),
            c(10,length(names(KreisPET_df_1995))-8), 
            c(11,length(names(KreisPET_df_1995))-7),
            c(12,length(names(KreisPET_df_1995))-6),
            c(13,length(names(KreisPET_df_1995))-5),
            c(14,length(names(KreisPET_df_1995))-4),
            c(15,length(names(KreisPET_df_1995))-3), 
            c(16,length(names(KreisPET_df_1995))-2), 
            c(17,length(names(KreisPET_df_1995))-1), 
            c(18,length(names(KreisPET_df_1995))))
# those list define the starting and ending point for the steps, 12 each
list0 


## Container 
listMonthYearNames <- list()

## Lenght of considered time period ##
i=1
length(seq(list0[[i]][1],list0[[i]][2],12))

#### Loop zum erstellen der Liste listMonthYearNames mit den IDs, welche jeweils den gleichen Monatesnamen, über alle Jahre hinweg, haben ####
for (i in 01:12){
  listMonthYearNames[i] <- list(c(names(KreisPET_df_1995)[c(seq(list0[[i]][1],list0[[i]][2],12))]))
}
# class(listMonthYearNames[1])
# class(listMonthYearNames)
listMonthYearNames # list including a list of the listMonthYearNames across the years. This is the basis for the new variables, i.e the PET in each month
listMonthYearNames[[3]] # list of all marches across the 21 years considered here

#### Make the names for the new variables
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
listMonthNames<-list(c("PET_Jan"), c( "PET_Feb"), c("PET_Mar"), c("PET_Apr"), c("PET_May"), c("PET_Jun"),c("PET_Jul"),c("PET_Aug"),c("PET_Sep"), c("PET_Oct"), c("PET_Nov"), c("PET_Dec"))
listMonthNames


### Reshape from wide to long für jeden Monat ####
# KreisPET_df_19953 <- reshape(KreisPET_df_1995, 
#              varying = listMonthYearNames[[1]],
#              v.names = listMonthNames[[1]],
#              timevar = "YearofJan", 
#              times = listMonthYearNames[[1]],
#              direction = "long")
# 
# names(KreisPET_df_19953)[184]
# names(KreisPET_df_19953)
# head(KreisPET_df_19953)
# rownames(KreisPET_df_19953)<-NULL
# rownames(KreisPET_df_19953)

#### Loop, welche mir die PET pro Monat jeweils in einer Column ausgibt ####

8652/21

y <- data.frame(1:412*21) # set container
x <- data.frame() # set container

##############################################################################################################
### loop over the twelve listMonthYearNames, split up, an then filled up with the values across the years ####
for(i in 1:12) {
  x <- reshape (KreisPET_df_1995, 
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
PET_months <- cbind(x[,c(1:4, length(x) - 1)],y)

names(PET_months)
head(PET_months)
rownames(PET_months) <- NULL

## Explore whether comIds and years match ##
head(PET_months, 15) # check
tail(PET_months, 15) # check
# In den rownames steht der Monat Dezember, da dies der letzte Monat ist, der gemelted, also in eine Spalte gefasst wurde. 
# Sieht aber sonst recht ordentlich aus.

####################################################
## Change strings which are necessary for merging ##

## Befreien der year Variable von den Monatsangaben, hier Dezember
PET_months$Year <- as.integer(str_sub(PET_months$Year,7,10 ))

#### Verkürzen des ID-Strings, also KreisPET_spdf_1995$RS  ####
str_length(PET_months$RS) #12
PET_months$RS <- as.factor(str_sub(PET_months$RS,1,5))
str_length(PET_months$RS) #5

unique(PET_months$RS)
head(PET_months)

## Anpassen der Namen fürs mergen
colnames(PET_months)[colnames(PET_months)=="Year"] <- "year"
colnames(PET_months)[colnames(PET_months)=="RS"] <- "comId"
head(PET_months)


##################################################################################
#### Abgleichen der neu erstellten Daten mit KreisPET_df_1995 (orginal Daten) ####
head(KreisPET_df_1995$PETJan1995)%in%head(PET_months$PET_Jan) 

tail(KreisPET_df_1995$PETDec2015)%in%tail(PET_months$PET_Dec) 

## Erstellen und überprüfen der beiden Vektoren, die jeweils den PET des Jahres 1999 im Monat Januar wiedergebeb


## Vector in PET_months des PET Januar 1999
PET_months$PET_Jan[PET_months$year == 1999]
length(PET_months$PET_Jan[PET_months$year==1999])

## Vector in KreisPET_df_1995 des PET Januar 1999
KreisPET_df_1995$PETJan1999
length(KreisPET_df_1995$PETJan1999)

all(PET_months$PET_Jan[PET_months$year==1999]%in%KreisPET_df_1995$PETJan1999) # check
# Daten sehen gut aus
names(KreisPET_df_1995)
all(  PET_months$PET_Dec[PET_months$year==1999]    %in%      KreisPET_df_1995$PETDec1999  ) # check

##########################################################################
#### Produce lagged variabled of PET for the months after the harvest ####
##########################################################################
## PET ##
names(PET_months)
"Das erstellen der lags muss vor dem mergen passsieren, da yield nur bis 1999 geht."


PET_months <- slide(PET_months, Var ="PET_Aug", GroupVar= "comId", slideBy = -1)
PET_months <- slide(PET_months, Var ="PET_Sep", GroupVar= "comId", slideBy = -1)
PET_months <- slide(PET_months, Var ="PET_Oct", GroupVar= "comId", slideBy = -1)
PET_months <- slide(PET_months, Var ="PET_Nov", GroupVar= "comId", slideBy = -1)
PET_months <- slide(PET_months, Var ="PET_Dec", GroupVar= "comId", slideBy = -1)

head(PET_months)                                                

## Check for validity
head(PET_months[PET_months$year==1999,])[13:17] %in% head(PET_months[PET_months$year==2000,])[18:22]  
' Da zum Beispiel der August des Jahre 1999 mit dem lag des Jahres 2000 übereinstimmt, scheint alles zu passen'

table(PET_months$comId)  

## Umbennen der lagged Variablen ##
names(PET_months)
names(PET_months)[names(PET_months)==c("PET_Aug-1" )]  <- c("PET_Aug_lag")
names(PET_months)[names(PET_months)==c("PET_Sep-1" )]  <- c("PET_Sep_lag")
names(PET_months)[names(PET_months)==c("PET_Oct-1" )]  <- c("PET_Oct_lag")
names(PET_months)[names(PET_months)==c("PET_Nov-1" )]  <- c("PET_Nov_lag")
names(PET_months)[names(PET_months)==c("PET_Dec-1" )]  <- c("PET_Dec_lag")

head(PET_months)
dim(PET_months)
###########################################
#### Write newly created PET_months ####

write.csv(PET_months, "data/data_processed/PET_months.csv")
write.table(PET_months, "data/data_processed/PET_months")
