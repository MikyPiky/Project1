#############################################################################################################################################################################################
#############################################################################################################################################################################################

#### File Description ####
' - Verarbeiten der NetCDF Datei mit den Precipitation Werten, so dass als time series für die einzelnen Landkreise.
    (- Precipitation (4km Auflösung) maskiert für non- irrigated land, dazu disaggregiert auf 100 * 100 m, und danach extrahiert auf Polygon durch Stephan Thober.)
    - Einlesen der NetCDF
    - Verbinden mit KreisPolOGR (vg2500_krs) für räumlich Zuordnung (vg2500_krs shape wurde auch für die Berechnung der 4km_prec.nc genutzt. Daher ist es die gleiche Zuordnung(SMI Were
        beruhend auf dieses Vorgehen zeigen die gleichen Strukturen))
    - Plotten der erstellten Daten mit hilfe von spplot{sp}
    - Mergen des erstellten SpatialPolygonsDataFrame mit Yield_Prec_detrend (kommt aus Merge_SMI_yield Skript)
    - Manipulation of the precepitation data
      - Löschen der comId 02000 und 11000 #
      - Berechnen der Lags der Monate August bis Oktober
      - Demean Precipitation Data with constant (not conditioned on communities)
      - Standardize Precipitation (not condition on communities) -> z-score

'

#### Dependencies and Input ####
'
  - 4km_pre.nc (Stephan Thober) -> precipitiation netcdf
  - vg2500_krs (extern bereitgestellt) -> shape mit polygonen der Kreise
  - Yield_SMI_detrend <- MergeSMI_Yield.R
'
#### Output ####
##Files
' 
  - Prec_months_1999
  - Yield_SMI_Prec
'
## Plots
' - Plot des Jahres 2003 der extrahierten prec Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
  - Plot des Mai 2003 der extrahierten prec Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
'


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
library(ggplot2)
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(plyr)
library(ncdf4)
library(reshape)
library(stringr)
library(eeptools)
library("plm")
library("car")
library("lmtest")
library("lattice")
library("zoo")
library("scales")
library("nlme")
library("lme4")
library("mgcv")
library("apsrtable")
library("texreg")
library("DataCombine")
library("reshape2")
library("gplots")
library("pracma")

#############################################################################################################################################################################################
#############################################################################################################################################################################################


###############################
#### Laden des prec NetCDF ####
###############################
# prec<-brick("/home/peichl/Documents/projects/correlation/data/data_raw/4km_pre.nc", varname="pre_lk")
# prec
## Funktioniert mit dieser File nicht so gut, daher benutze ich die Alternative

# #### Alternatives Laden: ncdf4 ####
prec_open <- nc_open("/home/peichl/Documents/projects/correlation/data/data_raw/4km_pre.nc")
print(prec_open) 
prec <- ncvar_get(prec_open, varid="pre_lk")
nc_close(prec_open)
#offensichtlich habe ich hier 61 Jahre bzw 732 Monate seit Januar 1950 
dim(prec) 
732/12
class(prec)
levelplot(prec)
head(prec)



#######################
#### -9999 Werte in NA ####
u<-prec== -9999
prec[u]<-NA

u<-prec== 0
prec[u]<-NA
levelplot(prec)

# Interpretation
'
Es ist sehr unwahrscheilich, dass es in einem Monat nicht geregnet hat. Dennoch könnte man sich das vorstellen. Daher wandle 
ich die Nullwerte nicht in NAs um.

'

###########################################################################################################
#### Datensatz an Spatialpolygondataframe der Landkreise anhängen, um räumliche Beziehung herzustellen ####
###########################################################################################################

####################################################################################
#### Laden der shape mit den Polygonen der Kreise und deren räumliche Zuordnung ####
####################################################################################
KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")
str(KreisPolOGR,2)
head(KreisPolOGR)
dim(KreisPolOGR)
spplot(KreisPolOGR, zcol="SHAPE_AREA")

## Transformieren der class matrix in die class data.frame
prec_df<-as.data.frame(prec)
dim(prec_df)


#### Verändern der Namen im prec_df ####
# Precipitation geht ein Jahr vor SMI los, also im Januar 1950
names(prec_df)
idx <-seq(from=as.Date('1950-1-1'), to=as.Date('2015-12-1'), 'month')
idx <-as.yearmon(idx)
idx
idx_str<-paste("Prc",idx, sep="")
idx_names<-make.names(c(idx_str), unique = TRUE)
idx_gsub<-gsub(" ","",idx_str)
idx_names<-make.names(c(idx_gsub), unique = TRUE)
idx_names
length(idx_names)
length(names(prec_df))
names(prec_df)<-idx_names

#### Rownames anpassen ####
dim(prec_df)
rownames(prec_df) <-0:411

head(prec_df)

######################################
#### Data.frame: 1995 produzieren ####
names(prec_df)
names(prec_df)[(length(prec_df)-191):length(prec_df)]
prec_df_1995<-prec_df[,(length(prec_df)-191):length(prec_df)]

head(prec_df_1995)
names(prec_df_1995)
rownames(prec_df_1995)<-0:411


######################################
#### Data.frame: 1999 produzieren ####
names(prec_df)[(length(prec_df)-143):length(prec_df)]
prec_df_1999<-prec_df[,(length(prec_df)-143):length(prec_df)]
head(prec_df_1999)
names(prec_df_1999)

rownames(prec_df_1999)<-0:411

###########################
#### Maptools::spCbind ####
KreisPrec_spdf<-spCbind(KreisPolOGR,prec_df)
KreisPrec_spdf_1995 <- spCbind(KreisPolOGR,prec_df_1995)
KreisPrec_spdf_1999 <- spCbind(KreisPolOGR,prec_df_1999)
 
summary(KreisPrec_spdf_1999) # Nach Umwandlung der 0 Werte macht die Summary Statistik nun mehr Sinn.

# Interpretation:
'
Ich setze ich vor die Prec Daten einen Vektor mit der räumlichen Zuordnung. Dafür muss dieser Vektor die gleiche Spaltenlänge und Reihenfolge wie der Data.frame haben.
Davon ist auszugehen, da diese Shapefile für die räumlich hochrechnung der Prec Daten benutzt wurde. 
'


# ###################################################################
# #### Export der erstellten Dateien (SpatialPolygoneDataFrames) ####
# ## ... für den gesamten Zeitraum
# writePolyShape(KreisPrec_spdf, "./data/data_raw/KreisPrec_spdf")
# ## ... ab 1995
# writePolyShape(KreisPrec_spdf_1995, "./data/data_raw/KreisPrec_spdf_1995.shp")
# ## ... ab 1999
# writePolyShape(KreisPrec_spdf_1999, "./data/data_raw/KreisPrec_spdf_1999.shp")
# 
# ###################################################################
# #### Import der erstellten Dateien (SpatialPolygoneDataFrames) ####
# 
# KreisPrec_spdf<-readShapePoly("./data/data_raw/KreisPrec_spdf.shp")
# KreisPrec_spdf_1995<-readShapePoly("./data/data_raw/KreisPrec_spdf_1995.shp")
# KreisPrec_spdf_1999<-readShapePoly("./data/data_raw/KreisPrec_spdf_1999.shp")
# names(KreisPrec_spdf)
# names(KreisPrec_spdf_1995)
# names(KreisPrec_spdf_1999)
# 

#### Projection des ursrünglichen SpatialPolygonsDataFrame übertragen
proj4string(KreisPrec_spdf)<-proj4string(KreisPolOGR)


###################################################################################################################################
############################################# Plotten der Niederschlagsdaten ######################################################
###################################################################################################################################

## Set intervalls  for coloring ##
summary(KreisPrec_spdf)
KreisPrec_spdf_1995_nona <- na.omit(KreisPrec_1995)
# sapply(KreisPrec_spdf_1995_nona, mean)


at=(seq(0,300,1)) # das tatsächliche maximum liegt um die 300 in dem 1999 Datensatz, jedoch kommt diese eher selten vor. Daher macht es wohl Sinn einen niedrigeren Wert zu wählen.

names(KreisPrec_spdf)

ztdrei<-c( "PrcSep2003"  ,"PrcOkt2003" , "PrcNov2003" , "PrcDez2003",  "PrcMai2003" , "PrcJun2003" ,"PrcJul2003" , "PrcAug2003","PrcJan2003",  "PrcFeb2003" , "PrcMär2003", "PrcApr2003" )

## Modification of the trellis, i.e. lattice, i.e. ssplot ##
my.theme = trellis.par.get()
names(my.theme)
my.theme$panel.background
trellis.par.set("background", list(col = "white")) 
trellis.par.set("panel.background", list(col = "white")) 
trellis.par.set("strip.background", list(col = "white")) 

show.settings()


## Set colors
# one option via color brewer
# colors <- brewer.pal(9, "YlGn")
# or manual
colors = colorRampPalette(c("orangered3", "cornsilk", "skyblue3"))(1e3)

## Plot des Jahres 2003 der extrahierten prec Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
KreisPrec_spdf_Plt1 <- spplot(KreisPrec_spdf, at=at, ztdrei, col.regions= colors)
KreisPrec_spdf_Plt2 <- spplot(KreisPrec_spdf, at=at, "PrcFeb2003", col.regions= colors)

## Interpretation ##
'
Interpretation der Niederschlagswerte ist schwierig, da man nicht weiß, was das langjährige Mittel ist. Für die Interpretation 
die Gragfik wären wohl Anomalien (betrachtung der Standardabweichung) besser. 
Ich gehe hier von einem Maximum von 300 aus, was etwa der Regenmenge im Februar 2002 entspricht.

'


#############################################################################################################################################################################################
############################# Mergen des erstellten SpatialPolygonsDataFrame mit Yield_Prec_detrend ##########################################################################################
#############################################################################################################################################################################################


'Mergen soll mit Hilfe der ComId und des Jahres passieren'

############################################
##  Erstellen der ComId für den DatenSatz ##
head(KreisPrec_spdf_1995)
KreisPrec_spdf_1995$RS
KreisPrec_spdf_1995$comId<-as.factor(str_sub(KreisPrec_spdf_1995$RS,1,5))
unique(KreisPrec_spdf_1995$comId)

##################################
#### Reshapen des Datensatzes ####
##################################

class(KreisPrec_spdf_1995)
KreisPrec_df_1995 <- as.data.frame(KreisPrec_spdf_1995)
dim(KreisPrec_1995) # die Dimension ist die selbe wie in MergeSMI_Yield, daher werde ich einfach den gleichen reshape Algorithmus verwenden

## Reorder Names ##
names(KreisPrec_df_1995)
length(names(KreisPrec_df_1995))
KreisPrec_df_1995 <-KreisPrec_df_1995[c(1:5,198,(6:197))]
head(KreisPrec_df_1995)

#### Make a list of vectors of months ####
names(KreisPrec_df_1995)
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
list0<-list(c(7,187), c(8,188), c(9,189), c(10,190), c(11,191), c(12,192),c(13,193),c(14,194),c(15,195), c(16,196), c(17,197), c(18,198))
# those list define the starting and ending point for the steps, 12 each
list0

#### Loop zum erstellen der Liste listMonthYearNames mit den IDs, welche jeweils den gleichen Monatesnamen, über alle Jahre hinweg, haben ####
listMonthYearNames<-list() # set container

for (i in 01:12){
  listMonthYearNames[i]<-list(c(names(KreisPrec_df_1995)[c(seq(list0[[i]][1],list0[[i]][2],12))]))
}
listMonthYearNames # list including a list of the listMonthYearNames across the years. This is the basis for the new variables, i.e the Prec in each month
listMonthYearNames[[3]] # least of all marches across the 16 years considered here

#### Make the names for the new variables
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
listMonthNames<-list(c("Prec_Jan"), c( "Prec_Feb"), c("Prec_Mar"), c("Prec_Apr"), c("Prec_Mai"), c("Prec_Jun"),c("Prec_Jul"),c("Prec_Aug"),c("Prec_Sep"), c("Prec_Oct"), c("Prec_Nov"), c("Prec_Dec"))
listMonthNames

#### Loop, welche mir die Prec pro Monat jeweils in einer Column ausgibt ####
y<-data.frame(1:6592) # set container
x<-data.frame() # set container


### loop over the twelve listMonthYearNames, split up, an then filled up with the values across the years
for(i in 1:12) 
  {
  x <- reshape (KreisPrec_df_1995, 
                varying = listMonthYearNames[[i]],
                v.names = listMonthNames[[i]],
                timevar = "Year", 
                idvar = "RS",
                times = listMonthYearNames[[i]],
                direction = "long")
  print(names(x[listMonthNames[[i]]])) # here I print the name of the newly created variable
  y<- cbind(y,x[listMonthNames[[i]]]) # here I make a dataframe with the twelfe newly created variables
  }
## Explore x, also einen komplett reshaped Datensatz, aber nur eine neue Variable, da loop ##
head(x)
names(x)
dim(x)
'Die reshape fn ist bis December durchgelaufen, passt also'


## Explore the newly build dataframe with the Precipitiation for each Monnth ##
names(y)
rownames(y)
## Get rid of non necessary variables
y$X1.6592<-NULL
head(y)
# Die Reihenfolge ist so, dass erst nach dem Jahr und dann nach comID geordnet ist. Also erst alle comIDs für das Jahr 1995, usw.

####################################################
#### Erstellen eines data.frames mit ID angaben ####
'bestehend aus einem komplett reshapden Datensatz (x), und dem Datensatz welcher der neue gebildete Variable der Loops aus dem Reshape Prozess
vereint'
names(x)
names(y)

head(x[,c(5:6, 183)])
Prec_months<-cbind(x[,c(5:6, 183)],y)

names(Prec_months)
head(Prec_months)
rownames(Prec_months)<-NULL

## Explore whether comIds and years match ##
head(Prec_months, 15) # check
tail(Prec_months, 15) # check
# In den rownames steht der Monat Dezember, da dies der letzte Monat ist, der gemelted, also in eine Spalte gefasst wurde. 
# Sieht aber sonst recht ordentlich aus.

## Befreien der year Variable von den Monatsangaben, hier Dezember ##
str_sub(Prec_months$Year,7,10 )
Prec_months$Year<-as.integer(str_sub(Prec_months$Year,7,10 ))
head(Prec_months)
unique(Prec_months$Year)

## Anpassen der Namen fürs mergen
colnames(Prec_months)
colnames(Prec_months)[3] <- "year"

###################################################################################
#### Abgleichen der neu erstellten Daten mit KreisPrec_df_1995 (orginal Daten) ####
head(KreisPrec_df_1995)

## Erstellen und überprüfen der beiden Vektoren, die jeweils den Prec des Jahres 1999 im Monat Januar wiedergeben ##
## Vector in Prec_months des Prec Januar 1999
Prec_months$Prec_Jan[Prec_months$year==1999]
length(Prec_months$Prec_Jan[Prec_months$year==1999])

## Vector in KreisPrec_df_1995 des Prec Januar 1999
names(KreisPrec_df_1995)
KreisPrec_df_1995$PrcJan1999
length(KreisPrec_df_1995$PrcJan1999)

all(Prec_months$Prec_Jan[Prec_months$year==1999]%in%KreisPrec_df_1995$PrcJan1999)
# Daten sehen gut aus

#############################################################################################################################################################################################
################################################################## Manipulation der Precipitation Daten #####################################################################################
#############################################################################################################################################################################################

#######################################
## Löschen der comId 02000 und 11000 ##
#######################################
' Both cities are neglected '
unique(Prec_months$comId)
names(Prec_months)
Prec_months<-Prec_months[!Prec_months$comId=="02000",]  #Hamburg
Prec_months<-Prec_months[!Prec_months$comId=="11000",]  #Berlin

######################################################################################
## Berechnen der lags der Precipitaion Variablen für die Monate August bis December ##
######################################################################################

head(Prec_months)

## Ordnen der Spalten so, dass die Variablen erst nach comID und dann nach time geordnet werden.
Prec_months<-Prec_months[order(Prec_months$comId, Prec_months$year),]

Prec_months_arr<-slide(Prec_months, Var ="Prec_Aug", GroupVar= "comId", slideBy = -1)
Prec_months_arr<-slide(Prec_months_arr, Var ="Prec_Sep", GroupVar= "comId", slideBy = -1)
Prec_months_arr<-slide(Prec_months_arr, Var ="Prec_Oct", GroupVar= "comId", slideBy = -1)
Prec_months_arr<-slide(Prec_months_arr, Var ="Prec_Nov", GroupVar= "comId", slideBy = -1)
Prec_months_arr<-slide(Prec_months_arr, Var ="Prec_Dec", GroupVar= "comId", slideBy = -1)

head(Prec_months_arr)

## Change names
names(Prec_months_arr)
names(Prec_months_arr)[names(Prec_months_arr)==c("Prec_Aug-1" )] <-c("Prec_Aug_lag")
names(Prec_months_arr)[names(Prec_months_arr)==c("Prec_Sep-1" )] <-c("Prec_Sep_lag")
names(Prec_months_arr)[names(Prec_months_arr)==c("Prec_Oct-1" )] <-c("Prec_Oct_lag")
names(Prec_months_arr)[names(Prec_months_arr)==c("Prec_Nov-1" )] <-c("Prec_Nov_lag")
names(Prec_months_arr)[names(Prec_months_arr)==c("Prec_Dec-1" )] <-c("Prec_Dec_lag")

###################################
####  Data Frame Starting 1999 ####
names(Prec_months_arr)
unique(Prec_months_arr$year)
Prec_months_1999<-Prec_months_arr[Prec_months_arr$year >= 1999,] 
unique(Prec_months_1999$year)


#############################################
## Demean Precipitation Data with constant ##

## Operations with no conditioning of coms
## April

head(Prec_months_1999)

## Löschen der Daten mit NULL ##
'Es ist sehr unwahrscheinlich, dass es einem Monat in Deutschland nicht geregnet hat.'
any(Prec_months_1999$Prec_Apr == 0)
Prec_months_1999$Prec_Apr[Prec_months_1999$Prec_Ap r== 0] <- NA

###############
## demeaning ##
names(Prec_months_1999)
length(names(Prec_months_1999))

x<-NULL
demeanPrec <- NULL
for (i in 4:20)
{
  x <- Prec_months_1999[,i] - mean(Prec_months_1999[,i], na.rm = T)
  demeanPrec <- cbind(demeanPrec,x)
}
head(demeanPrec)
dim(demeanPrec)

colnames(demeanPrec) <- c("Prec_Jan_demean",  "Prec_Feb_demean" , "Prec_Mar_demean" , "Prec_Apr_demean" ,"Prec_Mai_demean" , "Prec_Jun_demean" ,"Prec_Jul_demean" , "Prec_Aug_demean" , "Prec_Sep_demean" ,
                 "Prec_Oct_demean", "Prec_Nov_demean","Prec_Dec_demean","Prec_Aug_lag_demean","Prec_Sep_lag_demean","Prec_Oct_lag_demean","Prec_Nov_lag_demean" ,"Prec_Dec_lag_demean" )


Prec_months_1999_demean <- cbind(Prec_months_1999, demeanPrec)
names(Prec_months_1999_demean)

## Compare to seperately produced vector for April ##
Prec_months_1999 <- transform(Prec_months_1999_demean , demeanPrec_Apr = detrend(Prec_Apr, "constant"))
all(Prec_months_1999$demeanPrec_Apr %in% Prec_months_1999$Prec_Apr_demean)
## Delete demeanPrec_Apr ##
Prec_months_1999$demeanPrec_Apr <- NULL

# Interpretation
'
Für den Monat April scheinen die Ergebnisse zu stimmen.
'

###################
## standardizing ##

names(Prec_months_1999_demean)
length(names(Prec_months_1999_demean))

x<-NULL
zscore<-NULL
for (i in 4:20)
{
  x <- (Prec_months_1999_demean[,i] - mean(Prec_months_1999_demean[,i], na.rm = T))/sd(Prec_months_1999_demean[,i], na.rm=T)
  zscore <- cbind(zscore,x)
}
head(zscore)
dim(zscore)

colnames(zscore) <- c("Prec_Jan_zscore",  "Prec_Feb_zscore" , "Prec_Mar_zscore" , "Prec_Apr_zscore" ,"Prec_Mai_zscore" , "Prec_Jun_zscore" ,"Prec_Jul_zscore" , "Prec_Aug_zscore" , "Prec_Sep_zscore" ,
                 "Prec_Oct_zscore", "Prec_Nov_zscore","Prec_Dec_zscore","Prec_Aug_lag_zscore","Prec_Sep_lag_zscore","Prec_Oct_lag_zscore","Prec_Nov_lag_zscore" ,"Prec_Dec_lag_zscore" )


## Cbind the new created z-score dataframe with the original Dataframe
Prec_months_1999_demean_zscore <- cbind(Prec_months_1999_demean, zscore)
names(Prec_months_1999_demean_zscore)



## Compare to seperately produced vector for April ##
Prec_months_1999_demean_zscore <- transform(Prec_months_1999_demean_zscore , zScore_Pre_Apr <- (Prec_Apr - mean(Prec_Apr, na.rm=T))/sd(Prec_Apr, na.rm=T))
all(Prec_months_1999_demean_zscore$zScore_Pre_Apr %in% Prec_months_1999_demean_zscore$Prec_Apr_zscore)
## Delete zScore_Pre_Apr
Prec_months_1999_demean_zscore$zScore_Pre_Apr <- NULL

# Interpretation
'
Für den Monat April scheinen die Ergebnisse zu stimmen.
'


#   ################  
# # ## detrending ##
# # Prec_months_1999 <- transform(Prec_months_1999 , detrendPrec_Apr = detrend(demeanPrec_Apr, "linear"))
# # Prec_months_1999$detrendPrec_Apr
# 
# 
# ## z-score and detrending ##
# # z-score #
# Prec_months_1999 <- transform(Prec_months_1999 , zScore_Prec_Apr = demeanPrec_Apr/sd(Prec_Apr, na.rm=T))
# Prec_months_1999$zScore_Prec_Apr
# # # detrend z-score #
# # Prec_months_1999 <- transform(Prec_months_1999 , detrendzScore_Prec_Apr = detrend(zScore_Prec_Apr, "linear"))
# 
# 
# ## Plots ##
# par(mfrow=c(2,2))
# 
# # Plot of absolute values #
# plot(Prec_months_1999$Prec_Apr~Prec_months_1999$year)
# abline(mean(Prec_months_1999$Prec_Apr, na.rm=T),0)
# 
# # Plot of demeaned absolute Values #
# plot(Prec_months_1999$demeanPrec_Apr~Prec_months_1999$year)
# fit1 <- lm(Prec_months_1999$demeanPrec_Apr~Prec_months_1999$year)
# abline(fit1)
# abline(0,0,col=2)
# 
# any(Prec_months_1999$Prec_Apr==0)
# summary(Prec_months_1999$Prec_Apr)
# 
# # # Plot of detrended absolute Values #
# # plot(Prec_months_1999$detrendPrec_Apr~Prec_months_1999$year)
# # fit2 <- lm(Prec_months_1999$detrendPrec_Apr~Prec_months_1999$year)
# # abline(fit2)
# # abline(0,0,col=2)
# 
# # Plot of z-score #
# plot(Prec_months_1999$zScore_Prec_Apr~Prec_months_1999$year)
# fit3 <- lm(Prec_months_1999$zScore_Prec_Apr~Prec_months_1999$year)
# abline(fit3)
# abline(0,0,col=2)
# 
# # # Plot of detrended z-score #
# # plot(Prec_months_1999$detrendzScore_Prec_Apr~Prec_months_1999$year)
# # fit4 <- lm(Prec_months_1999$detrendzScore_Prec_Apr~Prec_months_1999$year)
# # abline(fit4)
# # abline(0,0,col=2)


head(Prec_months_1999)


## Interpretation
'
Wie schon bei yield hat das detrenden, welches nicht explicit nach den seperaten räumlichen Einheiten bedingt wird, keinen sichtbare Wirkung im Plot. Die Zahlen haben sich jedoch verändert.
Frage, welche Daten ich daher übernehmen. Also die demeaned data machen Sinn (nach Luis) und die Standardisierten. Detrended Daten nehme ich wieder raus.
'
################################
#### Write Prec_months_1999 ####


write.csv(Prec_months_1999_demean_zscore, "data//data_processed/Prec_months_1999")

##########################################
#### Read Prec_months for cbinding it ####
Prec_months <- read.csv("data//data_processed/Prec_months_1999")
head(Prec_months)
Prec_months$X <- NULL 

#########################################################################
#### Mergen von Prec_months_1999 mit Yield_SMI_detrend anhand von RS ####

## Read Yield_SMI_detrend ##
Yield_SMI_detrend <- read.csv("data//data_processed/Yield_SMI_detrend")

names(Yield_SMI_detrend)
head(Yield_SMI_detrend)
dim(Yield_SMI_detrend)


## Delete not needed IDvalues ##
Yield_SMI_detrend$X.1 <- Yield_SMI_detrend$X <- NULL
length(Yield_SMI_detrend)

## Vergleichen der beiden year Vektoren ##
all(Yield_SMI_detrend$year == Prec_months$year)

## Vergleichen der beiden comId Vektoren ##
Yield_SMI_detrend$comId # aufsteigend von 1001 zu 16077
Prec_months$comId # aufsteigend von 1001 zu 16077
all(Yield_SMI_detrend$comId %in% Prec_months$comId) # funxt nur, wenn beiden Datensätze neu eingelesen wurden. 

# Interpretation
' 
Die Reihenfolge der beiden Datensätze ist die gleiche
'
## Da mir die comID darstellung aus Prec_months besser gefällt übernehme ich diese
Yield_SMI_detrend$comId <- Prec_months$comId

## Delete year and comId in Prec_months ##
Prec_months$year <- Prec_months$comId <- NULL
names(Prec_months)

## cbind Yield_SMI_detrend und Prec_months ##
Yield_SMI_Prec<-cbind(Yield_SMI_detrend,Prec_months)
names(Yield_SMI_Prec)
rownames(Yield_SMI_Prec)<-NULL

## Delete not needed IDvalues ##
length(Yield_SMI_Prec)
names(Yield_SMI_Prec)

## Reorder Spalten von Yield_SMI_Prec###
names(Yield_SMI_Prec[c(1:6,44, 7:43, 45:length(Yield_SMI_Prec))])

Yield_SMI_Prec <- Yield_SMI_Prec[c(1:6,44, 7:43, 45:length(Yield_SMI_Prec))]
names(Yield_SMI_Prec)
dim(Yield_SMI_Prec)
head(Yield_SMI_Prec,15)
tail(Yield_SMI_Prec,16)

table(Yield_SMI_Prec$year) # statt 525 nur noch 410 Beobachtungen

##############################
#### Write Yield_SMI_Prec ####
##############################

write.csv(Yield_SMI_Prec, "data//data_processed/Yield_SMI_Prec")

