#############################################################################################################################################################################################
#############################################################################################################################################################################################

#### File Description ####
' - Verarbeiten der NetCDF Datei mit den average Temperature Werten, so dass als time series für die einzelnen Landkreise.
(- average Temperature (4km Auflösung) maskiert für non- irrigated land, dazu disaggregiert auf 100 * 100 m, und danach extrahiert auf Polygon durch Stephan Thober.)
- Einlesen der NetCDF
- Verbinden mit KreisPolOGR (vg2500_krs) für räumlich Zuordnung (vg2500_krs shape wurde auch für die Berechnung der 4km_Tavg.nc genutzt. Daher ist es die gleiche Zuordnung(SMI Were
beruhend auf dieses Vorgehen zeigen die gleichen Strukturen))
- Plotten der erstellten Daten mit hilfe von spplot{sp}
- Mergen des erstellten SpatialPolygonsDataFrame mit Yield_SMI_Prec (kommt aus 4km_prec Skript)
- Manipulation of the Tavgepitation data
- Löschen der comId 02000 und 11000 #
- Berechnen der Lags der Monate August bis Oktober
- Demean average Temperature Data with constant (not conditioned on communities)
- Standardize average Temperature (not condition on communities) -> z-score

'

#### Dependencies and Input ####
'
- 4km_tavg.nc (Stephan Thober) 
- vg2500_krs (extern bereitgestellt) ->shape mit polygonen der Kreise
- Yield_SMI_Prec <-  Input aus 4km_prec
'
#### Output ####
##Files
' 
- Tavg_months_1999
- Yield_SMI_Prec_Tavg (File, welche Daten der Regionalstatistic (yield), SMI, Prec, und average temperature enthält)
'
## Plots
' - Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
- Plot des Mai 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
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
#### Laden des Tavg NetCDF ####
###############################
# Tavg<-brick("/home/peichl/Documents/projects/correlation/data/data_raw/4km_pre.nc", varname="pre_lk")
# Tavg
## Funktioniert mit dieser File nicht so gut, daher benutze ich die Alternative

# #### Alternatives Laden: ncdf4 ####
Tavg_open <- nc_open("/home/peichl/Documents/projects/correlation/data/data_raw/4km_tavg.nc")
print(Tavg_open) 
Tavg <- ncvar_get(Tavg_open, varid="tavg_lk")
nc_close(Tavg_open)
#offensichtlich habe ich hier 64 2/12 Jahre oder 770 Monate seit Januar 1950

dim(Tavg) 
770/12 #= 64 2/12  
# Datensatz deckt Januar 1950 - Februar 2014 ab
# Diese Datensatz ist 30 Monate länger als prec, geht aber zur gleichen Zeit los. 
class(Tavg)
levelplot(Tavg)
head(Tavg)
summary(Tavg)



###############################
#### -9999 & 0 Werte in NA ####
###############################

u <- Tavg== -9999
sum(u)
Tavg[u]<-NA


u<-Tavg== 0
Tavg[,1] == 0 #319 (Oberallgäu),315 (Lindau),308 (Kempten),236( Garmisch Partenkirchen)
Tavg[u] <- NA

3080/4


# Interpretation
'
Es ist sehr unwahrscheilich, dass es in einem exact Null Grad hat im Durchschnitt. Und nach Betrachtung der Daten macht es wohl mehr Sinn, die 0 Werte in NAs umzuwandeln.

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

Tavg <- as.data.frame(Tavg)
names(Tavg)

#############################
#### Verändern der Namen ####
# average Temperature geht ein Jahr vor SMI los, also im Januar 1950 und geht 38 Monate länger, also statt bis 1.12.2010 bis 1.2.2014
idx <-seq(from=as.Date('1950-1-1'), to=as.Date('2014-2-1'), 'month')
idx <-as.yearmon(idx)
idx
idx_str<-paste("Tav",idx, sep="")
idx_names<-make.names(c(idx_str), unique = TRUE)
idx_gsub<-gsub(" ","",idx_str)
idx_names<-make.names(c(idx_gsub), unique = TRUE)
idx_names
length(Tavg)==length(idx_names) # der Namensvektor hat die gleiche länge wie der Datensatz breit ist
colnames(Tavg)<-idx_names
names(Tavg)
###############################################
#### Rownames anpassen für merging process ####
dim(Tavg)
rownames(Tavg) <-0:411 

#############################################
## Delete German Expression in Columnnames ##
head(Tavg)
names(Tavg)
# names(KreisSMI_spdf_1999_s)[7:length(names(KreisSMI_spdf_1999_s))]
# names(KreisSMI_spdf_1999_s)[7:length(names(KreisSMI_spdf_1999_s))] <- substring(names(KreisSMI_spdf_1999_s)[7:length(names(KreisSMI_spdf_1999_s))], 3,9)
names(Tavg) <- chartr("ä","a",names(Tavg))
names(Tavg) <- chartr("z","c",names(Tavg))
names(Tavg) <- chartr("Mai","May",names(Tavg))
names(Tavg) <- chartr("Okt","Oct",names(Tavg))
names(Tavg)



##########################
## Überprüfen der Daten ##
head(Tavg)
summary(Tavg) # nach Umwandlung der 0 Werte in NAs macht das nun mehr Sinn
names(Tavg)

######################################
#### Data.frame: 1995 produzieren ####
names(Tavg)

names(Tavg)[((length(Tavg)-191):length(Tavg))-38] # Da diese Dataframe 38 Monate länger als der Prec ist (nach Dec 2010), muss ich darauf reagieren: -38 

Tavg_1995<-Tavg[,((length(Tavg)-191):length(Tavg))-38]

head(Tavg_1995)
names(Tavg_1995) # check
rownames(Tavg_1995)<-0:411

######################################
#### Data.frame: 1999 produzieren ####
names(Tavg)[(length(Tavg)-143):length(Tavg)-38]
Tavg_1999<-Tavg[,(length(Tavg)-143):length(Tavg)-38]
head(Tavg_1999)
names(Tavg_1999)

rownames(Tavg_1999)<-0:411

###########################
#### Maptools::spCbind ####
KreisTavg_spdf<-spCbind(KreisPolOGR,Tavg)
KreisTavg_spdf_1995 <- spCbind(KreisPolOGR,Tavg_1995)
KreisTavg_spdf_1999 <- spCbind(KreisPolOGR,Tavg_1999)

summary(KreisTavg_spdf_1999)


# ###################################################################
# #### Export der erstellten Dateien (SpatialPolygoneDataFrames) ####
# ## ... für den gesamten Zeitraum
# writePolyShape(KreisTavg_spdf, "./data/data_raw/KreisTavg_spdf")
# ## ... ab 1995
# writePolyShape(KreisTavg_spdf_1995, "./data/data_raw/KreisTavg_spdf_1995.shp")
# ## ... ab 1999
# writePolyShape(KreisTavg_spdf_1999, "./data/data_raw/KreisTavg_spdf_1999.shp")
# 
# ###################################################################
# #### Import der erstellten Dateien (SpatialPolygoneDataFrames) ####
# 
# KreisTavg_spdf<-readShapePoly("./data/data_raw/KreisTavg_spdf.shp")
# KreisTavg_spdf_1995<-readShapePoly("./data/data_raw/KreisTavg_spdf_1995.shp")
# KreisTavg_spdf_1999<-readShapePoly("./data/data_raw/KreisTavg_spdf_1999.shp")
# names(KreisTavg_spdf)
# names(KreisTavg_spdf_1995)
# names(KreisTavg_spdf_1999)
# 

#### Projection des ursrünglichen SpatialPolygonsDataFrame übertragen
proj4string(KreisTavg_spdf)<-proj4string(KreisPolOGR)


###################################################################################################################################
############################################# Plotten der Niederschlagsdaten ######################################################
###################################################################################################################################
names(KreisTavg_spdf)

## Absolute Werte
ztdrei<-c( "TavSep2003"  ,"TavOkt2003" , "TavNov2003" , "TavDez2003",  "TavMai2003" , "TavJun2003" ,"TavJul2003" , "TavAug2003","TavJan2003",  "TavFeb2003" , "TavMär2003", "TavApr2003" )
summary(KreisTav_spdf[,ztdrei])
at=(seq(-10 ,18,1)) # Intervall ausgewählt anhand der min av Werte.

## Z-score Werte erzeugen
data <- as.data.frame(KreisTav_spdf[,ztdrei])
class(data)
data_zscore <- as.data.frame(scale(data, center=T, scale=T)) # scale gibt matrix aus, dies funktioniert aber nicht mit spCbind
class(data_zscore)
summary(data_zscore)
Kreisdata_zscore_spdf <- spCbind(KreisPolOGR,data_zscore) 

## Paramete für ssplot ##
ztdrei <- c( "TavSep2003"  ,"TavOkt2003" , "TavNov2003" , "TavDez2003",  "TavMai2003" , "TavJun2003" ,"TavJul2003" , "TavAug2003","TavJan2003",  "TavFeb2003" , "TavMär2003", "TavApr2003" )
summary(KreisTavg_spdf[,ztdrei])
at <- (seq(-10 ,18,1)) # Intervall ausgewählt anhand der min avg Werte.
summary(Kreisdata_zscore_spdf)
at2 <- seq(-3.8, 3.8, 0.01)

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
colors = colorRampPalette(c("skyblue3", "white", "orangered3"))(1e3)


## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
KreisTavg_spdf_Plt1 <- spplot(KreisTavg_spdf, at=at, ztdrei, col.regions= colors) # absolute Daten
## exported als png mit width 700
KreisTavg_spdf_Plt2 <- spplot(KreisTavg_spdf, at=at, "TavgSep2003", col.regions= colors) # absolute Daten

## Plot des Jahres 2003 der extrahierten Tavg Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
Kreisdata_zscore_spdf_Plt1 <- spplot(Kreisdata_zscore_spdf, at=at2, ztdrei, col.regions= colors) # z -score
## exported als png mit width 700
Kreisdata_zscore_spdf_Plt2 <- spplot(Kreisdata_zscore_spdf, "TavgSep2003", col.regions= colors) # z -score

## Interpretation ##
'
Interpretation der Niederschlagswerte ist schwierig, da man nicht weiß, was das langjährige Mittel ist. Für die Interpretation 
die Grafik wären wohl Anomalien (betrachtung der Standardabweichung) besser. 
Denoch kann man hier Entwicklungen erkennen.
'



#############################################################################################################################################################################################
############################# Mergen des erstellten SpatialPolygonsDataFrame mit Yield_SMI_Prec ##########################################################################################
#############################################################################################################################################################################################


'Mergen soll mit Hilfe der ComId und des Jahres passieren'

############################################
##  Erstellen der ComId für den DatenSatz ##
head(KreisTavg_spdf_1995)
KreisTavg_spdf_1995$RS
KreisTavg_spdf_1995$comId<-as.factor(str_sub(KreisTavg_spdf_1995$RS,1,5))
unique(KreisTavg_spdf_1995$comId)

##################################
#### Reshapen des Datensatzes ####
##################################

class(KreisTavg_spdf_1995)
KreisTavg_1995 <- as.data.frame(KreisTavg_spdf_1995)
dim(KreisTavg_1995) # die Dimension ist die selbe wie in MergeSMI_Yield, daher werde ich einfach den gleichen reshape Algorithmus verwenden

## Reorder Names ##
names(KreisTavg_1995)
length(names(KreisTavg_1995))
KreisTavg_1995 <-KreisTavg_1995[c(1:5,198,(6:197))]
head(KreisTavg_1995)

#### Make a list of vectors of months ####
names(KreisTavg_1995)
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
list0<-list(c(7,187), c(8,188), c(9,189), c(10,190), c(11,191), c(12,192),c(13,193),c(14,194),c(15,195), c(16,196), c(17,197), c(18,198))
# those list define the starting and ending point for the steps, 12 each
list0

#### Loop zum erstellen der Liste listMonthYearNames mit den IDs, welche jeweils den gleichen Monatesnamen, über alle Jahre hinweg, haben ####
listMonthYearNames<-list() # set container

for (i in 01:12){
  listMonthYearNames[i]<-list(c(names(KreisTavg_1995)[c(seq(list0[[i]][1],list0[[i]][2],12))]))
}
listMonthYearNames # list including a list of the listMonthYearNames across the years. This is the basis for the new variables, i.e the Tavg in each month
listMonthYearNames[[3]] # lIst of all marches across the 16 years considered here

#### Make the names for the new variables
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
listMonthNames<-list(c("Tavg_Jan"), c( "Tavg_Feb"), c("Tavg_Mar"), c("Tavg_Apr"), c("Tavg_Mai"), c("Tavg_Jun"),c("Tavg_Jul"),c("Tavg_Aug"),c("Tavg_Sep"), c("Tavg_Oct"), c("Tavg_Nov"), c("Tavg_Dec"))
listMonthNames

#### Loop, welche mir die Tavg pro Monat jeweils in einer Column ausgibt ####
y<-data.frame(1:6592) # set container
x<-data.frame() # set container


### loop over the twelve listMonthYearNames, split up, an then filled up with the values across the years
for(i in 1:12) 
{
  x <- reshape (KreisTavg_1995, 
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


## Explore the newly build dataframe with the Tavgipitiation for each Monnth ##
names(y)
rownames(y)
## Get rid of non necessary variables
y$X1.6592<-NULL
head(y)

####################################################
#### Erstellen eines data.frames mit ID angaben ####
'bestehend aus einem komplett reshapden Datensatz (x), und dem Datensatz welcher der neue gebildete Variable der Loops aus dem Reshape Prozess
vereint'
names(x)
names(y)

head(x[,c(5:6, 183)])
Tavg_months<-cbind(x[,c(5:6, 183)],y)

names(Tavg_months)
head(Tavg_months)
rownames(Tavg_months)<-NULL

## Explore whether comIds and years match ##
head(Tavg_months, 15) # check
tail(Tavg_months, 15) # check
# In den rownames steht der Monat Dezember, da dies der letzte Monat ist, der gemelted, also in eine Spalte gefasst wurde. 
# Sieht aber sonst recht ordentlich aus.

## Befreien der year Variable von den Monatsangaben, hier Dezember ##
str_sub(Tavg_months$Year,7,10 )
Tavg_months$Year<-as.integer(str_sub(Tavg_months$Year,7,10 ))
head(Tavg_months)
unique(Tavg_months$Year)

## Anpassen der Namen fürs mergen
colnames(Tavg_months)
colnames(Tavg_months)[3] <- "year"

###################################################################################
#### Abgleichen der neu erstellten Daten mit KreisTavg_1995 (orginal Daten) ####
head(KreisTavg_1995)

## Erstellen und überprüfen der beiden Vektoren, die jeweils den Tavg des Jahres 1999 im Monat Januar wiedergeben ##
## Vector in Tavg_months des Tavg Januar 1999
Tavg_months$Tavg_Jan[Tavg_months$year==1999]
length(Tavg_months$Tavg_Jan[Tavg_months$year==1999])

## Vector in KreisTavg_1995 des Tavg Januar 1999
names(KreisTavg_1995)
KreisTavg_1995$TavJan1999
length(KreisTavg_1995$TavJan1999)

all(Tavg_months$Tavg_Jan[Tavg_months$year==1999]%in%KreisTavg_1995$TavJan1999)
# Daten sehen gut aus

#############################################################################################################################################################################################
################################################################## Manipulation der Temperature Daten #####################################################################################
#############################################################################################################################################################################################

#######################################
## Löschen der comId 02000 und 11000 ##
#######################################
' Both cities are neglected '
unique(Tavg_months$comId)
names(Tavg_months)
Tavg_months<-Tavg_months[!Tavg_months$comId=="02000",]  #Hamburg
Tavg_months<-Tavg_months[!Tavg_months$comId=="11000",]  #Berlin

######################################################################################
## Berechnen der lags der Avg. Temperature Variablen für die Monate August bis December ##
######################################################################################

head(Tavg_months)

## Ordnen der Spalten so, dass die Variablen erst nach comID und dann nach time geordnet werden.
Tavg_months<-Tavg_months[order(Tavg_months$comId, Tavg_months$year),]

Tavg_months_arr<-slide(Tavg_months, Var ="Tavg_Aug", GroupVar= "comId", slideBy = -1)
Tavg_months_arr<-slide(Tavg_months_arr, Var ="Tavg_Sep", GroupVar= "comId", slideBy = -1)
Tavg_months_arr<-slide(Tavg_months_arr, Var ="Tavg_Oct", GroupVar= "comId", slideBy = -1)
Tavg_months_arr<-slide(Tavg_months_arr, Var ="Tavg_Nov", GroupVar= "comId", slideBy = -1)
Tavg_months_arr<-slide(Tavg_months_arr, Var ="Tavg_Dec", GroupVar= "comId", slideBy = -1)

head(Tavg_months_arr)

## Change names
names(Tavg_months_arr)
names(Tavg_months_arr)[names(Tavg_months_arr)==c("Tavg_Aug-1" )] <-c("Tavg_Aug_lag")
names(Tavg_months_arr)[names(Tavg_months_arr)==c("Tavg_Sep-1" )] <-c("Tavg_Sep_lag")
names(Tavg_months_arr)[names(Tavg_months_arr)==c("Tavg_Oct-1" )] <-c("Tavg_Oct_lag")
names(Tavg_months_arr)[names(Tavg_months_arr)==c("Tavg_Nov-1" )] <-c("Tavg_Nov_lag")
names(Tavg_months_arr)[names(Tavg_months_arr)==c("Tavg_Dec-1" )] <-c("Tavg_Dec_lag")

###################################
####  Data Frame Starting 1999 ####
names(Tavg_months_arr)
unique(Tavg_months_arr$year)
Tavg_months_1999<-Tavg_months_arr[Tavg_months_arr$year >= 1999,] 
unique(Tavg_months_1999$year)


#############################################
## Demean average Temperature Data with constant ##

## Operations with no conditioning of communities
## April

head(Tavg_months_1999)

## Löschen der Daten mit NULL ##
'Es ist sehr unwahrscheinlich, dass es einem Monat in Deutschland nicht geregnet hat.'
any(Tavg_months_1999$Tavg_Apr==0) # habe ich vorher schon gemacht 
Tavg_months_1999$Tavg_Apr[Tavg_months_1999$Tavg_Apr==0] <- NA

###############
## demeaning ##
names(Tavg_months_1999)
length(names(Tavg_months_1999))

x<-NULL
demean<-NULL
for (i in 4:20)
{
  x <- Tavg_months_1999[,i] - mean(Tavg_months_1999[,i], na.rm = T)
  demean <- cbind(demean,x)
}
head(demean)
dim(demean)

colnames(demean) <- c("Tavg_Jan_demean",  "Tavg_Feb_demean" , "Tavg_Mar_demean" , "Tavg_Apr_demean" ,"Tavg_Mai_demean" , "Tavg_Jun_demean" ,"Tavg_Jul_demean" , "Tavg_Aug_demean" , "Tavg_Sep_demean" ,
                 "Tavg_Oct_demean", "Tavg_Nov_demean","Tavg_Dec_demean","Tavg_Aug_lag_demean","Tavg_Sep_lag_demean","Tavg_Oct_lag_demean","Tavg_Nov_lag_demean" ,"Tavg_Dec_lag_demean" )


Tavg_months_1999_demean<-cbind(Tavg_months_1999, demean)
names(Tavg_months_1999_demean)

## Compare to seperately produced vector for April ##
Tavg_months_1999_demean<- transform(Tavg_months_1999_demean , demeanTavg_Apr = detrend(Tavg_Apr, "constant"))
names(Tavg_months_1999_demean)
all(Tavg_months_1999_demean$demeanTavg_Apr%in%Tavg_months_1999_demean$Tavg_Apr_demean)
Tavg_months_1999_demean$demeanTavg_Apr <- NULL
# Interpretation
'
Für den Monat April scheinen die Ergebnisse zu stimmen.
'


###################
## standardizing ##

names(Tavg_months_1999)
length(names(Tavg_months_1999))

x<-NULL
zscore<-NULL
for (i in 4:20)
{
  x <- (Tavg_months_1999[,i] - mean(Tavg_months_1999[,i], na.rm = T))/sd(Tavg_months_1999[,i], na.rm=T)
  zscore <- cbind(zscore,x)
}
head(zscore)
dim(zscore)

colnames(zscore) <- c("Tavg_Jan_zscore",  "Tavg_Feb_zscore" , "Tavg_Mar_zscore" , "Tavg_Apr_zscore" ,"Tavg_Mai_zscore" , "Tavg_Jun_zscore" ,"Tavg_Jul_zscore" , "Tavg_Aug_zscore" , "Tavg_Sep_zscore" ,
                 "Tavg_Oct_zscore", "Tavg_Nov_zscore","Tavg_Dec_zscore","Tavg_Aug_lag_zscore","Tavg_Sep_lag_zscore","Tavg_Oct_lag_zscore","Tavg_Nov_lag_zscore" ,"Tavg_Dec_lag_zscore" )


Tavg_months_1999_demean_zscore<-cbind(Tavg_months_1999_demean, zscore)
names(Tavg_months_1999_demean_zscore)


#####################################################
## Compare to seperately produced vector for April ##
Tavg_months_1999_demean_zscore <- transform(Tavg_months_1999_demean_zscore , zScore_Pre_Apr<- (Tavg_Apr - mean(Tavg_Apr, na.rm=T))/sd(Tavg_Apr, na.rm=T))
all(Tavg_months_1999_demean_zscore$zScore_Pre_Apr%in%Tavg_months_1999_demean_zscore$Tavg_Apr_zscore)
## Löschen von "demeanTavg_Apr"
Tavg_months_1999_demean_zscore$demeanTavg_Apr <- NULL


# Interpretation
'
Für den Monat April scheinen die Ergebnisse zu stimmen.
'


#   ################  
# # ## detrending ##
# # Tavg_months_1999 <- transform(Tavg_months_1999 , detrendTavg_Apr = detrend(demeanTavg_Apr, "linear"))
# # Tavg_months_1999$detrendTavg_Apr
# 
# 
# ## z-score and detrending ##
# # z-score #
# Tavg_months_1999 <- transform(Tavg_months_1999 , zScore_Tavg_Apr = demeanTavg_Apr/sd(Tavg_Apr, na.rm=T))
# Tavg_months_1999$zScore_Tavg_Apr
# # # detrend z-score #
# # Tavg_months_1999 <- transform(Tavg_months_1999 , detrendzScore_Tavg_Apr = detrend(zScore_Tavg_Apr, "linear"))
# 
# 
# ## Plots ##
# par(mfrow=c(2,2))
# 
# # Plot of absolute values #
# plot(Tavg_months_1999$Tavg_Apr~Tavg_months_1999$year)
# abline(mean(Tavg_months_1999$Tavg_Apr, na.rm=T),0)
# 
# # Plot of demeaned absolute Values #
# plot(Tavg_months_1999$demeanTavg_Apr~Tavg_months_1999$year)
# fit1 <- lm(Tavg_months_1999$demeanTavg_Apr~Tavg_months_1999$year)
# abline(fit1)
# abline(0,0,col=2)
# 
# any(Tavg_months_1999$Tavg_Apr==0)
# summary(Tavg_months_1999$Tavg_Apr)
# 
# # # Plot of detrended absolute Values #
# # plot(Tavg_months_1999$detrendTavg_Apr~Tavg_months_1999$year)
# # fit2 <- lm(Tavg_months_1999$detrendTavg_Apr~Tavg_months_1999$year)
# # abline(fit2)
# # abline(0,0,col=2)
# 
# # Plot of z-score #
# plot(Tavg_months_1999$zScore_Tavg_Apr~Tavg_months_1999$year)
# fit3 <- lm(Tavg_months_1999$zScore_Tavg_Apr~Tavg_months_1999$year)
# abline(fit3)
# abline(0,0,col=2)
# 
# # # Plot of detrended z-score #
# # plot(Tavg_months_1999$detrendzScore_Tavg_Apr~Tavg_months_1999$year)
# # fit4 <- lm(Tavg_months_1999$detrendzScore_Tavg_Apr~Tavg_months_1999$year)
# # abline(fit4)
# # abline(0,0,col=2)

## Interpretation
'
Wie schon bei yield hat das detrenden, welches nicht explicit nach den seperaten räumlichen Einheiten bedingt wird, keinen sichtbare Wirkung im Plot. Die Zahlen haben sich jedoch verändert.
Frage, welche Daten ich daher übernehmen. Also die demeaned data machen Sinn (nach Luis) und die Standardisierten. Detrended Daten nehme ich wieder raus.
'

################################
#### Write Tavg_months_1999 ####

head(Tavg_months_1999_demean_zscore)
names(Tavg_months_1999_demean_zscore)

write.csv(Tavg_months_1999_demean_zscore, "data//data_processed/Tavg_months_1999")


#########################################################################
#### Mergen von Tavg_months_1999 mit Yield_SMI_Prec anhand von RS ####
Yield_SMI_Prec<- read.csv("data//data_processed/Yield_SMI_Prec")
names(Yield_SMI_Prec)
head(Yield_SMI_Prec)
dim(Yield_SMI_Prec)
Yield_SMI_Prec$X <-NULL

## Average Temperature Data ##
# Rename Tavg_months_1999_demean_zscore
Tavg_months_1999<- read.csv("data/data_processed/Tavg_months_1999")

names(Tavg_months_1999)
head(Tavg_months_1999)
dim(Tavg_months_1999)
Tavg_months_1999$X <- NULL

## Vergleich den beiden year Vectoren ##
all(Yield_SMI_Prec$year==Tavg_months_1999$year)

## Vergleichen der beiden comId Vektoren ##
all(Yield_SMI_Prec$comId==Tavg_months_1999$comId)


# Interpretation
' 
Die Reihenfolge der beiden Datensätze ist die gleiche
'

## Löschen der reduntanten Vektoren ##
names(Yield_SMI_Prec[colnames(Yield_SMI_Prec)%in%names(Tavg_months_1999)])
Tavg_months_1999$SHAPE_AREA <- Tavg_months_1999$comId <- Tavg_months_1999$year <- NULL

## Cbind der beiden Datensätze ##
Yield_SMI_Prec_Tavg<-cbind(Yield_SMI_Prec,Tavg_months_1999)
names(Yield_SMI_Prec_Tavg)
rownames(Yield_SMI_Prec_Tavg)<-NULL
Yield_SMI_Prec_Tavg$X.1<-NULL
Yield_SMI_Prec_Tavg$X<-NULL

dim(Yield_SMI_Prec_Tavg)
head(Yield_SMI_Prec_Tavg,15)
tail(Yield_SMI_Prec_Tavg,16)

table(Yield_SMI_Prec_Tavg$year) # statt 525 nur noch 410 Beobachtungen

##############################
#### Write Yield_SMI_Prec_Tavg ####
##############################


write.csv(Yield_SMI_Prec_Tavg, "data//data_processed/Yield_SMI_Prec_Tavg")

