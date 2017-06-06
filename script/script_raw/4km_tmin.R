#############################################################################################################################################################################################
#############################################################################################################################################################################################

#### File Description ####
' - Verarbeiten der NetCDF Datei mit den minimum Temperature  Werten, so dass als time series für die einzelnen Landkreise.
(- minimum Temperature  (4km Auflösung) maskiert für non- irrigated land, dazu disaggregiert auf 100 * 100 m, und danach extrahiert auf Polygon durch Stephan Thober.)
- Einlesen der NetCDF
- Verbinden mit KreisPolOGR (vg2500_krs) für räumlich Zuordnung (vg2500_krs shape wurde auch für die Berechnung der 4km_Tmin.nc genutzt. Daher ist es die gleiche Zuordnung(SMI Were
beruhend auf dieses Vorgehen zeigen die gleichen Strukturen))
- Plotten der erstellten Daten mit hilfe von spplot{sp}
- Mergen des erstellten SpatialPolygonsDataFrame mit Yield_SMI_Prec_Tavg_Pet_Dem_Por (kommt aus 4km_prec Skript)
- Manipulation of the Tminepitation data
- Löschen der comId 02000 und 11000 #
- Berechnen der Lags der Monate August bis Oktober
- Demean minimum Temperature  Data with constant (not conditioned on communities) <- hier nehme ich den mean über alle comIds und Jahre für jeden Monat
- Standardize minimum Temperature  (not condition on communities) -> z-score <- hier nehme ich den z-score, welcher oben genannten mean und die sd über alle comIds und Jahre benutzt.
- Frage, wie sich dieser mean und diese sd auf das Ergebnis auswirken. Aber erstmalszweitrangig.

'

#### Dependencies and Input ####
'
- 4km_Tmin.nc (Stephan Thober) 
- vg2500_krs (extern bereitgestellt) ->shape mit polygonen der Kreise
- Yield_SMI_Prec_Tavg_Pet_Dem_Por <-  Input aus 4km_por
'
#### Output ####
##Files
' 
- Tmin_months_1999
- Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin (File, welche Daten der Regionalstatistic (yield), SMI, Prec, und minimum Temperature  enthält)
'
## Plots
'
- Plot des Jahres 2003 der extrahierten Tmin Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
- Plot des Mai 2003 der extrahierten Tmin Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
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
#### Laden des Tmin NetCDF ####
###############################
# Tmin<-brick("/home/peichl/Documents/projects/correlation/data/data_raw/4km_pre.nc", varname="pre_lk")
# Tmin
## Funktioniert mit dieser File nicht so gut, daher benutze ich die Alternative

# #### Alternatives Laden: ncdf4 ####
Tmin_open <- nc_open("/home/peichl/Documents/projects/correlation/data/data_raw/4km_tmin.nc")
print(Tmin_open) 
Tmin <- ncvar_get(Tmin_open, varid = "tmin_lk")
nc_close(Tmin_open)
#offensichtlich habe ich hier 64 2/12 Jahre oder 770 Monate seit Januar 1950

dim(Tmin) 
770/12 #= 64 2/12  
# Datensatz deckt Januar 1950 - Februar 2014 ab
# Diese Datensatz ist 30 Monate länger als prec, geht aber zur gleichen Zeit los. 
class(Tmin)
levelplot(Tmin)
head(Tmin)
summary(Tmin)



###############################
#### -9999 & 0 Werte in NA ####
###############################
u<-Tmin== -9999
Tmin[u]<-NA


u<-Tmin== 0
Tmin[u]<-NA

# Interpretation
'
Es ist sehr unwahrscheilich, dass es in einem exact Null Grad hat im Durchschnitt. Und nach Betrachtung der Daten macht es wohl mehr Sinn, die 0 Werte in NAs umzuwandeln .
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

Tmin <- as.data.frame(Tmin)
names(Tmin)

#############################
#### Verändern der Namen ####
# minimum Temperature  geht ein Jahr vor SMI los, also im Januar 1950 und geht 38 Monate länger, also statt bis 1.12.2010 bis 1.2.2014
idx <-seq(from=as.Date('1950-1-1'), to=as.Date('2014-2-1'), 'month')
idx <-as.yearmon(idx)
idx
idx_str<-paste("Tmin",idx, sep="")
idx_names<-make.names(c(idx_str), unique = TRUE)
idx_gsub<-gsub(" ","",idx_str)
idx_names<-make.names(c(idx_gsub), unique = TRUE)
idx_names
length(Tmin)==length(idx_names) # der Namensvektor hat die gleiche länge wie der Datensatz breit ist
colnames(Tmin)<-idx_names
names(Tmin)
###############################################
#### Rownames anpassen für merging process ####
dim(Tmin)
rownames(Tmin) <-0:411 

##########################
## Überprüfen der Daten ##
head(Tmin)
summary(Tmin) # nach Umwandlung der 0 Werte in NAs macht das nun mehr Sinn
names(Tmin)

######################################
#### Data.frame: 1995 produzieren ####
names(Tmin)

names(Tmin)[((length(Tmin)-191):length(Tmin))-38] # Da diese Dataframe 38 Monate länger als der Prec ist (nach Dec 2010), muss ich darauf reagieren: -38 

Tmin_1995<-Tmin[,((length(Tmin)-191):length(Tmin))-38]

head(Tmin_1995)
names(Tmin_1995) # check
rownames(Tmin_1995)<-0:411

######################################
#### Data.frame: 1999 produzieren ####
names(Tmin)[(length(Tmin)-143):length(Tmin)-38]
Tmin_1999<-Tmin[,(length(Tmin)-143):length(Tmin)-38]
head(Tmin_1999)
names(Tmin_1999)

rownames(Tmin_1999) <- 0:411

###########################
#### Maptools::spCbind ####
KreisTmin_spdf<-spCbind(KreisPolOGR,Tmin)
KreisTmin_spdf_1995 <- spCbind(KreisPolOGR,Tmin_1995)
KreisTmin_spdf_1999 <- spCbind(KreisPolOGR,Tmin_1999)

summary(KreisTmin_spdf_1999)

#### Projection des ursrünglichen SpatialPolygonsDataFrame übertragen
proj4string(KreisTmin_spdf)<-proj4string(KreisPolOGR)


###################################################################################################################################
############################################# Plotten der Niederschlagsdaten ######################################################
###################################################################################################################################
names(KreisTmin_spdf)

## Absolute Werte
ztdrei<-c( "TminSep2003"  ,"TminOkt2003" , "TminNov2003" , "TminDez2003",  "TminMai2003" , "TminJun2003" ,"TminJul2003" , "TminAug2003","TminJan2003",  "TminFeb2003" , "TminMär2003", "TminApr2003" )
summary(KreisTmin_spdf[,ztdrei])
at=(seq(-10 ,18,1)) # Intervall ausgewählt anhand der min max Werte.

## Z-score Werte erzeugen
data <- as.data.frame(KreisTmin_spdf[,ztdrei])
class(data)
data_zscore <- as.data.frame(scale(data, center=T, scale=T)) # scale gibt matrix aus, dies funktioniert aber nicht mit spCbind
class(data_zscore)
summary(data_zscore)
Kreisdata_zscore_spdf <- spCbind(KreisPolOGR,data_zscore) 

## Paramete für ssplot ##
ztdrei <- c( "TminSep2003"  ,"TminOkt2003" , "TminNov2003" , "TminDez2003",  "TminMai2003" , "TminJun2003" ,"TminJul2003" , "TminAug2003","TminJan2003",  "TminFeb2003" , "TminMär2003", "TminApr2003" )
summary(KreisTmin_spdf[,ztdrei])
at <- (seq(-10 ,18,1)) # Intervall ausgewählt anhand der min max Werte.
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
colors = colorRampPalette(c("skyblue3", "cornsilk", "orangered3"))(1e3)


## Plot des Jahres 2003 der extrahierten Tmin Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
KreisTmin_spdf_Plt1 <- spplot(KreisTmin_spdf, at=at, ztdrei, col.regions= colors) # absolute Daten
## exported als png mit width 700
KreisTmin_spdf_Plt2 <- spplot(KreisTmin_spdf, at=at, "TminSep2003", col.regions= colors) # absolute Daten

## Plot des Jahres 2003 der extrahierten Tmin Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
Kreisdata_zscore_spdf_Plt1 <- spplot(Kreisdata_zscore_spdf, at=at2, ztdrei, col.regions= colors) # z -score
## exported als png mit width 700
Kreisdata_zscore_spdf_Plt2 <- spplot(Kreisdata_zscore_spdf, at=at2, "TminSep2003", col.regions= colors) # z -score

## Interpretation ##
'
Interpretation der Niederschlagswerte ist schwierig, da man nicht weiß, was das langjährige Mittel ist. Für die Interpretation 
die Grafik wären wohl Anomalien (betrachtung der Standardabweichung) besser. 
Denoch kann man hier Entwicklungen erkennen.
'


##########################################################################################################################################################################################
############################# Mergen des erstellten SpatialPolygonsDataFrame mit Yield_SMI_Prec_Tavg_Pet_Dem_Por ##########################################################################################
##########################################################################################################################################################################################


'Mergen soll mit Hilfe der ComId und des Jahres passieren'

############################################
##  Erstellen der ComId für den DatenSatz ##
head(KreisTmin_spdf_1995)
KreisTmin_spdf_1995$RS
KreisTmin_spdf_1995$comId<-as.factor(str_sub(KreisTmin_spdf_1995$RS,1,5))
unique(KreisTmin_spdf_1995$comId)

##################################
#### Reshapen des Datensatzes ####
##################################

class(KreisTmin_spdf_1995)
KreisTmin_1995 <- as.data.frame(KreisTmin_spdf_1995)
dim(KreisTmin_1995) # die Dimension ist die selbe wie in MergeSMI_Yield, daher werde ich einfach den gleichen reshape Algorithmus verwenden

## Reorder Names ##
names(KreisTmin_1995)
length(names(KreisTmin_1995))
KreisTmin_1995 <-KreisTmin_1995[c(1:5,198,(6:197))]
head(KreisTmin_1995)

#### Make a list of vectors of months ####
names(KreisTmin_1995)
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
list0<-list(c(7,187), c(8,188), c(9,189), c(10,190), c(11,191), c(12,192),c(13,193),c(14,194),c(15,195), c(16,196), c(17,197), c(18,198))
# those list define the starting and ending point for the steps, 12 each
list0

#### Loop zum erstellen der Liste listMonthYearNames mit den IDs, welche jeweils den gleichen Monatesnamen, über alle Jahre hinweg, haben ####
listMonthYearNames<-list() # set container

for (i in 01:12){
  listMonthYearNames[i]<-list(c(names(KreisTmin_1995)[c(seq(list0[[i]][1],list0[[i]][2],12))]))
}
listMonthYearNames # list including a list of the listMonthYearNames across the years. This is the basis for the new variables, i.e the Tmin in each month
listMonthYearNames[[3]] # lIst of all marches across the 16 years considered here

#### Make the names for the new variables
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
listMonthNames<-list(c("Tmin_Jan"), c( "Tmin_Feb"), c("Tmin_Mar"), c("Tmin_Apr"), c("Tmin_Mai"), c("Tmin_Jun"),c("Tmin_Jul"),c("Tmin_Aug"),c("Tmin_Sep"), c("Tmin_Oct"), c("Tmin_Nov"), c("Tmin_Dec"))
listMonthNames

#### Loop, welche mir die Tmin pro Monat jeweils in einer Column ausgibt ####
y<-data.frame(1:6592) # set container
x<-data.frame() # set container


### loop over the twelve listMonthYearNames, split up, an then filled up with the values across the years
for(i in 1:12) 
{
  x <- reshape (KreisTmin_1995, 
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


## Explore the newly build dataframe with the Tminipitiation for each Monnth ##
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
Tmin_months <- cbind(x[,c(5:6, 183)],y)

names(Tmin_months)
head(Tmin_months)
rownames(Tmin_months)<-NULL

## Explore whether comIds and years match ##
head(Tmin_months, 15) # check
tail(Tmin_months, 15) # check
# In den rownames steht der Monat Dezember, da dies der letzte Monat ist, der gemelted, also in eine Spalte gefasst wurde. 
# Sieht aber sonst recht ordentlich aus.

## Befreien der year Variable von den Monatsangaben, hier Dezember ##
str_sub(Tmin_months$Year,8,11 )
Tmin_months$Year <- as.integer(str_sub(Tmin_months$Year,8,11 ))
head(Tmin_months)
unique(Tmin_months$Year)

## Anpassen der Namen fürs mergen
colnames(Tmin_months)
colnames(Tmin_months)[3] <- "year"

###################################################################################
#### Abgleichen der neu erstellten Daten mit KreisTmin_1995 (orginal Daten) ####
head(KreisTmin_1995)

## Erstellen und überprüfen der beiden Vektoren, die jeweils den Tmin des Jahres 1999 im Monat Januar wiedergeben ##
## Vector in Tmin_months des Tmin Januar 1999
Tmin_months$Tmin_Jan[Tmin_months$year == 1999]
length(Tmin_months$Tmin_Jan[Tmin_months$year == 1999])

## Vector in KreisTmin_1995 des Tmin Januar 1999
names(KreisTmin_1995)
KreisTmin_1995$TminJan1999
length(KreisTmin_1995$TminJan1999)

all(Tmin_months$Tmin_Jan[Tmin_months$year == 1999] == KreisTmin_1995$TminJan1999, na.rm = T)
# Daten sehen gut aus

#############################################################################################################################################################################################
################################################################## Manipulation der Temperature Daten #####################################################################################
#############################################################################################################################################################################################

#######################################
## Löschen der comId 02000 und 11000 ##
#######################################
' Both cities are neglected '
unique(Tmin_months$comId)
names(Tmin_months)
Tmin_months<-Tmin_months[!Tmin_months$comId == "02000",]  #Hamburg
Tmin_months<-Tmin_months[!Tmin_months$comId == "11000",]  #Berlin

######################################################################################
## Berechnen der lags der Avg. Temperature Variablen für die Monate August bis December ##
######################################################################################

head(Tmin_months)

## Ordnen der Spalten so, dass die Variablen erst nach comID und dann nach time geordnet werden.
Tmin_months<-Tmin_months[order(Tmin_months$comId, Tmin_months$year),]

Tmin_months_arr <- slide(Tmin_months, Var ="Tmin_Aug", GroupVar= "comId", slideBy = -1)
Tmin_months_arr <- slide(Tmin_months_arr, Var = "Tmin_Sep", GroupVar = "comId", slideBy = -1)
Tmin_months_arr <- slide(Tmin_months_arr, Var = "Tmin_Oct", GroupVar = "comId", slideBy = -1)
Tmin_months_arr <- slide(Tmin_months_arr, Var = "Tmin_Nov", GroupVar = "comId", slideBy = -1)
Tmin_months_arr <- slide(Tmin_months_arr, Var = "Tmin_Dec", GroupVar = "comId", slideBy = -1)

head(Tmin_months_arr)

## Change names
names(Tmin_months_arr)
names(Tmin_months_arr)[names(Tmin_months_arr) == c("Tmin_Aug-1" )] <- c("Tmin_Aug_lag")
names(Tmin_months_arr)[names(Tmin_months_arr) == c("Tmin_Sep-1" )] <- c("Tmin_Sep_lag")
names(Tmin_months_arr)[names(Tmin_months_arr) == c("Tmin_Oct-1" )] <- c("Tmin_Oct_lag")
names(Tmin_months_arr)[names(Tmin_months_arr) == c("Tmin_Nov-1" )] <- c("Tmin_Nov_lag")
names(Tmin_months_arr)[names(Tmin_months_arr) == c("Tmin_Dec-1" )] <- c("Tmin_Dec_lag")

###################################
####  Data Frame Starting 1999 ####
names(Tmin_months_arr)
unique(Tmin_months_arr$year)
Tmin_months_1999 <- Tmin_months_arr[Tmin_months_arr$year >= 1999,] 
unique(Tmin_months_1999$year)

#############################################
## Demean minimum Temperature  Data with constant ##

## Operations with no conditioning of communities
## April

head(Tmin_months_1999)

# ## Löschen der Daten mit NULL ##
# 'Es ist sehr unwahrscheinlich, dass es einem Monat in Deutschland nicht geregnet hat.'
# any(Tmin_months_1999$Tmin_Apr==0) # habe ich vorher schon gemacht 
# Tmin_months_1999$Tmin_Apr[Tmin_months_1999$Tmin_Apr==0] <- NA

###############
## demeaning ##
names(Tmin_months_1999)
length(names(Tmin_months_1999))

x <- NULL
demean <- NULL


for (i in 4:20)
{
  x <- Tmin_months_1999[,i] - mean(Tmin_months_1999[,i], na.rm = T)
  demean <- cbind(demean,x)
  
}
head(demean)
dim(demean)

colnames(demean) <- c("Tmin_Jan_demean",  "Tmin_Feb_demean" , "Tmin_Mar_demean" , "Tmin_Apr_demean" ,"Tmin_Mai_demean" , "Tmin_Jun_demean" ,"Tmin_Jul_demean" , "Tmin_Aug_demean" , "Tmin_Sep_demean" ,
                      "Tmin_Oct_demean", "Tmin_Nov_demean","Tmin_Dec_demean","Tmin_Aug_lag_demean","Tmin_Sep_lag_demean","Tmin_Oct_lag_demean","Tmin_Nov_lag_demean" ,"Tmin_Dec_lag_demean" )


Tmin_months_1999_demean<-cbind(Tmin_months_1999, demean)
names(Tmin_months_1999_demean)


## Compare to seperately produced vector for April ##
Tmin_months_1999_demean <- transform(Tmin_months_1999_demean , demeanTmin_Apr = detrend(Tmin_Apr, "constant"))
names(Tmin_months_1999_demean)
all(Tmin_months_1999_demean$demeanTmin_Apr%in%Tmin_months_1999_demean$Tmin_Apr_demean)
Tmin_months_1999_demean$demeanTmin_Apr <- NULL
# Interpretation
'
Für den Monat April scheinen die Ergebnisse zu stimmen.
'


###################
## standardizing ##

names(Tmin_months_1999)
length(names(Tmin_months_1999))

x<-NULL
zscore<-NULL
for (i in 4:20)
{
  x <- (Tmin_months_1999[,i] - mean(Tmin_months_1999[,i], na.rm = T))/sd(Tmin_months_1999[,i], na.rm=T)
  zscore <- cbind(zscore,x)
}
head(zscore)
dim(zscore)

colnames(zscore) <- c("Tmin_Jan_zscore",  "Tmin_Feb_zscore" , "Tmin_Mar_zscore" , "Tmin_Apr_zscore" ,"Tmin_Mai_zscore" , "Tmin_Jun_zscore" ,"Tmin_Jul_zscore" , "Tmin_Aug_zscore" , "Tmin_Sep_zscore" ,
                      "Tmin_Oct_zscore", "Tmin_Nov_zscore","Tmin_Dec_zscore","Tmin_Aug_lag_zscore","Tmin_Sep_lag_zscore","Tmin_Oct_lag_zscore","Tmin_Nov_lag_zscore" ,"Tmin_Dec_lag_zscore" )


Tmin_months_1999_demean_zscore <- cbind(Tmin_months_1999_demean, zscore)
names(Tmin_months_1999_demean_zscore)


#####################################################
## Compare to seperately produced vector for April ##
Tmin_months_1999_demean_zscore <- transform(Tmin_months_1999_demean_zscore , zScore_Pre_Apr<- (Tmin_Apr - mean(Tmin_Apr, na.rm=T))/sd(Tmin_Apr, na.rm=T))
all(Tmin_months_1999_demean_zscore$zScore_Pre_Apr%in%Tmin_months_1999_demean_zscore$Tmin_Apr_zscore)
## Löschen von "demeanTmin_Apr"
Tmin_months_1999_demean_zscore$demeanTmin_Apr <- NULL


# Interpretation
'
Für den Monat April scheinen die Ergebnisse zu stimmen.
'


#   ################  
# # ## detrending ##
## Interpretation
'
Wie schon bei yield hat das detrenden, welches nicht explicit nach den seperaten räumlichen Einheiten bedingt wird, keinen sichtbare Wirkung im Plot. Die Zahlen haben sich jedoch verändert.
Frage, welche Daten ich daher übernehmen. Also die demeaned data machen Sinn (nach Luis) und die Standardisierten. Detrended Daten nehme ich wieder raus.
'

################################
#### Write Tmin_months_1999 ####
head(Tmin_months_1999_demean_zscore)
names(Tmin_months_1999_demean_zscore)

write.csv(Tmin_months_1999_demean_zscore, "data//data_processed/Tmin_months_1999")


########################################################################################
#### Mergen von Tmin_months_1999 mit Yield_SMI_Prec_Tavg_Pet_Dem_Por anhand von RS ####
Yield_SMI_Prec_Tavg_Pet_Dem_Por<- read.csv("data//data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por.csv")
names(Yield_SMI_Prec_Tavg_Pet_Dem_Por)
head(Yield_SMI_Prec_Tavg_Pet_Dem_Por)
dim(Yield_SMI_Prec_Tavg_Pet_Dem_Por)
Yield_SMI_Prec_Tavg_Pet_Dem_Por$X <-NULL

## minimum Temperature  Data ##
# Rename Tmin_months_1999_demean_zscore
Tmin_months_1999<- read.csv("data/data_processed/Tmin_months_1999")

names(Tmin_months_1999)
head(Tmin_months_1999)
dim(Tmin_months_1999)
Tmin_months_1999$X <- NULL

## Vergleich den beiden year Vectoren ##
all(Yield_SMI_Prec_Tavg_Pet_Dem_Por$year == Tmin_months_1999$year)

## Vergleichen der beiden comId Vektoren ##
all(Yield_SMI_Prec_Tavg_Pet_Dem_Por$comId == Tmin_months_1999$comId)


# Interpretation
' 
Die Reihenfolge der beiden Datensätze ist die gleiche
'

## Löschen der reduntanten Vektoren ##
names(Yield_SMI_Prec_Tavg_Pet_Dem_Por[colnames(Yield_SMI_Prec_Tavg_Pet_Dem_Por) %in% names(Tmin_months_1999)])
Tmin_months_1999$SHAPE_AREA <- Tmin_months_1999$comId <- Tmin_months_1999$year <- NULL

## Cbind der beiden Datensätze ##
Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin <- cbind(Yield_SMI_Prec_Tavg_Pet_Dem_Por,Tmin_months_1999)
names(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin)
rownames(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin) <- NULL
Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin$X.1 <- NULL
Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin$X <- NULL

dim(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin)
head(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin,15)
tail(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin,16)

table(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin$year) # statt 525 nur noch 410 Beobachtungen

####################################################
#### Write Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin ####
####################################################


write.csv(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin, "data//data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin")
Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin <- read.csv("data//data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin")
names(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin)

