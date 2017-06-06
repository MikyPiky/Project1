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
- 4km_tmax.nc (Stephan Thober) 
- vg2500_krs (extern bereitgestellt) ->shape mit polygonen der Kreise
- Yield_SMI_Prec_Tavg_Pet_Dem_Por <-  Input aus 4km_por
'
#### Output ####
##Files
' 
- Tmin_months_1999
- Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax (File, welche Daten der Regionalstatistic (yield), SMI, Prec, und minimum Temperature  enthält)
'
## Plots
'
- Plot des Jahres 2003 der extrahierten Tmax Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
- Plot des Mai 2003 der extrahierten Tmax Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
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
#### Laden des Tmax NetCDF ####
###############################
# Tmax<-brick("/home/peichl/Documents/projects/correlation/data/data_raw/4km_pre.nc", varname="pre_lk")
# Tmax
## Funktioniert mit dieser File nicht so gut, daher benutze ich die Alternative

# #### Alternatives Laden: ncdf4 ####
Tmax_open <- nc_open("/home/peichl/Documents/projects/correlation/data/data_raw/4km_tmax.nc")
print(Tmax_open) 
Tmax <- ncvar_get(Tmax_open, varid = "tmax_lk")
nc_close(Tmax_open)
#offensichtlich habe ich hier 64 2/12 Jahre oder 770 Monate seit Januar 1950

dim(Tmax) 
770/12 #= 64 2/12  
# Datensatz deckt Januar 1950 - Februar 2014 ab
# Diese Datensatz ist 30 Monate länger als prec, geht aber zur gleichen Zeit los. 
class(Tmax)
levelplot(Tmax)
head(Tmax)
summary(Tmax)



###############################
#### -9999 & 0 Werte in NA ####
###############################
u <-Tmax == -9999
Tmax[u]<-NA


u<-Tmax== 0
Tmax[u]<-NA

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

Tmax <- as.data.frame(Tmax)
names(Tmax)

#############################
#### Verändern der Namen ####
# minimum Temperature  geht ein Jahr vor SMI los, also im Januar 1950 und geht 38 Monate länger, also statt bis 1.12.2010 bis 1.2.2014
idx <-seq(from=as.Date('1950-1-1'), to=as.Date('2014-2-1'), 'month')
idx <-as.yearmon(idx)
idx
idx_str<-paste("Tmax",idx, sep="")
idx_names<-make.names(c(idx_str), unique = TRUE)
idx_gsub<-gsub(" ","",idx_str)
idx_names<-make.names(c(idx_gsub), unique = TRUE)
idx_names
length(Tmax)==length(idx_names) # der Namensvektor hat die gleiche länge wie der Datensatz breit ist
colnames(Tmax)<-idx_names
names(Tmax)
###############################################
#### Rownames anpassen für merging process ####
dim(Tmax)
rownames(Tmax) <-0:411 

##########################
## Überprüfen der Daten ##
head(Tmax)
summary(Tmax) # nach Umwandlung der 0 Werte in NAs macht das nun mehr Sinn
names(Tmax)

######################################
#### Data.frame: 1995 produzieren ####
names(Tmax)

names(Tmax)[((length(Tmax)-191):length(Tmax))-38] # Da diese Dataframe 38 Monate länger als der Prec ist (nach Dec 2010), muss ich darauf reagieren: -38 

Tmax_1995<-Tmax[,((length(Tmax)-191):length(Tmax))-38]

head(Tmax_1995)
names(Tmax_1995) # check
rownames(Tmax_1995)<-0:411

######################################
#### Data.frame: 1999 produzieren ####
names(Tmax)[(length(Tmax)-143):length(Tmax)-38]
Tmax_1999<-Tmax[,(length(Tmax)-143):length(Tmax)-38]
head(Tmax_1999)
names(Tmax_1999)

rownames(Tmax_1999) <- 0:411

###########################
#### Maptools::spCbind ####
KreisTmax_spdf<-spCbind(KreisPolOGR,Tmax)
KreisTmax_spdf_1995 <- spCbind(KreisPolOGR,Tmax_1995)
KreisTmax_spdf_1999 <- spCbind(KreisPolOGR,Tmax_1999)

summary(KreisTmax_spdf_1999)

#### Projection des ursrünglichen SpatialPolygonsDataFrame übertragen
proj4string(KreisTmax_spdf)<-proj4string(KreisPolOGR)


###################################################################################################################################
############################################# Plotten der Niederschlagsdaten ######################################################
###################################################################################################################################
names(KreisTmax_spdf)

## Absolute Werte
ztdrei<-c( "TmaxSep2003"  ,"TmaxOkt2003" , "TmaxNov2003" , "TmaxDez2003",  "TmaxMai2003" , "TmaxJun2003" ,"TmaxJul2003" , "TmaxAug2003","TmaxJan2003",  "TmaxFeb2003" , "TmaxMär2003", "TmaxApr2003" )
summary(KreisTmax_spdf[,ztdrei])
at=(seq(-10 ,18,1)) # Intervall ausgewählt anhand der min max Werte.

## Z-score Werte erzeugen
data <- as.data.frame(KreisTmax_spdf[,ztdrei])
class(data)
data_zscore <- as.data.frame(scale(data, center=T, scale=T)) # scale gibt matrix aus, dies funktioniert aber nicht mit spCbind
class(data_zscore)
summary(data_zscore)
Kreisdata_zscore_spdf <- spCbind(KreisPolOGR,data_zscore) 

## Paramete für ssplot ##
ztdrei <- c( "TmaxSep2003"  ,"TmaxOkt2003" , "TmaxNov2003" , "TmaxDez2003",  "TmaxMai2003" , "TmaxJun2003" ,"TmaxJul2003" , "TmaxAug2003","TmaxJan2003",  "TmaxFeb2003" , "TmaxMär2003", "TmaxApr2003" )
summary(KreisTmax_spdf[,ztdrei])
at <- (seq(-10 ,18,1)) # Intervall ausgewählt anhand der min max Werte.
summary(Kreisdata_zscore_spdf)
at2 <- seq(-3.8, 3.8, 0.001)

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
colors = colorRampPalette(c("skyblue3", "white", "orangered3"))(1e4)


## Plot des Jahres 2003 der extrahierten Tmax Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
KreisTmax_spdf_Plt1 <- spplot(KreisTmax_spdf, at=at, ztdrei, col.regions= colors) # absolute Daten
## exported als png mit width 700
KreisTmax_spdf_Plt2 <- spplot(KreisTmax_spdf, at=at, "TmaxSep2003", col.regions= colors) # absolute Daten

## Plot des Jahres 2003 der extrahierten Tmax Werte auf Kreisniveau für Deutschland mit farbwahl für Presentation
Kreisdata_zscore_spdf_Plt1 <- spplot(Kreisdata_zscore_spdf, at=at2, ztdrei, col.regions= colors) # z -score
## exported als png mit width 700
Kreisdata_zscore_spdf_Plt2 <- spplot(Kreisdata_zscore_spdf, at=at2, "TmaxSep2003", col.regions= colors) # z -score

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
head(KreisTmax_spdf_1995)
KreisTmax_spdf_1995$RS
KreisTmax_spdf_1995$comId<-as.factor(str_sub(KreisTmax_spdf_1995$RS,1,5))
unique(KreisTmax_spdf_1995$comId)

##################################
#### Reshapen des Datensatzes ####
##################################

class(KreisTmax_spdf_1995)
KreisTmax_1995 <- as.data.frame(KreisTmax_spdf_1995)
dim(KreisTmax_1995) # die Dimension ist die selbe wie in MergeSMI_Yield, daher werde ich einfach den gleichen reshape Algorithmus verwenden

## Reorder Names ##
names(KreisTmax_1995)
length(names(KreisTmax_1995))
KreisTmax_1995 <-KreisTmax_1995[c(1:5,198,(6:197))]
head(KreisTmax_1995)

#### Make a list of vectors of months ####
names(KreisTmax_1995)
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
list0<-list(c(7,187), c(8,188), c(9,189), c(10,190), c(11,191), c(12,192),c(13,193),c(14,194),c(15,195), c(16,196), c(17,197), c(18,198))
# those list define the starting and ending point for the steps, 12 each
list0

#### Loop zum erstellen der Liste listMonthYearNames mit den IDs, welche jeweils den gleichen Monatesnamen, über alle Jahre hinweg, haben ####
listMonthYearNames<-list() # set container

for (i in 01:12){
  listMonthYearNames[i]<-list(c(names(KreisTmax_1995)[c(seq(list0[[i]][1],list0[[i]][2],12))]))
}
listMonthYearNames # list including a list of the listMonthYearNames across the years. This is the basis for the new variables, i.e the Tmax in each month
listMonthYearNames[[3]] # lIst of all marches across the 16 years considered here

#### Make the names for the new variables
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
listMonthNames<-list(c("Tmax_Jan"), c( "Tmax_Feb"), c("Tmax_Mar"), c("Tmax_Apr"), c("Tmax_Mai"), c("Tmax_Jun"),c("Tmax_Jul"),c("Tmax_Aug"),c("Tmax_Sep"), c("Tmax_Oct"), c("Tmax_Nov"), c("Tmax_Dec"))
listMonthNames

#### Loop, welche mir die Tmax pro Monat jeweils in einer Column ausgibt ####
y<-data.frame(1:6592) # set container
x<-data.frame() # set container


### loop over the twelve listMonthYearNames, split up, an then filled up with the values across the years
for(i in 1:12) 
{
  x <- reshape (KreisTmax_1995, 
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


## Explore the newly build dataframe with the Tmaxipitiation for each Monnth ##
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
Tmax_months <- cbind(x[,c(5:6, 183)],y)

names(Tmax_months)
head(Tmax_months)
rownames(Tmax_months)<-NULL

## Explore whether comIds and years match ##
head(Tmax_months, 15) # check
tail(Tmax_months, 15) # check
# In den rownames steht der Monat Dezember, da dies der letzte Monat ist, der gemelted, also in eine Spalte gefasst wurde. 
# Sieht aber sonst recht ordentlich aus.

## Befreien der year Variable von den Monatsangaben, hier Dezember ##
str_sub(Tmax_months$Year,8,11 )
Tmax_months$Year <- as.integer(str_sub(Tmax_months$Year,8,11 ))
head(Tmax_months)
unique(Tmax_months$Year)

## Anpassen der Namen fürs mergen
colnames(Tmax_months)
colnames(Tmax_months)[3] <- "year"

###################################################################################
#### Abgleichen der neu erstellten Daten mit KreisTmax_1995 (orginal Daten) ####
head(KreisTmax_1995)

## Erstellen und überprüfen der beiden Vektoren, die jeweils den Tmax des Jahres 1999 im Monat Januar wiedergeben ##
## Vector in Tmax_months des Tmax Januar 1999
Tmax_months$Tmax_Jan[Tmax_months$year == 1999]
length(Tmax_months$Tmax_Jan[Tmax_months$year == 1999])

## Vector in KreisTmax_1995 des Tmax Januar 1999
names(KreisTmax_1995)
KreisTmax_1995$TmaxJan1999
length(KreisTmax_1995$TmaxJan1999)

all(Tmax_months$Tmax_Jan[Tmax_months$year == 1999] == KreisTmax_1995$TmaxJan1999, na.rm = T)
# Daten sehen gut aus

#############################################################################################################################################################################################
################################################################## Manipulation der Temperature Daten #####################################################################################
#############################################################################################################################################################################################

#######################################
## Löschen der comId 02000 und 11000 ##
#######################################
' Both cities are neglected '
unique(Tmax_months$comId)
names(Tmax_months)
Tmax_months<-Tmax_months[!Tmax_months$comId == "02000",]  #Hamburg
Tmax_months<-Tmax_months[!Tmax_months$comId == "11000",]  #Berlin

######################################################################################
## Berechnen der lags der Avg. Temperature Variablen für die Monate August bis December ##
######################################################################################

head(Tmax_months)

## Ordnen der Spalten so, dass die Variablen erst nach comID und dann nach time geordnet werden.
Tmax_months<-Tmax_months[order(Tmax_months$comId, Tmax_months$year),]

Tmax_months_arr <- slide(Tmax_months, Var ="Tmax_Aug", GroupVar= "comId", slideBy = -1)
Tmax_months_arr <- slide(Tmax_months_arr, Var = "Tmax_Sep", GroupVar = "comId", slideBy = -1)
Tmax_months_arr <- slide(Tmax_months_arr, Var = "Tmax_Oct", GroupVar = "comId", slideBy = -1)
Tmax_months_arr <- slide(Tmax_months_arr, Var = "Tmax_Nov", GroupVar = "comId", slideBy = -1)
Tmax_months_arr <- slide(Tmax_months_arr, Var = "Tmax_Dec", GroupVar = "comId", slideBy = -1)

head(Tmax_months_arr)

## Change names
names(Tmax_months_arr)
names(Tmax_months_arr)[names(Tmax_months_arr) == c("Tmax_Aug-1" )] <- c("Tmax_Aug_lag")
names(Tmax_months_arr)[names(Tmax_months_arr) == c("Tmax_Sep-1" )] <- c("Tmax_Sep_lag")
names(Tmax_months_arr)[names(Tmax_months_arr) == c("Tmax_Oct-1" )] <- c("Tmax_Oct_lag")
names(Tmax_months_arr)[names(Tmax_months_arr) == c("Tmax_Nov-1" )] <- c("Tmax_Nov_lag")
names(Tmax_months_arr)[names(Tmax_months_arr) == c("Tmax_Dec-1" )] <- c("Tmax_Dec_lag")

###################################
####  Data Frame Starting 1999 ####
names(Tmax_months_arr)
unique(Tmax_months_arr$year)
Tmax_months_1999 <- Tmax_months_arr[Tmax_months_arr$year >= 1999,] 
unique(Tmax_months_1999$year)

#############################################
## Demean minimum Temperature  Data with constant ##

## Operations with no conditioning of communities
## April

head(Tmax_months_1999)

# ## Löschen der Daten mit NULL ##
# 'Es ist sehr unwahrscheinlich, dass es einem Monat in Deutschland nicht geregnet hat.'
# any(Tmax_months_1999$Tmax_Apr==0) # habe ich vorher schon gemacht 
# Tmax_months_1999$Tmax_Apr[Tmax_months_1999$Tmax_Apr==0] <- NA

###############
## demeaning ##
names(Tmax_months_1999)
length(names(Tmax_months_1999))

x <- NULL
demean <- NULL


for (i in 4:20)
{
  x <- Tmax_months_1999[,i] - mean(Tmax_months_1999[,i], na.rm = T)
  demean <- cbind(demean,x)
  
}
head(demean)
dim(demean)

colnames(demean) <- c("Tmax_Jan_demean",  "Tmax_Feb_demean" , "Tmax_Mar_demean" , "Tmax_Apr_demean" ,"Tmax_Mai_demean" , "Tmax_Jun_demean" ,"Tmax_Jul_demean" , "Tmax_Aug_demean" , "Tmax_Sep_demean" ,
                      "Tmax_Oct_demean", "Tmax_Nov_demean","Tmax_Dec_demean","Tmax_Aug_lag_demean","Tmax_Sep_lag_demean","Tmax_Oct_lag_demean","Tmax_Nov_lag_demean" ,"Tmax_Dec_lag_demean" )


Tmax_months_1999_demean<-cbind(Tmax_months_1999, demean)
names(Tmax_months_1999_demean)


## Compare to seperately produced vector for April ##
Tmax_months_1999_demean <- transform(Tmax_months_1999_demean , demeanTmax_Apr = detrend(Tmax_Apr, "constant"))
names(Tmax_months_1999_demean)
all(Tmax_months_1999_demean$demeanTmax_Apr%in%Tmax_months_1999_demean$Tmax_Apr_demean)
Tmax_months_1999_demean$demeanTmax_Apr <- NULL
# Interpretation
'
Für den Monat April scheinen die Ergebnisse zu stimmen.
'


###################
## standardizing ##

names(Tmax_months_1999)
length(names(Tmax_months_1999))

x<-NULL
zscore<-NULL
for (i in 4:20)
{
  x <- (Tmax_months_1999[,i] - mean(Tmax_months_1999[,i], na.rm = T))/sd(Tmax_months_1999[,i], na.rm=T)
  zscore <- cbind(zscore,x)
}
head(zscore)
dim(zscore)

colnames(zscore) <- c("Tmax_Jan_zscore",  "Tmax_Feb_zscore" , "Tmax_Mar_zscore" , "Tmax_Apr_zscore" ,"Tmax_Mai_zscore" , "Tmax_Jun_zscore" ,"Tmax_Jul_zscore" , "Tmax_Aug_zscore" , "Tmax_Sep_zscore" ,
                      "Tmax_Oct_zscore", "Tmax_Nov_zscore","Tmax_Dec_zscore","Tmax_Aug_lag_zscore","Tmax_Sep_lag_zscore","Tmax_Oct_lag_zscore","Tmax_Nov_lag_zscore" ,"Tmax_Dec_lag_zscore" )


Tmax_months_1999_demean_zscore <- cbind(Tmax_months_1999_demean, zscore)
names(Tmax_months_1999_demean_zscore)


#####################################################
## Compare to seperately produced vector for April ##
Tmax_months_1999_demean_zscore <- transform(Tmax_months_1999_demean_zscore , zScore_Pre_Apr<- (Tmax_Apr - mean(Tmax_Apr, na.rm=T))/sd(Tmax_Apr, na.rm=T))
all(Tmax_months_1999_demean_zscore$zScore_Pre_Apr%in%Tmax_months_1999_demean_zscore$Tmax_Apr_zscore)
## Löschen von "demeanTmax_Apr"
Tmax_months_1999_demean_zscore$demeanTmax_Apr <- NULL


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
#### Write Tmax_months_1999 ####
head(Tmax_months_1999_demean_zscore)
names(Tmax_months_1999_demean_zscore)

write.csv(Tmax_months_1999_demean_zscore, "data//data_processed/Tmax_months_1999")


########################################################################################
#### Mergen von Tmax_months_1999 mit Yield_SMI_Prec_Tavg_Pet_Dem_Por anhand von RS ####
Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin <- read.csv("data//data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin")
names(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin)
head(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin)
dim(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin)
Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin$X <-NULL

## minimum Temperature  Data ##
# Rename Tmax_months_1999_demean_zscore
Tmax_months_1999<- read.csv("data/data_processed/Tmax_months_1999")

names(Tmax_months_1999)
head(Tmax_months_1999)
dim(Tmax_months_1999)
Tmax_months_1999$X <- NULL

## Vergleich den beiden year Vectoren ##
all(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin$year == Tmax_months_1999$year)

## Vergleichen der beiden comId Vektoren ##
all(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin$comId == Tmax_months_1999$comId)


# Interpretation
' 
Die Reihenfolge der beiden Datensätze ist die gleiche
'

## Löschen der reduntanten Vektoren ##
names(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin[colnames(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin) %in% names(Tmax_months_1999)])
Tmax_months_1999$SHAPE_AREA <- Tmax_months_1999$comId <- Tmax_months_1999$year <- NULL

## Cbind der beiden Datensätze ##
Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax <- cbind(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin,Tmax_months_1999)
names(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax)
rownames(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax) <- NULL
Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax$X.1 <- NULL
Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax$X <- NULL

dim(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax)
head(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax,15)
tail(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax,16)

table(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax$year) # statt 525 nur noch 410 Beobachtungen

####################################################
#### Write Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax ####
####################################################
write.csv(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax, "data//data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax")

####################################################################################################
#### Write Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax with rounded values and space as delimiter ####
####################################################################################################
Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax <- read.csv("data//data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax")

dim(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax)
names(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax)

drops <- (c("year", "comId", "com" , "comIdState","comState", "area100ha", "SHAPE_AREA" ))
Yield_Covariates_round <- round(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax[, !names(Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax) %in%  drops],5)

head(Yield_Covariates_round)
names(Yield_Covariates_round)
dim(Yield_Covariates_round)

names(Yield_Covariates[, names(Yield_Covariates) %in%  drops])
head(Yield_Covariates_round)
dim(Yield_Covariates[, names(Yield_Covariates) %in%  drops])

Yield_Covariates_round <- cbind(Yield_Covariates[, names(Yield_Covariates) %in%  drops], Yield_Covariates_round)
head(Yield_Covariates_round)

write.table(Yield_Covariates_round,file="~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_round", sep = " ",  row.names=FALSE  )

