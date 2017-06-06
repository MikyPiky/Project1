#### File Description ####
'
# Ziel ist es die SMI und Daten der Regionalstatistik zu mergen --> tidy data
  - Mergen: Anpassen der ComIds und Abgleichen notwendig, da es hier unterschiede gibt
  - Der finale Datensatz hat nur 410 räumliche Einheiten pro Jahr: Gründe dafür
  - Nur Kreis und Kreisfreie Städte berücksichtigt, also jede ComID mit 2 oder 3 Ziffern ist nicht berücksichtigt.

# Anschließend wird der Datensatz manipuliert, um diesen für weitere Arbeiten nutzen zu können. 
  - Hinzufügen von IDs und Namen der Bundesländer
  - Löschen der Daten, welche redundant sind
  - Ab dem Jahr 1995 
  - Output: Yield_SMI_long

# Anschließend erstelle ich lagged Daten des SMI für die Monate August bis December (Wachstumsyklus von Wintersorten)
  - Hier habe ich auch die yields==0 gelöscht und in NAs unbenannt
  - Output: Yield_SMI_arr (ab 1995) <- basierend auf Yield_SMI_long

# Anschließen kürze ich den Datensatz von 1995 auf 1999
  - Output: Yield_SMI basierend auf Yield_SMI_arr

# Anschließend Manipulation der Yield Daten
  - Datensatz basierend auf Yield_SMI 
  - Standardisierung der Yield Daten -> detrended z-score  (vor allem für Luis Fuzzy Rule und andere Data Mining Ansätze)
  - Output: Yield_SMI_detrend (detrended z-score of all crops an 1999)

# steht noch aus  
  - Detrending der absoluten Werte (für Panelansatz)
    - detrend() scheint nicht wirklich zu funktionieren, daher muss das manuel gemacht werden.
  



'
#### Dependencies and Input ####
'
# yieldData (Regionalstatistik) # Achtung, habe ich in in  yieldData.csv in data_processed Ordner umbennant
# KreisSMI_spdf_1995 (4km_SMI)
'

#### Vorgehen beim bearbeiten der SMI Daten ####
'Ziel: Ich habe 412 Beobachtungen pro Zeitschritt, also jeweils ein Monat in einem Jahr. 
Da mein Ziel-Datensatz tidy ist, also die Jahre untereinander fortlaufend angeordnet sind, müssen meine Variablen entsprechend angepasst werden.
Soll heißen aus den SMI Variablen mit Monats und Jahres zuordnung wird nun ein SMI mit Monats Zuordnung und die Jahre verschmelzen alle in 
eine Spalte. 
Damit habe ich 12 Spalten für den SMI, also eine pro Monat, und 16 (Jahre) mal 412 Beobachtungen, also 6592 Beobachtungen, je Spalte.
Für dieses Vorgehen reshape ich den Kreis_SMI_dataframe von wide nach long mit Hilfe der stats::reshape function. 
Dazu muss ich eine Liste mit den SMI mit Monat und Jahres Zuordnung erstellen, welche dann jeweils in die SMI mit Monats zuordnung
im long Format übergeht. -> listMonthYearNames[[i]]
Anschließend erstelle ich die Namen der SMI mit reiner Monats Zurdnung -> listMonthNames
Mit Hilfe der Year Variablen werden dann die Beeobachtungen innerhalb der SMI mit Monats Zuordnungen nach den Jahren differenziert.
'

#### Anzahl der Communities ####
'
- yieldData 525
- Kreis_SMI_spdf_1995: 410
    * Nur fünstellige ComIDs berücksichtigt: 55 ComIds weniger
    * Landkreis Aachen:  Der Kreis Aachen wurde gemäß Aachen-Gesetz mit Ablauf des 20. Oktober 2009 aufgelöst, und aus den neun Gemeinden des Kreises Aachen und 
                         der Stadt Aachen wurde mit Wirkung vom 21. Oktober 2009 als neuer Gemeindeverband die Gebietskörperschaft Städteregion Aachen gebildet
    * Kreisreformen: Sachsen(2008: 35 ComIds weniger) und Sachsen-Anhalt(2007: 24 ComIds weniger)
'

####  Validäts Check des Merge Prozesses ####
'
  - Die Daten machen nach den Validätschecks einen soliden Eindruck

'

#### Ouput ####
'
  - Yield_SMI_long (Daten ab 1995)
  - Yield_SMI (Daten ab 1999)
  - Yield_norm_1999 (Daten für cdf kernel smoother)
  - Yield_SMI_detrend ( this data are used further to combine it with other explanatory variables)
  - 
  
'

#### Hintergrund ####
### Hadely Wickham: Tidy Data ####
'
Tidy Data als Ziel, da diese auch als Panel Daten Format bezeichnet werden
Begriff tidy data, wie er von Hadley Wickham benutzt wird:
Jede Variable hat eine Spalte, jede Beobachtung ist eine Zeile, ?jede Observational unit bildet einen table?

id year var1 var2 var3 etc.
'

#### Pakete ####
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
library("ggplot2")
library("foreign")
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

#### Load large table with all the variables of regionalstatistik ####
  yieldData <- read.table("/Storage/ownCloud/Home/Klimabuero/Proj1/data/data_raw/Yield2015.csv", sep=";", dec=",", quote="\"", na.strings=(c("-","/", ".")),
                        col.names=c("year","comId","com","winterWheat","rye","winterBarley","summerBarley","oats","triticale","potatoes","sugarBeet","winterRape",
                                    "siloMaize"),   nrows=8925, skip=7, colClasses=c("integer","factor", "factor", rep("numeric", 10)))
View(yieldData)
str(yieldData)
head(yieldData)
tail(yieldData)

### Explore yieldData ####
head(yieldData)
tail(yieldData)
names(yieldData)
table(yieldData$year) # hier stimmt noch die Anzahl der Kreis pro Jahr
summary(yieldData)

###############################################################
#### Load SpatialPolygonesDataframe mit SMI Daten ab 1995 #### 
KreisSMI_spdf_1995<- readOGR("./data/data_raw/","KreisSMI_spdf_1995")

## Explore file
names(KreisSMI_spdf_1995)
head(KreisSMI_spdf_1995)

#################################################################
'Die yieldData und Yield Daten werden anhand der Jahre und comIDS gemerged. In KreisSMI sind diese Daten in der RS file und es gibt auch nur 5 stellig IDs. 
Daher müssen die IDs in KreisSMi angepasst werden (str_length ist 5)'

###Vergleichen Namen der Kreise####
# Sind COMIDs vom yield und yieldData aka data die gleichen?
# names(yieldData)
head(yieldData)[c("year","comId", "com" )]

# names(KreisSMI_spdf_1995)
head(KreisSMI_spdf_1995)[c("RS", "GEN")]

### Kreis IDs anschauen und vergleichen #### 
KreisSMI_spdf_1995$RS[1:20] 
yieldData$comId[1:20]
# Die Strings in KreisSMI_spdf sind länger, daher müssen diese erst auf 5 Stellen gekürzt werden

#### Verkürzen des ID-Strings, also KreisSMI_spdf_1995$RS  ####
str_length(KreisSMI_spdf_1995$RS) #12
KreisSMI_spdf_1995$RS5 <- as.factor(str_sub(KreisSMI_spdf_1995$RS,1,5))
str_length(KreisSMI_spdf_1995$RS5) #5

unique(KreisSMI_spdf_1995$RS5)


## Use RS5 Values for RS
KreisSMI_spdf_1995$RS <- KreisSMI_spdf_1995$RS5
KreisSMI_spdf_1995$RS5 <- NULL
head(KreisSMI_spdf_1995)
names(KreisSMI_spdf_1995)

### Kreis IDs (verkürzt) anschauen und vergleichen ####
head(yieldData)[c("comId", "com")]
head(KreisSMI_spdf_1995)[c("RS", "GEN")]
# sollte gehen, darüber hinaus ist "GEN" besser als "com" bezüglich der Darstellung der Namen

## Frage: Wieviele Beobachtungen pro Jahr gibt es in comID mit der Länge 5
length(unique(yieldData$comId[str_length(yieldData$comId)==5]))
# Antwort ist 470

###################################################################
#### Jetzt muss ich noch den SMI-Datensatz entsprechend kippen ####
###################################################################
KreisSMI_spdf_1995
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
'Anmerkung für reshape eines gesamtdatensatzes: diese Liste müsste wahrscheinlich verändert werden'
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
listMonthYearNames <- list()
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
y <- data.frame(1:8652) # set container
x <- data.frame() # set container


### loop over the twelve listMonthYearNames, split up, an then filled up with the values across the years
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
dim(x)
dim(y)
names(x)
names(y)
length(x) - 1
SMI_months<-cbind(x[,c(1:4, length(x) - 1)],y)

names(SMI_months)
head(SMI_months)
rownames(SMI_months)<-NULL

## Explore whether comIds and years match ##
head(SMI_months, 15) # check
tail(SMI_months, 15) # check
# In den rownames steht der Monat Dezember, da dies der letzte Monat ist, der gemelted, also in eine Spalte gefasst wurde. 
# Sieht aber sonst recht ordentlich aus.

## Befreien der year Variable von den Monatsangaben, hier Dezember
SMI_months$Year<-as.integer(str_sub(SMI_months$Year,6,9 ))
## Anpassen der Namen fürs mergen
colnames(SMI_months)[5]<-"year"
colnames(SMI_months)[3]<-"comId"
colnames(SMI_months)



#### Abgleichen der neu erstellten Daten mit KreisSMI_df_1995 (orginal Daten)
head(KreisSMI_df_1995)[1:7]
head(SMI_months) 

tail(KreisSMI_df_1995)[length(KreisSMI_df_1995)]
tail(SMI_months) 

## Erstellen und überprüfen der beiden Vektoren, die jeweils den SMI des Jahres 1999 im Monat Januar wiedergebeb
 

## Vector in SMI_months des SMI Januar 1999
SMI_months$SMI_Jan[SMI_months$year==1999]
length(SMI_months$SMI_Jan[SMI_months$year==1999])

## Vector in KreisSMI_df_1995 des SMI Januar 1999
KreisSMI_df_1995$SMJan1999
length(KreisSMI_df_1995$SMJan1999)

match(KreisSMI_df_1995$SMJan1999,SMI_months$SMI_Jan[SMI_months$year==1999]) # check
all(SMI_months$SMI_Jan[SMI_months$year==1999]%in%KreisSMI_df_1995$SMJan1999) # check
# Daten sehen gut aus



###################################################################################################################################################################################################
################################################################################# Manipulation of Yield Data ######################################################################################
###################################################################################################################################################################################################
Yield_SMI_long <- read.csv("./data/data_processed/Yield_SMI_long")


head(Yield_SMI_long)
names(Yield_SMI_long)

##########################################
#### Delete zero values in yield Data ####
## Hier lösche ich alle Yield mit Wert 0, da dies nicht nachzuvollziehen ist
## Darünerhinaus sollen die Variablen Anomalien erklären, wenn angepflanzt wurde.
Yield_SMI_long$winterWheat[Yield_SMI_long$winterWheat==0] <- NA
Yield_SMI_long$rye[Yield_SMI_long$rye==0] <- NA
Yield_SMI_long$winterBarley[Yield_SMI_long$winterBarley==0] <- NA
Yield_SMI_long$oats[Yield_SMI_long$oats==0] <- NA
Yield_SMI_long$triticale[Yield_SMI_long$triticale==0] <- NA
Yield_SMI_long$potatoes[Yield_SMI_long$potatoes==0] <- NA
Yield_SMI_long$sugarBeet[Yield_SMI_long$sugarBeet==0] <- NA
Yield_SMI_long$winterRape[Yield_SMI_long$winterRape==0] <- NA
Yield_SMI_long$siloMaize[Yield_SMI_long$siloMaize==0] <- NA


##########################################################################
#### Produce lagged variabled of SMI for the months after the harvest ####
## SMI ##

"Das erstellen der lags muss vor dem mergen passsieren, da yield nur bis 1999 geht."

Yield_SMI_arr <- slide(Yield_SMI_long, Var ="SMI_Aug", GroupVar= "comId", slideBy = -1)
Yield_SMI_arr <- slide(Yield_SMI_arr, Var ="SMI_Sep", GroupVar= "comId", slideBy = -1)
Yield_SMI_arr <- slide(Yield_SMI_arr, Var ="SMI_Oct", GroupVar= "comId", slideBy = -1)
Yield_SMI_arr <- slide(Yield_SMI_arr, Var ="SMI_Nov", GroupVar= "comId", slideBy = -1)
Yield_SMI_arr <- slide(Yield_SMI_arr, Var ="SMI_Dec", GroupVar= "comId", slideBy = -1)

head(Yield_SMI_arr)                                                

table(Yield_SMI_arr$comId)  

# Visual Check of created lag variables ##

# Yield_SMI_arr[Yield_SMI_arr$comId==1001,c("year","comId","SMI_Dec", "SMI_Dec-1")]
# Yield_SMI_arr[Yield_SMI_arr$comId==10043,c("year","comId","SMI_Dec", "SMI_Dec-1")]
# Yield_SMI_arr[Yield_SMI_arr$comId==16077,c("year","comId","SMI_Dec", "SMI_Dec-1")]
# 
# Yield_SMI_arr[Yield_SMI_arr$comId==1001,c("year","comId","SMI_Nov", "SMI_Nov-1")]
# Yield_SMI_arr[Yield_SMI_arr$comId==10043,c("year","comId","SMI_Nov", "SMI_Nov-1")]
# Yield_SMI_arr[Yield_SMI_arr$comId==16077,c("year","comId","SMI_Nov", "SMI_Nov-1")]
# 
# Yield_SMI_arr[Yield_SMI_arr$comId==1001,c("year","comId","SMI_Oct", "SMI_Oct-1")]
# Yield_SMI_arr[Yield_SMI_arr$comId==10043,c("year","comId","SMI_Oct", "SMI_Oct-1")]
# Yield_SMI_arr[Yield_SMI_arr$comId==16077,c("year","comId","SMI_Oct", "SMI_Oct-1")]
# 
# Yield_SMI_arr[Yield_SMI_arr$comId==1001,c("year","comId","SMI_Sep", "SMI_Sep-1")]
# Yield_SMI_arr[Yield_SMI_arr$comId==10043,c("year","comId","SMI_Sep", "SMI_Sep-1")]
# Yield_SMI_arr[Yield_SMI_arr$comId==16077,c("year","comId","SMI_Sep", "SMI_Sep-1")]
# 
# Yield_SMI_arr[Yield_SMI_arr$comId==1001,c("year","comId","SMI_Aug", "SMI_Aug-1")]
# Yield_SMI_arr[Yield_SMI_arr$comId==10043,c("year","comId","SMI_Aug", "SMI_Aug-1")]
# Yield_SMI_arr[Yield_SMI_arr$comId==16077,c("year","comId","SMI_Aug", "SMI_Aug-1")]

## Interpretation
'
Das erstellen der lagged Variablen scheint gut geklappt zu haben
'

## Umbennen der lagged Variablen ##
names(Yield_SMI_arr)
names(Yield_SMI_arr)[names(Yield_SMI_arr)==c("SMI_Aug-1" )]  <- c("SMI_Aug_lag")
names(Yield_SMI_arr)[names(Yield_SMI_arr)==c("SMI_Sep-1" )]  <- c("SMI_Sep_lag")
names(Yield_SMI_arr)[names(Yield_SMI_arr)==c("SMI_Oct-1" )]  <- c("SMI_Oct_lag")
names(Yield_SMI_arr)[names(Yield_SMI_arr)==c("SMI_Nov-1" )]  <- c("SMI_Nov_lag")
names(Yield_SMI_arr)[names(Yield_SMI_arr)==c("SMI_Dec-1" )]  <- c("SMI_Dec_lag")

head(Yield_SMI_arr)

###########################################
#### Write newly created Yield_SMI_arr ####

write.csv(Yield_SMI_arr, "data/data_processed/Yield_SMI_arr.csv")


#################################################################################################################################################################################
#################################################################################################################################################################################
###########################################################
#### Mergen von SMI_months mit yieldData anhand von RS ####
###########################################################

names(SMI_months)
names(yieldData)
unique(SMI_months$year)
unique(yieldData$year) 

head(yieldData)
head(SMI_months)

tail(yieldData)
tail(SMI_months)

mergeNames <- names(SMI_months)[names(SMI_months)%in%names(yieldData)]

Yield_SMI_long <- merge(yieldData,SMI_months, by=mergeNames)
head(Yield_SMI_long, 17)

yieldData[yieldData$year==1999,][1:15,]
Yield_SMI_long[Yield_SMI_long$year==1999,][1:15,]

table(Yield_SMI_long$year) # statt 525 nur noch 410 Beobachtungen

###################################################
#### Neuen Datensatz auf Richigkeit überprüfen ####

#### Vergleichend der Daten, v.a. yield Daten####

yieldData[yieldData$year==1999,][3:12,3:13]%in%Yield_SMI_long[Yield_SMI_long$year==1999,][1:10,3:13]

yieldData[yieldData$year==1999,][3:12,3:13]== Yield_SMI_long[Yield_SMI_long$year==1999,][1:10,3:13]

all(str_length(Yield_SMI_long$comId)==5)
# der neue Datensatz hat nur ComIds mit der länge 5, daher nehmen wir die anderen aus den yieldData Datensatz raus

### Nur noch ComId mit der Länge 5 ###
yieldData_red <- yieldData[str_length(yieldData$comId)==5,]
unique(yieldData_red$comId)
length(unique(yieldData_red$comId)) # 470

##  Vergleichen der Beobachtungen pro Jahr
table(yieldData$year) # 525
table(yieldData_red$year) # hier sind es nur noch 470 Beobachtungen, also 55 weniger
# Durch das weglassen der IDs kleiner als 5 fallen 55 Werte weg; Jedoch hat 
table(year) # Nur 410 Werte

### Der Wegfall der anderen 60 ComIDs lässt sich wie folgt erklären ####
all(yieldData_red[yieldData_red$year==1999,][1:60,1:12]%in%Yield_SMI_long[Yield_SMI_long$year==1999,][1:60,1:12])

yieldData_red[yieldData_red$year==1999,][1:60,1:12]==Yield_SMI_long[Yield_SMI_long$year==1999,][1:60,1:12]

yieldData_red[yieldData_red$year==2005,][c(1:82),1:12]%in%Yield_SMI_long[Yield_SMI_long$year==2005,][1:82,1:12]

# Zumindest für die ersten 80 Beobachtungen scheint es übereinstimmend zu sein, 
# aber was passiert danach
yieldData_red[yieldData_red$year==2005,][80:90,1:12]
Yield_SMI_long[Yield_SMI_long$year==2005,][80:90,1:12]

# Bei Yield_SMI_long fehlt der Kreis Aachen mit der comId 05354
## Fehlt der Kreis in den vorgänger Daten schon?

### KreisSMI_df_1995
any(levels(KreisSMI_df_1995$RS)=="05354")

# Das scheint der Fall zu sein

### Laden des ursprünglichen SpatialPolygonDataFrames
KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/","vg2500_krs")
dim(KreisPolOGR)

KreisPolOGR$RS=="053540000000"
# scheint es auch hier schon nicht zu geben
# Erklärung: Der Kreis Aachen wurde gemäß Aachen-Gesetz mit Ablauf des 20. Oktober 2009 aufgelöst, und aus den neun Gemeinden des Kreises Aachen und 
# der Stadt Aachen wurde mit Wirkung vom 21. Oktober 2009 als neuer Gemeindeverband die Gebietskörperschaft Städteregion Aachen gebildet
# Damit fällt eine Beobachtung weg

#### Weitere Inspektion der Daten ####
21*410
any(Yield_SMI_long$comId == 01)
length(unique(Yield_SMI_long$comId)) # es gibt pro Jahr 410 Beobachtungen
21*410/410 #= 16, so viele Jahre sind im Datensatz

# Welche weiteren IDs gibt es nicht?
yieldData_red[yieldData_red$year==2005,][c(1:82,84:361),1:12]%in%Yield_SMI_long[Yield_SMI_long$year==2005,][1:360,1:12]
# bis hierhin stimmmen die Datensätze überein

yieldData_red[yieldData_red$year==2005,][362:396,1:12]# comIds 13071 -14389 sind nicht im Datensatz Yield_SMI_long zu finden, Grund Kreisreform Sachsen 2008
dim(yieldData_red[yieldData_red$year==2005,][362:396,1:12]) # damit fallen 35 Beobachtungen weg

Yield_SMI_long[Yield_SMI_long$year==2005,][361:398,1:12]
# das sind Landkreis aus dem Osten

# Diese Abschnitte stimmen wieder überein
yieldData_red[yieldData_red$year==2005,][397:423,1:12]%in%Yield_SMI_long[Yield_SMI_long$year==2005,][361:387,1:12]
yieldData_red[yieldData_red$year==2005,][c(1:82,84:361, 397:423),1:12]%in%Yield_SMI_long[Yield_SMI_long$year==2005,][1:387,1:12]
# Ab Wittenberg stimmen die Daten wieder nicht überein
yieldData_red[yieldData_red$year==2005,][c(1:82,84:361, 397:424),1:12]%in%Yield_SMI_long[Yield_SMI_long$year==2005,][1:388,1:12]
Yield_SMI_long[Yield_SMI_long$year==2005,][386:410,1:12] # 15091 ist noch in Yield_SMI_long, 15101 
yieldData_red[yieldData_red$year==2005,][423:448 ,1:12] # 15091 ist noch in Yield_SMI_long, 15101 bis 15370  fehlt, Grund Kreisgebietsreform 2007 in Sachsen Anhalt
dim(yieldData_red[yieldData_red$year==2005,][423:448 ,1:12]) # hier fallen damit 24 Beobachtungen weg

# Vollständiger Abgleich der beiden Datensätze
yieldData_red[yieldData_red$year==2005,][c(1:82,84:361,397:423,448:470),1:12]%in%Yield_SMI_long[Yield_SMI_long$year==2005,][1:410,1:12] 
# check, Unterschiede Yield_SMI_long und yieldData_red sind erklärt

# Nun stellt sich die Frage, welche Landkreise aus dem orginalen SpatialPolygonesDataFrame fehlen und warum, da diese ursprünglich 412 statt jetzt 410 sind
Yield_SMI_long_comID<-unique(Yield_SMI_long$comId)[str_length(unique(Yield_SMI_long$comId))==5]
KreisSMI_df_1995_RS<-unique(KreisSMI_df_1995$RS)


length(Yield_SMI_long_comID)
length(unique(KreisSMI_df_1995_RS))

all(KreisSMI_df_1995_RS[c(1:14, 15,16:411)]%in%Yield_SMI_long_comID[1:410])
KreisSMI_df_1995_RS[15] # 02000 wurde nicht übernommen: Hamburg

all(KreisSMI_df_1995_RS[c(1:14,16:325,327:412)]%in%Yield_SMI_long_comID[c(1:410)])
KreisSMI_df_1995_RS[326] # 11000 wurde nicht übernommen: Berlin

KreisSMI_df_1995[KreisSMI_df_1995$RS==02000,]
unique(yieldData$comId)
yieldData[yieldData$year=="2000", 1:10]
yieldData[yieldData$year=="2001", 1:10]

############################################################################################################################################################################################
###################################################################### Manipulation des Datensatzes ########################################################################################
############################################################################################################################################################################################
Yield_SMI_long <- read.csv("./data/data_processed/Yield_SMI_long")


## Explore that data set
unique(Yield_SMI_long$year) # starting 1999
unique(Yield_SMI_long$comId)


##############################
#### Make state variables ####
##############################

## comIdState
# i.e. if comId starts with 1 give it a 01 and when it starts with 1 give it a 16 

class(unique(Yield_SMI_long$comId))
Yield_SMI_long$comId

x <- Yield_SMI_long$comId

str_sub(x, -3, -1) <- "" ;x
table(x)
Yield_SMI_long$comIdState<-x
head(Yield_SMI_long)
tail(Yield_SMI_long, 20)
unique(Yield_SMI_long$comIdState)

####################################
## make names for state variables ##
names(Yield_SMI_long)

# make list of comStateIds und Namen
csIds_list<-list("comStateIds"=unique(Yield_SMI_long$comIdState), "comStateNames"= c("Schleswig-Holstein","Lower Saxony", "Bremen","NRW", "Hesse","Rhineland-Palatinate","Baden-Württemberg","Bavaria","Saarland","Brandenburg","Mecklenburg-Vorpommern","Saxony","Saxony-Anhalt","Thuringia" )) ; csIds_list
csIds_list[[1]][1]
csIds_list[[2]][1]
seq_along(csIds_list$comStateIds)
seq_along(csIds_list$comStateNames) #beide sind gleich lang

## Loop durch die Liste um  comStateNames den entsprechende comStateIds zuzuordnen
for (i in seq_along(csIds_list$comStateIds))
{
  Yield_SMI_long$comState[Yield_SMI_long$comIdState==csIds_list[[1]][i]] <- csIds_list[[2]][i]
  
}
## Check, whether adding names to comIdState worked
table(Yield_SMI_long$comState=="Lower Saxony")
table(Yield_SMI_long$comIdState=="3")
# looks good
unique(Yield_SMI_long$comState)
unique(Yield_SMI_long$comIdState)
head(Yield_SMI_long, 15)
tail(Yield_SMI_long, 15)
# View(Yield_SMI_long)



##################################################
#### Schreiben des Yield_SMI_long Datensatzes #### 
head(Yield_SMI_long)
table(Yield_SMI_long$year)
# write.table(Yield_SMI_long,"./data/data_processed/Yield_SMI_long", row.names= FALSE)
write.csv(Yield_SMI_long,"./data/data_processed/Yield_SMI_long", row.names= FALSE )




# #############################################################################################################################################################################################
# #################################################################  For further analysis only use data starting 1999 #########################################################################
# #############################################################################################################################################################################################
# Yield_SMI_arr <- read.csv("data/data_processed/Yield_SMI_arr.csv")
# 
# 
# 
# ## Delete years 1995 - 1998 ##
# sapply(Yield_SMI_arr, class)
# Yield_SMI<-Yield_SMI_arr[Yield_SMI_arr$year >= 1999,] 
# 
# 
# ##################################################
# #### Schreiben des Yield_SMI Datensatzes #### 
# write.csv(Yield_SMI,"data/data_processed/Yield_SMI", row.names= FALSE )
# 
# 
# 
# # ###################################################################################################################################################################################################
# # ############################################################################################## Detrended Z-score ##################################################################################
# # ###################################################################################################################################################################################################
# # 
# # ######################
# # ####  winterWheat ####
# # 'First I consider winterWheat as a training set on which I do the calculations'
# # 
# # WW_SMI_1999 <- read.csv("data/data_processed/Yield_SMI")
# # names(WW_SMI_1999)
# # head(WW_SMI_1999)
# # dim(WW_SMI_1999)
# # 
# # ## Hier berechne ich den mean von Winterwheat der einzelnen Kreise über die Jahre 1999-2011
# # WW_SMI_1999<-ddply(WW_SMI_1999, .(comId), transform, meanWW_Kreise = mean(winterWheat) )
# # length(WW_SMI_1999$meanWW_Kreise) ## newly created vector has the appropriate length
# # 
# # ## Hier berechne ich die Standardabweichung von Winterwheat der einzelenn Kreise 
# # WW_SMI_1999<-ddply(WW_SMI_1999, .(comId), transform, sdWW_Kreise= sd(winterWheat) )
# # length(WW_SMI_1999$sdWW_Kreise)
# # 
# # ## Hie berechne ich den standard score der winterWheat Daten
# # WW_SMI_1999 <- transform(WW_SMI_1999, scoreWW_Kreise = (winterWheat - meanWW_Kreise)/sdWW_Kreise)
# # length(WW_SMI_1999$scoreWW_Kreise)
# # 
# # ## Detrend WinterWheat z-score Daten der einzelnen Kreise
# # WW_SMI_1999<-ddply(WW_SMI_1999, .(comId), transform, detrendScoreWW_Kreise= detrend(scoreWW_Kreise, "linear") )
# # length(WW_SMI_1999$detrendScoreWW_Kreise)
# 
# # ##############################################################
# # ## Compare the scoreWW_Kreise and the detrendScoreWW_Kreise ##
# # par(mfrow=c(2,1))
# # plot(WW_SMI_1999$scoreWW_Kreise~ WW_SMI_1999$year)
# # fit <- lm(WW_SMI_1999$scoreWW_Kreise~ WW_SMI_1999$year)
# # abline(fit,col=2,lwd=2)
# # abline(a=0, b=0)
# # plot(WW_SMI_1999$detrendScoreWW_Kreise~ WW_SMI_1999$year)
# # fit2 <- lm(WW_SMI_1999$detrendScoreWW_Kreise~ WW_SMI_1999$year)
# # abline(fit2,col=2,lwd=2)
# # abline(a=0, b=0)
# # summary(WW_SMI_1999$detrendScoreWW_Kreise)
# # ## Interpretation ##
# # '
# #   Looks like there is not trend left after detrending the data
# # '
# 
# # ##############################
# # ## Check out new data.frame ##
# # head(WW_SMI_1999, 15) # Order: First all years in comID 1001, then all year in comId 1002, and so on
# # tail(WW_SMI_1999, 15)
# # names(WW_SMI_1999)
# # dim(WW_SMI_1999)
# # ## Interpretation
# # '
# # Looks like the data are okay.
# # '
# 
# #############################################################################################################################################################################################
# ############################################################ Detrending and standardization for all yield Data ##############################################################################
# #############################################################################################################################################################################################
# 'Now I am doing the same procedure for each crop'
# 
# ## Load data.frame ##
# Yield_SMI <- read.csv("data/data_processed/Yield_SMI")
# head(Yield_SMI) # order: First all years of comID 1001, then all of comId 1002, and so on
# 
# #################################################################################
# ## Calculate mean of crops for the communities with help of ddply and colwise  ##
# #################################################################################
# mean<-ddply(Yield_SMI , .(comId), colwise(mean, c("winterWheat", "rye", "winterBarley" , "summerBarley",  "oats" , "triticale", "potatoes" , "sugarBeet", "winterRape", "siloMaize"  )))
# 
# names(mean)<-c("comId", "winterWheat_mean", "rye_mean", "winterBarley_mean" , "summerBarley_mean",  "oats_mean" , "triticale_mean", "potatoes_mean" , "sugarBeet_mean", "winterRape_mean", "siloMaize_mean"  )
# 
# head(mean,  20) 
# dim(mean) # only 410 observation per attribute, because mean aggregates data over the years
# 
# #####################################################################################################################
# ## Because data are now aggregated across the years, I need to repeat new data.frame 12 times and append it rowise ##
# mean2<-rbind(mean, mean, mean, mean, mean, mean, mean, mean, mean, mean, mean, mean)
# head(mean2)
# tail(mean2)
# 
# ########################################################################################
# ## Reorder data.frame for combining it with the year column gained from the Yield_SMI ##
# mean2<-mean2[order(mean2$comId),] # First, all years of comId 1001, that all years of comId 1002, and so on
# head(mean2, 25)
# dim(mean2)
# ## Interpretation
# 'This is the same order now as the Yield_SMI and WW_SMI_1999 data.frames'
# 
# ###############################################################################
# ## Cbind years from Yield_SMI with mean2 data.frame to get the time dimension ##
# head(Yield_SMI , 25) ## First, all years of comId 1001, than all years of comId 1002, and so on
# 
# ## Extract year vector ##
# year<-Yield_SMI$year
# head(year,24)
# tail(year, 24)
# dim(as.data.frame(year))
# 
# #####################################################
# ## Cbind the year vector with the mean2-data.frame ##
# mean3<-cbind(year, mean2)
# dim(mean3)
# head(mean3)
# 
# ########################################################################
# ## Compare created mean3$winterWheat_mean with WW_SMI_1999$meanWW_Kreise
# all(mean3$winterWheat_mean%in%WW_SMI_1999$meanWW_Kreise)
# # Intepretation: both vectors are the same
# 
# ##################################
# ## Calculate standard deviation ##
# ##################################
# sd<-ddply(Yield_SMI , .(comId), colwise(sd, c("winterWheat", "rye", "winterBarley" , "summerBarley",  "oats" , "triticale", "potatoes" , "sugarBeet", "winterRape", "siloMaize"  )))
# head(sd,  20)
# tail(sd)
# names(sd)<-c("comId", "winterWheat_sd", "rye_sd", "winterBarley_sd" , "summerBarley_sd",  "oats_sd" , "triticale_sd", "potatoes_sd" , "sugarBeet_sd", "winterRape_sd", "siloMaize_sd"  )
# dim(sd)
# 
# #####################################################################################################################
# ## Because data are now aggregated across the years, I need to repeat new data.frame 12 times and append it rowise ## 
# sd2<-rbind(sd, sd, sd, sd, sd, sd, sd, sd, sd, sd, sd, sd)
# head(sd2)
# tail(sd2)
# 
# #######################################################################
# ## Bring new data.frame in right order and cbind it with year vector ##
# sd2<-sd2[order(sd2$comId),]
# head(sd2)
# dim(sd2)
# sd3 <- cbind(year, sd2 )
# dim(sd3)
# head(sd3)
# 
# 
# #######################
# ## Calculate z-score ##
# #######################
# 
# ############################################
# ## Extract matrix of original crop values ##
# head(Yield_SMI )
# values<-Yield_SMI [c( "year","comId","winterWheat", "rye", "winterBarley" , "summerBarley",  "oats" , "triticale", "potatoes" , "sugarBeet", "winterRape", "siloMaize"  )]
# head(values)
# dim(values)
# 
# #################################################################
# ## do some simple matrix calculations to calculate the z-score ##
# dim(mean3)%in%dim(sd3)
# dim(sd3)%in%dim(values)
# 
# score = (values -mean3)/sd3 
# names(score)<-c("year","comId", "winterWheat_zscore", "rye_zscore", "winterBarley_zscore" , "summerBarley_zscore",  "oats_zscore" , "triticale_zscore", "potatoes_zscore" , "sugarBeet_zscore", "winterRape_zscore", "siloMaize_zscore"  )
# 
# dim(score)
# head(score)
# 
# ##############################################################################
# ## Compare newlycreate score of winterWheat with training winterWheat score ## 
# all(Yield_SMI $scoreWW_Kreise%in%score$winterWheat_zscore)
# ## Interpretation
# '
# Procedure was succesfull, both scores are the same
# '
# 
# ###################################
# ## Cbind z-score with data.frame ##
# 
# score$year<-NULL
# score$comId<- NULL
# 
# head(score)
# head(Yield_SMI)
# 
# scoreAll_1999<-cbind(Yield_SMI, score)
# dim(scoreAll_1999)
# head(scoreAll_1999, 13)
# names(scoreAll_1999)
# 
# ##################################
# ## Detrend z-score of all crops ##
# ##################################
# 
# detrend_zscore<-ddply(scoreAll_1999, .(comId), colwise(detrend, c("winterWheat_zscore", "rye_zscore", "winterBarley_zscore" , "summerBarley_zscore",  "oats_zscore" , "triticale_zscore", "potatoes_zscore" , "sugarBeet_zscore", "winterRape_zscore", "siloMaize_zscore"  )))
# dim(detrend_zscore)
# head(detrend_zscore)
# names(detrend_zscore)<-c("comId", "winterWheat_deZscore", "rye_deZscore", "winterBarley_deZscore" , "summerBarley_deZscore",  "oats_deZscore" , "triticale_deZscore", "potatoes_deZscore" , "sugarBeet_deZscore", "winterRape_deZscore", "siloMaize_deZscore"  )
# 
# #############################
# ## Cbind detrended z-score ##
# names(detrend_zscore)
# detrend_zscore$comId<-NULL
# 
# scoreAllDe_1999<-cbind(Yield_SMI, detrend_zscore)
# 
# dim(scoreAllDe_1999)
# head(scoreAllDe_1999)
# 
# 
# #############################################################################
# ## Compare results of procedure with only wheat and procedure of all crops ##
# all(WW_SMI_1999$detrendScoreWW_Kreise%in%scoreAllDe_1999$winterWheat_deZscore)
# ## Intepretation
# 'Results are the same!'
# 
# ##########################################################
# ## Compare not detrended z-score with detrended z-score ##
# par(mfrow=c(2,1))
# plot(scoreAll_1999$winterBarley_zscore~scoreAllDe_1999$year)
# fit <- lm(scoreAll_1999$winterBarley_zscore ~ scoreAllDe_1999$year)
# abline(fit,col=2,lwd=2)
# abline(0,0)
# plot(scoreAllDe_1999$winterBarley_deZscore~scoreAllDe_1999$year)
# fit2 <- lm(scoreAllDe_1999$winterBarley_deZscore~scoreAllDe_1999$year)
# abline(fit2,col=2,lwd=2)
# abline(0,0)
# 
# #######################################################
# ## Complete newly created data.frame scoreAllDe_1999 ##
# head(scoreAllDe_1999)
# names(scoreAllDe_1999)
# dim(scoreAllDe_1999)
# scoreAllDe_1999$X.1<-NULL
# scoreAllDe_1999$X <- NULL
# 
# #######################                     
# ## Reorder dataframe ##
# 
# #### Soft gecoded ####
# 
# length(scoreAllDe_1999)
# names(scoreAllDe_1999[c(1:5,16,6:15, 17:43)])
# scoreAllDe_1999 <- scoreAllDe_1999[c(1:5,16,6:15, 17:43)]
# names(scoreAllDe_1999[c(1:16, 34:43,17:33)])
# scoreAllDe_1999 <- scoreAllDe_1999[c(1:16, 34:43,17:33)]
# names(scoreAllDe_1999)
# 
# #####################################################################
# ## Write data.frame with the detrended z-scores of the crop yields ##
# write.csv(scoreAllDe_1999, "data//data_processed/Yield_SMI_detrend")
# 
# 
# #############################################################################################################################################################################################
# ############################################################################# Detrending of absolute crop yield values ######################################################################
# #############################################################################################################################################################################################
# 
# # Detrend WinterWheat Daten der einzelnen Kreise ##
# # Comment: Da detrend mit linear nicht funktioniert, demeane ich die Daten zuerst. Anschließend detrende ich diese dann. 
# # Attention: die detrend funtion scheint nicht wirklich gut für große Daten zu funktionieren. Daher sollte ich es mal per Hand versuchen.
# # Dies ist mir bisher auch nicht gelungen, da es nicht so einfach ist lm innerhalb von ddply zu benutzen
# # Das ist erst relevant, wenn ich den Panelansatz verfolge
# 
# summary(Yield_SMI$winterWheat)
# mean(Yield_SMI$winterWheat, na.rm = T)
# 
# ##########################
# ## Plot absolute values ##
# par(mfrow=c(3,1))
# plot(Yield_SMI$winterWheat~ Yield_SMI $year)
# fit0 <- lm(Yield_SMI$winterWheat~ Yield_SMI $year)
# abline(fit0, col=3)
# abline(mean(Yield_SMI$winterWheat, na.rm= T), 0, col=2)
# # Interpretation
# 'One can see a trend in the data'
# 
# ############################################
# ## Detrend absolute values of winterWheat ##
# Yield_SMI <-ddply(Yield_SMI , .(comId), transform, detrendWW_Kreise = detrend(winterWheat, "linear") )
# summary(Yield_SMI$detrendWW_Kreise) # Das der Mean hier nicht 0 ist, ist ein Zeichen dafür, dass nicht alles Daten durch die detrend funktion 
# plot(Yield_SMI $detrendWW_Kreise ~ Yield_SMI $year)
# fit1 <- lm(Yield_SMI$detrendWW_Kreise ~ Yield_SMI $year)
# abline(fit1)
# abline(0,0, col=2)
# 
# ## Interpretation
# '
# Offensichtlich werden nicht alle Daten detrended. 
# '
# 
# #######################################################
# ## Demean data, i.e. detrend data with constand mean ##
# Yield_SMI <-ddply(Yield_SMI , .(comId), transform, demeanWW_Kreise = detrend(winterWheat, "constant") )
# summary(Yield_SMI $demeanWW_Kreise)
# plot(Yield_SMI $demeanWW_Kreise ~ Yield_SMI $year)
# fit2 <- lm(Yield_SMI$demeanWW_Kreise ~ Yield_SMI $year)
# abline(fit2)
# abline(0,0, col=2)
# ## Interpretation
# '
# Es ist ein leichter Trend zu erkennen
# '
# 
# ###############################
# ## Detrend the demeaned data ##
# Yield_SMI <-ddply(Yield_SMI , .(comId), transform, detrendWW_Kreise= detrend(demeanWW_Kreise, "linear") )
# summary(Yield_SMI $detrendWW_Kreise)
# plot(Yield_SMI $detrendWW_Kreise ~ Yield_SMI $year)#
# fit3 <- lm(Yield_SMI$detrendWW_Kreise ~ Yield_SMI $year)
# abline(fit3)
# abline(0,0, col=2)
# 
# ##################################################
# ## Referenz: detrend unabhängig von den Kreisen ##
# # Comment: Does not work
# Yield_SMI  <- transform(Yield_SMI , demeanWW = detrend(winterWheat, "linear"))
# summary(Yield_SMI$demeanWW)
# plot(Yield_SMI$demeanWW) # no detrending apparent
# fit4 <- lm(Yield_SMI$demeanWW ~ Yield_SMI$year)
# abline(fit4)
# head(Yield_SMI )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
