#### File Description #### 
'
# Matchen und Mergen der Daten aus der Regionalstatistic

# Only use yield data. Do not use any other data since we replicate the approach used in the first paper. 
'

#### Input, Variablen (und Dependencies) ####
'
# Erklärung des Appendix
Num = Anzahl der Betriebe/Number of firms
HA = Landwirtschaftlich genutzte Fläche (Erklärung siehe weiter unten)


#################
# YieldData.csv #
  # GENESIS-Tabelle: 115-46-4;Hektarerträge ausgewählter landwirtschaftlicher Feldfrüchte- Jahressumme - regionale Tiefe: Kreise und krfr. Städte; Erntestatistik; 
  # Hektarerträge (dt/ha); dezitone/hectar = 100 kg/10000 m2
  # Jahre: 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011

  # "year","comId","com","winterWheat","rye","winterBarley","summerBarley","oats","triticale","potatoes","sugarBeet","winterRape","siloMaize"
  # names used in original file: Winterweizen  Roggen und Wintermenggetreide	Wintergerste	Sommergerste	Hafer	Triticale	Kartoffeln	Zuckerrüben	Winterraps	Silomais
  
  # Begriffsinhalt Erntertrag:
      Die Ertragsschätzungen erfolgen von fachkundigen und mit den speziellen Verhältnissen ihres Betriebes bzw. ihres
      Berichtsbezirks gut vertrauten Berichterstatterinnen und Berichterstattern. Als Berichterstatterinnen und
      Berichterstatter sind vielfach Leiterinnen oder Leiter landwirtschaftlicher Betriebe tätig. Bei Getreide,
      Kartoffeln und Raps erfolgen zusätzlich objektive Ertragsmessungen im Rahmen der "Besonderen Ernte- und
      Qualitätsermittlung". Der "Besonderen Ernte- und Qualitätsermittlung" liegt ein mathematisches
      Stichprobenverfahren zu Grunde, das auf die sehr genaue Bestimmung des im Landesdurchschnitt erzielten Ertrags
      ausgerichtet ist; die Messungen erfolgen dabei auf Flächeneinheiten, die mit Hilfe des Stichprobenverfahrens
      repräsentativ ausgewählt wurden. 
      Eine Dezitonne (dt) entspricht 100 kg.
  
  # Achtung bei Barley, da ich was die Flächendaten angeht teilweise nur Gesamtbarley habe
  # goal: dependent variabels for production function
  # important variables: Da die Erträge nicht absolut angeben sind, sondern in Ertrag pro Hektar, kann man die einzelen regionalen Einheiten vergleichen. Darüber hinaus 
    kann man sie wohl als Produktivität in diesem Jahr interpretieren.

  # liefert yieldData

'
#### Output #### 
'
# In data_raw
  # yieldData.csv /.R
  # mydata1.csv / .R
  #  ...
  # mydata11.csv / .R
  # mydata12.csv / .R

# In data_processed
  # regionalstatistic.csv
'

####

#### Pakete ####
library(downloader)
library(plyr)

#### Downloaddate und - herkunft ####
# Die hier verwendeten Daten wurden zwischen dem 2. und 6. Dezember 2013 runtergelanden, wenn nicht explizit anders berichtet.
# https://www.regionalstatistik.de/genesis/online/
# Stand "Thu Sep 11 15:26:59 2014" gibt es auch Daten für 2012, aber SMI gibt es nur bis 2010


######################################################################################################################################################################################

####################
#### Yield Data ####
####################

### Download Yield Data als CSV and rename ###
yieldData <- read.table("./data/data_raw/YieldData.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","winterWheat","rye","winterBarley","summerBarley","oats","triticale","potatoes","sugarBeet","winterRape","siloMaize"),  na.strings=c(".","-"), colClasses=c("integer","factor", "factor", rep("numeric", 10)),nrows=6825, skip=7)

### Explore yieldData ####
head(yieldData)
tail(yieldData)
names(yieldData)
table(yieldData$year) # hier stimmt noch die Anzahl der Kreis pro Jahr
summary(yieldData)

#### Write yield Data ####
write.csv(yieldData, "./data/data_raw/yieldData.csv")

################################################################################################################
#### AA:- 115-01-4- Landwirtschaftliche Betriebe und landwirtschaftlich genutzte Fläche (LF) n. Kulturarten ####
################################################################################################################

## Load Data ##
agArea <- read.table("./data/data_raw/AA_agArea.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","farmNum","farmHa","arableNum","arableHa","permCropNum","permCropHa","grassLandNum","grassLandHa"),  na.strings=c(".","-"), colClasses=c("integer","factor", "factor", rep("numeric", 8)),nrows=2625, skip=9)
# Jahre 2007 2005 2003 2001 1999

## Delete non-necessary files ##
# No need for Dauerkulturen und Dauergrünkulturen
agArea[,"permCropNum"]<-agArea[,"permCropHa"]<-agArea[,"grassLandNum"]<-agArea[,"grassLandHa"]<- NULL
names(agArea)

## Explorative Analyse ##
names(agArea)
head(agArea)
tail(agArea)

sapply(agArea, class)
# str(agArea)
summary(agArea)
unique(agArea$com) 
unique(agArea$year)
length(unique(agArea$com)) 
table(agArea$year)

###############################################################################################################################################################################
#### LZ -HE: - 116-31-4  Landwirtschaftliche Betriebe und deren landwirtschaftlich genutzte Fläche (LF) nach Kulturarten - Jahr - regionale Tiefe: Kreise und krfr. Städte ####
###############################################################################################################################################################################
# ergänzender Datensatz zu AA_agArea: Jahr 2010

# fileUrl9 <- "https://www.regionalstatistik.de/genesis/online/data/116-31-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386265201687&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl9,destfile ="./LZ_agArea.csv")

## Load Data ##
agAreaLZ <- read.table("./data/data_raw/LZ_agArea.csv", header = F, sep=";", dec=",", col.names=c("comId","com","farmNum","farmHa","arableNum","arableHa","permCropNum","permCropHa","grassLandNum","grassLandHa"),  na.strings=c(".","-"), colClasses=c("factor", "factor", rep("numeric", 8)),nrows=524, skip=11)
head(agAreaLZ)
dim(agAreaLZ) # Das Jahr 2010 hat nur 524 Landkreise

## Delete non-necessary files ##
# No need for Dauerkulturen und Dauergrünkulturen
agAreaLZ[,"permCropNum"]<-agAreaLZ[,"permCropHa"]<-agAreaLZ[,"grassLandNum"]<-agAreaLZ[,"grassLandHa"]<- NULL
names(agAreaLZ)


##############################
## Jahresvariable erstellen ##
year <- as.vector(rep(2010, 524))
agAreaLZ$year <-year
sapply(agAreaLZ[1,], class)

#############################################
### agAreaTot: Merge agArea and agAreaLZ ####
names(agArea)
names(agAreaLZ)
## Welche variables of agArea are on which position on agAreaLZ, if any
match(names(agArea), names(agAreaLZ))
## More general, is variable of agArea in agAreaLZ
names(agArea)%in%names(agAreaLZ) # both lists have the same length
## Names used to merge: only names of agArea which are also in agAreaLZ
agAreaTotMergeNames<-names(agArea)[names(agArea)%in%names(agAreaLZ)]

## merge ##
# both have the same length, thats why all=T can be used
# however, it makes no sense to add a variable when there are just values for the year 2010
agAreaTot <-merge (agArea, agAreaLZ, by=agAreaTotMergeNames, all=T)
names(agAreaTot)
length(names(agAreaTot))
head(agAreaTot)
dim(agAreaTot)
# View(agAreaTot[agAreaTot$year==2010,1:11])
table(agAreaTot$year)  ### Jahr 2010 hat nur 524 Beobachtungen

## Write agAreaTot ##
write.csv(agAreaTot, "./data/data_raw/agAreaTot.csv")

################################################
###MyData1: Merge yieldData und agAreaTot ####

## Mergen ##
myData1MergeName <- names(yieldData)[names(yieldData)%in%names(agAreaTot)]
myData1<-merge(yieldData, agAreaTot, by=c(myData1MergeName),all=T , sort=F) 

head(myData1)
names(myData1)

## Check for ComIds per year ##
table(myData1$year) #check
table(myData1$year)==525 #check

table(myData1$comId)
head(myData1[myData1$year==2010,])

# Write myData1
write.csv(myData1,"./data/data_raw/myData1.csv")

######################################################################################
#### Regionalatlas Deutschland: Indikatoren des Themenbereichs 'Gebiet und Fläche'####
######################################################################################

# fileUrl3 = "https://www.regionalstatistik.de/genesis/online/data/AI001.csv;jsessionid=1737194DADA6DE768C7E11D95CAA704F?operation=ergebnistabelleDownload&levelindex=2&levelid=1386257674416&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl3,destfile ="./areaShare.csv")
## Load File ##
areaShare <- read.table("./data/data_raw/areaShare.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","shareResInfra","shareRec","shareAg","shareFor"),  na.strings=c(".","-"), colClasses=c("integer","factor", "factor", rep("numeric", 4)),nrows=3675, skip=6)

## Explore File ##
names(areaShare)
head(areaShare)
tail(areaShare)

## Delete non-necessary files: shareResInfra","shareRec","shareFor" ##

areaShare$shareResInfra<-areaShare$shareRec<-areaShare$shareFor<-NULL
names(areaShare)

unique(areaShare$year) # 1996 2000 2004 2008 2009 2010 2011
table(areaShare$year) #check

####################################
### Merge myData1 and AreaShare ####

## Prepare
names(areaShare)
names(myData1)

myData2MergeNames<-names(myData1)[names(myData1)%in%names(areaShare)]
myData2MergeNames

## Merge 
myData2 <- merge(myData1, areaShare, by=c(myData2MergeNames), all=T)
names(myData2)

## Analyse merge of myData1 and AreaShare
head(myData2)
tail(myData2)

table(myData2$year) # Jahre 1996 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011
table(myData2$year) ==525 #check

head(myData2)


## Write myData2
write.csv(myData2, "data/data_raw/Data2.csv" )



#####################################################################################################################################################################
#### AA -115-02-4- Landwirtschaftliche Betriebe mit Ackerland und deren Ackerfläche nach Fruchtarten - Erhebungsjahr -  regionale Tiefe: Kreise und krfr. Städte ####
#####################################################################################################################################################################
# fileUrl4 <-"https://www.regionalstatistik.de/genesis/online/data/115-02-4.csv;jsessionid=1737194DADA6DE768C7E11D95CAA704F?operation=ergebnistabelleDownload&levelindex=3&levelid=1386257075015&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl4,destfile ="./AA_area.csv")
# Jahre 1999 2003 2007
# Es gibt noch das Jahr 2010

## Load File ##
areaCropType <- read.table("data/data_raw/AA_areaCropType.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","dummy","type","arable", "wheat","rye","winBarley","sumBarley", "oat", "triticale", "grainfarm", "potatoes", "sugarBeet", "rootCropfarm", "siloMaize", "fodderCropfarm", "winRape","industrialCrops") ,  na.strings=c(".","-"), colClasses=c("integer","factor", "factor", "factor", "factor", rep("numeric", 15)),nrows=3150, skip=7)

## Explore File ##
head(areaCropType)
# unique(areaCropType$year)

## delete dummy variable ##
unique(areaCropType$dummy)
names(areaCropType)
areaCropType$dummy <- NULL

## delete unnecessary variables ##
areaCropType$fodderCropfarm<-areaCropType$industrialCrops<-areaCropType$grainfarm<-areaCropType$rootCropfarm<-areaCropType$arable <-NULL
names(areaCropType)

### Make a new data frame, which is wide instead of long for the Anzahl and Hektar(ha)  Charakteristics ##

## Split up the data according to that characteristics ##
areaCropTypeNum <- areaCropType[areaCropType$type == "Anzahl",]
areaCropTypeHa <- areaCropType[areaCropType$type == "ha", ]

head(areaCropTypeNum)
head(areaCropTypeHa)
names(areaCropTypeNum)==names(areaCropTypeHa) # check

## Merge the two newly built dataframes ## 

areaCropType2 <- merge(areaCropTypeNum, areaCropTypeHa, by=c("year","comId", "com"), all=T , suffixes = c("Num","Ha"))
names(areaCropType2)

## delete Type Variables ##
areaCropType2$typeNum <- areaCropType2$typeHa <- NULL     
names(areaCropType2)

## add up winBarley and sumBarley to barley, for both Ha and Num ##
# reason: in LZ there is only Barley, no seperate data

areaCropType2$BarleyNum<-(areaCropType2$winBarleyNum+areaCropType2$sumBarleyNum)
areaCropType2$BarleyHa<-(areaCropType2$winBarleyNum+areaCropType2$sumBarleyNum)

head(areaCropType2)

# because summerbarley and winterBarley both have Relevance I would not delete it.

## Check for ComIds per Year ##
table(areaCropType2$year) # 1999 2003 2007
table(areaCropType2$year) ==525 #check

######################################################################################################################################################
#### LZ-HE 116-42-4  Anbau auf dem Ackerland in landwirtschaftlichen Betrieben nach Fruchtarten - Jahr - regionale Tiefe: Kreise und krfr. Städte ####
######################################################################################################################################################
# fileUrl14 <- "https://www.regionalstatistik.de/genesis/online/data/116-42-4.csv;jsessionid=2742212F42B288847613FD1B208092B3?operation=ergebnistabelleDownload&levelindex=2&levelid=1386267000327&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl14, destfile="./LZ_areaCropType.csv")
# Jahr 2010
# Only has ha figures, no Num figures, but areaCropType2 has both
# 

#### Load file
areaCropTypeLZ<-read.table("data/data_raw/LZ_areaCropType.csv", header = F, sep=";", dec=",", col.names=c("comId","com","arableHa", "cropHa", "wheatHa","winWheat","ryeHa","triticaleHa","barleyHa", "oatHa","maizCornCobMixHA", "otherGrainHa", "PlantsForGreenHarvestingHa","siloMaizeHa", "sugarBeetHa","potatoesHa", "oilseedHA" , "winRapeHa","legumeHa") ,  na.strings=c(".","-"), colClasses=c("factor", "factor",  rep("numeric", 16)),nrows=524, skip=11)
head(areaCropTypeLZ)
## Ad years, because LZ file
areaCropTypeLZ$year <-year

## Explorative Analysis
head(areaCropTypeLZ)
tail(areaCropTypeLZ)
names(areaCropTypeLZ)

## Delete non-necessary variables ##
areaCropTypeLZ$maizCornCobMixHA <-areaCropTypeLZ$otherGrainHa<-areaCropTypeLZ$PlantsForGreenHarvestingHa<-areaCropTypeLZ$oilseedHA<-areaCropTypeLZ$legumeHa<-areaCropTypeLZ$arableHa<-areaCropTypeLZ$winWheat<- NULL
names(areaCropTypeLZ)


###############################################
### Merge areaCropType2 and areaCropTypeLZ ####

## Check for matching variable names
names(areaCropTypeLZ) # less variables than areaCropType2
names(areaCropType2)

## Which variables in areaCropTypeLZ are on which position in areaCropType2
match(names(areaCropTypeLZ),names(areaCropType2))
## More general: Is the variable of areaCropTypeLZ in areaCropType2 anyway
names(areaCropTypeLZ)%in%names(areaCropType2)

## Only consider the variables of areaCropTypeLZ which are also in areaCropType2
areaCropTypefarmMergeNames <- names(areaCropTypeLZ)[names(areaCropTypeLZ)%in%names(areaCropType2)]
areaCropTypefarmMergeNames

## The year 2010 is added
unique(areaCropType2$year)
unique(areaCropTypeLZ$year)

#### Merge ####
areaCropTypefarm <-merge(areaCropType2, areaCropTypeLZ, by = c(areaCropTypefarmMergeNames), all=T)
names(areaCropTypefarm)
table(areaCropTypefarm$year) # 1999 2003 2007 2010 

#### Explore new file areaCropTypefarm ####
head(areaCropTypefarm)
tail(areaCropTypefarm)

## Write areaCropTypefarm

#############################################
#### Merge areaCropTypefarm with myData2 ####

## Preparation of merge
names(myData2)
names(areaCropTypefarm)
## Merge only by comID and year
myData3MergeNames <- names(myData2)[names(myData2)%in%names(areaCropTypefarm)]
myData3MergeNames

myData3 <- merge(myData2, areaCropTypefarm,by=myData3MergeNames, all=T) 

## Check ComIDs per year ##
table(myData2$year) #1996 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011
table(areaCropTypefarm$year) # 1999 2003 2007 2010 
table(myData3$year) # looks not good when all= T in merge function

## Explore new myData3
head(myData3)
names(myData3)
tail(myData3)
myData3[myData3$year==2000]

## Write myData3
write.csv(myData3, "data/data_raw/myData3.csv")



##################################################
#### Da AA_size vorerst nicht berücksichtigt: ####
##################################################
myData4 <- myData3

##############################################################################################################################
#### AA: -115-35-4 -Landwirtschaftliche Betriebe und landwirtschaftlich genutzte Fläche nach der Art der Bewirtschaftung -####
#### Erhebungsjahr -   regionale Tiefe: Kreise und krfr. Städte                                                           ####
##############################################################################################################################

# fileUrl6 <- "https://www.regionalstatistik.de/genesis/online/data/115-35-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386263974958&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl6,destfile ="./AA_organic.csv")
# datedownload6 <- date()

## Load File ##
organic <- read.table("data/data_raw/AA_organic.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","farmNum", "farmHa","organicAgrNum","organicAgrHa") ,  na.strings=c(".","-", "..."), colClasses=c("integer","factor", "factor", rep("numeric", 4)),nrows=2625, skip=9)

## Explore File ##
names(organic) 
head(organic, 15)
unique(organic$year)
## Löschen der farmNum und farmHa Variablen, da diese doppelt sind und meine Beobachtungsanzahl für die Jahr 1999,2003,2007 verändern
organic$farmNum<-organic$farmHa<-NULL

table(organic$year)
length(unique(organic$com))
length(unique(myData4$com))

unique(organic$com)%in%unique(myData4$com)
organic$com%in%myData4$com

length(unique(organic$com)%in%unique(myData4$com))

length(unique(organic$comId))
length(unique(myData4$comId))

unique(organic$comId)%in%unique(myData4$comId)
length(unique(organic$comId)%in%unique(myData4$comId))

##########################################################################################################################################################################################
#### LZ-HE: - 116-34-4  Landwirtschaftliche Betriebe und deren landwirtschaftlich genutzte Fläche (LF) nach Art der Bewirtschaftung - Jahr - regionale Tiefe: Kreise und krfr. Städte ####
##########################################################################################################################################################################################

# fileUrl11 <-"https://www.regionalstatistik.de/genesis/online/data/116-34-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386265887936&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl11,destfile ="./LZ_organic.csv")
# Year 2010
organicLZ <- read.table("data/data_raw/LZ_organic.csv", header = F, sep=";", dec=",", col.names=c("comId","com","organicAgrNum","organicAgrHa", "organicTransFin", "organicTrans", "organicTransNot" ) ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor", rep("numeric", 5)),nrows=524, skip=11)

## Zeitvariable hinzufügen:2010 ##
organicLZ$year <-year

## Löschen der nicht in AA-Datensatz vorhanden Variablen ##
#löschen hat keinen Einfluss auf Doppelung
organicLZ$organicTransFin<-organicLZ$organicTrans<-organicLZ$organicTransNot <-NULL
names(organicLZ)

# Vergleiche organic mit organicLZ
head(organicLZ, 15)
head(organic, 15)

names(organicLZ)
names(organic)
head(organicLZ)
tail(organicLZ)
dim(organicLZ)

#### Merge Organic und OrganicLZ

mergeNamesOrganic <- names(organicLZ)[names(organicLZ)%in%names(organic)]
mergeNamesOrganic

## Merge
organicTot<-merge(organic, organicLZ, by=mergeNamesOrganic, all=T)
table(organicTot$year)
names(organicTot)
head(organicTot)

## Write organicTot
write.csv(organicTot,"data/data_raw/organicTot.csv ")

#### Merge von myData4 und organicTot ohne farmNum bzw Ha und farmAgr
## Vorbereitung
names(myData4)
table(myData4$year) #1996 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 
names(organicTot)
table(organicTot$year) #1999 2001 2003 2005 2007 2010

head(myData4)[,1:3]
head(organicTot)

mergeNamesorganicTot <- names(myData4)[names(myData4)%in%names(organicTot)]
mergeNamesorganicTot

## Merge
myData5<-merge(myData4, organicTot, by=mergeNamesorganicTot, all = T)

## Überprüfen der gemergeden Datei
table(myData5$year) # passt
head(myData5)

## write myData5
write.csv(myData5, "data/data_raw/myData5.csv")

#########################################################################################################
#### Da AA_sgm aus dem laufenden Datesatz genommmen wurde muss ich myData5 in myData6 transformieren ####
#########################################################################################################
myData6<-myData5


####################################################################################################################################################################################################
#### AA: 115-44-4  Landwirtschaftl. Betriebe u. deren landwirtschaftl. genutzte Fläche n. d. betriebswirtschaftl. Ausrichtung-Erhebungsjahr- regionale Tiefe: Kreise und krfr. Städte (ab 2003) ####
####################################################################################################################################################################################################
# fileUrl8 <- "https://www.regionalstatistik.de/genesis/online/data/115-44-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386264657606&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl8,destfile ="./AA_kind.csv")

kind <-read.table("data/data_raw/AA_kind.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","dummy","type", "farmKind", "kindFarm","kindHorti","kindPermanent","kindGrazLivest", "kindRefining", "kindPlant", "kindLivestock", "kindPlantLivestock") ,  na.strings=c(".","-", "..."), colClasses=c("integer","factor", "factor","factor", "factor", rep("numeric", 9)),nrows=2099, skip=8)

## Explore Data
head(kind)
tail(kind)
names(kind)
unique(kind$year)

## Löschen der Dummy Variablen ##
kind$dummy <- NULL

## Delete "kindFarm","kindHorti","kindPermanent","kindGrazLivest", "kindRefining", "kindPlant", "kindLivestock", "kindPlantLivestock, because its not used ##
kind$farmKind<-kind$kindHorti<-kind$kindPermanent<-kind$kindGrazLivest<-kind$kindRefining<-kind$kindPlant<-kind$kindLivestock<-kind$kindPlantLivestock <- NULL
names(kind)

## Reshape dataframe ##
kindNum <- kind[kind$type == "Anzahl",]
kindHa <- kind[kind$type == "ha", ]

kind2 <- merge(kindNum, kindHa, by=c("year","comId", "com"), all=T , suffixes = c("Num","Ha"))

## Delete Type Variables, not necessary anymore
names(kind2)
kind2$typeNum <- kind2$typeHa <- NULL
names(kind2)

### Merge myData6 and kind2
names(myData6)

mergeNamesKind <- names(myData6)[names(myData6)%in%names(kind2)]
mergeNamesKind

names(kind2)
myData7 <- merge(myData6, kind2, by=c(mergeNamesKind), all=T )

head(myData7)
names(myData7)

## Check observations per year
table(myData7$year) # check

write.csv(myData7, "./data/data_raw/myData7.csv")


#########################################################
#### Da own nicht berücksichtigt, myData8 <- myData7 ####
#########################################################
myData8 <- myData7

########################################
#### Da labour nicht berücksichtigt #### 
########################################
myData9 <- myData8

########################################
#### Da legal nicht berücksichtigt #### 
########################################
myData10 <- myData9


######################################################################################################################
#### GB:171-01-4  Gebietsstand: Gebietsfläche in qkm - Stichtag 31.12. - regionale Tiefe: Kreise und krfr. Städte ####
######################################################################################################################
# fileUrl16 <-"https://www.regionalstatistik.de/genesis/online/data/171-01-4.csv;jsessionid=2842FBDFB894D6E54A574D00B5E501A5?operation=ergebnistabelleDownload&levelindex=3&levelid=1386267503980&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl16, destfile="./GB_area.csv")
  # Jahre 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 
  
GbArea <- read.table("./data/data_raw/GB_area.csv", header = F, sep=";", dec=",", col.names=c("date","comId","com","area100ha") ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor", "factor","numeric"),nrows=8924, skip=7)

head(GbArea)
tail(GbArea)

names(GbArea)

table(GbArea$comId)

## Convert date to year variables ##
table(GbArea$date)
GbAreaDate <-GbArea$date

## Switch class to character ##
class(GbAreaDate)
GbAreaDate<-as.character(GbAreaDate)

#### Get rid of 31.12. in variable year ####
year <- as.integer(gsub("31.12.","" ,GbAreaDate) )

table(year)
table(GbArea$date)

## Cbind GbArea and the new created year variable ##
GbArea <-cbind(GbArea, year)

head(GbArea)
tail(GbArea)

table(GbArea$year) #Warum

GbArea[GbArea$year=="2001"|GbArea$year=="2002", c("date", "year")][520:530,]  #scheint zu passen, auch der Übergang von 2001 zu 2002

## Delete GbArea ##
GbArea$date <- NULL

#### Merge GbArea with myData10
names(GbArea)
names(myData10)

mergeNamesmyData10 <- names(myData10)[names(myData10)%in%names(GbArea)]

mergeNamesmyData10

myData11<-merge(myData10, GbArea, by=mergeNamesmyData10, all=T)
names(myData11)

table(myData11$year) #Es sind die Jahre 1995,1997,1998 dazu gekommen, da  sind

write.csv(myData11,"./data/data_processed/myData11.csv ")

###################################################################################################################################
#### 449-01-4 - GB Bodenfläche nach Art der tatsächlichen Nutzung - Stichtag 31.12. - regionale Tiefe: Kreise und krfr. Städte ####
###################################################################################################################################
# fileUrl19<- "https://www.regionalstatistik.de/genesis/online/data/449-01-4.html;jsessionid=C0C161BAC65CF9CD099E8112C0DF5F58?operation=ergebnistabelleDownload&levelindex=1&levelid=1387371272067&option=csv&doDownload=csv&contenttype='csv'"
# Jahre: 1996 2000 2004 2008 2009 2010 2011
GbLanduse<- read.table("./data/data_raw/GB_landuse.csv", header = F, sep=";", dec=",", col.names=c("date","comId","com","areaHa","areaStrucOpensitefarmHa","areaStrucOpensiteResidenceHa","areaStrucOpenesiteBuisnessHa","areaStrucHa", "areaIndustrExclOpenPitminingHa","areaRecrTotHa","areaRecrParkHa","areaCemetryHa","areaInfrastrucTotHa", "areaInfrastrucPublicHa", "areaAgrTotHa","areaAgrMireHa","areaAgrHeathHa","areaForestHa","areaWaterHa", "areaOpenPitminingHa","areaOtherTotHa","areaOtherWasteland") ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor", "factor",rep("numeric", 19)) ,nrows=3675, skip=8)

##  Explore File ##
names(GbLanduse)
head(GbLanduse)
tail(GbLanduse)
table(GbLanduse$date)

## Delete not needed variablens ##
GbLanduse$areaStrucOpensitefarmHa<-GbLanduse$areaStrucOpensiteResidenceHa<-GbLanduse$areaStrucOpenesiteBuisnessHa<-GbLanduse$areaStrucHa<-GbLanduse$areaIndustrExclOpenPitminingHa<-GbLanduse$areaRecrTotHa<-NULL
GbLanduse$areaForestHa<-GbLanduse$areaWaterHa<-GbLanduse$areaOpenPitminingHa<-GbLanduse$areaOtherTotHa<-GbLanduse$areaOtherWasteland<-GbLanduse$areaRecrParkHa<-GbLanduse$areaCemetryHa<-GbLanduse$areaInfrastrucTotHa<-GbLanduse$areaInfrastrucPublicHa<-NULL
names(GbLanduse)

## Bereinige "areaAgrTotHa" von "areaAgrMireHa" (Landwirtscahftsfläche Moore) und "areaAgrHeathHa" (Landwirtschaftsfläche Heide) ##
## mit dem Ziel der Annäherung an farmHa                                                                                         ##

GbLanduse$areaAgrTotHa<-GbLanduse$areaAgrTotHa - GbLanduse$areaAgrMireHa - GbLanduse$areaAgrHeathHa
head(GbLanduse)

GbLanduse$areaAgrMireHa <- GbLanduse$areaAgrHeathHa <- NULL

## Switch date to year  ##
## Nun wir für GbLanduse die Variable year aus der Variable date erstellt
year <- as.integer(gsub("31.12.","" ,GbLanduse$date) )
year[1:100]
table(year)

GbLanduse$year<-year
table(GbLanduse$year)

head(GbLanduse) #Landuse geht mit DG los
GbLanduse[524:527,c("date","year")] #auch der Übergang passt

GbLanduse$date <- NULL

table(GbLanduse$year)

### Problem mit comId, nicht die selbe Dartstellung wie in anderen Datensätzen
### Hier versuche ich nun folgenden Weg: nehme den comId Vektor aus myData11 und kopier in dann in
### Gblanduse und merge anhand dieses Vorganges

#Dafür brauch ich zuerst myData11 und GbLanduse

head(myData11)
head(GbLanduse)
GbLanduse[525:526,]
dim(GbLanduse) #3675 9
#### Change comId Data...
table(GbLanduse$comId)

7*525

## Da myData mit DG endet, GBlanduse aber damit beginnt, muss ich dies berücksichtigen
myData11$comId[525:4199]
head(myData11$comId[525:4199])
tail(myData11$comId[525:4199])
length(myData11$comId[525:4199])  #von derläng ist der Verktor richtig

GbLanduse$comIdFix<-myData11$comId[525:4199]

names(GbLanduse)
head(GbLanduse,20)
tail(GbLanduse, 20)
# Die neu gebildeten comIds stimmen mit den vorhergehenden überein

## Ersetze comId Werte durch comIdFix Wertde
GbLanduse$comId <- GbLanduse$comIdFix
head(GbLanduse) # passt
## Nun kann ich comIDFix löschen
GbLanduse$comIdFix <- NULL

## 
names(myData11)
names(GbLanduse)
mergeNamesMyData12 <- names(myData11)[names(myData11)%in%names(GbLanduse)]
mergeNamesMyData12 

myData12<-merge(myData11, GbLanduse, by=c(mergeNamesMyData12), all=T)

table(myData12$year) # check
table(myData12$year)==525
names(myData12)

## Überprüfen des gebildeten Datensatzes
myData12[1:526,c("comId")]
sum(table(myData12$comId)!="17") 
any(table(myData12$comId)!="17")
all(table(myData12$comId)=="17")
#jede comId wird weiterhin 17 mal aufgeführt

write.csv(myData12,"./data/data_raw/myData12.csv ")

###################################################
#### Finaler Output der Regionalstatistic File ####
###################################################
write.csv(myData12,"./data/data_processed/regionalstatistic.csv ")
#### Write that it fits Luis demands ####
write.table(myData12,"./data/data_processed/regionalstatistic")

myData12<- read.csv2("./data/data_processed/regionalstatistic")

##

#Ersetzten der variablen Namen
colnames(myData12)<-"V"
colnames(myData12)

ncol(myData12)

##Set up vector for colnames
x<-c("a","b","c","d")

for (i in seq_len(ncol(myData12))){
  x[i]<-paste("V",i,sep="")
  append(k,x)
}
x

##Rename Colnames
colnames(myData12) <- x

write.table(myData12,./data/data_processed/myData12.txt ", sep="\t")

installed.packages("foreign")
library(foreign)

write.dta(myData12,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData12 ")


