#### File Description #### 

# Matchen und Mergen der Daten aus der Regionalstatistik
# Umbenennen der Variablen


#### Input, Variablen (und Dependencies) ####
# YieldData.csv : "year","comId","com","winterWheat","rye","winterBarley","summerBarley","oats","triticale","potatoes","sugarBeet","winterRape","siloMaize"
# AA_agarae.csv : "year","comId","com","totalNum","totalHa","farmLandNum","farmLandHa","perCropNum","perCropHa","grassLandNum","grassLandHa"   "grassLandNum" "grassLandHa"


#### Output #### 
# yieldData.csv <-> myData1.R  :

#### Pakete ####
library(downloader)
library(lattice)

#### Downloaddate und - herkunft ####
# Die hier verwendeten Daten wurden zwischen dem 2. und 6. Dezember 2013 runtergelanden, wenn nicht explizit anders berichtet.
# https://www.regionalstatistik.de/genesis/online/
#####################################################################################################################################
#### Yield Data ####
### Download Yield Data als CSV and rename ###
yieldData <- read.table("./data/data_raw/YieldData.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","winterWheat","rye","winterBarley","summerBarley","oats","triticale","potatoes","sugarBeet","winterRape","siloMaize"),  na.strings=c(".","-"), colClasses=c("integer","factor", "factor", rep("numeric", 10)),nrows=6825, skip=7)


#### Write yield Data ####
write.csv(yieldData, "./data/data_processed/yieldData.csv")
dput(yieldData, file = "myData1.R")

### Explore yieldData ####
head(yieldData)
tail(yieldData)
class(yieldData$summerBarley)
names(yieldData)
# 
# is.na(yieldData)
# 
# sapply(yieldData[1,],class)
# 
# str(yieldData)
# 
# summary(yieldData)
# 
# quantile(yieldData$potatoes, na.rm=T)
# 
# unique(yieldData$com)
# 
# length(unique(yieldData$com))
# 
# table(yieldData$com, useNA="ifany")
# 
# yieldData$winterWheat[1:10] 
# 
# any(yieldData$winterWheat < 0)
# 
# any(yieldData$winterWheat > 40)
# 
# all(yieldData$winterWheat > 0, na.rm=T)
# 
# yieldData[yieldData$winterWheat > 90 & yieldData$winterBarley >30, c("winterWheat", "winterBarley")]
# 
# is.na(yieldData$winterWheat)
# 
# sum(is.na(yieldData$winterWheat))
# 
# table(is.na(yieldData$winterWheat))
# 
# plot(yieldData$winterWheat)
# 
# class(yieldData[5,5])
# 
# colSums(yieldData[4:13], na.rm=T, )

#####################################################################################################################################
#### AA:- 115-01-4- Landwirtschaftliche Betriebe und landwirtschaftlich genutzte Fläche (LF) n. Kulturarten ####

# fileUrl2 <- "https://www.regionalstatistik.de/genesis/online/data/115-01-4.csv;jsessionid=4D926869756412A1CFD82786365ECAEE?operation=ergebnistabelleDownload&levelindex=3&levelid=1386238020687&option=csv&doDownload=csv&contenttype='csv'"
# 
# download(fileUrl2, destfile ="./AA_agarae.csv")
# dateDownloaded2 <- date()
agArea <- read.table("./data/data_raw/AA_agArea.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","totalNum","totalHa","farmLandNum","farmLandHa","perCropNum","perCropHa","grassLandNum","grassLandHa"),  na.strings=c(".","-"), colClasses=c("integer","factor", "factor", rep("numeric", 8)),nrows=2625, skip=9)

names(agArea)
head(agArea)
tail(agArea)

sapply(agArea[1,], class)

agArea[,3]

names(agArea)
head(agArea)
str(agArea)
summary(agArea)

unique(agArea$com) #stellt die einzelen Ausprägungen den Variable dar

length(unique(agArea$com)) #beantwortet die Frage, wieviele Ausprägungen es gibt

#### Merge and match of agArea and Yield ####

help(merge)

names(yieldData)
names(agArea)
myData1 <- merge(yieldData, agArea, by=c("year","comId", "com"), all=T)

dim(myData1)
str(myData1)

names(myData1)
head(myData1)

table(myData1$V1)
table(myData1$com, useNA="ifany")

##Sort values

myData1$comId

myData1comIdSort <- as.data.frame( sort(myData1$comId))

myData1comIdSort

unique(myData1comIdSort) # kumuliert die einzelen Ausprägungen 


dput(myData1comIdSort, file="myData1_comId_sort.R " )

getwd()


order(myData1$year, decreasing=T)

myData1_sorted <-myData1[order(myData1$year, decreasing=T),]

row.names(myData1_sorted) <- NULL

myData1_sorted[myData1_sorted$com == "Deutschland",]
#zeigt alle rows, in denen Deutschland zu finden ist

dput(myData1, file = "myData1.R") #

write.csv(myData1,"correlation/data/data_processed/myData1.csv")

#### LZ -HE: - 116-31-4  Landwirtschaftliche Betriebe und deren landwirtschaftlich genutzte Fläche (LF) nach Kulturarten - Jahr - regionale Tiefe: Kreise und krfr. Städte ####


fileUrl9 <- "https://www.regionalstatistik.de/genesis/online/data/116-31-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386265201687&option=csv&doDownload=csv&contenttype='csv'"

# download(fileUrl9,destfile ="./LZ_agArea.csv")
#ergänzender Datensatz zu AA_agArea

agAreaLZ <- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/LZ_agArea.csv", header = F, sep=";", dec=",", col.names=c("comId","com","totalNum","totalHa","farmLandNum","farmLandHa","perCropNum","perCropHa","grassLandNum","grassLandHa"),  na.strings=c(".","-"), colClasses=c("factor", "factor", rep("numeric", 8)),nrows=524, skip=11)

dim(agAreaLZ)

year <- as.vector(rep(2010, 524))

agAreaLZ$year <-year

sapply(agAreaLZ[1,], class)

names(agAreaLZ)



### agAreaTot: Merge agArea and agAreaLZ

names(agArea)
names(agAreaLZ)



agAreaTot <-merge (agArea, agAreaLZ, by=c("comId","com","year","totalNum","totalHa","farmLandNum","farmLandHa","perCropNum","perCropHa","grassLandNum","grassLandHa"), all=T  )
names(agAreaTot)
dim(agAreaTot)

View(agAreaTot[agAreaTot$year==2010,1:11])

write.csv(agAreaTot, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/agAreaTot.csv")

###MyData1LZ: Merge yieldData und agAreaTot



# myData1LZ<-merge(yieldData, agAreaTot, by=c("comId","com","year","totalNum","totalHa","farmLandNum","farmLandHa","perCropNum","perCropHa","grassLandNum","grassLandHa"),all=T ) 
table(yieldData$year)
table(agAreaTot$year)


myData1LZ<-merge(yieldData, agAreaTot, by=c("comId","com","year"),all=T , sort=F) 

names(myData1LZ)
table(myData1LZ$year)
table(myData1$comId)

head(myData1[myData1$year==2010,])
head(myData1LZ[myData1LZ$year==2010,])
#Diese beiden Befehle sollte ich häufiger ausführen


####################################################################################################################################
#### Regionalatlas Deutschland: Indikatoren des Themenbereichs 'Gebiet und Fläche'####
###diese Quelle sollte ich mit der Quelle aus AA: area vergleichen, könnte sein, dass es eine Doppelung gibt  

fileUrl3 = "https://www.regionalstatistik.de/genesis/online/data/AI001.csv;jsessionid=1737194DADA6DE768C7E11D95CAA704F?operation=ergebnistabelleDownload&levelindex=2&levelid=1386257674416&option=csv&doDownload=csv&contenttype='csv'"

# library(downloader)

getwd()

setwd("./correlation/data/data_raw")
getwd()

# download(fileUrl3,destfile ="./areaShare.csv")

list.files()

areaShare <- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/areaShare.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","shareResInfra","shareRec","shareAg","shareFor"),  na.strings=c(".","-"), colClasses=c("integer","factor", "factor", rep("numeric", 4)),nrows=3675, skip=6)

names(areaShare)
head(areaShare)
tail(areaShare)

sapply(areaShare[1,], class)


#### Merge areaShare mit myData1 ####

names(areaShare)
names(myData1)

myData2 <- merge(myData1, areaShare, by=c("year","comId", "com"), all=T)
names(myData2)

head(myData2)
tail(myData2)

write.csv(myData2, "../Data_processed/myData2.csv" )

#### Merge areaShare mit myData1LZ ####

names(areaShare)
names(myData1LZ)

myData2LZ <- merge(myData1LZ, areaShare, by=c("year","comId", "com"), all=T)
names(myData2LZ)

table(myData2LZ$year) #hier kommt noch das Jahr 1996 dazu

head(myData2LZ)
tail(myData2LZ)

####Comparing myData2 and myData2LZ

head(myData2[myData2$year==2010,])
head(myData2LZ[myData2$year==2010,])



write.csv(myData2LZ, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData2LZ.csv")



#####################################################################################################################################
#### AA -115-02-4- Landwirtschaftliche Betriebe mit Ackerland und deren Ackerfläche nach Fruchtarten - Erhebungsjahr -  regionale Tiefe: Kreise und krfr. Städte ####

fileUrl4 <-"https://www.regionalstatistik.de/genesis/online/data/115-02-4.csv;jsessionid=1737194DADA6DE768C7E11D95CAA704F?operation=ergebnistabelleDownload&levelindex=3&levelid=1386257075015&option=csv&doDownload=csv&contenttype='csv'"

library(downloader)

getwd()

setwd("./correlation/data/data_raw")
getwd()

# download(fileUrl4,destfile ="./AA_area.csv")

areaCropType <- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/AA_areaCropType.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","dummy","type","farmland", "wheat","rye","winBarley","sumBarley", "oat", "triticale", "grainTotal", "potatoes", "sugarBeet", "rootCropTotal", "siloMaize", "fodderCropTotal", "winRape","industrialCrops") ,  na.strings=c(".","-"), colClasses=c("integer","factor", "factor", "factor", "factor", rep("numeric", 15)),nrows=3150, skip=7)

#delete dummy variable
areaCropType$dummy
names(areaCropType)
areaCropType$dummy <- NULL
names(areaCropType)

areaCropTypeNum <- areaCropType[areaCropType$type == "Anzahl",]
areaCropTypeHa <- areaCropType[areaCropType$type == "ha", ]

areaCropType2 <- merge(areaCropTypeNum, areaCropTypeHa, by=c("year","comId", "com"), all=T , suffixes = c("Num","Ha"))
names(areaCropType2)

#delete Type Variables

areaCropType2$typeNum <- areaCropType2$typeHa <- NULL     
names(areaCropType2)

#### Merge areaCropType2 with my myData2 ####
names(myData2)

myData3 <- merge(myData2, areaCropType2, by.x=c("year","comId", "com", "farmLandNum", "farmLandHa"), by.y=c("year","comId", "com","farmlandNum","farmlandHa"), all=T)
#da die farmLand und farmland Variablen sich sehr ähnlich sind, habe ich sie Variablen hier differenzierte gemerged als vorher
#hier kommen 30 Variablen dazu, da anhand von 5 variablen gemerged wird und die restlichen 15 Variablen mir 2 unterschiedlichen Appendix versehen werden, also ha und num

head(myData3)
namesdata3<-names(myData3)

names(myData2)%in%names(myData3)
namesdata2<-names(myData2)
namesdata3[names(myData2)%in%names(myData3)]
length(names(myData2)%in%names(myData3)) 

length(namesdata3[names(myData2)%in%names(myData3)])

names(myData3)%in%names(myData2)
length(names(myData3)%in%names(myData2))

tail(myData3)

write.csv(myData3, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData3.csv" )






#### LZ-HE 116-42-4  Anbau auf dem Ackerland in landwirtschaftlichen Betrieben nach Fruchtarten - Jahr - regionale Tiefe: Kreise und krfr. Städte ####
fileUrl14 <- "https://www.regionalstatistik.de/genesis/online/data/116-42-4.csv;jsessionid=2742212F42B288847613FD1B208092B3?operation=ergebnistabelleDownload&levelindex=2&levelid=1386267000327&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl14, destfile="./LZ_areaCropType.csv")

areaCropTypeLZ<-read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/LZ_areaCropType.csv", header = F, sep=";", dec=",", col.names=c("comId","com","farmlandHa", "cropHa", "wheatHa","winWheat","ryeHa","triticaleHa","barleyHa", "oatHa","maizCornCobMixHA", "otherGrainHa", "PlantsForGreenHarvestingHa","siloMaizeHa", "sugarBeetHa","potatoesHa", "oilseedHA" , "winRapeHa","legumeHa") ,  na.strings=c(".","-"), colClasses=c("factor", "factor",  rep("numeric", 16)),nrows=524, skip=11)

areaCropTypeLZ$year <-year

head(areaCropTypeLZ)
tail(areaCropTypeLZ)

#Achtung, dieser Datensatz muss noch bearbeitet werden, bevor er dann gemerged werden kann:

###Merge areaCropType2 and areaCropTypeLZ: areaCropTypeTotal
names(areaCropTypeLZ)
names(areaCropType2)


namesAreaCropTypeLZ <-names(areaCropTypeLZ)
namesAreaCropTypeLZ
namesAreaCropType2 <-names(areaCropType2)
namesAreaCropType2

match(namesAreaCropTypeLZ,namesAreaCropType2)
table(namesAreaCropTypeLZ%in%namesAreaCropType2)

mergeNames <- as.vector(namesAreaCropTypeLZ[namesAreaCropTypeLZ%in%namesAreaCropType2])
#hier wird der Vector erstellt, mit dessen Hilfe ich die beiden Datensätze mergen kann anhand der gleichen
#Variablen Namen, die es in beiden Sätzen gibt.

mergeNames
mergeNames%in%namesAreaCropTypeLZ
table(namesAreaCropTypeLZ%in%mergeNames)
table(namesAreaCropType2%in%mergeNames)
  
  
areaCropTypeTotal <- merge(areaCropType2, areaCropTypeLZ, by = mergeNames, all=T)

table(names(areaCropTypeTotal))


head(areaCropTypeTotal)
tail(areaCropTypeTotal)


write.csv(areaCropTypeTotal,"correlation/data/data_processed/areaCropTypeTotal.csv")

#### Merge areaCropTypeTotal with myData2LZ ####
names(myData2LZ)
names(areaCropTypeTotal)
myData3LZ <- merge(myData2LZ, areaCropTypeTotal,by.x=c("year","comId", "com", "farmLandNum", "farmLandHa"), by.y=c("year","comId", "com","farmlandNum","farmlandHa"), all=T)
#da die farmLand und farmland Variablen sich sehr ähnlich sind, habe ich sie Variablen hier differenzierte gemerged als vorher
##########!!!!!!!!!!!!!!!!!!!!!!!!!################

head(myData3LZ)
names(myData3LZ)
tail(myData3LZ)

write.csv(myData3LZ, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData3LZ.csv" )



####Comparing myData3 and myData3LZ

head(myData3[myData3$year==2010,])
na.exclude(myData3[myData3$year,"farmLandHa"])
head(myData3LZ[myData3$year==2010,])
names(myData3)%in%names(myData3LZ)
length(names(myData3))
length(names(myData3LZ))

#####################################################################################################################################
#### AA - 115-31-4-  Landwirtschaftl. Betriebe der Rechtsform Einzelunternehmen - Erhebungsjahr - regionale Tiefe: Kreise und krfr. Städte ####
## Hab ich vorerst weggelassen , da ich keinen direkten Sinn darin erkenn.
## Variablen: Betriebstyp: Haupt - und Nebenerwerbstbetriebe


#### GENESIS-Tabelle: 115-32-4
# Landwirtschaftliche Betriebe und landwirtschaftlich genutzte Fläche (LF) nach Größenklasse der LF - Erhebungsjahr - regionale Tiefe: Kreise und krfr. Städte Allgemeine Agrarstrukturerhebung;;;;;;;;;;;;;;

fileUrl5 <-"https://www.regionalstatistik.de/genesis/online/data/115-32-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386262531133&option=csv&doDownload=csv&contenttype='csv'"
getwd()

# download(fileUrl5,destfile ="./AA_sizeClass.csv")
# datedownload5 <-date()

sizeClass <- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/AA_sizeClass.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","dummy","type","total", "class2","class5","class10","class20","class30", "class50", "class75", "class100", "classMore") ,  na.strings=c(".","-", "..."), colClasses=c("integer","factor", "factor", "factor", "factor", rep("numeric", 10)),nrows=5250, skip=7)
tail(sizeClass)

#delete dummy 
sizeClass$dummy <- NULL



#Reshape dataframe
sizeClassNum <- sizeClass[sizeClass$type == "Anzahl",]
sizeClassHa <- sizeClass[sizeClass$type == "ha", ]

sizeClass2 <- merge(sizeClassNum, sizeClassHa, by=c("year","comId", "com"), all=T , suffixes = c("Num","Ha"))
names(sizeClass2) ## !! Hier gibt es wieder Variablen, welche gedoppelt sein könnten


#delete type variablen
sizeClass2$typeNum <- sizeClass2$typeHa <- NULL

names(sizeClass2)
names(myData3)
#Variablen totalNum und totalNum scheinen equivalent zu sein, genauso totalHa und totalHa


#### Merge sizeClass2 with my myData3 ####

myData4 <- merge(myData3, sizeClass2, by=c("year","comId", "com", "totalNum", "totalHa"), all=T )
names(myData4)


#Variablen totalNum und totalNum scheinen equivalent zu sein, genauso totalHa und totalHa, deswegen merge ich diese
#Variablen


head(myData4)
tail(myData4)
names(myData4)

write.csv(myData4, "correlation/data/data_processed/myData4.csv" )

#### Merge sizeClass2 with my myData3LZ ####
names(sizeClass2)
table(sizeClass2$year)
names(myData3LZ)
table(myData3LZ$year)
myData4LZ <- merge(myData3LZ, sizeClass2, by=c("year","comId", "com", "totalNum", "totalHa"), all=T )


head(myData4LZ)
tail(myData4LZ)
names(myData4LZ)

write.csv(myData4LZ, "../Data_processed/myData4LZ.csv" )


####################################################################################################################################
#### AA: -115-35-4 -Landwirtschaftliche Betriebe und landwirtschaftlich genutzte Fläche nach der Art der Bewirtschaftung - Erhebungsjahr -   regionale Tiefe: Kreise und krfr. Städte ####

fileUrl6 <- "https://www.regionalstatistik.de/genesis/online/data/115-35-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386263974958&option=csv&doDownload=csv&contenttype='csv'"

# download(fileUrl6,destfile ="./AA_organic.csv")
# datedownload6 <- date()
organic <- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/AA_organic.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","totalAgrNum", "totalAgrHa","organicAgrNum","organicAgrHa") ,  na.strings=c(".","-", "..."), colClasses=c("integer","factor", "factor", rep("numeric", 4)),nrows=2625, skip=9)



names(organic) #totalAgrNum und totalAgrHa könnten hier an anderer Stelle auftauchen ---> notwendig, dass ich vergleiche


#### Merge organic mit myData4 ####

myData5beta<-merge(myData4, organic, by=c("year","comId", "com"), all=T )
names(myData5beta)


#Check for Doubles
names(myData5beta)
myData5beta[1000:1020, c("totalNum","totalAgrNum", "totalAgrHa","totalHa")] 
# Variablen c("totalNum","totalAgrNum") , c("totalAgrHa","totalHa") scheinen jeweils ähnlich zu sein,
# deswegen mergen ich die jeweiligen Variablen aus den unterschiedlichen Datensätzen

myData5<-merge(myData4, organic, by.x=c("year","comId", "com", "totalNum","totalHa"), by.y=c("year","comId", "com","totalAgrNum", "totalAgrHa"), all=T )
names(myData5)




write.csv(myData5, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData5.csv", eol="\n" )

# write.table(myData5,  "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData5.dta", sep="\t")
# library("MASS")
# write.matrix(myData5,  "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData5.dta", sep="\t")
# hat R erstmal abstürzen lassen!!!



#### LZ-HE: - 116-34-4  Landwirtschaftliche Betriebe und deren landwirtschaftlich genutzte Fläche (LF) nach Art der Bewirtschaftung - Jahr - regionale Tiefe: Kreise und krfr. Städte####
fileUrl11 <-"https://www.regionalstatistik.de/genesis/online/data/116-34-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386265887936&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl11,destfile ="./LZ_organic.csv")
##merge

organicLZ <- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/LZ_organic.csv", header = F, sep=";", dec=",", col.names=c("comId","com","organicAgrNum","organicAgrHa", "organicTransFin", "organicTrans", "organicTransNot" ) ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor", rep("numeric", 5)),nrows=524, skip=11)


names(organicLZ)
# organicLZ$organicTransFin<-organicLZ$organicTrans<-organicLZ$organicTransNot <-NULL
#löschen hat keinen Einfluss auf Doppelung
names(organicLZ)
names(organic)
head(organicLZ)
tail(organicLZ)
dim(organicLZ)

organicLZ$year <-year

sapply(organicLZ[1,], class)

names(organicLZ)



#### Merge Organic und OrganicLZ

namesOrganicLZ <-names(organicLZ)
namesOrganicLZ
namesOrganic<-names(organic)
namesOrganic

table(organic$year)
table(organicLZ$year)


mergeNamesOrganic <- as.vector(namesOrganicLZ[namesOrganicLZ%in%namesOrganic])
mergeNamesOrganic

organicTot<-merge(organic, organicLZ, by=mergeNamesOrganic, all=T)
table(organicTot$year)

names(organicTot)
write.csv(organicTot,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/organicTot.csv ")

##Sort values of organicTot by year

# 
# 
# organicTotOrdered<-organicTot[order(organicTot$year, decreasing=F),]
# 
# 


####Merge organicTot und mydata4LZ


# myData5LZ<-merge(myData4LZ, organicTot, by.x=c("year","comId", "com", "totalNum","totalHa"), by.y=c("year","comId", "com","totalAgrNum", "totalAgrHa"), all=T )
# names(myData5LZ)
# write.csv(myData5LZ, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData5LZ.csv")
# 
# table(myData5LZ$year)
# #Versuch, den Table ohne organicLZ$organicTransFin<-organicLZ$organicTrans<-organicLZ$organicTransNot aufzustellen,
# #hat auch nichts funktioniert
# myData5LZ[myData5LZ$year==2010, "year","organicAgrNum"] #hier wurde für organic eine seperate Spalte eingeführt, warum?
# head(myData5[myData5$year==2010,])


# #1. Versuch, Problem zu lösen
# ####Merge organicTotOrdered und mydata4LZ
# names(myData4LZ)
# table(myData4LZ$year)
# names(organicTotOrdered)
# table(organicTotOrdered$year)
# 
# namesMyData4LZ <-names(myData4LZ)
# namesMyData4LZ
# namesorganicTotOrdered<-names(organicTotOrdered)
# namesorganicTotOrdered
# 
# mergeNamesorganicTotOrdered <- as.vector(namesMyData4LZ[namesMyData4LZ%in%namesorganicTotOrdered])
# mergeNamesorganicTotOrdered
# 
# myData5LZOrdered<-merge(myData4LZ, organicTotOrdered, by.x=c("year","comId", "com", "totalNum","totalHa"), by.y=c("year","comId", "com","totalAgrNum", "totalAgrHa"), all=T )
# names(myData5LZOrdered)
# write.csv(myData5LZOrdered, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData5LZ.csv")
# 
# table(myData5LZOrdered$year)


#2.Versuch:  -> Merge von myData4LZ und organicTot ohne totalNum bze Ha und totalAgr

names(myData4LZ)
table(myData4LZ$year)
names(organicTot)
table(organicTot$year)

namesMyData4LZ <-names(myData4LZ)
namesMyData4LZ
namesorganicTot<-names(organicTot)
namesorganicTot

mergeNamesorganicTot <- as.vector(namesMyData4LZ[namesMyData4LZ%in%namesorganicTot])
mergeNamesorganicTot

myData5LZ<-merge(myData4LZ, organicTot, by=mergeNamesorganicTot, all = T)

table(myData5LZ$year)

#Da totalAgrNum und totalAgrHa überflüssig sind, lösche ich diese
myData5LZ$totalAgrNum <-myData5LZ$totalAgrHa <-NULL 
names(myData5LZ)

write.csv(myData5LZ, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData5LZ.csv")
# 
# #schon mal gutes Ergebnis, jetzt merge ich vorher noch die beiden Variablen, die ich eben weggelassen habe
# #und cbind diesen Datenframe an myData5LZordered frame
# myData4LZex<-myData4[,c("year", "totalNum","totalHa" )]
# names(myData4LZ)
# names(myData4LZex)
# head(myData4LZex)
# table(myData4LZex)
# 
# organicTotOrderedEx<-organicTotOrdered[, c("year","totalAgrNum", "totalAgrHa" )]
# names(organicTotOrderedEx)
# 
# #geht leider wieder nicht. Nach visueller Überprüfung von lösche ich daher die Variablen totalAgrNum und totalAgrHa



#merge organicTotOrderedEx and myData4LZEx
myData5LZex<-merge(myData4LZex,organicTotOrderedEx, by.x=c("year", "totalNum","totalHa" ),by.y=c("year","totalAgrNum", "totalAgrHa" ), all=T )
table(myData5LZex$year)

##3. Versuch  den Table ohne organicLZ$organicTransFin<-organicLZ$organicTrans<-organicLZ$organicTransNot aufzustellen,
#hat auch nichts funktioniert
myData5LZ[myData5LZ$year==2010, "year","organicAgrNum"] #hier wurde für organic eine seperate Spalte eingeführt, warum?
head(myData5[myData5$year==2010,])





####################################################################################################################################
#### AA: -115-37-4-  Landwirtschaftliche Betriebe mit Viehhaltung - Stichtag: 03.05 - regionale Tiefe: Kreise und krfr. Städte ####
##Hab ich vorerst weggelassen

#### AA: 15-43-4  Landwirtschaftl. Betriebe u. deren landwirtschaftl. genutzte Fläche n. Größenkl. d. Standarddeckungsbeitrages-Erhebungs- jahr- regionale Tiefe: Kreise und krfr. Städte (ab 2003) ####

fileUrl7 <- "https://www.regionalstatistik.de/genesis/online/data/115-43-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386264337555&option=csv&doDownload=csv&contenttype='csv'"

# download(fileUrl7,destfile ="./AA_ege.csv")

sgm <-read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/AA_sgm.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","dummy","type", "totalSgm", "l2Esu","Esu2to8","Esu8to16","Esu16to24","Esu24to32", "Esu32to40", "Esu40to60", "m60Esu") ,  na.strings=c(".","-", "..."), colClasses=c("integer","factor", "factor","factor", "factor", rep("numeric", 8)),nrows=2098, skip=9)

#Daten scheinbar nur 2003 und 2005 und auch eher unvollständig
tail(sgm)

sgm$dummy <- NULL

names(sgm)

#Reshape dataframe
sgmNum <- sgm[sgm$type == "Anzahl",]
sgmHa <- sgm[sgm$type == "ha", ]

sgm2 <- merge(sgmNum, sgmHa, by=c("year","comId", "com"), all=T , suffixes = c("Num","Ha"))

sgm2$typeNum <- sgm2$typeHa <- NULL
names(sgm2)

###Merge sgm2 and mydata5 ###
names(myData5)
myData5$farmLandNum
names(sgm2)
myData6 <- merge(myData5, sgm2, by=c("year","comId", "com"), all=T )

# head(myData6)
names(myData6)

write.csv(myData6, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData6.csv")

getwd()
setwd("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/")


####Merge myData5LZ und sgm2
names(myData5LZ)
names(sgm2)

myData6LZ <- merge(myData5LZ, sgm2, by=c("year","comId", "com"), all=T )
names(myData6LZ)



write.csv(myData6LZ, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData6LZ.csv")

####################################################################################################################
#Überprüfung, wo sich ein Fehler im Jahr 2010 eingeschlichen hat
table(myData1LZ$year)
table(myData2LZ$year)
table(myData3LZ$year)
table(myData4LZ$year)
table(myData5LZ$year)#->here it gets critical:WHY
table(myData6LZ$year) 
table(myData6$year)
table(myData7LZ$year)
table(myData8LZ$year)
table(myData9LZ$year)
table(myData1LZ$year)
table(myData1LZ$year)

#####################################################################################################################################
#### AA: 115-44-4  Landwirtschaftl. Betriebe u. deren landwirtschaftl. genutzte Fläche n. d. betriebswirtschaftl. Ausrichtung-Erhebungsjahr- regionale Tiefe: Kreise und krfr. Städte (ab 2003)####

fileUrl8 <- "https://www.regionalstatistik.de/genesis/online/data/115-44-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386264657606&option=csv&doDownload=csv&contenttype='csv'"

# download(fileUrl8,destfile ="./AA_kind.csv")

kind <-read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/AA_kind.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","dummy","type", "totalKind", "kindFarm","kindHorti","kindPermanent","kindGrazLivest", "kindRefining", "kindPlant", "kindLivestock", "kindPlantLivestock") ,  na.strings=c(".","-", "..."), colClasses=c("integer","factor", "factor","factor", "factor", rep("numeric", 9)),nrows=2099, skip=8)

tail(kind)
names(kind)
unique(kind$year)

kind$dummy <- NULL

names(kind)

#What about totalKind and totalFarm?????? 

#Reshape dataframe
kindNum <- kind[kind$type == "Anzahl",]
kindHa <- kind[kind$type == "ha", ]

kind2 <- merge(kindNum, kindHa, by=c("year","comId", "com"), all=T , suffixes = c("Num","Ha"))

names(kind2)
kind2$typeNum <- kind2$typeHa <- NULL
names(kind2)


### Merge myData6 and kind2
names(myData6)

names(kind2)
myData7 <- merge(myData6, kind2, by=c("year","comId", "com"), all=T )

head(myData7)
names(myData7)
####Check for doubles: totalHa bzw totalNum , farmlandNum &..Ha, kindFarmNum & ..Ha, 
myData7[myData7$year==c("2003", "2007"), c("year","totalHa", "totalNum", "farmlandNum", "farmlandHa", "kindFarmNum", "kindFarmHa") ] 
# After visuell check there seems to be no doubles


write.csv(myData7, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData7.csv")

getwd()

### Merge myData6lZ and kind2

names(myData6LZ)
names(kind2)

myData7LZ <- merge(myData6LZ, kind2, by=c("year","comId", "com"), all=T )

head(myData7LZ)
names(myData7LZ)
####Check, ob manche Variablen sich ähnlich sind: totalHa bzw totalNum , farmlandNum &..Ha, kindFarmNum & ..Ha, 
myData7LZ[myData7LZ$year==c("2003", "2007"), c("year","totalHa", "totalNum", "farmlandNum", "farmlandHa", "kindFarmNum", "kindFarmHa") ] 
# After visuell check there seems to be no doubles


write.csv(myData7LZ, "~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData7LZ.csv")


####################################################################################################################################
#### LZ-HE: -116-33-4  Landwirtschaftliche Betriebe mit Viehhaltung und Zahl der Tiere - Stichtag - regionale Tiefe: Kreise und krfr. Städte####

# fileUrl10 <-"https://www.regionalstatistik.de/genesis/online/data/116-33-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386265350329&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl10,destfile ="./LZ_cattle.csv")
# datedownload10 <- date()

##Wohl wieder nicht so relevant



#### LZ-HE -116-36-4  Landwirtschaftliche Betriebe und deren landwirtschaftlich genutzte Fläche (LF) nach Eigentums- und Pachtverhältnissen - Jahr - regionale Tiefe: Kreise und krfr. Städte

fileUrl12<-"https://www.regionalstatistik.de/genesis/online/data/116-36-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386266095664&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl12,destfile ="./LZ_own.csv")

#Wenns die Zahlen im Zeitablauf geben würde wäre das überragen für hedonic Pricing


own <- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/LZ_own.csv", header = F, sep=";", dec=",", col.names=c("comId","com","totalNum","totalHa", "ownLsHa", "ownLsNum","ownLsSelfHa","rentLsNum","rentLsUsedHa", "rentLsRentHa", "rentPerHa") ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor",rep("numeric", 9)),nrows=524, skip=11)

tail(own)
names(own)

own$year<-year
names(own)
unique(own$year)

####Merge own mit myData7####

names(own)
names(myData7)
namesMyData7 <-names(myData7)
namesMyData7
namesOwn<-names(own)
namesOwn

mergeNamesMyData7 <- as.vector(namesMyData7[namesMyData7%in%namesOwn])
mergeNamesMyData7

myData8<-merge(myData7, own, by=mergeNamesMyData7, all=T)

names(myData8)
write.csv(myData8,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData8.csv ")


####Merge own with myData7LZ

names(own)
names(myData7LZ)
namesmyData7LZ <-names(myData7LZ)
namesmyData7LZ
namesOwn<-names(own)
namesOwn

mergeNamesmyData7LZ <- as.vector(namesmyData7LZ[namesmyData7LZ%in%namesOwn])
mergeNamesmyData7LZ

myData8LZ<-merge(myData7LZ, own, by=mergeNamesmyData7LZ, all=T)

names(myData8LZ)
write.csv(myData8LZ,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData8LZ.csv ")



####  LZ -He - 116-37-4  Landwirtschaftliche Betriebe nach Rechtsform und sozialökonomische Betriebstypen - Jahr - regionale Tiefe: Kreise und krfr. Städte####

fileUrl13 <- "https://www.regionalstatistik.de/genesis/online/data/116-37-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386266185328&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl13, destfile="./LZ_legalForm.csv")
### bisher nichts zum mergen gefunden


legal <- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/LZ_legalForm.csv", header = F, sep=";", dec=",", col.names=c("comId","com","legatTotalNum","legalPartner", "legalEntity", "legalProprietorship","legalProprietorshipMain","legalProprietorshipSide") ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor",rep("numeric", 6)),nrows=524, skip=11)

tail(legal)

legal$year <- year

names(legal)

####Merge legal with myData8LZ

names(legal)
names(myData8LZ)
namesmyData8LZ <-names(myData8LZ)
namesmyData8LZ
nameslegal<-names(legal)
nameslegal

mergeNamesmyData8LZ <- as.vector(namesmyData8LZ[namesmyData8LZ%in%nameslegal])


mergeNamesmyData8LZ

sapply(legal[1,],class)


myData9LZ<-merge(myData8LZ, legal, by=mergeNamesmyData8LZ, all=T)



names(myData9LZ)
write.csv(myData9LZ,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData9LZ.csv ")



#### LZ -HE 116-39-4  Landwirtschaftliche Betriebe mit Hofnachfolge - Jahr - regionale Tiefe: Kreise und krfr. Städte ####
#Als nicht so wichtig erachtet



#### LZ -HE 116-48-4  Arbeitskräfte und deren Arbeitsleistung in landwirtschaftlichen Betrieben - Jahr - regionale Tiefe: Kreise und krfr. Städte ####

fileUrl15 <-"https://www.regionalstatistik.de/genesis/online/data/116-48-4.csv;jsessionid=2842FBDFB894D6E54A574D00B5E501A5?operation=ergebnistabelleDownload&levelindex=3&levelid=1386267176228&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl15, destfile="./LZ_labour.csv")

labour<- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/LZ_labour.csv", header = F, sep=";", dec=",", col.names=c("comId","com","legatTotalNum","legalPartner", "legalEntity", "legalProprietorship","legalProprietorshipMain","legalProprietorshipSide") ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor",rep("numeric", 6)),nrows=524, skip=11)
head(myData9LZ[myData9LZ$year==2010,"totalHa"])

tail(labour)

labour$year <- year

names(labour)

####Merge labour with myData9LZ

names(labour)
names(myData9LZ)
namesmyData9LZ <-names(myData9LZ)
namesmyData9LZ
nameslabour<-names(labour)
nameslabour

mergeNamesmyData9LZ <- as.vector(namesmyData9LZ[namesmyData9LZ%in%nameslabour])


mergeNamesmyData9LZ

sapply(labour[1,],class)


myData10LZ<-merge(myData9LZ, labour, by=mergeNamesmyData9LZ, all=T)



names(myData10LZ)
write.csv(myData10LZ,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData9LZ.csv ")

#### GB:171-01-4  Gebietsstand: Gebietsfläche in qkm - Stichtag 31.12. - regionale Tiefe: Kreise und krfr. Städte####

fileUrl16 <-"https://www.regionalstatistik.de/genesis/online/data/171-01-4.csv;jsessionid=2842FBDFB894D6E54A574D00B5E501A5?operation=ergebnistabelleDownload&levelindex=3&levelid=1386267503980&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl16, destfile="./GB_area.csv")


#### GB:171-01-4  Gebietsstand: Gebietsfläche in qkm - Stichtag 31.12. - regionale Tiefe: Kreise und krfr. Städte####

fileUrl16 <-"https://www.regionalstatistik.de/genesis/online/data/171-01-4.csv;jsessionid=2842FBDFB894D6E54A574D00B5E501A5?operation=ergebnistabelleDownload&levelindex=3&levelid=1386267503980&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl16, destfile="./GB_area.csv")

GbArea<- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/GB_area.csv", header = F, sep=";", dec=",", col.names=c("date","comId","com","comArea100ha") ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor", "factor","numeric"),nrows=8924, skip=7)


tail(GbArea)

names(GbArea)

#Get rid of 31.12. in variable year

head(GbArea)

GbAreaDate <-GbArea$date
head(GbAreaDate)
class(GbAreaDate)
GbAreaDate<-as.character(GbAreaDate)

GbAreaDate[1:100]
year <- as.integer(gsub("31.12.","" ,GbAreaDate) )

head(year)
head(GbArea)

GbAreaTot <-cbind(GbArea, year)
head(GbAreaTot)
tail(GbAreaTot)
summary(GbAreaTot$year)
unique(GbAreaTot$year)
table(GbAreaTot$year)#Warum
table(GbAreaTot$date, GbArea$year) # Das gleiche gilt immerhin auch für date, scheint eventuell ein Fehler beim einlesen zu sein oder im Datensatz
GbAreaTot[GbAreaTot$year=="2001"|GbAreaTot$year=="2002", c("date", "year")] #scheint zu passen, auch der Übergang von 2001 zu 2002



####Merge GbAreaTot with myData9LZ

names(GbAreaTot)
head(GbAreaTot)
names(myData9LZ)
namesmyData9LZ <-names(myData9LZ)
namesmyData9LZ
namesGbArea<-names(GbAreaTot)
namesGbArea

mergeNamesmyData9LZ <- as.vector(namesmyData9LZ[namesmyData9LZ%in%namesGbArea])

mergeNamesmyData9LZ

sapply(GbAreaTot[1,],class)
class(myData9LZ$year)

myData10LZ<-merge(myData9LZ, GbAreaTot, by=mergeNamesmyData9LZ, all=T)
head(myData10LZ)


table(myData10LZ$year)
table(myData9LZ$year) #Es sind die Jahre 1995,1997,1998 dazu gekommen, da  sind
3*525

#1575 Variablen, die Differenz passt entsprechend:
8925-1575
#7350


myData10LZ$comId[1:20]

names(myData10LZ)

write.csv(myData10LZ,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData10LZ.csv ")


#### 449-01-4 - GB Bodenfläche nach Art der tatsächlichen Nutzung - Stichtag 31.12. - regionale Tiefe: Kreise und krfr. Städte ####
#### 




fileUrl19<- "https://www.regionalstatistik.de/genesis/online/data/449-01-4.html;jsessionid=C0C161BAC65CF9CD099E8112C0DF5F58?operation=ergebnistabelleDownload&levelindex=1&levelid=1387371272067&option=csv&doDownload=csv&contenttype='csv'"

# library(downloader)
# download(fileUrl19, destfile="~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/GB_landuse.csv")
# dateUrl19 <-date()
# dateUrl19
# "Wed Dec 18 13:58:08 2013"
getwd()
setwd("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/")
ls()

GbLanduse<- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/GB_landuse.csv", header = F, sep=";", dec=",", col.names=c("date","comId","com","comAreaHa","comAreaStrucOpensiteTotalHa","comAreaStrucOpensiteResidenceHa","comAreaStrucOpenesiteBuisnessHa","comAreaStrucHa", "comAreaIndustrExclOpenPitminingHa","comAreaRecrTotHa","comAreaRecrParkHa","comAreaCemetryHa","comAreaInfrastrucTotHa", "comAreaInfrastrucPublicHa", "comAreaAgrTotHa","comAreaAgrMireHa","comAreaAgrHeathHa","comAreaForestHa","comAreaWaterHa", "comAreaOpenPitminingHa","comAreaOtherTotHa","comAreaOtherWasteland") ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor", "factor",rep("numeric", 19)) ,nrows=3675, skip=8)

names(GbLanduse)
head(GbLanduse)
tail(GbLanduse)


###Hier versuche ich nun folgenden Weg: nehme den comId Vektor aus myData10Lz und kopier in dann in
### Gblanduse und merge anhand diesen Vorgang

#Dafür brauch ich zuerst myData10LZ und GbLanduse

head(myData10LZ)
head(GbLanduse)
dim(GbLanduse) #3675 23


#Nun wir für GbLanduse die Variable year aus der Variable date erstellt

year <- as.integer(gsub("31.12.","" ,GbLanduse$date) )
year[1:100]
table(year)

GbLanduse$year<-year

head(GbLanduse) #Landuse geht mit DG los
GbLanduse[500:550,c("date","year")] #auch der Übergang passt




####Hier wird  nun der ComID vektor extrahiert
3675+524

head(myData10LZ$comId[525:4199])
tail(myData10LZ$comId[525:4199])
length(myData10LZ$comId[525:4199])  #von derläng ist der Verktor richtig

GbLanduse$comIdFix<-myData10LZ$comId[525:4199]

names(GbLanduse)
GbLanduse$comIdFix[1:50]
GbLanduse$year

####Merge myData10LZ mit GbLanduse by comIdFix####

##get rid of some variables: com, ComID, date
names(GbLanduse)


GbLanduse[500:550,c("comId","comIdFix")]

GbLanduse$com <-GbLanduse$comId<- GbLanduse$date <- NULL

names(GbLanduse)
myData10LZ$date

##

myData11LZ<-merge(myData10LZ, GbLanduse, by.x=c("year","comId" ), by.y=c("year","comIdFix"), all=T)


names(myData11LZ)
#Observations did not change, very good
#order seems also to be okay

myData11LZ[1:526,c("comId")]
sum(table(myData11LZ$comId)!="17") #jede comId wird weiterhin 17 mal aufgeführt
any(table(myData11LZ$comId)!="17")
all(table(myData11LZ$comId)=="17")


write.csv(myData11LZ,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData11LZ.csv ")


###Write that it fits Luis demands
write.table(myData11LZ,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData11LZ ")

#Ersetzten der variablen Namen
colnames(myData11LZ)<-"V"
colnames(myData11LZ)

ncol(myData11LZ)

##Set up vector for colnames
x<-c("a","b","c","d")

for (i in seq_len(ncol(myData11LZ))){
  x[i]<-paste("V",i,sep="")
  append(k,x)
}
x

##Rename Colnames
colnames(myData11LZ) <- x

write.table(myData11LZ,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData11LZ.txt ", sep="\t")

installed.packages("foreign")
library(foreign)

write.dta(myData11LZ,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData11LZ ")



#### GB: 171-01-5  Gebietsstand: Gebietsfläche in qkm - Stichtag 31.12. - regionale Tiefe: Gemeinden, Samt-/Verbandsgemeinden #### 
fileUrl17 <-"https://www.regionalstatistik.de/genesis/online/data/171-01-5.csv;jsessionid=2842FBDFB894D6E54A574D00B5E501A5?operation=ergebnistabelleDownload&levelindex=3&levelid=1386267777202&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl17, destfile="./GB_comArea.csv")
#spatial resolution to high






#### 449-01-5 Bodenfläche nach Art der tatsächlichen Nutzung regionale Tiefe: Gemeinden, Samt-/Verbandsgemeinden ####
#Auftrag muss noch erstellt werden

fileUrl20 <- 

