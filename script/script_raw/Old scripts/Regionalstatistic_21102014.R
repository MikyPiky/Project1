#### File Description #### 
'
# Matchen und Mergen der Daten aus der Regionalstatistic
# Umbenennen der Variablen
# When merging AA based files and LZ files, i use the Argument all.x, wheres x is derives from AA to avoid that variables are added which only have values for the year 2010
# Because of that, there are suppossed to be less variables in the data.set
# I will stop to consider the myData[..] files without LZ appendix because those files are just missing the year 2010
'

#### Input, Variablen (und Dependencies) ####
'
# YieldData.csv 
  #: "year","comId","com","winterWheat","rye","winterBarley","summerBarley","oats","triticale","potatoes","sugarBeet","winterRape","siloMaize"

# AA_agarae.csv 
  # (Landwirtschaftliche Betriebe und landwirtschaftlich genutzte Fläche (LF) n. Kulturarten
  #  2007 2005 2003 2001 1999 
  # "year","comId","com","totalNum","totalHa","farmlandNum","farmlandHa","perCropNum","perCropHa","grassLandNum","grassLandHa"   "grassLandNum" "grassLandHa"
  # ergänzender Datensatz zu AA_agArea: Jahr 2010 AA_agaraeLZ.csv: LZ -HE: - 116-31-4  Landwirtschaftliche Betriebe und deren landwirtschaftlich genutzte Fläche (LF) nach Kulturarten - Jahr - regionale Tiefe: Kreise und krfr. Städte ####

# areaShare.csv
  # 1996 2000 2004 2008 2009 2010 2011
  # (Regionalatlas Deutschland: Indikatoren des Themenbereichs Gebiet und Fläche)
  # "year","comId","com","shareResInfra","shareRec","shareAg","shareFor"

# AA_areaCropType.csv 
  # AA -115-02-4- Landwirtschaftliche Betriebe mit Ackerland und deren Ackerfläche nach Fruchtarten - Erhebungsjahr -  regionale Tiefe: Kreise und krfr. Städte ####
  # 2007 2003 1999  
  # "year","comId","com","dummy","type","farmland", "wheat","rye","winBarley","sumBarley", "oat", "triticale", "grainTotal", "potatoes", "sugarBeet", 
  # "rootCropTotal", "siloMaize", "fodderCropTotal", "winRape","industrialCrops"
  # dabei jeweils Anzahl und Hektar
    # weiter file aus Landwirtschafszählung: Jahr 2010 LZ_areaCropType.csv

# AA_sizeClass.csv
  # 2007 2005 2003 2001 1999
  # "year","comId","com","dummy","type","total", "class2","class5","class10","class20","class30", "class50", "class75", "class100", "classMore") ,  na.strings=c(".","-", "..."), colClasses=c("integer","factor", "factor", "factor", "factor", rep("numeric", 10)),nrows=5250, skip=7)
  # Landwirtschaftliche Betriebe und landwirtschaftlich genutzte Fläche (LF) nach Größenklasse der LF - Erhebungsjahr - regionale Tiefe: Kreise und krfr. Städte Allgemeine Agrarstrukturerhebung;;;;;;;;;;;;;;
  
# AA_organic.csv
  # 2007 2005 2003 2001 1999
  # "year","comId","com","totalAgrNum", "totalHa","organicAgrNum","organicAgrHa"
  # AA: -115-35-4 -Landwirtschaftliche Betriebe und landwirtschaftlich genutzte Fläche nach der Art der Bewirtschaftung - Erhebungsjahr -   regionale Tiefe: Kreise und krfr. Städte ####
  # Year 2010: LZ_organic.csv
    # auch weiter Variblen: "comId","com","organicAgrNum","organicAgrHa", "organicTransFin", "organicTrans", "organicTransNot" 
    # # organicLZ$organicTransFin<-organicLZ$organicTrans<-organicLZ$organicTransNot <-NULL
'
#### Output #### 
'
# yieldData.csv /.R
# mydata1.csv / .R
# mydata1LZ.csv / .R
# ...
# mydata11.csv / .R
# mydata11LZ.csv / .R
'

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

#### Write yield Data ####
write.csv(yieldData, "./data/data_processed/yieldData.csv")
write.csv(yieldData, "./data/data_raw/yieldData.csv")

################################################################################################################
#### AA:- 115-01-4- Landwirtschaftliche Betriebe und landwirtschaftlich genutzte Fläche (LF) n. Kulturarten ####
################################################################################################################

## Load Data ##
agArea <- read.table("./data/data_raw/AA_agArea.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","totalNum","totalHa","farmlandNum","farmlandHa","perCropNum","perCropHa","grassLandNum","grassLandHa"),  na.strings=c(".","-"), colClasses=c("integer","factor", "factor", rep("numeric", 8)),nrows=2625, skip=9)
# Jahre 2007 2005 2003 2001 1999
## Explorative Analyse ##
names(agArea)
head(agArea)
tail(agArea)

sapply(agArea, class)
str(agArea)
summary(agArea)
unique(agArea$com) 
unique(agArea$year)
length(unique(agArea$com)) 

#############################################
#### Merge and match of agArea and Yield ####
names(yieldData)
names(agArea)

## merge ##
myData1MergeNames <- names(yieldData)[names(yieldData)%in%names(agArea)]
myData1 <- merge(yieldData, agArea, by=c(myData1MergeNames), all=T)
# no LZ data yet

dim(myData1)
str(myData1)
names(myData1)
head(myData1)

table(myData1$V1)
table(myData1$com, useNA="ifany")
table(myData1$year) # check
all(table(myData1$year)==525) # check

#### Write myData1
write.csv(myData1,"./data/data_raw/myData1.csv")

###############################################################################################################################################################################
#### LZ -HE: - 116-31-4  Landwirtschaftliche Betriebe und deren landwirtschaftlich genutzte Fläche (LF) nach Kulturarten - Jahr - regionale Tiefe: Kreise und krfr. Städte ####
###############################################################################################################################################################################
# ergänzender Datensatz zu AA_agArea: Jahr 2010

# fileUrl9 <- "https://www.regionalstatistik.de/genesis/online/data/116-31-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386265201687&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl9,destfile ="./LZ_agArea.csv")

## Load Data ##
agAreaLZ <- read.table("./data/data_raw/LZ_agArea.csv", header = F, sep=";", dec=",", col.names=c("comId","com","totalNum","totalHa","farmlandNum","farmlandHa","perCropNum","perCropHa","grassLandNum","grassLandHa"),  na.strings=c(".","-"), colClasses=c("factor", "factor", rep("numeric", 8)),nrows=524, skip=11)
head(agAreaLZ)
dim(agAreaLZ) # Das Jahr 2010 hat nur 524 Landkreise


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
agAreaTot <-merge (agArea, agAreaLZ, by=agAreaTotMergeNames, all=T  )
names(agAreaTot)
length(names(agAreaTot))
head(agAreaTot)
dim(agAreaTot)
View(agAreaTot[agAreaTot$year==2010,1:11])
table(agAreaTot$year)  ### Jahr 2010 hat nur 524 Beobachtungen

## Write agAreaTot ##
write.csv(agAreaTot, "./data/data_raw/agAreaTot.csv")

################################################
###MyData1LZ: Merge yieldData und agAreaTot ####

## Mergen ##
myData1LZMergeName <- names(yieldData)[names(yieldData)%in%names(agAreaTot)]
myData1LZ<-merge(yieldData, agAreaTot, by=c(myData1LZMergeName),all=T , sort=F) 

head(myData1LZ)
names(myData1LZ)

## Check for ComIds per year ##
table(myData1LZ$year) #check
table(myData1LZ$year)==525 #check

table(myData1$comId)

head(myData1[myData1$year==2010,])
head(myData1LZ[myData1LZ$year==2010,])

# Write myData1LZ
write.csv(myData1,"./data/data_rawmyData1LZ.csv")

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

sapply(areaShare[1,], class)
unique(areaShare$year) # 1996 2000 2004 2008 2009 2010 2011

#####################################
#### Merge myData1 and AreaShare ####
## Frage, ob dieser Schritt überhaupt notwendig ist, da durch LZ das Jahr 2010 hinzufefügt wird, dieses aber bei Daten ohne LZ fehlt
## 

# ## Prepare
# names(areaShare)
# names(myData1)
# 
# myData2MergeNames<-names(myData1)[names(myData1)%in%names(areaShare)]
# 
# ## Merge 
# myData2 <- merge(myData1, areaShare, by=c(myData2MergeNames), all=T)
# names(myData2)
# 
# ## Analyse merge of myData1 and AreaShare
# head(myData2)
# tail(myData2)
# 
# table(myData2$year)
# table(myData2$year) ==525 #check
# 
# ## Write myData2
# write.csv(myData2, "data/data_raw/Data2.csv" )

#################################################
#### myData2LZ: Merge myData1LZ mit areaShare####
names(areaShare)
names(myData1LZ)

## Variables, which are in both files ##
myData2LZMergeNames<-names(myData1LZ)[names(myData1LZ)%in%names(areaShare)]

## Merge ##
myData2LZ <- merge(myData1LZ, areaShare, by=c(myData2LZMergeNames), all=T)
names(myData2LZ)

## Check for ComIds per year ##
table(myData2LZ$year)==525 #hier kommt noch das Jahr 1996 dazu, check

## Explore myData2LZ #
head(myData2LZ)
tail(myData2LZ)

# #### Comparing myData2 and myData2LZ
# names(myData2)%in%names(myData2LZ)
# ## Only compare year 2010 ##
# head(myData2[myData2$year==2010,])==head(myData2LZ[myData2$year==2010,])
# head(myData2[myData2$year==2010,])%in%head(myData2LZ[myData2$year==2010,])

## Write myData2LZ ##
write.csv(myData2LZ, "data/data_processed/myData2LZ.csv")

#####################################################################################################################################################################
#### AA -115-02-4- Landwirtschaftliche Betriebe mit Ackerland und deren Ackerfläche nach Fruchtarten - Erhebungsjahr -  regionale Tiefe: Kreise und krfr. Städte ####
#####################################################################################################################################################################
# fileUrl4 <-"https://www.regionalstatistik.de/genesis/online/data/115-02-4.csv;jsessionid=1737194DADA6DE768C7E11D95CAA704F?operation=ergebnistabelleDownload&levelindex=3&levelid=1386257075015&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl4,destfile ="./AA_area.csv")
# Jahre 1999 2003 2007
# Es gibt noch das Jahr 2010

## Load File ##
areaCropType <- read.table("data/data_raw/AA_areaCropType.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","dummy","type","farmland", "wheat","rye","winBarley","sumBarley", "oat", "triticale", "grainTotal", "potatoes", "sugarBeet", "rootCropTotal", "siloMaize", "fodderCropTotal", "winRape","industrialCrops") ,  na.strings=c(".","-"), colClasses=c("integer","factor", "factor", "factor", "factor", rep("numeric", 15)),nrows=3150, skip=7)

## Explore File ##
head(areaCropType)
unique(areaCropType$year)

## delete dummy variable ##
unique(areaCropType$dummy)
names(areaCropType)
areaCropType$dummy <- NULL

### Make a new data frame, which is wide instead of long for the Anzahl and Hektar(ha)  Charakteristics ##
## Split up the data according to that characteristics ##
areaCropTypeNum <- areaCropType[areaCropType$type == "Anzahl",]
areaCropTypeHa <- areaCropType[areaCropType$type == "ha", ]

head(areaCropTypeNum)
head(areaCropTypeHa)

## Merge the two newly built dataframes ## 
areaCropType2 <- merge(areaCropTypeNum, areaCropTypeHa, by=c("year","comId", "com"), all=T , suffixes = c("Num","Ha"))
names(areaCropType2)

## delete Type Variables ##
areaCropType2$typeNum <- areaCropType2$typeHa <- NULL     
names(areaCropType2)

## Check for ComIds per Year ##
table(areaCropType2$year) 
table(areaCropType2$year) ==525 #check

###############################################
#### Merge areaCropType2 with my myData2LZ ####
# #### Vorher sollte ich das Jahr 2010 hinzufügen an areaCropType2 ####
# ## Prepare
# ## check whether mergening variables match ##
# ## ... ComID ##
# match(myData2LZ$comId, areaCropType2$comId)
# all(myData2LZ$comId%in%areaCropType2$comId ==T) # check
# 
# ## ... Com ##
# match(myData2LZ$com, areaCropType2$com)
# all(myData2LZ$com%in%areaCropType2$com ==T) # check, be cautinous, because it makes later problems with the merging command --> double rows
# 
# ## .. Year ##
# unique(myData2LZ$year)
# unique(areaCropType2$year)
# match(myData2LZ$year, areaCropType2$year)
# all(myData2LZ$year%in%areaCropType2$year ==T) # no chek, macht aber Sinn, da sehr unterschiedliche Jahre berücksichtigt werden
# 
# ## Compare names ##
# names(myData2LZ)
# names(areaCropType2)
# 
# ## Compare oservations in each year ## 
# table(myData2LZ$year) ==525 #check
# table(areaCropType2$year) ==525 #check
# 
# ## MergeNames ##
# myData3MergeNames <- names(myData2LZ)[names(myData2LZ)%in%names(areaCropType2)]
# 
# 
# ### Merge ohne com Variable als mergeName ##
# myData3LZ <- merge(myData2LZ, areaCropType2, by=myData3MergeNames ) 
# names(myData3LZ)
# 
# ## Eplorative Analysis of merge ##
# table(myData3LZ$year)
# table(myData3LZ$year) ==525
# all(table(myData3LZ$year) ==525) #check
# 
# ### Alternative plyr::join statt merge
# myData3Join <- join(myData2LZ, areaCropType2, by=c(myData3MergeNames), type="left")
# table(myData2LZ$year)
# table(myData3Join$year)
# names(myData3Join)
# 
# ### Vergleich von myData3 and myData3join
# myData3%in%myData3Join
# #######################################################################################################################
# # Join produziert andere Ergebisse als merge ohne com, aber join mit type="full" die gleichen wie merge mit com und 
# # Visuelle Inspektion hat ergeben, dass join mit type="left" NAs produziert, wohingegen durch merge ohne die Variable com 
# # diese Beobachtungeswerte berücksichtigt wurden. 
# ########################################################################################################################
# table(myData3$year) ==525
# all(table(myData3$year) ==525) # check
# 
# ### Alternative merge mit Com Variable
# myData3MergeNames <- merge(myData2LZ, areaCropType2, by=c(myData3MergeNames), all=T) 
# table(myData3MergeNames$year)
# ##########################################################################################################################
# #  Hier gibt es nun Doppelungen, obwohl man bei visueller keine Unterschiede erkennen kann bei den com Variablen. Beispiel
# #  sind Landkreise in Mecklenburg Vorpommern (Parchim in Zeile 937)
# ##########################################################################################################################
# 
# ## Check, whether class of com are differen
# sapply(myData2LZ, class)
# sapply(areaCropType2, class)
# ###########################################################################################################################
# # Gleich Klassen für com in den beiden Datensätzen #
# ###########################################################################################################################
# 
# ### Alternative join mit type="full" and no com Variable
# myData3MergeNamesJoin <-join(myData2LZ, areaCropType2, by=c("year","comId"), type="full") 
# table(myData3MergeNamesJoin$year)
# ###########################################################################################################################
# # Keine NAS in der Variablen: der Fehler scheint bei der MergeVariable com zu liegen #
# ###########################################################################################################################
# 
# ## Write myData3 (aus merge ohne com)
# write.csv(myData3, "data/data_raw/myData3.csv" )
# 
# ## Write myData3Join (type="left")
# write.csv(myData3Join, "data/data_raw/myData3Join.csv" )
# 
# ## Write myData3Join (type="left")
# write.csv(myData3MergeNames, "data/data_raw/myData3MergeNames.csv" )
# 
# ## Write myData3MergeNamesJoin
# write.csv(myData3MergeNamesJoin, "data/data_raw/myData3MergeNamesJoin.csv" )

#############################################################################################
#############################################################################################
# be aware when using com variable for matching, there are some problems with that in merging context #
#############################################################################################
#############################################################################################

######################################################################################################################################################
#### LZ-HE 116-42-4  Anbau auf dem Ackerland in landwirtschaftlichen Betrieben nach Fruchtarten - Jahr - regionale Tiefe: Kreise und krfr. Städte ####
######################################################################################################################################################
# fileUrl14 <- "https://www.regionalstatistik.de/genesis/online/data/116-42-4.csv;jsessionid=2742212F42B288847613FD1B208092B3?operation=ergebnistabelleDownload&levelindex=2&levelid=1386267000327&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl14, destfile="./LZ_areaCropType.csv")
# Jahr 2010

#### Load file
areaCropTypeLZ<-read.table("data/data_raw/LZ_areaCropType.csv", header = F, sep=";", dec=",", col.names=c("comId","com","farmlandHa", "cropHa", "wheatHa","winWheat","ryeHa","triticaleHa","barleyHa", "oatHa","maizCornCobMixHA", "otherGrainHa", "PlantsForGreenHarvestingHa","siloMaizeHa", "sugarBeetHa","potatoesHa", "oilseedHA" , "winRapeHa","legumeHa") ,  na.strings=c(".","-"), colClasses=c("factor", "factor",  rep("numeric", 16)),nrows=524, skip=11)
head(areaCropTypeLZ)
## Ad years, because LZ file
areaCropTypeLZ$year <-year

## Explorative Analysis
head(areaCropTypeLZ)
tail(areaCropTypeLZ)



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
areaCropTypeTotalMergeNames <- names(areaCropTypeLZ)[names(areaCropTypeLZ)%in%names(areaCropType2)]
areaCropTypeTotalMergeNames

## The year 2010 is added
unique(areaCropType2$year)
unique(areaCropTypeLZ$year)

#### Merge ####
areaCropTypeTotal <-merge(areaCropType2, areaCropTypeLZ, by = c(areaCropTypeTotalMergeNames), all=T)
names(areaCropTypeTotal)
table(areaCropTypeTotal$year)

#### Explore new file areaCropTypeTotal ####
head(areaCropTypeTotal)
tail(areaCropTypeTotal)

## Write areaCropTypeTotal

################################################
#### Merge areaCropTypeTotal with myData2LZ ####
################################################

## Preparation of merge
names(myData2LZ)
names(areaCropTypeTotal)
## Merge only by comID and year
myData3LZMergeNames <- names(myData2LZ)[names(myData2LZ)%in%names(areaCropTypeTotal)]

# Keine Com
myData3LZMergeNames <-myData3LZMergeNames[-2]

myData3LZ <- merge(myData2LZ, areaCropTypeTotal,by=myData3LZMergeNames)

## Check ComIDs per year ##
table(myData3LZ$year) #looks good

## Explore new myData3LZ
head(myData3LZ)
names(myData3LZ)
tail(myData3LZ)

## Write myData3LZ
write.csv(myData3LZ, "data/data_raw/myData3LZ.csv")


####Comparing myData3 and myData3LZ
head(myData3[myData3$year==2010,])
na.exclude(myData3[myData3$year,"farmlandHa"])
head(myData3LZ[myData3$year==2010,])
names(myData3)%in%names(myData3LZ)
length(names(myData3))
length(names(myData3LZ))

###############################################################################################################################################
#### AA - 115-31-4-  Landwirtschaftl. Betriebe der Rechtsform Einzelunternehmen - Erhebungsjahr - regionale Tiefe: Kreise und krfr. Städte ####
###############################################################################################################################################
## Hab ich vorerst weggelassen , da ich keinen direkten Sinn darin erkenn.
## Variablen: Betriebstyp: Haupt - und Nebenerwerbstbetriebe

#####################################################################################################################################
#### GENESIS-Tabelle: 115-32-4 Landwirtschaftliche Betriebe und landwirtschaftlich genutzte Fläche (LF) nach Größenklasse der LF ####
####- Erhebungsjahr - regionale Tiefe: Kreise und krfr. Städte Allgemeine Agrarstrukturerhebung;                                 ####
#####################################################################################################################################

# fileUrl5 <-"https://www.regionalstatistik.de/genesis/online/data/115-32-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386262531133&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl5,destfile ="./AA_sizeClass.csv")
# datedownload5 <-date()

## Load File ##
sizeClass <- read.table("data/data_raw/AA_sizeClass.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","dummy","type","total", "class2","class5","class10","class20","class30", "class50", "class75", "class100", "classMore") ,  na.strings=c(".","-", "..."), colClasses=c("integer","factor", "factor", "factor", "factor", rep("numeric", 10)),nrows=5250, skip=7)

## Explore File ##
head(sizeClass)
tail(sizeClass)
unique(sizeClass$year)
names(sizeClass)
## delete dummy ## 
sizeClass$dummy <- NULL

## Reshape dataframe ##
sizeClassNum <- sizeClass[sizeClass$type == "Anzahl",]
sizeClassHa <- sizeClass[sizeClass$type == "ha", ]

sizeClass2 <- merge(sizeClassNum, sizeClassHa, by=c("year","comId", "com"), all=T , suffixes = c("Num","Ha"))
names(sizeClass2) ## !! Hier gibt es wieder Variablen, welche gedoppelt sein könnten


## delete type variablen ##
sizeClass2$typeNum <- sizeClass2$typeHa <- NULL
names(sizeClass2)
names(myData3)
#Variablen totalNum und totalNum scheinen equivalent zu sein, genauso totalHa und totalHa, deswegen merge ich diese
#Variablen

# ##########################################
# #### Merge sizeClass2 with my myData3 ####
# myData4 <- merge(myData3, sizeClass2, by=c("year","comId", "com", "totalNum", "totalHa"), all=T )
# names(myData4)
# 
# head(myData4)
# tail(myData4)
# names(myData4)
# 
# ## Write myData4
# write.csv(myData4, "correlation/data/data_processed/myData4.csv" )

############################################
#### Merge sizeClass2 with my myData3LZ ####
names(sizeClass2)
table(sizeClass2$year)

names(myData3LZ)
table(myData3LZ$year)
match(names(myData3LZ), names(sizeClass2))
names(myData3LZ)%in%names(sizeClass2)
myData4LZMergeNames<-names(myData3LZ)[names(myData3LZ)%in%names(sizeClass2)]

myData4LZ <- merge(myData3LZ, sizeClass2, by=c(myData4LZMergeNames), all=T )
table(myData4LZ$year)

head(myData4LZ)
tail(myData4LZ)
names(myData4LZ)

##  Write myData4LZ
write.csv(myData4LZ, "./data/data_raw/myData4LZ.csv" )

##############################################################################################################################
#### AA: -115-35-4 -Landwirtschaftliche Betriebe und landwirtschaftlich genutzte Fläche nach der Art der Bewirtschaftung -####
#### Erhebungsjahr -   regionale Tiefe: Kreise und krfr. Städte                                                           ####
##############################################################################################################################

# fileUrl6 <- "https://www.regionalstatistik.de/genesis/online/data/115-35-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386263974958&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl6,destfile ="./AA_organic.csv")
# datedownload6 <- date()

## Load File ##
organic <- read.table("data/data_raw/AA_organic.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","totalNum", "totalHa","organicAgrNum","organicAgrHa") ,  na.strings=c(".","-", "..."), colClasses=c("integer","factor", "factor", rep("numeric", 4)),nrows=2625, skip=9)

## Explore File ##
names(organic) 
head(organic)
unique(organic$year)
## Löschen der totalNum und totalHa Variablen, da diese doppelt sind und meine Beobachtungsanzahl für die Jahr 1999,2003,2007 verändern
organic$totalNum<-organic$totalHa<-NULL

table(organic$year)
length(unique(organic$com))
length(unique(myData4$com))

unique(organic$com)%in%unique(myData4$com)
organic$com
myData4$com
length(unique(organic$com)%in%unique(myData4$com))

length(unique(organic$comId))
length(unique(myData4$comId))

unique(organic$comId)%in%unique(myData4$comId)
length(unique(organic$comId)%in%unique(myData4$comId))

## Auch Löschen der com Variablen
organic$com<-NULL

#### Merge organic mit myData4 ####
names(myData4)
names(organic)
myData4MergeNames<-names(myData4)[names(myData4)%in%names(organic)]

myData5beta<-merge(myData4, organic, by=c(myData4MergeNames), all=T )
names(myData5beta)
table(myData5beta$year)


myData5<-merge(myData4, organic, by=c(myData4MergeNames), all=T )
names(myData5)
table(myData5$year)

## Write myData5
write.csv(myData5, "data/data_raw/myData5.csv", eol="\n" )

#### LZ-HE: - 116-34-4  Landwirtschaftliche Betriebe und deren landwirtschaftlich genutzte Fläche (LF) nach Art der Bewirtschaftung - Jahr - regionale Tiefe: Kreise und krfr. Städte####
# fileUrl11 <-"https://www.regionalstatistik.de/genesis/online/data/116-34-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386265887936&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl11,destfile ="./LZ_organic.csv")
# Year 2010
organicLZ <- read.table("data/data_raw/LZ_organic.csv", header = F, sep=";", dec=",", col.names=c("comId","com","organicAgrNum","organicAgrHa", "organicTransFin", "organicTrans", "organicTransNot" ) ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor", rep("numeric", 5)),nrows=524, skip=11)
names(organicLZ)


organicLZ$organicTransFin<-organicLZ$organicTrans<-organicLZ$organicTransNot <-NULL
#löschen hat keinen Einfluss auf Doppelung
names(organicLZ)
names(organic)
head(organicLZ)
tail(organicLZ)
dim(organicLZ)

organicLZ$year <-year

sapply(organicLZ[1,], class)

#### Merge Organic und OrganicLZ
## Vorbereitung
namesOrganicLZ <-names(organicLZ)
namesOrganicLZ
namesOrganic<-names(organic)
namesOrganic

table(organic$year)
table(organicLZ$year)
 
mergeNamesOrganic <- as.vector(namesOrganicLZ[namesOrganicLZ%in%namesOrganic])
mergeNamesOrganic

## Merge
organicTot<-merge(organic, organicLZ, by=mergeNamesOrganic, all=T)
table(organicTot$year)
names(organicTot)

## Write organicTot
write.csv(organicTot,"data/data_processed/organicTot.csv ")

#### Merge von myData4LZ und organicTot ohne totalNum bzw Ha und totalAgr
## Vorbereitung
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

## Merge
myData5LZ<-merge(myData4LZ, organicTot, by=mergeNamesorganicTot, all = T)
table(myData5LZ$year)

#Da totalAgrNum und totalHa überflüssig sind, lösche ich diese
myData5LZ$totalAgrNum <-myData5LZ$totalHa <-NULL 
names(myData5LZ)


## write myData5LZ
write.csv(myData5LZ, "data/data_processed/myData5LZ.csv")

###################################################################################################################################
#### AA: -115-37-4-  Landwirtschaftliche Betriebe mit Viehhaltung - Stichtag: 03.05 - regionale Tiefe: Kreise und krfr. Städte ####
##Hab ich vorerst weggelassen                                                                                                  ####                              
###################################################################################################################################

######################################################################################################################################
#### AA: 15-43-4  Landwirtschaftl. Betriebe u. deren landwirtschaftl. genutzte Fläche n. Größenkl. d. Standarddeckungsbeitrages-  ####             
#### Erhebungs- jahr- regionale Tiefe: Kreise und krfr. Städte (ab 2003)                                                          ####
######################################################################################################################################
# fileUrl7 <- "https://www.regionalstatistik.de/genesis/online/data/115-43-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386264337555&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl7,destfile ="./AA_ege.csv")

#### Load Files ###
sgm <-read.table("data/data_raw/AA_sgm.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","dummy","type", "totalSgm", "l2Esu","Esu2to8","Esu8to16","Esu16to24","Esu24to32", "Esu32to40", "Esu40to60", "m60Esu") ,  na.strings=c(".","-", "..."), colClasses=c("integer","factor", "factor","factor", "factor", rep("numeric", 8)),nrows=2098, skip=9)
#
#### Explore File ####
unique(sgm$year)
#Daten scheinbar nur 2003 und 2005 und auch eher unvollständig

tail(sgm)
sgm$dummy <- NULL
names(sgm)

#Reshape dataframe for a better merge
sgmNum <- sgm[sgm$type == "Anzahl",]
sgmHa <- sgm[sgm$type == "ha", ]

sgm2 <- merge(sgmNum, sgmHa, by=c("year","comId", "com"), all=T , suffixes = c("Num","Ha"))

sgm2$typeNum <- sgm2$typeHa <- NULL
names(sgm2)

###Merge sgm2 and mydata5 ###
names(myData5)
myData5$farmlandNum
names(sgm2)
myData6 <- merge(myData5, sgm2, by=c("year","comId", "com"), all=T )

# head(myData6)
names(myData6)

## Write myData6
write.csv(myData6, "data/data_processed/myData6.csv")

####Merge myData5LZ und sgm2
names(myData5LZ)
names(sgm2)

myData6LZ <- merge(myData5LZ, sgm2, by=c("year","comId", "com"), all=T )
names(myData6LZ)

## Write myData6LZ
write.csv(myData6LZ, "data/data_processed/myData6LZ.csv")

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
# fileUrl8 <- "https://www.regionalstatistik.de/genesis/online/data/115-44-4.csv;jsessionid=C83E88F578C83BF815807F1EF62EE7FC?operation=ergebnistabelleDownload&levelindex=3&levelid=1386264657606&option=csv&doDownload=csv&contenttype='csv'"
# download(fileUrl8,destfile ="./AA_kind.csv")

kind <-read.table("data/data_raw/AA_kind.csv", header = F, sep=";", dec=",", col.names=c("year","comId","com","dummy","type", "totalKind", "kindFarm","kindHorti","kindPermanent","kindGrazLivest", "kindRefining", "kindPlant", "kindLivestock", "kindPlantLivestock") ,  na.strings=c(".","-", "..."), colClasses=c("integer","factor", "factor","factor", "factor", rep("numeric", 9)),nrows=2099, skip=8)

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

myData11LZ<- read.csv2("./data/data_processed/myData11LZ.csv")

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

