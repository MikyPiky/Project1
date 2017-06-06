
#### 449-01-4 - GB Bodenfläche nach Art der tatsächlichen Nutzung - Stichtag 31.12. - regionale Tiefe: Kreise und krfr. Städte ####


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

#transform date to year:
#Get rid of 31.12. in variable year

head(GbLanduse)

GbLanduseDate <-GbLanduse$date
head(GbLanduseDate)
class(GbLanduseDate)

GbLanduseDate[1:100]
year <- as.integer(gsub("31.12.","" ,GbLanduseDate) )

head(year)
table(year)
head(GbLanduse) 

GbLanduseTot <-cbind(GbLanduse, year)

table(GbLanduseTot$date, GbLanduseTot$year)

names(GbLanduseTot)

GbLanduseTot$date 


####Analyse der Problems, dass wieder zu viele Beobachtungen entstehen
#Focus ComID

dim(myData10LZ)[1]/length(unique(myData10LZ$year)) #für jedes Jahr gibt es in myData10 525 Beobachtungen
dim(GbLanduse)[1]/length(unique(GbLanduse$date)) #das gleiche ist auch hier war
myData10LZ$comId[myData10LZ$year=="2010"]
GbLanduseTot$comId[GbLanduseTot$year=="2010"] ## jetzt seh ich das Problem, bei GbLanduse fehlt vorne die Null bei comID mit 4 stelligen oder weniger Ziffern u


myData10LZ[myData10LZ$year=="2011", "comId"]%in%GbLanduseTot[GbLanduseTot$year=="2011","comId"]

myData10LZ[myData10LZ$year=="2011", "comId"][!myData10LZ[myData10LZ$year=="2011", "comId"]%in%GbLanduseTot[GbLanduseTot$year=="2011","comId"]]
###Alle kleiner 5 stelligen ComIds mit anfänglicher Null, also alles ComIds kleiner 10000
###Das sind dann umgekehrt die 4 stelligen ComIds in GBLanduse

GbLanduseTot$comId3<-as.character(GbLanduseTot$comId)
GbLanduseTot$comId3[nchar(GbLanduseTot$comId3)=="4"]





#####LÖSUNGSANSÄTZE#####
####1. Lösungsansatz
###An die 4 stelligen wird eine Null vorgestellt und daraus dann 5 stellige gemacht
###Dann wird an die zwei stelligen eine Null vorgestellt und daraus eine 4 stellige ComiD gemacht
###dann wird ein die einstellige ComID eine Null vorangestellt und dann daraus eine zweistellige gemacht
####dann wird aus 0DG ein DG gemacht
###ist vorerst auch gescheitert.
length(!myData10LZ[myData10LZ$year=="2011", "comId"]%in%GbLanduseTot[GbLanduseTot$year=="2011","comId"])

GbLanduseTotComId <-as.character(GbLanduseTot$comId)
GbLanduseTot$comIdFirst <- GbLanduseTotComId
table(GbLanduseTot$comIdFirst)
table(GbLanduseTot$comId)
summary(GbLanduseTotComId)

GbLanduseTot$comIdFirst [nchar(GbLanduseTot$comIdFirst)=="4"]

GbLanduseTotComId4<-GbLanduseTotComId[nchar(GbLanduseTotComId)=="4"]
GbLanduseTotComId4
summary(GbLanduseTotComId4)

#So, jetzt muss ich da vorne ne Null anfügen
summary(GbLanduseTotComId4) #2233 values
table(GbLanduseTotComId4)
zero4<-as.vector(rep(0,2233))

GbLanduseTotComId4Zero<-as.character(paste(zero4,GbLanduseTotComId4 ))
GbLanduseTotComId4Zero[1:50]
GbLanduseTotComId4Zero<-gsub(" ","", GbLanduseTotComId4Zero)
summary(GbLanduseTotComId4Zero)
# remove(GbLanduseTotComId4Zero)
##Jetzt muss ich diese Werte noch ersetzen im Orginal Vektor
##Dazu wähle die werte aus, der länge 4 ist

GbLanduseTot$comIdFirst [nchar(GbLanduseTot$comIdFirst)=="4"] <- (GbLanduseTotComId4Zero)
GbLanduseTot$comIdFirst [nchar(GbLanduseTot$comIdFirst)=="4"]
#Umwandeln in factor
GbLanduseTot$comIdFirst [nchar(GbLanduseTot$comIdFirst)=="5"]


#nun gibt es keine Werte mehr, die 4 stellig sind
GbLanduseTot[GbLanduseTot$year =="2011","comIdFirst"]
length(GbLanduseTot[GbLanduseTot$year =="2011","comIdFirst"])
myData10LZ[myData10LZ$year=="2011", "comId"]

length(GbLanduseTot[GbLanduseTot$year =="2011","comIdFirst"]%in%myData10LZ[myData10LZ$year=="2011", "comId"])
#hier gibte es auch Probleme, irgendwie will das sicht nicht richtig vergleichen lassen



#Nun schaue ich, wie sich der Wert reduziert hat für das Jahr 2011


####Nun setzt ich vor alle Werte deren Ziffernlänger kleiner als 5 ist eine 0
# #Erstmal werden die entsprechenden Werte ausgewählt
# GbLanduseTotComId5<-GbLanduseTot$comIdFirst [nchar(GbLanduseTot$comIdFirst)<"5"]
# GbLanduseTotComId5
# 
# #So, jetzt muss ich da vorne ne Null anfügen
# summary(GbLanduseTotComId5) #364 values
# table(GbLanduseTotComId5)
# 364/7
# zero5<-as.vector(rep(0,364))
# 
# GbLanduseTotComId5Zero<-as.character(paste(zero5,GbLanduseTotComId5 ))
# GbLanduseTotComId5Zero[1:50]
# GbLanduseTotComId5Zero<-gsub(" ","", GbLanduseTotComId5Zero)
# summary(GbLanduseTotComId5Zero)
# GbLanduseTotComId5Zero
# 
# ###Nun ersetze 0DG durch DG
# GbLanduseTotComId5Zero[GbLanduseTotComId5Zero=="0DG"]<-"DG"
# GbLanduseTotComId5Zero
# 
# ###Vector ersetzten
# GbLanduseTot$comIdFirst [nchar(GbLanduseTot$comIdFirst)<"5"] <- as.factor(GbLanduseTotComId5Zero)
# GbLanduseTot[GbLanduseTot$year=="2011", "comIdFirst"]
# #scheinbar gibts beim Ersetzen Probleme

####Da es eben Probleme gab, alle Werte kleiner 5 zu ersetzten, werde ich nun sukzessive vorgehen
###Als erste werden die 2 stelligen Werte mit einer Null versehen
GbLanduseTotComId2<-GbLanduseTot$comIdFirst [nchar(GbLanduseTot$comIdFirst)=="2"]
GbLanduseTotComId2

#So, jetzt muss ich da vorne ne Null anfügen
summary(GbLanduseTotComId2) #301 values
table(GbLanduseTotComId2)
301/7
zero2<-as.vector(rep(0,301))

GbLanduseTotComId2Zero<-as.character(paste(zero2,GbLanduseTotComId2 ))
GbLanduseTotComId2Zero[1:50]
GbLanduseTotComId2Zero<-gsub(" ","", GbLanduseTotComId2Zero)
summary(GbLanduseTotComId2Zero)
GbLanduseTotComId2Zero

###Vector ersetzten
GbLanduseTot$comIdFirst [nchar(GbLanduseTot$comIdFirst)=="2"] <- GbLanduseTotComId2Zero
GbLanduseTot[GbLanduseTot$year=="2011", "comIdFirst"]
head(GbLanduseTot)
table(GbLanduseTot$comId)
table(GbLanduseTot$comIdFirst)

#Problem: nicht nur die Reihenfolge hat sich geändert, sondern auch die Verteilung der Variablen, daher
#passt dieses vorgehen nicht. Deswegen werde ich jetzt mal mit plyr versuchern






####2. Lösungsansatz ####
#In diesem Ansatz wird an jede ComID zuerst eine Null vorangestellt und dann im nachhinein bei den entsprechenden Ziffernm also allen ungleich Ziffernlänge 4 im ursproünglichem
#Zustande die Null weggelöscht


#Extract comId to concetate a frame with zeros

GbLanduse$comId
GbLanduseComId <-GbLanduse$comId
summary(GbLanduseComId)
length(GbLanduseComId)

zero<-as.vector(rep(0,3675))
GbLanduseComIdZero<-as.character(paste(zero,GbLanduseComId ))

GbLanduseComIdZero<-gsub(" ","", GbLanduseComIdZero)

GbLanduseTot$comId2[GbLanduseTot$comId2%in% myData10LZ$comId]
tail(GbLanduseTot)
#Achtung, auch hier stimmten die Werte der comId2 nicht mit den comIDs überein, das heißt bei der Reihenfolge ist was schief gegangen

#okay, scheinbar gibt es Prolbeme mit ComIDs, die vorher schon 6 Ziffern hatten
#


GbLanduseTot$comId2 <-GbLanduseComIdZero
head(GbLanduseTot$comId2)

#hier werden die Nullen vor den 6stelligen comId2s rausgenommen
GbLanduseTot$comId2[nchar(GbLanduseTot$comId2)=="6"]
GbLanduseTot$comId2[nchar(GbLanduseTot$comId2)=="6"]<-substring(GbLanduseTot$comId2[nchar(GbLanduseTot$comId2)=="6"],2,6)
GbLanduseTot$comId2[nchar(GbLanduseTot$comId2)=="6"] #Bam, hier gibt es keine 6stelligen Ziffern mehr

GbLanduseTot$comId2[GbLanduseTot$year=="2010"] 

#hier wird  0DG in DG umgewandelt
GbLanduseTot$comId2[nchar(GbLanduseTot$comId2)=="3"]
GbLanduseTot$comId2[GbLanduseTot$comId2=="0DG"]
GbLanduseTot$comId2[GbLanduseTot$comId2=="0DG"]<-"DG" #ersetzen kann so leicht sein

GbLanduseTot$comId2[1:100]
#Umwandeln von ComId2 Character to Factor
GbLanduseTot$comId2<-as.factor(GbLanduseTot$comId2)


GbLanduseTot[GbLanduseTot$year=="2011",c("comId","comId2")]

levels(myData10LZ[myData10LZ$year=="2011", "comId"])

class(GbLanduseTot[GbLanduseTot$year=="2011","comId2"])

levels(GbLanduseTot[GbLanduseTot$year=="2011","comId2"])


####Hier schaue ich, welche Variablen im Jahr 2011 jeweils nicht übereinstimmen
myData10LZ[myData10LZ$year=="2011", "comId"][!myData10LZ[myData10LZ$year=="2011", "comId"]%in%GbLanduseTot[GbLanduseTot$year=="2011","comId2"]]
#Ach to dicke schieße, das geht ja tatsächlich so: hier lass ich mir wohl alle false values, also nicht übreinstimmenden Werte anzeigen
length(myData10LZ[myData10LZ$year=="2011", "comId"][!myData10LZ[myData10LZ$year=="2011", "comId"]%in%GbLanduseTot[GbLanduseTot$year=="2011","comId2"]])
#Es gibt also mindestes 17 ComIds (2011), die nicht übereinstimmen
ComIdFalseMyData<-as.factor(myData10LZ[myData10LZ$year=="2011", "comId"][!myData10LZ[myData10LZ$year=="2011", "comId"]%in%GbLanduseTot[GbLanduseTot$year=="2011","comId2"]])
ComIdFalseMyData
class(ComIdFalse)

#Jetzt aus der Perspektive von GBLandUseTot
GbLanduseTot[GbLanduseTot$year=="2011","comId2"][!GbLanduseTot[GbLanduseTot$year=="2011","comId2"]%in%myData10LZ[myData10LZ$year=="2011", "comId"]]
ComIdFalseLanduse<-GbLanduseTot[GbLanduseTot$year=="2011","comId2"][!GbLanduseTot[GbLanduseTot$year=="2011","comId2"]%in%myData10LZ[myData10LZ$year=="2011", "comId"]]
ComIdFalseLanduse
length(ComIdFalseMyData)

ComIdFalseLanduse
length(ComIdFalseLanduse)
###von beiden Perspektiven aus stimmt die gleich Anzahl an Variablen jeweils nicht überein


###Jetzt schaue ich, welche Variablen im ganzen Datensatz nicht übereinstimmen

!myData10LZ[, "comId"]%in%GbLanduseTot[, "comId2"]

myData10LZ[, "comId"][!myData10LZ[, "comId"]%in%GbLanduseTot[,"comId2"]]
comIdFalseMyDataTotal<-myData10LZ[, "comId"][!myData10LZ[, "comId"]%in%GbLanduseTot[,"comId2"]]

length(comIdFalseMyDataTotal)
#dh, es stimmen etwa 289 Werte der Variable comId in myData10LZ nicht mit den Werten der Variable comId2 in GbLandUseTot überein
#Strategie könnte sein, den Vektor mit diesen Variablen aus GbLanduseToT zu extrahieren, dann mit dem entsprechenden Vector aus myData10 zu ersetzten und dann wieder einzufügen
#?st das nicht eine klassische Aufgabe für plyr?
??plyr

#Vorher aber erstmal der Versuch ohne
comIdFalseLanduseTotal<-GbLanduseTot[,"comId2"][!GbLanduseTot[,"comId2"]%in%myData10LZ[, "comId"]]
comIdFalseLanduseTotal#in diesem Vector müssen scheinbar alle Nullen weg
length(comIdFalseLanduseTotal)

#neue Strategie: erst alle 3 Stelligen Ziffern anschauen und eventuell die NUll vorne rausnehmen
#dann das gleich mit den 4 stelligen Ziffern in ComId


#hier werden die Nullen vor den 4stelligen comId2s rausgenommen

GbLanduseTot$comId2<-as.character(GbLanduseTot$comId2)
GbLanduseTot$comId2[nchar(GbLanduseTot$comId2)=="4"]%in%comIdFalseLanduseTotal#hier gibt es komplette Übereinstimmung


GbLanduseTot$comId2[nchar(GbLanduseTot$comId2)=="4"]<-substring(GbLanduseTot$comId2[nchar(GbLanduseTot$comId2)=="4"],2,4)
GbLanduseTot$comId2[nchar(GbLanduseTot$comId2)=="4"] #Bam, hier gibt es keine 4stelligen Ziffern mehr

GbLanduseTot$comId2[GbLanduseTot$year=="2010"] 

#hier wird  0DG in DG umgewandelt
GbLanduseTot$comId2[nchar(GbLanduseTot$comId2)=="3"]
GbLanduseTot$comId2[GbLanduseTot$comId2=="0DG"]
GbLanduseTot$comId2[GbLanduseTot$comId2=="0D]


                    
                    
                    comIdFalseMyDataTotal
                    
                    
                    
                    
                    
                    
                    
                    #Drop comID
                    GbLanduseTot$comId<-NULL
                    
                    levels(GbLanduseTot$comId2)
                    GbLanduseTot$comId2%in% myData10LZ$comId
                    
                    
                    
                    
                    
                    
                    ####Merge GbLanduseTot with myData10LZ by comID2
                    names(GbLanduseTot)
                    
                    myData11LZ<-merge(myData10LZ, GbLanduseTot, by.x=c("year","comId"), by.y=c("year","comId2"), all=T)
                    
                    names(a)
                    
                    
                    # 
                    # names(myData11LZ)
                    # 
                    # table(myData11LZ$comId.x)
                    # table(myData11LZ$comId.y)
                    # unique(myData11LZ$comId.y)
                    # unique(myData11LZ$comId.x)
                    # lengthlevels1<-length(levels(myData11LZ$comId.y))
                    # lengthlevels2<-length(levels(myData11LZ$comId.x))
                    # levels1<-levels(myData11LZ$comId.y)
                    # head(levels1)
                    # levels2<-levels(myData11LZ$comId.x)
                    # head(levels2)
                    # levels1[levels1%in%levels2]
                    # head(levels1[levels1%in%levels2])
                    # length(levels1[levels1%in%levels2]) #es scheint 169 levels in levels1 comId.y zu geben, die es nicht in comId.x gibt
                    # levels1.1<-levels1[levels1%in%levels2] 
                    # myData10LZ$comId[1:20]
                    
                    
                    table(myData11LZ$year) #Probleme bei den Jahren 1996,200,2004,2008,2009,2010,2011
                    #Warum gibt es hier noch 
                    table(GbLanduse$year)
                    #Es scheint aber keine Doppelungen zu geben
                    head(myData11LZ[myData11LZ$year=="2011",])
                    
                    
                    
                    
                    ####Merge GbLanduseTot with myData10LZ
                    
                    namesmyData10LZ <-names(myData10LZ)
                    namesmyData10LZ
                    namesGbLanduse<-names(GbLanduseTot)
                    namesGbLanduse
                    
                    
                    mergeNamesmyData10LZ <- as.vector(namesmyData10LZ[namesmyData10LZ%in%namesGbLanduse])
                    
                    
                    mergeNamesmyData10LZ
                    
                    sapply(GbLanduseTot[1,],class)
                    class(myData10LZ$year)
                    
                    myData11LZ<-merge(myData10LZ, GbLanduseTot, by=mergeNamesmyData10LZ, all=T)
                    
                    
                    table(myData11LZ$year) #Probleme bei den Jahren 1996,200,2004,2008,2009,2010,2011
                    table(GbLanduseTot$year)
                    #Es scheint aber keine Doppelungen zu geben
                    myData11LZ[myData11LZ$year=="2011",]
                    
                    
                    
                    names(myData11LZ)
                    write.csv(myData11LZ,"~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData11LZ.csv ")
                    
                    ####Merge GbLanduse with myData10LZ
                    
                    namesmyData10LZ <-names(myData10LZ)
                    namesmyData10LZ
                    namesGbLanduse<-names(GbLanduse)
                    namesGbLanduse
                    table(myData10LZ$date)
                    table(GbLanduse$date)
                    
                    
                    mergeNamesmyData10LZ <- as.vector(namesmyData10LZ[namesmyData10LZ%in%namesGbLanduse])
                    mergeNamesmyData10LZ
                    sapply(GbLanduse[1,],class)
                    class(myData10LZ$date)
                    class(GbLanduse$date)
                    
                    
                    
                    myData11LZ<-merge(myData10LZ, GbLanduse, by=c("com","date"), all=T) 
                    #scheinbar scheint des Problem bei comId zu liegen
                    
# remove(GbLanduse)
# remove(GbLanduseComId)
# remove(GbLanduseComIdZero)
                    # remove(GbLanduseDate)
                    # remove(GbLanduseTot)
                    # remove(GbLanduseTot2)
                    # remove(GbLanduseTotComId)
                    # remove(GbLanduseTotComId4)
                    # remove(GbLanduseTotComId4Zero)
                    
                    GbLanduseManp<- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/GB_landuse_manipuliert.csv", header = F, sep=";", dec=",", col.names=c("date","comIdmanp","comId","com","comAreaHa","comAreaStrucOpensiteTotalHa","comAreaStrucOpensiteResidenceHa","comAreaStrucOpenesiteBuisnessHa","comAreaStrucHa", "comAreaIndustrExclOpenPitminingHa","comAreaRecrTotHa","comAreaRecrParkHa","comAreaCemetryHa","comAreaInfrastrucTotHa", "comAreaInfrastrucPublicHa", "comAreaAgrTotHa","comAreaAgrMireHa","comAreaAgrHeathHa","comAreaForestHa","comAreaWaterHa", "comAreaOpenPitminingHa","comAreaOtherTotHa","comAreaOtherWasteland") ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor","factor", "factor",rep("numeric", 19)) ,nrows=3675, skip=8)
                    
                    names(GbLanduseManp)
                    head(GbLanduseManp)
                    tail(GbLanduseManp)
                    
                    #Da es schwierig ist, bei Excel oder Libre Zahlen Formattierungen zu haben, die vorne eine Null haben
                    #ist es vll. einfacher, die ComIds aus GbLanduse in den myData10 Datensatz zu kopieren
                    #Problem ist nur, dass sich dann die Reihenfolge ändert
                    
                    
                    myData10Manp2<- read.csv("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_processed/myData10LZ_manipuliert2.csv")
                    
                    myData10Manp2$comId[table(myData10Manp2$comId)!="17"] ##alle Werte in myData10Manp$comId werden 17 mal wiederholt
                    
                    table(myData10Manp2$year) ##für jedes Jahr gibt es 525 Beobachtungen
                    525*17
                    #8925 Beobachtungen also
                    
                    levels(myData10Manp2$comId)
                    levels(myData10LZ$comId)
                    
                    
                    
                    ####Jetzt wird die normale Landuse Datei benutzt
                    #### 449-01-4 - GB Bodenfläche nach Art der tatsächlichen Nutzung - Stichtag 31.12. - regionale Tiefe: Kreise und krfr. Städte ####
                    
                    
                    fileUrl19<- "https://www.regionalstatistik.de/genesis/online/data/449-01-4.html;jsessionid=C0C161BAC65CF9CD099E8112C0DF5F58?operation=ergebnistabelleDownload&levelindex=1&levelid=1387371272067&option=csv&doDownload=csv&contenttype='csv'"
                    
                    
                    GbLanduse<- read.table("~/Documents_shared/Projekte/Yield - SMI/correlation/data/data_raw/GB_landuse.csv", header = F, sep=";", dec=",", col.names=c("date","comId","com","comAreaHa","comAreaStrucOpensiteTotalHa","comAreaStrucOpensiteResidenceHa","comAreaStrucOpenesiteBuisnessHa","comAreaStrucHa", "comAreaIndustrExclOpenPitminingHa","comAreaRecrTotHa","comAreaRecrParkHa","comAreaCemetryHa","comAreaInfrastrucTotHa", "comAreaInfrastrucPublicHa", "comAreaAgrTotHa","comAreaAgrMireHa","comAreaAgrHeathHa","comAreaForestHa","comAreaWaterHa", "comAreaOpenPitminingHa","comAreaOtherTotHa","comAreaOtherWasteland") ,  na.strings=c(".","-", "..."), colClasses=c("factor","factor", "factor",rep("numeric", 19)) ,nrows=3675, skip=8)
                    
                    names(GbLanduse)
                    head(GbLanduse)
                    tail(GbLanduse)
                    
                    #Nun wir für GbLanduse die Variable year aus der Variable date erstellt
                    
                    year <- as.integer(gsub("31.12.","" ,GbLanduse$date) )
                    year[1:100]
                    table(year)
                    
                    GbLanduse$year<-year
                    
                    
                    GbLanduse[500:550,c("date","year")] #auch der Übergang passt
                    
                    
                    
                    ##Nun wird ein comId Block für ein Jahr rausgezogen aus GbLandsuse und dann 17 mal wiederholt
                    
                    GbLanduse[GbLanduse$year=="2011","comId"]
                    length(GbLanduse[GbLanduse$year=="2011","comId"])
                    comIdManp<-rep(GbLanduse[GbLanduse$year=="2011","comId"],17)
                    length(comIdManp)
                    comIdManp[500:550] #es scheint funktioniert zu haben
                    table(comIdManp)
                    
                    #Anhängen des Vectors an myData10Manp2
                    comIdManp[1:100] #geht mit DG los  pro Jahr
                    myData10Manp2$comId[1:100] #endet mit DG
                    
                    comIdManp<-comIdManp[-1]
                    length(comIdManp)
                    tail(comIdManp)
                    
                    comIdManp[8925]="DG"
                    tail(comIdManp)
                    length(comIdManp)
                    
                    
                    myData10Manp2$comIdManp<-comIdManp
                    myData10Manp2[500:550, c("comId","comIdManp")]
                    myData10Manp2[1:50, c("comId","comIdManp")]
                    myData10Manp2[8900:8925, c("comId","comIdManp")]
                    #okay, die beiden comId scheinen übereinstimmend zu sein
                    
                    ####Merge myData10Manp2 mit GbLanduse
                    
                    myData11LZ<-merge(myData10Manp2, GbLanduse, by.x=c("year", "comIdManp"), by.y=c("year", "comId"), all=T)
                    
                    ###okay, die Observationen haben sich schonmal nicht vervielfacht, was seht gut ist
                    ####aber die Reihenfolge macht nun Probleme, da die ComIds mit einer 1 vorneweg sin
myData11LZ[1:526,c("comId","comIdManp")]
                    
                    
                    