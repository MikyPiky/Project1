###Hier versuche ich nun folgenden Weg: nehme den comId Vektor aus myData10Lz und kopier in dann in
### Gblanduse und merge anhand diesen Vorgang

#Dafür brauch ich zuerst myData10LZ und GbLanduse

head(myData10LZ)
head(GbLanduse)
dim(GbLanduse) #3675 23

####Hier wird  nun der ComID vektor extrahiert
3675+524

head(myData10LZ$comId[526:4200])
tail(myData10LZ$comId[526:4200])
length(myData10LZ$comId[526:4200])  #von derläng ist der Verktor richtig

GbLanduse$comIdFix<-myData10LZ$comId[526:4200]

names(GbLanduse)
GbLanduse$comIdFix[1:50]
GbLanduse$year

#Merge myData10LZ mit GbLanduse by comIdFix

myData11LZ<-merge(myData10LZ, GbLanduse, by.x=c("year","comId"), by.y=c("year","comIdFix"), all=T)

#Observations did not change, very good

myData11LZ[1:526,c("comId")]
sum(table(myData11LZ$comId)!="17") #jede comId wird weiterhin 17 mal aufgeführt

write



