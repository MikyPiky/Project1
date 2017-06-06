setwd("/gpfs0/home/peichl/Projekte/correlation")#
getwd()
#install.packages("ncdf")
library(ncdf)
library(chron)
library(RColorBrewer)

# open a netCDF file


getwd()
ncfname <- "/correlation/data/data_raw/4_michael/mSMI.nc"
SMI<- open.ncdf(file.choose())


# print some basic information
print(SMI)
str(SMI)

SMI$dim$easting$vals
SMI$dim$northing$vals


# get latitudes and longitudes
eastSMI<-get.var.ncdf(SMI,"easting", verbose=F)
eastSMI


dim(eastSMI)
nEastSMI <- dim(eastSMI)
nEastSMI

eastUnitsSMI<-att.get.ncdf(SMI, "easting", "units")
eastUnitsSMI

northSMI<-get.var.ncdf(SMI,"northing", verbose=T)
nNorthSMI<-dim(northSMI)
nNorthSMI

northUnitsSMI<-att.get.ncdf(SMI, "northing", "units")
northUnitsSMI

# get time dimension
str(SMI)
tSMI <- get.var.ncdf(SMI, "time", verbose=T)
dim(tSMI)
nTSMI <- dim(tSMI)
tUnitsSMI<-att.get.ncdf(SMI, "time", "units")
tUnitsSMI

# print t und tunits
head(tSMI,20)
tSMI[360:380]
tUnitsSMI$value

# # show "real" times
# tunits.SMI$value
# tustr <- strsplit(tunits.SMI$value, " ") # parse tunits$value
# tustr
# tdstr <- strsplit(unlist(tustr)[3], "-")
# tdstr
# tmonth.SMI=as.integer(unlist(tdstr)[2])
# print(tmonth.SMI)
# tday.SMI=as.integer(unlist(tdstr)[3])
# print(tday.SMI)
# tyear.SMI=as.integer(unlist(tdstr)[1])
# print(tyear.SMI)
# chron(t.SMI, origin=c(tmonth.SMI, tday.SMI, tyear.SMI) )
# 
# #hier bekomme ich gerade keine monatliche timesteps hin

# get the data and attributes


str(SMI$var, max.level=2)
SMiArray <- get.var.ncdf(SMI,"SMI")
str(SMiArray)

lName <- att.get.ncdf(SMI,"SMI","long_name")
lName

dUnits <- att.get.ncdf(SMI,"SMI","units")
dUnits


fillvalue <- att.get.ncdf(SMI,"SMI","_FillValue")
fillvalue

dim(SMiArray)


# replace fillvalues  with NAs
SMiArray[SMiArray==fillvalue$value]<-NA


# done with the netCDF file, so close it
close.ncdf(SMI)

#get slice of the data
month <- 600
SMiSlice1 <- SMiArray[,,month]
dim(SMiSlice1)

month <- 601
SMiSlice2<- SMiArray[,,month]
str(SMiSlice2)

#plot
image(SMiSlice1, col=rev(brewer.pal(9,"RdBu")))

image(SMiSlice2, col=rev(brewer.pal(9,"RdBu")))



#convert the nlon by nlat ny nt array into a nlon*nlat by nt matrix

SMiVecLong <- as.vector(SMiArray)

length(SMiVecLong)
SMiMat <- matrix(SMiVecLong, nrow=nEastSMI*nNorthSMI, ncol=nTSMI)
dim(SMiMat)
head(na.omit(SMiMat))[,c(1:20)]


# create a data frame from the matrix
lonLatSMI <- expand.grid(eastSMI, northSMI)
dim(lonLatSMI)
head(lonlatSMI)

SMIdf02<-(data.frame(cbind(lonLatSMI,SMiMat)))

dim(SMIdf02)

names(SMI.df02)<- c("lon","lat",rep(1950,12), )


#Versuch, die Variablen zu benennen
y<-1955:2014
y
y2<-rep(w,12)
y3<-sort(y2)
length(y3)/12

m<-1:12
m
m2<-rep(m,65)
length(m2)
m2

t<-paste(m2,y3,sep="_")
t



t<-for(i in 1:60)print(x[i])

head(na.omit(SMI.df02,))[,c(1:20)]
SMI.df02[c(3000:3050),c(1:12)]

na.omit(SMI.df02[c(50618:50640),])

# get row means
SMI.rowmeans <- apply(SMI.mat,1, mean)
length(SMI.rowmeans)
head(na.omit(SMI.rowmeans), 20)

#plot them
image(matrix(SMI.rowmeans, nrow=175, ncol=225), col=rev(brewer.pal(9,"RdBu")))

#add the means to the data frame
SMI.df02 <- cbind(SMI.df02,SMI.rowmeans)
colnames(SMI.df02, SMI.rowmeans)
head(na.omit(tmp.df02),20)
dim(tmp.df02)

#write the data frame out as a .csv file, omit rows with NAS
csvfile <- "../../data/data_raw/SMI.csv"
write.table(SMI.df02,csvfile, row.names=FALSE, sep=",")
getwd()

### Raster Examples ###
library(maptools)
library(ncdf)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(zoo)
library(rgdal)
library(foreign)

#install.packages("maps")
library(maps)


# #Raster Layer with default parameters
# x<-raster()
# x
# res(x)
# > # With other parameters 
# x <- raster(ncol=36, nrow=18, xmn=-1000, xmx=1000, ymn=-100, ymx=900)  # that can be changed > res(x)

### read in the mSMI.ncd via the raster brick function
getwd()
mSMiRas<-brick("./data/data_raw/4_michael/mSMI.nc")

mSMiRas
str(mSMiRas)
plot(mSMiRas)

str(mSMiRas[layer=1])
#plot(mSMiRas[layer=1]) #weiß nicht genau, was ich hier gema

image(mSMiRas)
plot(mSMiRas, c(700:720))
nlayers(mSMiRas)
# extract particular layers
mSMI1<-raster(mSMiRas,layer=1)

mSMI1
str(mSMI1)
mSMI1@data@band
str(mSMI1)

print(raster(mSMiRas, layer=1))

plot(mSMI1, main="SMI for the 1st month")

paste("SMI",1)

# #Versuch, einzelne layers mit loops ausgeben zu lassen
# for(i in 1:10){paste("i =", i)<-raster(mSMI, layer=i)}
# smiList<-vector("list",10)
# smiList
# 
# hm<-function(x,y)for(i in x:y){
#       str<-paste("smi",i, sep="")
#       as.matrix(print(str))
#       x<-raster(mSMI, layer=i)
#       assign(str, x,envir=.GlobalEnv )
# }
### Mit dieser Function kann ich seperate layers ausgeben lassen, so dass ich sie danach auch bearbeiten kann!
#############################################################################################################
hm(690,690)
smi690
str(smi690)
plot(smi690)
smiList
str(str10)
plot(str)
x
str10
### global outlines shape file
# browse to world.shps

getwd()
setwd("/gpfs0/home/peichl/projects/correlation/correlation")

projection=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")

####Maptools-Package:
worldShp <- readShapeLines("./data/data_raw/4_michael/adminitrative_borders_Ger/vg2500_krs")
worldShp

#Explore structure of the shape file
str(worldShp, max.level=2)
worldShp@data
#die comID kann ich dann nutzen, um Daten beruhend auf den Kreisen mit data der Ertragszahlen zu verschneiden
plot(worldShp, col="black", lwd=1)



####hier lese ich die Shape file als Polygon ein
worldPol<-readShapePoly(fn="./data/data_raw/4_michael/adminitrative_borders_Ger/vg2500_krs") 
###einlesen geht seht schnell, daher macht es keinen Sinn, es anders abzuspeichern
###sehr gut, kann mit extract eingelesen werden. 
# write.dbf(worldPol, "./data/data_raw/worldPol")
# worldPolDbf<- read.dbf("./data/data_raw/worldPol.dbf")
plot(worldPol)

worldPol
mSMI
projection(worldPol)= projection
#hier verpasse ich dem ganzen nochmals die gleiche Projection
coordinates(mSMI)
coordinates(worldPol)#gibt die coordinaten der Mittelpunkte der Polygone wieder

extent(worldPol)
extent(mSMI)

plot(mSMI,690)
plot(worldPol, , add=T)


### Bin mit aber nicht sicher, welche Folgen es hat, dass die Extends nicht die gleichen sind.
###Im Beispiel sind es aber auch nicht die gleichen
### Der Meinung von Stefan zufolge scheint das kein Problem zu sein, da Koordinaten die wichtige Referenzierung sind

####RGDAL Package
# world2Shp <- readOGR(".","vg2500_krs", verbose=T)
# names(world2Shp)
# world2Shp




################################################################################################
####EXTRACT 
mSMiExAvg<-extract(mSMI, worldPol,na.rm=T, fun=mean, weights=T, layer=540)
  ###das sieht unheimlich gut aus!!!!
  ###kommt als Large materix format

str(mSMiExAvg)
mSMiExAvg[,"X540"]
plot(mSMiExAvg[,"X540"])
mSMiExAvg[,"X540"]
View(mSMiExAvg)
names(mSMiExAvg)

write.csv(mSMiExAvg,"./data/data_raw/mSMiExAvg")  
###load
mSMiAVG<-as.matrix(read.csv("./data/data_raw/mSMiExAvg"))
mSMiExAvg<-mSMiAVG[,c(2:ncol(mSMiAVG))]

coordinates(mSMiExAvg) #scheinbar haben sich die Koordinaten verändert, irgendwie ist hier eine Null davor
coordinates(worldPol)

## Jetzt müssen die Daten wieder eine räumliche Zuordnung bekommen
worldPol
row.names(worldPol) # row names gehen bei Null los
row.names(mSMiExAvg)
worldPol
## das bedeutet, meine Data Frame benötigt die gleiche row names
mSMiExAvg_df<-as.data.frame(mSMiExAvg)
str(mSMiExAvg_df)
FIPS<-row.names(worldPol)
FIPS
length(FIPS)

# eventuell kann es problematisch sein, dass die row.names bei Null beginnen, da R scheibar die Indezierung bei eins beginnnt
#aber: row.names(worldPol) lassen sich nicht so einfach ändern

###spCbind um die Polygon mit data frame zu verbinden
row.names(mSMiExAvg_df)<-FIPS
row.names(mSMiExAvg_df)
mSMiExAvg_df
worldmSMiExAvg<-spCbind(worldPol, mSMiExAvg_df)
worldmSMiExAvg
plot(worldmSMiExAvg@data$X540)
summary(worldmSMiExAvg@data)
getwd()
writePolyShape(worldmSMiExAvg,"./data/data_raw/worldmSMiExAvg")

###Ähnliches geht wohl auch mit SpatialPolygonsDataFrame
worldmSMiExAvg2<-SpatialPolygonsDataFrame(worldPol, mSMiExAvg_df)
str(worldmSMiExAvg2, maxlevel=1)

# Examine Structur and content of shapefile
summary(worldmSMiExAvg)
str(as.data.frame(worldmSMiExAvg))
attributes(worldmSMiExAvg)
attributes(worldmSMiExAvg$data)
attributes(worldmSMiExAvg$data$X541)
worldmSMiExAvg@data$X540
# worldShp@data
# worldPol@data
# merge<-merge(worldPol, mSMiExAvg)
# str(merge)
# mergeShape<- merge(worldShp, mSMiExAvg)
# mergePol<- cbind(worldPol@data, mSMiExAvg)
# plot(mergeShape)
# 

#### Some simple maps

### Oslo SS Lecture 1, Olinda Example
names(worldmSMiExAvg)
str(worldmSMiExAvg)
spplot(worldmSMiExAvg, "X583", col.regions=grey.colors(20, 0.9, 0.3 ) )
#scheinbar passt was nicht mit den nrows
#vll gibt es auch ein Problem mit den IDS, sodass diese nicht matchen
nrow(worldmSMiExAvg$X541)
is.na(worldmSMiExAvg$X541) 
str(as(worldmSMiExAvg, "data.frame"))

# Mal die dbf file seperat anschauen
worldmSMiExAvg.dbf<-read.dbf(file.choose(), as.is = FALSE)


####Aus dem Oregon Tutorial
plotvar <- worldmSMiExAvg@data$X540 # gets data from shapefile .dbf

plotvar <- rank(worldmSMiExAvg$X540)
nclr <- 8
plotclr <- brewer.pal(nclr,"PuOr")
colornum <- cut(plotvar, nclr, labels=FALSE)
colorcode <- plotclr[colornum]
plot(worldmSMiExAvg, colorcode <- plotclr[colornum], xlab="Latitude", ylab="Longitude")



#############################################################################################################
####Rasterize World Shape
#world<-rasterize(world.shp, nrow=225, ncol=175, res=4000, xmin=4038000, xmax = 4738000, ymin = 5220000,ymax = 6120000, projection )


ext <- extent(4038000, 4738000, 5220000, 6120000)
worldRaster<- raster(ext, nrow=225, ncol=175)
worldRaster


worldShpRas<-rasterize(worldShp, worldRaster)
worldShpRas
View(worldShpRas)
writeRaster(worldShpRas, "/home/micha/Documents/frontend_share/projects/correlation/correlation/data/data_raw/WorldShpRas")
#################################################

# getwd()
str(world)
world
smi690
res(world)<- 4000


writeRaster(world, "testRaster2")
getwd()

world_pol<-readShapeSpatial(world)
extract<- extract(smi690, world)
str(world)
image(world)

SMI <- raster(SMI.slice1)
str(SMI)
print(SMI)
print(world.shp)
SMI
SMI.data<-as.data.frame(SMI)
filename(SMI)
hasValues(SMI)
inMemory(SMI)
cellStats(SMI, stat="min")
cellStats(SMI, stat="max")

#recode values < 0
SMI[SMI<0]<-0
#basic plot
levelplot(SMI)

# plot with outlines, a better color scale, no marginal plots
mapTheme <- rasterTheme(region=brewer.pal(8,"Greens"))
plt <- levelplot(SMI, margin=F,  par.settings=mapTheme)
warnings()
plt + layer(sp.lines(world.shp, col="gray", lwd=0.5))

# plot with outlines, a better color scale, marginal plots
mapTheme <- rasterTheme(region=brewer.pal(8,"Reds"))
plt <- levelplot(SMI, margin=T, par.settings=mapTheme)

plt + layer(sp.lines(world.shp, col="blue", lwd=0.9))

#The rasterVis package provides a couple of interesting Lattice-type 
#plots that can be used to visualize 3-D data  (usuallya function of  
#latitude, longitude and time).  The HovmÃ¶ller plot is a 2-D time/space
#diagram, where, for example, zonal or meridional averages are plotted 
#relative to time.  The horizon plot plots multiple time series 
#(here average values for individual latitudinal zones) in a way that 
#allows common trends to be visualized. 

# Hovmöller plots -- rasterVis example

# read a 3-D netCDF dataset 

SST <- brick(file.choose())

print(SST) # netCDF info
SST # raster info
filename(SST); hasValues(SST); inMemory(SST)

# setup add date information
idx <- seq(as.Date('1955-01-01'), as.Date('2014-12-1'), by='month')
idx
idx <- as.yearmon(idx)
SST <- setZ(SST, idx)
SST
names(SST) <- as.character(idx)
names(SST)

hovmoller(SST,
          at = do.breaks(c(0,1),10),
          contour=F, interpolate=F,
          par.settings=RdBuTheme(region=rev(brewer.pal(9,'RdBu'))),
          main="SMI GERMANY" )

# könnte tatsächlich sinnvoll in nen Paper sein

horizonplot(SST, ylab="Latitude", xlab="Time (counterintutive colors!)")

# 
# # flip sign to fix counterintuitive colors
# fun <- function(x) {-1.0*x}
# SST2 <- calc(SST, fun)
# 
# SST2 <- setZ(SST2, idx)
# names(SST2) <- as.character(idx)
# 
# horizonplot(SST2, ylab="Latitude", xlab="Time (red=increase, blue=decrease)")

# HADCRUT3 Combined Air Temperature/SST Anomalies
# read a 3-D netCDF dataset
hadcrut3 <- brick(file.choose()) # browse to air.mon.anom.nc
hadcrut3
filename(hadcrut3); hasValues(hadcrut3); inMemory(hadcrut3)

#Setup
idx <- seq(as.Date('1850-01-01'), as.Date('2013-12-01'), 'month')
idx <- as.yearmon(idx)
tmpplt <- setZ(hadcrut3, idx)
names(tmpplt) <- as.character(idx)

# plot
trellis.device('pdf', file='hov02.pdf')
hovmoller(tmpplt, dirXY=y,
          at = do.breaks(c(-2.5,2.5),10),
          contour=F, interpolate=F,
          par.settings=RdBuTheme(region=rev(brewer.pal(9,'RdBu'))),
          main="HadCRUT3 Anomalies, (1961-1990 base period)" )
dev.off()

# Example -- controls of global fire

# read a bioclimatic variable
# browse to cru10min30_bio.nc:
alpha=raster(SMI.slice2)
print(alpha)
alpha
levelplot(alpha)

mapTheme <- rasterTheme(region=brewer.pal(8,"BrBG"))
plt <- levelplot(alpha, margin=F, par.settings=mapTheme, main="AE/PE")
plt + layer(sp.lines(world.shp, col="gray", lwd=0.5))

# read burned fraction ltm
# browse to GFEDv3.1_ltm.nc:
burnf <- raster(file.choose(), varname="bf_ann_ave")
print(burnf)
burnf
levelplot(burnf)

mapTheme <- rasterTheme(region=brewer.pal(8,"YlOrRd"))
plt <- levelplot(burnf, margin=F, par.settings=mapTheme,
                 at = do.breaks(c(0,1),10),
                 main="Burned Fraction")
plt + layer(sp.lines(world.shp, col="gray", lwd=0.5))

logtrans <- function(x) {log10(x+0.00001)}
logburnf <- calc(burnf, logtrans)

mapTheme <- rasterTheme(region=brewer.pal(8,"YlOrRd"))
plt <- levelplot(logburnf, margin=F, par.settings=mapTheme,
                 at = do.breaks(c(-5,0),10),
                 main="log10 Burned Fraction")
plt + layer(sp.lines(world.shp, col="gray", lwd=0.5))

# read potential natural vegetation data
# browse to sage_veg30.nc:
vegtype <- raster(file.choose(), varname="vegtype")
print(vegtype)
vegtype
levelplot(vegtype)

# nicer map
mapTheme <- rasterTheme(region=rev(brewer.pal(8,"Greens")))
plt <- levelplot(vegtype, margin=F, par.settings=mapTheme,
                 #at = do.breaks(c(-5,0),10),
                 main="Potential Natural Vegetation")
plt + layer(sp.lines(world.shp, col="gray", lwd=0.5))

# get vegtype lables from netCDF file for later use
# reread sage_veg30.nc
veg.nc <- open.ncdf(file.choose()) # browse to sage_veg30.nc
vegkey <- att.get.ncdf(veg.nc,0,"key")
close.ncdf(veg.nc)
veg.nc
vegkey$value
vtype <- strsplit(vegkey$value,",")
vtype <- unlist(vtype)
vtype

# convert and merge rasters into a data frame
gfire2 <- cbind(as.data.frame(alpha, xy=TRUE),as.data.frame(logburnf),as.data.frame(vegtype))
alpha
logburnf
vegtype
tree
world.shp

names(gfire2) <- c("lon","lat","alpha","logburnf","vegtype")
head(gfire2)

# remove NAs
gfire2 <- na.omit(gfire2)
dim(gfire2)
head(gfire2)

## alternative approach:
# raster to rectangular data set using stack() and getValues()
gfire <- stack(alpha, logburnf, vegtype)
head(gfire)
names(gfire) <- c("alpha","logburnf","vegtype")
head(gfire)
dim(gfire)
gfire2 <- getValues(gfire)
gfire2
dim(gfire2)


# remove NAs and convert to a data frame
gfire2<- na.omit(gfire2)
dim(gfire2)
gfire2 <- as.data.frame(gfire2, xy=TRUE, centroids=TRUE)
head(gfire2)

# recode vegtype to a factor
vtypechar <- vtype[as.integer(gfire2$vegtype)]
gfire2$vfact <- factor(vtypechar, levels=vtype)


# a standard plot, with an added loess curve
# This takes a while.....

# plot data using transparent symbols
transp_black <- rgb(0,0,0,0.2)
plot(logburnf ~ alpha, data=gfire2, col=transp_black, pch=16, cex=0.5)

# fit a loess curve
loess.model <- loess(logburnf ~ alpha, data=gfire2, span=0.50,
                     degree=1)
loess.model
hat <- predict(loess.model)

lines(gfire2$alpha[order(gfire2$alpha)], hat[order(gfire2$alpha)],
      lwd=3, col="green")

# various lattice-type plots
# (which also take a little while...)

xyplot(logburnf ~ alpha | vfact, data=gfire2, pch=16, cex=0.5, col=transp_black)

histogram(~ logburnf | vfact, data=gfire2)

densityplot(~ logburnf | vfact, data=gfire2, plot.points=F, ref=T)

densityplot(~ logburnf[logburnf > -5] | vfact[logburnf > -5],
            data=gfire2, plot.points=F, ref=T)

stripplot(~ logburnf | vfact, data=gfire2,
          jitter.data=T, factor=10.0, pch=16, cex=0.5, col=transp_black)

# PNW cloudiness trends using 20th-Century Reanalysis v. 2 data

# browse to tcdc.eatm.mondm.mean.nc:
tcdc <- brick(file.choose(), varname="tcdc")
tcdc

# find the gridpoint closest to the Olympic peninsula
#targlat <- 40.0333; targlon <- 236.9000
targlat <- 47.67656; targlon = -124.01817 + 360
dm <- distanceFromPoints(tcdc, c(targlon,targlat))
levelplot(dm)
dmat <- as.matrix(dm)
closest.rc <- which(dmat == min(dmat), arr.ind=T)
closest.rc

# grid point for time series
col <- closest.rc[2]; row <- closest.rc[1]
col; row

# extract time series at grid point
cld <- tcdc[row, col, 1:nlayers(tcdc)]

dim(cld)
cld <- cld[1,]
head(cld)
plot(cld, type="l")

# convert to a time-series object
cld.ts <- ts(cld, frequency=12, start=c(1871,1))
attributes(cld.ts)
plot(cld.ts)

# fit a decomposition model
cld.stl <- stl(cld.ts, s.window=25, t.window=241, robust=T)
#summary(cld.stl)
plot(cld.stl)

# check irregular component for autocorrelation
#acf(cld.stl$time.series[,3])
Box.test(cld.stl$time.series[,3], lag = 1, type = "Ljung-Box")
