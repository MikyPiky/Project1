library(raster)
require(rgdal)
require(maptools)
require(rasterVis)
require(ncdf)#
requir7

# RasterLayer with the default parameters
x<-raster()

x
res(x)

#With other parameters
x<-raster(ncol=36,nrow=18, xmn=-1000, xmx=1000, ymn=-100, ymx=900)

x
res(x)
getValues(x)
res(x)<-100
res(x)
x

ncol(x)

#change number of col, affects res
ncol(x)<-18
x

#set the coordinate reference system(CRS), define the projection
projection(x)<-"+proj=utm +zone=48 +datum=WGS84"
x

r<-raster(ncol=10, nrow=10)
ncell(r)

hasValues(r)

#use the ′values′ function
values(r)<-1/ncell(r)
r
#or
set.seed(0)
values(r)<-runif(ncell(r))
hasValues(r)
inMemory(r)
head(values(r))

plot(r, main= "Raster with 100 cells")

hasValues(r)

res(r)

dim(r)

xmax(r)

#change the maximunm x coordinate of the extend (bounding box) of the RasterLayer

xmax(r)<-0
hasValues(r)
res(r)
plot(r)
dim(r)

ncol(r)<-6
hasValues(r)

plot(r)
#no values

res(r)
dim(r)
xmax(r)

#get te name of an example file installed with the package
#do not use this construction of your own files
filename<-system.file("external/test.grd", package="raster")
filename
r<-raster(filename)
filename(r)
hasValues(r)
inMemory(r)
plot(r, main="RasterLayer from file" )

# create three identical RasterLayer objects
r1 <- r2 <- r3 <- raster(nrow=10, ncol=10)
#Assign randowm cell values
rs=c(r1,r2,r3)

values(r1)<-runif(ncell(r1))
values(r2)<-runif(ncell(r2))
values(r3)<-runif(ncell(r3))

s<-stack(r1,r2,r3)
s
nlayers(s)
# combine three RasterLayer objects into a RasterBrick
b1 <-brick(r1,r2,r3)
#equivalent to
b2<-brick(s)

#create a RasterBrick from file
filename <-system.file("external/rlogo.grd",package="raster")

filename


b<-brick(file.choose())
#funktionier scheinabr auch mit mSMI.nc, 
#das ist sehr gut
nlayers(b)
b


b_old<-brick(file.choose())
b_old

#Resample new raster to extend of old
b_agg<-aggregate(b, b_old)
b_resample<-resample(b_agg,b_old)
###funktioniert gerade noch nicht

# extract a single layer
r<-raster(b, layer=600)
r
hasValues(r)
#r4<-raster(file.choose(), band=602)

plot(r, main="SMI for 600th timestep" )
#plot(r4)

image(r)
#sehr gut, diesmal ist es auch richtig rum angezeigt

###nun Versuche ich die gerKrs shapefile zu rasterizen

gerKrs<- readShapeLines(file.choose())
gerKrs
plot(gerKrs)
##Set up a raster "template" to use in rasterize()

ext <- extent(4038000, 4738000, 5220000, 6120000)
gerKrsRasTemp<-raster(ext, ncol=175, nrow=225)
gerKrsRasTemp
projection(gerKrsRasTemp)=CRS("+proj=longlat +datum=WGS84")
gerKrsRasTemp
gerKrsRas<-rasterize(gerKrs,gerKrsRasTemp )
gerKrsRas
gerKrsRas[]
#hier muss ich aufpassen, dass es keine Verzerrungen oder Ãhnliches gibt
#eventuell muss ich hier mit aggregate und resample function arbeiten

prr<-plot(gerKrsRas)
prr
pr<-levelplot(r)
RgesKrsRasBrick<-brick(r,gerKrsRas)
RgesKrsMerge<-merge(r,gerKrsRas)
#vll. bringt mich dieser Befehl ja schon weiter!?
RgesKrsStack<-
rr4
plot(rr4)


# jetzt muss ich noch schauen, wie ich die beiden Kombinieren kann
#Extract funktion
#majority function

###weiter mit Tutorial

### 4 Raster Algebra# 

#create an empty RasterLayer
ra <- raster(ncol=10, nrow=10)
#assign values to cells
values(ra)<-1:ncell(ra)
values(ra)
ncell(ra)
ra
plot(ra)
sa<-ra+10
values(sa)
sa<-sqrt(sa)
plot(sa)
sa<- sa* ra +5
values(sa)
plot(sa)
ra[] <- runif(ncell(ra))
ra[] #substitute for values function
values(ra)
ra<-round(ra)
ra[]
values(ra)
ra <-ra ==1 #logical values
ra[]

sa[ra]
sa[]

#use of replacement functions
sa[ra] <- -0.5
sa[]
sa[!ra] <-5
sa[]
sa[sa == 5]<- 15
sa[]

#objects with different numberts of layers
ra<-raster(ncol=5, nrow=5)

ra[]<-1
ra
sa<- stack(ra, ra+1)
q<-stack(ra, ra+2, ra+4, ra+6)
x<- ra+sa+q
x
##keep the longer layer

#Summary functions
aa<- mean(ra,sa,10)
ra[]
sa[]
aa[]
aa
ba<-sum(ra,sa)
ba[]
sta<-stack(ra,sa,aa,ba)

sta
sta[]

ssta<-sum(sta)
ssta
ssta[]

#single number summarizing zhe cell values of each layer
CsSta<-cellStats(sta,"sum")
CsSta[]
CssSta<-cellStats(ssta,"sum")
CssSta[]


####High level functions####

### Modifying a raster object
help(crop)
help(trim)
help(aggregate)
help(disaggregate)

ra1<-raster()
ra1[]<-1:ncell(ra1)
ra1
plot(ra1)
ra2<-aggregate(ra1,10) # resolution is now tenfold
ra3<-disaggregate(ra2,10)
ra3

plot(ra3)
ra4<-crop(ra3, extent(-180,0,0,30))
plot(ra4)
ra4

ra5<-crop(ra3, extent(-10,180,-20,10))
plot(ra5)

m<-merge(r,gerKrs, filename="test.grd", overwrite=TRUE)
plot(m)

help(flip)
help(rotate)
b_wrong<-raster(file.choose())
plot(b_wrong)
b_flip<-flip(b_wrong,2)
plot(b_flip)
b_rotate<-t(b_wrong)
plot(b_rotate)


###Overlay functions
help(overlay)

help(mask)
help(cover)#beide könnten sehr intressant sein, wenn ich mit lulc(i.e. corrine) arbeite

###Calc
help(calc)
help(stackApply) #könnte auch nützlich werden

###Reclassify
help(cut)
help(reclassify)
help(subs)

ra<-raster(ncol=3, nrow=2)
ra[]<-1:ncell(ra)
getValues(ra)
ra[]
values(ra)
sa<-calc(ra, fun=function(x){x[x<4]<-NA; return(x)})
sa[]
as.matrix(sa)
ta<-overlay(ra,sa,fun=function(x,y){x/(2*sqrt(y))+5})
as.matrix(ta)

ua<-mask(ra,ta)
as.matrix(ua)

va= ua==sa
as.matrix(va)

wa<-cover(ta,ra)
as.matrix(wa)

xa<-reclassify(wa, c(  c(0,2,1, 2,5,2, 4,10,3)))
as.matrix(xa)

ya<-subs(xa, data.frame(id=c(2,3), v=c(40,50)))
as.matrix(ya)

###Focal functions
help(focal) #stichwort: moving window
??focalFilter
help(focalNA)

###Distance
help(distance)
help(pointDistance)
?gridDistance
?direction
?adjacency
?pointDistance
??gdistance
install.packages("gdistance")

###Spatial configuration
help(clump)
?edge
?area  ###könnte Sinne machen in meiner Analyse

ra<-raster(nrow=45, ncol=90)
ra[]<- round(runif(ncell(r))*3)
ra[]
plot(ra)
aa<-area(ra)
plot(aa)
ra
aa
aa[]

?zonal  ###könnte Sinn machen in meiner Analyse
zonal(aa,ra,"mean")
cellStats(aa, stat="mean")

###Predctions
?predict ####könnte auch Sinn machen in meiner Analyse, meine Ähnliches bei Roberts Schlenker gelesen zu haben
?interpolate ###

### Vector to Raster Conversion
?rasterize
#polygon to raster conversion: creata a layer that canact as mask: summarize values on a raster by zone
#For polygons, values are transferred if the polygon covers the center of a raster cell. 
#For lines, values are transferred to all cells that are touched by a line. You can combine this behaviour by rasterizing polygons as lines first and then as polygons.
# wahrscheinlich ist extract function vorzuziehen, obwohl dort ähnliches passiert

### Summarizing functions
?freq ### Sinnvoll
?zonal ###Sinnvoll
?crosstab ###Sinnvoll

ra<- raster(ncol=36, nrow=18)
ra[]<-runif(ncell(ra))
ra[]
cellStats(ra, mean)

sa=ra
sa[]<-round(runif(ncell(ra))*5)
sa[]
zonal(ra,sa, "mean")
freq(ra)
freq(sa)
freq(sa, value= 3)
crosstab(ra*3, sa, long=T) 


###Plotting
?rasterImage
?plot 
?rasterVis ###könnte Sinn machen: lattice based plotting, facilatastes use of ggplot2
?click

ba <- brick(system.file("external/rlogo.grd", package="raster"))
plot(ba)

?plotRGB
plotRGB(ba, r=1, g=2, b=3)

###Writing files
#raster can read and write several raster file formats via the rgdal package
#but: directly reads and weites a native rasterfile format

###Helper functions
### werden sicherlich später noch relevant
ra<-raster(ncol=36, nrow=18)
ncol(ra)
nrow(ra)
?rowFromCell
rowFromCell(ra,100)
colFromCell(ra,100)
cellFromRowCol(ra,3,28)
xyFromCell(ra,100)
?xyFromCell
cellFromXY(ra, c(95,65))
?cellFromXY
colFromX(ra, 95)
rowFromY(ra, 65)

###Accessing cell values
ra <-raster(system.file("external/test.grd", package="raster"))
ra[] 
hasValues(ra)
ra
plot(ra)
View(ra)
View(as.matrix(ra))
va<-getValues(ra,50)
?getValues #returns values of a particular row
va[]

str(va)
View(va)
va[35:39]

getValuesBlock(ra,50,5,35,5) #1. position der Zeile, von da 5 Zellen einschließlich, dann gleiches mit Spalte
?getValuesBlock

?extract
#-extract values by cell number
cells<-cellFromRowCol(ra,50, 35:39)
cells

extract(ra, cells)

#now with coordinates
xy = xyFromCell(ra, cells) 
xy
extract(ra, xy)

# extract values with points
rb<-raster(ncol=36, nrow=18)
rb[]<-1:ncell(rb)
rb
plot(rb)
View(as.matrix(rb))


xbyb<- cbind(-50, seq(-80,80,by=20))
xbyb                    
                    
extract(rb,xbyb)
cellFromXY(rb, c(-50,-80))
#Dieser Ansatz scheint dem Coordinates Ansatz vorher sehr ähnlich zu sein

?SpatialPoints #hier kann noch eine CRS Projektion hinzugeügt werden
spb<-SpatialPoints(xbyb)
spb

spa<-SpatialPoints(xy)
spa

extract(rb, spb)
extract(ra, spa)

#examples with a buffer
extract(rb, spb, buffer=1000000)
extract(rb, spb, buffer=1000000, fun = mean)

# illustrating the varying size of a buffer (expressed in meters) on a long/lat raster
z<- extract(rb, spb, buffer=1000000)

z
z[[1]]

1:length(z)
s<- raster(rb)

s[z[[1]]]<-1
s[z[[1]]]
for (i in 1:length(z)){s[z[[i]]]<-i}
plot(s)

#compare with raster that is not longitutde/latitude
projection(rb) <- "+proj=utm +zone=17"
rb
plot(rb)
xbyb
xbyb[,1]<-50,
xbyb
z<-extract(rb, xbyb, buffer=8)
z
for (i in 1:length(z)){s[z[[i]]]<-i}
plot(s)

library(maptools)
data(wrld_simpl)
plot(wrld_simpl, add=TRUE)

# extract values with lines

?rbind
cds1 <- rbind(c(-50,0), c(0,60), c(40,5), c(15,-45), c(-10,-25))
cds2 <- rbind(c(80,20), c(140,60), c(160,0), c(140,-55))
?SpatialLines
lines <- SpatialLines(list(Lines(list(Line(cds1)), "1"), Lines(list(Line(cds2)), "2") ))
lines

plot(rb, add=T)
plot(lines, add=T)
extract(rb, lines)

#extract values with polygons

cds3 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds4 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys<- SpatialPolygons(list(Polygons(list(Polygon(cds3)),1),Polygons(list(Polygon(cds4)),2)))
polys
plot(polys)
rb
plot(rb)
plot(polys,add=T)
v<- extract(rb, polys)
v

#mean for each polygon

unlist(lapply(v, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA)),

v<- extract(rb, polys, cellnumbers=T)
v

#weighted mean

v<-extract(rb, polys, weights=T, fun=mean)
v

#equivalent to
v<- extract(rb, polys, weights=T)
v
sapply(v, function(x) if (!is.null(x)) {sum(apply(x,1, prod))/ sum(x[,2])} else NA)

# extract values with an extent
e<- extent(150,170,-60,-40)
extract(rb, e)
plot(rb)
plot(e, add=T)

# standard R indexing
r<-raster(system.file("external/test.grd", package="raster"))

#raster is presented as (one-dimensional) vector
cells
r[cells]
r[1:4]
filename(r)
r[2:3]<-10
r[1:4]
filename(r)

#Values can be insprectes using a (two-dimensional) matrix noatation
#first index row, second index col
r[1]
r[2,2]
as.matrix(r)
r[1,]
r[,2]
r[1:3,1:3]
#keep the matrix structure
r[1:3,1:3, drop=F]

##indexing less efficient compare to function as getValues inside of functions

#### Coercion to objects of other classes
?SpatialGrid
??im


r1<- raster(ncol=36, nrow=18)
r2<-r1

r1[]<-runif(ncell(r1))
r2[]<-runif(ncell(r1))
s<-stack(r1,r2)
s
sgdf<- as(s, "SpatialGridDataFrame")
sgdf
newr2<-raster(sgdf,2)
newr2
news<-stack(sgdf)
news

#Extending raster objects

setClass ("myRaster", 
            contains = "RasterLayer",
          
            representation (
              
                important = "data.frame",
              
                essential = "character"
              
            ) ,
          
            prototype (
              
                important = data.frame(),
              
                essential = ""
              
            )
          ) 
r = raster(nrow=10, ncol=10) 
r
m <- as(r, "myRaster") 
m
m@important <- data.frame(id=1:10, value=runif(10)) 
m@essential <- "my own slot" 
m[] <- 1:ncell(m)
setMethod ("show" , "myRaster", 
               function(object) {
                 
                   callNextMethod(object) # call show(RasterLayer)
                 
                   cat("essential:", object@essential, "\n")
                 
                   cat("important information:\n")
                 
                   print( object@important)
                 
               })
m