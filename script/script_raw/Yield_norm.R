#### File Description ####
' # Read out the normalized Yield for all the crops from the ncdf file generated with the program myread written in Fortran
    - Unterschiedliche 
  # Mergen der Daten mit der Datei data/data_processed/Yield_SMI_long <- MergeSMI_Yield (starts in 1999)
  - Unt
'

#### Input and Dependencies ####
' - normalized  (kumulative kernel density) Yield: data/data_processed/norm.nc 
  - data/data_processed/Yield_SMI<- MergeSMI_Yield (starts in 1995)

'

#### Results are not satisfying ####
'
Die Daten haben sich kaum verändert. 
Genrelles Problem: Da eine Density function um den Wert Null gelegt wird (diese Density Funktionen werden später addiert), aber der Wertbereich nicht negativ ist, 
also nach unten Beschränkt, ist der Wert des ausgegeben Quantiles
halb so groß, wie man intuituv erwarten würde. Also bei 5 von 12 Null Werten wird ein Wert um 0.4 erwartet, aber es wird ein Wert um 0.2 ausgegeben. 
An der generellen Problemstellung ändert sich auch nichts, wenn man den Silverman Bandwidth Alogrithmus verwendet.
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

##############################################################################################################################################################################################

################################
#### Laden des Yield NetCDF ####
################################

#### Laden mit ncdf4: mit Silverman Optimierung ####
Yield_norm_open <- nc_open("/home/peichl/Documents/projects/correlation/data/data_processed/norm_opt.nc")
print(Yield_norm_open)

Yield_norm_open$var[2]$yield_cdf # Ich sollte das Procedure erstmal mit yield machen, um die Ergebnisse dann vergleichen zu können mit der orginal yield daten. 

Yield_norm_opt <- ncvar_get(Yield_norm_open, varid="yield")
Yield_norm_cdf_opt <- ncvar_get(Yield_norm_open, varid="yield_cdf")


nc_close(Yield_norm_open)

#### Laden mit ncdf4:old ####
Yield_norm_open_old <- nc_open("/home/peichl/Documents/projects/correlation/data/data_processed/norm_old.nc")
print(Yield_norm_open_old)

Yield_norm_open_old$var[2]$yield_cdf # Ich sollte das Procedure erstmal mit yield machen, um die Ergebnisse dann vergleichen zu können mit der orginal yield daten. 

Yield_norm_old <- ncvar_get(Yield_norm_open_old, varid="yield")
Yield_norm_cdf_old <- ncvar_get(Yield_norm_open_old, varid="yield_cdf")


nc_close(Yield_norm_open_old)

#### Betrachten und vergleichen der Netcdf Frames ####
View(Yield_norm_cdf_opt)
View(Yield_norm_cdf_old)

Yield_norm_cdf_opt%in%Yield_norm_cdf_old # Silverman Optimierung der Bandbreite hat die Daten verändert

all(Yield_norm_opt%in%Yield_norm_old) # Die absoluten Werte sind weiterhin gleich



########################################################################################################################################################################################################
####################################################### Use of Yield_norm instead of Yield_norm_cdf for Validation Reasons #######################################################################
u<-Yield_norm== -9999.0
Yield_norm[u]<-NA

dim(Yield_norm)
class(Yield_norm)
levelplot(Yield_norm)

print(Yield_norm)



## Auslesen der einzelenen Dimensionen aus dem netcdf Format
Yield_norm[1, ,] # gibt eine Matrix mit den Dimensionen Crop und Zeit aus, die Werte sind jeweils die ersten Werte der verschiedenen Crops für das County 1

Yield_norm[,1 ,] # gibt eine Matrix mit den Dimensionen County (y) und Zeit (x) aus, die Werte sind winterwheat:: Crop 1

Yield_norm[, ,1] # gibt eine Matrix mit den Dimensionen County (y) und Crop (x) aus, die Werte sind die Crops für das Jahr 1


## Loop, um Daten so zu formatieren, sodass sie dem tidy format entsprechen
crops <- NULL
crops_all <- NULL

for (j in 1:10)
{
for (i in 1:12)
    { 
      
      crops <- append(crops, Yield_norm[,j,][,i])
       
    } 
  crops_all <- cbind(crops_all, crops)   
  crops <- NULL
}

 crops_all<-as.data.frame(crops_all)
 
names(crops_all) <- list("winterWheat_norm" ,"rye_norm" ,"winterBarley_norm" ,"summerBarley_norm", "oats_norm" ,"triticale_norm" ,"potatoes_norm", "sugarBeet_norm" ,"winterRape_norm" ,"siloMaize_norm")

View(crops_all)


## Verbinden der Daten mit dem Datensatz SMI_Yield

## Load SMI_Yield
Yield_SMI <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI")
Yield_SMI<-Yield_SMI[order(Yield_SMI$year, Yield_SMI$comId),]
head(Yield_SMI, 20)
tail(Yield_SMI, 20)
Yield_SMI$X<-1:4920
dim(Yield_SMI)
dim(crops_all)

Yield_norm_SMI <- cbind(Yield_SMI, crops_all)
head(Yield_norm_SMI)
Yield_norm_SMI[c(3500:3600),"winterWheat"]
Yield_norm_SMI[c(3500:3600),"winterWheat_norm"]


as.integer(Yield_norm_SMI[c(3500:3600),"winterWheat"])%in%as.integer(Yield_norm_SMI[c(3500:3600),"winterWheat_norm"]) 
## Kommentar :Es gibt wohl leichte Abweichungen bei der Präzisierung der Nachkomma stellen. Das liegt sicherlich daran, dass
## im Fortran Programm eine eigene Präzisierung stattfindet. Bei integer Betrachtung ist es daher kein Problem
all(as.integer(Yield_norm_SMI[,"winterWheat"])%in%as.integer(Yield_norm_SMI[,"winterWheat_norm"]) )

#### Interpretation ####
'Da eine Validierung nun sowohl technisch als auch per eyeballing stattgefunden hat, kann ich nun die yield mit den yield_norm Daten austauschen'

########################################################################################################################################################################################################
############################################ Use of Yield_norm_cdf_old Data now ############################################################################################################################
## Hier benutze ich die den mit old markierten Datensatz. Diese sind die nicht Silverman Optimierten Bandwidth in der kernel cdf estimation

u<-Yield_norm_cdf_old == -9999.0
Yield_norm_cdf_old [u]<-NA

dim(Yield_norm_cdf_old )
class(Yield_norm_cdf_old )
levelplot(Yield_norm_cdf_old )

print(Yield_norm_cdf_old )

## Auslesen der einzelenen Dimensionen aus dem netcdf Format
Yield_norm_cdf_old [1, ,] # gibt eine Matrix mit den Dimensionen Crop und Zeit aus, die Werte sind jeweils die ersten Werte der verschiedenen Crops für das County 1

Yield_norm_cdf_old [,1 ,] # gibt eine Matrix mit den Dimensionen County (y) und Zeit (x) aus, die Werte sind winterwheat:: Crop 1

Yield_norm_cdf_old [, ,1] # gibt eine Matrix mit den Dimensionen County (y) und Crop (x) aus, die Werte sind die Crops für das Jahr 1


## Loop, um Daten so zu formatieren, sodass sie dem tidy format entsprechen
crops <- NULL
crops_all_old <- NULL

for (j in 1:10)
{
  for (i in 1:12)
  { 
    
    crops <- append(crops, Yield_norm_cdf_old [,j,][,i])
    
  } 
  crops_all_old <- cbind(crops_all_old, crops)   
  crops <- NULL
}

crops_all_old<-as.data.frame(crops_all_old)

names(crops_all_old) <- list("winterWheat_norm" ,"rye_norm" ,"winterBarley_norm" ,"summerBarley_norm", "oats_norm" ,"triticale_norm" ,"potatoes_norm", "sugarBeet_norm" ,"winterRape_norm" ,"siloMaize_norm")

View(crops_all_old)


## Verbinden der cops_all_old (non Silverman) Daten mit dem Datensatz SMI_Yield ##
## Load SMI_Yield
Yield_SMI <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI")
Yield_SMI<-Yield_SMI[order(Yield_SMI$year, Yield_SMI$comId),]
head(Yield_SMI, 20)
tail(Yield_SMI, 20)
Yield_SMI$X<-1:4920
dim(Yield_SMI)
dim(crops_all_old)

Yield_norm_cdf_old_SMI <- cbind(Yield_SMI, crops_all_old)
Yield_norm_cdf_old_SMI <- Yield_norm_cdf_old_SMI[order(Yield_norm_cdf_old_SMI$year, Yield_norm_cdf_old_SMI$comId),]
dim(Yield_norm_cdf_old_SMI)
head(Yield_norm_cdf_old_SMI)
tail(Yield_norm_cdf_old_SMI)

####################################################################
#### Betrachten eininger interessanter Landkreise für Triticale ####

Yield_norm_cdf_old_SMI$triticale[Yield_norm_cdf_old_SMI$comId==1001]
Yield_norm_cdf_old_SMI$triticale_norm[Yield_norm_cdf_old_SMI$comId==1001]
summary(Yield_norm_cdf_old_SMI$triticale_norm[Yield_norm_cdf_old_SMI$comId==1001])
plot(  Yield_norm_cdf_old_SMI$triticale_norm[Yield_norm_cdf_old_SMI$comId==1001])

Yield_norm_cdf_old_SMI$triticale[Yield_norm_cdf_old_SMI$comId==1002]
Yield_norm_cdf_old_SMI$triticale_norm[Yield_norm_cdf_old_SMI$comId==1002]
summary(Yield_norm_cdf_old_SMI$triticale_norm[Yield_norm_cdf_old_SMI$comId==1002])
plot(Yield_norm_cdf_old_SMI$triticale_norm[Yield_norm_cdf_old_SMI$comId==1002])

Yield_norm_cdf_old_SMI$triticale[Yield_norm_cdf_old_SMI$comId==1003]
Yield_norm_cdf_old_SMI$triticale_norm[Yield_norm_cdf_old_SMI$comId==1003]
plot(Yield_norm_cdf_old_SMI$triticale_norm[Yield_norm_cdf_old_SMI$comId==1003])
summary(Yield_norm_cdf_old_SMI$triticale_norm[Yield_norm_cdf_old_SMI$comId==1003])


Yield_norm_cdf_old_SMI$triticale[Yield_norm_cdf_old_SMI$comId==1004]
Yield_norm_cdf_old_SMI$triticale_norm[Yield_norm_cdf_old_SMI$comId==1004]
plot(Yield_norm_cdf_old_SMI$triticale_norm[Yield_norm_cdf_old_SMI$comId==1004])

## Interpretation ##
'
ComID 1003 lässt sich cdf Normierung schwer nachvollziehen. Daher sollte Silverman Optimierung der Bandwidth angewandt werden.
'


########################################################################################################################################################################################################
############################################ Use of Yield_norm_cdf_opt Data now ############################################################################################################################
u<-Yield_norm_cdf_opt== -9999.0
Yield_norm_cdf_opt[u]<-NA

dim(Yield_norm_cdf_opt)
class(Yield_norm_cdf_opt)
levelplot(Yield_norm_cdf_opt)

print(Yield_norm_cdf_opt)

## Auslesen der einzelenen Dimensionen aus dem netcdf Format
Yield_norm_cdf_opt[1, ,] # gibt eine Matrix mit den Dimensionen Crop und Zeit aus, die Werte sind jeweils die ersten Werte der verschiedenen Crops für das County 1

Yield_norm_cdf_opt[,1 ,] # gibt eine Matrix mit den Dimensionen County (y) und Zeit (x) aus, die Werte sind winterwheat:: Crop 1

Yield_norm_cdf_opt[, ,1] # gibt eine Matrix mit den Dimensionen County (y) und Crop (x) aus, die Werte sind die Crops für das Jahr 1


## Loop, um Daten so zu formatieren, sodass sie dem tidy format entsprechen
crops <- NULL
crops_all_opt <- NULL

for (j in 1:10)
{
  for (i in 1:12)
  { 
    
    crops <- append(crops, Yield_norm_cdf_opt[,j,][,i])
    
  } 
  crops_all_opt <- cbind(crops_all_old, crops)   
  crops <- NULL
}

crops_all_opt<-as.data.frame(crops_all_opt)

names(crops_all_opt) <- list("winterWheat_norm_opt" ,"rye_norm_opt" ,"winterBarley_norm_opt" ,"summerBarley_norm_opt", "oats_norm_opt" ,"triticale_norm_opt" ,"potatoes_norm_opt",
                             "sugarBeet_norm_opt" ,"winterRape_norm_opt" ,"siloMaize_norm_opt")

View(crops_all_opt)


## Verbinden der cops_all_opt (Silverman) Daten mit dem Datensatz SMI_Yield ##
Yield_norm_cdf <- cbind(Yield_norm_cdf_old_SMI, crops_all_opt)
Yield_norm_cdf <- Yield_norm_cdf_opt_SMI[order(Yield_norm_cdf_opt_SMI$year, Yield_norm_cdf_opt_SMI$comId),]
dim(Yield_norm_cdf)
head(Yield_norm_cdf)
tail(Yield_norm_cdf)

####################################################################
#### Betrachten eininger interessanter Landkreise für Triticale ####

## ComId 1001
Yield_norm_cdf$triticale[Yield_norm_cdf$comId==1001]
Yield_norm_cdf$triticale_norm[Yield_norm_cdf$comId==1001]
Yield_norm_cdf$triticale_norm_opt[Yield_norm_cdf$comId==1001]

summary(Yield_norm_cdf$triticale_norm[Yield_norm_cdf$comId==1001])
plot(  Yield_norm_cdf$triticale_norm[Yield_norm_cdf$comId==1001])


## ComId 1002
Yield_norm_cdf$triticale[Yield_norm_cdf$comId==1002]
Yield_norm_cdf$triticale_norm[Yield_norm_cdf$comId==1002]
Yield_norm_cdf$triticale_norm_opt[Yield_norm_cdf$comId==1002]

summary(Yield_norm_cdf$triticale_norm[Yield_norm_cdf$comId==1002])
par(mfrow=c(2,1))
plot(Yield_norm_cdf$triticale_norm[Yield_norm_cdf$comId==1002])
plot(Yield_norm_cdf$triticale_norm_opt[Yield_norm_cdf$comId==1002])

## ComId 1003
Yield_norm_cdf$triticale[Yield_norm_cdf$comId==1003]
Yield_norm_cdf$triticale_norm[Yield_norm_cdf$comId==1003]
Yield_norm_cdf$triticale_norm_opt[Yield_norm_cdf$comId==1003]


plot(Yield_norm_cdf$triticale_norm[Yield_norm_cdf$comId==1003])
plot(Yield_norm_cdf$triticale_norm_opt[Yield_norm_cdf$comId==1003])
summary(Yield_norm_cdf$triticale_norm[Yield_norm_cdf$comId==1003])

## ComId 1004
Yield_norm_cdf$triticale[Yield_norm_cdf$comId==1004]
Yield_norm_cdf$triticale_norm[Yield_norm_cdf$comId==1004]
plot(Yield_norm_cdf$triticale_norm[Yield_norm_cdf$comId==1004])

## Interpretation ##
'
Die Daten haben sich kaum verändert. 
Genrelles Problem: Da eine Density function um den Wert Null gelegt wird (diese Density Funktionen werden später addiert), aber der Wertbereich nicht negativ ist, 
also nach unten Beschränkt, ist der Wert des ausgegeben Quantiles halb so groß, wie man intuituv erwarten würde. Also bei 5 von 12 Null Werten wird ein Wert um 0.4 erwartet, aber es wird ein Wert um 0.2 ausgegeben. 
An der generellen Problemstellung ändert sich auch nichts, wenn man den Silverman Bandwidth Alogrithmus verwendet.
'

