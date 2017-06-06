##########################
#### Cross Validation ####
###########################
####  SiloMaize in July ####
###########################
' Daten wurden am 23.2.2016 vom Cluster geladen'

library("plm")
library("boot")
library("gtools")
library("lme4")
library(lmtest)
library(car)

## Read data frame with siloMaize as only depedent variable ##
# Yield_Covariates <- read.csv( "/home/peichl/projects/crossvalid/siloMaize/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv")

Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv")
head(Yield_Covariates)

Yield_Covariates$X <- NULL
names(Yield_Covariates)
head(Yield_Covariates)

levels(Yield_Covariates$com)

## For publication worth regression output need to change data names ##
# Get rid of variables which are not necessary
names(Yield_Covariates)
names <- names(Yield_Covariates)

################
################
names_Jul <- grep(c("*Jul"), names)
names_Jul

Yield_Covariates_Jul <- Yield_Covariates[,names_Jul]
names(Yield_Covariates_Jul)
Yield_Covariates_Jul$silo_Maize <- NULL  # Nur bei Silomaize im Mai notwendig
dim(Yield_Covariates_Jul)

## Delete all but SMI, Prec, Tavg and Pet
names(Yield_Covariates_Jul)
Yield_Covariates_Jul <- Yield_Covariates_Jul[,c(1:4)]

## Establish first part of data frame ##
Yield_Covariates_SM <- Yield_Covariates[,c(2,1,3:5,7)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_SM_Jul <- cbind(Yield_Covariates_SM, Yield_Covariates_Jul)
names(Yield_Covariates_SM_Jul)
names(Yield_Covariates_SM_Jul) <- c( "comId" , "year","com","idState","State","siloMaize","SMI", "Prec","Tavg", "Pet")
names(Yield_Covariates_SM_Jul)


#################################
#### Define comId as factors ####
class(Yield_Covariates_SM_Jul$comId)
Yield_Covariates_SM_Jul$comId <- as.factor(Yield_Covariates_SM_Jul$comId )
head(Yield_Covariates_SM_Jul$comId)


## Drought Monitor Spezification ##
Yield_Covariates_SM_Jul$SMI_GDM <- cut(Yield_Covariates_SM_Jul$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))

## Change Indexing so that it can be used in plm package
Yield_Covariates_SM_Jul <- plm.data(Yield_Covariates_SM_Jul, index=c("comId", "year"))
str(Yield_Covariates_SM_Jul)



# ## Remove linear trend ##
# 'Fit yield on time and use the residuals of that for detrended yields'
# 
# plot(Yield_Covariates_SM_Jul$siloMaize ~ as.integer(Yield_Covariates_SM_Jul$year))
# lineartrend <- lm(siloMaize ~ as.integer(year), data= Yield_Covariates_SM_Jul )
# summary(lineartrend)
# abline(lineartrend)
# abline(453.5511,0, col="green")
# length(resid(lineartrend))
# Yield_Covariates_SM_Jul$siloMaize_lintren <- resid(lineartrend)
# 
# formula_Jul_sm_lintren  <- siloMaize_lintren ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
#   dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) 

## Na-omit ##
head(Yield_Covariates_SM_Jul)
sum(is.na(Yield_Covariates_SM_Jul) )
dim(Yield_Covariates_SM_Jul)
Yield_Covariates_SM_Jul_nna <- na.omit(Yield_Covariates_SM_Jul) 
dim(Yield_Covariates_SM_Jul_nna)
any(is.na(Yield_Covariates_SM_Jul_nna))
rownames(Yield_Covariates_SM_Jul_nna) <- NULL
head(Yield_Covariates_SM_Jul_nna)

Yield_Covariates_SM_Jul <- Yield_Covariates_SM_Jul_nna

## Remove log trend ##
'Fit log of yield on log of time and use the residuals of that for yields'
plot(log(Yield_Covariates_SM_Jul$siloMaize) ~ log(as.integer(Yield_Covariates_SM_Jul$year)))
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Jul)
abline(logtrend, col="green")
summary(logtrend)
abline(6.1115,0)
Yield_Covariates_SM_Jul$siloMaize_logtrend <- resid(logtrend)


## Attach Data Set ##
head(Yield_Covariates_SM_Jul)
attach(Yield_Covariates_SM_Jul)


##########################################################################
#### BIC to choose the degrees of the polynomials and natural splines ####
##########################################################################

## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

formula_Jul_sm_detrendlog <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

## Print formula
formula_Jul_sm_detrendlog

#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################


####################################################################################################
### Loop over two variables - Tavg and Prec - to dertermine their polygons starting with 8 grades ##

##  Define empty lists for loop ##
# cv.error <- rep(0,9)
# cv.error
BIC <- rep(0,9)
BIC


print("start loop")

for(r in 1:9){
  glm.fit <- glm(formula = formula_Jul_sm_detrendlog,  data = Yield_Covariates_SM_Jul) 
  
  BIC[r] <- BIC(glm.fit)
  #   cv.error[r] <- cv.glm( Yield_Covariates_SM_Jul, glm.fit )$delta[1] # hat funktioniert (dummies dürfen nicht nur als Faktor definiert werden)
  #   save.image(file = "/home/peichl/projects/crossvalid/siloMaize/7.RData")
}



# par(mfrow=c(2,2))
par(mfrow=c(1,1))
## Compare with crossvalidation results of 
# plot(cv.error_loocv_2v8poly_Prec)  # aus ursprünlichen, crossvalidated run
# plot(BIC_loop_2v8poly_Prec)  # aus ursprünlichen, crossvalidated run
plot(BIC)


which.min(BIC)
# which.min(cv.error_loocv_Jul_SiloMaize) 

# ich denke, dass hier Model Nummer 5 am besten ist
r = r_Jul = 5


####################
## GLM Ergebnisse ##
glm.fit2 <- glm(formula = formula_Jul_sm_detrendlog,  data =  Yield_Covariates_SM_Jul) 
'Caveat: Hier werden aufgrund von singulariät eine Menge NAs in den Fixed Effekten produziert.'

summary(glm.fit2)
coefficients(glm.fit2)[1:15]

## plot ##
plot(glm.fit2)
' There is an issue with leverage and outliers'

################
### Outliers ###
################

## Outliers via rstudent Verteilung ##
plot(rstudent(glm.fit2))
rstd <- rstudent(glm.fit2)
dim( Yield_Covariates_SM_Jul)
head(Yield_Covariates_SM_Jul)
summary(Yield_Covariates_SM_Jul$siloMaize)
hist(Yield_Covariates_SM_Jul$siloMaize)


Yield_Covariates_SM_Jul[1317,]
Yield_Covariates_SM_Jul[771,]
Yield_Covariates_SM_Jul[847,]
' Diese Werte sind in Silomaize nicht nachzuvollziehen und sollten daher gelöscht werden, dass es wohl Messfehler sind.'
' Hier werden die gleichen Outlier wie für Mai angegeben. Daher sollten diese definitiv immer gelöscht werden'


## Generate data without outliers rstd > |5|
na.omit(Yield_Covariates_SM_Jul[rstd > 6 | rstd < -6,])  #11)

## Anschauem der Struktur der coms, welche hier ausgegeben wurden: hier sind vor allem die SiloMaize Werte relevant ##
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "5315",] # hier scheint nur ein Jahr ein Messfehler zu sein
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "5378",] # sollte ich insgesamt rausnehmen, da die Meldungen nicht valide scheinen. 
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "6532",] # hier scheint nur ein Jahr ein Messfehler zu sein 
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "12052",] # unvollsätändige Berichterstattung, sollte ich insgesamt rausnehmen
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "12053",] # hier scheint nur ein Jahr ein Messfehler zu sein
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "12069",] # hier mache ich am besten nichts
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "14730",] # hier mache ich auch am besten nichts

## Generate data without outliers rstd > |6.5|
na.omit(Yield_Covariates_SM_Jul[rstd > 6 | rstd < -6,])  
dim(Yield_Covariates_SM_Jul)
temp <-  Yield_Covariates_SM_Jul[!rstd > 6 & !rstd < -6,]
dim(temp)

## Löschen vom zweifelhaften Landkreisen ##
dim(temp)
temp <- temp[!temp$comId == "5378",] # Rheinisch Bergischer Kreis
dim(temp)
temp <- temp[!temp$comId == "12052",] # Cottbus
dim(temp)

## Generate data without outliers rstd > |6| and dubious coms (systematic measurement errors)
temp <- na.omit(temp)
rownames(temp) <- NULL

## GLM with no outliers rstd > |6| and dubious coms (systematic measurement errors) ##
r=5
glm.fit3 <- glm(formula = formula_Jul_sm_detrendlog,  data = temp)
summary(glm.fit3)
'Achtung, hier werden wieder manchen Fixed Effects aufgrund von Singularities zu NAs'

## Determine min BIC for model without rstd > |6| ##
BIC_temp <- rep(0,9)
for(r in 1:9){
  glm.fit3 <- glm(formula = formula_Jul_sm_detrendlog,  data = temp) 
  BIC_temp[r] <- BIC(glm.fit3)
}
par(mfrow = c(2,1))
plot(BIC_temp,ylab="BIC without outliers" )
plot(BIC)
which.min(BIC_temp)
r = 5
glm.fit3 <- glm(formula = formula_Jul_sm_detrendlog,  data = temp)
summary(glm.fit3)
coeftest(glm.fit3)[1:12,]

plot(glm.fit3) # Observations with leverage larger than one:181,763,1008

## Untersuch outlier erneut ##
plot(rstudent(glm.fit3))
rstd2 <- rstudent(glm.fit3)
na.omit(temp[rstd2 > 6 | rstd2 < -6,])  

temp[temp$comId==12069,] # Lasse ich drin, dass es ein nachvollziehbarer outlier ist
## Interpretation ##
' Die outlier Sache könnte mann auch nach vorne Stellen, da diese für alle Datensätze gilt. Dies hat sich hier bestätigt'

################
### Leverage ###
################

######################################################################
# Explore values which are shown within warning of plot() function ##
plot(glm.fit3)
' here is no warning function'

plot(glm.fit3, which=4)
#
# dim(temp)
# ## Explore Variables leverage größer 1 ##
# temp[c(181,763,1008),]
# # Explore comIDs
# temp[temp$comId == "3101",] # jeweils nur eine Beobachtung 763
# temp[temp$comId == "5314",] # jeweils nur eine Beobachtung 1008
# temp[temp$comId == "5911",] # jeweils nur eine Beobachtung 181
# 
# ## Delete Variabels with leverage large than one ##
# temp1<-  temp[-c(181,763,1008),]
# dim(temp1)
# temp1 <- na.omit(temp1)
# rownames(temp1) <- NULL


## Da ich diese Stufe nicht durchführe (es gibt keine Warnung im plot Befehl) benutzte ich weiterhin den alten Datensatz
temp1 <- temp

############################################################
## GLM with less influential values (leverage größer 1 -> in plot() warnings ) ##
glm.fit4<- glm(formula = formula_Jul_sm_detrendlog,  data = temp1)
summary(glm.fit4)
plot(glm.fit4) 

############################################################
## Cooks Distance as measure of influential observations ##

## Show plot with Cooks Distance ##
plot(glm.fit4, which=4)

## Delete observations with high Cooks Distance
temp1[c( 3217,3334,3681),]

####################
## Explore comIds ##
temp[temp$comId == "12053",] 
temp[temp$comId == "12069",] 
temp[temp$comId == "15001",] 
' Hier sehe ich keine besonderen Gründe, warum man diese Daten löschen sollte. Höchstens, dass es hier erst Beonachtungen ab 2006 gibt.'

temp2<-  temp1[-c(3217,3334,3681),]
temp2 <- na.omit(temp2)
rownames(temp2) <- NULL

## GLM with even less influential values (großer Cooks Distance ) ##
glm.fit5<- glm(formula = formula_Jul_sm_detrendlog,  data = temp2)
summary(glm.fit5)
coeftest(glm.fit5)
' Hier gibt es keine NAs mehr, wahrscheinlich aufgrund der Werte mit weniger leverage'
plot(glm.fit5)


## Show plot with Cooks Distance ##
plot(glm.fit5, which=4)

## Cooks Distance as measure for influence(combination of outlier and leverage)
# cook <- cooks.distance(glm.fit5)
# com <- temp2$com
# year <- temp$year
# library(faraway)
# halfnorm(cook, 3)
# which.max(cook)
# library(car)
# plot(cookd(glm.fit5))
# identify(1:50, cooks.distance(glm.fit5))



# ## Combine leverage and outlier statistic ##
# plot(hatvalues(glm.fit5), rstudent(glm.fit5))
' Gleiche Grafik letzte aus dem plot() Befehl'


## GLM with less leverage ##

coefficients(glm.fit3)[1:10] # mit temp
coefficients(glm.fit4)[1:15] # mit temp1
coefficients(glm.fit5)[1:15] # mit temp2
' Vor allem das weglassen der Punkte mit Leverage größer 1 und großer Cooks Distance hat einen Einfluss auf die Coefficienten'

####################
## PLM Ergebnisse ##
plm.fit_Jul1 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = Yield_Covariates_SM_Jul,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul1)

# no outliers
plm.fit_Jul2 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul2)
' Hier gibt es relevante Änderung zum Model vorher. Es macht wohl wirklich Sinn die Outlier rauszunehmen. Sowohl der min_BIC als auch die Coefficienten und Std. Errors ändern sich'

# no outliers and less influential values( leverage =1 )
plm.fit_Jul3 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp1,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul3)
'  Es scheint keine Änderung zum Model vorher zu geben. Wahrscheinlich werden diese Werte automatisch nicht berücksichtigt.'

# no outliers and even less influential values( leverage =1) and deleted largest cooks distance
plm.fit_Jul4 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul4)
'Für Mai: Da sich an den Resultaten des Models relativ wenig ändert, wenn ich Werte mit großer Cooks Distance raus nehme, werde ich das Model ohne outliers und leverage >= 1 nehmen.
Mehr noch: da es keine Unterschiede zwischen plm.fit_Jul3 und plm.fit_Jul2 zu geben scheint, macht es wohl Sinn nur die outlier raus zu nehmen. 
Diese sind besonders kleine Werte und wohl Messfehler. Dass könnte man auch direkt am Anfang für alle Durchgänge machen'
' Für July: Es macht sehr wohl Sinn high leverage Werte rauszunehmen, da dann die FE via glm geschätzt werden.

'

###################################################################
## Verification of model with no outliers and less influential values( leverage =1 )
BIC_temp2 <- rep(0,9)

for(r in 1:9){
#   glm.fit4 <- glm(formula = formula_Jul_sm_detrendlog,  data = temp1)
  glm.fit5<- glm(formula = formula_Jul_sm_detrendlog,  data = temp2)
  BIC_temp2[r] <- BIC(glm.fit5)
}
par(mfrow=c(3,1))
plot(BIC, ylab="BIC")
plot(BIC_temp, ylab="BIC without outliers and less leverage")
plot(BIC_temp2,ylab="BIC without outliers and less leverage" )
which.min(BIC_temp2) # hier ist der Wert nun auch 6, also Model sechs. Die Struktur ist nun auch der crossvalidation ähnlicher. 
r = 6
r_Jul = 6 



###################################################
## Compare Models with and without Precipitation ##
###################################################
' Das sollte ich nochmals mit anderen Covariance Matrix machen?!?'
plm.fit_Jul4 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul4)

# Model ohne Preicpitation
plm.fit_Jul_noPrec <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId) - poly(Prec, degree[r, 1], raw = T)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul_noPrec)

## Waldtest with heteroskedasdicity adapted variance covariance matrix
waldtest(plm.fit_Jul4 , plm.fit_Jul_noPrec, vcov= function(x) vcovHC(plm.fit_Jul4, method = "arellano", , type = "HC4"))
' Es macht Sinn die NiederschlagsVariablen im Model zu lassen, da diese auch gemeinsam significant sind.'
' Dieser Test funktioniert für July nicht, dass muss ich nochmals überdenken.'


######################
#### Final Modell ####
######################
## Here I decide on the model after the model selection and data cleaning is finished ##
plm.fit_Jul4 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))
plm.fit_Jul <- plm.fit_Jul4
summary(plm.fit_Jul)

##################################################################################################################################################################################################

####################
#### Inference #####
####################


########################
## Heteroskedasdicity ##
########################
library(lmtest)
library(car)
bptest(glm.fit4) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_Jul)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_Jul, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

## Compare ordinary with Robust covariance matrix estimators a la White for panel models (heteroscedasticity-consistent (HC) standard errors)
coeftest(plm.fit_Jul)
coeftest(plm.fit_Jul3,vcov=vcovHC)
' hier werden die Standardfehler teilweise sogar kleiner'


## Test ob andere Configuration heteroskedasdicity ausweisen
coeftest(plm.fit_Jul2,vcov=vcovHC(plm.fit_Jul1, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Jul2,vcov=vcovHC(plm.fit_Jul2, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4")) 

coeftest(plm.fit_Jul3,vcov=pvcovHC)
coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul2, type = "HC3"))

' Ich denke es macht Sinn das Modell 3 (plm.fit_Jul3 ) ohne outlier und weniger leverage zu nehmen. Darüberhinaus nehme ich arrelano als methode. Der Grund ist, dass durch 
quasi demeaning serial coorelation verursacht wird. Als Option nehme ich HC4, um für die anderen influential observations zu kontollieren. '


#########################
#### Autocorrelation ####
#########################
## Ordinary FE standard error ##
coeftest(plm.fit_Jul)

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_Jul)
pbgtest(plm.fit_Jul) 
'
both, H_1 of serial autocorrelation cannot be rejected
'

' Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time'

## PLM version of the robust covariance estimator vcovHC() - based on White´s formula and (partial) demeaning
''
coeftest(plm.fit_Jul,vcov=vcovHC(plm.fit_Jul, method = "arellano", , type = "HC4")) 


#########################################
## Tests for cross sectiom correlation ##
pcdtest(plm.fit_Jul, test = c("lm"),index = c("comId","year"))
' Funktioniert nicht, evtl habe ich zu viel serial autocorrelation in den Daten. Das kann ich mir eigentlich nicht vorstellen.
 Update aus July: Vielmehr habe ich wahrscheinlich einfach zu viele Beonachtungen. '
' Es ist mit sehr hoher Wahrscheinlichkeit davon auszugehen, dass die Daten zumindest über den Fehlerterm räumlich korreliert sind'
' Zudem ist zu festzuhalten, dass es cross sectional dependence eine Überart von spatial dependence ist. Des weiteren kann cross sectional
dependence zu Ineffizienz führen, wenn die error componenten korreliert sind. Gibt es hingegen einen gemeinsamen Faktor dann kann das
Model sogar inconsistent werden. 
y_it = X_i*t_β +γ_i*µ_t +e_it
'

# Driscoll Kraay:
coeftest(plm.fit_Jul,vcov=function(x) vcovSCC(plm.fit_Jul,type = "HC4"), cluster="comId", maxlag=0, wj=function(j, maxlag) 1-j/(maxlag+1)) 
' Heterskedasdicity: HC4, um für die anderen influential observations zu kontollieren
Serial Autocorrelation:
Spatial Autocorrelation: '
' Achtung: Driscoll Kraay ist nicht geeignet für kurze Panel. Ist das bei mir der Fall? '
'Driscoll Kraay: hier sind viele Werte nicht mehr significant. Was bedeutet das für mein Ergebniss und räumlicher autocorrelation:
Es gibt wohl cross sectional dependence, vor allem über den Raum. Denn wenn '
' Die Alternative ist wohl Parametrisierung via splm oder block/cluster bootstrapping in Verbindung mit feml-Schätzung.
Parameterisierung via splm geht nur mit balanced panels und eher kleinen Datensätzen. '


## Cameron et all=S doouble-clustering estimator
coeftest(plm.fit_Jul,vcov = vcovDC(plm.fit_Jul,type = "HC4"))
'Auch hier werden die Standardfehler sehr viel größer'


########################################
## Correct for spatial autocrrelation ##
library(sp)
library(spdep)
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
library(stringr)
library(lfe)
library(data.table)



###########################################################
## Create lat log data and append it to temp1 data frame ##

KreisPoly <- readShapePoly("./data/data_raw/4_michael/adminitrative_borders_Ger/vg2500_krs","RS" ) # liest auch Projektion der shaphe file mit aus;
KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")

## Transformier der Projection in Lat Lon von EPSD 31464 ##
proj4string(KreisPoly) <- proj4string(KreisPolOGR) # Projection bisher ist 3-degree Gauss zone 4 : EPSG "31464"
CRS("+init=epsg:31464")
KreisPoly <- spTransform(KreisPoly, CRS("+init=epsg:4326")) # unprojected coordinates;

## Plot different Projections ##
plot(KreisPoly)
plot(KreisPolOGR)

## Explore KreisPoly ##
class (KreisPoly)
names(KreisPoly)
str(KreisPoly,2)
head(KreisPoly@data)

## Auslesen der Coordinates ##
coor2 <- coordinates(KreisPoly)
coor2
colnames(coor2) <- c("lon", "lat")
dim(KreisPoly)
dim(coor2)

## Cbind Coordinates ##
KreisPoly2 <-spCbind(KreisPoly, as.data.frame(coor2))
dim(KreisPoly2)
str(KreisPoly2,2)

########################################
## Mergen mit temp1 by comId ans year ##

## Umbennen der RS in ComId of KreisPoly2 ##
KreisDf <- as.data.frame(KreisPoly2)
str(KreisDf,2)
KreisDf$comId <- as.factor(str_sub(KreisDf$RS,1,5))
head(KreisDf)
str(KreisDf)

## Repeat KreisDf 12 times ##
KreisDf2 <- KreisDf[rep(seq_len(nrow(KreisDf)), 12),]
dim(KreisDf2)
str(KreisDf2)
head(KreisDf2)

## Build vector for year: 412*(1999:2010) ##
year1<-NULL
year2<-NULL
for (i in 1999:2010)
{
  year1 <- as.data.frame(rep(i,412))
  colnames(year1) <- c("year")
  year2 <- rbind(year2,year1)
  colnames(year2) <- c("year")
  
}
dim(year2)
head(year2)
tail(year2)
dim(year2)/12

## Cbind KreisDf2 and year 2 ##
KreisDf3 <- cbind(KreisDf2, year2)
head(KreisDf3[,c(8,9,6,7)])
KreisDf3 <- KreisDf3[,c(8,9,6,7)]
rownames(KreisDf3) <- NULL
str(KreisDf3)

## Write and read created data.frame ##
write.csv(KreisDf3, "data/data_raw/KreisDf3.csv")
KreisDf4 <- read.csv("data/data_raw/KreisDf3.csv")
head(KreisDf4)


## Transform comId and year to factor ##
KreisDf4[,c("comId","year")] <- lapply(KreisDf4[,c("comId","year")], factor )

## Mergen anhand von comId und Jahr ##
temp1_geo <- merge(temp2, KreisDf4, by=c("comId","year"))
str(temp1_geo)
dim(temp1_geo)[1]
dim(temp1)[1]
head(temp1_geo)

temp1_geo$X <- NULL

## Check whether comIds have the same coordinates ##
temp1_geo[temp1_geo$comId==1001,] 
unique(temp1_geo$comId)
temp1_geo[temp1_geo$comId==9779,]
' Appears to be good'


##########
## LFE  ##

library(foreign)
library(RcppArmadillo)
library(lfe)
library(geosphere)

## felm without clustering ##
class(temp1$comId)
r <- 6
m <-felm( siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + poly(Tavg,  degree[r, 2], raw = T) + 
            dummy(SMI_GDM, c("severe drought",  "moderate drought", "abnormal dry", "abnormal wet", "abundant wet", "severe wet")) - 1 |comId |0 | 0,    
          data = temp1, keepCX = TRUE)
summary(m)
summary(plm.fit_Jul)
' Das ist sehr gut, die Ergebnisse scheinen weitestgehend gleich zu sein.'


## felm with clustering ##
## with comId clustering ##
str(temp1_geo)
head(temp1_geo)
temp1_geo <- data.table(temp1_geo)
r=6

## with comId clustering ##
m_geo <-felm( siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + poly(Tavg,  degree[r, 2], raw = T) + 
                dummy(SMI_GDM, c("severe drought",  "moderate drought", "abnormal dry", "abnormal wet", "abundant wet", "severe wet")) - 1 | comId + idState |0 | comId,    
              data = temp1_geo, keepCX = TRUE)
summary(m_geo)
summary(plm.fit_Jul) # non clustered stanard error
coeftest(plm.fit_Jul, vcov=vcovHC(plm.fit_Jul)) # clustered standard errors, but no 
'Einmal habe ich normal Standard Error und einmal Clustered Standard Errors. Das ist ja schon mal ein Fortschritt. Diese kann'

## with comId and year clustering ##
m_geo_time <-felm( siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + poly(Tavg,  degree[r, 2], raw = T) + 
                     dummy(SMI_GDM, c("severe drought",  "moderate drought", "abnormal dry", "abnormal wet", "abundant wet", "severe wet")) - 1 | comId + idState |0 | comId + year,    
                   data = temp1_geo, keepCX = TRUE)
summary(m_geo_time)

summary(plm.fit_Jul) # non clustered stanard error
coeftest(plm.fit_Jul, vcov=vcovHC(plm.fit_Jul)) # clustered standard errors, but no


m_geo$fe[[2]]
m_geo$fe
names(m_geo)
getfe(m_geo)


## Conley SE ## 
source("ConleySEs_17June2015.R")

SE <-ConleySEs(reg = m_geo_time,
               unit = "comId",
               time = "idState",
               lat = "lat", lon = "lon",
               dist_fn = "SH", dist_cutoff = 500, 
               lag_cutoff = 5,
               cores = 1, 
               verbose = T)

' Funktioniert leider nicht mit nur local Fixed Effects (muss auch time berücksichtigen)'

SE
sapply(SE, sqrt)
warning()

' Die Alternative könnte in der Tat blocked bootstrap sein. Oder ich benutze eine Implementierung in Stata. 
 Update July: Eine weitere Alternative könnte natürlich auch Driscoll Kray sein. '


# ## BIC for Models without precipitation ##
# BIC_noprec <- rep(0,3)
# for(r in 1:3){
#   glm.fit5 <- glm(formula = update(formula_Jul_sm_detrendlog, .~. - poly(Prec_Jul, degree[r, 1], raw = T)),  data = temp2) 
#   BIC_noprec[r] <- BIC(glm.fit5)
# }
# BIC_noprec
# plot(BIC_noprec, ylab="BIC no precipitation")
# 
# r <- which.min(BIC_temp) # cubic Ansatz ist weiterhin am besten ohne Precipitation


#########################################
# #### Bootstrap ####
#########################################
# boot.fn = function (data ,index)  return(coef(  plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))  ))
# 
# dim(temp2)
# boot.fn(temp2, 1:3961 )
# boot(temp2, boot.fn, 200, stype="w")
# Boot1 <- Boot(glm.fit4, f=Std. Error, R=2000)
# summary(Boot1)
# confint(Boot1)
# summary(glm.fit4)
# 
# library (ISLR)
# boot.fn=function (data ,index) return(coef(lm(mpg ~ horsepower ,data=data ,subset=index))) 
# boot.fn(Auto ,1:392)
# 
# ## Jacknife ##
# library("bootstrap")
# jackknife(temp2, boot.fn)


save("plm.fit_Jul3 ", file="/home/peichl/Documents/projects/correlation/script/script_raw/Crossvalidation/Output_Jul.RData")


###################################################################################################################################################################################################
###################################################################################################################################################################################################
###########################
## Explore Fixed Effects ##


names(plm.fit_Jul3)
fixef(plm.fit_Jul3)
hist(fixef(plm.fit_Jul))
plot(fixef(plm.fit_Jul)) # es gibt hier definitiv clustering, das sollte eventuell räumlich dargestellt werden. Vor allem, wenn sich dies bei den anderen Daten wiederholen sollte. 
# Diese Struktur gibt es beim Mais auch in den anderen Monaten. Das macht auch so Sinn, da die Time-invarianten Effecte als seperater Fehler Term dargestellte werden. 
## Map of Fixed Effects

class(fixef(plm.fit_Jul))
str(fixef(plm.fit_Jul))
fixef <- as.data.frame(as.matrix(fixef(plm.fit_Jul)))
# plot(fixef)
str(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")

summary(siloMaize_logtrend)
summary(fixef[,2])

##########################
## Map of Fixed Effects ##
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(reshape)
library(stringr)
library(classInt)
library(RColorBrewer)

## Lade Datei mit räumlichen Referenzen ##
KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")
str(KreisPolOGR,2)
head(KreisPolOGR)

# spplot(KreisPolOGR, zcol="SHAPE_AREA")
## Transform RS to comId compatible references
KreisPolOGR$comId<-as.integer(str_sub(KreisPolOGR$RS,1,5))
head(fixef)

## Create new spatial data.frame ##
## Merge fixed Effects with Spatial Reference DataFrame via comId
KreisPolOGR_fixef <- merge(KreisPolOGR, fixef, by = "comId")
dim(KreisPolOGR_fixef)
str(KreisPolOGR_fixef, 2)
is.na(KreisPolOGR_fixef)
head(KreisPolOGR_fixef)
class(KreisPolOGR_fixef)

## Plot Spatial Dataframe without color specification
spplot(KreisPolOGR_fixef, zcol="FE", main="Spatial Distribution of Fixed Effects of Mai - Silo Maize" )

## Plot Spatial Data Frame with colours in accordance to cluster algorithm ##
# Create Class Intervalls via cluster algorith #
classInt <- classIntervals(KreisPolOGR_fixef$FE, n=3, style = "jenks")
classInt
plot(classInt)
pal <- brewer.pal(3, "Blues")

plot(classInt, pal=pal, main="Fisher-Jenks natural breaks", xlab="", ylab="")

## Define breacks
at1 <- classInt$brks
at1
# ##  Anpassen der Breaks ##
# at1[1] <- at1[1] - at1[1]/100
# at1[length(at1)] <- at1[length(at1)] + at1[length(at1)]/100

## Plot with breaks derived Fisher - Jenins Natural Breaks Algorithm with properly assigned colors
spplot(KreisPolOGR_fixef, zcol="FE", at=at1, col.regions=colorRampPalette(pal)(length(at1)-1),main="Spatial Distribution of Fixed Effects of _Jul - Silo Maize" )

#####################################################
## Plot Functions of Precipitation and Temperature ##
summary(temp1$siloMaize_logtrend)
hist(temp1$siloMaize_logtrend)
## Temperature ##
coeftest(plm.fit_Jul,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))
b3 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[3]
b4 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[4]
b5 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[5]
b3
b4
b5


## Log
plot(temp1$Tavg, temp1$siloMaize_logtrend, main="Fitted Polynom of Temperature - Log")
summary(temp1$Tavg)
summary(temp1$siloMaize_logtrend)
xcurve <- seq(8.967, 17.4, 0.1)
ycurve <-0 + b3 * xcurve  + b4 * xcurve^2   + b5 * xcurve^3
lines(xcurve,ycurve, col="green", lwd = 2)

## Exp
plot(Yield_Covariates_SM_Jul$Tavg, Yield_Covariates_SM_Jul$siloMaize- mean(Yield_Covariates_SM_Jul$siloMaize), main="Fitted Polynom of Temperature - Exp")
summary(Yield_Covariates_SM_Jul$Tavg)
xcurve <- seq(8.967, 17.4, 0.1)
ycurve <- exp( b3 * xcurve  + b4 * xcurve^2   + b5 * xcurve^3)
lines(xcurve,ycurve, col="green", lwd = 2)

## Precipitation
b1 <-coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[1]
b2 <-  coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[2]

## Log
plot(Yield_Covariates_SM_Jul$Prec, log(Yield_Covariates_SM_Jul$siloMaize - mean(Yield_Covariates_SM_Jul$siloMaize)), main="Fitted Polynom of Precipitation - Log")

summary(Yield_Covariates_SM_Jul$Prec)
xcurve <- seq(5.008, 308.700, 0.1)
ycurve <-   b1 * xcurve + b2 * xcurve^2
summary(ycurve)
lines(xcurve,ycurve, col="green", lwd = 2)

## Exp
plot(Yield_Covariates_SM_Jul$Prec, Yield_Covariates_SM_Jul$siloMaize- mean(Yield_Covariates_SM_Jul$siloMaize), main="Fitted Polynom of Precipitation -Exp")
# plot(Yield_Covariates_SM_Jul$Prec, Yield_Covariates_SM_Jul$siloMaize, main="Fitted Polynom of Precipitation")

summary(Yield_Covariates_SM_Jul$Prec)
xcurve <- seq(5.008, 308.700, 0.1)
ycurve <-  exp(b1 * xcurve + b2 * xcurve^2)
summary(ycurve)
lines(xcurve,ycurve, col="green", lwd = 2)

## Hier sollte ich mir die leverage Statistiken nochmals anschauen. Es gibt einige Werte im Niederschlag, die extrem groß sind und den fit sehr stark beeiflussen;

###########################################################
## Calculate exact percentage change of SMI coefficients ##
b6 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[6]
b7 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[7]
b8 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[8]
b9<- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[9]
b10 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[10]
b11 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[11]
100*(exp(b6)-1)
100*(exp(b7)-1)
100*(exp(b8)-1)
100*(exp(b9)-1)
100*(exp(b10)-1)
100*(exp(b11)-1)


## Yield vs SMI ##
plot(temp1$SMI, SM_Jul$siloMaize_logtrend- mean(Yield_Covariates_SM_Jul$siloMaize_logtrend), main="Yield vs SMI")


x <- seq(1,12,1)
x
log(x)
y <- seq(1999,2010,1)
log(y)
summary(log(siloMaize))

######################################################
## Berechnen des Modells mit trend auf linker Seite ##

summary(plm.fit_Jul)

## Remove linear trend ##
'Fit yield on time and use the residuals of that for detrended yields'

plot(Yield_Covariates_SM_Jul$siloMaize ~ as.integer(Yield_Covariates_SM_Jul$year))
lineartrend <- lm(siloMaize ~ as.integer(year), data= Yield_Covariates_SM_Jul )
summary(lineartrend)
abline(lineartrend)
abline(453.5511,0, col="green")
length(resid(lineartrend))
Yield_Covariates_SM_Jul$siloMaize_lintren <- resid(lineartrend)

r=6
formula_Jul_sm_lintren  <- siloMaize_lintren ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) 

## Remove log trend ##
'Fit log of yield on log of time and use the residuals of that for yields'
plot(log(Yield_Covariates_SM_Jul$siloMaize) ~ log(as.integer(Yield_Covariates_SM_Jul$year)))
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Jul)
abline(logtrend)
summary(logtrend)
abline(6.1115,0, col="green")
Yield_Covariates_SM_Jul$siloMaize_logtrend <- resid(logtrend)


r=6
formula_Jul_sm_detrendlog <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) 

###################################################################
## Determine right degree of poylnomials via BIC for log detrend ##
BIC_logdetrend <- rep(0,9)


for(r in 1:9){
  glm.fit_logdetrend <- glm(formula= update(formula_Jul_sm_detrendlog, .~. + dummy(comId)),  data = Yield_Covariates_SM_Jul) 
  BIC_logdetrend[r] <- BIC(glm.fit_logdetrend)
}

par(mfrow=c(1,1))
plot(BIC_logdetrend, main="BIC without outliers and less leverage with left side detrend" )
which.min(BIC_logdetrend) # hier ist der Wert nun 4
r_logdetrend = 6 

'Hier ändert sich nachvollziehbar in der Struktur und im Ergebnis wenig'

plot(glm.fit_logdetrend)
####################
## PLM Ergebnisse ##
r = r_logdetrend
plm.fit_Jul_detrendlinear <- plm(formula= formula_Jul_sm_detrendlinear,  data = Yield_Covariates_SM_Jul, effect="individual", model="within")
plm.fit_Jul_detrendlog <- plm(formula= formula_Jul_sm_detrendlog,  data = Yield_Covariates_SM_Jul, effect="individual", model="within")

summary(plm.fit_Jul)
summary(plm.fit_Jul_detrendlinear)
summary(plm.fit_Jul_detrendlog)

coeftest(plm.fit_Jul,vcov=vcovHC(plm.fit_Jul, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Jul_detrendlinear,vcov=vcovHC(plm.fit_Jul_detrendlinear, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Jul_detrendlog,vcov=vcovHC(plm.fit_Jul_detrendlog, method = "arellano", , type = "HC4"))

## Gleiche Konfiguration aber andere Jahre: 1:12 statt 1999 - 2010
head(Yield_Covariates_SM_Jul)
Yield_Covariates_SM_Jul$yearshort <- as.integer(Yield_Covariates_SM_Jul$year) - 1999



##############################
#############################
## Use PET instead of Temp ##
#############################
## Plot SMI Pet ##
plot(Yield_Covariates_SM_Jul$siloMaize ~ Yield_Covariates_SM_Jul$Pet)
plot(Yield_Covariates_SM_Jul$siloMaize ~ Yield_Covariates_SM_Jul$Tavg)

## Determine formula
formula_Jul_sm_Pet <- log(siloMaize) ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year))
formula_Jul_sm_Pet_detrendlog <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) 


############################################################################
## Determine right degree of poylnomials via BIC for PET with log detrend ##
BIC_Pet <- rep(0,9)


for(r in 1:9){
  glm.fit_Pet <- glm(formula= update(formula_Jul_sm_Pet_detrendlog, .~. + dummy(comId)),  data = temp1) 
  BIC_Pet[r] <- BIC(glm.fit_Pet)
}

par(mfrow=c(1,1))
plot(BIC_Pet, main="BIC without outliers and less leverage of PET configuration with left side detrend" )
which.min(BIC_Pet) # hier ist der Wert nun 4
r_Pet = 4 
lm.fit_Pet <- lm(formula= update(formula_Jul_sm_Pet_detrendlog, .~. + dummy(comId)),  data = temp1) 
summary(lm.fit_Pet)   # hier scheinen die Fixed Effekte 61 Prozent der Variabilität zu erklären. Das ist sicherlich auch eine Begründung, warum diese weiterhin mit aufgeführt werden
# sollten und vor allema auch für die Vertiefung via maps.

####################
## PLM Ergebnisse ##
head(temp1)
plm.fit_Jul_Pet <- plm(formula= formula_Jul_sm_Pet,  data = temp1, effect="individual", model="within")
plm.fit_Jul_Pet_detrendlog <- plm(formula= formula_Jul_sm_Pet_detrendlog,  data = temp1, effect="individual", model="within")

summary(plm.fit_Jul)

summary(plm.fit_Jul_Pet) # R² ist größer, aber Anteil von SMI ist kleiner.
summary(plm.fit_Jul_Pet_detrendlog)
summary(plm.fit_Jul_detrendlog)

coeftest(plm.fit_Jul_Pet_detrendlog,vcov=vcovHC(plm.fit_Jul_Pet_detrendlog, method = "arellano", , type = "HC4"))



## Heteroskedasdicity ##
r=r_Pet
library(lmtest)
library(car)
bptest(plm.fit_Jul_Pet_detrendlog) # Null, dass es keine heteroskedasdicity gibt, wurde zurückgewiesen. 

coeftest(glm.fit_Pet,vcov=vcovHC(glm.fit_Pet)) # funktioniert nicht, wohl zu viele Dummies
coeftest(plm.fit_Jul_Pet_detrendlog,vcov=vcovHC(plm.fit_Jul_Pet_detrendlog, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Jul_Pet_detrendlog,vcov=vcovHC(plm.fit_Jul_Pet_detrendlog, type = "HC3"))
' Ich denke es macht Sinn das Modell 3 ohne outlier und weniger leverage zu nehmen. Darüberhinaus nehme ich arrelano als methode. Der Grund ist, dass durch 
quasi demeaning serial coorelation verursacht wird. Als Option nehme ich HC4, um für die anderen influential observations zu kontollieren. '
###########################
####  SiloMaize in _Jul ####
###########################

library("plm")
library("boot")
library("gtools")
library("lme4")
library(lmtest)
library(car)

## Read data frame with siloMaize as only depedent variable ##
# Yield_Covariates <- read.csv( "/home/peichl/projects/crossvalid/siloMaize/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv")

Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv")
head(Yield_Covariates)

Yield_Covariates$X <- NULL
names(Yield_Covariates)
head(Yield_Covariates)

levels(Yield_Covariates$com)

## For publication worth regression output need to change data names ##
# Get rid of variables which are not necessary
names(Yield_Covariates)
names <- names(Yield_Covariates)
_Jul <- grep(c("*Jul"), names)
_Jul

Yield_Covariates_Jul <- Yield_Covariates[,_Jul]
names(Yield_Covariates_Jul)
Yield_Covariates_Jul$siloMaize <- NULL  # Nur bei Silomaize im _Jul notwendig
dim(Yield_Covariates_Jul)

## Delete all but SMI, Prec, Tavg and Pet
names(Yield_Covariates_Jul)
Yield_Covariates_Jul <- Yield_Covariates_Jul[,c(1:4)]

## Establish first part of data frame ##
Yield_Covariates_SM <- Yield_Covariates[,c(2,1,3:5,7)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_SM_Jul <- cbind(Yield_Covariates_SM, Yield_Covariates_Jul)
names(Yield_Covariates_SM_Jul)
names(Yield_Covariates_SM_Jul) <- c( "comId" , "year","com","idState","State","siloMaize","SMI", "Prec","Tavg", "Pet")
names(Yield_Covariates_SM_Jul)


#################################
#### Define comId as factors ####
class(Yield_Covariates_SM_Jul$comId)
Yield_Covariates_SM_Jul$comId <- as.factor(Yield_Covariates_SM_Jul$comId )
head(Yield_Covariates_SM_Jul$comId)


## Drought Monitor Spezification ##
Yield_Covariates_SM_Jul$SMI_GDM <- cut(Yield_Covariates_SM_Jul$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))

## Change Indexing so that it can be used in plm package
Yield_Covariates_SM_Jul <- plm.data(Yield_Covariates_SM_Jul, index=c("comId", "year"))
str(Yield_Covariates_SM_Jul)



# ## Remove linear trend ##
# 'Fit yield on time and use the residuals of that for detrended yields'
# 
# plot(Yield_Covariates_SM_Jul$siloMaize ~ as.integer(Yield_Covariates_SM_Jul$year))
# lineartrend <- lm(siloMaize ~ as.integer(year), data= Yield_Covariates_SM_Jul )
# summary(lineartrend)
# abline(lineartrend)
# abline(453.5511,0, col="green")
# length(resid(lineartrend))
# Yield_Covariates_SM_Jul$siloMaize_lintren <- resid(lineartrend)
# 
# formula_Jul_sm_lintren  <- siloMaize_lintren ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
#   dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) 

## Na-omit ##
head(Yield_Covariates_SM_Jul)
sum(is.na(Yield_Covariates_SM_Jul) )
dim(Yield_Covariates_SM_Jul)
Yield_Covariates_SM_Jul_nna <- na.omit(Yield_Covariates_SM_Jul) 
dim(Yield_Covariates_SM_Jul_nna)
any(is.na(Yield_Covariates_SM_Jul_nna))
rownames(Yield_Covariates_SM_Jul_nna) <- NULL
head(Yield_Covariates_SM_Jul_nna)

Yield_Covariates_SM_Jul <- Yield_Covariates_SM_Jul_nna

## Remove log trend ##
'Fit log of yield on log of time and use the residuals of that for yields'
plot(log(Yield_Covariates_SM_Jul$siloMaize) ~ log(as.integer(Yield_Covariates_SM_Jul$year)))
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Jul)
abline(logtrend, col="green")
summary(logtrend)
abline(6.1115,0)
Yield_Covariates_SM_Jul$siloMaize_logtrend <- resid(logtrend)


## Attach Data Set ##
head(Yield_Covariates_SM_Jul)
attach(Yield_Covariates_SM_Jul)


##########################################################################
#### BIC to choose the degrees of the polynomials and natural splines ####
##########################################################################

## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

formula_Jul_sm_detrendlog <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

## Print formula
formula_Jul_sm_detrendlog

#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################


####################################################################################################
### Loop over two variables - Tavg and Prec - to dertermine their polygons starting with 8 grades ##

##  Define empty lists for loop ##
# cv.error <- rep(0,9)
# cv.error
BIC <- rep(0,9)
BIC


print("start loop")

for(r in 1:9){
  glm.fit <- glm(formula = formula_Jul_sm_detrendlog,  data = Yield_Covariates_SM_Jul) 
  
  BIC[r] <- BIC(glm.fit)
  #   cv.error[r] <- cv.glm( Yield_Covariates_SM_Jul, glm.fit )$delta[1] # hat funktioniert (dummies dürfen nicht nur als Faktor definiert werden)
  #   save.image(file = "/home/peichl/projects/crossvalid/siloMaize/cross5.RData")
}

## Load data from correlation project folder
# load("/home/peichl/Documents/projects/correlation/data/data_processed/Crossvalidation/siloMaize/cross5.RData")

# cv.error_loocv_Jul_SiloMaize <- cv.error

# par(mfrow=c(2,2))
par(mfrow=c(1,1))
## Compare with crossvalidation results of 
# plot(cv.error_loocv_2v8poly_Prec)  # aus ursprünlichen, crossvalidated run
# plot(BIC_loop_2v8poly_Prec)  # aus ursprünlichen, crossvalidated run
plot(BIC)


which.min(BIC)
# which.min(cv.error_loocv_Jul_SiloMaize) 

# ich denke, dass hier Model Nummer 5 am besten ist
r = r_Jul = 5


####################
## GLM Ergebnisse ##
glm.fit2 <- glm(formula = formula_Jul_sm_detrendlog,  data =  Yield_Covariates_SM_Jul) 
'Caveat: Hier werden aufgrund von singulariät eine Menge NAs in den Fixed Effekten produziert.'

summary(glm.fit2)
coefficients(glm.fit2)[1:15]

## plot ##
plot(glm.fit2)
' There is an issue with leverage and outliers'

################
### Outliers ###
################

## Outliers via rstudent Verteilung ##
plot(rstudent(glm.fit2))
rstd <- rstudent(glm.fit2)
dim( Yield_Covariates_SM_Jul)
head(Yield_Covariates_SM_Jul)
summary(Yield_Covariates_SM_Jul$siloMaize)
hist(Yield_Covariates_SM_Jul$siloMaize)


Yield_Covariates_SM_Jul[1317,]
Yield_Covariates_SM_Jul[771,]
Yield_Covariates_SM_Jul[847,]
' Diese Werte sind in Silomaize nicht nachzuvollziehen und sollten daher gelöscht werden, dass es wohl Messfehler sind.'


## Generate data without outliers rstd > |5|
na.omit(Yield_Covariates_SM_Jul[rstd > 6 | rstd < -6,])  #11)

## Anschauem der Struktur der coms, welche hier ausgegeben wurden: hier sind vor allem die SiloMaize Werte relevant ##
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "5315",] # hier scheint nur ein Jahr ein Messfehler zu sein
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "5378",] # sollte ich insgesamt rausnehmen, da die Meldungen nicht valide scheinen. 
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "6532",] # hier scheint nur ein Jahr ein Messfehler zu sein 
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "12052",] # unvollsätändige Berichterstattung, sollte ich insgesamt rausnehmen
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "12053",] # hier scheint nur ein Jahr ein Messfehler zu sein
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "12069",] # hier mache ich am besten nichts
Yield_Covariates_SM_Jul[Yield_Covariates_SM_Jul$comId == "14730",] # hier mache ich auch am besten nichts

## Generate data without outliers rstd > |6.5|
na.omit(Yield_Covariates_SM_Jul[rstd > 6 | rstd < -6,])  
dim(Yield_Covariates_SM_Jul)
temp <-  Yield_Covariates_SM_Jul[!rstd > 6 & !rstd < -6,]
dim(temp)

## Löschen vom zweifelhaften Landkreisen ##
dim(temp)
temp <- temp[!temp$comId == "5378",] # Rheinisch Bergischer Kreis
dim(temp)
temp <- temp[!temp$comId == "12052",] # Cottbus
dim(temp)

## Generate data without outliers rstd > |6| and dubious coms (systematic measurement errors)
temp <- na.omit(temp)
rownames(temp) <- NULL

## GLM with no outliers rstd > |6| and dubious coms (systematic measurement errors) ##
r=5
glm.fit3 <- glm(formula = formula_Jul_sm_detrendlog,  data = temp)
summary(glm.fit3)
'Achtung, hier werden wieder manchen Fixed Effects aufgrund von Singularities zu NAs'

## Determine min BIC for model without leverage rstd > |6| ##
BIC_temp <- rep(0,9)
for(r in 1:9){
  glm.fit3 <- glm(formula = formula_Jul_sm_detrendlog,  data = temp) 
  BIC_temp[r] <- BIC(glm.fit3)
}
plot(BIC_temp,ylab="BIC without outliers" )
which.min(BIC_temp) # hier ist der Wert nun auch 6, also Model sechs. Die Struktur ist nun auch der crossvalidation ähnlicher. 
' Ohne die Outlier ergibt sich ein anderere Wert für degrees (6 statt 5). Dieser Schritt scheint sich wirklich zu lohnen.'
r = 6
glm.fit3 <- glm(formula = formula_Jul_sm_detrendlog,  data = temp)
summary(glm.fit3)

plot(glm.fit3) # Observations with leverage larger than one:181,763,1008

## Untersuch outlier erneut ##
plot(rstudent(glm.fit3))
rstd2 <- rstudent(glm.fit3)
na.omit(temp[rstd2 > 6 | rstd2 < -6,])  

temp[temp$comId==12069,] # Lasse ich drin, dass es ein nachvollziehbarer outlier ist

################
### Leverage ###
################

#######################################################################
## Explore values which are shown within warning of plot() function ##
plot(glm.fit3)
plot(glm.fit3, which=4)
#
dim(temp)
## Explore Variables leverage größer 1 ##
temp[c(181,763,1008),]
# Explore comIDs
temp[temp$comId == "3101",] # jeweils nur eine Beobachtung 763
temp[temp$comId == "5314",] # jeweils nur eine Beobachtung 1008
temp[temp$comId == "5911",] # jeweils nur eine Beobachtung 181

## Delete Variabels with leverage large than one ##
temp1<-  temp[-c(181,763,1008),]
dim(temp1)
temp1 <- na.omit(temp1)
rownames(temp1) <- NULL

############################################################
## GLM with less influential values (leverage größer 1 ) ##
glm.fit4<- glm(formula = formula_Jul_sm_detrendlog,  data = temp1)
summary(glm.fit4)
plot(glm.fit4) 

############################################################
## Cooks Distance as measure of influential observations ##

## Show plot with Cooks Distance ##
plot(glm.fit4, which=4)

## Delete observations with high Cooks Distance
temp1[c( 3697,3683,3719),]

####################
## Explore comIds ##
temp[temp$comId == "15082",] 
temp[temp$comId == "15001",] 
temp[temp$comId == "15086",] 
' Hier sehe ich keine besonderen Gründe, warum man diese Daten löschen sollte. Höchstens, dass es hier erst Beonachtungen ab 2006 gibt.'

temp2<-  temp1[-c( 3697,3683,3719),]
temp2 <- na.omit(temp2)
rownames(temp2) <- NULL

## GLM with even less influential values (großer Cooks Distance ) ##
glm.fit5<- glm(formula = formula_Jul_sm_detrendlog,  data = temp2)
summary(glm.fit5)
coeftest(glm.fit5)
plot(glm.fit5)
' Das Verändert Resultate nur unwesentlich. Daher sollte ich wohl beim Modell vorher bleiben.'

## Show plot with Cooks Distance ##
plot(glm.fit5, which=4)

## Cooks Distance as measure for influence(combination of outlier and leverage)
# cook <- cooks.distance(glm.fit5)
# com <- temp2$com
# year <- temp$year
# library(faraway)
# halfnorm(cook, 3)
# which.max(cook)
# library(car)
# plot(cookd(glm.fit5))
# identify(1:50, cooks.distance(glm.fit5))



# ## Combine leverage and outlier statistic ##
# plot(hatvalues(glm.fit5), rstudent(glm.fit5))
' Gleiche Grafik letzte aus dem plot() Befehl'


## GLM with less leverage ##

coefficients(glm.fit3)[1:10] # mit temp
coefficients(glm.fit4)[1:15] # mit temp1
coefficients(glm.fit5)[1:15] # mit temp2
' Vor allem das weglassen der Punkte mit Leverage größer 1 und großer Cooks Distance hat einen Einfluss auf die Coefficienten'

####################
## PLM Ergebnisse ##
plm.fit_Jul1 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = Yield_Covariates_SM_Jul,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul1)

# no outliers
plm.fit_Jul2 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul2)
' Hier gibt es relevante Änderung zum Model vorher. Es macht wohl wirklich Sinn die Outlier rauszunehmen. Sowohl der min_BIC als auch die Coefficienten und Std. Errors ändern sich'

# no outliers and less influential values( leverage =1 )
plm.fit_Jul3 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp1,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul3)
'  Es scheint keine Änderung zum Model vorher zu geben. Wahrscheinlich werden diese Werte automatisch nicht berücksichtigt.'

# no outliers and even less influential values( leverage =1) and deleted largest cooks distance
plm.fit_Jul4 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul4)
' Da sich an den Resultaten des Models relativ wenig ändert, wenn ich Werte mit großer Cooks Distance raus nehme, werde ich das Model ohne outliers und leverage >= 1 nehmen.
Mehr noch: da es keine Unterschiede zwischen plm.fit_Jul3 und plm.fit_Jul2 zu geben scheint, macht es wohl Sinn nur die outlier raus zu nehmen. 
Diese sind besonders kleine Werte und wohl Messfehler. Dass könnte man auch direkt am Anfang für alle Durchgänge machen'


###################################################################
## Verification of model with no outliers and less influential values( leverage =1 )
BIC_temp2 <- rep(0,9)

for(r in 1:9){
  glm.fit4 <- glm(formula = formula_Jul_sm_detrendlog,  data = temp1) 
  BIC_temp2[r] <- BIC(glm.fit4)
}
plot(BIC_temp2,ylab="BIC without outliers and less leverage" )
which.min(BIC_temp2) # hier ist der Wert nun auch 6, also Model sechs. Die Struktur ist nun auch der crossvalidation ähnlicher. 
r = 6
r_Jul = 6 
' Das rausnehmen der Punkte mit großer Cooks Distance hat nicht mehr wirklich viel geändert. Wahrscheinlich ist vor allem wichtig, dass ich nicht nachvollziehbare outlier und Werte
mit leverage größer 1 rausnehme: plm.fit_Jul3 '


###################################################
## Compare Models with and without Precipitation ##
###################################################
' Das sollte ich nochmals mit anderen Covariance Matrix machen?!?'
plm.fit_Jul3 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp1,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul3)

# Model ohne Preicpitation
plm.fit_Jul_noPrec <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId) - poly(Prec, degree[r, 1], raw = T)),  data = temp1,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Jul_noPrec)

## Waldtest with heteroskedasdicity adapted variance covariance matrix
waldtest(plm.fit_Jul3 , plm.fit_Jul_noPrec, vcov= function(x) vcovHC(plm.fit_Jul2, method = "arellano", , type = "HC4"))
' Es macht Sinn die NiederschlagsVariablen im Model zu lassen, da diese auch gemeinsam significant sind.'


######################
#### Final Modell ####
######################
## Here I decide on the model after the model selection and data cleaning is finished ##
plm.fit_Jul3 <- plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp1,  effect="individual", model=("within"), index = c("comId","year"))
plm.fit_Jul <- plm.fit_Jul3
summary(plm.fit_Jul)

##################################################################################################################################################################################################

####################
#### Inference #####
####################


########################
## Heteroskedasdicity ##
########################
library(lmtest)
library(car)
bptest(glm.fit4) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_Jul)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_Jul, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

## Compare ordinary with Robust covariance matrix estimators a la White for panel models (heteroscedasticity-consistent (HC) standard errors)
coeftest(plm.fit_Jul)
coeftest(plm.fit_Jul3,vcov=vcovHC)
' hier werden die Standardfehler teilweise sogar kleiner'


## Test ob andere Configuration heteroskedasdicity ausweisen
coeftest(plm.fit_Jul2,vcov=vcovHC(plm.fit_Jul1, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Jul2,vcov=vcovHC(plm.fit_Jul2, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4")) 

coeftest(plm.fit_Jul3,vcov=pvcovHC)
coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul2, type = "HC3"))

' Ich denke es macht Sinn das Modell 3 (plm.fit_Jul3 ) ohne outlier und weniger leverage zu nehmen. Darüberhinaus nehme ich arrelano als methode. Der Grund ist, dass durch 
quasi demeaning serial coorelation verursacht wird. Als Option nehme ich HC4, um für die anderen influential observations zu kontollieren. '


#########################
#### Autocorrelation ####
#########################
## Ordinary FE standard error ##
coeftest(plm.fit_Jul)

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_Jul)
pbgtest(plm.fit_Jul) 
'
both, H_1 of serial autocorrelation cannot be rejected
'

' Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time'

## PLM version of the robust covariance estimator vcovHC() - based on White´s formula and (partial) demeaning
''
coeftest(plm.fit_Jul,vcov=vcovHC(plm.fit_Jul, method = "arellano", , type = "HC4")) 


#########################################
## Tests for cross sectiom correlation ##
pcdtest(plm.fit_Jul, test = c("lm"),index = c("comId","year"))
' Funktioniert nicht, evtl habe ich zu viel serial autocorrelation in den Daten. Das kann ich mir eigentlich nicht vorstellen'
' Es ist mit sehr hoher Wahrscheinlichkeit davon auszugehen, dass die Daten zumindest über den Fehlerterm räumlich korreliert sind'
' Zudem ist zu festzuhalten, dass es cross sectional dependence eine Überart von spatial dependence ist. Des weiteren kann cross sectional
dependence zu Ineffizienz führen, wenn die error componenten korreliert sind. Gibt es hingegen einen gemeinsamen Faktor dann kann das
Model sogar inconsistent werden. 
y_it = X_i*t_β +γ_i*µ_t +e_it
'

# Driscoll Kraay:
coeftest(plm.fit_Jul,vcov=function(x) vcovSCC(plm.fit_Jul,type = "HC4"), cluster="comId", maxlag=0, wj=function(j, maxlag) 1-j/(maxlag+1)) 
' Heterskedasdicity: HC4, um für die anderen influential observations zu kontollieren
Serial Autocorrelation:
Spatial Autocorrelation: '
' Achtung: Driscoll Kraay ist nicht geeignet für kurze Panel. Ist das bei mir der Fall? '
'Driscoll Kraay: hier sind viele Werte nicht mehr significant. Was bedeutet das für mein Ergebniss und räumlicher autocorrelation:
Es gibt wohl cross sectional dependence, vor allem über den Raum. Denn wenn '
' Die Alternative ist wohl Parametrisierung via splm oder block/cluster bootstrapping in Verbindung mit feml-Schätzung.
Parameterisierung via splm geht nur mit balanced panels und eher kleinen Datensätzen. '


## Cameron et all=S doouble-clustering estimator
coeftest(plm.fit_Jul,vcov = vcovDC(plm.fit_Jul,type = "HC4"))
'Auch hier werden die Standardfehler sehr viel größer'


########################################
## Correct for spatial autocrrelation ##
library(sp)
library(spdep)
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
library(stringr)
library(lfe)
library(data.table)



###########################################################
## Create lat log data and append it to temp1 data frame ##

KreisPoly <- readShapePoly("./data/data_raw/4_michael/adminitrative_borders_Ger/vg2500_krs","RS" ) # liest auch Projektion der shaphe file mit aus;
KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")

## Transformier der Projection in Lat Lon von EPSD 31464 ##
proj4string(KreisPoly) <- proj4string(KreisPolOGR) # Projection bisher ist 3-degree Gauss zone 4 : EPSG "31464"
CRS("+init=epsg:31464")
KreisPoly <- spTransform(KreisPoly, CRS("+init=epsg:4326")) # unprojected coordinates;

## Plot different Projections ##
plot(KreisPoly)
plot(KreisPolOGR)

## Explore KreisPoly ##
class (KreisPoly)
names(KreisPoly)
str(KreisPoly,2)
head(KreisPoly@data)

## Auslesen der Coordinates ##
coor2 <- coordinates(KreisPoly)
coor2
colnames(coor2) <- c("lon", "lat")
dim(KreisPoly)
dim(coor2)

## Cbind Coordinates ##
KreisPoly2 <-spCbind(KreisPoly, as.data.frame(coor2))
dim(KreisPoly2)
str(KreisPoly2,2)

########################################
## Mergen mit temp1 by comId ans year ##

## Umbennen der RS in ComId of KreisPoly2 ##
KreisDf <- as.data.frame(KreisPoly2)
str(KreisDf,2)
KreisDf$comId <- as.factor(str_sub(KreisDf$RS,1,5))
head(KreisDf)
str(KreisDf)

## Repeat KreisDf 12 times ##
KreisDf2 <- KreisDf[rep(seq_len(nrow(KreisDf)), 12),]
dim(KreisDf2)
str(KreisDf2)
head(KreisDf2)

## Build vector for year: 412*(1999:2010) ##
year1<-NULL
year2<-NULL
for (i in 1999:2010)
{
  year1 <- as.data.frame(rep(i,412))
  colnames(year1) <- c("year")
  year2 <- rbind(year2,year1)
  colnames(year2) <- c("year")
  
}
dim(year2)
head(year2)
tail(year2)
dim(year2)/12

## Cbind KreisDf2 and year 2 ##
KreisDf3 <- cbind(KreisDf2, year2)
head(KreisDf3)
KreisDf3 <- KreisDf3[,c(8,9,6,7)]
rownames(KreisDf3) <- NULL
str(KreisDf3)

## Write and read created data.frame ##
write.csv(KreisDf3, "data/data_raw/KreisDf3.csv")
KreisDf4 <- read.csv("data/data_raw/KreisDf3.csv")
head(KreisDf4)


## Transform comId and year to factor ##
KreisDf4[,c("comId","year")] <- lapply(KreisDf4[,c("comId","year")], factor )

## Mergen anhand von comId und Jahr ##
temp1_geo <- merge(temp1, KreisDf4, by=c("comId","year"))
str(temp1_geo)
dim(temp1_geo)[1]
dim(temp1)[1]
head(temp1_geo)

temp1_geo$X <- NULL

## Check whether comIds have the same coordinates ##
temp1_geo[temp1_geo$comId==1001,] 
unique(temp1_geo$comId)
temp1_geo[temp1_geo$comId==9779,]
' Appears to be good'


##########
## LFE  ##

library(foreign)
library(RcppArmadillo)
library(lfe)
library(geosphere)

## felm without clustering ##
class(temp1$comId)
r <- 6
m <-felm( siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + poly(Tavg,  degree[r, 2], raw = T) + 
            dummy(SMI_GDM, c("severe drought",  "moderate drought", "abnormal dry", "abnormal wet", "abundant wet", "severe wet")) - 1 |comId |0 | 0,    
          data = temp1, keepCX = TRUE)
summary(m)
summary(plm.fit_Jul)
' Das ist sehr gut, die Ergebnisse scheinen weitestgehend gleich zu sein.'


## felm with clustering ##
## with comId clustering ##
str(temp1_geo)
head(temp1_geo)
temp1_geo <- data.table(temp1_geo)
r=6

## with comId clustering ##
m_geo <-felm( siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + poly(Tavg,  degree[r, 2], raw = T) + 
                dummy(SMI_GDM, c("severe drought",  "moderate drought", "abnormal dry", "abnormal wet", "abundant wet", "severe wet")) - 1 | comId + idState |0 | comId,    
              data = temp1_geo, keepCX = TRUE)
summary(m_geo)
summary(plm.fit_Jul) # non clustered stanard error
coeftest(plm.fit_Jul, vcov=vcovHC(plm.fit_Jul)) # clustered standard errors, but no 
'Einmal habe ich normal Standard Error und einmal Clustered Standard Errors. Das ist ja schon mal ein Fortschritt. Diese kann'

## with comId and year clustering ##
m_geo_time <-felm( siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + poly(Tavg,  degree[r, 2], raw = T) + 
                     dummy(SMI_GDM, c("severe drought",  "moderate drought", "abnormal dry", "abnormal wet", "abundant wet", "severe wet")) - 1 | comId + idState |0 | comId + year,    
                   data = temp1_geo, keepCX = TRUE)
summary(m_geo_time)

summary(plm.fit_Jul) # non clustered stanard error
coeftest(plm.fit_Jul, vcov=vcovHC(plm.fit_Jul)) # clustered standard errors, but no


m_geo$fe[[2]]
m_geo$fe
names(m_geo)
getfe(m_geo)


## Conley SE ## 
source("ConleySEs_17June2015.R")

SE <-ConleySEs(reg = m_geo_time,
               unit = "comId",
               time = "idState",
               lat = "lat", lon = "lon",
               dist_fn = "SH", dist_cutoff = 500, 
               lag_cutoff = 5,
               cores = 1, 
               verbose = T)

' Funktioniert leider nicht mit nur local Fixed Effects (muss auch time berücksichtigen'

SE
sapply(SE, sqrt)
warning()

' Die Alternative könnte in der Tat blocked bootstrap sein. Oder ich benutze eine Implementierung in Stata. Diese soll  '

## Remove Rügen ##
KreisPoly_3 <- KreisPoly2[-361,] # Rügen
dim(KreisPoly_3)
plot(KreisPoly_3)
KreisPoly <- KreisPoly_3



## Ich muss noch meine 

## Calculate contiguity-based neigbors Weighting Matrix
'
Wahrscheinlich macht eine derartige neighbouring matrix für mich keinen Sinn, da ich viele Nachbarn rausgenommen habe und so lose Verbindungen 
in meiner Matrix sind. Auch theoretisch macht eine Distanz decaying Matrix bei mir wesentich mehr Sinn.
'
contnb_2 <- poly2nb(KreisPolOGR, queen=T)
contnb <- poly2nb(KreisPoly, queen=T)

str(contnb,2) 
str(contnb_2,2)

par(mfrow=c(1,1))
plot(contnb, coord)
plot(contnb_2, coord)
## Calculate distance-based neigbors Weighting Matrix
contmtd <- dnearneigh(coordinates(KreisPoly),0,380000, longlat = F)
str(contmtd)
plot(contmtd, coord)

## Obtain W list: spatial weights for neighbours list
' Unterschiedliche '
W <- nb2listw(contnb, style="W", zero.policy=TRUE)
' '
W
can.be.simmed(W)

## Obtain W matrix
W_mat <-nb2mat(contmtd)
W_mat


## Local CD(p) test with neighbourhood matrix ##
'Millo benutzt hier eine standardized Matrix für den Test'
pcdtest(plm.fit_Jul2, w=W) 
' Achtung, der CD(p) test is CD restricted to neighbouring observations -> Damit ist dieser Test für mich vorerst nicht relevant. 
Funktioniert auch nicht, damn it. Hier ist es aber der Grund, dass meine Matrix nicht stimmt.
Es müssen noch entsprechend die comIds aus der Matrix genommen werden, welche nicht in beiden auftauchen. 
Grundsätzlich scheinen diese Tests nur für balanced Panels zu funktionieren. Daher gibt es die Überlegung die Daten für splm aufzubereiten. 
Dafür spricht auch, dass diese auf Maximum Likelihood beruhen man entsprechen  Model selection machen kann. 
Dagegen spricht das das Model nicht consistent für große N ist. 
'

# ## BIC for Models without precipitation ##
# BIC_noprec <- rep(0,3)
# for(r in 1:3){
#   glm.fit5 <- glm(formula = update(formula_Jul_sm_detrendlog, .~. - poly(Prec_Jul, degree[r, 1], raw = T)),  data = temp2) 
#   BIC_noprec[r] <- BIC(glm.fit5)
# }
# BIC_noprec
# plot(BIC_noprec, ylab="BIC no precipitation")
# 
# r <- which.min(BIC_temp) # cubic Ansatz ist weiterhin am besten ohne Precipitation


#########################################
# #### Bootstrap ####
#########################################
# boot.fn = function (data ,index)  return(coef(  plm(formula = update(formula_Jul_sm_detrendlog, .~. - dummy(comId)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))  ))
# 
# dim(temp2)
# boot.fn(temp2, 1:3961 )
# boot(temp2, boot.fn, 200, stype="w")
# Boot1 <- Boot(glm.fit4, f=Std. Error, R=2000)
# summary(Boot1)
# confint(Boot1)
# summary(glm.fit4)
# 
# library (ISLR)
# boot.fn=function (data ,index) return(coef(lm(mpg ~ horsepower ,data=data ,subset=index))) 
# boot.fn(Auto ,1:392)
# 
# ## Jacknife ##
# library("bootstrap")
# jackknife(temp2, boot.fn)


save("plm.fit_Jul3 ", file="/home/peichl/Documents/projects/correlation/script/script_raw/Crossvalidation/Output_Jul.RData")


###################################################################################################################################################################################################
###################################################################################################################################################################################################
###########################
## Explore Fixed Effects ##


names(plm.fit_Jul3)
fixef(plm.fit_Jul3)
hist(fixef(plm.fit_Jul))
plot(fixef(plm.fit_Jul)) # es gibt hier definitiv clustering, das sollte eventuell räumlich dargestellt werden. Vor allem, wenn sich dies bei den anderen Daten wiederholen sollte. 
# Diese Struktur gibt es beim Mais auch in den anderen Monaten. Das macht auch so Sinn, da die Time-invarianten Effecte als seperater Fehler Term dargestellte werden. 
## Map of Fixed Effects

class(fixef(plm.fit_Jul))
str(fixef(plm.fit_Jul))
fixef <- as.data.frame(as.matrix(fixef(plm.fit_Jul)))
# plot(fixef)
str(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")

summary(siloMaize_logtrend)
summary(fixef[,2])

##########################
## Map of Fixed Effects ##
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(reshape)
library(stringr)
library(classInt)
library(RColorBrewer)

## Lade Datei mit räumlichen Referenzen ##
KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")
str(KreisPolOGR,2)
head(KreisPolOGR)

# spplot(KreisPolOGR, zcol="SHAPE_AREA")
## Transform RS to comId compatible references
KreisPolOGR$comId<-as.integer(str_sub(KreisPolOGR$RS,1,5))
head(fixef)

## Create new spatial data.frame ##
## Merge fixed Effects with Spatial Reference DataFrame via comId
KreisPolOGR_fixef <- merge(KreisPolOGR, fixef, by = "comId")
dim(KreisPolOGR_fixef)
str(KreisPolOGR_fixef, 2)
is.na(KreisPolOGR_fixef)
head(KreisPolOGR_fixef)
class(KreisPolOGR_fixef)

## Plot Spatial Dataframe without color specification
spplot(KreisPolOGR_fixef, zcol="FE", main="Spatial Distribution of Fixed Effects of _Jul - Silo Maize" )

## Plot Spatial Data Frame with colours in accordance to cluster algorithm ##
# Create Class Intervalls via cluster algorith #
classInt <- classIntervals(KreisPolOGR_fixef$FE, n=3, style = "jenks")
classInt
plot(classInt)
pal <- brewer.pal(3, "Blues")

plot(classInt, pal=pal, main="Fisher-Jenks natural breaks", xlab="", ylab="")

## Define breacks
at1 <- classInt$brks
at1
# ##  Anpassen der Breaks ##
# at1[1] <- at1[1] - at1[1]/100
# at1[length(at1)] <- at1[length(at1)] + at1[length(at1)]/100

## Plot with breaks derived Fisher - Jenins Natural Breaks Algorithm with properly assigned colors
spplot(KreisPolOGR_fixef, zcol="FE", at=at1, col.regions=colorRampPalette(pal)(length(at1)-1),main="Spatial Distribution of Fixed Effects of Mai - Silo Maize" )

#####################################################
## Plot Functions of Precipitation and Temperature ##
summary(temp1$siloMaize_logtrend)
hist(temp1$siloMaize_logtrend)
## Temperature ##
coeftest(plm.fit_Jul,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))
b3 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[3]
b4 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[4]
b5 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[5]
b3
b4
b5


## Log
plot(temp1$Tavg, temp1$siloMaize_logtrend, main="Fitted Polynom of Temperature - Log")
summary(temp1$Tavg)
summary(temp1$siloMaize_logtrend)
xcurve <- seq(8.967, 17.4, 0.1)
ycurve <-0 + b3 * xcurve  + b4 * xcurve^2   + b5 * xcurve^3
lines(xcurve,ycurve, col="green", lwd = 2)

## Exp
plot(Yield_Covariates_SM_Jul$Tavg, Yield_Covariates_SM_Jul$siloMaize- mean(Yield_Covariates_SM_Jul$siloMaize), main="Fitted Polynom of Temperature - Exp")
summary(Yield_Covariates_SM_Jul$Tavg)
xcurve <- seq(8.967, 17.4, 0.1)
ycurve <- exp( b3 * xcurve  + b4 * xcurve^2   + b5 * xcurve^3)
lines(xcurve,ycurve, col="green", lwd = 2)

## Precipitation
b1 <-coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[1]
b2 <-  coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[2]

## Log
plot(Yield_Covariates_SM_Jul$Prec, log(Yield_Covariates_SM_Jul$siloMaize - mean(Yield_Covariates_SM_Jul$siloMaize)), main="Fitted Polynom of Precipitation - Log")

summary(Yield_Covariates_SM_Jul$Prec)
xcurve <- seq(5.008, 308.700, 0.1)
ycurve <-   b1 * xcurve + b2 * xcurve^2
summary(ycurve)
lines(xcurve,ycurve, col="green", lwd = 2)

## Exp
plot(Yield_Covariates_SM_Jul$Prec, Yield_Covariates_SM_Jul$siloMaize- mean(Yield_Covariates_SM_Jul$siloMaize), main="Fitted Polynom of Precipitation -Exp")
# plot(Yield_Covariates_SM_Jul$Prec, Yield_Covariates_SM_Jul$siloMaize, main="Fitted Polynom of Precipitation")

summary(Yield_Covariates_SM_Jul$Prec)
xcurve <- seq(5.008, 308.700, 0.1)
ycurve <-  exp(b1 * xcurve + b2 * xcurve^2)
summary(ycurve)
lines(xcurve,ycurve, col="green", lwd = 2)

## Hier sollte ich mir die leverage Statistiken nochmals anschauen. Es gibt einige Werte im Niederschlag, die extrem groß sind und den fit sehr stark beeiflussen;

###########################################################
## Calculate exact percentage change of SMI coefficients ##
b6 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[6]
b7 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[7]
b8 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[8]
b9<- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[9]
b10 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[10]
b11 <- coeftest(plm.fit_Jul3,vcov=vcovHC(plm.fit_Jul3, method = "arellano", , type = "HC4"))[11]
100*(exp(b6)-1)
100*(exp(b7)-1)
100*(exp(b8)-1)
100*(exp(b9)-1)
100*(exp(b10)-1)
100*(exp(b11)-1)


## Yield vs SMI ##
plot(temp1$SMI, SM_Jul$siloMaize_logtrend- mean(Yield_Covariates_SM_Jul$siloMaize_logtrend), main="Yield vs SMI")


x <- seq(1,12,1)
x
log(x)
y <- seq(1999,2010,1)
log(y)
summary(log(siloMaize))

######################################################
## Berechnen des Modells mit trend auf linker Seite ##

summary(plm.fit_Jul)

## Remove linear trend ##
'Fit yield on time and use the residuals of that for detrended yields'

plot(Yield_Covariates_SM_Jul$siloMaize ~ as.integer(Yield_Covariates_SM_Jul$year))
lineartrend <- lm(siloMaize ~ as.integer(year), data= Yield_Covariates_SM_Jul )
summary(lineartrend)
abline(lineartrend)
abline(453.5511,0, col="green")
length(resid(lineartrend))
Yield_Covariates_SM_Jul$siloMaize_lintren <- resid(lineartrend)

r=6
formula_Jul_sm_lintren  <- siloMaize_lintren ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) 

## Remove log trend ##
'Fit log of yield on log of time and use the residuals of that for yields'
plot(log(Yield_Covariates_SM_Jul$siloMaize) ~ log(as.integer(Yield_Covariates_SM_Jul$year)))
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Jul)
abline(logtrend)
summary(logtrend)
abline(6.1115,0, col="green")
Yield_Covariates_SM_Jul$siloMaize_logtrend <- resid(logtrend)


r=6
formula_Jul_sm_detrendlog <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) 

###################################################################
## Determine right degree of poylnomials via BIC for log detrend ##
BIC_logdetrend <- rep(0,9)


for(r in 1:9){
  glm.fit_logdetrend <- glm(formula= update(formula_Jul_sm_detrendlog, .~. + dummy(comId)),  data = Yield_Covariates_SM_Jul) 
  BIC_logdetrend[r] <- BIC(glm.fit_logdetrend)
}

par(mfrow=c(1,1))
plot(BIC_logdetrend, main="BIC without outliers and less leverage with left side detrend" )
which.min(BIC_logdetrend) # hier ist der Wert nun 4
r_logdetrend = 6 

'Hier ändert sich nachvollziehbar in der Struktur und im Ergebnis wenig'

plot(glm.fit_logdetrend)
####################
## PLM Ergebnisse ##
r = r_logdetrend
plm.fit_Jul_detrendlinear <- plm(formula= formula_Jul_sm_detrendlinear,  data = Yield_Covariates_SM_Jul, effect="individual", model="within")
plm.fit_Jul_detrendlog <- plm(formula= formula_Jul_sm_detrendlog,  data = Yield_Covariates_SM_Jul, effect="individual", model="within")

summary(plm.fit_Jul)
summary(plm.fit_Jul_detrendlinear)
summary(plm.fit_Jul_detrendlog)

coeftest(plm.fit_Jul,vcov=vcovHC(plm.fit_Jul, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Jul_detrendlinear,vcov=vcovHC(plm.fit_Jul_detrendlinear, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Jul_detrendlog,vcov=vcovHC(plm.fit_Jul_detrendlog, method = "arellano", , type = "HC4"))

## Gleiche Konfiguration aber andere Jahre: 1:12 statt 1999 - 2010
head(Yield_Covariates_SM_Jul)
Yield_Covariates_SM_Jul$yearshort <- as.integer(Yield_Covariates_SM_Jul$year) - 1999



##############################
#############################
## Use PET instead of Temp ##
#############################
## Plot SMI Pet ##
plot(Yield_Covariates_SM_Jul$siloMaize ~ Yield_Covariates_SM_Jul$Pet)
plot(Yield_Covariates_SM_Jul$siloMaize ~ Yield_Covariates_SM_Jul$Tavg)

## Determine formula
formula_Jul_sm_Pet <- log(siloMaize) ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year))
formula_Jul_sm_Pet_detrendlog <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) 


############################################################################
## Determine right degree of poylnomials via BIC for PET with log detrend ##
BIC_Pet <- rep(0,9)


for(r in 1:9){
  glm.fit_Pet <- glm(formula= update(formula_Jul_sm_Pet_detrendlog, .~. + dummy(comId)),  data = temp1) 
  BIC_Pet[r] <- BIC(glm.fit_Pet)
}

par(mfrow=c(1,1))
plot(BIC_Pet, main="BIC without outliers and less leverage of PET configuration with left side detrend" )
which.min(BIC_Pet) # hier ist der Wert nun 4
r_Pet = 4 
lm.fit_Pet <- lm(formula= update(formula_Jul_sm_Pet_detrendlog, .~. + dummy(comId)),  data = temp1) 
summary(lm.fit_Pet)   # hier scheinen die Fixed Effekte 61 Prozent der Variabilität zu erklären. Das ist sicherlich auch eine Begründung, warum diese weiterhin mit aufgeführt werden
# sollten und vor allema auch für die Vertiefung via maps.

####################
## PLM Ergebnisse ##
head(temp1)
plm.fit_Jul_Pet <- plm(formula= formula_Jul_sm_Pet,  data = temp1, effect="individual", model="within")
plm.fit_Jul_Pet_detrendlog <- plm(formula= formula_Jul_sm_Pet_detrendlog,  data = temp1, effect="individual", model="within")

summary(plm.fit_Jul)

summary(plm.fit_Jul_Pet) # R² ist größer, aber Anteil von SMI ist kleiner.
summary(plm.fit_Jul_Pet_detrendlog)
summary(plm.fit_Jul_detrendlog)

coeftest(plm.fit_Jul_Pet_detrendlog,vcov=vcovHC(plm.fit_Jul_Pet_detrendlog, method = "arellano", , type = "HC4"))



## Heteroskedasdicity ##
r=r_Pet
library(lmtest)
library(car)
bptest(plm.fit_Jul_Pet_detrendlog) # Null, dass es keine heteroskedasdicity gibt, wurde zurückgewiesen. 

coeftest(glm.fit_Pet,vcov=vcovHC(glm.fit_Pet)) # funktioniert nicht, wohl zu viele Dummies
coeftest(plm.fit_Jul_Pet_detrendlog,vcov=vcovHC(plm.fit_Jul_Pet_detrendlog, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Jul_Pet_detrendlog,vcov=vcovHC(plm.fit_Jul_Pet_detrendlog, type = "HC3"))
' Ich denke es macht Sinn das Modell 3 ohne outlier und weniger leverage zu nehmen. Darüberhinaus nehme ich arrelano als methode. Der Grund ist, dass durch 
quasi demeaning serial coorelation verursacht wird. Als Option nehme ich HC4, um für die anderen influential observations zu kontollieren. '
