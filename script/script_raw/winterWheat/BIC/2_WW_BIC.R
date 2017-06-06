###################################
####  Winter Wheat in February ####
###################################

'
######################
## File Discription ##

The purpose of this script is to estimate the impact of weather fluctuations in the month mentionend above on yearly crop yield.

This is done by the following the steps:
- Create data frame with Winterwheat as dependent and variables of the month above as independent variables
- Create stepwise function which is based on drought categories of german drought monitor
- Remove comIds with less than 7 observations to avoid leveage issues
- Remove log trend of indepedent variable
- Delete outliers which appear to be measurement error
- Use BIC to choose the degrees of the polynomial and to compare various model configurations
- Loop through polynomials configuration of each model; highest possible polynomial is of degree 3
- Compare models graphically
- Explore Models
- Model with lowest BIC in general: Tavg, Prec, SMI
- Model with lowest BIC of standard configuration: Tavg, Prec, SMI
- Model with lowest BIC with SMI: Tavg, Prec, SMI

- Correct Standard Errors with either Driscoll Kray or Cameron et al /Thompson estimator

The --vcovHC– function estimates three heteroskedasticity-consistent covariance estimators:
• "white1" - for general heteroskedasticity but no serial correlation. Recommended for random effects.
• "white2" - is "white1" restricted to a common variance within groups. Recommended for random effects.
• "arellano" - both heteroskedasticity and serial correlation. Recommended for fixed effects.
The following options apply*:
• HC0 - heteroskedasticity consistent. The default.
• HC1,HC2, HC3 – Recommended for small samples. HC3 gives less weight to influential
observations.
• HC4 - small samples with influential observations
• HAC - heteroskedasticity and autocorrelation consistent (type ?vcovHAC for more
details)
Solution for serial correlation: Cluster by groups.
Solution for cross sectional correlation: Cluster by time

Ich arbeitet vorerst mir Driscoll Kraay und weighting von 1 (maxlag=0). Die Ergebnisse sollten solide sein, da Cameron/Thompson ähnlich ist



## Input ##

- aus 4km_tmax: Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_ww.csv (komplete data.frame)


## Output ##
- Yield_Covariates_WW_Feb.csv (auf February reduzierter Data.Frame)
- Export Data frame for use in BIC_Graphic: file="./data/data_raw/BIC/BIC_WW_Feb.csv")

- Export Data Frame of Fixed Effects to be used in Script FixedEffects_Graphic: 
"./figures/figures_exploratory/FixedEffects/Winterwheat/..."
'

###################
## Load Packages ##
library(plm)
library(boot)
library(gtools)
library(lme4)
library(lmtest)
library(car)
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(reshape)
library(stringr)
library(classInt)
library(RColorBrewer)
library(stargazer)
library(ggplot2)
####################################################################################################################################################################

#################################################################################################################
#### Create data frame with Winterwheat as dependent and variables of the month above as independent variables ####
#################################################################################################################

## Read in large Dataframe for Maize ##
Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_ww.csv")
Yield_Covariates$X <- NULL

## For publication worth regression output need to change data names ##
'Get rid of variables which are not necessary: other months and other not needed variables'
names(Yield_Covariates)
names <- names(Yield_Covariates)
names_Feb <- grep(c("*_Feb"), names)
names_Feb
Yield_Covariates_Feb <- Yield_Covariates[,names_Feb]
names(Yield_Covariates_Feb)
dim(Yield_Covariates_Feb)

## Delete all but SMI, Prec, Tavg and Pet
names(Yield_Covariates_Feb)
Yield_Covariates_Feb <- Yield_Covariates_Feb[,c(1:4)]

## Establish first part of data frame_ time and spatial reference plus Winterwheat ##
names(Yield_Covariates[,c(2,1,3:5,7)])
Yield_Covariates_SM <- Yield_Covariates[,c(2,1,3:5,7)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_WW_Feb <- cbind(Yield_Covariates_SM, Yield_Covariates_Feb)
names(Yield_Covariates_WW_Feb)
names(Yield_Covariates_WW_Feb) <- c( "comId" , "year","com","stateId","state","Winterwheat","SMI", "Prec","Tavg", "Pet")
names(Yield_Covariates_WW_Feb)

#########################################
#### Create stepwise function of SMI ####
#########################################
' Drought Monitor Spezification '
Yield_Covariates_WW_Feb$SMI_GDM <- cut(Yield_Covariates_WW_Feb$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))

table(Yield_Covariates_WW_Feb$SMI_GDM )
table <- table(Yield_Covariates_WW_Feb$SMI_GDM,Yield_Covariates_WW_Feb$year  )
write.csv(table, "./figures/figures_exploratory/SMI/Feb_SMI_year" )


#############
## Na-omit ##
sum(is.na(Yield_Covariates_WW_Feb) )
dim(Yield_Covariates_WW_Feb)
Yield_Covariates_WW_Feb_nna <- na.omit(Yield_Covariates_WW_Feb) 
dim(Yield_Covariates_WW_Feb_nna)

## Check for NAs
any(is.na(Yield_Covariates_WW_Feb_nna))
## Reset Rownames
rownames(Yield_Covariates_WW_Feb_nna) <- NULL

## Further work with DataFrame without Yield_Covariates_WW_Feb index ##
Yield_Covariates_WW_Feb <- Yield_Covariates_WW_Feb_nna

#########################################################################
## Remove comIds with less than 7 observations to avoid leveage issues ## 
#########################################################################

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_WW_Feb$comId) < 7 )
table(Yield_Covariates_WW_Feb$comId) < 7 

## comIds mit weniger als 7 Beoachtungen: ##
list <- c(3402, 5117, 5124, 5314, 5334, 5916, 8421, 9762, 12052, 12053, 15001, 15002, 
          15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091)
length(list)
list[[1]]

temp <- Yield_Covariates_WW_Feb
for (i in 1:length(list))
{
  print(Yield_Covariates_WW_Feb[Yield_Covariates_WW_Feb$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

## Number of deleted rows
dim(temp)-dim(Yield_Covariates_WW_Feb)

## Further use old name for data.frame
Yield_Covariates_WW_Feb <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_WW_Feb <- na.omit(Yield_Covariates_WW_Feb) 
rownames(Yield_Covariates_WW_Feb) <- NULL
Yield_Covariates_WW_Feb <- plm.data(Yield_Covariates_WW_Feb, index=c("comId", "year"))
Yield_Covariates_WW_Feb[,c("comId","stateId")] <- lapply(Yield_Covariates_WW_Feb[,c("comId","stateId")], factor )

#################################################
#### Remove log trend of indepedent variable ####
#################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(Winterwheat) ~ log(as.integer(year)), data= Yield_Covariates_WW_Feb)

##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 

## Look Outliers Values ##

Yield_Covariates_WW_Feb[c(3382, 3442, 3454, 2574,3451,3511),]

## Look at other values of outliers com #
Yield_Covariates_WW_Feb[Yield_Covariates_WW_Feb$comId == "12060",] #2003
Yield_Covariates_WW_Feb[Yield_Covariates_WW_Feb$comId == "12065",] #2003
Yield_Covariates_WW_Feb[Yield_Covariates_WW_Feb$comId == "12066",] #2003 
Yield_Covariates_WW_Feb[Yield_Covariates_WW_Feb$comId == "9276",] # 1999: hier sehe ich keinen Grund, warum die Daten geläscht werden sollten
Yield_Covariates_WW_Feb[Yield_Covariates_WW_Feb$comId == "12060",] # 2003
Yield_Covariates_WW_Feb[Yield_Covariates_WW_Feb$comId == "12071",] # 2000

## Interpretation ##
' Im Gegensatz zu SoliMoais nehme ich hier keine Beobachtungen wegen Outlier und  Leverage raus, da es wohl keine Messfehler sind.'

Yield_Covariates_WW_Feb <- na.omit(Yield_Covariates_WW_Feb)
rownames(Yield_Covariates_WW_Feb) <- NULL


#################################################
#### Remove log trend of indepedent variable ####
logtrend <- lm(log(Winterwheat) ~ log(as.integer(year)), data= Yield_Covariates_WW_Feb)
summary(logtrend)
Yield_Covariates_WW_Feb$Winterwheat_logtrend <- resid(logtrend)

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_WW_Feb <- plm.data(Yield_Covariates_WW_Feb, index=c("comId", "year"))
str(Yield_Covariates_WW_Feb)


## Transform comId and stateId to factor ##
Yield_Covariates_WW_Feb[,c("comId","stateId")] <- lapply(Yield_Covariates_WW_Feb[,c("comId","stateId")], factor )
lapply(Yield_Covariates_WW_Feb, class)


###############################################
##### Save Yield_Covariates_WW_Febober extern ####
write.csv(Yield_Covariates_WW_Feb, file="./data/data_raw/Yield_Covariates_WW_Feb.csv")

#######################################################
#### BIC to choose the degrees of the polynomials  ####
#######################################################
## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

################################################
## Formulas for Model Variations to be tested ##

## with SMI
formula_Feb_WW_detrendlog_SMIPrecTavg <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Feb_WW_detrendlog_SMIPrecPet  <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Feb_WW_detrendlog_SMIPrec     <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Feb_WW_detrendlog_SMIPet      <- Winterwheat_logtrend ~ poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Feb_WW_detrendlog_SMITavg     <- Winterwheat_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Feb_WW_detrendlog_SMI         <- Winterwheat_logtrend ~  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

## no SMI
formula_Feb_WW_detrendlog_PrecTavg <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

formula_Feb_WW_detrendlog_PrecPet  <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + dummy(comId)

formula_Feb_WW_detrendlog_Prec     <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) + dummy(comId)

formula_Feb_WW_detrendlog_Pet      <- Winterwheat_logtrend ~ poly(Pet, degree[r, 2], raw = T)  +  dummy(comId)

formula_Feb_WW_detrendlog_Tavg     <- Winterwheat_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + dummy(comId)


#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################

##################################################
## Loop through various variable configurations ##
BIC_SMIPrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecTavg  <- glm(formula = formula_Feb_WW_detrendlog_SMIPrecTavg, data = Yield_Covariates_WW_Feb) 
  BIC_SMIPrecTavg[r] <- BIC(glm.fit_SMIPrecTavg)
}

BIC_SMIPrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecPet  <- glm(formula = formula_Feb_WW_detrendlog_SMIPrecPet,  data = Yield_Covariates_WW_Feb)
  BIC_SMIPrecPet[r] <- BIC(glm.fit_SMIPrecPet)
}

BIC_SMIPrec <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPrec      <- glm(formula = formula_Feb_WW_detrendlog_SMIPrec,     data = Yield_Covariates_WW_Feb) 
  BIC_SMIPrec[r] <- BIC(glm.fit_SMIPrec)
}

BIC_SMIPet <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPet       <- glm(formula = formula_Feb_WW_detrendlog_SMIPet,      data = Yield_Covariates_WW_Feb) 
  BIC_SMIPet[r] <- BIC(glm.fit_SMIPet)
}

BIC_SMITavg <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMITavg      <- glm(formula = formula_Feb_WW_detrendlog_SMITavg,     data = Yield_Covariates_WW_Feb) 
  BIC_SMITavg[r] <- BIC(glm.fit_SMITavg)
}

BIC_SMI <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMI      <- glm(formula = formula_Feb_WW_detrendlog_SMI,     data = Yield_Covariates_WW_Feb) 
  BIC_SMI[r] <- BIC(glm.fit_SMI)
}

BIC_PrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecTavg  <- glm(formula = formula_Feb_WW_detrendlog_PrecTavg, data = Yield_Covariates_WW_Feb) 
  BIC_PrecTavg[r] <- BIC(glm.fit_PrecTavg)
}

BIC_PrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecPet  <- glm(formula = formula_Feb_WW_detrendlog_PrecPet, data = Yield_Covariates_WW_Feb) 
  BIC_PrecPet[r] <- BIC(glm.fit_PrecPet)
}

BIC_Prec <- rep(0,9)
for(r in 1:9){
  glm.fit_Prec  <- glm(formula = formula_Feb_WW_detrendlog_Prec, data = Yield_Covariates_WW_Feb) 
  BIC_Prec[r] <- BIC(glm.fit_Prec)
}

BIC_Pet  <- rep(0,9)
for(r in 1:9){
  glm.fit_Pet   <- glm(formula = formula_Feb_WW_detrendlog_Pet , data = Yield_Covariates_WW_Feb) 
  BIC_Pet [r] <- BIC(glm.fit_Pet )
}

BIC_Tavg  <- rep(0,9)
for(r in 1:9){
  glm.fit_Tavg   <- glm(formula = formula_Feb_WW_detrendlog_Tavg , data = Yield_Covariates_WW_Feb) 
  BIC_Tavg [r] <- BIC(glm.fit_Tavg )
}

## Compare BIC values ##
BIC <- c(BIC_SMIPrecTavg, BIC_SMIPrecPet, BIC_SMIPrec, BIC_SMIPet, BIC_SMITavg, BIC_SMI, BIC_Prec, BIC_Tavg, BIC_Pet, BIC_PrecTavg, BIC_PrecPet)
BIC
par(mfrow=c(1,1))
plot(BIC)

###########################
## Plot BIC with ggplot2 ##
###########################

##############################################
## Create Dataframe for plotting in ggplot2 ##

## repeat name of modelconfiguration ##
list <-c("01_SMIPrecTavg", "02_SMIPrecPet", "03_SMIPrec", "04_SMIPet",
         "05_SMITavg", "06_SMI", "07_Prec", "08_Tavg", "09_Pet", "10_PrecTavg", "11_PrecPet")
list2 <- 1:11

model <- NULL
model_index <- NULL

for (i in 1:11)
{
  x <- rep(list[i],9)
  y <- rep(list2[i],9)
  model <- append(model, x)
  model_index <- as.numeric(append(model_index, y))
}


###################################
## Combine data in on data.frame ##
BIC <- as.data.frame(BIC)
model <- as.data.frame(model)
model_index <- as.data.frame(model_index)
index <- 1:99
month <-rep("February",99)

BIC_Feb <- cbind(BIC, model ,model_index, index, month)

#######################
## Delete Duplicates ##
which(duplicated(BIC_Feb$BIC))
list3 <- c(20,21,23,24,26,27,31,32,33,34,35,36,40,41,42,43,44,45,47,48,49,50,51,52,53,54,56,57,59,60,62,63,67,68,69,70,71,72,76,77,78,79,80,81)

length(list3)
temp <- BIC_Feb

for (i in 1:44)
{
  print(BIC_Feb[BIC_Feb$index ==list3[i],])
  temp <- (temp[!temp$index==list3[i],])
}
dim(BIC_Feb)
dim(temp)

################################
## Correct created data.frame ##
rownames(temp) <- NULL
BIC_Feb <- temp
lapply(BIC_Feb, class)

############################
## Plot data with ggplot2 ##
g <- ggplot(BIC_Feb,aes(y=BIC, x=index))
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark()
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark() +
  facet_wrap( ~ month)
BIC_Feb

## Export Data frame for use in BIC_Grafic
BIC_WW_Feb <- BIC_Feb
class(BIC_WW_Feb)
write.csv(BIC_WW_Feb, file="./data/data_raw/BIC/BIC_WW_Feb.csv")

################################################################
################################### Explore Models #############
################################################################
###################
## Load Data Set ##
# Yield_Covariates_WW_Feb <- read.csv( file="./data/data_raw/Yield_Covariates_WW_Feb.csv")
# names(Yield_Covariates_WW_Feb)
# Yield_Covariates_WW_Feb$X <- NULL

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_WW_Feb <- plm.data(Yield_Covariates_WW_Feb, index=c("comId", "year"))

## Transform comId and stateId to factor ##
Yield_Covariates_WW_Feb[,c("comId","stateId")] <- lapply(Yield_Covariates_WW_Feb[,c("comId","stateId")], factor )
str(Yield_Covariates_WW_Feb)

#################################
###############################
## Results with smallest BIC ##
###############################
plot(BIC_SMIPrecTavg)
which.min(BIC_SMIPrecTavg)  
r = 8 # Da 8 weniger als 8 von 9 weg ist 
best_formula <- formula_Feb_WW_detrendlog_SMIPrecTavg


###################
## GLM Ergebniss ##
glm.fit_WW_BEST_Feb  <- glm(formula = best_formula,  data = Yield_Covariates_WW_Feb)
summary(glm.fit_WW_BEST_Feb)
'AIC: -7931.7'

####################
## PLM Ergebnisse ##
plm.fit_WW_BEST_Feb <- plm(formula = update(best_formula, .~. - dummy(comId)),  data = Yield_Covariates_WW_Feb,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_WW_BEST_Feb)
'Adj. R-Squared: 0.21293'

fixef <-  fixef(plm.fit_WW_BEST_Feb)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
fixef
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Winterwheat/plm.fit_WW_BEST_Feb_FE.csv")

##################
## LM Ergebniss ##
lm.fit_WW_BEST_Feb  <-lm(formula = best_formula,  data = Yield_Covariates_WW_Feb)
summary(lm.fit_WW_BEST_Feb) 
'Adjusted R-squared:  0.7201'

################################################
## Assessing Influence (Leverage*discrepancy) ##
cutoff_Feb <- 4/((nrow(Yield_Covariates_WW_Feb)-length(lm.fit_WW_BEST_Feb$coefficients)-1)) 
cutoff_Feb
plot(lm.fit_WW_BEST_Feb, which=4, cook_Feb.levels=cutoff_Feb)
cook_Feb <- cooks.distance(lm.fit_WW_BEST_Feb)

nrow(Yield_Covariates_WW_Feb[cook_Feb > cutoff_Feb,]) # 214

year_cooks_Feb <- table(Yield_Covariates_WW_Feb$year[cook_Feb > cutoff_Feb ]) 
year_cooks_Feb
'1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 
   6   10    4   17   48   26   11    8   48   13   10   13 
'

com_cooks_Feb <- sort(table(Yield_Covariates_WW_Feb$com[cook_Feb > cutoff_Feb ] ) )
tail(com_cooks_Feb,20)
'

                                   F\xfcrth                             Lahn-Dill-Kreis                              Leipzig, Stadt                 Magdeburg, Kreisfreie Stadt 
                                          3                                           3                                           3                                           3 
       M\xf6nchengladbach, Kreisfreie Stadt                      Nordsachsen, Landkreis                        Oberbergischer Kreis                        Oberhavel, Landkreis 
                                          3                                           3                                           3                                           3 
              Potsdam-Mittelmark, Landkreis                  Rheinisch-Bergischer Kreis                            Steinfurt, Kreis                Teltow-Fl\xe4ming, Landkreis 
                                          3                                           3                                           3                                           3 
                       Weimar, krsfr. Stadt       Bad T\xf6lz-Wolfratshausen, Landkreis                           Barnim, Landkreis                     Landkreis Uecker-Randow 
                                          3                                           4                                           4                                           4 
                        Miesbach, Landkreis                   Spree-Nei\xdfe, Landkreis            Oberspreewald-Lausitz, Landkreis                                 Olpe, Kreis 
                                          4                                           4                                           5                                           5 '


########################
## Heteroskedasdicity ##
bptest(glm.fit_WW_BEST_Feb) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_WW_BEST_Feb)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_WW_BEST_Feb, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_WW_BEST_Feb)
pbgtest(plm.fit_WW_BEST_Feb) 
'
both, H_1 of serial autocorrelation cannot be rejected
'

#################################
## Correct the Standard Errors ##
#################################

## Correct Standard Errors used in table ##
coeftest(plm.fit_WW_BEST_Feb)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_WW_BEST_Feb,vcov=vcovHC(plm.fit_WW_BEST_Feb,method = "arellano", type = "HC0")) 
cov0_WW_BEST_Feb <- vcovHC(plm.fit_WW_BEST_Feb,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_WW_BEST_Feb <- sqrt(diag(cov0_WW_BEST_Feb))

cov0.1_WW_BEST_Feb <- vcovHC(plm.fit_WW_BEST_Feb,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_WW_BEST_Feb <- sqrt(diag(cov0.1_WW_BEST_Feb))
# 
# ## Beck Katz:
# # coeftest(plm.fit_WW_BEST_Feb, vcov = function(x) vcovBK(plm.fit_WW_BEST_Feb,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_WW_BEST_Feb,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# ## Driscoll Kraay ##
# summary(plm.fit_WW_BEST_Feb)
coeftest(plm.fit_WW_BEST_Feb,  vcov=function(x) vcovSCC(plm.fit_WW_BEST_Feb,method = "arellano",type = "HC0")) 
cov2_WW_BEST_Feb     <- vcovSCC(plm.fit_WW_BEST_Feb,method = "arellano",type = "HC0")
DK.se_WW_BEST_Feb    <- sqrt(diag(cov2_WW_BEST_Feb))
# 
# cov2.1_WW_BEST_Feb   <- vcovSCC(plm.fit_WW_BEST_Feb,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_WW_BEST_Feb <- sqrt(diag(cov2.1_WW_BEST_Feb))

# cov2.2_WW_BEST_Feb   <- vcovSCC(plm.fit_WW_BEST_Feb,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_WW_BEST_Feb <- sqrt(diag(cov2.2_WW_BEST_Feb))
# 
# cov2.3_WW_BEST_Feb   <- vcovSCC(plm.fit_WW_BEST_Feb,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_WW_BEST_Feb <- sqrt(diag(cov2.3_WW_BEST_Feb))
# 
# cov2.4_WW_BEST_Feb   <- vcovSCC(plm.fit_WW_BEST_Feb,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_WW_BEST_Feb <- sqrt(diag(cov2.4_WW_BEST_Feb))
# 
cov2.5_WW_BEST_Feb   <- vcovSCC(plm.fit_WW_BEST_Feb,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_WW_BEST_Feb <- sqrt(diag(cov2.5_WW_BEST_Feb))

## Cameron et al /Thompson : doouble-clustering estimator  ##
# coeftest(plm.fit_WW_BEST_Feb, vcovDC(plm.fit_WW_BEST_Feb, method = "arellano", type = "HC0"))
cov3_WW_BEST_Feb <- vcovDC(plm.fit_WW_BEST_Feb, method = "arellano", type = "HC0")
CT.se_WW_BEST_Feb <- sqrt(diag(cov3_WW_BEST_Feb))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_WW_BEST_Feb, Wh.se_serial_WW_BEST_Feb,  DK.se_WW_BEST_Feb, DK2.5.se_WW_BEST_Feb, CT.se_WW_BEST_Feb)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb,plm.fit_WW_BEST_Feb,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - February",
          dep.var.labels = "log(Winterwheat)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Winterwheat/WW_Feb_best.txt"
)


#########################################################
## Results with smallest BIC of Standard Configuration ##
#########################################################
' Best standard is also best model in general'
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_WW_BEST_Feb, Wh.se_serial_WW_BEST_Feb,  DK.se_WW_BEST_Feb, DK2.5.se_WW_BEST_Feb, CT.se_WW_BEST_Feb)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb,plm.fit_WW_BEST_Feb,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - February",
          dep.var.labels = "log(Winterwheat)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,      
          type="text", out="./figures/figures_exploratory/BIC/Winterwheat/WW_Feb_bestStandard.txt"
)

########################################
## Results with smallest BIC with SMI ##
########################################

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_WW_BEST_Feb, Wh.se_serial_WW_BEST_Feb,  DK.se_WW_BEST_Feb, DK2.5.se_WW_BEST_Feb, CT.se_WW_BEST_Feb)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb, plm.fit_WW_BEST_Feb,plm.fit_WW_BEST_Feb,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - February",
          dep.var.labels = "log(Winterwheat)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,             
          type="text", out="./figures/figures_exploratory/BIC/Winterwheat/WW_Feb_bestSM.txt"
)
