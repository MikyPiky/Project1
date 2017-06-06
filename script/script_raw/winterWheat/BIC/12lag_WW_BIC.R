#######################################
####  Winter Wheat in December_lag ####
#######################################

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
- Yield_Covariates_WW_Dec_lag.csv (auf December_lag reduzierter Data.Frame)
- Export Data frame for use in BIC_Graphic: file="./data/data_raw/BIC/BIC_WW_Dec_lag.csv")

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
names_Dec_lag <- grep(c("*_Dec_lag"), names)
names_Dec_lag
Yield_Covariates_Dec_lag <- Yield_Covariates[,names_Dec_lag]
names(Yield_Covariates_Dec_lag)
dim(Yield_Covariates_Dec_lag)

## Delete all but SMI, Prec, Tavg and Pet
names(Yield_Covariates_Dec_lag)
Yield_Covariates_Dec_lag <- Yield_Covariates_Dec_lag[,c(1:4)]

## Establish first part of data frame_ time and spatial reference plus Winterwheat ##
names(Yield_Covariates[,c(2,1,3:5,7)])
Yield_Covariates_SM <- Yield_Covariates[,c(2,1,3:5,7)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_WW_Dec_lag <- cbind(Yield_Covariates_SM, Yield_Covariates_Dec_lag)
names(Yield_Covariates_WW_Dec_lag)
names(Yield_Covariates_WW_Dec_lag) <- c( "comId" , "year","com","stateId","state","Winterwheat","SMI", "Prec","Tavg", "Pet")
names(Yield_Covariates_WW_Dec_lag)

#########################################
#### Create stepwise function of SMI ####
#########################################
' Drought Monitor Spezification '
Yield_Covariates_WW_Dec_lag$SMI_GDM <- cut(Yield_Covariates_WW_Dec_lag$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                           labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
#############
## Na-omit ##
sum(is.na(Yield_Covariates_WW_Dec_lag) )
dim(Yield_Covariates_WW_Dec_lag)
Yield_Covariates_WW_Dec_lag_nna <- na.omit(Yield_Covariates_WW_Dec_lag) 
dim(Yield_Covariates_WW_Dec_lag_nna)

## Check for NAs
any(is.na(Yield_Covariates_WW_Dec_lag_nna))
## Reset Rownames
rownames(Yield_Covariates_WW_Dec_lag_nna) <- NULL

## Further work with DataFrame without Yield_Covariates_WW_Dec_lag index ##
Yield_Covariates_WW_Dec_lag <- Yield_Covariates_WW_Dec_lag_nna

#########################################################################
## Remove comIds with less than 7 observations to avoid leveage issues ## 
#########################################################################

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_WW_Dec_lag$comId) < 7 )
table(Yield_Covariates_WW_Dec_lag$comId) < 7 

## comIds mit weniger als 7 Beoachtungen: ##
list <- c(3402, 5117, 5124, 5314, 5334, 5916, 8421, 9762, 12052, 12053, 15001, 15002, 
          15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091)
length(list)
list[[1]]

temp <- Yield_Covariates_WW_Dec_lag
for (i in 1:length(list))
{
  print(Yield_Covariates_WW_Dec_lag[Yield_Covariates_WW_Dec_lag$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

## Number of deleted rows
dim(temp)-dim(Yield_Covariates_WW_Dec_lag)

## Further use old name for data.frame
Yield_Covariates_WW_Dec_lag <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_WW_Dec_lag <- na.omit(Yield_Covariates_WW_Dec_lag) 
rownames(Yield_Covariates_WW_Dec_lag) <- NULL
Yield_Covariates_WW_Dec_lag <- plm.data(Yield_Covariates_WW_Dec_lag, index=c("comId", "year"))
Yield_Covariates_WW_Dec_lag[,c("comId","stateId")] <- lapply(Yield_Covariates_WW_Dec_lag[,c("comId","stateId")], factor )

#################################################
#### Remove log trend of indepedent variable ####
#################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(Winterwheat) ~ log(as.integer(year)), data= Yield_Covariates_WW_Dec_lag)

##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 

## Look Outliers Values ##

Yield_Covariates_WW_Dec_lag[c(3382, 3442, 3454, 2574,3451,3511),]

## Look at other values of outliers com #
Yield_Covariates_WW_Dec_lag[Yield_Covariates_WW_Dec_lag$comId == "12060",] #2003
Yield_Covariates_WW_Dec_lag[Yield_Covariates_WW_Dec_lag$comId == "12065",] #2003
Yield_Covariates_WW_Dec_lag[Yield_Covariates_WW_Dec_lag$comId == "12066",] #2003 
Yield_Covariates_WW_Dec_lag[Yield_Covariates_WW_Dec_lag$comId == "9276",] # 1999: hier sehe ich keinen Grund, warum die Daten geläscht werden sollten
Yield_Covariates_WW_Dec_lag[Yield_Covariates_WW_Dec_lag$comId == "12060",] # 2003
Yield_Covariates_WW_Dec_lag[Yield_Covariates_WW_Dec_lag$comId == "12071",] # 2000

## Interpretation ##
' Im Gegensatz zu SoliMoais nehme ich hier keine Beobachtungen wegen Outlier und  Leverage raus, da es wohl keine Messfehler sind.'

Yield_Covariates_WW_Dec_lag <- na.omit(Yield_Covariates_WW_Dec_lag)
rownames(Yield_Covariates_WW_Dec_lag) <- NULL


#################################################
#### Remove log trend of indepedent variable ####
logtrend <- lm(log(Winterwheat) ~ log(as.integer(year)), data= Yield_Covariates_WW_Dec_lag)
summary(logtrend)
Yield_Covariates_WW_Dec_lag$Winterwheat_logtrend <- resid(logtrend)

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_WW_Dec_lag <- plm.data(Yield_Covariates_WW_Dec_lag, index=c("comId", "year"))
str(Yield_Covariates_WW_Dec_lag)


## Transform comId and stateId to factor ##
Yield_Covariates_WW_Dec_lag[,c("comId","stateId")] <- lapply(Yield_Covariates_WW_Dec_lag[,c("comId","stateId")], factor )
lapply(Yield_Covariates_WW_Dec_lag, class)


###############################################
##### Save Yield_Covariates_WW_Dec_lagober extern ####
write.csv(Yield_Covariates_WW_Dec_lag, file="./data/data_raw/Yield_Covariates_WW_Dec_lag.csv")

#######################################################
#### BIC to choose the degrees of the polynomials  ####
#######################################################
## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

################################################
## Formulas for Model Variations to be tested ##

## with SMI
formula_Dec_lag_WW_detrendlog_SMIPrecTavg <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Dec_lag_WW_detrendlog_SMIPrecPet  <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Dec_lag_WW_detrendlog_SMIPrec     <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Dec_lag_WW_detrendlog_SMIPet      <- Winterwheat_logtrend ~ poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Dec_lag_WW_detrendlog_SMITavg     <- Winterwheat_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Dec_lag_WW_detrendlog_SMI         <- Winterwheat_logtrend ~  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

## no SMI
formula_Dec_lag_WW_detrendlog_PrecTavg <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

formula_Dec_lag_WW_detrendlog_PrecPet  <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + dummy(comId)

formula_Dec_lag_WW_detrendlog_Prec     <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) + dummy(comId)

formula_Dec_lag_WW_detrendlog_Pet      <- Winterwheat_logtrend ~ poly(Pet, degree[r, 2], raw = T)  +  dummy(comId)

formula_Dec_lag_WW_detrendlog_Tavg     <- Winterwheat_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + dummy(comId)


#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################

##################################################
## Loop through various variable configurations ##
BIC_SMIPrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecTavg  <- glm(formula = formula_Dec_lag_WW_detrendlog_SMIPrecTavg, data = Yield_Covariates_WW_Dec_lag) 
  BIC_SMIPrecTavg[r] <- BIC(glm.fit_SMIPrecTavg)
}

BIC_SMIPrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecPet  <- glm(formula = formula_Dec_lag_WW_detrendlog_SMIPrecPet,  data = Yield_Covariates_WW_Dec_lag)
  BIC_SMIPrecPet[r] <- BIC(glm.fit_SMIPrecPet)
}

BIC_SMIPrec <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPrec      <- glm(formula = formula_Dec_lag_WW_detrendlog_SMIPrec,     data = Yield_Covariates_WW_Dec_lag) 
  BIC_SMIPrec[r] <- BIC(glm.fit_SMIPrec)
}

BIC_SMIPet <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPet       <- glm(formula = formula_Dec_lag_WW_detrendlog_SMIPet,      data = Yield_Covariates_WW_Dec_lag) 
  BIC_SMIPet[r] <- BIC(glm.fit_SMIPet)
}

BIC_SMITavg <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMITavg      <- glm(formula = formula_Dec_lag_WW_detrendlog_SMITavg,     data = Yield_Covariates_WW_Dec_lag) 
  BIC_SMITavg[r] <- BIC(glm.fit_SMITavg)
}

BIC_SMI <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMI      <- glm(formula = formula_Dec_lag_WW_detrendlog_SMI,     data = Yield_Covariates_WW_Dec_lag) 
  BIC_SMI[r] <- BIC(glm.fit_SMI)
}

BIC_PrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecTavg  <- glm(formula = formula_Dec_lag_WW_detrendlog_PrecTavg, data = Yield_Covariates_WW_Dec_lag) 
  BIC_PrecTavg[r] <- BIC(glm.fit_PrecTavg)
}

BIC_PrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecPet  <- glm(formula = formula_Dec_lag_WW_detrendlog_PrecPet, data = Yield_Covariates_WW_Dec_lag) 
  BIC_PrecPet[r] <- BIC(glm.fit_PrecPet)
}

BIC_Prec <- rep(0,9)
for(r in 1:9){
  glm.fit_Prec  <- glm(formula = formula_Dec_lag_WW_detrendlog_Prec, data = Yield_Covariates_WW_Dec_lag) 
  BIC_Prec[r] <- BIC(glm.fit_Prec)
}

BIC_Pet  <- rep(0,9)
for(r in 1:9){
  glm.fit_Pet   <- glm(formula = formula_Dec_lag_WW_detrendlog_Pet , data = Yield_Covariates_WW_Dec_lag) 
  BIC_Pet [r] <- BIC(glm.fit_Pet )
}

BIC_Tavg  <- rep(0,9)
for(r in 1:9){
  glm.fit_Tavg   <- glm(formula = formula_Dec_lag_WW_detrendlog_Tavg , data = Yield_Covariates_WW_Dec_lag) 
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
month <-rep("December_lag",99)

BIC_Dec_lag <- cbind(BIC, model ,model_index, index, month)

#######################
## Delete Duplicates ##
which(duplicated(BIC_Dec_lag$BIC))
list3 <- c(20,21,23,24,26,27,31,32,33,34,35,36,40,41,42,43,44,45,47,48,49,50,51,52,53,54,56,57,59,60,62,63,67,68,69,70,71,72,76,77,78,79,80,81)

length(list3)
temp <- BIC_Dec_lag

for (i in 1:44)
{
  print(BIC_Dec_lag[BIC_Dec_lag$index ==list3[i],])
  temp <- (temp[!temp$index==list3[i],])
}
dim(BIC_Dec_lag)
dim(temp)

################################
## Correct created data.frame ##
rownames(temp) <- NULL
BIC_Dec_lag <- temp
lapply(BIC_Dec_lag, class)

############################
## Plot data with ggplot2 ##
g <- ggplot(BIC_Dec_lag,aes(y=BIC, x=index))
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark()
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark() +
  facet_wrap( ~ month)
BIC_Dec_lag

## Export Data frame for use in BIC_Grafic
BIC_WW_Dec_lag <- BIC_Dec_lag
class(BIC_WW_Dec_lag)
write.csv(BIC_WW_Dec_lag, file="./data/data_raw/BIC/BIC_WW_Dec_lag.csv")

################################################################
################################### Explore Models #############
################################################################
###################
## Load Data Set ##
# Yield_Covariates_WW_Dec_lag <- read.csv( file="./data/data_raw/Yield_Covariates_WW_Dec_lag.csv")
# names(Yield_Covariates_WW_Dec_lag)
# Yield_Covariates_WW_Dec_lag$X <- NULL

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_WW_Dec_lag <- plm.data(Yield_Covariates_WW_Dec_lag, index=c("comId", "year"))

## Transform comId and stateId to factor ##
Yield_Covariates_WW_Dec_lag[,c("comId","stateId")] <- lapply(Yield_Covariates_WW_Dec_lag[,c("comId","stateId")], factor )
str(Yield_Covariates_WW_Dec_lag)

#################################
###############################
## Results with smallest BIC ##
###############################
plot(BIC_SMIPrecTavg)
which.min(BIC_SMIPrecTavg)  
r = 5 # wegen Singularität kann  die Kinfiguration 9 nicht berücksichtigt werden. 
best_formula <- formula_Dec_lag_WW_detrendlog_SMIPrecTavg

###################
## GLM Ergebniss ##
glm.fit_WW_BEST_Dec_lag  <- glm(formula = best_formula,  data = Yield_Covariates_WW_Dec_lag)
summary(glm.fit_WW_BEST_Dec_lag)
'AIC: -8106.9'

####################
## PLM Ergebnisse ##
plm.fit_WW_BEST_Dec_lag <- plm(formula = update(best_formula, .~. - dummy(comId)),  data = Yield_Covariates_WW_Dec_lag,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_WW_BEST_Dec_lag)
'Adj. R-Squared: 0.24177'

fixef <-  fixef(plm.fit_WW_BEST_Dec_lag)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
fixef
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Winterwheat/plm.fit_WW_BEST_Dec_lag_FE.csv")

##################
## LM Ergebniss ##
lm.fit_WW_BEST_Dec_lag  <-lm(formula = best_formula,  data = Yield_Covariates_WW_Dec_lag)
summary(lm.fit_WW_BEST_Dec_lag) 
'Adjusted R-squared:  0.7317 '


################################################
## Assessing Influence (Leverage*discrepancy) ##
cutoff_Dec_lag <- 4/((nrow(Yield_Covariates_WW_Dec_lag)-length(lm.fit_WW_BEST_Dec_lag$coefficients)-1)) 
cutoff_Dec_lag
plot(lm.fit_WW_BEST_Dec_lag, which=4, cook_Dec_lag.levels=cutoff_Dec_lag)
cook_Dec_lag <- cooks.distance(lm.fit_WW_BEST_Dec_lag)

nrow(Yield_Covariates_WW_Dec_lag[cook_Dec_lag > cutoff_Dec_lag,]) # 194

year_cooks_Dec_lag <- table(Yield_Covariates_WW_Dec_lag$year[cook_Dec_lag > cutoff_Dec_lag ]) 
year_cooks_Dec_lag
'1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 
  17   11   16   11   56   14    8   10    9   17   11   14 
'

com_cooks_Dec_lag <- sort(table(Yield_Covariates_WW_Dec_lag$com[cook_Dec_lag > cutoff_Dec_lag ] ) )
tail(com_cooks_Dec_lag,20)
'                 Rheinisch-Bergischer Kreis                                   Schwabach                  Siegen-Wittgenstein, Kreis                          S\xf6mmerda, Kreis 
                                          2                                           2                                           2                                           2 
               Teltow-Fl\xe4ming, Landkreis                         Waldshut, Landkreis                        Weimar, krsfr. Stadt       Bad T\xf6lz-Wolfratshausen, Landkreis 
                                          2                                           2                                           2                                           3 
            Berchtesgadener Land, Landkreis                             Lahn-Dill-Kreis                     Mainz-Bingen, Landkreis                      Nordsachsen, Landkreis 
                                          3                                           3                                           3                                           3 
                       Oberbergischer Kreis                        Oberhavel, Landkreis                Leverkusen, Kreisfreie Stadt        M\xf6nchengladbach, Kreisfreie Stadt 
                                          3                                           3                                           4                                           4 
           Oberspreewald-Lausitz, Landkreis                           Barnim, Landkreis                                 Olpe, Kreis                   Spree-Nei\xdfe, Landkreis 
                                          4                                           5                                           5                                           5 '

########################
## Heteroskedasdicity ##
bptest(glm.fit_WW_BEST_Dec_lag) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_WW_BEST_Dec_lag)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_WW_BEST_Dec_lag, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_WW_BEST_Dec_lag)
pbgtest(plm.fit_WW_BEST_Dec_lag) 
'
both, H_1 of serial autocorrelation cannot be rejected
'

#################################
## Correct the Standard Errors ##
#################################

## Correct Standard Errors used in table ##
coeftest(plm.fit_WW_BEST_Dec_lag)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_WW_BEST_Dec_lag,vcov=vcovHC(plm.fit_WW_BEST_Dec_lag,method = "arellano", type = "HC0")) 
cov0_WW_BEST_Dec_lag <- vcovHC(plm.fit_WW_BEST_Dec_lag,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_WW_BEST_Dec_lag <- sqrt(diag(cov0_WW_BEST_Dec_lag))

cov0.1_WW_BEST_Dec_lag <- vcovHC(plm.fit_WW_BEST_Dec_lag,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_WW_BEST_Dec_lag <- sqrt(diag(cov0.1_WW_BEST_Dec_lag))
# 
# ## Beck Katz:
# # coeftest(plm.fit_WW_BEST_Dec_lag, vcov = function(x) vcovBK(plm.fit_WW_BEST_Dec_lag,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_WW_BEST_Dec_lag,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# ## Driscoll Kraay ##
# summary(plm.fit_WW_BEST_Dec_lag)
coeftest(plm.fit_WW_BEST_Dec_lag,  vcov=function(x) vcovSCC(plm.fit_WW_BEST_Dec_lag,method = "arellano",type = "HC0")) 
cov2_WW_BEST_Dec_lag     <- vcovSCC(plm.fit_WW_BEST_Dec_lag,method = "arellano",type = "HC0")
DK.se_WW_BEST_Dec_lag    <- sqrt(diag(cov2_WW_BEST_Dec_lag))
# 
# cov2.1_WW_BEST_Dec_lag   <- vcovSCC(plm.fit_WW_BEST_Dec_lag,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_WW_BEST_Dec_lag <- sqrt(diag(cov2.1_WW_BEST_Dec_lag))

# cov2.2_WW_BEST_Dec_lag   <- vcovSCC(plm.fit_WW_BEST_Dec_lag,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_WW_BEST_Dec_lag <- sqrt(diag(cov2.2_WW_BEST_Dec_lag))
# 
# cov2.3_WW_BEST_Dec_lag   <- vcovSCC(plm.fit_WW_BEST_Dec_lag,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_WW_BEST_Dec_lag <- sqrt(diag(cov2.3_WW_BEST_Dec_lag))
# 
# cov2.4_WW_BEST_Dec_lag   <- vcovSCC(plm.fit_WW_BEST_Dec_lag,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_WW_BEST_Dec_lag <- sqrt(diag(cov2.4_WW_BEST_Dec_lag))
# 
cov2.5_WW_BEST_Dec_lag   <- vcovSCC(plm.fit_WW_BEST_Dec_lag,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_WW_BEST_Dec_lag <- sqrt(diag(cov2.5_WW_BEST_Dec_lag))

## Cameron et al /Thompson : doouble-clustering estimator  ##
# coeftest(plm.fit_WW_BEST_Dec_lag, vcovDC(plm.fit_WW_BEST_Dec_lag, method = "arellano", type = "HC0"))
cov3_WW_BEST_Dec_lag <- vcovDC(plm.fit_WW_BEST_Dec_lag, method = "arellano", type = "HC0")
CT.se_WW_BEST_Dec_lag <- sqrt(diag(cov3_WW_BEST_Dec_lag))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_WW_BEST_Dec_lag, Wh.se_serial_WW_BEST_Dec_lag,  DK.se_WW_BEST_Dec_lag, DK2.5.se_WW_BEST_Dec_lag, CT.se_WW_BEST_Dec_lag)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag,plm.fit_WW_BEST_Dec_lag,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - December_lag",
          dep.var.labels = "log(Winterwheat)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Winterwheat/WW_Dec_lag_best.txt"
)


#########################################################
## Results with smallest BIC of Standard Configuration ##
#########################################################
## Caveat: Für Winterweizen ist die Standard -  Configuration immer am besten (außer im November_lag) ##
' Deswegen produziere ich keine anderen Output, dennoch schreibe ich die anderen Tables, da 
das eventiuell spätere Arbeiten erleichtert.'
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_WW_BEST_Dec_lag, Wh.se_serial_WW_BEST_Dec_lag,  DK.se_WW_BEST_Dec_lag, DK2.5.se_WW_BEST_Dec_lag, CT.se_WW_BEST_Dec_lag)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag,plm.fit_WW_BEST_Dec_lag,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - December_lag",
          dep.var.labels = "log(Winterwheat)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Winterwheat/WW_Dec_lag_bestStandard.txt"
)

########################################
## Results with smallest BIC with SMI ##
########################################
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_WW_BEST_Dec_lag, Wh.se_serial_WW_BEST_Dec_lag,  DK.se_WW_BEST_Dec_lag, DK2.5.se_WW_BEST_Dec_lag, CT.se_WW_BEST_Dec_lag)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag, plm.fit_WW_BEST_Dec_lag,plm.fit_WW_BEST_Dec_lag,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - December_lag",
          dep.var.labels = "log(Winterwheat)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Winterwheat/WW_Dec_lag_bestSM.txt"
)

