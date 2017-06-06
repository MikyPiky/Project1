################################
####  Winter Wheat in March ####
################################

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
- Yield_Covariates_WW_Mar.csv (auf March reduzierter Data.Frame)
- Export Data frame for use in BIC_Graphic: file="./data/data_raw/BIC/BIC_WW_Mar.csv")

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
names_Mar <- grep(c("*_Mar"), names)
names_Mar
Yield_Covariates_Mar <- Yield_Covariates[,names_Mar]
names(Yield_Covariates_Mar)
dim(Yield_Covariates_Mar)

## Delete all but SMI, Prec, Tavg and Pet
names(Yield_Covariates_Mar)
Yield_Covariates_Mar <- Yield_Covariates_Mar[,c(1:4)]

## Establish first part of data frame_ time and spatial reference plus Winterwheat ##
names(Yield_Covariates[,c(2,1,3:5,7)])
Yield_Covariates_SM <- Yield_Covariates[,c(2,1,3:5,7)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_WW_Mar <- cbind(Yield_Covariates_SM, Yield_Covariates_Mar)
names(Yield_Covariates_WW_Mar)
names(Yield_Covariates_WW_Mar) <- c( "comId" , "year","com","stateId","state","Winterwheat","SMI", "Prec","Tavg", "Pet")
names(Yield_Covariates_WW_Mar)

#########################################
#### Create stepwise function of SMI ####
#########################################
' Drought Monitor Spezification '
Yield_Covariates_WW_Mar$SMI_GDM <- cut(Yield_Covariates_WW_Mar$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
#############
## Na-omit ##
sum(is.na(Yield_Covariates_WW_Mar) )
dim(Yield_Covariates_WW_Mar)
Yield_Covariates_WW_Mar_nna <- na.omit(Yield_Covariates_WW_Mar) 
dim(Yield_Covariates_WW_Mar_nna)

## Check for NAs
any(is.na(Yield_Covariates_WW_Mar_nna))
## Reset Rownames
rownames(Yield_Covariates_WW_Mar_nna) <- NULL

## Further work with DataFrame without Yield_Covariates_WW_Mar index ##
Yield_Covariates_WW_Mar <- Yield_Covariates_WW_Mar_nna

#########################################################################
## Remove comIds with less than 7 observations to avoid leveage issues ## 
#########################################################################

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_WW_Mar$comId) < 7 )
table(Yield_Covariates_WW_Mar$comId) < 7 

## comIds mit weniger als 7 Beoachtungen: ##
list <- c(3402, 5117, 5124, 5314, 5334, 5916, 8421, 9762, 12052, 12053, 15001, 15002, 
          15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091)
length(list)
list[[1]]

temp <- Yield_Covariates_WW_Mar
for (i in 1:length(list))
{
  print(Yield_Covariates_WW_Mar[Yield_Covariates_WW_Mar$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

## Number of deleted rows
dim(temp)-dim(Yield_Covariates_WW_Mar)

## Further use old name for data.frame
Yield_Covariates_WW_Mar <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_WW_Mar <- na.omit(Yield_Covariates_WW_Mar) 
rownames(Yield_Covariates_WW_Mar) <- NULL
Yield_Covariates_WW_Mar <- plm.data(Yield_Covariates_WW_Mar, index=c("comId", "year"))
Yield_Covariates_WW_Mar[,c("comId","stateId")] <- lapply(Yield_Covariates_WW_Mar[,c("comId","stateId")], factor )

#################################################
#### Remove log trend of indepedent variable ####
#################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(Winterwheat) ~ log(as.integer(year)), data= Yield_Covariates_WW_Mar)

##########################
## Issue with Outliers ###
##########################
par(mfrow = c(2,2))
plot(logtrend) 

## Look Outliers Values ##

Yield_Covariates_WW_Mar[c(3382, 3442, 3454, 2574,3451,3511),]

## Look at other values of outliers com #
Yield_Covariates_WW_Mar[Yield_Covariates_WW_Mar$comId == "12060",] #2003
Yield_Covariates_WW_Mar[Yield_Covariates_WW_Mar$comId == "12065",] #2003
Yield_Covariates_WW_Mar[Yield_Covariates_WW_Mar$comId == "12066",] #2003 
Yield_Covariates_WW_Mar[Yield_Covariates_WW_Mar$comId == "9276",] # 1999: hier sehe ich keinen Grund, warum die Daten geläscht werden sollten
Yield_Covariates_WW_Mar[Yield_Covariates_WW_Mar$comId == "12060",] # 2003
Yield_Covariates_WW_Mar[Yield_Covariates_WW_Mar$comId == "12071",] # 2000

## Interpretation ##
' Im Gegensatz zu SoliMoais nehme ich hier keine Beobachtungen wegen Outlier und  Leverage raus, da es wohl keine Messfehler sind.'

Yield_Covariates_WW_Mar <- na.omit(Yield_Covariates_WW_Mar)
rownames(Yield_Covariates_WW_Mar) <- NULL


#################################################
#### Remove log trend of indepedent variable ####
logtrend <- lm(log(Winterwheat) ~ log(as.integer(year)), data= Yield_Covariates_WW_Mar)
summary(logtrend)
Yield_Covariates_WW_Mar$Winterwheat_logtrend <- resid(logtrend)

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_WW_Mar <- plm.data(Yield_Covariates_WW_Mar, index=c("comId", "year"))
str(Yield_Covariates_WW_Mar)


## Transform comId and stateId to factor ##
Yield_Covariates_WW_Mar[,c("comId","stateId")] <- lapply(Yield_Covariates_WW_Mar[,c("comId","stateId")], factor )
lapply(Yield_Covariates_WW_Mar, class)


###############################################
##### Save Yield_Covariates_WW_Marober extern ####
write.csv(Yield_Covariates_WW_Mar, file="./data/data_raw/Yield_Covariates_WW_Mar.csv")

#######################################################
#### BIC to choose the degrees of the polynomials  ####
#######################################################
## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

################################################
## Formulas for Model Variations to be tested ##

## with SMI
formula_Mar_WW_detrendlog_SMIPrecTavg <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Mar_WW_detrendlog_SMIPrecPet  <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Mar_WW_detrendlog_SMIPrec     <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Mar_WW_detrendlog_SMIPet      <- Winterwheat_logtrend ~ poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Mar_WW_detrendlog_SMITavg     <- Winterwheat_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Mar_WW_detrendlog_SMI         <- Winterwheat_logtrend ~  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

## no SMI
formula_Mar_WW_detrendlog_PrecTavg <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

formula_Mar_WW_detrendlog_PrecPet  <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + dummy(comId)

formula_Mar_WW_detrendlog_Prec     <- Winterwheat_logtrend ~ poly(Prec, degree[r, 1], raw = T) + dummy(comId)

formula_Mar_WW_detrendlog_Pet      <- Winterwheat_logtrend ~ poly(Pet, degree[r, 2], raw = T)  +  dummy(comId)

formula_Mar_WW_detrendlog_Tavg     <- Winterwheat_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + dummy(comId)


#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################

##################################################
## Loop through various variable configurations ##
BIC_SMIPrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecTavg  <- glm(formula = formula_Mar_WW_detrendlog_SMIPrecTavg, data = Yield_Covariates_WW_Mar) 
  BIC_SMIPrecTavg[r] <- BIC(glm.fit_SMIPrecTavg)
}

BIC_SMIPrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecPet  <- glm(formula = formula_Mar_WW_detrendlog_SMIPrecPet,  data = Yield_Covariates_WW_Mar)
  BIC_SMIPrecPet[r] <- BIC(glm.fit_SMIPrecPet)
}

BIC_SMIPrec <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPrec      <- glm(formula = formula_Mar_WW_detrendlog_SMIPrec,     data = Yield_Covariates_WW_Mar) 
  BIC_SMIPrec[r] <- BIC(glm.fit_SMIPrec)
}

BIC_SMIPet <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPet       <- glm(formula = formula_Mar_WW_detrendlog_SMIPet,      data = Yield_Covariates_WW_Mar) 
  BIC_SMIPet[r] <- BIC(glm.fit_SMIPet)
}

BIC_SMITavg <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMITavg      <- glm(formula = formula_Mar_WW_detrendlog_SMITavg,     data = Yield_Covariates_WW_Mar) 
  BIC_SMITavg[r] <- BIC(glm.fit_SMITavg)
}

BIC_SMI <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMI      <- glm(formula = formula_Mar_WW_detrendlog_SMI,     data = Yield_Covariates_WW_Mar) 
  BIC_SMI[r] <- BIC(glm.fit_SMI)
}

BIC_PrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecTavg  <- glm(formula = formula_Mar_WW_detrendlog_PrecTavg, data = Yield_Covariates_WW_Mar) 
  BIC_PrecTavg[r] <- BIC(glm.fit_PrecTavg)
}

BIC_PrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecPet  <- glm(formula = formula_Mar_WW_detrendlog_PrecPet, data = Yield_Covariates_WW_Mar) 
  BIC_PrecPet[r] <- BIC(glm.fit_PrecPet)
}

BIC_Prec <- rep(0,9)
for(r in 1:9){
  glm.fit_Prec  <- glm(formula = formula_Mar_WW_detrendlog_Prec, data = Yield_Covariates_WW_Mar) 
  BIC_Prec[r] <- BIC(glm.fit_Prec)
}

BIC_Pet  <- rep(0,9)
for(r in 1:9){
  glm.fit_Pet   <- glm(formula = formula_Mar_WW_detrendlog_Pet , data = Yield_Covariates_WW_Mar) 
  BIC_Pet [r] <- BIC(glm.fit_Pet )
}

BIC_Tavg  <- rep(0,9)
for(r in 1:9){
  glm.fit_Tavg   <- glm(formula = formula_Mar_WW_detrendlog_Tavg , data = Yield_Covariates_WW_Mar) 
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
month <-rep("March",99)

BIC_Mar <- cbind(BIC, model ,model_index, index, month)

#######################
## Delete Duplicates ##
which(duplicated(BIC_Mar$BIC))
list3 <- c(20,21,23,24,26,27,31,32,33,34,35,36,40,41,42,43,44,45,47,48,49,50,51,52,53,54,56,57,59,60,62,63,67,68,69,70,71,72,76,77,78,79,80,81)

length(list3)
temp <- BIC_Mar

for (i in 1:44)
{
  print(BIC_Mar[BIC_Mar$index ==list3[i],])
  temp <- (temp[!temp$index==list3[i],])
}
dim(BIC_Mar)
dim(temp)

################################
## Correct created data.frame ##
rownames(temp) <- NULL
BIC_Mar <- temp
lapply(BIC_Mar, class)

############################
## Plot data with ggplot2 ##
g <- ggplot(BIC_Mar,aes(y=BIC, x=index))
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark()
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark() +
  facet_wrap( ~ month)
BIC_Mar

## Export Data frame for use in BIC_Grafic
BIC_WW_Mar <- BIC_Mar
class(BIC_WW_Mar)
write.csv(BIC_WW_Mar, file="./data/data_raw/BIC/BIC_WW_Mar.csv")

################################################################
################################### Explore Models #############
################################################################
###################
## Load Data Set ##
# Yield_Covariates_WW_Mar <- read.csv( file="./data/data_raw/Yield_Covariates_WW_Mar.csv")
# names(Yield_Covariates_WW_Mar)
# Yield_Covariates_WW_Mar$X <- NULL

#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_WW_Mar <- plm.data(Yield_Covariates_WW_Mar, index=c("comId", "year"))

## Transform comId and stateId to factor ##
Yield_Covariates_WW_Mar[,c("comId","stateId")] <- lapply(Yield_Covariates_WW_Mar[,c("comId","stateId")], factor )
str(Yield_Covariates_WW_Mar)

#################################
###############################
## Results with smallest BIC ##
###############################
plot(BIC_SMIPrecTavg)
which.min(BIC_SMIPrecTavg)  
r = 9 # Da 8 weniger als 8 von 9 weg ist 
best_formula <- formula_Mar_WW_detrendlog_SMIPrecTavg


###################
## GLM Ergebniss ##
glm.fit_WW_BEST_Mar  <- glm(formula = best_formula,  data = Yield_Covariates_WW_Mar)
summary(glm.fit_WW_BEST_Mar)
'AIC: -7701.1'

####################
## PLM Ergebnisse ##
plm.fit_WW_BEST_Mar <- plm(formula = update(best_formula, .~. - dummy(comId)),  data = Yield_Covariates_WW_Mar,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_WW_BEST_Mar)
'Adj. R-Squared: 0.17301'

fixef <-  fixef(plm.fit_WW_BEST_Mar)
fixef <- as.data.frame(as.matrix(fixef))
head(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")
fixef
write.csv(fixef, "./figures/figures_exploratory/FixedEffects/Winterwheat/plm.fit_WW_BEST_Mar_FE.csv")

##################
## LM Ergebniss ##
lm.fit_WW_BEST_Mar  <-lm(formula = best_formula,  data = Yield_Covariates_WW_Mar)
summary(lm.fit_WW_BEST_Mar) 
'Adjusted R-squared:  0.7041'

################################################
## Assessing Influence (Leverage*discrepancy) ##
cutoff_Mar <- 4/((nrow(Yield_Covariates_WW_Mar)-length(lm.fit_WW_BEST_Mar$coefficients)-1)) 
cutoff_Mar
plot(lm.fit_WW_BEST_Mar, which=4, cook_Mar.levels=cutoff_Mar)
cook_Mar <- cooks.distance(lm.fit_WW_BEST_Mar)

nrow(Yield_Covariates_WW_Mar[cook_Mar > cutoff_Mar,]) # 177

year_cooks_Mar <- table(Yield_Covariates_WW_Mar$year[cook_Mar > cutoff_Mar ]) 
year_cooks_Mar
'1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 
 4r11    9    2    7   46   30    9   20   10   11   11   11 
'

com_cooks_Mar <- sort(table(Yield_Covariates_WW_Mar$com[cook_Mar > cutoff_Mar ] ) )
tail(com_cooks_Mar,20)
'
Stendal, Landkreis                        Waldshut, Landkreis                       Weimar, krsfr. Stadt            Berchtesgadener Land, Landkreis 
2                                          2                                          2                                          3 
Duisburg, Kreisfreie Stadt                     Elbe-Elster, Landkreis                       Havelland, Landkreis                    Landkreis Uecker-Randow 
3                                          3                                          3                                          3 
Magdeburg, Kreisfreie Stadt       M\xf6nchengladbach, Kreisfreie Stadt                     Nordsachsen, Landkreis                       Oberhavel, Landkreis 
3                                          3                                          3                                          3 
Olpe, Kreis              Potsdam-Mittelmark, Landkreis                 Rheinisch-Bergischer Kreis                         S\xf6mmerda, Kreis 
3                                          3                                          3                                          3 
Teltow-Fl\xe4ming, Landkreis                          Barnim, Landkreis                  Spree-Nei\xdfe, Landkreis           Oberspreewald-Lausitz, Landkreis 
3                                          4                                          4                                          5 '


########################
## Heteroskedasdicity ##
bptest(glm.fit_WW_BEST_Mar) # Breusch Pagan Test of Heteroskedastie in den Störgrößen: Null: Homoskedasdicity.
bptest(plm.fit_WW_BEST_Mar)
' In beiden Fällen kann die Null widerlegt werden. Es gibt also heteroskedasdicity '

## Koenkers Version on BP Test: robuste Modification wenn die Störgrößen nicht normalverteilt sind. 
bptest(plm.fit_WW_BEST_Mar, studentize = TRUE)
'Auch hier kann die Null widerlegt werden. Need to use robust covariance variance matrix to correct standard errors'

######################################
## Tests for serial autocorrelation ##
pwartest(plm.fit_WW_BEST_Mar)
pbgtest(plm.fit_WW_BEST_Mar) 
'
both, H_1 of serial autocorrelation cannot be rejected
'

#################################
## Correct the Standard Errors ##
#################################

## Correct Standard Errors used in table ##
coeftest(plm.fit_WW_BEST_Mar)

## Robust covariance matrix estimators a la White ##
# coeftest(plm.fit_WW_BEST_Mar,vcov=vcovHC(plm.fit_WW_BEST_Mar,method = "arellano", type = "HC0")) 
cov0_WW_BEST_Mar <- vcovHC(plm.fit_WW_BEST_Mar,method = "arellano", type = "HC0", cluster="group")
Wh.se_serial_WW_BEST_Mar <- sqrt(diag(cov0_WW_BEST_Mar))

cov0.1_WW_BEST_Mar <- vcovHC(plm.fit_WW_BEST_Mar,method = "arellano", type = "HC0", cluster="time")
Wh.se_cross_WW_BEST_Mar <- sqrt(diag(cov0.1_WW_BEST_Mar))
# 
# ## Beck Katz:
# # coeftest(plm.fit_WW_BEST_Mar, vcov = function(x) vcovBK(plm.fit_WW_BEST_Mar,method = "arellano", type = "HC0"))
# cov1 <- vcovBK(plm.fit_WW_BEST_Mar,method = "arellano", type = "HC0",  cluster="time")
# BK.se <- sqrt(diag(cov1))

# ## Driscoll Kraay ##
# summary(plm.fit_WW_BEST_Mar)
coeftest(plm.fit_WW_BEST_Mar,  vcov=function(x) vcovSCC(plm.fit_WW_BEST_Mar,method = "arellano",type = "HC0")) 
cov2_WW_BEST_Mar     <- vcovSCC(plm.fit_WW_BEST_Mar,method = "arellano",type = "HC0")
DK.se_WW_BEST_Mar    <- sqrt(diag(cov2_WW_BEST_Mar))
# 
# cov2.1_WW_BEST_Mar   <- vcovSCC(plm.fit_WW_BEST_Mar,method = "arellano",type = "HC0", maxlag=1)
# DK2.1.se_WW_BEST_Mar <- sqrt(diag(cov2.1_WW_BEST_Mar))

# cov2.2_WW_BEST_Mar   <- vcovSCC(plm.fit_WW_BEST_Mar,method = "arellano",type = "HC0", maxlag=2)
# DK2.2.se_WW_BEST_Mar <- sqrt(diag(cov2.2_WW_BEST_Mar))
# 
# cov2.3_WW_BEST_Mar   <- vcovSCC(plm.fit_WW_BEST_Mar,method = "arellano",type = "HC0", maxlag=3)
# DK2.3.se_WW_BEST_Mar <- sqrt(diag(cov2.3_WW_BEST_Mar))
# 
# cov2.4_WW_BEST_Mar   <- vcovSCC(plm.fit_WW_BEST_Mar,method = "arellano",type = "HC0", maxlag=4)
# DK2.4.se_WW_BEST_Mar <- sqrt(diag(cov2.4_WW_BEST_Mar))
# 
cov2.5_WW_BEST_Mar   <- vcovSCC(plm.fit_WW_BEST_Mar,method = "arellano",type = "HC0", maxlag=5)
DK2.5.se_WW_BEST_Mar <- sqrt(diag(cov2.5_WW_BEST_Mar))

## Cameron et al /Thompson : doouble-clustering estimator  ##
# coeftest(plm.fit_WW_BEST_Mar, vcovDC(plm.fit_WW_BEST_Mar, method = "arellano", type = "HC0"))
cov3_WW_BEST_Mar <- vcovDC(plm.fit_WW_BEST_Mar, method = "arellano", type = "HC0")
CT.se_WW_BEST_Mar <- sqrt(diag(cov3_WW_BEST_Mar))
'Our estimator is qualitatively similar to the ones presented in White and Domowitz (1984), for
time series data, and Conley (1999), for spatial data. '

## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_WW_BEST_Mar, Wh.se_serial_WW_BEST_Mar,  DK.se_WW_BEST_Mar, DK2.5.se_WW_BEST_Mar, CT.se_WW_BEST_Mar)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar,plm.fit_WW_BEST_Mar,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - March",
          dep.var.labels = "log(Winterwheat)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,          
          type="text", out="./figures/figures_exploratory/BIC/Winterwheat/WW_Mar_best.txt"
)


#########################################################
## Results with smallest BIC of Standard Configuration ##
#########################################################
' Standard Configuration has lowest BIC'
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_WW_BEST_Mar, Wh.se_serial_WW_BEST_Mar,  DK.se_WW_BEST_Mar, DK2.5.se_WW_BEST_Mar, CT.se_WW_BEST_Mar)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar,plm.fit_WW_BEST_Mar,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - March",
          dep.var.labels = "log(Winterwheat)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,                
          type="text", out="./figures/figures_exploratory/BIC/Winterwheat/WW_Mar_bestStandard.txt"
)

########################################
## Results with smallest BIC with SMI ##
########################################

################################
## Generate Table with Output ##
se <- list(NULL, Wh.se_cross_WW_BEST_Mar, Wh.se_serial_WW_BEST_Mar,  DK.se_WW_BEST_Mar, DK2.5.se_WW_BEST_Mar, CT.se_WW_BEST_Mar)
labels1 <-c("NULL","WhiteCross","WhiteSerial", "DriscollKraay", "DriscollKray2.5","CameronThompson")
stargazer(plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar, plm.fit_WW_BEST_Mar,plm.fit_WW_BEST_Mar,
          se = se,   
          dep.var.caption  = "Model with smallest BIC - March",
          dep.var.labels = "log(Winterwheat)",
          style="default",
          model.numbers = FALSE,
          column.labels = labels1,           
          type="text", out="./figures/figures_exploratory/BIC/Winterwheat/WW_Mar_bestSM.txt"
)

