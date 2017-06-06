###############################
####  SiloMaize in October ####
###############################

'
######################
## File Discription ##

The purpose of this script is to estimate the impact of weather fluctuations in the month mentionend above on yearly crop yield.

This is done by the following the steps:
- Create data frame with siloMaize as dependent and variables of the month above as independent variables



'
###################
## Load Packages ##
library("plm")
library("boot")
library("gtools")
library("lme4")
library(lmtest)
library(car)

#################################################################################################################
#### Create data frame with siloMaize as dependent and variables of the month above as independent variables ####

## Read in large Dataframe for Maize ##
Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv")

## Chech out content of the data.frame ##
names(Yield_Covariates)

## Delete X column ##
Yield_Covariates$X <- NULL
head(Yield_Covariates)

## For publication worth regression output need to change data names ##
'Get rid of variables which are not necessary: other months and other not needed variables'

names(Yield_Covariates)
names <- names(Yield_Covariates)
names_Oct <- grep(c("*_Oct"), names)
names_Oct
Yield_Covariates_Oct <- Yield_Covariates[,names_Oct]
names(Yield_Covariates_Oct)
dim(Yield_Covariates_Oct)

## Delete all but SMI, Prec, Tavg and Pet
names(Yield_Covariates_Oct)
Yield_Covariates_Oct <- Yield_Covariates_Oct[,c(1:4)]

## Establish first part of data frame_ time and spatial reference plus Silomaize ##
names(Yield_Covariates[,c(2,1,3:5,7)])
Yield_Covariates_SM <- Yield_Covariates[,c(2,1,3:5,7)] # Achtung, darauf achten, dass comId und year in der richtigen Reihenfolge sind.
names(Yield_Covariates_SM)
head(Yield_Covariates_SM)

Yield_Covariates_SM_Oct <- cbind(Yield_Covariates_SM, Yield_Covariates_Oct)
names(Yield_Covariates_SM_Oct)
names(Yield_Covariates_SM_Oct) <- c( "comId" , "year","com","stateId","state","siloMaize","SMI", "Prec","Tavg", "Pet")
names(Yield_Covariates_SM_Oct)




############################################
#### Prepare data for stepwise function ####
' Drought Monitor Spezification '
Yield_Covariates_SM_Oct$SMI_GDM <- cut(Yield_Covariates_SM_Oct$SMI, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                       labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
#############
## Na-omit ##
sum(is.na(Yield_Covariates_SM_Oct) )
dim(Yield_Covariates_SM_Oct)
Yield_Covariates_SM_Oct_nna <- na.omit(Yield_Covariates_SM_Oct) 
dim(Yield_Covariates_SM_Oct_nna)
any(is.na(Yield_Covariates_SM_Oct_nna))
rownames(Yield_Covariates_SM_Oct_nna) <- NULL
head(Yield_Covariates_SM_Oct_nna)

## Further work with DataFrame without Yield_Covariates_SM_Oct index ##
Yield_Covariates_SM_Oct <- Yield_Covariates_SM_Oct_nna

##########################################################################
## Remove comIds with less than 7 observations due avoid leveage issues ## 
##########################################################################
# ## Leverage one ##
# # Warnings in plot()
# Yield_Covariates_SM_Oct[c(181, 182, 765, 851, 1006, 1557) ,]
# 
# # Crucial Observations in plot()
# Yield_Covariates_SM_Oct[c(3335,3673,3219, 3681, 1557),]

## Anschauem der Struktur der coms, welche hier ausgegeben wurden: hier sind vor allem die SiloMaize Werte relevant ##
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "3101",] # < 1 Beobachtung
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "3102",] # # < 1 Beobachtung
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "5314",] # < 1 Beobachtung 
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "5512",] # < 1 Beobachtung
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "5911",] # < 1 Beobachtung
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "7331",] # < 1 Beobachtung

Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "15001",] # < 4 Beobachtungen
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "12069",] # hier mache ich am besten nichts
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "14730",] # hier mache ich auch am besten nichts
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "12053",] # hier mache ich auch am besten nichts

' Es macht wohl Sinn alle Werte mit weniger als 6 Beobachtungen rauszunehmen um leverage zu vermeiden.'

#####################################################
## Delete all comIds with less than 7 observations ##
sum(table(Yield_Covariates_SM_Oct$comId) < 7 )
table(Yield_Covariates_SM_Oct$comId) < 7 

## comIds mit weniger als 7 Beoachtungen: ##
list <- c(3101, 3102, 3158, 3402, 5114, 5117, 5314,5315, 5334,5378, 5512, 5911, 5916, 6413, 7131, 7135, 7233, 7331, 7332, 7337, 7338, 7339, 8111,12052, 14612, 15001, 15082, 15083, 15084, 15085, 15086, 15087, 15088, 15089, 15091, 16051, 16052 )
length(list)
list[[1]]

temp <- Yield_Covariates_SM_Oct
for (i in 1:34)
{
  print(Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}

dim(temp)-dim(Yield_Covariates_SM_Oct)

Yield_Covariates_SM_Oct <- temp

################################
## Befehle nach jedem löschen ##
Yield_Covariates_SM_Oct <- na.omit(Yield_Covariates_SM_Oct) 
rownames(Yield_Covariates_SM_Oct) <- NULL
Yield_Covariates_SM_Oct <- plm.data(Yield_Covariates_SM_Oct, index=c("comId", "year"))
Yield_Covariates_SM_Oct[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Oct[,c("comId","stateId")], factor )

attach(Yield_Covariates_SM_Oct)

#################################################
#### Remove log trend of indepedent variable ####
##################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Oct)

##########################
## Issue with Outliers ###
##########################
par(mfrow = c(1,1))
plot(logtrend, which=c(1:6)) 

## Look Outliers Values ##
Yield_Covariates_SM_Oct[c(1276, 3262, 3283, 3171,3255),]
## Look at other values of outliers com #
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "6532",] # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "12067",] # 2006 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "12069",] # 2003 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "12067",] # 2006 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 

Yield_Covariates_SM_Oct[Yield_Covariates_SM_Oct$comId == "12060",] # verändere ich nicht 


## Delete outliers ##
Yield_Covariates_SM_Oct <- Yield_Covariates_SM_Oct[!(Yield_Covariates_SM_Oct$comId == "6532" & Yield_Covariates_SM_Oct$year == "2008"),]
Yield_Covariates_SM_Oct <- Yield_Covariates_SM_Oct[!(Yield_Covariates_SM_Oct$comId == "12067" & Yield_Covariates_SM_Oct$year == "2006"),]
Yield_Covariates_SM_Oct <- Yield_Covariates_SM_Oct[!(Yield_Covariates_SM_Oct$comId == "12069" & Yield_Covariates_SM_Oct$year == "2003"),]
Yield_Covariates_SM_Oct <- Yield_Covariates_SM_Oct[!(Yield_Covariates_SM_Oct$comId == "12067" & Yield_Covariates_SM_Oct$year == "2006"),]

Yield_Covariates_SM_Oct <- na.omit(Yield_Covariates_SM_Oct)
rownames(Yield_Covariates_SM_Oct) <- NULL

#################################################
#### Remove log trend of indepedent variable ####
##################################################
'Fit log of yield on log of time and use the residuals of that for yields'
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Oct)
Yield_Covariates_SM_Oct$siloMaize_logtrend <- resid(logtrend)


#######################################
## Prepare dataframe for plm package ##
'Change Indexing so that it can be used in plm package'
Yield_Covariates_SM_Oct <- plm.data(Yield_Covariates_SM_Oct, index=c("comId", "year"))
str(Yield_Covariates_SM_Oct)

###########################################
## Transform comId and stateId to factor ##
Yield_Covariates_SM_Oct[,c("comId","stateId")] <- lapply(Yield_Covariates_SM_Oct[,c("comId","stateId")], factor )
lapply(Yield_Covariates_SM_Oct, class)

#####################
## Attach Data Set ##
head(Yield_Covariates_SM_Oct)
dim(Yield_Covariates_SM_Oct)
attach(Yield_Covariates_SM_Oct)


#########################################################################
#### BIC to choose the degrees of the polynomials  ####
##########################################################################

## create a matrix which contains all possible degree combinations, here for three variables ##
degree <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
degree

formula_Oct_sm_detrendlog_SMIPrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_SMIPrecPet <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_SMIPrec <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_SMIPet <- siloMaize_logtrend ~ poly(Pet, degree[r, 1], raw = T) +  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)


formula_Oct_sm_detrendlog_SMITavg <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

formula_Oct_sm_detrendlog_SMI <- siloMaize_logtrend ~  
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + dummy(comId)

## no SMI

formula_Oct_sm_detrendlog_PrecTavg <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Tavg, degree[r, 2], raw = T) + dummy(comId)

formula_Oct_sm_detrendlog_PrecPet <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) +  poly(Pet, degree[r, 2], raw = T) + dummy(comId)

formula_Oct_sm_detrendlog_Prec <- siloMaize_logtrend ~ poly(Prec, degree[r, 1], raw = T) + dummy(comId)

formula_Oct_sm_detrendlog_Pet <- siloMaize_logtrend ~ poly(Pet, degree[r, 1], raw = T)  + dummy(comId)

formula_Oct_sm_detrendlog_Tavg <- siloMaize_logtrend ~ poly(Tavg, degree[r, 2], raw = T)  + dummy(comId)

## Print formula
formula_Oct_sm_detrendlog_SMIPrecTavg
formula_Oct_sm_detrendlog_SMIPrecPet
formula_Oct_sm_detrendlog_SMIPrec
formula_Oct_sm_detrendlog_SMIPet
formula_Oct_sm_detrendlog_SMITavg
formula_Oct_sm_detrendlog_SMI
formula_Oct_sm_detrendlog_PrecTavg
formula_Oct_sm_detrendlog_PrecPet
formula_Oct_sm_detrendlog_Prec
formula_Oct_sm_detrendlog_Pet 
formula_Oct_sm_detrendlog_Tavg

#################################################################################################
# Loop through the container list to cover all permutations of posssible degree of freedoms of ##
# of the polynomials of the variables                                                          ##
#################################################################################################

##################################################
## Loop through various variable configurations ##
print("start loop")
r
BIC_SMIPrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecTavg  <- glm(formula = formula_Oct_sm_detrendlog_SMIPrecTavg, data = Yield_Covariates_SM_Oct) 
  BIC_SMIPrecTavg[r] <- BIC(glm.fit_SMIPrecTavg)
}

BIC_SMIPrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_SMIPrecPet  <- glm(formula = formula_Oct_sm_detrendlog_SMIPrecPet,  data = Yield_Covariates_SM_Oct)
  BIC_SMIPrecPet[r] <- BIC(glm.fit_SMIPrecPet)
}

BIC_SMIPrec <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPrec      <- glm(formula = formula_Oct_sm_detrendlog_SMIPrec,     data = Yield_Covariates_SM_Oct) 
  BIC_SMIPrec[r] <- BIC(glm.fit_SMIPrec)
}

BIC_SMIPet <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMIPet       <- glm(formula = formula_Oct_sm_detrendlog_SMIPet,      data = Yield_Covariates_SM_Oct) 
  BIC_SMIPet[r] <- BIC(glm.fit_SMIPet)
}

BIC_SMITavg <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMITavg      <- glm(formula = formula_Oct_sm_detrendlog_SMITavg,     data = Yield_Covariates_SM_Oct) 
  BIC_SMITavg[r] <- BIC(glm.fit_SMITavg)
}

BIC_SMI <- rep(0,9)
for(r in 1:9){  
  glm.fit_SMI      <- glm(formula = formula_Oct_sm_detrendlog_SMI,     data = Yield_Covariates_SM_Oct) 
  BIC_SMI[r] <- BIC(glm.fit_SMI)
}

BIC_PrecTavg <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecTavg  <- glm(formula = formula_Oct_sm_detrendlog_PrecTavg, data = Yield_Covariates_SM_Oct) 
  BIC_PrecTavg[r] <- BIC(glm.fit_PrecTavg)
}


BIC_PrecPet <- rep(0,9)
for(r in 1:9){
  glm.fit_PrecPet  <- glm(formula = formula_Oct_sm_detrendlog_PrecPet, data = Yield_Covariates_SM_Oct) 
  BIC_PrecPet[r] <- BIC(glm.fit_PrecPet)
}


BIC_Prec <- rep(0,9)
for(r in 1:9){
  glm.fit_Prec  <- glm(formula = formula_Oct_sm_detrendlog_Prec, data = Yield_Covariates_SM_Oct) 
  BIC_Prec[r] <- BIC(glm.fit_Prec)
}

BIC_Pet  <- rep(0,9)
for(r in 1:9){
  glm.fit_Pet   <- glm(formula = formula_Oct_sm_detrendlog_Pet , data = Yield_Covariates_SM_Oct) 
  BIC_Pet [r] <- BIC(glm.fit_Pet )
}

BIC_Tavg  <- rep(0,9)
for(r in 1:9){
  glm.fit_Tavg   <- glm(formula = formula_Oct_sm_detrendlog_Tavg , data = Yield_Covariates_SM_Oct) 
  BIC_Tavg [r] <- BIC(glm.fit_Tavg )
}



#   cv.error[r] <- cv.glm( Yield_Covariates_SM_Oct, glm.fit )$delta[1] # hat funktioniert (dummies dürfen nicht nur als Faktor definiert werden)
#   save.image(file = "/home/peichl/projects/crossvalid/siloMaize/cross5.RData")


###############################################
## Load data from correlation project folder ##
# load("/home/peichl/Documents/projects/correlation/data/data_processed/Crossvalidation/siloMaize/cross5.RData")
# cv.error_loocv_Oct_SiloMaize <- cv.error

## Plot Results ##
# par(mfrow=c(2,2))

## Compare with crossvalidation results of 
# plot(cv.error_loocv_2v8poly_Prec)  # aus ursprünlichen, crossvalidated run
# plot(BIC_loop_2v8poly_Prec)  # aus ursprünlichen, crossvalidated run

#####################################################
## Compare plots of various formula specifications ##
#####################################################
# degree
# par(mfrow=c(4,3))
# plot(BIC_SMIPrecTavg) # 
# plot(BIC_SMIPrecPet) #
# plot(BIC_SMIPrec) # 
# plot(BIC_SMIPet) # kaum Zugewinn durch Komplexität
# plot(BIC_SMITavg)
# plot(BIC_SMI)
# plot(BIC_PrecTavg)
# plot(BIC_PrecPet) #
# plot(BIC_Prec) # 
# plot(BIC_Pet) # kaum Zugewinn durch Komplexität
# plot(BIC_Tavg)

BIC <- c(BIC_SMIPrecTavg,BIC_SMIPrecPet,BIC_SMIPrec, BIC_SMIPet, BIC_SMITavg, BIC_SMI, BIC_PrecTavg, BIC_PrecPet, BIC_Prec, BIC_Pet, BIC_Tavg)
BIC
par(mfrow=c(1,1))
plot(BIC)
length(BIC)

###########################
## Plot BIC with ggplot2 ##
###########################
library(ggplot2)

##############################################
## Create Dataframe for plotting in ggplot2 ##

## repeat name of modelconfiguration ##
list <-c("SMIPrecTavg", "SMIPrecPet", "SMIPrec", "SMIPet",
         "SMITavg", "SMI", "PrecTavg", "PrecPet", "Prec", "Pet", "Tavg")
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

model <- as.data.frame(model)
model_index <- as.data.frame(model_index)

####################################
## Combine data in one data.frame ##
class(BIC)
BIC <- as.data.frame(BIC)
BIC2 <-cbind(BIC, model)
BIC2 <- cbind(BIC2,model_index)
index <- 1:99
BIC2 <- cbind(BIC2, index)
month <-rep("October",99)
BIC2 <- cbind(BIC2, month)
BIC2

#######################
## Delete Duplicates ##
which(duplicated(BIC2$BIC))
list3 <- c(20,21,23,24,26,27,31,32,33,34,35,36,40,41,42,43,44,45,47,48,49,50,51,52,53,54,74,75,77,78,80,81,85,86,87,88,89,90,94,95,96,97,98,99)

length(list3)
temp <- BIC2
for (i in 1:44)
{
  print(BIC2[BIC2$index ==list3[i],])
  temp <- (temp[!temp$index==list3[i],])
}
dim(BIC2)
dim(temp)

################################
## Correct created data.frame ##
rownames(temp) <- NULL
BIC2 <- temp
lapply(BIC2, class)

############################
## Plot data with ggplot2 ##
g <- ggplot(BIC2,aes(y=BIC, x=index))
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark()
g + geom_point(aes(color=model)) + labs(title="BIC of various model configurations", x="") + theme(plot.title=element_text(size=15, face="bold")) + theme_dark() +
  facet_wrap( ~ month)
BIC2


BIC_SM_10 <- BIC2
class(BIC_SM_10)
write.csv(BIC_SM_10, file="./data/data_raw/BIC/BIC_SM_10.csv")
