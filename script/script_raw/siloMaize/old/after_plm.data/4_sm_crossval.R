##########################
#### Cross Validation ####
##########################

library("plm")
library("boot")
library("gtools")
library("lme4")

## Read data frame with siloMaize as only depedent variable ##
Yield_Covariates <- read.csv( "/home/peichl/projects/crossvalid/siloMaize/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv")

# Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_ww.csv")

Yield_Covariates$X <- NULL

## Na-omit ##
sum(is.na(Yield_Covariates$siloMaize) )
Yield_Covariates_nna <- na.omit(Yield_Covariates) 

#################################
#### Define comId as factors ####
class(Yield_Covariates_nna$comId)
Yield_Covariates_nna$comId <- as.factor(Yield_Covariates_nna$comId )
head(Yield_Covariates_nna$comId)

attach(Yield_Covariates_nna)

## Drought Monitor Spezification ##
Yield_Covariates_nna$SMI_Apr_GDM <- cut(Yield_Covariates_nna$SMI_Apr, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                        labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))


## Change Indexing so that it can be used in plm package
Yield_Covariates_nna <- plm.data(Yield_Covariates_nna, index=c("comId", "year"))
str(Yield_Covariates_nna)

## Attach Data Set ##
attach(Yield_Covariates_nna)
any(is.na(Yield_Covariates_nna))


#######################################################################################
#### Cross Validation to choose the degrees of the polynomials and natural splines ####
#######################################################################################

## create a matrix which contains all possible degree combinations, here for three variables ##
container_2v3poly <- permutations(n=3,r=2,v=c(1:3),repeats.allowed=T)
container_2v3poly

## Define formula to be considered
formula_PrecTavgSmiGDM_2v3poly <- log(siloMaize) ~ poly(Prec_Apr, container_2v3poly[r, 1], raw = T) +  poly(Tavg_Apr, container_2v3poly[r, 2], raw = T) + 
  dummy(SMI_Apr_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year)) + 
  dummy(comId)

## Print formula
formula_PrecTavgSmiGDM_2v3poly

##################################################################################################
## Loop through the container list to cover all permutations of posssible degree of freedoms of ##
## of the polynomials of the variables                                                          ##
##################################################################################################


#####################################################################################################
#### Loop over two variables - Tavg and Prec - to dertermine their polygons starting with 8 grades ##

##  Define empty lists for loop ##
cv.error_loocv_2v8poly_Prec <- rep(0,9)
cv.error_loocv_2v8poly_Prec
BIC_loop_2v8poly_Prec <- rep(0,9)
BIC_loop_2v8poly_Prec


print("start loop")

for(r in 1:9){
  glm.fit <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = Yield_Covariates_nna) 
  
  BIC_loop_2v8poly_Prec[r] <- BIC(glm.fit)
  cv.error_loocv_2v8poly_Prec[r] <- cv.glm(Yield_Covariates_nna, glm.fit )$delta[1] # hat funktioniert (dummies dürfen nicht nur als Faktor definiert werden)
  save.image(file = "/home/peichl/projects/crossvalid/siloMaize/cross4.RData")
}

## Load data from correlation project folder
load("/home/peichl/Documents/projects/correlation/data/data_processed/Crossvalidation/siloMaize/cross4.RData")

BIC_loop_Apr_SiloMaize <- BIC_loop_2v8poly_Prec
cv.error_loocv_Apr_SiloMaize <- cv.error_loocv_2v8poly_Prec

par(mfrow=c(2,2))
plot(BIC_loop_2v8poly_Prec)
plot(cv.error_loocv_2v8poly_Prec)
plot(BIC_loop_Apr_SiloMaize)
plot(cv.error_loocv_Apr_SiloMaize, main="April", ylab="test MSE")
which.min(BIC_loop_Apr_SiloMaize)
which.min(cv.error_loocv_Apr_SiloMaize) 


# ich denke, dass hier Model Nummer 9 am besten ist
r_Apr = 9

formula_PrecTavgSmiGDM_2v3poly

####################
## GLM Ergebnisse ##
## plm.data frame
glm.fit2 <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = Yield_Covariates_nna)

summary(glm.fit2)
coefficients(glm.fit2)[1:15]

## plot ##
plot(glm.fit2)

####################
## PLM Ergebnisse ##
plm.fit_Apr <- plm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - dummy(comId)),  data = Yield_Covariates_nna,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Apr)
plot(fixef(plm.fit_Apr))

## For publication worth regression output need to change data names ##
# Get rid of variables which are not necessary
names(Yield_Covariates_nna)
r <- names(Yield_Covariates_nna)
Apr <- grep(c("*Apr"), r)
Apr

Yield_Covariates_Apr <- Yield_Covariates_nna[,Apr]
names(Yield_Covariates_Apr)
dim(Yield_Covariates_Apr)
Yield_Covariates_SM <- Yield_Covariates_nna[,1:7]
names(Yield_Covariates_SM)
Yield_Covariates_SM_Apr <- cbind(Yield_Covariates_SM, Yield_Covariates_Apr)
names(Yield_Covariates_SM_Apr) <- c( "comId" , "year","com","comIdState","comState","SHAPE_AREA","siloMaize","SMI", "Prec","Tavg", "Pet" , "Tmin"   
                                    ,"Tmax", "SMI_GDM")
names(Yield_Covariates_SM_Apr)

## Determine formula
r <- r_Apr
container_2v3poly[r_Apr, 1] #3
container_2v3poly[r_Apr, 2] #3

formula_Apr_sm <- log(siloMaize) ~ poly(Prec, container_2v3poly[r, 1], raw = T) +  poly(Tavg, container_2v3poly[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year))


####################
## PLM Ergebnisse ##
head(Yield_Covariates_SM_Apr)
plm.fit_Apr <- plm(formula= formula_Apr_sm,  data = Yield_Covariates_SM_Apr, effect="individual", model=("within"))

summary(plm.fit_Apr)

#########################################
## Test for cross sectional dependence ##
pcdtest(plm.fit_Apr)
' Genause wie im SM Mai funktioniert der Test hier auch nicht'

save("plm.fit_Apr", file="/home/peichl/Documents/projects/correlation/script/script_raw/Crossvalidation/Output_Apr.RData")
plm.fit_Apr <- load(file="/home/peichl/Documents/projects/correlation/script/script_raw/Crossvalidation/Output_Apr.RData")
summary(plm.fit_Apr)
