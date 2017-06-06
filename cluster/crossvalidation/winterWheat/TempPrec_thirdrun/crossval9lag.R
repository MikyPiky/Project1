#################################################################################
#### Cross Validation to determine the model of WinterWheat for September:lag ####
#################################################################################

## Packages ##
  library("plm")
  library("boot")
  library("gtools")
  library("lme4")

## Rationale ##


## Read data frame with winterWheat as only depedent variable ##
Yield_Covariates <- read.csv( "/home/peichl/projects/crossvalid/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_ww.csv")

# Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_ww.csv")

Yield_Covariates$X <- NULL
names(Yield_Covariates)

## Na-omit ##
Yield_Covariates_nna <- na.omit(Yield_Covariates) 
sum(is.na(Yield_Covariates$winterWheat) )

#################################
#### Define comId as factors ####
class(Yield_Covariates_nna$comId)
Yield_Covariates_nna$comId <- as.factor(Yield_Covariates_nna$comId )
head(Yield_Covariates_nna$comId)
dummy(Yield_Covariates_nna$comId)

## Drought Monitor Spezification ##
Yield_Covariates_nna$SMI_Sep_lag_GDM <- cut(Yield_Covariates_nna$SMI_Sep_lag, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
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
formula_PrecTavgSmiGDM_2v3poly<- log(winterWheat) ~ poly(Prec_Sep_lag, container_2v3poly[r, 1], raw = T) +  poly(Tavg_Sep_lag, container_2v3poly[r, 2], raw = T) + 
  dummy(SMI_Sep_lag_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year)) + 
  dummy(comId)


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
  cv.error_loocv_2v8poly_Prec[r] <- cv.glm(Yield_Covariates_nna, glm.fit )$delta[1] # hat funktioniert (dummies dürfen nicht nur als Faktor definiert werden, 
                                                                                    # zumindest bei normalen data.frames und nicht plm.data)
  save.image(file = "/home/peichl/projects/crossvalid/cross9lag.RData")
}

which.min(BIC_loop_2v8poly_Prec)
which.min(cv.error_loocv_2v8poly_Prec) 

BIC_loop_2v8poly_Prec
cv.error_loocv_2v8poly_Prec

print("success")
quit("yes")