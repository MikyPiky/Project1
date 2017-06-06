###########################################################################
#### Cross Validation to determine the model of WinterWheat for August ####
###########################################################################

library("plm")
library("boot")
library("gtools")
library("lme4")

## Read data frame with winterWheat as only depedent variable ##
Yield_Covariates <- read.csv( "/home/peichl/projects/crossvalid/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_ww.csv")

# Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_ww.csv")
Yield_Covariates$X <- NULL

## Na-omit ##
sum(is.na(Yield_Covariates$winterWheat) )
Yield_Covariates_nna <- na.omit(Yield_Covariates) 

#################################
#### Define comId as factors ####
class(Yield_Covariates_nna$comId)
Yield_Covariates_nna$comId <- as.factor(Yield_Covariates_nna$comId )
head(Yield_Covariates_nna$comId)

attach(Yield_Covariates_nna)

## Drought Monitor Spezification ##
Yield_Covariates_nna$SMI_Aug_GDM <- cut(Yield_Covariates_nna$SMI_Aug, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                        labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))



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
formula_PrecTavgSmiGDM_2v3poly <- log(winterWheat) ~ poly(Prec_Aug, container_2v3poly[r, 1], raw = T) +  poly(Tavg_Aug, container_2v3poly[r, 2], raw = T) + 
  dummy(SMI_Aug_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year)) + 
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
  cv.error_loocv_2v8poly_Prec[r] <- cv.glm(Yield_Covariates_nna, glm.fit )$delta[1] # hat funktioniert (dummies dürfen nicht nur als Faktor definiert werden)
  save.image(file = "/home/peichl/projects/crossvalid/cross8.RData")
}



## Load data from correlation project folder
load("/home/peichl/Documents/projects/correlation/data/data_processed/Crossvalidation/winterWheat/cross8.RData")
names(Yield_Covariates_nna)
Yield_Covariates_nna_plm <- pdata.frame(Yield_Covariates_nna, index = c("comId","year"))
names(Yield_Covariates_nna_plm)

BIC_loop_Aug_ww <- BIC_loop_2v8poly_Prec
cv.error_loocv_Aug_ww <- cv.error_loocv_2v8poly_Prec

par(mfrow=c(1,2))
plot(BIC_loop_2v8poly_Prec)
plot(cv.error_loocv_2v8poly_Prec)
plot(BIC_loop_Aug_ww)
plot(cv.error_loocv_Aug_ww)

which.min(BIC_loop_Aug_ww )
which.min(cv.error_loocv_Aug_ww) 


# ich denke, dass hier Model Nummer 3 am besten ist
r_Aug_ww = which.min(BIC_loop_Aug_ww )
r = r_Aug_ww 
##########################
## Create plm dataframe ##
Yield_Covariates_nna_plm <- pdata.frame(Yield_Covariates_nna, index = c("comId","year"))

####################
## GLM Ergebnisse ##
## plm.data frame
glm.fit1 <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = Yield_Covariates_nna_plm)

summary(glm.fit1)
coefficients(glm.fit1)[1:15]

## normal data frame
glm.fit2 <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = Yield_Covariates_nna)

summary(glm.fit2)
coefficients(glm.fit2)[1:15]

####################
## PLM Ergebnisse ##

head(Yield_Covariates_nna_plm)[,1:10]
plm.fit <- plm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - dummy(comId)),  data = Yield_Covariates_nna_plm)
summary(plm.fit)
coefficients(plm.fit)
coefficients(glm.fit1)[1 :15]

fixef(plm.fit)

names(Yield_Covariates_nna_plm)

# Interpretation der Ergebnisse
class(Yield_Covariates_nna_plm)
str(Yield_Covariates_nna_plm)
str(Yield_Covariates_nna)
'Wenn man glm mit plm.data und data vergleicht, dann sind die Ergebnisse unterschiedlich.
Die Daten Struktur ist anders in plm.data, da noch indexes zu den Werten zugeordnet werden.
Die glm Ergenisse stimmen für den plm.data Ansatz mit denen von plm() überein.
Für den glm Ansatz mit normalen Daten unterscheiden sich die Werte leicht.
Daher vergleiche ich die BIC für den plm.data und data, da dies sowieso die relevanter 
Entscheidungsgröße zu sein scheint. 
'
##################################
#### Calculate BIC for plm.data ##
##  Define empty lists for loop ##
cv.error_loocv_plm <- rep(0,9)
cv.error_loocv_plm
BIC_loop_plm <- rep(0,9)
BIC_loop_plm


for(r in 1:9){
  glm.fit <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = Yield_Covariates_nna_plm) 
  
  BIC_loop_plm[r] <- BIC(glm.fit)
}
BIC_loop_plm_Aug <- BIC_loop_plm

plot(BIC_loop_plm, col="red" )
plot(BIC_loop_2v8poly_Prec)

# Interpretation
'Die Struktur ändert sich nicht wirklich, das heißt, ich lasse die Läufe nicht nochmal durchlaufen.
 Für die Zukunft muss ich mir überlegen, ob die Daten nicht erst mit der within funktion transformiert
 und die transformierten Daten dann einfach in glm und vor allem cv.glm genutzt werden könenn. 
 Ähnlich überlegungen gelten übrigens auch für die räumlichen Panel Modelle. Dort werden die Modelle 
 sowieso über ML oder GMM geschätz, wodurch auch Informationskriterien Sinnvoll sein könnten.'

## Überprüfen, ob glm mit within transformierten Daten funktioniert ##
names(Yield_Covariates_nna_plm)
drop <- c("comIdState" ,  "comState"  ,   "SHAPE_AREA" )
Yield_Covariates_nna_plm <- Yield_Covariates_nna_plm[,!(names(Yield_Covariates_nna_plm) %in% drop)]

Yield_Covariates_nna_plm$newSMI_Jan <- NULL
Yield_Covariates_nna_plm$SMI_Aug_within <- Within(Yield_Covariates_nna_plm$SMI_Aug)
Yield_Covariates_nna_plm$Prec_Aug_within <- Within(Yield_Covariates_nna_plm$Prec_Aug)
Yield_Covariates_nna_plm$Prec_Aug_within <- Within(Yield_Covariates_nna_plm$Tavg_Aug)
Yield_Covariates_nna_plm$SMI_Aug_GDM_within <- Within(Yield_Covariates_nna_plm$SMI_Aug_GDM)
sumJuny(Yield_Covariates_nna_plm$Prec_Aug$)

head(Yield_Covariates_nna_plm)
head(Yield_Covariates_nna_plm)

## For publication worth regression output need to change data names ##
# Get rid of variables which are not necessary
names(Yield_Covariates_nna_plm)
r <- names(Yield_Covariates_nna_plm)
Aug <- grep(c("*Aug"), r)
Aug
dim(Aug)

Yield_Covariates_Aug <- Yield_Covariates_nna_plm[,Aug]
names(Yield_Covariates_Aug)
dim(Yield_Covariates_Aug)

Yield_Covariates_ww <- Yield_Covariates_nna_plm[,1:7]
names(Yield_Covariates_ww)

Yield_Covariates_Aug_ww <- cbind(Yield_Covariates_ww, Yield_Covariates_Aug)
names(Yield_Covariates_Aug_ww)
names(Yield_Covariates_Aug_ww) <- c("year","comId","com","comIdState","comState","SHAPE_AREA","winterWheat", "SMI","SMI_lag","Prec","Prec_lag"
                                    ,"Tavg","Tavg_lag","Pet", "Pet_lag" , "Tmin","Tmin_lag","Tmax","Tmax_lag","SMI_GDM" )

names(Yield_Covariates_Aug_ww)
head(Yield_Covariates_Aug_ww)

## Determine formula
r <- r_Aug_ww
container_2v3poly[r_Aug_ww, 1]
container_2v3poly[r_Aug_ww, 2]

formula_Aug_ww_sm <- log(winterWheat) ~ poly(Prec, container_2v3poly[r, 1], raw = T) +  poly(Tavg, container_2v3poly[r, 2], raw = T) + dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year)) + 
  dummy(comId)

####################
## PLM Ergebnisse ##
Yield_Covariates_Aug_ww_plm <- pdata.frame(Yield_Covariates_Aug_ww, index = c("comId","year"))
head(Yield_Covariates_Aug_ww_plm)
plm.fit_Aug_ww <- plm(formula= update(formula_Aug_ww_sm, .~. - dummy(comId)),  data = Yield_Covariates_Aug_ww_plm)
summary(plm.fit_Aug_ww)


