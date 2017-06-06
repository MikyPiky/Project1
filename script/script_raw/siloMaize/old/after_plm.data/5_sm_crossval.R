###########################################
#### Cross Validation SiloMaize in Mai ####
###########################################

library("plm")
library("boot")
library("gtools")
library("lme4")
library(lmtest)
library(car)

## Read data frame with siloMaize as only depedent variable ##
Yield_Covariates <- read.csv( "/home/peichl/projects/crossvalid/siloMaize/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv")

Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv")

Yield_Covariates$X <- NULL
names(Yield_Covariates)

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
Yield_Covariates_nna$SMI_Mai_GDM <- cut(Yield_Covariates_nna$SMI_Mai, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
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
formula_PrecTavgSmiGDM_2v3poly <- log(siloMaize) ~ poly(Prec_Mai, container_2v3poly[r, 1], raw = T) +  poly(Tavg_Mai, container_2v3poly[r, 2], raw = T) + 
  dummy(SMI_Mai_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year)) + 
  dummy(comId)

## Print formula
formula_PrecTavgSmiGDM_2v3poly

##################################################################################################
## Loop through the container list to cover all permutations of posssible degree of freedoms of ##
## of the polynomials of the variables                                                          ##
##################################################################################################


#####################################################################################################
#### Loop over two variables - Tavg and Prec - to dertermine their polygons starting with 8 grades ##

# ##  Define empty lists for loop ##
# cv.error_loocv_2v8poly_Prec <- rep(0,9)
# cv.error_loocv_2v8poly_Prec
# BIC_loop_2v8poly_Prec <- rep(0,9)
# BIC_loop_2v8poly_Prec
# 
# 
# print("start loop")
# 
# for(r in 1:9){
#   glm.fit <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = Yield_Covariates_nna) 
#   
#   BIC_loop_2v8poly_Prec[r] <- BIC(glm.fit)
#   cv.error_loocv_2v8poly_Prec[r] <- cv.glm(Yield_Covariates_nna, glm.fit )$delta[1] # hat funktioniert (dummies dürfen nicht nur als Faktor definiert werden)
#   save.image(file = "/home/peichl/projects/crossvalid/siloMaize/cross5.RData")
# }

## Load data from correlation project folder
load("/home/peichl/Documents/projects/correlation/data/data_processed/Crossvalidation/siloMaize/cross5.RData")

BIC_loop_Mai_SiloMaize <- BIC_loop_2v8poly_Prec
cv.error_loocv_Mai_SiloMaize <- cv.error_loocv_2v8poly_Prec

par(mfrow=c(2,2))
plot(BIC_loop_2v8poly_Prec)
plot(cv.error_loocv_2v8poly_Prec)
plot(BIC_loop_Mai_SiloMaize, main="Mai", ylab="BIC")
plot(cv.error_loocv_Mai_SiloMaize, main="Mai", ylab="test MSE")
which.min(BIC_loop_Mai_SiloMaize)
which.min(cv.error_loocv_Mai_SiloMaize) 

# ich denke, dass hier Model Nummer 5 am besten ist
r_Mai = 5
formula_PrecTavgSmiGDM_2v3poly

####################
## GLM Ergebnisse ##
## plm.data frame
glm.fit2 <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = Yield_Covariates_nna)

summary(glm.fit2)
coefficients(glm.fit2)[1:15]

## plot ##
plot(glm.fit2)
' There is an issue with leverage and outliers'

## Outliers ##
plot(rstudent(glm.fit2))
rstd <- rstudent(glm.fit2)
dim(Yield_Covariates_nna)

Yield_Covariates_nna[rstd > 5 | rstd < -5,] 

## Generate data without outliers rstd > |5|
Yield_Covariates_nna[rstd > 5 | rstd < -5,] #11
temp <- Yield_Covariates_nna[!rstd > 5 & !rstd < -5,]
dim(temp)
any(is.na(temp))

temp <- na.omit(temp)

## GLM without outliers ##
glm.fit3 <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = temp)
plot(glm.fit3)
hist(glm.fit3$residuals)
plot(rstudent(glm.fit3))
coefficients(glm.fit3)[1:15]
coefficients(glm.fit2)[1:15]

## Compute leverage of each point ##
lev <- hatvalues(glm.fit3)
plot(hatvalues(glm.fit3))

Yield_Covariates_nna[lev > 0.9,]
temp[lev > 0.9,]
temp[990:1050,]

## Check the high leverage points
temp[temp$comId == "3101",][,1:10] # für diese comId gibt es nur eine Beobachtung. Daher werde ich diese löschen.
temp[temp$comId == "3102",][,1:10] # für diese comId gibt es nur eine Beobachtung. Daher werde ich diese löschen.
temp[temp$comId == "5117",][,1:10] # für diese comId gibt es nur eine Beobachtung. Daher werde ich diese löschen.
temp[temp$comId == "5911",][,1:10] #  für diese comId gibt es nur eine Beobachtung. Daher werde ich diese löschen.

temp[temp$comId == "5774",][,1:10] # lösche ich auch

## Löschen der comIds für welche es nur eine Beobachtung gibt.
temp2 <- temp[!temp$comId == "3101",]
temp2 <- temp2[!temp2$comId == "3102",]
temp2 <- temp2[!temp2$comId == "5117",]
temp2 <- temp2[!temp2$comId == "5991",]
temp2 <- temp2[!temp2$comId == "5774",]

## GLM with less leverage ##
glm.fit4 <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = temp2)
plot(glm.fit4)
hist(glm.fit4$residuals)
plot(rstudent(glm.fit4))
coefficients(glm.fit3)[1:15]
coefficients(glm.fit4)[1:15]
' Es macht in der Coefficienten keinen unterschied, ob die leverage statistics hohen Werte herausgenommen werden oder nicht. 
Outlier machen hier aber Sinn.'

## Combine leverage and outlier statistic ##
plot(hatvalues(glm.fit4), rstudent(glm.fit4))
     
####################
## PLM Ergebnisse ##
plm.fit_Mai1 <- plm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - dummy(comId)),  data = Yield_Covariates_nna,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Mai1)

#no outliers
plm.fit_Mai2 <- plm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - dummy(comId)),  data = temp,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Mai2)

#no outliers and less leverage
plm.fit_Mai3 <- plm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - dummy(comId)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Mai3)

###################################################################
## Nochmaliges überprüfen der BICs der einzelnen Specificationen ##
BIC_loop_temp <- rep(0,9)

for(r in 1:9){
  glm.fit3 <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = temp2) 
  BIC_loop_temp[r] <- BIC(glm.fit3)
}
plot(BIC_loop_temp,ylab="BIC without outliers and less leverage" )
which.min(BIC_loop_temp) # hier ist der Wert nun auch 6, also Model sechs. Die Struktur ist nun auch der crossvalidation ähnlicher. 
r = 6
r_Mai = 6 
summary(glm.fit3 <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = temp2) )

plot(cv.error_loocv_2v8poly_Prec)
plot(BIC_loop_Mai_SiloMaize) # bricht aus der Struktur aus

## Heteroskedasdicity ##
library(lmtest)
library(car)
bptest(glm.fit4)
coeftest(glm.fit3,vcov=hccm(glm.fit3))

bptest(plm.fit_Mai2)
bptest(plm.fit_Mai3)

## Test ob andere Configuration heteroskedasdicity ausweisen
coeftest(plm.fit_Mai2)
coeftest(plm.fit_Mai3)

coeftest(plm.fit_Mai2,vcov=pvcovHC)
coeftest(plm.fit_Mai2,vcov=vcovHC(plm.fit_Mai2, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Mai2,vcov=vcovHC(plm.fit_Mai2, type = "HC3"))
' Ich denke es macht Sinn das Modell 3 ohne outlier und weniger leverage zu nehmen. Darüberhinaus nehme ich arrelano als methode. Der Grund ist, dass durch 
quasi demeaning serial coorelation verursacht wird. Als Option nehme ich HC4, um für die anderen influential observations zu kontollieren. '

###################################################
## Compare Models with and without Precipitation ##
plm.fit_Mai3 <- plm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - dummy(comId)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Mai3)

# Model ohne Preicpitation
plm.fit_Mai_noPrec <- plm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - dummy(comId) - poly(Prec_Mai, container_2v3poly[r, 1], raw = T)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))
summary(plm.fit_Mai_noPrec)

## Waldtest with heteroskedasdicity adapted variance covariance matrix
waldtest(plm.fit_Mai3 , plm.fit_Mai_noPrec, vcov= function(x) vcovHC(plm.fit_Mai2, method = "arellano", , type = "HC4"))
' Es macht Sinn die NiederschlagsVariablen im Model zu lassen, da diese auch gemeinsam significant sind.'
# anova(plm.fit_Mai3 , plm.fit_Mai_noPrec)

# ## BIC for Models without precipitation ##
# BIC_loop_noprec <- rep(0,3)
# for(r in 1:3){
#   glm.fit5 <- glm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - poly(Prec_Mai, container_2v3poly[r, 1], raw = T)),  data = temp2) 
#   BIC_loop_noprec[r] <- BIC(glm.fit5)
# }
# BIC_loop_noprec
# plot(BIC_loop_noprec, ylab="BIC no precipitation")
# 
# r <- which.min(BIC_loop_temp) # cubic Ansatz ist weiterhin am besten ohne Precipitation


# ## Bootstrap ##
# boot.fn = function (data ,index)  return(coef(  plm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - dummy(comId)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))  ))
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



## Determine formula
r <- r_Mai
container_2v3poly[r_Mai, 1] #3
container_2v3poly[r_Mai, 2] #3

formula_Mai_sm <- log(siloMaize) ~ poly(Prec, container_2v3poly[r, 1], raw = T) +  poly(Tavg, container_2v3poly[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year))


####################
## PLM Ergebnisse ##
head(Yield_Covariates_SM_Mai)
plm.fit_Mai <- plm(formula= formula_Mai_sm,  data = Yield_Covariates_SM_Mai, effect="individual", model="within")

summary(plm.fit_Mai)
save("plm.fit_Mai", file="/home/peichl/Documents/projects/correlation/script/script_raw/Crossvalidation/Output_Mai.RData")

###########################
## Explore Fixed Effects ##
attach(Yield_Covariates_SM_Mai)

fixef(plm.fit_Mai)
hist(fixef(plm.fit_Mai))
plot(fixef(plm.fit_Mai)) # es gibt hier definitiv clustering, das sollte eventuell räumlich dargestellt werden. Vor allem, wenn sich dies bei den anderen Daten wiederholen sollte. 
# Diese Struktur gibt es beim Mais auch in den anderen Monaten. Das macht auch so Sinn, da die Time-invarianten Effecte als seperater Fehler Term dargestellte werden. 
## Map of Fixed Effects

class(fixef(plm.fit_Mai))
str(fixef(plm.fit_Mai))
fixef <- as.data.frame(as.matrix(fixef(plm.fit_Mai)))
# plot(fixef)
str(fixef)
fixef <- cbind(rownames(fixef), fixef)
rownames(fixef) <- NULL
names(fixef) <- c("comId", "FE")

summary(log(siloMaize))
summary(fixef[,2])

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

KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")
str(KreisPolOGR,2)
head(KreisPolOGR)
dim(KreisPolOGR)
# spplot(KreisPolOGR, zcol="SHAPE_AREA")
KreisPolOGR$comId<-as.integer(str_sub(KreisPolOGR$RS,1,5))
head(fixef)
KreisPolOGR_fixef <- merge(KreisPolOGR, fixef, by = "comId")
dim(KreisPolOGR_fixef)
is.na(KreisPolOGR_fixef)
head(KreisPolOGR_fixef)
class(KreisPolOGR_fixef)
spplot(KreisPolOGR_fixef, zcol="FE", main="Spatial Distribution of Fixed Effects of Mai - Silo Maize" )
KreisPolOGR_fixef_df <- as.data.frame(KreisPolOGR_fixef[KreisPolOGR_fixef$])

# Plot mit Classintervalls ##
classInt <- classIntervals(KreisPolOGR_fixef$FE, n=3, style = "jenks")
classInt
plot(classInt)
pal <- brewer.pal(3, "Blues")

plot(classInt, pal=pal, main="Fisher-Jenks natural breaks", xlab="", ylab="")

at1 <- classInt$brks
at1[1] <- at1[1] - at1[1]/100
at1[length(at1)] <- at1[length(at1)] + at1[length(at1)]/100
spplot(KreisPolOGR_fixef, zcol="FE", at=at1, col.regions=colorRampPalette(pal)(length(at1)-1),main="Spatial Distribution of Fixed Effects of Mai - Silo Maize" )

#####################################################
## Plot Functions of Precipitation and Temperature ##
summary(Yield_Covariates_SM_Mai$siloMaize)
## Temperature ##
coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))
b3 <- coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))[3]
b4 <- coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))[4]
b5 <- coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))[5]

## Log
plot(Yield_Covariates_SM_Mai$Tavg, log(Yield_Covariates_SM_Mai$siloMaize- mean(Yield_Covariates_SM_Mai$siloMaize)), main="Fitted Polynom of Temperature - Log")
summary(Yield_Covariates_SM_Mai$Tavg)
xcurve <- seq(8.967, 17.4, 0.1)

ycurve <- b3 * xcurve  + b4 * xcurve^2   + b5 * xcurve^3
lines(xcurve,ycurve, col="green", lwd = 2)

## Exp
plot(Yield_Covariates_SM_Mai$Tavg, Yield_Covariates_SM_Mai$siloMaize- mean(Yield_Covariates_SM_Mai$siloMaize), main="Fitted Polynom of Temperature - Exp")
summary(Yield_Covariates_SM_Mai$Tavg)
xcurve <- seq(8.967, 17.4, 0.1)

ycurve <- exp( b3 * xcurve  + b4 * xcurve^2   + b5 * xcurve^3)
lines(xcurve,ycurve, col="green", lwd = 2)

## Precipitation
b1 <-coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))[1]
b2 <-  coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))[2]

## Log
plot(Yield_Covariates_SM_Mai$Prec, log(Yield_Covariates_SM_Mai$siloMaize - mean(Yield_Covariates_SM_Mai$siloMaize)), main="Fitted Polynom of Precipitation - Log")

summary(Yield_Covariates_SM_Mai$Prec)
xcurve <- seq(5.008, 308.700, 0.1)
ycurve <-   b1 * xcurve + b2 * xcurve^2
summary(ycurve)
lines(xcurve,ycurve, col="green", lwd = 2)

## Exp
plot(Yield_Covariates_SM_Mai$Prec, Yield_Covariates_SM_Mai$siloMaize- mean(Yield_Covariates_SM_Mai$siloMaize), main="Fitted Polynom of Precipitation -Exp")
# plot(Yield_Covariates_SM_Mai$Prec, Yield_Covariates_SM_Mai$siloMaize, main="Fitted Polynom of Precipitation")

summary(Yield_Covariates_SM_Mai$Prec)
xcurve <- seq(5.008, 308.700, 0.1)
ycurve <-  exp(b1 * xcurve + b2 * xcurve^2)
summary(ycurve)
lines(xcurve,ycurve, col="green", lwd = 2)

## Hier sollte ich mir die leverage Statistiken nochmals anschauen. Es gibt einige Werte im Niederschlag, die extrem groß sind und den fit sehr stark beeiflussen;

###########################################################
## Calculate exact percentage change of SMI coefficients ##
b6 <- coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))[6]
b7 <- coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))[7]
b8 <- coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))[8]
b9<- coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))[9]
b10 <- coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))[10]
b11 <- coeftest(plm.fit_Mai3,vcov=vcovHC(plm.fit_Mai3, method = "arellano", , type = "HC4"))[11]
100*(exp(b6)-1)
100*(exp(b7)-1)
100*(exp(b8)-1)
100*(exp(b9)-1)
100*(exp(b10)-1)
100*(exp(b11)-1)


## Yield vs SMI ##
plot(Yield_Covariates_SM_Mai$SMI, Yield_Covariates_SM_Mai$siloMaize- mean(Yield_Covariates_SM_Mai$siloMaize), main="Yield vs SMI")


x <- seq(1,12,1)
x
log(x)
y <- seq(1999,2010,1)
log(y)
summary(log(siloMaize))

######################################################
## Berechnen des Modells mit trend auf linker Seite ##

summary(plm.fit_Mai)

## Remove linear trend ##
'Fit yield on time and use the residuals of that for detrended yields'

plot(Yield_Covariates_SM_Mai$siloMaize ~ as.integer(Yield_Covariates_SM_Mai$year))
lineartrend <- lm(siloMaize ~ as.integer(year), data= Yield_Covariates_SM_Mai )
summary(lineartrend)
abline(lineartrend)
abline(453.5511,0, col="green")
length(resid(lineartrend))
Yield_Covariates_SM_Mai$siloMaize_lintren <- resid(lineartrend)

r=6
formula_Mai_sm_lintren  <- siloMaize_lintren ~ poly(Prec, container_2v3poly[r, 1], raw = T) +  poly(Tavg, container_2v3poly[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) 

## Remove log trend ##
'Fit log of yield on log of time and use the residuals of that for yields'
plot(log(Yield_Covariates_SM_Mai$siloMaize) ~ log(as.integer(Yield_Covariates_SM_Mai$year)))
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates_SM_Mai)
abline(logtrend)
summary(logtrend)
abline(6.1115,0, col="green")
Yield_Covariates_SM_Mai$siloMaize_logtrend <- resid(logtrend)


r=6
formula_Mai_sm_detrendlog <- siloMaize_logtrend ~ poly(Prec, container_2v3poly[r, 1], raw = T) +  poly(Tavg, container_2v3poly[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) 

###################################################################
## Determine right degree of poylnomials via BIC for log detrend ##
BIC_loop_logdetrend <- rep(0,9)


for(r in 1:9){
  glm.fit_logdetrend <- glm(formula= update(formula_Mai_sm_detrendlog, .~. + dummy(comId)),  data = Yield_Covariates_SM_Mai) 
  BIC_loop_logdetrend[r] <- BIC(glm.fit_logdetrend)
}

par(mfrow=c(1,1))
plot(BIC_loop_logdetrend, main="BIC without outliers and less leverage with left side detrend" )
which.min(BIC_loop_logdetrend) # hier ist der Wert nun 4
r_logdetrend = 6 

'Hier ändert sich nachvollziehbar in der Struktur und im Ergebnis wenig'

plot(glm.fit_logdetrend)
####################
## PLM Ergebnisse ##
r = r_logdetrend
plm.fit_Mai_detrendlinear <- plm(formula= formula_Mai_sm_detrendlinear,  data = Yield_Covariates_SM_Mai, effect="individual", model="within")
plm.fit_Mai_detrendlog <- plm(formula= formula_Mai_sm_detrendlog,  data = Yield_Covariates_SM_Mai, effect="individual", model="within")

summary(plm.fit_Mai)
summary(plm.fit_Mai_detrendlinear)
summary(plm.fit_Mai_detrendlog)

coeftest(plm.fit_Mai,vcov=vcovHC(plm.fit_Mai, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Mai_detrendlinear,vcov=vcovHC(plm.fit_Mai_detrendlinear, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Mai_detrendlog,vcov=vcovHC(plm.fit_Mai_detrendlog, method = "arellano", , type = "HC4"))

## Gleiche Konfiguration aber andere Jahre: 1:12 statt 1999 - 2010
head(Yield_Covariates_SM_Mai)
Yield_Covariates_SM_Mai$yearshort <- as.integer(Yield_Covariates_SM_Mai$year) - 1999



##############################
#############################
## Use PET instead of Temp ##
#############################
## Plot SMI Pet ##
plot(Yield_Covariates_SM_Mai$siloMaize ~ Yield_Covariates_SM_Mai$Pet)
plot(Yield_Covariates_SM_Mai$siloMaize ~ Yield_Covariates_SM_Mai$Tavg)

## Determine formula
formula_Mai_sm_Pet <- log(siloMaize) ~ poly(Prec, container_2v3poly[r, 1], raw = T) +  poly(Pet, container_2v3poly[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year))
formula_Mai_sm_Pet_detrendlog <- siloMaize_logtrend ~ poly(Prec, container_2v3poly[r, 1], raw = T) +  poly(Pet, container_2v3poly[r, 2], raw = T) + 
  dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) 


############################################################################
## Determine right degree of poylnomials via BIC for PET with log detrend ##
BIC_loop_Pet <- rep(0,9)

     
       for(r in 1:9){
         glm.fit_Pet <- glm(formula= update(formula_Mai_sm_Pet_detrendlog, .~. + dummy(comId)),  data = Yield_Covariates_SM_Mai) 
         BIC_loop_Pet[r] <- BIC(glm.fit_Pet)
       }
       
 par(mfrow=c(1,1))
 plot(BIC_loop_Pet, main="BIC without outliers and less leverage of PET configuration with left side detrend" )
 which.min(BIC_loop_Pet) # hier ist der Wert nun 4
 r_Pet = 4 
 lm.fit_Pet <- lm(formula= update(formula_Mai_sm_Pet_detrendlog, .~. + dummy(comId)),  data = Yield_Covariates_SM_Mai) 
 summary(lm.fit_Pet)   # hier scheinen die Fixed Effekte 61 Prozent der Variabilität zu erklären. Das ist sicherlich auch eine Begründung, warum diese weiterhin mit aufgeführt werden
                       # sollten und vor allema auch für die Vertiefung via maps.

####################
## PLM Ergebnisse ##
head(Yield_Covariates_SM_Mai)
plm.fit_Mai_Pet <- plm(formula= formula_Mai_sm_Pet,  data = Yield_Covariates_SM_Mai, effect="individual", model="within")
plm.fit_Mai_Pet_detrendlog <- plm(formula= formula_Mai_sm_Pet_detrendlog,  data = Yield_Covariates_SM_Mai, effect="individual", model="within")

summary(plm.fit_Mai)

summary(plm.fit_Mai_Pet) # R² ist größer, aber Anteil von SMI ist kleiner.
summary(plm.fit_Mai_Pet_detrendlog)
summary(plm.fit_Mai_detrendlog)

coeftest(plm.fit_Mai_Pet_detrendlog,vcov=vcovHC(plm.fit_Mai_Pet_detrendlog, method = "arellano", , type = "HC4"))



## Heteroskedasdicity ##
r=r_Pet
library(lmtest)
library(car)
bptest(plm.fit_Mai_Pet_detrendlog) # Null, dass es keine heteroskedasdicity gibt, wurde zurückgewiesen. 

coeftest(glm.fit_Pet,vcov=vcovHC(glm.fit_Pet)) # funktioniert nicht, wohl zu viele Dummies
coeftest(plm.fit_Mai_Pet_detrendlog,vcov=vcovHC(plm.fit_Mai_Pet_detrendlog, method = "arellano", , type = "HC4"))
coeftest(plm.fit_Mai_Pet_detrendlog,vcov=vcovHC(plm.fit_Mai_Pet_detrendlog, type = "HC3"))
' Ich denke es macht Sinn das Modell 3 ohne outlier und weniger leverage zu nehmen. Darüberhinaus nehme ich arrelano als methode. Der Grund ist, dass durch 
quasi demeaning serial coorelation verursacht wird. Als Option nehme ich HC4, um für die anderen influential observations zu kontollieren. '

