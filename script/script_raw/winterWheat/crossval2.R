#############################################################################
#### Cross Validation to determine the model of WinterWheat for February ####
#############################################################################

library("plm")
library("boot")
library("gtools")
library("lme4")
library("sandwich")
library("lmtest")
library(car)

## Read data frame with winterWheat as only depedent variable ##
# Yield_Covariates <- read.csv( "/home/peichl/projects/crossvalid/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_ww.csv")

Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_ww.csv")
names(Yield_Covariates)

Yield_Covariates$X <- NULL

## Na-omit ##
sum(is.na(Yield_Covariates$winterWheat) )
Yield_Covariates_nna <- na.omit(Yield_Covariates) 
dim(Yield_Covariates_nna)

#################################
#### Define comId as factors ####
class(Yield_Covariates_nna$comId)
Yield_Covariates_nna$comId <- as.factor(Yield_Covariates_nna$comId )
head(Yield_Covariates_nna$comId)


## Drought Monitor Spezification ##
Yield_Covariates_nna$SMI_Feb_GDM <- cut(Yield_Covariates_nna$SMI_Feb, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),                                            ,
                                        labels = c("severe drought","moderate drought","abnormal dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))

## Change Indexing so that it can be used in plm package
Yield_Covariates_nna_normal <- Yield_Covariates_nna 
str(Yield_Covariates_nna_normal)

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
formula_PrecTavgSmiGDM_2v3poly <- log(winterWheat) ~ poly(Prec_Feb, container_2v3poly[r, 1], raw = T) +  poly(Tavg_Feb, container_2v3poly[r, 2], raw = T) + 
  dummy(SMI_Feb_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year)) + 
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
  save.image(file = "/home/peichl/projects/crossvalid/cross2.RData")
}
####################################################################
## Load environment with data which has been produced on cluster ##
load("/home/peichl/Documents/projects/correlation/data/data_processed/Crossvalidation/winterWheat/cross2.RData")


######################################################################
## Plot Bayesian Information Criterion and test Mean Squared Errors ##

BIC_loop_Feb_ww <- BIC_loop_2v8poly_Prec
cv.error_loocv_Feb_ww <- cv.error_loocv_2v8poly_Prec

par(mfrow=c(2,2))
plot(BIC_loop_2v8poly_Prec)
plot(cv.error_loocv_2v8poly_Prec)
plot(BIC_loop_Feb_ww)
plot(cv.error_loocv_Feb_ww, main="February", ylab="test MSE")

which.min(BIC_loop_Feb_ww)
which.min(cv.error_loocv_Feb_ww) 


# ich denke, dass hier Model Nummer 9  am besten ist
r_Feb_ww = which.min(BIC_loop_Feb_ww)
r = r_Feb_ww


#####################
## Check function ##
formula_PrecTavgSmiGDM_2v3poly

####################
## GLM Ergebnisse ##
## pdata.frame
glm.fit1 <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = Yield_Covariates_nna_plm)
summary(glm.fit1)

## plm.data frame
glm.fit3 <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = Yield_Covariates_nna_plm2)
summary(glm.fit3)

coefficients(glm.fit1)[1:15]
coefficients(glm.fit3)[1:15]

par(mfrow=c(1,1))
plot(glm.fit1)


## normal data frame
glm.fit2 <- glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = Yield_Covariates_nna_normal)

summary(glm.fit2)
coefficients(glm.fit2)[1:15]

## Analytic Plot ##
plot(glm.fit2) 
hist(glm.fit2$residuals)

## Heteroskedasdicity robust coefficients ##
vcovHC(glm.fit2)
coeftest(glm.fit2, vcov. = vcovHC)

# Assessing Outliers
?outlierTest
outlierTest(glm.fit2) # Bonferonni p-value for most extreme obs

# Influential Observations
# added variable plots
avPlot(glm.fit2)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

####################
## PLM Ergebnisse ##

head(Yield_Covariates_nna_plm)[,1:10]
plm.fit <- plm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - dummy(comId)),  data = Yield_Covariates_nna_plm, ,model="within", effect="individual")
summary(plm.fit)

plm.fit2 <- plm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - dummy(comId)),  data = Yield_Covariates_nna_plm2, ,model="within", effect="individual")
summary(plm.fit2)
' Die Ergebnisse sind leicht unterschiedliche, je nachdem welchen Datensatz man zugrunde legt.'


## Vergleich der 5 Modelle
## plm mit pdata.frame
coefficients(plm.fit)
## plm mit plm.data
coefficients(plm.fit2)
## glm mit data.frame
coefficients(glm.fit1)[1 :15]
## glm mit pdata.frame
coefficients(glm.fit2)[1 :15]
## glm mit plm.data
coefficients(glm.fit3)[1 :15]

' Die Daten sind sich tatsächlich am ähnlichsten wenn man mit dem plm.data frame arbeitet
 Nun lasse ich diese Daten auf dem Cluster neu laufen.'


fixef(plm.fit)
fixef(plm.fit2)
names(plm.fit)

## histogramm of residuals ##
hist(plm.fit$residuals)

## Compute and plot the leverage of each point
lev =   hat(model.matrix(plm.fit))
plot(lev)

# Identify the points with leverage larger tha 0.2
(Yield_Covariates_nna_plm[lev >0.2,]) # 5374-2002
Yield_Covariates_nna_plm[,comId="5374"]

## Test for heteroskedasdocity ##
bptest(plm.fit,  studentize=F)
'Need to account for heteroskedasdicity'

pbgtest(plm.fit)
' There is serial correlation'

coeftest(plm.fit, vcov = pvcovHC) # Inference with var covar matrix which is robust to heteroskedasdicity
coeftest(plm.fit, vcovHC(plm.fit, nethod = "arrellano")) # Inference with var covar matrix which is robust to heteroskedasdicity and autoocorrelatoin
' Coefficienten unterscheiden sich nicht zu ursprünglichen Model und significance Niveaus sind auch die selben'


## Waldtest ##
waldtest(plm.fit, plm.fit)

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



# Interpretation
'Die Struktur ändert sich nicht wirklich, das heißt, ich lasse die Läufe nicht nochmal durchlaufen.
 Für die Zukunft muss ich mir überlegen, ob die Daten nicht erst mit der within funktion transformiert
 und die transformierten Daten dann einfach in glm und vor allem cv.glm genutzt werden könenn. 
 Ähnlich überlegungen gelten übrigens auch für die räumlichen Panel Modelle. Dort werden die Modelle 
 sowieso über ML oder GMM geschätz, wodurch auch Informationskriterien Sinnvoll sein könnten.'



#######################################################################
## For publication worth regression output need to change data names ##
# Get rid of variables which are not necessary$X <- NULL
names(Yield_Covariates_nna)
dim(Yield_Covariates_nna)
r <- names(Yield_Covariates_nna)
Feb <- grep(c("*Feb"), r)
Feb
dim(Feb)

Yield_Covariates_Feb <- Yield_Covariates_nna[,Feb]
names(Yield_Covariates_Feb)
dim(Yield_Covariates_Feb)

Yield_Covariates_ww <- Yield_Covariates_nna[,1:7]
names(Yield_Covariates_ww)

Yield_Covariates_Feb_ww <- cbind(Yield_Covariates_ww, Yield_Covariates_Feb)
names(Yield_Covariates_Feb_ww)

head(Yield_Covariates_Feb_ww)
names(Yield_Covariates_Feb_ww) <- c("year","comId","com","comIdState","comState","SHAPE_AREA","winterWheat", "SMI","Prec",
                                    "Tavg","Pet", "Tmin","Tmax","SMI_GDM" )

names(Yield_Covariates_Feb_ww)
head(Yield_Covariates_Feb_ww)

## Determine formula
r <- r_Feb_ww
container_2v3poly[r_Feb_ww, 1]
container_2v3poly[r_Feb_ww, 2]

formula_Feb_ww <- log(winterWheat) ~ poly(Prec, container_2v3poly[r, 1], raw = T) +  poly(Tavg, container_2v3poly[r, 2], raw = T) + dummy(SMI_GDM,c("severe drought","moderate drought","abnormal dry", "abnormal wet", "abundant wet","severe wet")) + log(as.integer(year)) + 
  dummy(comId)

####################
## PLM Ergebnisse ##
Yield_Covariates_Feb_ww_plm <- pdata.frame(Yield_Covariates_Feb_ww, index = c("comId","year"))
head(Yield_Covariates_Feb_ww_plm)
r
plm.fit_Feb_ww <- plm(formula= update(formula_Feb_ww, .~. - dummy(comId)),  data = Yield_Covariates_Feb_ww_plm, model="within", effect="individual")
summary(plm.fit_Feb_ww)
fixef(plm.fit_Feb_ww)
mean(fixef(plm.fit_Feb_ww))
coef(plm.fit_Feb_ww, subset=3955)
par(mfrow=c(1,1))
plot(residuals(plm.fit_Feb_ww), predict(plm.fit_Feb_ww) )


###############################
## Bootstrap Standard Errors ##
dim(Yield_Covariates_Feb_ww_plm)
boot.fn <- function (data ,index) return (coef(lm(formula_Feb_ww),  data = data,  subset=index))
boot.fn <- function (data ,index) return (coef(lm(formula= update(formula_Feb_ww, .~. - dummy(comId)),  data = data ,subset=index)))

boot.fn(Yield_Covariates_Feb_ww, 1:4215)

##  Bootstrap the cluster ##
cluster.bs.plm(mod=plm.fit_Feb_ww, dat=Yield_Covariates_Feb_ww_plm, cluster="group", ci.level = 0.95,
               boot.reps = 1000, cluster.se = TRUE, report = TRUE,
               prog.bar = TRUE)


######################
#### PLot Results ####
summary(Prec)
Yield_Covariates_nna$winterWheat_log <- log(Yield_Covariates_nna$winterWheat)
names(Yield_Covariates_nna)
head(Yield_Covariates_nna)


## Precipitation ##
names(Yield_Covariates_nna)
p <- ggplot(Yield_Covariates_nna, aes_string(x="Prec_Feb",y=("winterWheat")) )
p + geom_point()

t <- ggplot(Yield_Covariates_nna, aes_string(x="Prec_Feb",y=("winterWheat_log")) )
t + geom_point()
plm.fit
f <- function(x) coefficients(plm.fit)[[1]]*x^1 + coefficients(plm.fit)[[2]]*x^2 + coefficients(plm.fit)[[3]]*x^3 + mean(fixef(plm.fit))

t + geom_point() + stat_function(fun=f, colour="red")


## SMI ##
names(Yield_Covariates_Feb_ww)
p <- ggplot(Yield_Covariates_Feb_ww, aes_string(x="SMI_GDM",y=("winterWheat")) )
p + geom_point()

t <- ggplot(Yield_Covariates_Feb_ww, aes_string(x="SMI_GDM",y=("winterWheat_log")) )
t + geom_point()

f <- function(x) coefficients(plm.fit_Feb_ww)[[1]]*x^1 + coefficients(plm.fit_Feb_ww)[[2]]*x^2 + coefficients(plm.fit_Feb_ww)[[3]]*x^3 + mean(fixef(plm.fit_Feb_ww))

t + geom_point() + stat_function(fun=f, colour="red")

