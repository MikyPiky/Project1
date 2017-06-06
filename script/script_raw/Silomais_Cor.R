

#### File Description ####
' - Data Manipulation
      - 
- new dataframe Yield_Covariates: no demean data (detrending did not work unfortunately)
- standardize the variables: reason: make coefficients comparable (two standardization procedures are applied, z-score and kernel estimation (SMI))
- Produce Scatterplots for relationship per acre output of yield (annual) and SMI (monthly) for the years 1999 - 2010 
-

'
## Input and Dependencies
' data/data_processed/Yield_Covariates <- MergeSMI_Yield (starts in 1995)
'

## Description of wheat ##
'
from wikipedia(German):
- demanding on soil quality, climate and water supply
- siloMaize: 
- Winterweizen wird, nach Ende der Keimruhe des Saatguts, im Herbst abhängig von Höhenlage und Saatzeitpunkt ab Ende September bis in den Dezember hinein mit ungefähr 280 bis 520 Körnern pro m² ausgesät.
- Wie alle Wintergetreidearten benötigt auch Winterweizen zum Abbau der Schosshemmung eine Vernalisation durch Frosttemperaturen
- Die Hauptbestockung findet erst im Frühjahr statt und ist stark von Sorte und Pflegemaßnahmen abhängig. 
- Bei später Aussaat, die meistens mit niedrigen Bodentemperaturen verbunden ist, verläuft die Keimung langsamer. 
- Eine Keimung findet allerdings auch noch bei Bodentemperaturen von 2 bis 4 °C statt. -> enstsprechend für Temperaturen kontrollieren: statt Tempavg viel mehr anzahl der Tage über einem bestimmten Wert?!?

from Landwirtschaftlicher Pflanzenanbau
- Weizen weist eine gute Anpassungsfähigkeit an vielfältge Böden und Klimabedingungen aus
- Anbaudichte auf nördlicher Halbkugel am höchsten
- Wesrteurope sehrn hohe Erträge: 100 dt/ha
- gestiegene Produktion geht überwiegend auf erhöhte Hektarerträge zurück
# Gründe dafür
- INtensivierung und Optimierung der Produktionstechnik
- Züchtung: Erhöhung der Ertragsleistung und Anpassung der Sorten an sich verändernde Produktionstechnik -> erhöhte Standfestigkeit (Grüne Revolution Ende der 60 er Jahre)

- Deutschland
- Seit dem 70er enorme Ertragssteigerung
- Intensievierung Produktionstechnig durch gesteigerte Stickstoffdüngung, leistungsfähigere Funkzide und die Anpassung der Sorten an die intersiveren Bedingungen
- hauptächliche GEtreiederzeugungsgebiete: Bayern, Niedersachsen und neue Bundesländer
- auf guten Standorten wie Magdeburger Börde oder Niederbaern hauptsächlich Weizen
- Norddeutschland höere Erträge als Süddeutschland  
- bei ähnlichen Bodenverhältnissen wirken sich sowohl die meist gleichmäßrigeren Temperaturen und Niederschläge, die milderen Winter 
als auch die längeren Tage während der Hauptvegetationszeit positive auf das Ertragsniveau aus
- Heute ist von keinen wesentlichen Einfluss der Produktionstechnik auf die Ertragsteigerung auszugehen
- BAyern: größte Weizenproduktionsfläche (500 000 ha und 3,5 Mio t)
- Niedersachsen und Schleswig - Holstein: stärkste Ausdehnung der Weizenflächen seit 1950er
- Vorteile Norddeutschland für Getreideanbau:
- gute Böden, weniger Qualiätsweizen als in Bayern
- KLimatische und geografisch bedingte Gegebenheiten Norddeutschland
- ausgeglicheneres Klima: weniger extrem heiße Tage, mildere Winter, kühlere SOmmmer, ausgeglichernere Niederschlagsverteilung
- -> Kornfüllungsphase meist länger
- Süden; HItze und Trockenheit: vorzeitige Abreife -> schlechtere Konrausbildung
. Tage im frühjahr und Sommer sind länger

- 2012: 50% Getreideanteil

Allgemein Vorrausetzungen für den Getreidebau
- Licht, Temperatur und Wasser beeinkflussen maßgebich die Entwicklung des Getreides
- Licht: 
- Tagesläge -> Blütenbildung nd Übergang in generative Phase 
Kurztagesbedingungen: nur vegetatives Wachstum
Blüte erst wenn bestimmte Tageslänge überschritten sit
- Intensität:
Beinflusst Photosynthese -> Trockenmasseproduktion
- Qualität:
morphologische ENtwicklung, insbesondere Wuchshöhe

- Temperatur
-  Verantwortlich für
Abbau Keimruhe, KEimungsbeging, Vernalisation, Differenzierungder generativen Strukturen, Trockenmasseproduktion und Atmung
- Wasser
- benötigt für
Begin Keimung, Wachstum, gesamten Stoffwechsel, die Kühlung bei Hitze (Evapotranspiration), Nährstoffverfügbarkeit und - aufnahme

Ansprüche von Winterweizen an ausgewählte Standort und Umweltfaktoren
- Vegetationsdauer, Licht:
300 Tage (Zeitraum Aussaat - Reife), fotoperiodeisch Reaktino: langstagsempfindlich 
- Temperatur 
Min Keimtemp: 3, Frostresistenz bis -20, Mindestwachstum: 5
- Wasser:
Transpiratinoskoeffizient (Wasser/kg produzierte Trockenmasse): 500, Nutzungsgrade der Winterfeuchte hoch, Epfindlichkeite gegenüber Sommmertrockenheit: hoch
- Boden:
geeignet für leichte, sandige Bäden: gering; schwere, tonige Böden mittel, 
optimaler PH-BEreich ~7,0

Produktionstechnik
- Klima: 
- Die Klimatischen Ansprüche des Weizens sind höher als die der übrigen Getredidearten: Dies gilt beonders hinsichtlich des Wärme und Feuchtigkeitsbedarfs
- Weizen benötigt wegen der späten Abreife bis Ende Mayi ausreichend Wasserversorung, entweder durch Bodenvorat (SMI) oder Niederschlag
- Erreicht in Mittelgebirgen früher klimate Grenze als Winterrogen, Sommergerste und Sommerhafer
- Frosthärte geringer als bei Roggen, jedoch besser als bei Wintergerst oder Winterhafer
- Nach Keimen: sehr Frostempfindlich
- In 1- bis 2- Blat-Stadium sind Pflanzen relative wenig Widerstandsfähig
- 3-5- Blattstadium am unempfindlichsten
- Nach Abhärtephase mit zunehmend tieferen Temperaturen lann er (Sortenabhängig)Temperaturen von bis zu -20 Grad überdauern
- Plötzliche Kälteinflüsse, nach Wärmephasen (im späteren Winter), sind nicht so gut
- moorige Böden: Wechselfröste im Winter nicht so gut

- Böden
- Anforderung: 
- gute, tiefgründige Böden
- sichere Wasser- und Nährstoffversorgung
- ausreichend Kalk und Humusversorgung
- Hauptwasserbedarf relativ spät -> Wasserspeicherfähigeit der Böden wichtig
- sehr leichte Böden mit häufigen Trockenheitsabschnitten im Frühsommer wenig geeiget -> mit stärkeren Ertragsschwankungen zu rechnen
- schwere Tonböden: Trockenphasen eher zu Wasserstress
- leichte Böden mit guter Niederschlagsverteilung und Nährstoffverorgung sind gut geeignet

Aussaat
- Die optimale Saatzeit für Winterweizen liegt in den meisten klassischen Weizenanbaugebietenzwischen dem 1. und 20. Oktober
- Raue Lagen (Küsten, Höhenlagen im Mittelgebirge) -> früher angebaut
- Winterweizen muss vor Wintereinbruch nicht bestocken -> dennoch führen spätere Saaten zu niedrigeren Erträgen, außern in Köln - Aachener Bucht
- Saatstärke: 300 bis 400 Keimlinge im Durchschnitt, mehr bei ungünstigen Saatbett und rauen Klimalagen

Ernte
- Der richtige ZEitpunkt für den Mähdrusch ist die Totreife (BBCH - Stadium 92) des Weizens
- 14 - 16% Kornfeuchtigkeit
- Bei einer länger anhaltenden Regenphase gegen Ende der Reife ist besonders bei Weizen, Triticale und Roggen mit Auswuchs zu rechnen.
- Nach einer Homepage im Hochsommer, also Mitte Juni bis Mitte August
- www.planet-wissen.de : Ernte im Jahreskreislauf: Der Winterweizen und der Winterroggen sind normalerweise im Hochsommer ab Ende Juli erntereif.

--> Dann macht es eigentlich Sinn, die Monate nach August zu laggen, da diese sich auf das folgende Jahr auswirken.
Dennoch stellt sich mir hier die Frage, inwiefern dass überhaupt Sinn macht in einer Panel RandomEffects Model Spezifikation.

Düngung
- 




Schlussfolgerung; 
- Winterweizen wichtigstes Getreide
- detrenden relevant aufgrunde technolgischer Entwicklung
- demanding on soil quality, climate and water supply    
- Licht (PET), Temperatur (Temp), Wasser (SMI, Niederschlag), Boden (SMI, FE)
- Extremere Werte im Süden reduzieren Ertrag
'

## Procedure ##
'
CCFS6: Chapter 6 in Climate Change and Food Security

Metodisches Vorgehen
# Die einzelnen Methoden haben ihre Vor - und Nachteile und haben einen unterschiedlichen Interpretationsspielraum
- zwei Unterschiedliche Motivationen (CCFS6)
- forecast as yield at a given place in year under existing weather conditions: time series: assumes farmer make optimal decisions according to existing distribution of 
possible weather outcomes & corn is sowed before weather realizes
- simulate the effect of changes in average weather in the future (predict impact of changing conditions)
- cross section: 
- panel



- Cross Section (Problem: omitted Variable Bias)
- cross-sectionaly analysis of a specifc crop would incorporate how a farmer switches to other crop variety of the same crop (e.g. corn varities)
- need to account for all covariates to correctly identify the effect of SMI on corn yields (CCFS6)
- needs variation in climate/SMI wihin the considered population
- cross-sectionaly analysis of a specifc crop would incorporate how a farmer switches to other crop variety of the same crop (CCFS6)
-> hier habe ich nicht die Größe Crop Yield, sondern die differenzierten Feldfruchtarten

- alternative: Multinomial regression of crop choices: identify how farmers swith crops with changing climates (siehe hierzu cross section in CCFS6) (Seo)


- Lineare Panel Daten Modelle (generall kann man sagen, dass das die mehr ökonomische Perspektive , siehe Anwendungen von Wolfram Schlenker)
- Panel Datensatz, daher natürlicher Ansatz hier
- Vorest Linear
->  diese Annahme ist voerst sehr gewagt und muss hinterfragt werden
->  gerade thresholds können hier eine entscheidende Rolle spielen
->  Schlenker et al: Tchebychev Polynomial: dennoch wird interpretation dann auch schwieriger
- Id. Pooled 
- Regular OLS regression does not consider heterogeneity across groups or time

- Fixed Effects
- Use fixed-effects (FE) whenever you are only interested in analyzing the impact of variables that vary over time. 
->  check, SMI is changing over time -> anomalies, i.e deviation from the mean, are of particular interest
- fixed-effects models are designed to study the causes of changes within a person [or entity] 
-> methodocally often called within transformatio
->  check change within each community
- the estimated coefficients of the fixed-effects models are not so prone to be biased because of omitted time-invariant characteristics 
->  for instance no need to control for time invariant factors as soil texture and elevation characteristica within a entity 
- When using FE we assume that something within the individual may impact or bias the predictor or outcome variables and we need to control for this heterogeneity.
->  different communities have different preconditions: socially, naturally, economically
->  (CCFS6): recognizes for fundamental differences between spatial units and that it is difficult to account for all these differences explicitly in the model 
->  test whether it makes sense to controll for west vs east(ehemaliger LPG Strukturen in der Oststaaten) or to control for state dummies 
(wie wird agrarpolitik gestaltet?)
- FE remove the effect of those time-invariant characteristics so we can assess the net effect of the predictors on the outcome variable.
- (CCFS6): .. if covariates influence yield on a additive fashion
->  net effect of SMI (no timeinvariant impact)
- Another important assumption of the FE model is that those time-invariant characteristics are unique to the individual 
and should not be correlated with other individual characteristics.
->  this might be the case, because the soil quality might correlate over the entities
->  soil also correlated with SMI
->  hence, general FE might be reasonable, because the correlations are evened out due to aggregation
- Each entity is different therefore the entity’s error term and the constant (which captures individual characteristics) should not be correlated with the others.
If the error terms are correlated, then FE is no suitable since inferences may not be correct and  you need to model that relationship (probably using random-effects), 
this is the main rationale for the Hausman test (presented later on in this document).
- (CCFS6):fixed effect is a dummy or indicator variable that is set to one if observation from a group is included an are set to zero otherwise
-   cannot include time-invariant variable anymore: collinear with fixed effects
- in panel hedonic model cannot use average weather as explanatory combined with fixed effects 
because it is constant within a group and hence a linear multiple of the indicator variable
- (CCFS6): f.e. equal to joint group-specific demeaning and the dependent as well as the independent variables
- subtracting group specific avaerages for each variable and adjust the error in degrees of freedom -> thereby, timeinvariant effects are deleted
-	important for interpretation
-  regression uses deviations from group-specific averages to identify the parameter of interest, here community specific averages
-  equivalent to fit a regression line through each group where the slope is forced to be the same for each group b
but the intercept is allowed to vary by group
- all groups are forced to the same sensivity on the independent variable, i.e  here SMI
- allowing for a distribution of SMI sensitiveiies -> random coefficient model  
- (CCFS6): two ways of calculation fe-coefficients
- set dummy for each group, leave intercept out  (multicollinearity)
- demean each variablen (dependent and independent) and  run linear regression trought them
- demean means substract the entity specific average <- time -demeaning
- important for interpretation: regression uses deviations from entity specific averages to identify the parameter of interest
- equivalent to fit a regression line through each entity where the slope is forced to be the same for each entity but the intercept
is allowed to vary by entity
- all countries are forced to the same sensivity on the independenty variable
- allowing for a distribution of weather sensitiveties->random coefficient model
-> is demeaning some kind of normalization? -> was den center angeht, ja, weil alle Werte den Mean Null haben. Aber die Scale bleibt unberührt.


- Aspekte der Anpassung und die resultierende Ökonomischen Interpretation.

- Random Effects
- The random effects model is: Y_it = βX_it + α + u_it + ε_it with two error components: Within-entity error and Between-entity error
- Because the random effects regression error in (15.24) has two components, one for the individual and one for the regression, the random effects model
is often called an error components model
- May suffer from omitted variable bias problem: In random-effects you need to specify those individual characteristics that may or may not influence the predictor variables. 
The problem with this is that some  variables may not be available therefore leading to omitted variable bias in the model.
- The rationale behind random effects model is that, unlike the fixed effects model, the variation across entities is assumed to be random and uncorrelated with the
predictor or independent variables included in the model 
- can inlcude time-invariant variables (fixed effects they are absorbed by intercept)
- Random effects assume that the entity’s error term is not correlated with the predictors which allows for time-invariant variables to play a role as explanatory variables.
- RE allows to generalize the inferences beyond the sample used in the model.


- Fixed vs Random Effect
- Frage: Correliert der SMI mit nicht beobachtbaren time invarianten Effekten auf den Ertrag
- Hausmann Test:  null hypothesis is that the preferred model is random effects vs. the alternative the fixed effects; It basically tests whether the unique
errors (u_i) are correlated with the regressors, the null hypothesis is they are not
- Other tests, diagnostics
- joint test to see if the dummies for all years are equal to 0
- Testing for random effects: Breusch-Pagan Lagrange multiplier (LM): The LM test helps you decide between a random effects regression and a simple OLS regression.
The null hypothesis in the LM test is that variances across entities is zero. 
This is, no significant difference across units (i.e. no panel effect). 
- Testing for cross-sectional dependence/contemporaneous correlation: Hat das was mit spatial correlation zu tun?
- using Breusch-Pagan LM test of independence
According to Baltagi, cross-sectional dependence is a problem in macro panels with long time series (over 20-30 years). 
This is not much of a problem in micro panels (few years and large number of cases). 
The null hypothesis in the B-P/LM test of independence is that residuals across entities are not correlated.


- Using Pasaran CD test
As mentioned in the previous slide, cross-sectional dependence is more of an issue in macro panels with long 
time series (over 20-30 years) than in micro panels. 
Pasaran CD (cross-sectional dependence) test is used to test whether the residuals are correlated across entities*. 
Cross-sectional dependence can lead to bias in tests results (also called contemporaneous correlation). 
The null hypothesis is that residuals are not correlated.

- Testing for heteroscedasdicity
- Modified Wald test for groupwise heteroskedasticity in fixed effect regression model. The null is homoskedasticity (or constant variance).
- The null hypothesis for the Breusch-Pagan test is homoskedasticity.

- Testin for serial autocorrelation
- Serial correlation tests apply to macro panels with long time series (over 20-30 years). Not a problem in micro panels (with very few years). 
Serial correlation causes the standard errors of the coefficients to be smaller than they actually are and higher R-squared.
- A Lagram-Multiplier test for serial correlation
- Testing for unit roots/stationarity
- The Dickey-Fuller test to check for stochastic trends. The null hypothesis is that the series has a unit root (i.e. non-stationary). 
If unit root is present you can take the first difference of the variable.

- Mixed Effects Model

Eigenheiten der Yield Daten
- relative Daten, da yield pro Hektar
- ABER: Keine Anomalien wie etwa SMI im Sinne von SMI, also Daten die normalisiert wurden durch 
- Daher muss dies auch für die Yield Daten Geschehen nach Luis

Eigenheiten der SMI Daten
- relative Daten
- qualitative Variable
- Nach Rohini: Um Vergleichbarkeit der Variablen zu gewährleisten sollte der SMI Normalisiert werden <- Verstehe ich, da dies schon der Fall ist. 
- 

Frage: Welche Folgen hat es, dass sowohl yield als auch SMI relative Daten sind.

Transformation der SMI Daten: 
- Hier steht generell das Problem des Timing im Vordergrung. Wann hat SMI welche Einfluss? 
- SMI multivariate Verwenden, also die einzelen Monaten in Regression berücksichtigen. 
- Envetuell kann ich hier dann die Monate Clustern, in welche mit positiven und negativen Einflüssen. Oder nicht relevante Monate können rausgelassen werden.
- Einteilung in Wachstumsphasen: Verschiedene Kombinationen testen
- Dies kann auch multivariate geschehen, in dem ich zum Beispiel das Jahr in vier Teile unterteile.
- Manipulation nach dem Vorbild der Growing degree days

- Darüberhinaus kann man hier dem Vorschlag von Luis folgen, eine Zielfunktionen zu finden und diese dann zu optimieren, sodass dann die Monate und Cluster mit der 
größten Einfluss auf das Wachstum gewählt werden. 


Irrigation
- Eventuell müssen Gegenden mit hohen Irrigation Anteil rausgerechnet werden.

## Multivariate Verfahren ##
- Berücksichtigung anderen Variablen 
- bisher Verfügbar 
- Temperatur (monatlicher Durchschnitt)
- Die Daten können ähnlich gehandhabt werden wie SMI
- maximal Temperatur  Paper von Lobell et al 2013; Maximal Temperatur als wichtiger und kritischer Faktor des Pflanzenwachstums 
- Niederschlag (monatliche Summe)
- potentielle Evapotranspiration
- DEM (timeinvariant)


- bisher nicht Verfügbar
- Bodentextur (timeinvariant) 
- BÜK1000
- von mHM
- Bodenqualität
- Soil Quality Rating
- Bodenwert/Ackerzahl (Alternative Müncheberger Soil Quality Rating)
- Naturräume nach Andreas Marx
->  werden vor allem bei Random und Mixed Effects Modellen relevant
- Phenologische Daten
- LAI (LAI is used to predict photosynthetic primary production, evapotranspiration and as a reference tool for crop growth)
- NDVI      
- Dünger und Pestizide
- Meterogolische Daten  
- Growing Degree Days
- sunshine hours (wichtig für Energie Bilanz)      
->  Diese Daten sind timeinvariant und könnten auch ein fixed-effect Model noch gut unterfüttern.

## Wahl der Variablen ##
- Da meine Variablen meistens im monatlichen Durchschnitt zur Verfügung stehen habe ich viele Variablen. Daher sind wahrscheinlich Prozesse des statistical learning für die Modellwahl 
sinnvoll. 
- Achtung, da das Model durch Theorie und Empirie unterfüttert sein muss.
- Achtung: Die Auflösung der  erklärenden Variablen darf nicht größer sein als die der abhängigen Variable. Daher muss ein Aggregat gebildet werden. Bei der Bildung des Aggregates
kann man wohl non-linearities intergrieren
- stepwise regression (ist nicht mit den Monatsdaten möglich als nested Ansatz. Es könnten nur unterschiedliche Modelle verglichen werden.)
- Selection criterion -> trade off of over-fitting and right signal
- critcism: 
- Test are biased, since based on the same data
- need to adjust for degrees of freedom
- over-simplification of reality
--> verify model on a independent data set: PRESS statisitics: cross validation used in regression anaylsis to provide a summary measure of the fit of a model to a sample
of observations that were not themselves used to estimate the model
- aus CH6 of AITSL 2013: Linear Model Selection and Regularization
- Verschieden Vorschläge, welche in R umgesetzt werden können.
- Allgemeine bekannte Funktionen zum subsetten linearer Funktionen: leaps::leaps und stats::step (mainly glm and lm, based on AIC)



## Gewichtung ## 
- In der Regionalstatistic stehen Variablen zur Gewichtung der Regression zur Verfügungn
- Diese Aspekte stelle ich erstmals zurück

## Kontrolle von räumlicher Autocorrelation ##
-> Das erscheint mir generell relevant, sollte ich aber vorerst nach hinter verschieben.
- splm Package
- von Daniel Ambach und Reimund Schwarze als wichtig erachtet 

## Interaktionen ##
- Es erscheint definit interessant, Temperatur/Prec/Pet mit SMI Extremes zu interagieren, da SMI das Tröpfchen an der Wage sein kann.

## Technologischer Fortschritt und Trends ##
- generell ist trend zu beobachten im yield, i.e. sollte man sich die Anomalien anschauen und den Trend löschen
- 


## Fragen:
Kann ich model selection auf basis von lm und glm machen und dann zum Package plm übergehen, um fixed effects oder random effects Modelle zu schätzen?
Dabei ist zu berücksichtigen, dass plm random auch mit lme geschätzt werden kann und dafür eine step Funktion angewendet werden kann. (?)




'

## Output
'
WW_SMI

## Plots
Scatterplots siloMaize SMI
'


#### Packages ####
library(corrplot)
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(ncdf4)
library(zoo)
library(maps)
library(colorspace)
library(lattice)
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(ncdf4)
library(reshape)
library(stringr)
library(eeptools)
library("car")
library("lmtest")
library("lattice")
library("zoo")
library("scales")
library("nlme")
library("lme4")
library("mgcv")
library("apsrtable")
library("texreg")
library("DataCombine")
library("reshape2")
library("gplots")
library("pracma")
library("plm")
library("MASS")
library(classInt)
library(foreign)
library(splines)
# library("QuantPsyc")
############################################################################################################################################################################################
######################################################################## Laden der Daten ###################################################################################################
############################################################################################################################################################################################

Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax")
Yield_Covariates$X <- NULL


## Get rid of data with _demean suffix
r <- names(Yield_Covariates)
rr <- grep("*demean", r, invert=T)
Yield_Covariates_nodemean <- Yield_Covariates[,rr]
dim(Yield_Covariates_nodemean)
class(Yield_Covariates_nodemean)
head(Yield_Covariates_nodemean)
unique(Yield_Covariates_nodemean$year)
sum(is.na(Yield_Covariates_nodemean)) ## in diesem Data.Frame sind zu viele NAs!
dim(Yield_Covariates_nodemean)
4920*221



## Get rid of other crops ##
names(Yield_Covariates_nodemean)
drop <- (c("rye","winterWheat","winterBarley" ,"summerBarley", "oats", "triticale", "potatoes", "sugarBeet", "winterRape", "rye_deZscore",
           "siloMaize_deZscore", "winterBarley_deZscore","summerBarley_deZscore","oats_deZscore", "triticale_deZscore", "potatoes_deZscore", "sugarBeet_deZscore" ,
           "winterRape_deZscore"))
names(Yield_Covariates_nodemean[,!names(Yield_Covariates_nodemean) %in% drop])
Yield_Covariates_nodemean_sm <- Yield_Covariates_nodemean[,!names(Yield_Covariates_nodemean) %in% drop]
dim(Yield_Covariates_nodemean_sm )
sum(is.na(Yield_Covariates_nodemean_sm))

## Get rid of data with _deZscore suffix
r <- names(Yield_Covariates_nodemean_sm)
rr <- grep("*zscore", r, invert=T)
Yield_Covariates_nodemean_nozscore_sm <- Yield_Covariates_nodemean_sm[,rr]
dim(Yield_Covariates_nodemean_nozscore_sm)
class(Yield_Covariates_nodemean_nozscore_sm)
head(Yield_Covariates_nodemean_nozscore_sm)
unique(Yield_Covariates_nodemean_nozscore_sm$year)
sum(is.na(Yield_Covariates_nodemean_nozscore_sm)) 

dim(Yield_Covariates_nodemean_nozscore_sm)
is.na(Yield_Covariates_nodemean_nozscore_sm)

names(Yield_Covariates_nodemean_nozscore_sm)
any(is.na(Yield_Covariates_nodemean_nozscore_sm$comIdState))
any(is.na(Yield_Covariates_nodemean_nozscore_sm$comState))

sum(is.na(Yield_Covariates_nodemean_nozscore_sm$area100ha)) # Nas


sum(is.na(Yield_Covariates_nodemean_nozscore_sm$SHAPE_AREA)) # no Nas


sum(is.na(Yield_Covariates_nodemean_nozscore_sm$siloMaize)) # 668
# sum(is.na(Yield_Covariates_nodemean_nozscore_sm$siloMaize_deZscore)) # 900
sum(is.na(Yield_Covariates_nodemean_nozscore_sm$Tavg_Feb)) # 48/12 = 4 -> Das sind genau die vier #319 (Oberallgäu),315 (Lindau),308 (Kempten),236( Garmisch Partenkirchen)
sum(is.na(Yield_Covariates_nodemean_nozscore_sm$Por_L1L2_30))
sum(is.na(Yield_Covariates_nodemean_nozscore_sm$Por_L3_nd))
sum(is.na(Yield_Covariates_nodemean_nozscore_sm$SMI_Sep)) # 48
sum(is.na(Yield_Covariates_nodemean_nozscore_sm$SMI_Sep_lag)) #48

sum(is.na(Yield_Covariates_nodemean_nozscore_sm$year))

all(is.na(Yield_Covariates_nodemean_nozscore_sm$Tavg_Feb) == is.na(Yield_Covariates_nodemean_nozscore_sm$SMI_Feb))
all(is.na(Yield_Covariates_nodemean_nozscore_sm$Tavg_Feb) == is.na(Yield_Covariates_nodemean_nozscore_sm$Prec_Jan))    
all(is.na(Yield_Covariates_nodemean_nozscore_sm$Tavg_Feb) == is.na(Yield_Covariates_nodemean_nozscore_sm$Pet_Apr))    
all(is.na(Yield_Covariates_nodemean_nozscore_sm$Tavg_Feb) == is.na(Yield_Covariates_nodemean_nozscore_sm$Por_L1_5))    
all(is.na(Yield_Covariates_nodemean_nozscore_sm$Tavg_Feb) == is.na(Yield_Covariates_nodemean_nozscore_sm$Tavg_Mai))  
all(is.na(Yield_Covariates_nodemean_nozscore_sm$Tavg_Feb) == is.na(Yield_Covariates_nodemean_nozscore_sm$Tmin_Jun))  
all(is.na(Yield_Covariates_nodemean_nozscore_sm$Tavg_Feb) == is.na(Yield_Covariates_nodemean_nozscore_sm$Tmax_Jul))  
all(is.na(Yield_Covariates_nodemean_nozscore_sm$Tavg_Feb) == is.na(Yield_Covariates_nodemean_nozscore_sm$SMI_Sep_lag))  

Yield_Covariates_nodemean_nozscore_sm$area100ha <- Yield_Covariates_nodemean_nozscore_sm$siloMaize_deZscore <- NULL

Yield_Covariates <- Yield_Covariates_nodemean_nozscore_sm


#########################################################
#### Write Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax ####
#########################################################
write.csv(Yield_Covariates, "data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv")


############################################################################################################################################################################################
######################################################################################## Data Manipulation #################################################################################
############################################################################################################################################################################################

## Explore Silo Maize Variable
head(Yield_Covariates)






############################################################################################################################################################################################
######################################################################################## Create new variables ##############################################################################
############################################################################################################################################################################################

###################################################
#### Aggregates of all timvevariant Covariates ####
###################################################
names(Yield_Covariates)

##########################################################################
## Calculate Growth Months of siloMaize: nach Schindler April to June ##
# ## average
# Yield_Covariates <- transform(Yield_Covariates, SMI_GP_short_av = (SMI_Apr + SMI_Mai + SMI_Jun)/3)  # gibt an, wie stark die Abweichung vom langfristigen Mittel 
#                                                                                                                       # im Durchschitt ist
# Yield_Covariates <- transform(Yield_Covariates, Prec_GP_short_av = (Prec_Apr + Prec_Mai + Prec_Jun)/3) 
# Yield_Covariates <- transform(Yield_Covariates, Tavg_GP_short_av = (Tavg_Apr + Tavg_Mai + Tavg_Jun)/3) 
# Yield_Covariates <- transform(Yield_Covariates, Pet_GP_short_av = (Pet_Apr + Pet_Mai + Pet_Jun)/3) 
# Yield_Covariates <- transform(Yield_Covariates, Tmax_GP_short_av = (Tmax_Apr + Tmax_Mai + Tmax_Jun)/3) 
# Yield_Covariates <- transform(Yield_Covariates, Tmin_GP_short_av = (Tmin_Apr + Tmin_Mai + Tmin_Jun)/3) 
# names(Yield_Covariates)

## cumulative
# passt eigentlich besser zur agronomischen Theorie
# Yield_Covariates <- transform(Yield_Covariates, SMI_GP_short_cum = (SMI_Apr + SMI_Mai + SMI_Jun)) # gibt die Summe der Abweichungen an. 
# Yield_Covariates <- transform(Yield_Covariates, Prec_GP_short_cum = (Prec_Apr + Prec_Mai + Prec_Jun)) 
# Yield_Covariates <- transform(Yield_Covariates, Tavg_GP_short_cum = (Tavg_Apr + Tavg_Mai + Tavg_Jun)) 
# Yield_Covariates <- transform(Yield_Covariates, Pet_GP_short_cum = (Pet_Apr + Pet_Mai + Pet_Jun)) 
# Yield_Covariates <- transform(Yield_Covariates, Tmax_GP_short_cum = (Tmax_Apr + Tmax_Mai + Tmax_Jun)) 
# Yield_Covariates <- transform(Yield_Covariates, Tmin_GP_short_cum = (Tmin_Apr + Tmin_Mai + Tmin_Jun)) 
names(Yield_Covariates)

# #####################################################################################
# ## Calculate Growth Months of siloMaize: nach Schlenker in den USA March to Aug ##
# ## average
# Yield_Covariates <- transform(Yield_Covariates, SMI_GP_long_av = (SMI_Mar + SMI_Apr + SMI_Mai + SMI_Jun + SMI_Jul + SMI_Aug) / 6)  # gibt an, wie stark die Abweichung vom langfristigen Mittel 
#                                                                                                                           # im Durchschitt ist
# Yield_Covariates <- transform(Yield_Covariates, Prec_GP_long_av = (Prec_Mar + Prec_Apr + Prec_Mai + Prec_Jun + Prec_Jul + Prec_Aug) / 6) 
# Yield_Covariates <- transform(Yield_Covariates, Tavg_GP_long_av = (Tavg_Mar + Tavg_Apr + Tavg_Mai + Tavg_Jun + Tavg_Jul + Tavg_Aug) / 6) 
# Yield_Covariates <- transform(Yield_Covariates, Pet_GP_long_av = (Pet_Mar + Pet_Apr + Pet_Mai + Pet_Jun + Pet_Jul + Pet_Aug) / 6) 
# Yield_Covariates <- transform(Yield_Covariates, Tmax_GP_long_av = (Tmax_Mar + Tmax_Apr + Tmax_Mai + Tmax_Jun + Tmax_Jul + Tmax_Aug) / 6) 
# Yield_Covariates <- transform(Yield_Covariates, Tmin_GP_long_av = (Tmin_Mar + Tmin_Apr + Tmin_Mai + Tmin_Jun + Tmin_Jul + Tmin_Aug ) / 6) 
# names(Yield_Covariates)
# 
# ## cumulative
# # passt eigentlich besser zur agronomischen Theorie
# Yield_Covariates <- transform(Yield_Covariates, SMI_GP_long_cum = (SMI_Mar + SMI_Apr + SMI_Mai + SMI_Jun + SMI_Jul + SMI__Aug)) # gibt die Summe der Abweichungen an. 
# Yield_Covariates <- transform(Yield_Covariates, Prec_GP_long_cum = (Prec_Mar + Prec_Apr + Prec_Mai + Prec_Jun + Prec_Jul + Prec__Aug)) 
# Yield_Covariates <- transform(Yield_Covariates, Tavg_GP_long_cum = (Tavg_Mar + Tavg_Apr + Tavg_Mai + Tavg_Jun + Tavg_Jul + Tavg__Aug)) 
# Yield_Covariates <- transform(Yield_Covariates, Pet_GP_long_cum = (Pet_Mar + Pet_Apr + Pet_Mai + Pet_Jun + Pet_Jul + Pet_Aug)) 
# Yield_Covariates <- transform(Yield_Covariates, Tmax_GP_long_cum = (Tmax_Mar + Tmax_Apr + Tmax_Mai + Tmax_Jun + Tmax_Jul + Tmax_Aug)) 
# Yield_Covariates <- transform(Yield_Covariates, Tmin_GP_long_cum = (Tmin_Mar + Tmin_Apr + Tmin_Mai + Tmin_Jun + Tmin_Jul + Tmin_Aug)) 
# names(Yield_Covariates)

#################
#### Detrend ####

## 
boxplot(siloMaize ~ year, data= Yield_Covariates)
trend <- lm(siloMaize ~ year)

plot(trend ~ year)
abline(trend)
plot(trend)
summary.lm(trend)

plot(siloMaize)
plot(residuals(trend))

names(trend)
fitted.values(trend)

trend
plot(detrend ~ year)
detrend <- detrend(siloMaize, 'linear') 
abline(trend)
summary(detrend)

#################################################
#### Remove log trend of indepedent variable ####
logtrend <- lm(log(siloMaize) ~ log(as.integer(year)), data= Yield_Covariates)
summary(logtrend)
Yield_Covariates$siloMaize_logtrend <- resid(logtrend)



##########################################
#### Define names for guides in plots ####
federalStates <-guide_legend("Federal States")

#############################
#### Make list for loops ####
names(Yield_Covariates)
SMI_month_list<-NULL

SMI_month_list[[1]]<- c("SMI_Jan", "SMI_Feb", "SMI_Mar", "SMI_Apr", "SMI_Mai", "SMI_Jun", "SMI_Jul", "SMI_Aug","SMI_Sep", "SMI_Oct", "SMI_Nov", "SMI_Dec")
SMI_month_list[[2]]<- c("SMI_Sep_lag", "SMI_Oct_lag", "SMI_Nov_lag", "SMI_Dec_lag","SMI_Jan", "SMI_Feb", "SMI_Mar", "SMI_Apr", "SMI_Mai", "SMI_Jun", "SMI_Jul", "SMI_Aug")
SMI_month_list[[3]] <- c("SMI_Yearly_AprToJun" )
SMI_month_list[[4]] <-c("Jan","Feb","Mar","Apr","Mai","June","July","Aug","Sept","Oct","Nov","Dec")
SMI_month_list

###############################################
#### Define data.frame as Panel Data Frame ####
Yield_Covariates_plm <- plm.data(Yield_Covariates, indexes = c("comId","year"))
head(Yield_Covariates)


############################################################################################################################################################################################
#### Melt SMI of all months into one column #### 
## Purpose is faceting according to montly SMI 
names(Yield_Covariates)
idvars = c("comId", "year","comIdState","comState","siloMaize")
measurevars= SMI_month_list[[1]]
Yield_Covariates_melt <- melt (Yield_Covariates, id.vars= idvars, measure.vars = measurevars)

## with lagged variables 
measurevars= SMI_month_list[[2]]
Yield_Covariates_melt_lag <- melt (Yield_Covariates, id.vars= idvars, measure.vars = measurevars)


head(Yield_Covariates_melt)
tail(Yield_Covariates_melt)
table(Yield_Covariates_melt$comState)
head(Yield_Covariates_melt[Yield_Covariates_melt$comId == "1001",])
tail(Yield_Covariates_melt[Yield_Covariates_melt$comId == "1001",])

any(Yield_Covariates_melt[,"value"]!=Yield_Covariates_melt[,"value"], na.rm = T)
all(Yield_Covariates_melt[,"value"] == Yield_Covariates_melt[,"value"], na.rm = T)

###################################################################################
## Create variables, which only consider extreme values of SMI -> create subsets ## 
Yield_Covariates_melt_l01 <- subset( Yield_Covariates_melt,Yield_Covariates_melt$value <= 0.1 )
Yield_Covariates_melt_l02 <- subset( Yield_Covariates_melt,Yield_Covariates_melt$value <= 0.2 )
Yield_Covariates_melt_l03 <- subset( Yield_Covariates_melt,Yield_Covariates_melt$value <= 0.3 )
Yield_Covariates_melt_l04 <- subset( Yield_Covariates_melt,Yield_Covariates_melt$value <= 0.4 )
Yield_Covariates_melt_l05 <- subset( Yield_Covariates_melt,Yield_Covariates_melt$value <= 0.5 )

Yield_Covariates_melt_m05 <- subset( Yield_Covariates_melt,Yield_Covariates_melt$value >= 0.5 )
Yield_Covariates_melt_m06 <- subset( Yield_Covariates_melt,Yield_Covariates_melt$value >= 0.6 )
Yield_Covariates_melt_m07 <- subset( Yield_Covariates_melt,Yield_Covariates_melt$value >= 0.7 )
Yield_Covariates_melt_m08 <- subset( Yield_Covariates_melt,Yield_Covariates_melt$value >= 0.8 )
Yield_Covariates_melt_m09 <- subset( Yield_Covariates_melt,Yield_Covariates_melt$value >= 0.9 )

head(Yield_Covariates_melt_l01)

tail(Yield_Covariates_melt_l01)

## Interpretation ##

''
#############################################################################################################################################################################################
###################
#### Plottting ####
###################
# ## Read Data ##
# Yield_Covariates <- read.csv( "data/data_processed/Yield_Covariates.csv")
# attach(Yield_Covariates)
# names(Yield_Covariates)
##################
#### Montly SMI ##
##################


# ## Boxplot der monatlichen Durschschnitts-SMI pro Jahr (mit lag) mit Jitter der Daten
# SMI_month_list[[1]]
# qq_boxplot_SMImonths_lag<-lapply(SMI_month_list[[1]], function(i)  
#                   ggplot(Yield_Covariates, aes_string("year", i, color="year")) +  
#                   geom_boxplot() + geom_jitter() + 
#                   geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
#                   geom_smooth(se=F, color="red", aes(group=1))+ 
#                   theme_bw() + ggtitle( "Boxplot der monatlichen Durschschnitts-SMI pro Jahr mit Jitter der Daten"))
# 
# qq_boxplot_SMImonths_lag
# 
# ## Boxplot der monatlichen Durschschnitts-SMI pro Jahr mit Jitter der Daten und facets der einzelen Bundesländer
# qq_boxplot_SMImonths_lag_facets  <-lapply(SMI_month_list[[1]], function(i)  
#                    ggplot(Yield_Covariates, aes_string("year", i, color="year")) +  
# #                   geom_boxplot() +
#                    geom_jitter() + 
#                    geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
#                    geom_smooth(se=F, color="red", aes(group=1))+
#                    theme_bw() + 
#                    facet_wrap(~comState) +  
#                   ggtitle( "Boxplot der monatlichen Durschschnitts-SMI pro Jahr mit Jitter der Daten und facets der einzelen Bundesländer"))
# qq_boxplot_SMImonths_lag_facets

## Boxplot jährlicher Durchschnitts SMI (not lagged) mit Jitter der Daten
qq_boxplot_SMI_YearlyAvg  <- ggplot(Yield_Covariates, aes_string("year", "SMI_YearlyAvg", color="year")) +
  #                                       geom_boxplot() + 
  geom_jitter() +
  geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
  geom_smooth(se=F, color="red", aes(group=1))+
  theme_bw() + ggtitle( "Boxplot of yearly average of SMI over time")

qq_boxplot_SMI_YearlyAvg 
ggsave(file="./figures/figures_exploratory/Scatterplot_SMI/SMI_monthAvg_years.pdf", qq_boxplot_SMI_YearlyAvg, width=16, height=8)

## Boxplot jährlicher Durchschnitts SMI (lagged) mit Jitter der Daten
qq_boxplot_SMI_YearlyAvg_lag  <- ggplot(Yield_Covariates, aes_string("year", "SMI_YearlyAvg_lag", color="year")) +
  #                                       geom_boxplot() + 
  geom_jitter() +
  geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
  geom_smooth(se=F, color="red", aes(group=1))+
  theme_bw() + ggtitle( "Boxplot of yearly average of SMI over time")

qq_boxplot_SMI_YearlyAvg 
ggsave(file="./figures/figures_exploratory/Scatterplot_SMI/SMI_monthAvg_years_lag.pdf", qq_boxplot_SMI_YearlyAvg_lag, width=16, height=8)


## Boxplot jährlicher Durchschnitts SMI mit Jitter der Daten und facets der einzelnen Bundesländer
qq_boxplot_SMI_YearlyAvg_facets <- qq_boxplot_SMI_YearlyAvg + facet_wrap(~comState) + ggtitle( "Boxplot jährlicher Durchschnitts SMI mit Jitter der Daten und facets der einzelnen Bundesländer")
qq_boxplot_SMI_YearlyAvg_facets


#########################################
## Boxplots der SMI im Monatsvergleich ##
# hier möchte ich die einzelen Variablen vergleichen, um einen Eindruch zu bekommen, welche Niveau der SMI in einem Monat hat und wie diese relative zueinander stehen

## Make list for indexing in loops for ggplot ##
list <- c( 1  ,  10  ,  12  ,  13  ,  14  ,  15  ,  16   ,  3   ,  4  ,   5  ,   6  ,   7   ,  8   ,  9 )

lapply(list, function(i) unique(Yield_Covariates_melt[Yield_Covariates_melt$comIdState == i,"comState"]))

comState <- c ("Schleswig-Holstein","Schleswig-Holstein","Brandenburg","Mecklenburg-Vorpommern","Sachsen","Sachsen-Anhalt","Thüringen","Niedersachsen","Bremen","NRW","Hessen","Rheinland-Pfalz","Baden-Württemberg","Bayern")

## SMI of months average over years
gg_boxplot_SMI_months <- ggplot(Yield_Covariates_melt, aes(x=variable, y=value)) +
  geom_boxplot()  +
  geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
  geom_smooth(se=F, color="red", aes(group=1)) +
  theme_bw() + ggtitle(paste0("Montly SMi for each month from 1999 to 2010" ))

gg_boxplot_SMI_months

ggsave(gg_boxplot_SMI_months, file=".//figures/figures_exploratory/Scatterplot_SMI/SMImonths_avgOverMonths.pdf", width=32, height=16)


## SMI of months average over years, faceted by years
gg_boxplot_SMI_months_facetYear <- ggplot(Yield_Covariates_melt[Yield_Covariates_melt$comIdState ==  list[1],], aes(x=variable, y=value)) +
  geom_boxplot()  +
  facet_wrap(~year) +
  geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
  geom_smooth(se=F, color="red", aes(group=1)) +
  theme_bw() + ggtitle(paste0("Montly SMi for each month from 1999 to 2010 in ", print(comState[1])))

gg_boxplot_SMI_months_facetYear

gg_boxplot_SMI_months_facetYear_pesiloMaizear <- 
  lapply(1:14, function(i) 
    ggplot(Yield_Covariates_melt[Yield_Covariates_melt$comIdState== list[i],], aes(x=variable, y=value)) +
      geom_boxplot()  +
      facet_wrap(~year) +
      geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
      geom_smooth(se=F, color="red", aes(group=1)) +
      theme_bw() + ggtitle(paste0("Montly SMi for each month from 1999 to 2010 in ", print(comState[i])))
  )

gg_boxplot_SMI_months_facetYear_pesiloMaizear

## Boxplot der SMI nach Monaten unterteilt nach Jahren und Bundesländern - ergibt eine große Matrix##
gg_boxplot_SMI_months_facetComStateYear <- ggplot(Yield_Covariates_melt, aes(x=variable, y=value)) +
  geom_boxplot() +
  facet_wrap(comState ~ year, ncol=12) + 
  geom_smooth(se=F, color="red", aes(group=1)) + 
  labs(x = "time", y = "SMI") +
  theme_bw() 
gg_boxplot_SMI_months_facetComStateYear 


# Die Parameter der Achsenbeschriftung und anderer Titel wurden hier für eine Presentation angepasst.

gg_boxplot_SMI_months_facetComStateYear + theme(strip.text.x = element_text(size = 12 ,hjust = 0.5, vjust = 0.5), strip.background = element_rect(fill = "white"),  axis.title=element_text(size=50 ,face="bold"))

ggsave(gg_boxplot_SMI_months_facetComStateYear, file=".//figures/figures_exploratory/Scatterplot_SMI/SMImonths_facetStateYear.pdf", width=32, height=20)

# gg_violon_SMI_months_comState <- ggplot(Yield_Covariates_melt, aes(x=variable, y=value)) + geom_violin()  + facet_wrap(~comState)
# gg_violon_SMI_months_comState

############################################################################################################################################################################################
#####################################################
## siloMaize and manipulated data of siloMaize ##

## Interessant Fragestellung
# Gibt es einen Trend -> dies kann eigentlich mit Ja beantwortet werden
# Wie wirkt sich die Normalisierung aus -> z- Transformation

names(Yield_Covariates)

#################
## siloMaize ##

## Plot siloMaize pro Jahr mit jitter ##
qq_boxplot_sm  <- ggplot(data=Yield_Covariates,aes(as.factor(year), siloMaize) ) 

qq_boxplot_sm + 
                   geom_boxplot() +
  geom_point(aes(colour=comState))+
  geom_smooth(method = "lm", se=F, color="green3", size=1, aes(group=1)) +
  geom_smooth(se=F, color="red", size=1, aes(group=1))+
  theme_few() 

qq_boxplot_sm
ggsave(qq_boxplot_sm, file=".//figures/figures_exploratory/siloMaize_deviation/siloMaize.pdf", width=32, height=20)

## Interpretation
'
In den Daten ist offensichtlich ein leichter Trend über die Zeit zu erkennen
'


## Boxplot siloMaize pro Jahr mit jitter und facets der einzelnen Bundesländer ##
qq_boxplot_sm_facets<- qq_boxplot_sm + facet_wrap(~comState) + ggtitle( "Boxplot siloMaize per Jahr")
qq_boxplot_sm_facets


#################################
## Normalisiert:scoreWW_Kreise ##

names(Yield_Covariates)
## Plot scoreWW pro Jahr ##
qq_boxplot_scoreWW  <- ggplot(Yield_Covariates, aes_string("year", "siloMaize_deZscore")) +
  #   geom_boxplot() +
  geom_jitter() +
  geom_smooth(method = "lm", se=F, color="blue", size=1, aes(group=1)) +
  geom_smooth( se=F, color="dodgerblue4", size=4, aes(group=1)) +
  labs(x = "Year", y = "Detrended and standardized Silo Maize yield") +
  theme_bw()  +  theme(axis.text= element_text(size = 40 ,hjust = 0.5, vjust = 0.5), axis.title=element_text(size=50 ,face="bold"))

#  gtitle( "Boxplot and TimeSeries z-score of siloMaize per year")
qq_boxplot_scoreWW 
ggsave(qq_boxplot_scoreWW, file=".//figures/figures_exploratory/siloMaize_deviation/scoreWW.pdf", width=32, height=20)

## Interpretation
'
Auch in den z-transformierten Daten kann Trend erkannt werden
'

# ########################################################
# ## Linear Detrend mit der detrend Funktion: detrendWW ##
# ## Plot detrendWW pro Jahr ##
# qq_boxplot_detrendWW  <- ggplot(Yield_Covariates, aes_string("year", "detrendWW")) +
#   #                               geom_boxplot() +
#   geom_jitter() +
#   geom_smooth(method = "lm", se=F, color="green3", size=2, aes(group=1)) +
#   geom_smooth(se=F, color="red", size=2, aes(group=1))+
#   theme_bw() + ggtitle( "Boxplot and Time Series of detrended siloMaize per year")
# qq_boxplot_detrendWW
# ggsave(qq_boxplot_detrendWW, file=".//figures/figures_exploratory/siloMaize_deviation/detrendWW.pdf", width=32, height=20)
# ## Interpretation
# ' There is still a Trend in the data: Do not exactly know why!?! Wahrscheinlich weil nicht alle Daten durch die Detrend Funktion Transformiert wurden
# '

## Plot detrendWW_Kreise pro Jahr ##
qq_boxplot_detrendWW_Kreise <- ggplot(Yield_Covariates, aes_string("year", "detrendWW_Kreise")) +
  #                               geom_boxplot() +
  geom_jitter() +
  geom_smooth(method = "lm", se=F, color="green3", size=2, aes(group=1)) +
  geom_smooth(se=F, color="red", size=2, aes(group=1))+
  theme_bw() + ggtitle( "Boxplot and TimeSeries of detrended siloMaize (for each Kreis) per year")
qq_boxplot_detrendWW_Kreise
ggsave(qq_boxplot_detrendWW_Kreise, file=".//figures/figures_exploratory/siloMaize_deviation/detrendWW_Kreise.pdf", width=32, height=20)
## Interpretation
' No trend in the Data.
'

## Plot siloMaize_deZscore pro Jahr ##
qq_boxplot_sm_deZscore <- ggplot(Yield_Covariates, aes_string("year", "siloMaize_deZscore")) +
  #                               geom_boxplot() +
  geom_jitter() +
  geom_smooth(method = "lm", se=F, color="green3", size=2, aes(group=1)) +
  geom_smooth(se=F, color="red", size=2, aes(group=1))+
  labs(x = "time", y = "Detrended and Standardized Yield (acre/ton) of siloMaize") +
  theme_bw() 
# + ggtitle( "Detrended siloMaize z-score (for each Kreis) per year")
qq_boxplot_sm_deZscore
ggsave(qq_boxplot_sm_deZscore, file=".//figures/figures_exploratory/siloMaize_deviation/siloMaize_deZscore.pdf", width=32, height=20)
ggsave(qq_boxplot_sm_deZscore, file=".//figures/figures_exploratory/siloMaize_deviation/siloMaize_deZscore.png", width=32, height=20)
## Interpretation
' Diese Daten sehen eigentlich auch ganz gut aus, nur ist die Frage, ob wieder Daten bei der detrending nicht berücksichtigt wurden
'

qq_boxplot_sm_deZscore_facets <- qq_boxplot_sm_deZscore + facet_wrap(~comState, nrow=2)
qq_boxplot_sm_deZscore_facets 
ggsave(qq_boxplot_sm_deZscore_facets, file=".//figures/figures_exploratory/siloMaize_deviation/siloMaize_deZscore_facets.pdf", width=20, height=20)
ggsave(qq_boxplot_sm_deZscore_facets, file=".//figures/figures_exploratory/siloMaize_deviation/siloMaize_deZscore_facets.png", width=20, height=20)

# ###################
# #### Histogram ####
# #### Histogramm der SMI der einzelnen Monate ###
# gg_histo_months  <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_Covariates, aes_string(i)) +  geom_histogram(aes(fill=year))+ ggtitle(paste("Histogramm of", i)) + theme_bw())
# gg_histo_months
# 
# ###################
# #### barplots ####
# # ... der SMI Verteilung
# gg_barplot_SMImonths_lag  <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_Covariates, aes_string(i, fill="year")) +  geom_bar(color = "black"))
# gg_barplot_SMImonths_lag 
# 
# 
# #######################
# #### density plots ####
# 
# ## Plots der der monatlichen durchschnittlichen SMI für alle Jahre
# gg_density_SMImonths_lag_alpha  <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_Covariates, aes_string(i, fill="year")) +  geom_density(alpha=0.5))
# gg_density_SMImonths_lag_alpha 


# ######################
# #### Violin Plots ####
# 
# gg_violin_SMImonths_lag_stack  <-lapply(SMI_month_list[[1]][[1]], function(i)  ggplot(Yield_Covariates, aes_string("year",i)) +  geom_violin(position="stack") )
# gg_violin_SMImonths_lag_stack
# 
# gg_violin_SMImonths_lag_prop <-lapply(SMI_month_list[[1]][[2]], function(i)  ggplot(Yield_Covariates, aes_string("year",i)) + geom_violin(position="fill") )
# gg_violin_SMImonths_lag_prop
# 
# gg_violin_SMImonths_lag_absolut <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_Covariates, aes_string("year",i)) +  geom_violin())
# gg_violin_SMImonths_lag_absolut

######################################
#### Scatterplots oder xy - Plots ####
'Diese Plots waren unten bei der Regressionsanalyse, nochmals überprüfen, was damit zu tun ist'

## Scatterplots des Zusammenhanges siloMaize und SMI_MaiToJuni, aufgegliedert nach Jahren: linearer Zusammenhang
qp_Yearly_MaiToJul_lin<-qplot(SMI_Yearly_MaiToJul, siloMaize, data=Yield_Covariates, facets=~year, color=year,geom=c("point", "smooth"), method="lm")+ ylab("Yearly Silo Maize Yield in dt/ha") + ylab("Yearly Silo Maize Yield in dt/ha") + ggtitle("siloMaize, Linear, SMI_Yearly_MaiToJul")
qp_Yearly_MaiToJul_lin
summary(qp_Yearly_MaiToJul_lin)

## Scatterplots des Zusammenhanges siloMaize und SMI_MaiToJuni, aufgegliedert nach Jahren: polygoner Zusammenhang
qp_Yearly_MaiToJul_pol<-qplot(SMI_Yearly_MaiToJul, siloMaize, data=Yield_Covariates, facets=~year, color=factor(year),geom=c("point", "smooth"),shape=".")+ ylab("Yearly Silo Maize Yield in dt/ha") + ylab("Yearly Silo Maize Yield in dt/ha") + ggtitle("siloMaize, Pol, SMI_Yearly_MaiToJul")
qp_Yearly_MaiToJul_pol
summary(qp_Yearly_MaiToJul_pol)



# #### plotmeans ####
# plotmeans(siloMaize ~ comId, Yield_Covariates)
# plotmeans(siloMaize ~ comState, Yield_Covariates)
# plotmeans(siloMaize ~ year, Yield_Covariates)

## Interpretation ##
'
Die Scatterplots scheinen schon in den einzelnen Bundesländer eine Heterogenität bei den yields darzustellen.

Diese Heterogenität wird durch plotmeans nocheinmal bestätigt, sowohl auf Bundesländer als auch County Ebene.

Diese Heterogenität wird bei normalen OLS nicht berücksichtigt.
'

#########################################
## xyplots: siloMaize auf montly SMI ##
#########################################
##############################################
## Linear und Pooling: nur eine fitted line ##
gg_lin_months  <-lapply(SMI_month_list[[1]] , function(i)  ggplot(Yield_Covariates, aes_string(x=i,y="siloMaize")) +  stat_smooth(method="lm", se=F ) + geom_point(shape = 5, size = 0.5)          
                        + ylab ("Per acre yield of siloMaize") + ggtitle( "xy-Plot") + theme_bw() )
gg_lin_months 

###########################################################
## Linear mit unterschiedlichen fitted lines nach Jahren ##
gg_lin_months  <-lapply(SMI_month_list[[1]] , function(i)  ggplot(Yield_Covariates, aes_string(x=i,y="siloMaize", color="year")) +  stat_smooth(method="lm", se=F ) + geom_point(shape = 5, size = 0.5)          
                        + ylab ("Per acre yield of siloMaize") + ggtitle( "xy-Plot") + theme_bw() )
gg_lin_months 

# Interpretation
'
Die Coefficienten varieren. Am Anfang des Jahres sind sie negativ, danach werden sie tendenziell linear.

'


## Polygons
gg_pol_months_yearColor <- lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_Covariates, aes_string(x=i,y="siloMaize", color="year")) +  stat_smooth(se=F) + geom_point(shape = 5, size = 0.5)          
                                  + ylab ("Per acre yield of siloMaize")  + ggtitle( "xy-Plot") + theme_bw() )
gg_pol_months_yearColor

## Polygons
gg_pol_months <- lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_Covariates, aes_string(x=i,y="siloMaize")) +  stat_smooth(se=F) + geom_point(shape = 5, size = 0.5)          
                        + ylab ("Per acre yield of siloMaize")  + ggtitle( "xy-Plot") + theme_bw() )
gg_pol_months



### Darstellung mit Facets ###
## Linear line
gg_lin_months_facetCom  <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_Covariates, aes_string(x=i,y="siloMaize")) +  stat_smooth(method="lm", se=F ) + geom_point(shape = 5, size = 0.5)          
                                 + ylab ("Per acre yield of siloMaize")  + ggtitle( "xy-Plot") + theme_bw() + facet_wrap(~comState)) 
gg_lin_months_facetCom[1]

gg_lin_months_facetYear  <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_Covariates, aes_string(x=i,y="siloMaize", color="comState")) +  stat_smooth(method="lm", se=F ) + geom_point(shape = 5, size = 0.5)          
                                  + ylab ("Per acre yield of siloMaize")  + ggtitle( "xy-Plot") + theme_bw() + facet_wrap(~year)) 
gg_lin_months_facetYear[1]

## Polygons

gg_pol_months_facetCom <- lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_Covariates, aes_string(x=i,y="siloMaize")) +  stat_smooth(se=F) + geom_point(shape = 5, size = 0.5)          
                                 + ylab ("Per acre yield of siloMaize")  + ggtitle( "xy-Plot") + theme_bw() + facet_wrap(~comState))
gg_pol_months_facetCom[1]

gg_pol_months_facetYear <- lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_Covariates, aes_string(x=i,y="siloMaize", color="year")) +  stat_smooth( se=F) + geom_point(shape = 5, size = 0.5)          
                                  + ylab ("Per acre yield of siloMaize")  + ggtitle( "xy-Plot") + theme_bw() + facet_wrap(~year))
gg_pol_months_facetYear

########################################################
## Scatterplot siloMaize and SMI faceted by months ##

## Attention ## 
'Here I need to work with facets, because otherwise I have 12 data points on SMI for one Datapoint of Yield. For two dimensional plots, this is not a proper approach.'

#### siloMaize, No lag SMI ####
## facet, nonlinear
names(Yield_Covariates_melt)
ggplot_pol_SMI_facetMonths<- ggplot(Yield_Covariates_melt, aes(x=value, y=siloMaize)) + 
  stat_smooth(color="red",size = 2, se=T) +
  geom_point(shape = 5, size = 0.5) +
  facet_wrap(~variable)+ ylab ("Per acre yield of siloMaize")  +
  ggtitle("Scatterplot of siloMaize and SMI") + theme_bw()
ggplot_pol_SMI_facetMonths
# Interpretation: maybe using quadratics might help 

## facet, linear
ggplot_lin_SMI_facetMonths<- ggplot(Yield_Covariates_melt, aes(x=value, y=siloMaize)) + 
  stat_smooth(method="lm", color="red",size = 2, se=T) +
  geom_point(shape = 5, size = 0.5) +
  facet_wrap(~variable)+ ylab ("Per acre yield of siloMaize")  +
  ggtitle( "xy-Plot") + theme_bw()
ggplot_lin_SMI_facetMonths

# #############
# ## Subsets ##
# 
# ## Subset: only values less then 0.1 
# ## Scatterplot siloMaize and SMI faceted by months ##
# ## facet, non linear
# names(Yield_Covariates_melt)
# ggplot_pol_SMI_facetMonths_l01<- ggplot(Yield_Covariates_melt_l01, aes(x=value, y=siloMaize)) + 
#   stat_smooth(color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   facet_wrap(~variable)+ ylab ("Per acre yield of siloMaize")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_pol_SMI_facetMonths_l01
# 
# ## facet, linear
# names(Yield_Covariates_melt)
# ggplot_lin_SMI_facetMonths_l01<- ggplot(Yield_Covariates_melt_l01, aes(x=value, y=siloMaize)) + 
#   stat_smooth(method="lm",color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   facet_wrap(~variable)+ ylab ("Per acre yield of siloMaize")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_lin_SMI_facetMonths_l01
# 
# ## nofacet, linear
# ggplot_lin_SMI_l01 <- ggplot(Yield_Covariates_melt_l01, aes(x=(value), y=siloMaize)) + 
#   stat_smooth(method="lm",color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   ylab ("Per acre yield of siloMaize")  +
#   xlab ("SMI") +
#   ggtitle( "Scatterplot of SMI less 0.1 and siloMaize Yield") + theme_bw()
# ggplot_lin_SMI_l01
# ggsave(file="./figures/figures_exploratory/Scatterplot_Yield_SMI/Linear/l01.pdf", ggplot_lin_SMI_l01, width=16, height=12)
# 
# ## no facet, nonlinear
# ggplot_pol_SMI_l01 <- ggplot(Yield_Covariates_melt_l01, aes(x=(value), y=siloMaize)) + 
#   stat_smooth(color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   ylab ("Per acre yield of siloMaize")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_pol_SMI_l01
# 
# ## Subset: only values greater than 0.9 
# ## Scatterplot siloMaize and SMI faceted by months ##
# ## facet, non linear
# names(Yield_Covariates_melt)
# ggplot_pol_SMI_facetMonths_m09<- ggplot(Yield_Covariates_melt_m09, aes(x=value, y=siloMaize)) + 
#   stat_smooth(color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   facet_wrap(~variable)+ ylab ("Per acre yield of siloMaize")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_pol_SMI_facetMonths_m09
# 
# ## facet, linear
# names(Yield_Covariates_melt)
# ggplot_lin_SMI_facetMonths_m09<- ggplot(Yield_Covariates_melt_m09, aes(x=value, y=siloMaize)) + 
#   stat_smooth(method="lm",color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   facet_wrap(~variable)+ ylab ("Per acre yield of siloMaize")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_lin_SMI_facetMonths_m09
# 
# 
# 
# ## no facet, non linear
# ggplot_pol_SMI_m09 <- ggplot(Yield_Covariates_melt_m09, aes(x=(value), y=siloMaize)) + 
#   stat_smooth(color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   ylab ("Per acre yield of siloMaize")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_pol_SMI_m09
# 
# ## no facet, linear
# ggplot_lin_SMI_m09 <- ggplot(Yield_Covariates_melt_m09, aes(x=(value), y=siloMaize)) + 
#   stat_smooth(method="lm",color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   ylab ("Per acre yield of siloMaize")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_lin_SMI_m09
# 
# 
#  
#####################################################################
## Scatterplot standscore of siloMaize and SMI faceted by months ##
## non lag
names(Yield_Covariates_melt)
ggplot_pol_SMI_facetMonths<- ggplot(Yield_Covariates_melt, aes(x=value, y=siloMaize_deZscore)) + 
  stat_smooth(color="red",size = 2, se=T) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3),color="green",size = 2, se=T) +
  geom_point(shape = 5, size = 0.5) +
  facet_wrap(~variable)+ ylab ("Per acre yield of siloMaize")  +
  ggtitle("Scatterplot of siloMaize and SMI") + theme_bw()
ggplot_pol_SMI_facetMonths

## lagged
names(Yield_Covariates_melt)
ggplot_pol_SMI_facetMonths_lag<- ggplot(Yield_Covariates_melt_lag, aes(x=value, y=siloMaize)) + 
  geom_point(size = 0.4, alpha = 5/10) +
  stat_smooth(color="yellow",size = 1, se=T, alpha = 8/10) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw=TRUE),color="darkblue",size = 1.5, se=T) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3, raw=TRUE),color="red",size = 1, se=T, alpha = 8/10) +
  facet_wrap(~variable)+ 
  ylab ("Detrended and Standardized Yield of siloMaize")  + 
  xlab("SMI") +
  #   ggtitle("Scatterplot of siloMaize and SMI") + 
  theme_bw() +
  theme(strip.text.x = element_text(size = 15 ,hjust = 0.5, vjust = 0.5), strip.background = element_rect(fill = "white"),  axis.title=element_text(size=20 ,face="bold"))

ggplot_pol_SMI_facetMonths_lag 

ggsave(ggplot_pol_SMI_facetMonths_lag, file=".//figures/figures_exploratory/Scatterplot_Yield_SMI/ggplot_pol_SMI_facetMonths_lag.pdf", width=18, height=12)
ggsave(ggplot_pol_SMI_facetMonths_lag, file=".//figures/figures_exploratory/Scatterplot_Yield_SMI/ggplot_pol_SMI_facetMonths_lag.png", width=20, height=15)




# ####################################################
# #### Yearly average, Spatial Aggregation ComIDs ####
# 
# 
# ## Explore new data
# head(Yield_Covariates)
# 
# ## Plot ##
# qp_YearlyAvg_lin<-qplot(SMI_YearlyAvg, siloMaize, data=Yield_Covariates, facets=~year, color=factor(year),geom=c("point", "smooth"), method="lm",shape=".")+ ylab("Yearly Silo Maize Yield in dt/ha") + ggtitle("siloMaize, Linear, SMI_YearlyAvg")
# summary(qp_Aug_lin)
# 
# qp_YearlyAvg_pol<-qplot(SMI_YearlyAvg, siloMaize, data=Yield_Covariates, facets=~year, color=factor(year),geom=c("point", "smooth"),shape=".")+ ylab("Yearly Silo Maize Yield in dt/ha") + ggtitle("siloMaize, Pol, SMI_Yearly_Avg")
# summary(qp_Aug_lin)
# 
# 
# #### Plot the time series of each state ####
# qp_timeSeries_comIdState <-qplot(SMI_YearlyAvg, siloMaize, data=Yield_Covariates, facets=~comIdState,  geom=c("point", "smooth"), method="lm",shape=".", se=TRUE)+ ylab("Yearly Silo Maize Yield in dt/ha") + ggtitle("siloMaize, Linear, Jan")
# 
# qp_timeSeries_comIdState <-qplot(SMI_Jan, year, data=Yield_Covariates, facets=~comIdState,  geom=c("point", "smooth"), method="lm",shape=".", se=TRUE) + ylab("Yearly Silo Maize Yield in dt/ha") + ggtitle("siloMaize, Linear, Jan")
# 
# xyplot(SMI_YearlyAvg ~ year | comIdState,  data=Yield_Covariates, type="l")
# 
# ## Nach diesem Plot hier scheinen Werte zu fehlen für SMI
# ## Nein, dass ich nicht der Grund, sondern dass auch die Endpunkte miteiandner verbunden werden
# 
# ## Time Series Plot der ComIdState==4, also Bremen und Bremerhaven ##
# ## Split
# Yield_Covariates_4<-Yield_Covariates[comIdState==4,]
# head(Yield_Covariates_4)
# ## Apply ##
# ## plot
# plot(Yield_Covariates_4$SMI_YearlyAvg, type="l")
# Yield_Covariates_4_dt<-pdata.frame(Yield_Covariates_4, index=c("year", "comId"))
# 
# 
# xyplot(SMI_YearlyAvg~year |comIdState, data= Yield_Covariates_4_dt, type = "l")


###################################################
#### Growth Months of siloMaize: Mai to July ####
###################################################
'
- detailed information needed

'



############################################################################################################################################################################################
################################################################################ Panel Data Approaches #####################################################################################
############################################################################################################################################################################################

## Read data frame with siloMaize as only depedent variable ##
Yield_Covariates <- read.csv( "data//data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodemean_nozscore_sm.csv", row.names=NULL)
Yield_Covariates$X <- NULL

str(Yield_Covariates)

## Na-omit ##
sum(is.na(Yield_Covariates$siloMaize) )
Yield_Covariates_nna <- na.omit(Yield_Covariates) 
dim(Yield_Covariates_nna)
dim(Yield_Covariates)
4920-3990
930-893-48
' The difference is rational, because there are around 893 missing siloMaize values and 48 SMI, which are not necessarily orthogonal.  '

class(Yield_Covariates_nna)
names(Yield_Covariates_nna)

#######################################################
#### Define data.frame as Panel Data Frame, no NAs ####
Yield_Covariates_nna_plm <- pdata.frame(Yield_Covariates_nna, index = c("comId","year"))
class(Yield_Covariates_nna_plm)
class(Yield_Covariates_nna_plm$siloMaize)
names(Yield_Covariates_nna_plm)
str(Yield_Covariates_nna_plm)
str(Yield_Covariates_nna_plm$siloMaize)
summary(Yield_Covariates_nna_plm)

## Attach Data Set ##
attach(Yield_Covariates_nna_plm)
any(is.na(Yield_Covariates_nna_plm))

## Scale Data Set to z-score ##
drops <- (c("year", "comId", "com" , "comIdState","comState", "area100ha", "SHAPE_AREA" ))
Yield_Covariates_scale <- scale(Yield_Covariates_nna[, !names(Yield_Covariates_nna) %in%  drops],center = TRUE, scale = TRUE)
head(Yield_Covariates_scale)
Yield_Covariates_scale <- cbind(Yield_Covariates_nna[, names(Yield_Covariates_nna) %in%  drops],Yield_Covariates_scale )
any(is.na(Yield_Covariates_scale))
class(Yield_Covariates_scale)
Yield_Covariates_scale_plm <- pdata.frame(Yield_Covariates_scale, index = c("comId","year"))

################################
### OLS Regression: Pooling ####
################################
#### Time: all years; Spatial: all communities ####

#######################################################################
## Grundsätzliche Überlegungen, welche Variationen ich hier Versuche ##
'
- FE vs RE
- short vs long Growing Period -> erstmal kurz
- Polynom vs Stepwise vs Spline vs Smooth (dabei sollte ich überlegen, wass in Panel Ansatz überhaupt umzusetzen ist, oder wie man die Panelansätze in anderen Paketen umsetzt)
- Durch Multicollinearität aufgrund von Polygonen kommt es zu Problemem bei RE plm Modellierung. 
- bisher beste Ergebnisse mit Polygonen dritten Grades
- Ich sollte mir wohl einen anderen Weg überlegen, wie ich für non-linearity kontrolliere. Dazu mehr bei Schlenker. Eine kumulative aggregation macht evtl Sinn.
- ln/z-score vs non normalized (Schlenker benutzt log yield - log trend ). Ersteres hat neben normalisierung Einfluss 
- kumulative vs average (Schlenker benutzt in meinem Verständnis kumulierte Größen, da der Einfluss kumulativ additiv ist)
- Multicollinearität: PET und Tavg sind hoch kollinear, aber das führt nicht zu Varianz Inflation
- time trend: 
- year als factor oder numerisch? Als Faktor ist wohl ähnlich eines dummies, trend ist aber nicht der dummy. 
- in CCSF time series werden mehrer 
- Auch heteroskedasdiscity muss wohl berücksichtigt werden -> Breusch Pagan Test 
- FE: plm methode vs demeaning in OLS: Mann muss bei demeaning Ansatz die Degrees of Freedom korrigieren, das passiert bei plm automatisch
'

###############
## Strategie ##
'
Die Growth Period lasse ich erstmal auf April Mai Juni. Diese kann ich für eine Sensibilitätsanalyse dann eventuell noch ändern. 
Ich nehme vorerst die nicht veränderte Version von Yield, da log Transformation im saturierten Model mit short GP, lm, keine Hetereosedasdicity reduziert hat und
z-score das kleinste R² hat.
Scheinbar erhöhen Polygone höheren Grades das R², also den Anteil der erklärten Varianz. Deshalb sollte ich dies Polygone mal darstellen, um zu schauen, ob es 
vor allem die Extreme sind, welche einen Einfluss haben.
Das ist ja auch Roberts und Schlenker der Fall. 
Dann sollte ich mir Gedanken machen, wie ich diese nicht linearität umsetzen kann, so dass diese auch im plm Kontext angewendet werden kann. 
Eventuell muss man dann aus dem panel Kontext raus und dann mehr in the spatio temporal schiene gehen. 
'
##################################
## Favorisiertes Modell, bisher ##


##################################
## Variablen: SMI Growth Period ##

## Modell Estimation ##
names(Yield_Covariates_nna_plm)
summary(log(siloMaize))

## Pooled: Basic linear model, short, non poly, non transform, average ##
Pool_SMI_GP_short <-lm(formula = log(siloMaize) ~ SMI_GP_short_av)

## Pooled: Basic linear model, long, non poly, non transform, average ##
Pool_SMI_GP_long <-lm(formula = log(siloMaize) ~ SMI_GP_long_av)

## Diagnostics
summary(Pool_SMI_GP_short$model)
summary(Pool_SMI_GP_short)
names(Pool_SMI_GP_short) # zeigt, welche anderen Informationen im Model vorhanden sind

summary(Pool_SMI_GP_long)
# Diagnostic Plot
plot(Pool_SMI_GP_short)


# Residuals: 
plot(Pool_SMI_GP_short$residuals) # die daten 
plot(residuals(Pool_SMI_GP_short), type="l") # 


## Prediction Function with uncertainity intervalls ##
predict(Pool_SMI_GP_short, data.frame(SMI_GP_short_av = (c(0.2, 0.5, 0.8))), interval = "confidence") # produced confidenve intervalls: 95% Intervalls of the form contain the true value
# quantifies the uncertainity surrounding the average wheat yield over all SMI values
predict(Pool_SMI_GP_short, data.frame(SMI_GP_short_av = (c(0.2, 0.5, 0.8))), interval = "predict") # prduced prediction intervalls for log(siloMaize predictions for given SMI_GP_short_av)
# quantifies uncertainity for a particular value of SMI

## Plot ##
plot(log(siloMaize) ~ SMI_GP_short_av)
plot(log(siloMaize) ~ SMI_GP_short_av)
plot(siloMaize_deZscore ~ SMI_GP_short_av)

abline(Pool_SMI_GP_short)

# Interpretation
'
Hier sind definitv noch Muster sichtbar.
'

## Polygon 3. Grades ##
Pool_SMI_GP_short_poly <-lm(formula = log(siloMaize) ~ poly(SMI_GP_short_av, 3, raw=T))
Pool_SMI_GP_long_poly <-lm(formula = log(siloMaize) ~ poly(SMI_GP_long_av, 3, raw=T))
# Diagnostics
summary(Pool_SMI_GP_short_poly$model)
summary(Pool_SMI_GP_short_poly)
summary(Pool_SMI_GP_long_poly)
plot(Pool_SMI_GP_short_poly)
anova(Pool_SMI_GP_short_poly)

plot(log(siloMaize) ~ SMI_GP_short_av)
abline(Pool_SMI_GP_short_poly)

#####################
#### plm package ####
#####################

## Fixed Effects, basic Model ##
FE_SMI_GP_short <-plm(formula = siloMaize ~ SMI_GP_short_av, data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="within")

# Diagnostics
summary(FE_SMI_GP_short$model)
summary(FE_SMI_GP_short)
residuals()
plot(residuals(FE_SMI_GP_short), type="l")

## Fixed Effects, poly2, basic Model ##
FE_SMI_GP_short_poly <-plm(formula = siloMaize ~ poly(SMI_GP_short_av, 2, raw=T), data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="within")

# Diagnostics
summary(FE_SMI_GP_short_poly$model)
summary(FE_SMI_GP_short_poly)


## Random Effects, basic Model ##
RE_SMI_GP_short_re <-plm(formula = log(siloMaize) ~ SMI_GP_short_av, data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="random")

# Diagnostics
summary(OLS_SMI_GP_short_re$model)
summary(RE_SMI_GP_short_re)
plot(residuals(OLS_SMI_GP_short_re), type="l")

## Random Effects, poly, basic Model ##
RE_SMI_GP_short_poly_re <-plm(formula = log(siloMaize) ~ poly(SMI_GP_short_av, 2, raw=T), data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="random")

# Diagnostics
summary(OLS_SMI_GP_short_poly_re$model)
summary(RE_SMI_GP_short_poly_re)





############################################################################################################################################################################################
##################
#### Testing #####
##################
'
Tests for serial autocorrealtion and heteroskefasdiscitry: ->fgls approach
'

################################################
#### Fixed or Random Effects: Hausmann Test ####

phtest <- phtest(OLS_SMI_GP_short, OLS_SMI_GP_short_re)

'
Der Hausmann Test besagt, dass die Null, nämlich das Model ist Random Effects, nicht abgelehnt werden kann.
'

############################################################################
#### Testing for random effects: Breusch-Pagan Lagrange multiplier (LM) ####

plmtest(OLS_SMI_GP_short_re, type=c("bp"))

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
'
Here we can reject the null, that there is no panel effect. Therefore random effects is the preferred model.
'

######################################################################################################################################################################################
############################################################### Model without SMI ########################################################################################################
names(Yield_Covariates_nna_plm)

## Fixed Effects, basic Model ##
FE_nonSMI_GP_short <-plm(formula = log(siloMaize) ~ Prec_GP_short_av + Tavg_GP_short_av + Pet_GP_short_av,  data = Yield_Covariates_nna_plm, index=c("comId", "year"), model = "within", effect = "twoways")

# Diagnostics
summary(FE_nonSMI_GP_short$model)
summary(FE_nonSMI_GP_short)
names(FE_nonSMI_GP_short)
plot(residuals(FE_nonSMI_GP_short))


## Fixed Effects, poly2, basic Model ##
FE_nonSMI_poly_GP_short <-plm(formula = log(siloMaize) ~ poly(Prec_GP_short_av, 2, raw=T) + poly(Tavg_GP_short_av, 2, raw=T) + poly(Pet_GP_short_av , 2, raw=T), data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="within")

# Diagnostics
summary(OLS_nonSMI_poly_GP_short$model)
summary(OLS_nonSMI_poly_GP_short)


## Random Effects, basic Model ##
RE_nonSMI_GP_short <-plm(formula = log(siloMaize) ~ Prec_GP_short_av + Tavg_GP_short_av + Pet_GP_short_av , data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="random")

# Diagnostics
summary(RE_nonSMI_GP_short$model)
summary(RE_nonSMI_GP_short)


## Random Effects, poly2, basic Model ##
RE_SMI_GP_short_poly <-plm(formula = log(siloMaize) ~ poly(Prec_GP_short_av, 2, raw=T), data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="random")

# Diagnostics
summary(OLS_SMI_GP_short_poly$model)
summary(RE_SMI_GP_short_poly)


############################################################################################################################################################################################
##################
#### Testing #####
##################
'
Tests for serial autocorrealtion and heteroskefasdiscitry: ->fgls approach
'

################################################
#### Fixed or Random Effects: Hausmann Test ####

phtest <- phtest(OLS_nonSMI_GP_short, OLS_nonSMI_GP_short_re)

'
Der Hausmann Test besagt, dass die Null, nämlich das Model ist Random Effects, nicht abgelehnt werden kann.
Daher macht wohl random effects mehr Sinn

'

############################################################################
#### Testing for random effects: Breusch-Pagan Lagrange multiplier (LM) ####

plmtest(OLS_SMI_GP_short_re, type=c("bp"))

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
'
Here we can reject the null, that there is no panel effect. Therefore random effects is the preferred model.
'

#############

######################################################################################################################################################################################
############################################################### Model with SMI, Prec, Tavg, Pet ######################################################################################
######################################################################################################################################################################################
names(Yield_Covariates_nna_plm)

##############################################
#### Transformation of dependent Variable ####
##############################################

############################
## Pooled model saturated ##
Pool_TavgPrecPetSmi_GP_short <-lm(formula = siloMaize ~ Tavg_GP_short_av + Prec_GP_short_av + Pet_GP_short_av +  SMI_GP_short_av,  data = Yield_Covariates_nna_plm)

## Diagnostics
summary(Pool_TavgPrecPetSmi_GP_short)
plot(Pool_TavgPrecPetSmi_GP_short)

## Heteroskedasdicity
bptest(Pool_TavgPrecPetSmi_GP_short)
# Interpretation
'NULL: homoscedasdicity can be rejected '

#################################
## Pooled model saturated, log ##
Pool_TavgPrecPetSmi_GP_short_log <-lm(formula = log(siloMaize) ~ Tavg_GP_short_av + Prec_GP_short_av + Pet_GP_short_av +  SMI_GP_short_av,  data = Yield_Covariates_nna_plm)

## Diagnostics
summary(Pool_TavgPrecPetSmi_GP_short_log)

## Heteroskedasdicity
bptest(Pool_TavgPrecPetSmi_GP_short_log)
ncvTest(Pool_TavgPrecPetSmi_GP_short_log)
'NULL: homoscedasdicity can be rejected '

#####################################
## Pooled model saturated, zscore ##
Pool_TavgPrecPetSmi_GP_short_zscore <-lm(formula = siloMaize_deZscore ~ Tavg_GP_short_av + Prec_GP_short_av + Pet_GP_short_av +  SMI_GP_short_av,  data = Yield_Covariates_nna_plm)

## Diagnostics
summary(Pool_TavgPrecPetSmi_GP_short_zscore)

## Heteroskedasdicity
bptest(Pool_TavgPrecPetSmi_GP_short_zscore)
ncvTest(Pool_TavgPrecPetSmi_GP_short_zscore)
# Interpretation
'NULL BP: the error variacnes are all equal vs Alternative :the error variances are a multiplicative function of one or more variables
Here: reject the NULL'


###################################################################################
## Compare Effects of non-transformation with log and zscore detrend information ##
'NULL BP: the error variacnes are all equal vs Alternative :the error variances are a multiplicative function of one or more variables
Here: reject the NULL, i.e heteroskedasdicity
R² ist bei zscore am kleinsten. logTransformation kann Heteroskedasdicity nicht auflösen. Daher nehme ich die nicht veränderte Version von Yield. 
'


######################
## Mutlicollinearty ##
######################

## Variance Inflation Faktor 
'The ration of the variance of the coefficients considered when fitting the full model divided by the variance of the coefficient when fit on its own'
vif(Pool_TavgPrecPetSmi_GP_short) 
# Interpretation
'Alle Werte kleiner fünf sollten eigentlich kein Problem darstellen. Das ist hier der Fall.'
## Squared Variance Inflation Faktor 
sqrt(vif(Pool_TavgPrecPetSmi_GP_short)) 

## Correlation Matrix 
r <- names(Yield_Covariates_nna_plm)
rr <- grep("*GP_short_av", r)
Yield_Covariates_GP_short <- Yield_Covariates_nna_plm[,rr]
dim(Yield_Covariates_GP_short)

M <- cor(Yield_Covariates_GP_short)
corrplot(M, method = "number")


## Interpretation ##
'Pet und Temperaturen korrelieren sehr stark (0.85), sowie die Temperaturmaße untereinander (0.84 - 0.97).
Es ergibt sich ein anderes, gegenteiliges Bild als bei der VIF Betrachtung. 
SMI korreliert leicht stärkt mit PET.'

# Non constant error variance test - heteroskedasdictiy

## Pooling, poly2, basic Model ##
Pool_TavgPrecPetSmi_poly_GP_short <-lm(formula = siloMaize ~ poly(Prec_GP_short_av, 2, raw=T) +  poly(Tavg_GP_short_av, 2, raw=T) + poly(Pet_GP_short_av , 2, raw=T) + poly(SMI_GP_short_av, 2, raw=T),  data = Yield_Covariates_nna_plm)

# Diagnostics
summary(Pool_TavgPrecPetSmi_poly_GP_short)

# Multicollinearity
sqrt(vif(Pool_TavgPrecPetSmi_poly_GP_short)) # scheint auch keine Problem der Multicollinearity zu geben.
vif(Pool_TavgPrecPetSmi_poly_GP_short)


##################################
#### Mutliple Model Comparison  ##
## Type2 ANOVA
drop1(Pool_TavgPrecPetSmi_poly_GP_short,~.,test="F")

##
step(Pool_TavgPrecPetSmi_poly_GP_short)

# Interpretation
'
Beide Model Selection Algorithmen nehmen das saturierte Modell, löschen aber den Intercept.
Das spricht sehr für ein Fixed Effects oder RE Model, da es hier auch keine singulären Intercept gibt, da dieser durch time - demeaning gelöscht wird. 

'

#####################################################
## Fixed Effects, saturated Model, linear, zscore  ##
FE_TavgPrecPetSmi_GP_short <-plm(formula = siloMaize_deZscore ~ Prec_GP_short_av + Tavg_GP_short_av + Pet_GP_short_av + SMI_GP_short_av,  data = Yield_Covariates_nna_plm, index=c("comId", "year"), model = "within", effect = "twoways")

# Diagnostics
summary(FE_TavgPrecPetSmi_GP_short)

########################################################################
## Fixed Effects, poly2, basic Model (no trend, no time fixed effect) ##
FE_TavgPrecPetSmi_poly_GP_short <-plm(formula = siloMaize ~ poly(Pet_GP_short_av, 2, raw=T) +    poly(Prec_GP_short_av, 2, raw=T) +  poly(Tavg_GP_short_av, 1, raw=T) + poly(SMI_GP_short_av, 3, raw=T) , data = Yield_Covariates_nna_plm, , index=c("comId", "year"), model="within", effect = "individual")
# Diagnostics
summary(FE_TavgPrecPetSmi_poly_GP_short)

coeftest(FE_TavgPrecPetSmi_poly_GP_short, vcov = pvcovHC) # Inference with var covar matrix which is robust to heteroskedasdicity
coeftest(FE_TavgPrecPetSmi_poly_GP_short, vcovHC(FE_TavgPrecPetSmi_poly_GP_short, nethod = "arrellano")) # Inference with var covar matrix which is robust to heteroskedasdicity and autoocorrelatoin

plot(residuals(FE_TavgPrecPetSmi_poly_GP_short))
plot(FE_TavgPrecPetSmi_poly_GP_short ) 
vif(FE_TavgPrecPetSmi_poly_GP_short) 

summary(Yield_Covariates_GP_short)


#####################################
## Fixed Effects, poly2, mit trend ##
FE_TavgPrecPetSmi_poly_GP_short_trend <-plm(formula = siloMaize ~ poly(Pet_GP_short_av, 2, raw=T) +    poly(Prec_GP_short_av, 2, raw=T) +  poly(Tavg_GP_short_av, 1, raw=T) + poly(SMI_GP_short_av, 3, raw=T)  + poly(as.numeric(year),1, raw=T) , data = Yield_Covariates_nna_plm, , index=c("comId", "year"), model="within", effect = "individual")
## Diagnostics
summary(FE_TavgPrecPetSmi_poly_GP_short_trend)

# Heterokedasdicity robust Inference
coeftest(FE_TavgPrecPetSmi_poly_GP_short_trend, vcov = pvcovHC) # Inference with var covar matrix which is robust to heteroskedasdicity
coeftest(FE_TavgPrecPetSmi_poly_GP_short_trend, vcovHC(FE_TavgPrecPetSmi_poly_GP_short_trend, nethod = "arrellano")) # Inference with var covar matrix which is robust to heteroskedasdicity and autoocorrelatoin

# Ressidual Plot
plot(residuals(FE_TavgPrecPetSmi_poly_GP_short_trend))

plot(residuals(FE_TavgPrecPetSmi_poly_GP_short_trend) ~ siloMaize - residuals(FE_TavgPrecPetSmi_poly_GP_short_trend) )

vif(FE_TavgPrecPetSmi_poly_GP_short) 

# Plot Curve of SMI
x <- seq(0,1,length=100)
y <- function(x) 72 + coefficients(FE_TavgPrecPetSmi_poly_GP_short_trend)[[6]]*x + coefficients(FE_TavgPrecPetSmi_poly_GP_short_trend)[[7]]* x^2 + coefficients(FE_TavgPrecPetSmi_poly_GP_short_trend)[[8]]*x^3
curve(y, 0, 1)

##########################################
## Fixed Effects, poly2, mit time dummy ##
FE_TavgPrecPetSmi_poly_GP_short_time <-plm(formula = siloMaize ~  poly(Pet_GP_short_av, 2, raw=T) +   poly(Prec_GP_short_av, 2, raw=T) +  poly(Tavg_GP_short_av, 1, raw=T) + poly(SMI_GP_short_av, 2, raw=T)  + year , data = Yield_Covariates_nna_plm, , index=c("comId", "year"), model="within", effect = "individual")
## Diagnostics
summary(FE_TavgPrecPetSmi_poly_GP_short_time)
names(FE_TavgPrecPetSmi_poly_GP_short_time)

# Heterokedasdicity robust Inference
coeftest(FE_TavgPrecPetSmi_poly_GP_short_time, vcov = pvcovHC) # Inference with var covar matrix which is robust to heteroskedasdicity
coeftest(FE_TavgPrecPetSmi_poly_GP_short_time, vcovHC(FE_TavgPrecPetSmi_poly_GP_short_time, nethod = "arrellano")) # Inference with var covar matrix which is robust to heteroskedasdicity and autoocorrelatoin

# Ressidual Plot
plot(residuals(FE_TavgPrecPetSmi_poly_GP_short_time))


vif(FE_TavgPrecPetSmi_poly_GP_short_time) 


## Random Effects, poly2, mit trend ##
RE_TavgPrecPetSmi_poly_GP_short_trend <- plm(formula = siloMaize ~    poly(Pet_GP_short_av, 2, raw=T) + poly(Prec_GP_short_av, 2, raw=T) +  poly(Tavg_GP_short_av, 1, raw=T) + poly(SMI_GP_short_av, 2, raw=T)  + poly(as.numeric(year),1, raw=T) , data = Yield_Covariates_nna_plm, , index=c("comId", "year"), model="random", effect = "individual")
## Diagnostics
summary(RE_TavgPrecPetSmi_poly_GP_short_trend)

# Heterokedasdicity robust Inference
coeftest(RE_TavgPrecPetSmi_poly_GP_short_trend, vcov = pvcovHC) # Inference with var covar matrix which is robust to heteroskedasdicity

plot(residuals(RE_TavgPrecPetSmi_poly_GP_short_trend)) # hier zeigen die Residuals mehr Struktur als bei FE
plot(FE_TavgPrecPetSmi_poly_GP_short ) 

vif(FE_TavgPrecPetSmi_poly_GP_short) 

#################
#### Testing ####
#################

################################################
#### Fixed or Random Effects: Hausmann Test ####

phtest(FE_TavgPrecPetSmi_poly_GP_short_trend, RE_TavgPrecPetSmi_poly_GP_short_trend)

'
Der Hausmann Test besagt, dass die Null, nämlich das Model ist Random Effects, abgelehnt werden kann.
Daher macht FE setting hier mehr Sinn.

'

############################################################################
#### Testing for random effects: Breusch-Pagan Lagrange multiplier (LM) ####

plmtest(RE_TavgPrecPetSmi_poly_GP_short_trend, type=c("bp"))

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
'
Here we can reject the null, that there is no panel effect. Therefore random effects is the preferred model.
'

####################################
## Testing for time-fixed effects ##
plmtest(FE_TavgPrecPetSmi_poly_GP_short, c("time"), type=("bp"))
'
Here, from a fitting perspective it makes sense accodring to inference to use time fixed effects. However, this means there is a lot not explained what happened within the years. 
Ein Ansatz wäre, dass man ich mit Drought Dummies benutzt, welche entsprechende Dürre Effekte abdecken.

'

#######################################################################
## Testing for Cross Sectional Dependence/contempraneous correlation ##
pcdtest(FE_TavgPrecPetSmi_poly_GP_short_trend, test = c("lm"))
pcdtest(FE_TavgPrecPetSmi_poly_GP_short_time, test = c("lm"))

pcdtest(FE_TavgPrecPetSmi_poly_GP_short_trend, test = c("cd"))
pcdtest(FE_TavgPrecPetSmi_poly_GP_short_time, test = c("cd"))
'
No cross sectional dependence/contempraneous correlation (Correlation between the realizations of two time series variables in the same time period.)
'

########################################
## Testing for serial autocorrelation ##
pbgtest(FE_TavgPrecPetSmi_poly_GP_short_trend)
pbgtest(FE_TavgPrecPetSmi_poly_GP_short_time)
'
No serial autocorrelation -> 
'

####################################
## Testing for heteroskedasdicity ##
pbgtest(FE_TavgPrecPetSmi_poly_GP_short_trend)
pbgtest(FE_TavgPrecPetSmi_poly_GP_short_time)
'
No heteroscedasdicity
'




###################################################################################################################################################################
################################################### Now I have a look at the singular months ######################################################################
###################################################################################################################################################################


###################################################################################################################################################################
######################################################################## July #####################################################################################

###############################################
## Find classes of SMI for Stepwise Funktion ## 

# ## classIntervals - function ##
# fj5 <- classIntervals(as.numeric(SMI_Jul), n = 5, style = "hclust")
# plot(fj5, pal=pal, main = "Hierarchical Clustering", xlab = "", ylab = "")
# fj5
# Yield_Covariates_nna_plm$SMI_Jul_hclust <- cut(SMI_Jul, breaks = c(0.04716508,0.3016701, 0.455147,0.5564872, 0.7232872,0.9550925))
# table(Yield_Covariates_nna_plm$SMI_Jul_hclust)
# # Interpretation:
# ' Das hierarchische Clustering zeigt schon mal im unsupervised context, dass es cluster gibt. Dies kann dann evtl. durch einen supervised Ansatz ausgebaut werden.'

## Drought Monitor Spezification ##
Yield_Covariates_nna_plm$SMI_Jul_GDM <- cut(SMI_Jul, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                            labels = c("severe drought","moderate drought","abnormally dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
class(Yield_Covariates_nna_plm$SMI_Jul_GDM)
str(Yield_Covariates_nna_plm$SMI_Jul_GDM)
table(Yield_Covariates_nna_plm$SMI_Jul_GDM)
head(Yield_Covariates_nna_plm$SMI_Jul_GDM )
plot(Yield_Covariates_nna_plm$SMI_Jul_GDM)
## Relevel Faktor Variable for SMI classes ##
Yield_Covariates_nna_plm$SMI_Jul_GDM_nolevel <- Yield_Covariates_nna_plm$SMI_Jul_GDM
Yield_Covariates_nna_plm$SMI_Jul_GDM_re <- relevel(Yield_Covariates_nna_plm$SMI_Jul_GDM, "normal")
head(Yield_Covariates_nna_plm$SMI_Jul_GDM_re)

table(Yield_Covariates_nna_plm$SMI_Jul_GDM)
table(Yield_Covariates_nna_plm$SMI_Jul_GDM_re)
all(Yield_Covariates_nna_plm$SMI_Jul_GDM_re == Yield_Covariates_nna_plm$SMI_Jul_GDM)

# Rename 
Yield_Covariates_nna_plm$SMI_Jul_GDM <- Yield_Covariates_nna_plm$SMI_Jul_GDM_re

table(Yield_Covariates_nna_plm$SMI_Jul_GDM) 
head(model.matrix(~ SMI_Jul_GDM - 1, data= Yield_Covariates_nna_plm))

#######################################
## PLM - time demeaning, individuual ## 
#######################################
## Polygons of SMI ##
formula_saturated   <- siloMaize ~ poly(Prec_Jul,3, raw=T) + poly(Pet_Jul, 3, raw=T) + poly(Tavg_Jul, 3, raw=T) + poly(SMI_Jul,3, raw=T) + poly(as.numeric(year))
formula_more        <- siloMaize ~ poly(Prec_Jul,2, raw=T) + poly(Pet_Jul, 2, raw=T) + poly(Tavg_Jul, 1, raw=T) + poly(SMI_Jul,2, raw=T) + poly(as.numeric(year))
formula_spline      <- siloMaize ~ ns(Prec_Jul,2)          + ns(Pet_Jul, 2)          + ns(Tavg_Jul, 1)          + SMI_Jul_GDM            + poly(as.numeric(year))

FE_Jul_trend_poly_saturated <- plm(formula = formula_saturated, data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jul_trend_poly           <- plm(formula = formula_more,      data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jul_trend_poly_spline    <- plm(formula = formula_spline,    data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")

summary(FE_Jul_trend_poly_saturated)
summary(FE_Jul_trend_poly)
summary(FE_Jul_trend_poly_spline)
# Interpretation
'Momentan favorisiere ich den stepwise approach, da dieser sich leichter interpretieren lässt, bei entsprechende Konfiguration auf German Drought Monitor bezug
nimmt und flexibler ist. Zudem ist er relativ stabil, was die Konfiguration der Kontrollvariablen angeht.'

## Stepwise Function of SMI ##
formula_step              <-  siloMaize ~ poly(Prec_Jul,3 ,raw=T) + SMI_Jul_GDM + as.numeric(year) 
formula_step_ort          <-  siloMaize ~ poly(Prec_Jul,3 ,raw=F) + poly(Pet_Jul, 2, raw=F) + poly(Tavg_Jul, 3, raw=T) + SMI_Jul_GDM + as.numeric(year)  
formula_step_noPet        <-  siloMaize ~ poly(Prec_Jul,2, raw=T)                           + poly(Tavg_Jul, 2, raw=T) + SMI_Jul_GDM + as.numeric(year)
formula_step_noTemp       <-  siloMaize ~ poly(Prec_Jul,3 ,raw=T) + poly(Pet_Jul, 3, raw=T)                            + SMI_Jul_GDM + as.numeric(year) 
formula_step_spline       <-  siloMaize ~ ns(Prec_Jul,10)         + ns(Pet_Jul, 10)         + ns(Tavg_Jul, 10)         + SMI_Jul_GDM + as.numeric(year)
formula_step_linear       <-  siloMaize ~ Prec_Jul                + Pet_Jul                 + Tavg_Jul                 + SMI_Jul_GDM + as.numeric(year)
formula_step_nested       <-  siloMaize ~ poly(Prec_Jul,3 ,raw=T) + poly(Pet_Jul, 3, raw=T) + poly(Tavg_Jul, 3, raw=T) + SMI_Jul_GDM + as.numeric(year) 
formula_step_nested_noTemp<-  siloMaize ~ poly(Prec_Jul,2 ,raw=T) + poly(Pet_Jul, 2, raw=T)                            + SMI_Jul_GDM + as.numeric(year) 

formula_step_linear_noPet <-  update(formula_step_linear, ~. - Pet_Jul )


FE_Jul_step_trend              <- plm(formula = formula_step        , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jul_step_trend_noPet        <- plm(formula = formula_step_noPet  , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jul_step_trend_noTemp       <- plm(formula = formula_step_noTemp , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jul_step_trend_spline       <- plm(formula = formula_step_spline , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jul_step_trend_ort          <- plm(formula = formula_step_ort    , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jul_step_trend_linear       <- plm(formula = formula_step_linear , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jul_step_trend_nested       <- plm(formula = formula_step_nested , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jul_step_trend_nested_noTemp<- plm(formula = formula_step_nested_noTemp, data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jul_step_trend_linear_noPet <- plm(formula = formula_step_linear_noPet , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")

## Diagnostics ##
summary(FE_Jul_step_trend)
summary(FE_Jul_step_trend_noPet)
summary(FE_Jul_step_trend_noTemp)
summary(FE_Jul_step_trend_spline)
summary(FE_Jul_step_trend_ort)
summary(FE_Jul_step_trend_linear)
summary(FE_Jul_step_trend_nested)
summary(FE_Jul_step_trend_nested_noTemp)
summary(FE_Jul_step_trend_linear_noPet)
'Hier gibt es wieder sehr sensitive Ergebnisse. Vor allem die Polygonlösungen größer eins haben einen komischen Einfluss auf SMI. Lineare und Splines sind sich
im Ergebnis sehr ähnlich.
Es scheint vor allem an der Korrelation vom Temperatur/Tavg und SMI zu liegen. Wenn Temperatur und PET nicht berücksichtigt wird, dann ergibt sich wieder ein sinnvolles Muster
bei SMI'

# How to choose degree of polygons of control variables;
'Die Grade der Polygone sollte ich evtl via cross validation auswählen. Das Problem ist, dass ich nicht weiß, ob das dann auch im spatial setting umzusetzen ist.
Alternative ist Anova. T test allein nur wenn die polygones als orthogonale definiert sind.
Multicollinearity zwischen PET und Temp_avg zu groß wenn ich raw=F verwende, also orthogonale Polygone.
Daher stellt sich die Frage, ob Multicollinearität einen Einfluss auf die t-statisics hat. 
Das ist definitiv der Fall, da die Varianz im Fall von Multicllinearity größer wird.  
Da man nur den Vergleich t-test und F-test machen kann, wenn man raw=F definiert hat, kann man wohl auch nicht Ergebisse übertragen, also
das droppen von variablen, wenn raw=T
Daher sollte ich mich an den ortogonalen Werte orientieren, und dann dies mit den raw=T Daten und entsprecheden Anova Tests/F-test belegen.
Grundsätzlich kann man aber die Aussage treffen, dass die stepwise function nicht sehr sensitiv auf Veränderungen im Polygon Grad
der kontrollierenden Variablen reagieren. Was tatsächlich für eine derartige Specification spricht. 
Alternativen zu den Polygonen wäre auch natural splines. Genauso wie bei den Polygonen müssten hier die degrees of freedom wohl via cross validation
definiert werden. 
'

## Shoe fixed Effects ##
mean(summary(fixef(FE_Jul_step_trend_spline))[,1])
summary(fixef(FE_Jul_step_trend_spline))
## Ftest Polygon Grad 3 ##
waldtest(FE_Jul_step_trend_nested, FE_Jul_step_trend)
' funkioniert seltsamerweise nich '

# Heterokedasdicity robust Inference
' HIer sind die Polygone wohl zu korreliert, daher funktioniert der Test mit Splines und Orthogonalen Polygonen'
coeftest(FE_Jul_step_trend_ort, vcov = pvcovHC) # Inference with var covar matrix which is robust to heteroskedasdicity
coeftest(FE_Jul_step_trend_spline, vcov = pvcovHC) 
coeftest(FE_Jul_step_trend, vcov = pvcovHC) 
coeftest(FE_Jul_step_trend_spline, vcovHC(FE_Jul_step_trend_spline, method = "arellano")) # Inference with var covar matrix which is robust to heteroskedasdicity and autoocorrelatoin
' '

## Compare Policy Models: Pet and nonPet
# Use Waldtest instead of Anova, which is both an F-test
waldtest(FE_Jul_step_trend_linear_noPet,FE_Jul_step_trend_linear)
# Es macht wohl Sinn, Pet im Model zu lassen, da es einen significanten Unterschied zwischen den Modellen gibt. 

plot(FE_Jul_step_trend)

# Coefficients
coef(FE_Jul_step_trend)

# Predictions and Confidence Intervalls
predict(FE_Jul_step_trend)
confint(FE_Jul_step_trend)

residuals(FE_Jul_step_trend)

## Plot Residuals vs Fitted
plot(residuals(FE_Jul_step_trend), predict(FE_Jul_step_trend) )
plot(rstudent(FE_Jul_step_trend), predict(FE_Jul_step_trend) )
' Es ist keine wirklich heteroskedasdicity zu erkennen und auch keinen großen Ausreißer, auch keine funktionale Ungereimtheit, nur ein gewisses Clustering'

######################################
## Test for Random or Fixed Effects ##
RE_Jul_trend_linear     <- plm(formula = formula_step_linear      , data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="random", effect = "individual")
FE_Jul_trend_linear     <- plm(formula = formula_step_linear      , data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="within", effect = "individual")

phtest(FE_Jul_trend_linear, RE_Jul_trend_linear)

'the null that unique errors (fixed effects) are not correlated with explanatory can be rejected.
Therefore use fixed effects'


###########
## LSDV ##
LSDV_Jul_trend_poly_saturated <- lm(formula = update(formula_saturated, ~. + comId),    data = Yield_Covariates_nna_plm)
LSDV_Jul_trend_poly_poly      <- lm(formula = update(formula_more, ~. + comId),         data = Yield_Covariates_nna_plm)

LSDV_Jul_step_trend           <- lm(formula = update(formula_step, ~. + comId),       data = Yield_Covariates_nna_plm)
LSDV_Jul_step_trend_noPet     <- lm(formula = update(formula_step_noPet, ~. + comId), data = Yield_Covariates_nna_plm)
LSDV_Jul_step_trend_nested    <- lm(formula = update(formula_step_nested, ~. + comId),       data = Yield_Covariates_nna_plm)

## Diagnostics
summary(LSDV_Jul_step_trend)
summary(LSDV_Jul_step_trend_nested)

## Mutlicollinearity
vif(LSDV_Jul_step_trend_nested) # Grundsätzlich gibt es Multicollinearitätsproblem zwischen PET und Tavg
vif(LSDV_Jul_step_trend)

## Pairwise Correlation
pairs(formula = update(formula_step_linear, ~. + comId + SMI_Jul),    data = Yield_Covariates_nna_plm)



## Leverage Statistics
plot(hatvalues(LSDV_Jul_step_trend_nested)) # funxt für plm nicht
which.max (hatvalues (LSDV_Jul_step_trend_nested))
'Es sceint einen Wert zu geben, welcher eine ziemliche hohe leverage statistic hat. Evtl. sollte dieser Wert entfernt werden.'

###################################################################################################################################################################
######################################################################## June #####################################################################################
###################################################################################################################################################################

###############################################
## Find classes of SMI for Stepwise Funktion ## 

## classIntervals - function ##
# fj5 <- classIntervals(as.numeric(SMI_Jun), n = 5, style = "hclust")
# plot(fj5, pal=pal, main = "Hierarchical Clustering", xlab = "", ylab = "")
# fj5
# Yield_Covariates_nna_plm$SMI_Jun_hclust <- cut(SMI_Jun, breaks = c(0.04278951,0.1818897, 0.3516883,0.4455249, 0.6772797,0.9363028))
# table(Yield_Covariates_nna_plm$SMI_Jun_hclust)
# Interpretation:
' Das hierarchische Clustering zeigt schon mal im unsupervised context, dass es cluster gibt. Dies kann dann evtl. durch einen supervised Ansatz ausgebaut werden.'

## Drought Monitor Spezification ##
Yield_Covariates_nna_plm$SMI_Jun_GDM <- cut(SMI_Jun, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                            labels = c("severe drought","moderate drought","abnormally dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
class(Yield_Covariates_nna_plm$SMI_Jun_GDM)
str(Yield_Covariates_nna_plm$SMI_Jun_GDM)
table(Yield_Covariates_nna_plm$SMI_Jun_GDM)
head(Yield_Covariates_nna_plm$SMI_Jun_GDM )

## Relevel Faktor Variable for SMI classes ##
Yield_Covariates_nna_plm$SMI_Jun_GDM_nolevel <- Yield_Covariates_nna_plm$SMI_Jun_GDM
Yield_Covariates_nna_plm$SMI_Jun_GDM_re <- relevel(Yield_Covariates_nna_plm$SMI_Jun_GDM, "normal")
head(Yield_Covariates_nna_plm$SMI_Jun_GDM_re)

table(Yield_Covariates_nna_plm$SMI_Jun_GDM)
table(Yield_Covariates_nna_plm$SMI_Jun_GDM_re)
all(Yield_Covariates_nna_plm$SMI_Jun_GDM_re == Yield_Covariates_nna_plm$SMI_Jun_GDM)

# Rename 
Yield_Covariates_nna_plm$SMI_Jun_GDM <- Yield_Covariates_nna_plm$SMI_Jun_GDM_re

table(Yield_Covariates_nna_plm$SMI_Jun_GDM) 
head(model.matrix(~ SMI_Jun_GDM - 1, data= Yield_Covariates_nna_plm))

#######################################
## PLM - time demeaning, individuual ## 
#######################################
## Polygons of SMI ##
formula_saturated   <- siloMaize ~ poly(Prec_Jun,3, raw=T) + poly(Pet_Jun, 3, raw=T) + poly(Tavg_Jun, 3, raw=T) + poly(SMI_Jun,3, raw=T) + poly(as.numeric(year))
formula_more        <- siloMaize ~ poly(Prec_Jun,2, raw=T) + poly(Pet_Jun, 2, raw=T) + poly(Tavg_Jun, 1, raw=T) + poly(SMI_Jun,2, raw=T) + poly(as.numeric(year))
formula_spline      <- siloMaize ~ ns(Prec_Jun,2)          + ns(Pet_Jun, 2)          + ns(Tavg_Jun, 1)          + poly(SMI_Jun,3, raw=T) + poly(as.numeric(year))

FE_Jun_trend_poly_saturated <- plm(formula = formula_saturated, data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jun_trend_poly           <- plm(formula = formula_more,      data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jun_trend_poly_spline    <- plm(formula = formula_spline,    data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")

summary(FE_Jun_trend_poly_saturated)
summary(FE_Jun_trend_poly)
summary(FE_Jun_trend_poly_spline)
# Interpretation
'Momentan favorisiere ich den stepwise approach, da dieser sich leichter interpretieren lässt, bei entsprechende Konfiguration auf German Drought Monitor bezug
nimmt und flexibler ist. Zudem ist er relativ stabil, was die Konfiguration der Kontrollvariablen angeht.'

## Stepwise Function of SMI ##
formula_step              <-  siloMaize ~ poly(Prec_Jun,3 ,raw=T) + poly(Pet_Jun, 3, raw=T) + poly(Tavg_Jun, 3, raw=T) + SMI_Jun_GDM + as.numeric(year) 
formula_step_ort          <-  siloMaize ~ poly(Prec_Jun,2 ,raw=F) + poly(Pet_Jun, 2, raw=F) + poly(Tavg_Jun, 2, raw=F) + SMI_Jun_GDM + as.numeric(year)  
formula_step_noPet        <-  siloMaize ~ poly(Prec_Jun,3, raw=T)                           + poly(Tavg_Jun, 3, raw=T) + SMI_Jun_GDM + as.numeric(year)
formula_step_spline       <-  siloMaize ~ ns(Prec_Jun,5)         + ns(Pet_Jun, 5)         + ns(Tavg_Jun, 5)         + SMI_Jun_GDM + as.numeric(year)
formula_step_linear       <-  siloMaize ~ Prec_Jun                + Pet_Jun                 + Tavg_Jun                 + SMI_Jun_GDM + as.numeric(year)
formula_step_nested       <-  siloMaize ~ poly(Prec_Jun,2 ,raw=T) + poly(Pet_Jun, 2, raw=T) + poly(Tavg_Jun, 2, raw=T) + SMI_Jun_GDM + as.numeric(year)
formula_step_linear_noPet <-  update(formula_step_linear, ~. - Pet_Jun )


FE_Jun_step_trend             <- plm(formula = formula_step        , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jun_step_trend_noPet       <- plm(formula = formula_step_noPet  , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jun_step_trend_spline      <- plm(formula = formula_step_spline , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jun_step_trend_ort         <- plm(formula = formula_step_ort    , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jun_step_trend_linear      <- plm(formula = formula_step_linear , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jun_step_trend_nested      <- plm(formula = formula_step_nested , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Jun_step_trend_linear_noPet<- plm(formula = formula_step_linear_noPet , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")

## Diagnostics ##
summary(FE_Jun_step_trend)
summary(FE_Jun_step_trend_noPet)
summary(FE_Jun_step_trend_spline)
summary(FE_Jun_step_trend_ort)
summary(FE_Jun_step_trend_linear)
summary(FE_Jun_step_trend_nested)
'Hier sind die Kombinationen sehr sensitive, wobei die grasseste Ausnahme wohl FE_Jun_step_trend ist und diese ist nicht ganz konsistent.
Das beste Ergebnis, welche zumindest am ehesten nachzuvollziehen ist, liefert der natural spline Ansatz, wobei je höher das Spline-df, desto besser der Ansatz.
Bei moderate drought passiert etwas, dass nicht wirlich nachzuvollziehen ist.
Vor allem können damit Bauernregel wieder gegeben werden, denen zu Folge ein trockener Juni gut ist.
Achtung, bei zu großen SPlines DF wird hier precipitation rauserechnet und die Ergebnisse ändern sich'

# How to choose degree of polygons of control variables;
'Die Grade der Polygone sollte ich evtl via cross validation auswählen. Das Problem ist, dass ich nicht weiß, ob das dann auch im spatial setting umzusetzen ist.
Alternative ist Anova. T test allein nur wenn die polygones als orthogonale definiert sind.
Multicollinearity zwischen PET und Temp_avg zu groß wenn ich raw=F verwende, also orthogonale Polygone.
Daher stellt sich die Frage, ob Multicollinearität einen Einfluss auf die t-statisics hat. 
Das ist definitiv der Fall, da die Varianz im Fall von Multicllinearity größer wird.  
Da man nur den Vergleich t-test und F-test machen kann, wenn man raw=F definiert hat, kann man wohl auch nicht Ergebisse übertragen, also
das droppen von variablen, wenn raw=T
Daher sollte ich mich an den ortogonalen Werte orientieren, und dann dies mit den raw=T Daten und entsprecheden Anova Tests/F-test belegen.
Grundsätzlich kann man aber die Aussage treffen, dass die stepwise function nicht sehr sensitiv auf Veränderungen im Polygon Grad
der kontrollierenden Variablen reagieren. Was tatsächlich für eine derartige Specification spricht. 
Alternativen zu den Polygonen wäre auch natural splines. Genauso wie bei den Polygonen müssten hier die degrees of freedom wohl via cross validation
definiert werden. 
'

## See what knots are selected in natural cubric spline ##
attr(terms(FE_Jun_step_trend_spline), "factors")
terms(FE_Jun_step_trend_spline)

## Ftest Polygon Grad 3 ##
waldtest(FE_Jun_step_trend_nested, FE_Jun_step_trend)trend_step
' funkioniert seltsamerweise nich '

# Heterokedasdicity robust Inference
' HIer sind die Polygone wohl zu korreliert, daher funktioniert der Test mit Splines und Orthogonalen Polygonen'
coeftest(FE_Jun_step_trend_ort, vcov = pvcovHC) # Inference with var covar matrix which is robust to heteroskedasdicity
coeftest(FE_Jun_step_trend_spline, vcov = pvcovHC) 
coeftest(FE_Jun_step_trend, vcov = pvcovHC) 
coeftest(FE_Jun_step_trend_spline, vcovHC(FE_Jun_step_trend_spline, method = "arellano")) # Inference with var covar matrix which is robust to heteroskedasdicity and autoocorrelatoin


## Compare Policy Models: Pet and nonPet
# Use Waldtest instead of Anova, which is both an F-test
waldtest(FE_Jun_step_trend_linear_noPet,FE_Jun_step_trend_linear)
# Es macht wohl Sinn, Pet im Model zu lassen, da es einen significanten Unterschied zwischen den Modellen gibt. 

plot(FE_Jun_step_trend)

# Coefficients
coef(FE_Jun_step_trend)

# Predictions and Confidence Intervalls
predict(FE_Jun_step_trend)
confint(FE_Jun_step_trend)

residuals(FE_Jun_step_trend)

## Plot Residuals vs Fitted
plot(residuals(FE_Jun_step_trend), predict(FE_Jun_step_trend) )

' Es ist keine wirklich heteroskedasdicity zu erkennen und auch keinen großen Ausreißer, auch keine funktionale Ungereimtheit, nur ein gewisses Clustering'

######################################
## Test for Random or Fixed Effects ##
RE_Jun_trend_linear     <- plm(formula = formula_step_linear      , data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="random", effect = "individual")
FE_Jun_trend_linear     <- plm(formula = formula_step_linear      , data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="within", effect = "individual")

phtest(FE_Jun_trend_linear, RE_Jun_trend_linear)

'the null that unique errors (fixed effects) are not correlated with explanatory can be rejected.
Therefore use fixed effects'


###########
## LSDV ##
LSDV_Jun_trend_poly_saturated <- lm(formula = update(formula_saturated, ~. + comId),    data = Yield_Covariates_nna_plm)
LSDV_Jun_trend_poly_poly      <- lm(formula = update(formula_more, ~. + comId),         data = Yield_Covariates_nna_plm)

LSDV_Jun_step_trend           <- lm(formula = update(formula_step, ~. + comId),       data = Yield_Covariates_nna_plm)
LSDV_Jun_step_trend_noPet     <- lm(formula = update(formula_step_noPet, ~. + comId), data = Yield_Covariates_nna_plm)
LSDV_Jun_step_trend_nested    <- lm(formula = update(formula_step_nested, ~. + comId),       data = Yield_Covariates_nna_plm)

## Diagnostics
summary(LSDV_Jun_step_trend)
summary(LSDV_Jun_step_trend_nested)

## Mutlicollinearity
vif(LSDV_Jun_step_trend_nested) # Grundsätzlich gibt es Multicollinearitätsproblem zwischen PET und Tavg
vif(LSDV_Jun_step_trend)

## Pairwise Correlation
pairs(formula = update(formula_step_linear, ~. + comId + SMI_Jun),    data = Yield_Covariates_nna_plm)



## Leverage Statistics
plot(hatvalues(LSDV_Jun_step_trend_nested)) # funxt für plm nicht
which.max (hatvalues (LSDV_Jun_step_trend_nested))
'Es sceint einen Wert zu geben, welcher eine ziemliche hohe leverage statistic hat. Evtl. sollte dieser Wert entfernt werden.'
###################################################################################################################################################################
######################################################################## Mai ######################################################################################
###################################################################################################################################################################

###############################################
## Find classes of SMI for Stepwise Funktion ## 

## classIntervals - function ##
# fj5 <- classIntervals(as.numeric(SMI_Mai), n = 5, style = "hclust")
# plot(fj5, pal=pal, main = "Hierarchical Clustering", xlab = "", ylab = "")
# fj5
# Yield_Covariates_nna_plm$SMI_Mai_hclust <- cut(SMI_Mai, breaks = c(0.04278951,0.1818897, 0.3516883,0.4455249, 0.6772797,0.9363028))
# table(Yield_Covariates_nna_plm$SMI_Mai_hclust)
# Interpretation:
' Das hierarchische Clustering zeigt schon mal im unsupervised context, dass es cluster gibt. Dies kann dann evtl. durch einen supervised Ansatz ausgebaut werden.'

## Drought Monitor Spezification ##
Yield_Covariates_nna_plm$SMI_Mai_GDM <- cut(SMI_Mai, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                            labels = c("severe drought","moderate drought","abnormally dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
class(Yield_Covariates_nna_plm$SMI_Mai_GDM)
str(Yield_Covariates_nna_plm$SMI_Mai_GDM)
table(Yield_Covariates_nna_plm$SMI_Mai_GDM)
head(Yield_Covariates_nna_plm$SMI_Mai_GDM )

## Relevel Faktor Variable for SMI classes ##
Yield_Covariates_nna_plm$SMI_Mai_GDM_nolevel <- Yield_Covariates_nna_plm$SMI_Mai_GDM
Yield_Covariates_nna_plm$SMI_Mai_GDM_re <- relevel(Yield_Covariates_nna_plm$SMI_Mai_GDM, "normal")
head(Yield_Covariates_nna_plm$SMI_Mai_GDM_re)

table(Yield_Covariates_nna_plm$SMI_Mai_GDM)
table(Yield_Covariates_nna_plm$SMI_Mai_GDM_re)
all(Yield_Covariates_nna_plm$SMI_Mai_GDM_re == Yield_Covariates_nna_plm$SMI_Mai_GDM)

# Rename 
Yield_Covariates_nna_plm$SMI_Mai_GDM <- Yield_Covariates_nna_plm$SMI_Mai_GDM_re

table(Yield_Covariates_nna_plm$SMI_Mai_GDM) 
head(model.matrix(~ SMI_Mai_GDM - 1, data= Yield_Covariates_nna_plm))

#######################################
## PLM - time demeaning, individuual ## 
#######################################
## Polygons of SMI ##
formula_saturated   <- siloMaize ~ poly(Prec_Mai,3, raw=T) + poly(Pet_Mai, 3, raw=T) + poly(Tavg_Mai, 3, raw=T) + poly(SMI_Mai,3, raw=T) + poly(as.numeric(year))
formula_more        <- siloMaize ~ poly(Prec_Mai,2, raw=T) + poly(Pet_Mai, 2, raw=T) + poly(Tavg_Mai, 1, raw=T) + poly(SMI_Mai,2, raw=T) + poly(as.numeric(year))
formula_spline      <- siloMaize ~ ns(Prec_Mai,2)          + ns(Pet_Mai, 2)          + ns(Tavg_Mai, 1)          + SMI_Mai_GDM            + poly(as.numeric(year))

FE_Mai_trend_poly_saturated <- plm(formula = formula_saturated, data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mai_trend_poly           <- plm(formula = formula_more,      data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mai_trend_poly_spline    <- plm(formula = formula_spline,    data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")

summary(FE_Mai_trend_poly_saturated)
summary(FE_Mai_trend_poly)
summary(FE_Mai_trend_poly_spline)
# Interpretation
'Momentan favorisiere ich den stepwise approach, da dieser sich leichter interpretieren lässt, bei entsprechende Konfiguration auf German Drought Monitor bezug
nimmt und flexibler ist. Zudem ist er relativ stabil, was die Konfiguration der Kontrollvariablen angeht.'

## Stepwise Function of SMI ##
formula_step              <-  siloMaize ~ poly(Prec_Mai,3 ,raw=T) + poly(Pet_Mai, 3, raw=T) + poly(Tavg_Mai, 3, raw=T) + SMI_Mai_GDM + as.numeric(year) 
formula_step_ort          <-  siloMaize ~ poly(Prec_Mai,2 ,raw=F) + poly(Pet_Mai, 1, raw=F) + poly(Tavg_Mai, 1, raw=F) + SMI_Mai_GDM + as.numeric(year)  
formula_step_noPet        <-  siloMaize ~ poly(Prec_Mai,3, raw=T)                           + poly(Tavg_Mai, 3, raw=T) + SMI_Mai_GDM + as.numeric(year)
formula_step_spline       <-  siloMaize ~ ns(Prec_Mai,15)         + ns(Pet_Mai, 15)         + ns(Tavg_Mai, 15)         + SMI_Mai_GDM + as.numeric(year)
formula_step_linear       <-  siloMaize ~ Prec_Mai                + Pet_Mai                 + Tavg_Mai                 + SMI_Mai_GDM + as.numeric(year)
formula_step_nested       <-  siloMaize ~ poly(Prec_Mai,2 ,raw=T) + poly(Pet_Mai, 1, raw=T) + poly(Tavg_Mai, 1, raw=T) + SMI_Mai_GDM + as.numeric(year)
formula_step_linear_noPet <-  update(formula_step_linear, ~. - Pet_Mai )


FE_Mai_step_trend             <- plm(formula = formula_step        , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mai_step_trend_noPet       <- plm(formula = formula_step_noPet  , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mai_step_trend_spline      <- plm(formula = formula_step_spline , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mai_step_trend_ort         <- plm(formula = formula_step_ort    , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mai_step_trend_linear      <- plm(formula = formula_step_linear , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mai_step_trend_nested      <- plm(formula = formula_step_nested , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")

## Diagnostics ##
summary(FE_Mai_step_trend)
summary(FE_Mai_step_trend_noPet)
summary(FE_Mai_step_trend_spline)
summary(FE_Mai_step_trend_ort)
summary(FE_Mai_step_trend_linear)
summary(FE_Mai_step_trend_nested)
'Hier sind die Kombinationen nicht so sehr sensitiv. Aber sie machen auf den ersten Blick keinen Sinn. Zudem ist die Bodenfeuchte in den nassen Bereichen
nicht relevant. Alles sehr dubios hier.'

# How to choose degree of polygons of control variables;
'Die Grade der Polygone sollte ich evtl via cross validation auswählen. Das Problem ist, dass ich nicht weiß, ob das dann auch im spatial setting umzusetzen ist.
Alternative ist Anova. T test allein nur wenn die polygones als orthogonale definiert sind.
Multicollinearity zwischen PET und Temp_avg zu groß wenn ich raw=F verwende, also orthogonale Polygone.
Daher stellt sich die Frage, ob Multicollinearität einen Einfluss auf die t-statisics hat. 
Das ist definitiv der Fall, da die Varianz im Fall von Multicllinearity größer wird.  
Da man nur den Vergleich t-test und F-test machen kann, wenn man raw=F definiert hat, kann man wohl auch nicht Ergebisse übertragen, also
das droppen von variablen, wenn raw=T
Daher sollte ich mich an den ortogonalen Werte orientieren, und dann dies mit den raw=T Daten und entsprecheden Anova Tests/F-test belegen.
Grundsätzlich kann man aber die Aussage treffen, dass die stepwise function nicht sehr sensitiv auf Veränderungen im Polygon Grad
der kontrollierenden Variablen reagieren. Was tatsächlich für eine derartige Specification spricht. 
Alternativen zu den Polygonen wäre auch natural splines. Genauso wie bei den Polygonen müssten hier die degrees of freedom wohl via cross validation
definiert werden. 
'



## Ftest Polygon Grad 3 ##
waldtest(FE_Mai_step_trend_nested, FE_Mai_step_trend)
' funkioniert seltsamerweise nich '

# Heterokedasdicity robust Inference
' HIer sind die Polygone wohl zu korreliert, daher funktioniert der Test mit Splines und Orthogonalen Polygonen'
coeftest(FE_Mai_step_trend_ort, vcov = pvcovHC) # Inference with var covar matrix which is robust to heteroskedasdicity
coeftest(FE_Mai_step_trend_spline, vcov = pvcovHC) 
coeftest(FE_Mai_step_trend, vcov = pvcovHC) 
coeftest(FE_Mai_step_trend_spline, vcovHC(FE_Mai_step_trend_spline, method = "arellano")) # Inference with var covar matrix which is robust to heteroskedasdicity and autoocorrelatoin
' '

## Compare Policy Models: Pet and nonPet
# Use Waldtest instead of Anova, which is both an F-test
waldtest(FE_Mai_step_trend_linear_noPet,FE_Mai_step_trend_linear)
# Es macht wohl Sinn, Pet im Model zu lassen, da es einen significanten Unterschied zwischen den Modellen gibt. 

plot(FE_Mai_step_trend)

# Coefficients
coef(FE_Mai_step_trend)

# Predictions and Confidence Intervalls
predict(FE_Mai_step_trend)
confint(FE_Mai_step_trend)

residuals(FE_Mai_step_trend)

## Plot Residuals vs Fitted
plot(residuals(FE_Mai_step_trend), predict(FE_Mai_step_trend) )

' Es ist keine wirklich heteroskedasdicity zu erkennen und auch keinen großen Ausreißer, auch keine funktionale Ungereimtheit, nur ein gewisses Clustering'

######################################
## Test for Random or Fixed Effects ##
RE_Mai_trend_linear     <- plm(formula = formula_step_linear      , data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="random", effect = "individual")
FE_Mai_trend_linear     <- plm(formula = formula_step_linear      , data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="within", effect = "individual")

phtest(FE_Mai_trend_linear, RE_Mai_trend_linear)

'the null that unique errors (fixed effects) are not correlated with explanatory can be rejected.
Therefore use fixed effects'


###########
## LSDV ##
LSDV_Mai_trend_poly_saturated <- lm(formula = update(formula_saturated, ~. + comId),    data = Yield_Covariates_nna_plm)
LSDV_Mai_trend_poly_poly      <- lm(formula = update(formula_more, ~. + comId),         data = Yield_Covariates_nna_plm)

LSDV_Mai_step_trend           <- lm(formula = update(formula_step, ~. + comId),       data = Yield_Covariates_nna_plm)
LSDV_Mai_step_trend_noPet     <- lm(formula = update(formula_step_noPet, ~. + comId), data = Yield_Covariates_nna_plm)
LSDV_Mai_step_trend_nested    <- lm(formula = update(formula_step_nested, ~. + comId),       data = Yield_Covariates_nna_plm)

## Diagnostics
summary(LSDV_Mai_step_trend)
summary(LSDV_Mai_step_trend_nested)

## Mutlicollinearity
vif(LSDV_Mai_step_trend_nested) # Grundsätzlich gibt es Multicollinearitätsproblem zwischen PET und Tavg
vif(LSDV_Mai_step_trend)

## Pairwise Correlation
pairs(formula = update(formula_step_linear, ~. + comId + SMI_Mai),    data = Yield_Covariates_nna_plm)



## Leverage Statistics
plot(hatvalues(LSDV_Mai_step_trend_nested)) # funxt für plm nicht
which.max (hatvalues (LSDV_Mai_step_trend_nested))
'Es sceint einen Wert zu geben, welcher eine ziemliche hohe leverage statistic hat. Evtl. sollte dieser Wert entfernt werden.'

###################################################################################################################################################################
######################################################################## April ####################################################################################
###################################################################################################################################################################


###############################################
## Find classes of SMI for Stepwise Funktion ## 

## classIntervals - function ##
# fj5 <- classIntervals(as.numeric(SMI_Apr), n = 5, style = "hclust")
# plot(fj5, pal=pal, Aprn = "Hierarchical Clustering", xlab = "", ylab = "")
# fj5
# Yield_Covariates_nna_plm$SMI_Apr_hclust <- cut(SMI_Apr, breaks = c(0.04278951,0.1818897, 0.3516883,0.4455249, 0.6772797,0.9363028))
# table(Yield_Covariates_nna_plm$SMI_Apr_hclust)
# Interpretation:
' Das hierarchische Clustering zeigt schon mal im unsupervised context, dass es cluster gibt. Dies kann dann evtl. durch einen supervised Ansatz ausgebaut werden.'

## Drought Monitor Spezification ##
Yield_Covariates_nna_plm$SMI_Apr_GDM <- cut(SMI_Apr, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                            labels = c("severe drought","moderate drought","abnormally dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
class(Yield_Covariates_nna_plm$SMI_Apr_GDM)
str(Yield_Covariates_nna_plm$SMI_Apr_GDM)
table(Yield_Covariates_nna_plm$SMI_Apr_GDM)
head(Yield_Covariates_nna_plm$SMI_Apr_GDM )

## Relevel Faktor Variable for SMI classes ##
Yield_Covariates_nna_plm$SMI_Apr_GDM_nolevel <- Yield_Covariates_nna_plm$SMI_Apr_GDM
Yield_Covariates_nna_plm$SMI_Apr_GDM_re <- relevel(Yield_Covariates_nna_plm$SMI_Apr_GDM, "normal")
head(Yield_Covariates_nna_plm$SMI_Apr_GDM_re)

table(Yield_Covariates_nna_plm$SMI_Apr_GDM)
table(Yield_Covariates_nna_plm$SMI_Apr_GDM_re)
all(Yield_Covariates_nna_plm$SMI_Apr_GDM_re == Yield_Covariates_nna_plm$SMI_Apr_GDM)

# Rename 
Yield_Covariates_nna_plm$SMI_Apr_GDM <- Yield_Covariates_nna_plm$SMI_Apr_GDM_re

table(Yield_Covariates_nna_plm$SMI_Apr_GDM) 
head(model.matrix(~ SMI_Apr_GDM - 1, data= Yield_Covariates_nna_plm))

#######################################
## PLM - time demeaning, individuual ## 
#######################################
## Polygons of SMI ##
formula_saturated   <- siloMaize ~ poly(Prec_Apr,3, raw=T) + poly(Pet_Apr, 3, raw=T) + poly(Tavg_Apr, 3, raw=T) + poly(SMI_Apr,3, raw=T) + poly(as.numeric(year))
formula_more        <- siloMaize ~ poly(Prec_Apr,2, raw=T) + poly(Pet_Apr, 2, raw=T) + poly(Tavg_Apr, 1, raw=T) + poly(SMI_Apr,2, raw=T) + poly(as.numeric(year))
formula_spline      <- siloMaize ~ ns(Prec_Apr,2)          + ns(Pet_Apr, 2)          + ns(Tavg_Apr, 1)          + SMI_Apr_GDM            + poly(as.numeric(year))

FE_Apr_trend_poly_saturated <- plm(formula = formula_saturated, data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Apr_trend_poly           <- plm(formula = formula_more,      data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Apr_trend_poly_spline    <- plm(formula = formula_spline,    data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")

summary(FE_Apr_trend_poly_saturated)
summary(FE_Apr_trend_poly)
summary(FE_Apr_trend_poly_spline)
# Interpretation
'Momentan favorisiere ich den stepwise approach, da dieser sich leichter interpretieren lässt, bei entsprechende Konfiguration auf German Drought Monitor bezug
nimmt und flexibler ist. Zudem ist er relativ stabil, was die Konfiguration der Kontrollvariablen angeht.'

## Stepwise Function of SMI ##
formula_step              <-  siloMaize ~ poly(Prec_Apr,3 ,raw=T) + poly(Pet_Apr, 3, raw=T) + poly(Tavg_Apr, 3, raw=T) + SMI_Apr_GDM + as.numeric(year) 
formula_step_ort          <-  siloMaize ~ poly(Prec_Apr,3 ,raw=F) + poly(Pet_Apr, 2, raw=F) + poly(Tavg_Apr, 3, raw=F) + SMI_Apr_GDM + as.numeric(year)  
formula_step_noPet        <-  siloMaize ~ poly(Prec_Apr,3, raw=T)                           + poly(Tavg_Apr, 3, raw=T) + SMI_Apr_GDM + as.numeric(year)
formula_step_spline       <-  siloMaize ~ ns(Prec_Apr,15)         + ns(Pet_Apr, 15)         + ns(Tavg_Apr, 15)         + SMI_Apr_GDM + as.numeric(year)
formula_step_linear       <-  siloMaize ~ Prec_Apr                + Pet_Apr                 + Tavg_Apr                 + SMI_Apr_GDM + as.numeric(year)
formula_step_nested       <-  siloMaize ~ poly(Prec_Apr,2 ,raw=T) + poly(Pet_Apr, 1, raw=T) + poly(Tavg_Apr, 1, raw=T) + SMI_Apr_GDM + as.numeric(year)
formula_step_linear_noPet <-  update(formula_step_linear, ~. - Pet_Apr )


FE_Apr_step_trend             <- plm(formula = formula_step        , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Apr_step_trend_noPet       <- plm(formula = formula_step_noPet  , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Apr_step_trend_spline      <- plm(formula = formula_step_spline , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Apr_step_trend_ort         <- plm(formula = formula_step_ort    , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Apr_step_trend_linear      <- plm(formula = formula_step_linear , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Apr_step_trend_nested      <- plm(formula = formula_step_nested , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Apr_step_trend_linear_noPet<- plm(formula = formula_step_linear_noPet , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")

## Diagnostics ##
summary(FE_Apr_step_trend)
summary(FE_Apr_step_trend_noPet)
summary(FE_Apr_step_trend_spline)
summary(FE_Apr_step_trend_ort)
summary(FE_Apr_step_trend_linear)
summary(FE_Apr_step_trend_nested)
'Hier sind die Kombinationen nicht so sehr sensitiv. Generell ist es wohl besser, wenn es trockener ist oder mit Einschränkungen nasser.
Jetzt brauch ich nur noch nen Grund dafür?!?'

# How to choose degree of polygons of control variables;
'Die Grade der Polygone sollte ich evtl via cross validation auswählen. Das Problem ist, dass ich nicht weiß, ob das dann auch im spatial setting umzusetzen ist.
Alternative ist Anova. T test allein nur wenn die polygones als orthogonale definiert sind.
Multicollinearity zwischen PET und Temp_avg zu groß wenn ich raw=F verwende, also orthogonale Polygone.
Daher stellt sich die Frage, ob Multicollinearität einen Einfluss auf die t-statisics hat. 
Das ist definitiv der Fall, da die Varianz im Fall von Multicllinearity größer wird.  
Da man nur den Vergleich t-test und F-test machen kann, wenn man raw=F definiert hat, kann man wohl auch nicht Ergebisse übertragen, also
das droppen von variablen, wenn raw=T
Daher sollte ich mich an den ortogonalen Werte orientieren, und dann dies mit den raw=T Daten und entsprecheden Anova Tests/F-test belegen.
Grundsätzlich kann man aber die Aussage treffen, dass die stepwise function nicht sehr sensitiv auf Veränderungen im Polygon Grad
der kontrollierenden Variablen reagieren. Was tatsächlich für eine derartige Specification spricht. 
Alternativen zu den Polygonen wäre auch natural splines. Genauso wie bei den Polygonen müssten hier die degrees of freedom wohl via cross validation
definiert werden. 
'



## Ftest Polygon Grad 3 ##
waldtest(FE_Apr_step_trend_nested, FE_Apr_step_trend)
' funkioniert seltsamerweise nich '

# Heterokedasdicity robust Inference
' HIer sind die Polygone wohl zu korreliert, daher funktioniert der Test mit Splines und Orthogonalen Polygonen'
coeftest(FE_Apr_step_trend_ort, vcov = pvcovHC) # Inference with var covar matrix which is robust to heteroskedasdicity
coeftest(FE_Apr_step_trend_spline, vcov = pvcovHC) 
coeftest(FE_Apr_step_trend, vcov = pvcovHC) 
coeftest(FE_Apr_step_trend_spline, vcovHC(FE_Apr_step_trend_spline, method = "arellano")) # Inference with var covar matrix which is robust to heteroskedasdicity and autoocorrelatoin
' '

## Compare Policy Models: Pet and nonPet
# Use Waldtest instead of Anova, which is both an F-test
waldtest(FE_Apr_step_trend_linear_noPet,FE_Apr_step_trend_linear)
# Es macht wohl Sinn, Pet im Model zu lassen, da es einen significanten Unterschied zwischen den Modellen gibt. 

plot(FE_Apr_step_trend)

# Coefficients
coef(FE_Apr_step_trend)

# Predictions and Confidence Intervalls
predict(FE_Apr_step_trend)
confint(FE_Apr_step_trend)

residuals(FE_Apr_step_trend)

## Plot Residuals vs Fitted
plot(residuals(FE_Apr_step_trend), predict(FE_Apr_step_trend) )

' Es ist keine wirklich heteroskedasdicity zu erkennen und auch keinen großen Ausreißer, auch keine funktionale Ungereimtheit, nur ein gewisses Clustering'

######################################
## Test for Random or Fixed Effects ##
RE_Apr_trend_linear     <- plm(formula = formula_step_linear      , data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="random", effect = "individual")
FE_Apr_trend_linear     <- plm(formula = formula_step_linear      , data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="within", effect = "individual")

phtest(FE_Apr_trend_linear, RE_Apr_trend_linear)

'the null that unique errors (fixed effects) are not correlated with explanatory can be rejected.
Therefore use fixed effects'


###########
## LSDV ##
LSDV_Apr_trend_poly_saturated <- lm(formula = update(formula_saturated, ~. + comId),    data = Yield_Covariates_nna_plm)
LSDV_Apr_trend_poly_poly      <- lm(formula = update(formula_more, ~. + comId),         data = Yield_Covariates_nna_plm)

LSDV_Apr_step_trend           <- lm(formula = update(formula_step, ~. + comId),       data = Yield_Covariates_nna_plm)
LSDV_Apr_step_trend_noPet     <- lm(formula = update(formula_step_noPet, ~. + comId), data = Yield_Covariates_nna_plm)
LSDV_Apr_step_trend_nested    <- lm(formula = update(formula_step_nested, ~. + comId),       data = Yield_Covariates_nna_plm)

## Diagnostics
summary(LSDV_Apr_step_trend)
summary(LSDV_Apr_step_trend_nested)

## Mutlicollinearity
vif(LSDV_Apr_step_trend_nested) # Grundsätzlich gibt es Multicollinearitätsproblem zwischen PET und Tavg
vif(LSDV_Apr_step_trend)

## Pairwise Correlation
pairs(formula = update(formula_step_linear, ~. + comId + SMI_Apr),    data = Yield_Covariates_nna_plm)



## Leverage Statistics
plot(hatvalues(LSDV_Apr_step_trend_nested)) # funxt für plm nicht
which.max (hatvalues (LSDV_Apr_step_trend_nested))
'Es sceint einen Wert zu geben, welcher eine ziemliche hohe leverage statistic hat. Evtl. sollte dieser Wert entfernt werden.'


###################################################################################################################################################################
######################################################################## March ####################################################################################
###################################################################################################################################################################

###############################################
## Find classes of SMI for Stepwise Funktion ## 

## classIntervals - function ##
# fj5 <- classIntervals(as.numeric(SMI_Mar), n = 5, style = "hclust")
# plot(fj5, pal=pal, Marn = "Hierarchical Clustering", xlab = "", ylab = "")
# fj5
# Yield_Covariates_nna_plm$SMI_Mar_hclust <- cut(SMI_Mar, breaks = c(0.04278951,0.1818897, 0.3516883,0.4455249, 0.6772797,0.9363028))
# table(Yield_Covariates_nna_plm$SMI_Mar_hclust)
# Interpretation:
' Das hierarchische Clustering zeigt schon mal im unsupervised context, dass es cluster gibt. Dies kann dann evtl. durch einen supervised Ansatz ausgebaut werden.'

## Drought Monitor Spezification ##
Yield_Covariates_nna_plm$SMI_Mar_GDM <- cut(SMI_Mar, breaks = c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1),
                                            labels = c("severe drought","moderate drought","abnormally dry", "normal","abnormal wet" ,"abundant wet", "severe wet"))
class(Yield_Covariates_nna_plm$SMI_Mar_GDM)
str(Yield_Covariates_nna_plm$SMI_Mar_GDM)
table(Yield_Covariates_nna_plm$SMI_Mar_GDM)
head(Yield_Covariates_nna_plm$SMI_Mar_GDM )

## Relevel Faktor Variable for SMI classes ##
Yield_Covariates_nna_plm$SMI_Mar_GDM_nolevel <- Yield_Covariates_nna_plm$SMI_Mar_GDM
Yield_Covariates_nna_plm$SMI_Mar_GDM_re <- relevel(Yield_Covariates_nna_plm$SMI_Mar_GDM, "normal")
head(Yield_Covariates_nna_plm$SMI_Mar_GDM_re)

table(Yield_Covariates_nna_plm$SMI_Mar_GDM)
table(Yield_Covariates_nna_plm$SMI_Mar_GDM_re)
all(Yield_Covariates_nna_plm$SMI_Mar_GDM_re == Yield_Covariates_nna_plm$SMI_Mar_GDM)

# Rename 
Yield_Covariates_nna_plm$SMI_Mar_GDM <- Yield_Covariates_nna_plm$SMI_Mar_GDM_re

table(Yield_Covariates_nna_plm$SMI_Mar_GDM) 
head(model.matrix(~ SMI_Mar_GDM - 1, data= Yield_Covariates_nna_plm))

#######################################
## PLM - time demeaning, individuual ## 
#######################################
## Polygons of SMI ##
formula_saturated   <- siloMaize ~ poly(Prec_Mar,3, raw=T) + poly(Pet_Mar, 3, raw=T) + poly(Tavg_Mar, 3, raw=T) + poly(SMI_Mar,3, raw=T) + poly(as.numeric(year))
formula_more        <- siloMaize ~ poly(Prec_Mar,2, raw=T) + poly(Pet_Mar, 2, raw=T) + poly(Tavg_Mar, 1, raw=T) + poly(SMI_Mar,2, raw=T) + poly(as.numeric(year))
formula_spline      <- siloMaize ~ ns(Prec_Mar,2)          + ns(Pet_Mar, 2)          + ns(Tavg_Mar, 1)          + SMI_Mar_GDM            + poly(as.numeric(year))

FE_Mar_trend_poly_saturated <- plm(formula = formula_saturated, data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mar_trend_poly           <- plm(formula = formula_more,      data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mar_trend_poly_spline    <- plm(formula = formula_spline,    data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")

summary(FE_Mar_trend_poly_saturated)
summary(FE_Mar_trend_poly)
summary(FE_Mar_trend_poly_spline)
# Interpretation
'Momentan favorisiere ich den stepwise approach, da dieser sich leichter interpretieren lässt, bei entsprechende Konfiguration auf German Drought Monitor bezug
nimmt und flexibler ist. Zudem ist er relativ stabil, was die Konfiguration der Kontrollvariablen angeht.'

## Stepwise Function of SMI ##
formula_step              <-  siloMaize ~ poly(Prec_Mar,3 ,raw=T) + poly(Pet_Mar, 3, raw=T) + poly(Tavg_Mar, 3, raw=T) + SMI_Mar_GDM + as.numeric(year) 
formula_step_ort          <-  siloMaize ~ poly(Prec_Mar,3 ,raw=F) + poly(Pet_Mar, 3, raw=F) + poly(Tavg_Mar, 3, raw=F) + SMI_Mar_GDM + as.numeric(year)  
formula_step_noPet        <-  siloMaize ~ poly(Prec_Mar,3, raw=T)                           + poly(Tavg_Mar, 3, raw=T) + SMI_Mar_GDM + as.numeric(year)
formula_step_spline       <-  siloMaize ~ ns(Prec_Mar,15)         + ns(Pet_Mar, 15)         + ns(Tavg_Mar, 15)         + SMI_Mar_GDM + as.numeric(year)
formula_step_linear       <-  siloMaize ~ Prec_Mar                + Pet_Mar                 + Tavg_Mar                 + SMI_Mar_GDM + as.numeric(year)
formula_step_nested       <-  siloMaize ~ poly(Prec_Mar,2 ,raw=T) + poly(Pet_Mar, 1, raw=T) + poly(Tavg_Mar, 1, raw=T) + SMI_Mar_GDM + as.numeric(year)
formula_step_linear_noPet <-  update(formula_step_linear, ~. - Pet_Mar )


FE_Mar_step_trend             <- plm(formula = formula_step        , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mar_step_trend_noPet       <- plm(formula = formula_step_noPet  , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mar_step_trend_spline      <- plm(formula = formula_step_spline , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mar_step_trend_ort         <- plm(formula = formula_step_ort    , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mar_step_trend_linear      <- plm(formula = formula_step_linear , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mar_step_trend_nested      <- plm(formula = formula_step_nested , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")
FE_Mar_step_trend_linear_noPet<- plm(formula = formula_step_linear_noPet , data = Yield_Covariates_nna_plm, index = c("comId", "year"), model="within", effect = "individual")

## Diagnostics ##
summary(FE_Mar_step_trend)
summary(FE_Mar_step_trend_noPet)
summary(FE_Mar_step_trend_spline)
summary(FE_Mar_step_trend_ort)
summary(FE_Mar_step_trend_linear)
summary(FE_Mar_step_trend_nested)
'Hier sind die Kombinationen nicht so sehr sensitiv. Generell kann man wohl sagen, dass trockenes Wetter besser ist, wobei es dann auch wieder
zu trocken werden kann. Generell sind wohl hohe Polygone der Kovariat erklärender hier. '

# How to choose degree of polygons of control variables;
'Die Grade der Polygone sollte ich evtl via cross validation auswählen. Das Problem ist, dass ich nicht weiß, ob das dann auch im spatial setting umzusetzen ist.
Alternative ist Anova. T test allein nur wenn die polygones als orthogonale definiert sind.
Multicollinearity zwischen PET und Temp_avg zu groß wenn ich raw=F verwende, also orthogonale Polygone.
Daher stellt sich die Frage, ob Multicollinearität einen Einfluss auf die t-statisics hat. 
Das ist definitiv der Fall, da die Varianz im Fall von Multicllinearity größer wird.  
Da man nur den Vergleich t-test und F-test machen kann, wenn man raw=F definiert hat, kann man wohl auch nicht Ergebisse übertragen, also
das droppen von variablen, wenn raw=T
Daher sollte ich mich an den ortogonalen Werte orientieren, und dann dies mit den raw=T Daten und entsprecheden Anova Tests/F-test belegen.
Grundsätzlich kann man aber die Aussage treffen, dass die stepwise function nicht sehr sensitiv auf Veränderungen im Polygon Grad
der kontrollierenden Variablen reagieren. Was tatsächlich für eine derartige Specification spricht. 
Alternativen zu den Polygonen wäre auch natural splines. Genauso wie bei den Polygonen müssten hier die degrees of freedom wohl via cross validation
definiert werden. 
'



## Ftest Polygon Grad 3 ##
waldtest(FE_Mar_step_trend_nested, FE_Mar_step_trend)
' funkioniert seltsamerweise nich '

# Heterokedasdicity robust Inference
' HIer sind die Polygone wohl zu korreliert, daher funktioniert der Test mit Splines und Orthogonalen Polygonen'
coeftest(FE_Mar_step_trend_ort, vcov = pvcovHC) # Inference with var covar matrix which is robust to heteroskedasdicity
coeftest(FE_Mar_step_trend_spline, vcov = pvcovHC) 
coeftest(FE_Mar_step_trend, vcov = pvcovHC) 
coeftest(FE_Mar_step_trend_spline, vcovHC(FE_Mar_step_trend_spline, method = "arellano")) # Inference with var covar matrix which is robust to heteroskedasdicity and autoocorrelatoin
' '

## Compare Policy Models: Pet and nonPet
# Use Waldtest instead of Anova, which is both an F-test
waldtest(FE_Mar_step_trend_linear_noPet,FE_Mar_step_trend_linear)
# Es macht wohl Sinn, Pet im Model zu lassen, da es einen significanten Unterschied zwischen den Modellen gibt. 

plot(FE_Mar_step_trend)

# Coefficients
coef(FE_Mar_step_trend)

# Predictions and Confidence Intervalls
predict(FE_Mar_step_trend)
confint(FE_Mar_step_trend)

residuals(FE_Mar_step_trend)

## Plot Residuals vs Fitted
plot(residuals(FE_Mar_step_trend), predict(FE_Mar_step_trend) )

' Es ist keine wirklich heteroskedasdicity zu erkennen und auch keinen großen Ausreißer, auch keine funktionale Ungereimtheit, nur ein gewisses Clustering'

######################################
## Test for Random or Fixed Effects ##
RE_Mar_trend_linear     <- plm(formula = formula_step_linear      , data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="random", effect = "individual")
FE_Mar_trend_linear     <- plm(formula = formula_step_linear      , data = Yield_Covariates_nna_plm, index=c("comId", "year"), model="within", effect = "individual")

phtest(FE_Mar_trend_linear, RE_Mar_trend_linear)

'the null that unique errors (fixed effects) are not correlated with explanatory can be rejected.
Therefore use fixed effects'


###########
## LSDV ##
LSDV_Mar_trend_poly_saturated <- lm(formula = update(formula_saturated, ~. + comId),    data = Yield_Covariates_nna_plm)
LSDV_Mar_trend_poly_poly      <- lm(formula = update(formula_more, ~. + comId),         data = Yield_Covariates_nna_plm)

LSDV_Mar_step_trend           <- lm(formula = update(formula_step, ~. + comId),       data = Yield_Covariates_nna_plm)
LSDV_Mar_step_trend_noPet     <- lm(formula = update(formula_step_noPet, ~. + comId), data = Yield_Covariates_nna_plm)
LSDV_Mar_step_trend_nested    <- lm(formula = update(formula_step_nested, ~. + comId),       data = Yield_Covariates_nna_plm)

## Diagnostics
summary(LSDV_Mar_step_trend)
summary(LSDV_Mar_step_trend_nested)

## Mutlicollinearity
vif(LSDV_Mar_step_trend_nested) # Grundsätzlich gibt es Multicollinearitätsproblem zwischen PET und Tavg
vif(LSDV_Mar_step_trend)

## Pairwise Correlation
pairs(formula = update(formula_step_linear, ~. + comId + SMI_Mar),    data = Yield_Covariates_nna_plm)



## Leverage Statistics
plot(hatvalues(LSDV_Mar_step_trend_nested)) # funxt für plm nicht
which.max (hatvalues (LSDV_Mar_step_trend_nested))
'Es sceint einen Wert zu geben, welcher eine ziemliche hohe leverage statistic hat. Evtl. sollte dieser Wert entfernt werden.'

###################################################################################################################################################################
######################################################################## February #################################################################################
###################################################################################################################################################################

Yield_Covariates_nna_plm$lower_Feb <- as.numeric(SMI_Feb < 0.2067804)
table(lower_Feb)
Yield_Covariates_nna_plm$upper_Feb <- as.numeric(SMI_Feb > 0.7407877)
table(upper_Feb)
lower_Feb==upper_Feb
Yield_Covariates_nna_plm$siloMaize_zscore <-scale(siloMaize, center = TRUE, scale = TRUE)
table(siloMaize_zscore)

## Find classes of SMI ## 
# classIntervals - function
fj5 <- classIntervals(as.numeric(SMI_Feb), n=5, style="hclust")
plot(fj5, pal=pal, Marn="Hierarchical Clustering", xlab="", ylab="")
Yield_Covariates_nna_plm$SMI_Feb_hclust <- cut(SMI_Feb, breaks = c(0, 0.2067804,0.3916562,0.6060474,0.7407877,0.9683965))
table(Yield_Covariates_nna_plm$SMI_Feb_hclust)
Yield_Covariates_nna_plm$SMI_Feb_GDM <- cut(SMI_Feb, breaks = c(0, 0.2,0.8,1))
table(Yield_Covariates_nna_plm$SMI_Feb_GDM)

## PLM - time demeaning, individuual ##
formula_saturated <- siloMaize ~ poly(Prec_Feb,3, raw=T) + poly(Pet_Feb, 3, raw=T) + poly(Tavg_Feb, 3, raw=T) + poly(SMI_Feb,3, raw=T) + poly(as.numeric(year))
formula_more <- siloMaize ~      poly(Prec_Feb,2, raw=T) + poly(Pet_Feb, 2, raw=T) +  poly(Tavg_Feb, 1, raw=T) + poly(SMI_Feb,1, raw=T) + poly(as.numeric(year))
formula <- siloMaize ~           poly(Prec_Feb,1, raw=T) +  poly(Tavg_Feb, 2, raw=T) + lower_Feb + upper_Feb + as.numeric(year)

formula_step <-  siloMaize ~      poly(Prec_Feb,1, raw=T) +  poly(Pet_Feb, 1, raw=T) + poly(Tavg_Feb, 1, raw=T) + SMI_Feb_GDM + as.numeric(year)    

formula_step_noPet <-  siloMaize ~      poly(Prec_Feb,1, raw=T) +  poly(Tavg_Feb, 2, raw=T) + SMI_Feb_GDM + as.numeric(year)   

FE_poly_Feb_trend_saturated <-plm(formula = formula_saturated, data = Yield_Covariates_nna_plm, , index=c("comId", "year"), model="within", effect = "individual")
FE_poly_Feb_trend_more <-plm(formula = formula_more  , data = Yield_Covariates_nna_plm, , index=c("comId", "year"), model="within", effect = "individual")
FE_poly_Feb_trend <-plm(formula = formula  , data = Yield_Covariates_nna_plm, , index=c("comId", "year"), model="within", effect = "individual")
FE_poly_Feb_step_trend <-plm(formula = formula_step  , data = Yield_Covariates_nna_plm, , index=c("comId", "year"), model="within", effect = "individual")
FE_poly_Feb_step_trend_noPet <-plm(formula = formula_step_noPet  , data = Yield_Covariates_nna_plm, , index=c("comId", "year"), model="within", effect = "individual")

## Diagnostics
summary(FE_poly_Feb_trend_saturated)
summary(FE_poly_Feb_trend_more)
summary(FE_poly_Feb_trend)
summary(FE_poly_Feb_step_trend)
summary(FE_poly_Feb_step_trend_noPet)

## Compare Policy Models: Pet and nonPet
# Use Waldtest instead of Anova, which is both an F-test
waldtest( FE_poly_Feb_step_trend_noPet, FE_poly_Feb_step_trend)
# Es macht wohl Sinn, Pet im Model zu lassen, da es einen significanten Unterschied zwischen den Modellen gibt. 

plot(FE_poly_Feb_trend)

# Coefficients
coef(FE_poly_Feb_trend)

# Predictions and Confidence INtevalls
predict(FE_poly_Feb_trend)
confint(FE_poly_Feb_trend)

residuals(FE_poly_Feb_trend)

## Plot Residuals vs Fitted
plot(residuals(FE_poly_Feb_step_trend), predict(FE_poly_Feb_step_trend) )

## Leverage Statistics
# hatvalues(FE_poly_Feb_trend) # funxt für plm nicht

?summary.plm

# Heterokedasdicity robust Inference
coeftest(FE_poly_Feb_step_trend, vcov = pvcovHC) # Inference with var covar matrix which is robust to heteroskedasdicity
coeftest(FE_poly_Feb_step_trend, vcovHC(FE_poly_Feb_trend, nethod = "arrellano")) # Inference with var covar matrix which is robust to heteroskedasdicity and autoocorrelatoin


## LSDV ##
LSDV_poly_Feb_trend_saturated <-lm(formula = update(formula_saturated, ~. + comId)  , data = Yield_Covariates_nna_plm)
LSDV_poly_Feb_trend_more <-lm(formula = update(formula_more, ~. + comId)  , data = Yield_Covariates_nna_plm)
LSDV_poly_Feb_trend <-lm(formula = update(formula, ~. + comId)  , data = Yield_Covariates_nna_plm)
LSDV_poly_Feb_step_trend <- lm(formula = update(formula_step, ~. + comId)  , data = Yield_Covariates_nna_plm)
## Diagnostics
summary(LSDV_poly_Feb_trend)

# Mutlicollinearity
vif(LSDV_poly_Feb_trend) # Grundsätzlich gibt es Multicollinearitätsproblem zwischen  PET und Tavg
vif(LSDV_poly_Feb_step_trend)

## Modelselection based on AIC ##
# erlaubt wohl nicht für die Auswahl des Grades des Polygons, zumindest in diesem Setting
# Alternativen aus ISLM Buch, ANOVA und Cross Validation
LSDV_poly_Feb_trend_saturated_step <-step(LSDV_poly_Feb_trend_null, scope=list(upper=LSDV_poly_Feb_trend_saturated), direction="both")
# multicollinearity
## Anova
anova(LSDV_poly_Feb_trend_saturated, LSDV_poly_Feb_trend_more, LSDV_poly_Feb_trend)
anova(LSDV_poly_Feb_trend_more, LSDV_poly_Feb_trend)

# Ressidual Plot
plot(residuals(FE_poly_Feb_trend))

# Mutlicollinearity
search()
vif(FE_poly_Feb_trend) 
pairs( siloMaize ~ poly(Prec_Feb,1, raw=T) + poly(Pet_Feb, 1, raw=T) +  poly(Tavg_Feb, 1, raw=T) + poly(SMI_Feb,1, raw=T) + poly(as.numeric(year)), Yield_Covariates_nna_plm)

# Plot Curve of SMI
names(Yield_Covariates_nna_plm)
coefficients(FE_poly_Feb_trend)
intercept <- mean(fixef(FE_poly_Feb_trend))
Yield_Februar <- function(x) intercept + coefficients(FE_poly_Feb_trend)[[4]]*x 
# + coefficients(FE_poly_Feb_trend)[[7]]* x^2
# + coefficients(FE_poly_Feb_trend)[[6]]*x^3 
plot(siloMaize,SMI_Feb, data= Yield_Covariates_nna_plm)
plot(Yield_Februar, 0,1, Marn = "Februar_SMI", xlab="SMI")



