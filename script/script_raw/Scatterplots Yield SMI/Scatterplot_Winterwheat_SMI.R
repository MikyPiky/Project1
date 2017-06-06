#### File Description ####
' - Data Manipulation
      - data before 1999 are deleted
      - winterwheat -> all variables which are not related to winterWheat are deleted
      - Data, which need to be intrapolated are deleted (if necessary, needs to be done before or in extra script)
      - new dataframe Yield_SMI_detrend
      - standardize the variables: reason: make coefficients comparable (two standardization procedures are applied, z-score and kernel estimation (SMI))
  - Produce Scatterplots for relationship per acre output of yield (annual) and SMI (monthly) for the years 1999 - 2010 
  -
  
'
## Input and Dependencies
' data/data_processed/Yield_SMI_detrend <- MergeSMI_Yield (starts in 1995)
'

## Description of wheat ##
'
from wikipedia(German):
- demanding on soil quality, climate and water supply
- winterwheat: 
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
      - Heute ist von keinen wesentlichen Einfluss der Produktionstechnik  auf die Ertragsteigerung auszugehen
      - BAyern: größte Weizenproduktionsfläche (500 000 ha und 3,5 Mio t)
      - Niedersachsen und Schleswig - Holstein: stärkste Ausdehnung der Weizenflächen seit 1950er
    - Vorteile Norddeutschland für Getreideanbau:
      - gute Böden, weniger Qualiätsweizen als in Bayern
      - KLimatische und geografisch bedingte Gegebenheiten Norddeutschland
        - ausgeglicheneres Klima: weniger extrem heiße Tage, mildere Winter, kühlere SOmmmer, ausgeglichernere Niederschlagsverteilung
        - ->Kornfüllungsphase meist länger
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
      - Weizen benötigt wegen der späten Abreife bis Ende Juli ausreichend Wasserversorung, entweder durch Bodenvorat (SMI) oder Niederschlag
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
      - need to account for all covariates to correctly identify the effect of climate on corn yields
      - needs variation in climate/SMI wihin the considered population
      - cross-sectionaly analysis of a specifc crop would incorporate how a farmer switches to other crop variety of the same crop (CCFS6)
          -> hier habe ich nicht die Größe Crop Yield, sondern die differenzierten Feldfruchtarten
      - need to account for all covariates to correctly identify the effect of SMI on corn yields (CCFS6)
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
              ->  check, SMI is changing over time
          - fixed-effects models are designed to study the causes of changes within a person [or entity] 
              ->  check change within each community
          - the estimated coefficients of the fixed-effects models cannot be biased because of omitted time-invariant characteristics 
              ->  for instance no need to control for time invariant factors as soil texture and elevation characteristica within a entity 
          - When using FE we assume that something within the individual may impact or bias the predictor or outcome variables and we need to control for this heterogeneity.
              ->  different communities have different preconditions
              ->  (CCFS6): recognizes for fundamental differences between spatial units and that it is difficult to account for all these differences explicitly in the model 
              ->  test whether it makes sense to controll for west vs east(ehemaliger LPG Strukturen in der Oststaaten) or to control for state dummies (wie wird agrarpolitik gestaltet?)
          - FE remove the effect of those time-invariant characteristics so we can assess the net effect of the predictors on the outcome variable.
          - (CCFS6): .. if covariates influence yield on a additive fashion
              ->  net effect of SMI (no timeinvariant impact)
          - Another important assumption of the FE model is that those time-invariant characteristics are unique to the individual 
            and should not be correlated with other individual characteristics.
              ->  this might be the case, because the soil quality might correlate over the entities
          - Each entity is different therefore the entity’s error term and the constant (which captures individual characteristics) should not be correlated with the others. If the 
              error terms are correlated, then FE is no suitable since inferences may not be correct and  you need to model that relationship (probably using random-effects), 
              this is the main rationale for the Hausman test (presented later on in this document).
          - (CCFS6):fixed effect is a dummy or indicator variable that is set to one if onservation from a group is included an are set to zero otherwise
                - cannot include time-invariant variable anymore: collinear with fixed effects
	              - in panel hedonic model cannot use average weather as explanatory combined with fixed effects 
                  because it is constant within a group and hence a linear multiple of the indicator variable
          - (CCFS6): f.e. equal to joint group-specific demeaning and the dependent as well as the independent variables
                - subtracting group specific avaerages for each variable and adjust the error in degrees of freedom 
                -	important for interpretation
                		- regression uses deviations from group-specific averages to identify the parameter of interest, here community specific averages
                		- equivalent to fit a regression line through each group where the slope is forced to be the same for each group but the intercept is allowed to vary by group
                		- all groups are forced to the same sensivity on the independent variable, i.e  here SMI
                		- allowing for a distribution of SMI sensitiveties -> random coefficient model  
          - (CCFS6): two ways of calculation fe-coefficients
                          - set dummy for each group, leave intercept out  (multicollinearity)
                          - demean each variablen (dependent and independent) and  run linear regression trought them
                          - demean means substract the entity specific average
                          - important for interpretation: regression uses deviations from entity specific averages to identify the parameter of interest
                          - equivalent to fit a regression line through each entity where the slope is forced to be the same for each entity but the intercept
                            is allowed to vary by entity
                          - all countries are forced to the same sensivity on the independenty variable
                          - allowing for a distribution of weather sensitiveties->random coefficient model
                          -> is demeaning some kind of normalization?


          - Aspekte der Anpassung und die resultierende Ökonomische Interpretation.
           
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
          - Frage: Correliert der SMI mit nicht beobachtbaren timeinvarianten Effekten auf den Ertrag
          - Hausmann Test:  null hypothesis is that the preferred model is random effects vs. the alternative the fixed effects; It basically tests whether the unique
                            errors (u i ) are correlated with the regressors, the null hypothesis is they are not
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
  - Nach Rohini: Um Vergleichbarkeit der Variablen zu gewährleisten sollte der SMI Normalisiert werden
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
  - stepwise regression
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
      ->  Do
  
## Kontrolle von räumlicher Autocorrelation ##
      -> Das erscheint mir generell relevant, sollte ich aber vorerst nach hinter verschieben.
      - splm Package

## Interaktionen ##

## Fragen:
  Kann ich model selection auf basis von lm und glm machen und dann zum Package plm übergehen, um fixed effects oder random effects Modelle zu schätzen?
  Dabei ist zu berücksichtigen, dass plm random auch mit lme geschätzt werden kann und dafür eine step Funktion angewendet werden kann.




'

## Output
'
WW_SMI

## Plots
Scatterplots Winterwheat SMI
'

## Packages
library("ggplot2")
library("plyr")
library("foreign")
library("plm")
library("car")
library("lmtest")
library("lattice")
library("zoo")
library("stringr")
library("scales")
library("nlme")
library("lme4")
library("mgcv")
library("apsrtable")
library("texreg")
library("reshape2")
library("gplots")
library("pracma")

############################################################################################################################################################################################
######################################################################## Laden der Daten ###################################################################################################
############################################################################################################################################################################################

Yield_Covariates <- read.csv("~/Documents/projects/correlation/data/data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax")
Yield_Covariates$X <- NULL

dim(Yield_Covariates)

## Get rid of data with _demean suffix
r <- names(Yield_Covariates)
rr <- grep("*demean", r, invert=T)
Yield_Covariates_nodemean <- Yield_Covariates[,rr]
dim(Yield_Covariates_nodemean)
class(Yield_Covariates_nodemean)
head(Yield_Covariates_nodemean)

#########################################################
#### Write Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax ####
#########################################################
write.csv(Yield_Covariates_nodemean, "data//data_processed/Yield_SMI_Prec_Tavg_Pet_Dem_Por_Tmin_Tmax_nodeam.csv")

############################################################################################################################################################################################
######################################################################################## Data Manipulation #################################################################################
############################################################################################################################################################################################

## Explore Winter Wheat Variable
head(Yield_SMI_detrend)
summary(Yield_SMI_detrend$winterWheat)
plot(Yield_SMI_detrend$winterWheat)

## Hier lösche ich alle Yield mit Wert 0, da dies nicht nachzuvollziehen ist
Yield_SMI_detrend$winterWheat[Yield_SMI_detrend$winterWheat==0] <- NA




############################################################################################################################################################################################
######################################################################################## Create new variables ##############################################################################
############################################################################################################################################################################################

###########################
#### Aggregates of SMI ####
###########################

## Calculate Growth Months of Winterwheat: Mai to July ##
Yield_SMI_detrend <-transform(Yield_SMI_detrend,SMI_Yearly_MaiToJul = (SMI_Mai + SMI_Jun + SMI_Jul)/3) #modifies existing dataframe
head(Yield_SMI_detrend)

## Calculate Average which account for signs in fixed effects regression over all SMI ##
Yield_SMI_detrend<-transform(Yield_SMI_detrend,SMI_Yearly_JanFebMar = (SMI_Jan + SMI_Feb + SMI_Mar)/3)
Yield_SMI_detrend<-transform(Yield_SMI_detrend,SMI_Yearly_AprMai = (SMI_Apr + SMI_Mai)/2)

## Calculate yearly average with lagged variables ##
Yield_SMI_detrend <- transform(Yield_SMI_detrend,SMI_YearlyAvg_lag =  (SMI_Dec_lag+SMI_Nov_lag+SMI_Oct_lag+ SMI_Sep_lag+ SMI_Jan+SMI_Feb+SMI_Mar+ SMI_Apr + SMI_Mai + SMI_Jun + SMI_Jul +  SMI_Aug)/12)

## Calculate yearly average with lagged variables ##
Yield_SMI_detrend <- transform(Yield_SMI_detrend,SMI_YearlyAvg =  (SMI_Dec+SMI_Nov+SMI_Oct+ SMI_Sep+ SMI_Jan+SMI_Feb+SMI_Mar+ SMI_Apr + SMI_Mai + SMI_Jun + SMI_Jul +  SMI_Aug)/12)



##########################################
#### Define names for guides in plots ####
federalStates <-guide_legend("Federal States")

#############################
#### Make list for loops ####
names(Yield_SMI_detrend)
SMI_month_list<-NULL

SMI_month_list[[1]]<- c("SMI_Jan", "SMI_Feb", "SMI_Mar", "SMI_Apr", "SMI_Mai", "SMI_Jun", "SMI_Jul", "SMI_Aug","SMI_Sep", "SMI_Oct", "SMI_Nov", "SMI_Dec")
SMI_month_list[[2]]<- c("SMI_Sep_lag", "SMI_Oct_lag", "SMI_Nov_lag", "SMI_Dec_lag","SMI_Jan", "SMI_Feb", "SMI_Mar", "SMI_Apr", "SMI_Mai", "SMI_Jun", "SMI_Jul", "SMI_Aug")
SMI_month_list[[3]] <- c("SMI_Yearly_MaiToJul",  "SMI_Yearly_JanFebMar",  "SMI_Yearly_AprMai",     "SMI_YearlyAvg" )
SMI_month_list[[4]] <-c("Jan","Feb","Mar","Apr","Mai","June","July","Aug","Sept","Oct","Nov","Dec")
SMI_month_list

###############################################
#### Define data.frame as Panel Data Frame ####
Yield_SMI_detrend <- plm.data(Yield_SMI_detrend, indexes = c("comId","year"))
head(Yield_SMI_detrend)


############################################################################################################################################################################################
#### Melt SMI of all months into one column #### 
## Purpose is faceting according to montly SMI 
names(Yield_SMI_detrend)
idvars = c("comId", "year","comIdState","comState","winterWheat" , "winterWheat_deZscore")
measurevars= SMI_month_list[[1]]
Yield_SMI_detrend_melt <- melt (Yield_SMI_detrend, id.vars= idvars, measure.vars = measurevars)

## with lagged variables 
measurevars= SMI_month_list[[2]]
Yield_SMI_detrend_melt_lag <- melt (Yield_SMI_detrend, id.vars= idvars, measure.vars = measurevars)


head(Yield_SMI_detrend_melt)
tail(Yield_SMI_detrend_melt)
table(Yield_SMI_detrend_melt$comState)
head(Yield_SMI_detrend_melt[Yield_SMI_detrend_melt$comId=="1001",])
tail(Yield_SMI_detrend_melt[Yield_SMI_detrend_melt$comId=="1001",])

any(Yield_SMI_detrend_melt[,"value"]!=Yield_SMI_detrend_melt[,"value"], na.rm = T)
all(Yield_SMI_detrend_melt[,"value"]==Yield_SMI_detrend_melt[,"value"], na.rm = T)

## Create variables, which only consider extreme values of SMI -> create subsets
Yield_SMI_detrend_melt_l01 <- subset( Yield_SMI_detrend_melt,Yield_SMI_detrend_melt$value <= 0.1 )
Yield_SMI_detrend_melt_l02 <- subset( Yield_SMI_detrend_melt,Yield_SMI_detrend_melt$value <= 0.2 )
Yield_SMI_detrend_melt_l03 <- subset( Yield_SMI_detrend_melt,Yield_SMI_detrend_melt$value <= 0.3 )
Yield_SMI_detrend_melt_l04 <- subset( Yield_SMI_detrend_melt,Yield_SMI_detrend_melt$value <= 0.4 )
Yield_SMI_detrend_melt_l05 <- subset( Yield_SMI_detrend_melt,Yield_SMI_detrend_melt$value <= 0.5 )

Yield_SMI_detrend_melt_m05 <- subset( Yield_SMI_detrend_melt,Yield_SMI_detrend_melt$value >= 0.5 )
Yield_SMI_detrend_melt_m06 <- subset( Yield_SMI_detrend_melt,Yield_SMI_detrend_melt$value >= 0.6 )
Yield_SMI_detrend_melt_m07 <- subset( Yield_SMI_detrend_melt,Yield_SMI_detrend_melt$value >= 0.7 )
Yield_SMI_detrend_melt_m08 <- subset( Yield_SMI_detrend_melt,Yield_SMI_detrend_melt$value >= 0.8 )
Yield_SMI_detrend_melt_m09 <- subset( Yield_SMI_detrend_melt,Yield_SMI_detrend_melt$value >= 0.9 )

head(Yield_SMI_detrend_melt_l01)

tail(Yield_SMI_detrend_melt_l01)

## Interpretation ##

''
#############################################################################################################################################################################################
###################
#### Plottting ####
###################
## Read Data ##
Yield_SMI_detrend <- read.csv( "data/data_processed/Yield_SMI_detrend.csv")

attach(Yield_SMI_detrend)
names(Yield_SMI_detrend)
##################
#### Montly SMI ##
##################


# ## Boxplot der monatlichen Durschschnitts-SMI pro Jahr (mit lag) mit Jitter der Daten
# SMI_month_list[[1]]
# qq_boxplot_SMImonths_lag<-lapply(SMI_month_list[[1]], function(i)  
#                   ggplot(Yield_SMI_detrend, aes_string("year", i, color="year")) +  
#                   geom_boxplot() + geom_jitter() + 
#                   geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
#                   geom_smooth(se=F, color="red", aes(group=1))+ 
#                   theme_bw() + ggtitle( "Boxplot der monatlichen Durschschnitts-SMI pro Jahr mit Jitter der Daten"))
# 
# qq_boxplot_SMImonths_lag
# 
# ## Boxplot der monatlichen Durschschnitts-SMI pro Jahr mit Jitter der Daten und facets der einzelen Bundesländer
# qq_boxplot_SMImonths_lag_facets  <-lapply(SMI_month_list[[1]], function(i)  
#                    ggplot(Yield_SMI_detrend, aes_string("year", i, color="year")) +  
# #                   geom_boxplot() +
#                    geom_jitter() + 
#                    geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
#                    geom_smooth(se=F, color="red", aes(group=1))+
#                    theme_bw() + 
#                    facet_wrap(~comState) +  
#                   ggtitle( "Boxplot der monatlichen Durschschnitts-SMI pro Jahr mit Jitter der Daten und facets der einzelen Bundesländer"))
# qq_boxplot_SMImonths_lag_facets

## Boxplot jährlicher Durchschnitts SMI (not lagged) mit Jitter der Daten
qq_boxplot_SMI_YearlyAvg  <- ggplot(Yield_SMI_detrend, aes_string("year", "SMI_YearlyAvg", color="year")) +
#                                       geom_boxplot() + 
                                      geom_jitter() +
                                      geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
                                      geom_smooth(se=F, color="red", aes(group=1))+
                                      theme_bw() + ggtitle( "Boxplot of yearly average of SMI over time")

qq_boxplot_SMI_YearlyAvg 
ggsave(file="./figures/figures_exploratory/Scatterplot_SMI/SMI_monthAvg_years.pdf", qq_boxplot_SMI_YearlyAvg, width=16, height=8)

## Boxplot jährlicher Durchschnitts SMI (lagged) mit Jitter der Daten
qq_boxplot_SMI_YearlyAvg_lag  <- ggplot(Yield_SMI_detrend, aes_string("year", "SMI_YearlyAvg_lag", color="year")) +
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

lapply(list, function(i) unique(Yield_SMI_detrend_melt[Yield_SMI_detrend_melt$comIdState==i,"comState"]))

comState <- c ("Schleswig-Holstein","Schleswig-Holstein","Brandenburg","Mecklenburg-Vorpommern","Sachsen","Sachsen-Anhalt","Thüringen","Niedersachsen","Bremen","NRW","Hessen","Rheinland-Pfalz","Baden-Württemberg","Bayern")

## SMI of months average over years
gg_boxplot_SMI_months <- ggplot(Yield_SMI_detrend_melt, aes(x=variable, y=value)) +
  geom_boxplot()  +
  geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
  geom_smooth(se=F, color="red", aes(group=1)) +
  theme_bw() + ggtitle(paste0("Montly SMi for each month from 1999 to 2010" ))

gg_boxplot_SMI_months

ggsave(gg_boxplot_SMI_months, file=".//figures/figures_exploratory/Scatterplot_SMI/SMImonths_avgOverMonths.pdf", width=32, height=16)


## SMI of months average over years, faceted by years
gg_boxplot_SMI_months_facetYear <- ggplot(Yield_SMI_detrend_melt[Yield_SMI_detrend_melt$comIdState== list[1],], aes(x=variable, y=value)) +
                                    geom_boxplot()  +
                                     facet_wrap(~year) +
                                    geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
                                    geom_smooth(se=F, color="red", aes(group=1)) +
                                      theme_bw() + ggtitle(paste0("Montly SMi for each month from 1999 to 2010 in ", print(comState[1])))
                                      
gg_boxplot_SMI_months_facetYear

gg_boxplot_SMI_months_facetYear_perYear <- 
  lapply(1:14, function(i) 
    ggplot(Yield_SMI_detrend_melt[Yield_SMI_detrend_melt$comIdState== list[i],], aes(x=variable, y=value)) +
      geom_boxplot()  +
      facet_wrap(~year) +
      geom_smooth(method = "lm", se=F, color="black", aes(group=1)) +
      geom_smooth(se=F, color="red", aes(group=1)) +
      theme_bw() + ggtitle(paste0("Montly SMi for each month from 1999 to 2010 in ", print(comState[i])))
  )

gg_boxplot_SMI_months_facetYear_perYear

## Boxplot der SMI nach Monaten unterteilt nach Jahren und Bundesländern - ergibt eine große Matrix##
gg_boxplot_SMI_months_facetComStateYear <- ggplot(Yield_SMI_detrend_melt, aes(x=variable, y=value)) +
                                    geom_boxplot() +
                                    facet_wrap(comState ~ year, ncol=12) + 
                                    geom_smooth(se=F, color="red", aes(group=1)) + 
                                    labs(x = "time", y = "SMI") +
                                    theme_bw() 
gg_boxplot_SMI_months_facetComStateYear 


# Die Parameter der Achsenbeschriftung und anderer Titel wurden hier für eine Presentation angepasst.

gg_boxplot_SMI_months_facetComStateYear + theme(strip.text.x = element_text(size = 12 ,hjust = 0.5, vjust = 0.5), strip.background = element_rect(fill = "white"),  axis.title=element_text(size=50 ,face="bold"))

ggsave(gg_boxplot_SMI_months_facetComStateYear, file=".//figures/figures_exploratory/Scatterplot_SMI/SMImonths_facetStateYear.pdf", width=32, height=20)

# gg_violon_SMI_months_comState <- ggplot(Yield_SMI_detrend_melt, aes(x=variable, y=value)) + geom_violin()  + facet_wrap(~comState)
# gg_violon_SMI_months_comState

############################################################################################################################################################################################
#####################################################
## WinterWheat and manipulated data of winterWheat ##

## Interessant Fragestellung
# Gibt es einen Trend -> dies kann eigentlich mit Ja beantwortet werden
# Wie wirkt sich die Normalisierung aus -> z- Transformation

names(Yield_SMI_detrend)

#################
## WinterWheat ##

## Plot winterwheat pro Jahr mit jitter ##
qq_boxplot_winterWheat  <- ggplot(Yield_SMI_detrend, aes_string("year", "winterWheat")) +
#                               geom_boxplot() +
                              geom_jitter() +
                              geom_smooth(method = "lm", se=F, color="green3", size=2, aes(group=1)) +
                              geom_smooth(se=F, color="red", size=2, aes(group=1))+
                              theme_bw() + ggtitle( "Boxplot winterwheat per Jahr")
qq_boxplot_winterWheat
ggsave(qq_boxplot_winterWheat, file=".//figures/figures_exploratory/winterWheat_deviation/winterwheat.pdf", width=32, height=20)

## Interpretation
'
In den Daten ist offensichtlich ein leichter Trend über die Zeit zu erkennen
'


## Boxplot winterwheat pro Jahr mit jitter und facets der einzelnen Bundesländer ##
qq_boxplot_winterWheat_facets<- qq_boxplot_winterWheat + facet_wrap(~comState) + ggtitle( "Boxplot winterwheat per Jahr")
qq_boxplot_winterWheat_facets


#################################
## Normalisiert:scoreWW_Kreise ##

names(Yield_SMI_detrend)
## Plot scoreWW pro Jahr ##
qq_boxplot_scoreWW  <- ggplot(Yield_SMI_detrend, aes_string("year", "winterWheat_deZscore")) +
#   geom_boxplot() +
  geom_jitter() +
  geom_smooth(method = "lm", se=F, color="blue", size=1, aes(group=1)) +
  geom_smooth( se=F, color="dodgerblue4", size=4, aes(group=1)) +
  labs(x = "Year", y = "Detrended and standardized winter wheat yield") +
  theme_bw()  +  theme(axis.text= element_text(size = 40 ,hjust = 0.5, vjust = 0.5), axis.title=element_text(size=50 ,face="bold"))

#  gtitle( "Boxplot and TimeSeries z-score of WinterWheat per year")
qq_boxplot_scoreWW 
ggsave(qq_boxplot_scoreWW, file=".//figures/figures_exploratory/winterWheat_deviation/scoreWW.pdf", width=32, height=20)

## Interpretation
'
Auch in den z-transformierten Daten kann Trend erkannt werden
'

# ########################################################
# ## Linear Detrend mit der detrend Funktion: detrendWW ##
# ## Plot detrendWW pro Jahr ##
# qq_boxplot_detrendWW  <- ggplot(Yield_SMI_detrend, aes_string("year", "detrendWW")) +
#   #                               geom_boxplot() +
#   geom_jitter() +
#   geom_smooth(method = "lm", se=F, color="green3", size=2, aes(group=1)) +
#   geom_smooth(se=F, color="red", size=2, aes(group=1))+
#   theme_bw() + ggtitle( "Boxplot and Time Series of detrended WinterWheat per year")
# qq_boxplot_detrendWW
# ggsave(qq_boxplot_detrendWW, file=".//figures/figures_exploratory/winterWheat_deviation/detrendWW.pdf", width=32, height=20)
# ## Interpretation
# ' There is still a Trend in the data: Do not exactly know why!?! Wahrscheinlich weil nicht alle Daten durch die Detrend Funktion Transformiert wurden
# '

## Plot detrendWW_Kreise pro Jahr ##
qq_boxplot_detrendWW_Kreise <- ggplot(Yield_SMI_detrend, aes_string("year", "detrendWW_Kreise")) +
  #                               geom_boxplot() +
  geom_jitter() +
  geom_smooth(method = "lm", se=F, color="green3", size=2, aes(group=1)) +
  geom_smooth(se=F, color="red", size=2, aes(group=1))+
  theme_bw() + ggtitle( "Boxplot and TimeSeries of detrended WinterWheat (for each Kreis) per year")
qq_boxplot_detrendWW_Kreise
ggsave(qq_boxplot_detrendWW_Kreise, file=".//figures/figures_exploratory/winterWheat_deviation/detrendWW_Kreise.pdf", width=32, height=20)
## Interpretation
' No trend in the Data.
'

## Plot winterWheat_deZscore pro Jahr ##
qq_boxplot_winterWheat_deZscore <- ggplot(Yield_SMI_detrend, aes_string("year", "winterWheat_deZscore")) +
  #                               geom_boxplot() +
  geom_jitter() +
  geom_smooth(method = "lm", se=F, color="green3", size=2, aes(group=1)) +
  geom_smooth(se=F, color="red", size=2, aes(group=1))+
  labs(x = "time", y = "Detrended and Standardized Yield (acre/ton) of Winterwheat") +
  theme_bw() 
# + ggtitle( "Detrended WinterWheat z-score (for each Kreis) per year")
qq_boxplot_winterWheat_deZscore
ggsave(qq_boxplot_winterWheat_deZscore, file=".//figures/figures_exploratory/winterWheat_deviation/winterWheat_deZscore.pdf", width=32, height=20)
ggsave(qq_boxplot_winterWheat_deZscore, file=".//figures/figures_exploratory/winterWheat_deviation/winterWheat_deZscore.png", width=32, height=20)
## Interpretation
' Diese Daten sehen eigentlich auch ganz gut aus, nur ist die Frage, ob wieder Daten bei der detrending nicht berücksichtigt wurden
'

qq_boxplot_winterWheat_deZscore_facets <- qq_boxplot_winterWheat_deZscore + facet_wrap(~comState, nrow=2)
qq_boxplot_winterWheat_deZscore_facets 
ggsave(qq_boxplot_winterWheat_deZscore_facets, file=".//figures/figures_exploratory/winterWheat_deviation/winterWheat_deZscore_facets.pdf", width=20, height=20)
ggsave(qq_boxplot_winterWheat_deZscore_facets, file=".//figures/figures_exploratory/winterWheat_deviation/winterWheat_deZscore_facets.png", width=20, height=20)

# ###################
# #### Histogram ####
# #### Histogramm der SMI der einzelnen Monate ###
# gg_histo_months  <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_SMI_detrend, aes_string(i)) +  geom_histogram(aes(fill=year))+ ggtitle(paste("Histogramm of", i)) + theme_bw())
# gg_histo_months
# 
# ###################
# #### barplots ####
# # ... der SMI Verteilung
# gg_barplot_SMImonths_lag  <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_SMI_detrend, aes_string(i, fill="year")) +  geom_bar(color = "black"))
# gg_barplot_SMImonths_lag 
# 
# 
# #######################
# #### density plots ####
# 
# ## Plots der der monatlichen durchschnittlichen SMI für alle Jahre
# gg_density_SMImonths_lag_alpha  <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_SMI_detrend, aes_string(i, fill="year")) +  geom_density(alpha=0.5))
# gg_density_SMImonths_lag_alpha 


# ######################
# #### Violin Plots ####
# 
# gg_violin_SMImonths_lag_stack  <-lapply(SMI_month_list[[1]][[1]], function(i)  ggplot(Yield_SMI_detrend, aes_string("year",i)) +  geom_violin(position="stack") )
# gg_violin_SMImonths_lag_stack
# 
# gg_violin_SMImonths_lag_prop <-lapply(SMI_month_list[[1]][[2]], function(i)  ggplot(Yield_SMI_detrend, aes_string("year",i)) + geom_violin(position="fill") )
# gg_violin_SMImonths_lag_prop
# 
# gg_violin_SMImonths_lag_absolut <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_SMI_detrend, aes_string("year",i)) +  geom_violin())
# gg_violin_SMImonths_lag_absolut

######################################
#### Scatterplots oder xy - Plots ####
'Diese Plots waren unten bei der Regressionsanalyse, nochmals überprüfen, was damit zu tun ist'

## Scatterplots des Zusammenhanges WinterWheat und SMI_MaiToJuni, aufgegliedert nach Jahren: linearer Zusammenhang
qp_Yearly_MaiToJul_lin<-qplot(SMI_Yearly_MaiToJul, winterWheat, data=Yield_SMI_detrend, facets=~year, color=year,geom=c("point", "smooth"), method="lm")+ ylab("Yearly Winter Wheat Yield in dt/ha") + ylab("Yearly Winter Wheat Yield in dt/ha") + ggtitle("Winterwheat, Linear, SMI_Yearly_MaiToJul")
qp_Yearly_MaiToJul_lin
summary(qp_Yearly_MaiToJul_lin)

## Scatterplots des Zusammenhanges WinterWheat und SMI_MaiToJuni, aufgegliedert nach Jahren: polygoner Zusammenhang
qp_Yearly_MaiToJul_pol<-qplot(SMI_Yearly_MaiToJul, winterWheat, data=Yield_SMI_detrend, facets=~year, color=factor(year),geom=c("point", "smooth"),shape=".")+ ylab("Yearly Winter Wheat Yield in dt/ha") + ylab("Yearly Winter Wheat Yield in dt/ha") + ggtitle("Winterwheat, Pol, SMI_Yearly_MaiToJul")
qp_Yearly_MaiToJul_pol
summary(qp_Yearly_MaiToJul_pol)



# #### plotmeans ####
# plotmeans(winterWheat ~ comId, Yield_SMI_detrend)
# plotmeans(winterWheat ~ comState, Yield_SMI_detrend)
# plotmeans(winterWheat ~ year, Yield_SMI_detrend)

## Interpretation ##
'
Die Scatterplots scheinen schon in den einzelnen Bundesländer eine Heterogenität bei den yields darzustellen.

Diese Heterogenität wird durch plotmeans nocheinmal bestätigt, sowohl auf Bundesländer als auch County Ebene.

Diese Heterogenität wird bei normalen OLS nicht berücksichtigt.
'

#########################################
## xyplots: winterWheat auf montly SMI ##
#########################################
##############################################
## Linear und Pooling: nur eine fitted line ##
gg_lin_months  <-lapply(SMI_month_list[[1]] , function(i)  ggplot(Yield_SMI_detrend, aes_string(x=i,y="winterWheat")) +  stat_smooth(method="lm", se=F ) + geom_point(shape = 5, size = 0.5)          
                        + ylab ("Per acre yield of Winterwheat") + ggtitle( "xy-Plot") + theme_bw() )
gg_lin_months 

###########################################################
## Linear mit unterschiedlichen fitted lines nach Jahren ##
gg_lin_months  <-lapply(SMI_month_list[[1]] , function(i)  ggplot(Yield_SMI_detrend, aes_string(x=i,y="winterWheat", color="year")) +  stat_smooth(method="lm", se=F ) + geom_point(shape = 5, size = 0.5)          
                        + ylab ("Per acre yield of Winterwheat") + ggtitle( "xy-Plot") + theme_bw() )
gg_lin_months 

# Interpretation
'
Die Coefficienten varieren. Am Anfang des Jahres sind sie negativ, danach werden sie tendenziell linear.

'


## Polygons
gg_pol_months_yearColor <- lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_SMI_detrend, aes_string(x=i,y="winterWheat", color="year")) +  stat_smooth(se=F) + geom_point(shape = 5, size = 0.5)          
                     + ylab ("Per acre yield of Winterwheat")  + ggtitle( "xy-Plot") + theme_bw() )
gg_pol_months_yearColor

## Polygons
gg_pol_months <- lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_SMI_detrend, aes_string(x=i,y="winterWheat")) +  stat_smooth(se=F) + geom_point(shape = 5, size = 0.5)          
                        + ylab ("Per acre yield of Winterwheat")  + ggtitle( "xy-Plot") + theme_bw() )
gg_pol_months



### Darstellung mit Facets ###
## Linear line
gg_lin_months_facetCom  <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_SMI_detrend, aes_string(x=i,y="winterWheat")) +  stat_smooth(method="lm", se=F ) + geom_point(shape = 5, size = 0.5)          
                        + ylab ("Per acre yield of Winterwheat")  + ggtitle( "xy-Plot") + theme_bw() + facet_wrap(~comState)) 
gg_lin_months_facetCom[1]

gg_lin_months_facetYear  <-lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_SMI_detrend, aes_string(x=i,y="winterWheat", color="comState")) +  stat_smooth(method="lm", se=F ) + geom_point(shape = 5, size = 0.5)          
                                 + ylab ("Per acre yield of Winterwheat")  + ggtitle( "xy-Plot") + theme_bw() + facet_wrap(~year)) 
gg_lin_months_facetYear[1]

## Polygons

gg_pol_months_facetCom <- lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_SMI_detrend, aes_string(x=i,y="winterWheat")) +  stat_smooth(se=F) + geom_point(shape = 5, size = 0.5)          
                        + ylab ("Per acre yield of Winterwheat")  + ggtitle( "xy-Plot") + theme_bw() + facet_wrap(~comState))
gg_pol_months_facetCom[1]

gg_pol_months_facetYear <- lapply(SMI_month_list[[1]], function(i)  ggplot(Yield_SMI_detrend, aes_string(x=i,y="winterWheat", color="year")) +  stat_smooth( se=F) + geom_point(shape = 5, size = 0.5)          
                                 + ylab ("Per acre yield of Winterwheat")  + ggtitle( "xy-Plot") + theme_bw() + facet_wrap(~year))
gg_pol_months_facetYear

########################################################
## Scatterplot winterWheat and SMI faceted by months ##

## Attention ## 
'Here I need to work with facets, because otherwise I have 12 data points on SMI for one Datapoint of Yield. For two dimensional plots, this is not a proper approach.'

#### WinterWheat, No lag SMI ####
## facet, nonlinear
names(Yield_SMI_detrend_melt)
ggplot_pol_SMI_facetMonths<- ggplot(Yield_SMI_detrend_melt, aes(x=value, y=winterWheat)) + 
                stat_smooth(color="red",size = 2, se=T) +
                geom_point(shape = 5, size = 0.5) +
                facet_wrap(~variable)+ ylab ("Per acre yield of Winterwheat")  +
                ggtitle("Scatterplot of WinterWheat and SMI") + theme_bw()
ggplot_pol_SMI_facetMonths
# Interpretation: maybe using quadratics might help 

## facet, linear
ggplot_lin_SMI_facetMonths<- ggplot(Yield_SMI_detrend_melt, aes(x=value, y=winterWheat)) + 
  stat_smooth(method="lm", color="red",size = 2, se=T) +
  geom_point(shape = 5, size = 0.5) +
  facet_wrap(~variable)+ ylab ("Per acre yield of Winterwheat")  +
  ggtitle( "xy-Plot") + theme_bw()
ggplot_lin_SMI_facetMonths

# #############
# ## Subsets ##
# 
# ## Subset: only values less then 0.1 
# ## Scatterplot winterWheat and SMI faceted by months ##
# ## facet, non linear
# names(Yield_SMI_detrend_melt)
# ggplot_pol_SMI_facetMonths_l01<- ggplot(Yield_SMI_detrend_melt_l01, aes(x=value, y=winterWheat)) + 
#   stat_smooth(color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   facet_wrap(~variable)+ ylab ("Per acre yield of Winterwheat")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_pol_SMI_facetMonths_l01
# 
# ## facet, linear
# names(Yield_SMI_detrend_melt)
# ggplot_lin_SMI_facetMonths_l01<- ggplot(Yield_SMI_detrend_melt_l01, aes(x=value, y=winterWheat)) + 
#   stat_smooth(method="lm",color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   facet_wrap(~variable)+ ylab ("Per acre yield of Winterwheat")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_lin_SMI_facetMonths_l01
# 
# ## nofacet, linear
# ggplot_lin_SMI_l01 <- ggplot(Yield_SMI_detrend_melt_l01, aes(x=(value), y=winterWheat)) + 
#   stat_smooth(method="lm",color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   ylab ("Per acre yield of Winterwheat")  +
#   xlab ("SMI") +
#   ggtitle( "Scatterplot of SMI less 0.1 and WinterWheat Yield") + theme_bw()
# ggplot_lin_SMI_l01
# ggsave(file="./figures/figures_exploratory/Scatterplot_Yield_SMI/Linear/l01.pdf", ggplot_lin_SMI_l01, width=16, height=12)
# 
# ## no facet, nonlinear
# ggplot_pol_SMI_l01 <- ggplot(Yield_SMI_detrend_melt_l01, aes(x=(value), y=winterWheat)) + 
#   stat_smooth(color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   ylab ("Per acre yield of Winterwheat")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_pol_SMI_l01
# 
# ## Subset: only values greater than 0.9 
# ## Scatterplot winterWheat and SMI faceted by months ##
# ## facet, non linear
# names(Yield_SMI_detrend_melt)
# ggplot_pol_SMI_facetMonths_m09<- ggplot(Yield_SMI_detrend_melt_m09, aes(x=value, y=winterWheat)) + 
#   stat_smooth(color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   facet_wrap(~variable)+ ylab ("Per acre yield of Winterwheat")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_pol_SMI_facetMonths_m09
# 
# ## facet, linear
# names(Yield_SMI_detrend_melt)
# ggplot_lin_SMI_facetMonths_m09<- ggplot(Yield_SMI_detrend_melt_m09, aes(x=value, y=winterWheat)) + 
#   stat_smooth(method="lm",color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   facet_wrap(~variable)+ ylab ("Per acre yield of Winterwheat")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_lin_SMI_facetMonths_m09
# 
# 
# 
# ## no facet, non linear
# ggplot_pol_SMI_m09 <- ggplot(Yield_SMI_detrend_melt_m09, aes(x=(value), y=winterWheat)) + 
#   stat_smooth(color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   ylab ("Per acre yield of Winterwheat")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_pol_SMI_m09
# 
# ## no facet, linear
# ggplot_lin_SMI_m09 <- ggplot(Yield_SMI_detrend_melt_m09, aes(x=(value), y=winterWheat)) + 
#   stat_smooth(method="lm",color="red",size = 2, se=T) +
#   geom_point(shape = 5, size = 0.5) +
#   ylab ("Per acre yield of Winterwheat")  +
#   ggtitle( "xy-Plot") + theme_bw()
# ggplot_lin_SMI_m09
# 
# 
#  
#####################################################################
## Scatterplot standscore of winterWheat and SMI faceted by months ##
## non lag
names(Yield_SMI_detrend_melt)
ggplot_pol_SMI_facetMonths<- ggplot(Yield_SMI_detrend_melt, aes(x=value, y=winterWheat_deZscore)) + 
  stat_smooth(color="red",size = 2, se=T) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3),color="green",size = 2, se=T) +
  geom_point(shape = 5, size = 0.5) +
  facet_wrap(~variable)+ ylab ("Per acre yield of Winterwheat")  +
  ggtitle("Scatterplot of WinterWheat and SMI") + theme_bw()
ggplot_pol_SMI_facetMonths

## lagged
names(Yield_SMI_detrend_melt)
ggplot_pol_SMI_facetMonths_lag<- ggplot(Yield_SMI_detrend_melt_lag, aes(x=value, y=winterWheat_deZscore)) + 
  geom_point(size = 0.4, alpha = 5/10) +
#   stat_smooth(color="yellow",size = 1, se=T, alpha = 8/10) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw=TRUE),color="darkblue",size = 1.5, se=T) +
   stat_smooth(method = "lm", formula = y ~ poly(x, 3, raw=TRUE),color="red",size = 1, se=T, alpha = 8/10) +
  facet_wrap(~variable)+ 
  ylab ("Detrended and Standardized Yield of Winterwheat")  + 
  xlab("SMI") +
#   ggtitle("Scatterplot of WinterWheat and SMI") + 
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
# head(Yield_SMI_detrend)
# 
# ## Plot ##
# qp_YearlyAvg_lin<-qplot(SMI_YearlyAvg, winterWheat, data=Yield_SMI_detrend, facets=~year, color=factor(year),geom=c("point", "smooth"), method="lm",shape=".")+ ylab("Yearly Winter Wheat Yield in dt/ha") + ggtitle("Winterwheat, Linear, SMI_YearlyAvg")
# summary(qp_Aug_lin)
# 
# qp_YearlyAvg_pol<-qplot(SMI_YearlyAvg, winterWheat, data=Yield_SMI_detrend, facets=~year, color=factor(year),geom=c("point", "smooth"),shape=".")+ ylab("Yearly Winter Wheat Yield in dt/ha") + ggtitle("Winterwheat, Pol, SMI_Yearly_Avg")
# summary(qp_Aug_lin)
# 
# 
# #### Plot the time series of each state ####
# qp_timeSeries_comIdState <-qplot(SMI_YearlyAvg, winterWheat, data=Yield_SMI_detrend, facets=~comIdState,  geom=c("point", "smooth"), method="lm",shape=".", se=TRUE)+ ylab("Yearly Winter Wheat Yield in dt/ha") + ggtitle("Winterwheat, Linear, Jan")
# 
# qp_timeSeries_comIdState <-qplot(SMI_Jan, year, data=Yield_SMI_detrend, facets=~comIdState,  geom=c("point", "smooth"), method="lm",shape=".", se=TRUE) + ylab("Yearly Winter Wheat Yield in dt/ha") + ggtitle("Winterwheat, Linear, Jan")
# 
# xyplot(SMI_YearlyAvg ~ year | comIdState,  data=Yield_SMI_detrend, type="l")
# 
# ## Nach diesem Plot hier scheinen Werte zu fehlen für SMI
# ## Nein, dass ich nicht der Grund, sondern dass auch die Endpunkte miteiandner verbunden werden
# 
# ## Time Series Plot der ComIdState==4, also Bremen und Bremerhaven ##
# ## Split
# Yield_SMI_detrend_4<-Yield_SMI_detrend[Yield_SMI_detrend$comIdState==4,]
# head(Yield_SMI_detrend_4)
# ## Apply ##
# ## plot
# plot(Yield_SMI_detrend_4$SMI_YearlyAvg, type="l")
# Yield_SMI_detrend_4_dt<-pdata.frame(Yield_SMI_detrend_4, index=c("year", "comId"))
# 
# 
# xyplot(SMI_YearlyAvg~year |comIdState, data= Yield_SMI_detrend_4_dt, type = "l")


###################################################
#### Growth Months of Winterwheat: Mai to July ####
###################################################
'
- detailed information needed

'



############################################################################################################################################################################################
################################################################################ Panel Data Approaches #####################################################################################
############################################################################################################################################################################################

## Na-omit ##
Yield_SMI_detrend_nna <- na.omit(Yield_SMI_detrend) 
# poly() does not allow NAs 


################################
### OLS Regression: Pooling ####
################################
#### Time: all years; Spatial: all communities ####

#################################
## Variable MI_Yearly_MaiToJul ##

# ## Model Estimation ##
# OLS_SMI_MaiToJul<-lm(formula=winterWheat_deZscore~SMI_Yearly_MaiToJul, data=Yield_SMI_detrend)
# summary(OLS_SMI_MaiToJul)
# 
# yhat<-OLS$fitted.values
# yhat
# 
# ## Plots ##
# with(Yield_SMI_detrend, plot(SMI_Yearly_MaiToJul, winterWheat_deZscore ))
# abline(OLS_SMI_MaiToJul,lwd=3, col="red")
# 
# ## gleiches Diagramm mit ggplot erstellt
# ## qp-plot of linear model ##
# gg_lin_months  <-lapply(SMI_month_list[[2]], function(i)  ggplot(Yield_SMI_detrend, aes_string(x=i,y="winterWheat_deZscore")) +  stat_smooth(method="lm", se=F ) + geom_point(shape = 5, size = 0.5) +
#                                                    ylab ("Per acre yield of winterWheat_deZscore") + ggtitle( "xy-Plot") + theme_bw() )
# gg_lin_months 
# 
# 
# # Interpretation #
# 'Diese Ansatz erklärt quasi keine Variation- Auch wird hier die heterogenität nicht berücksichtigt, sowohl in der Zeit als auch im Raum.
#  Darüber hinaus ist nur der Intercept significant-
#  Die Residuals scheinen relativ normal verteilt zu sein.
# '
# 
# plot(OLS_SMI_MaiToJul)
# # Interpretation
# ''

#######################################################################
## Variablen: all SMI_months, lag for months after harvest variables ##

## Modell Estimation ##
names(Yield_SMI_detrend)
## Basic linear model 
OLS_allSMI <-lm(formula=winterWheat_deZscore~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, data=Yield_SMI_detrend_nna)
# Diagnostics
vif(OLS_allSMI)
summary(OLS_allSMI$model)
summary(OLS_allSMI)
plot(OLS_allSMI)
anova(OLS_allSMI)

## Step Funktion for Variable Selection ##
# welche anhand der AIC Werte evaluiert, welche Variablen für das Modell das beste Ergebnis liefern
step(OLS_allSMI,data=Yield_SMI_detrend, direction="backward")
step(OLS_allSMI,data=Yield_SMI_detrend, direction="forward")
step(OLS_allSMI,data=Yield_SMI_detrend, direction="both")
# Interpretation: 

# Polygon 2.
names(Yield_SMI_detrend)
OLS_allSMI_poltest2 <-lm(formula=winterWheat_deZscore~SMI_Jan+I(SMI_Jan^2) + SMI_Feb+I(SMI_Feb^2) + SMI_Mar+I(SMI_Mar^2) + SMI_Apr+I(SMI_Apr^2) + SMI_Mai+I(SMI_Mai^2) +
                       SMI_Jun+I(SMI_Jun^2) + SMI_Jul+I(SMI_Jul^2) + SMI_Aug+I(SMI_Aug^2) + SMI_Sep_lag+I(SMI_Sep_lag^2) + SMI_Oct_lag+I(SMI_Oct_lag^2) + SMI_Nov_lag+I(SMI_Nov_lag^2) + 
                       SMI_Dec_lag+I(SMI_Dec_lag^2)  , data=Yield_SMI_detrend)
summary(OLS_allSMI_poltest2)

# Alternative version of poly 2. : ergibt nur minimale Abweichungen
# need to use Yield_SMI_detrend_nna, because of poly()

OLS_allSMI_pol2 <-lm(formula=winterWheat_deZscore~poly(SMI_Jan, 2 , raw=T) + poly(SMI_Feb, 2 , raw=T) + poly(SMI_Mar, 2 , raw=T) + poly(SMI_Apr, 2 , raw=T) + 
                          poly(SMI_Mai, 2 , raw=T) + poly(SMI_Jun, 2 , raw=T) + poly(SMI_Jul, 2 , raw=T) + poly(SMI_Aug, 2 , raw=T) + poly(SMI_Sep_lag, 2 , raw=T) + 
                          poly(SMI_Oct_lag, 2 , raw=T) + poly(SMI_Nov_lag, 2 , raw=T) + poly(SMI_Dec_lag, 2 , raw=T) ,  data=Yield_SMI_detrend_nna)
summary(OLS_allSMI_pol2)

step(OLS_allSMI_pol2,data=Yield_SMI_detrend, direction="backward")
step(OLS_allSMI_pol2,data=Yield_SMI_detrend, direction="forward")
step(OLS_allSMI_pol2,data=Yield_SMI_detrend, direction="both")

anova(OLS_allSMI, OLS_allSMI_pol2)

## Polygon 3
OLS_allSMI_pol3 <-lm(formula=winterWheat_deZscore~poly(SMI_Jan, 3 , raw=T) + poly(SMI_Feb, 3 , raw=T) + poly(SMI_Mar, 3 , raw=T) + poly(SMI_Apr, 3 , raw=T) + 
                       poly(SMI_Mai, 3 , raw=T) + poly(SMI_Jun, 3 , raw=T) + poly(SMI_Jul, 3 , raw=T) + poly(SMI_Aug, 3 , raw=T) + poly(SMI_Sep_lag, 3 , raw=T) + 
                       poly(SMI_Oct_lag, 3 , raw=T) + poly(SMI_Nov_lag, 3 , raw=T) + poly(SMI_Dec_lag, 3 , raw=T) ,  data=Yield_SMI_detrend_nna)
summary(OLS_allSMI_pol3)

step(OLS_allSMI_pol3,data=Yield_SMI_detrend, direction="backward")
step(OLS_allSMI_pol3,data=Yield_SMI_detrend, direction="forward")
step(OLS_allSMI_pol3,data=Yield_SMI_detrend, direction="both")

anova(OLS_allSMI_pol2 ,OLS_allSMI_pol3)

###########################################################################################################################################################################################
## plm package ##

## Polygon 3 with FE
OLS_allSMI_pol3_fe <-plm(formula=winterWheat_deZscore~poly(SMI_Jan, 3 , raw=T) + poly(SMI_Feb, 3 , raw=T) + poly(SMI_Mar, 3 , raw=T) + poly(SMI_Apr, 3 , raw=T) + 
                       poly(SMI_Mai, 3 , raw=T) + poly(SMI_Jun, 3 , raw=T) + poly(SMI_Jul, 3 , raw=T) + poly(SMI_Aug, 3 , raw=T) + poly(SMI_Sep_lag, 3 , raw=T) + 
                       poly(SMI_Oct_lag, 3 , raw=T) + poly(SMI_Nov_lag, 3 , raw=T) + poly(SMI_Dec_lag, 3 , raw=T) ,  data=Yield_SMI_detrend_nna, index=c("comId", "year"), model="within")
summary(OLS_allSMI_pol3_fe)

## Polygon 3 with RE
OLS_allSMI_pol3_re <-plm(formula=winterWheat_deZscore~poly(SMI_Jan, 3 , raw=T) + poly(SMI_Feb, 3 , raw=T) + poly(SMI_Mar, 3 , raw=T) + poly(SMI_Apr, 3 , raw=T) + 
                           poly(SMI_Mai, 3 , raw=T) + poly(SMI_Jun, 3 , raw=T) + poly(SMI_Jul, 3 , raw=T) + poly(SMI_Aug, 3 , raw=T) + poly(SMI_Sep_lag, 3 , raw=T) + 
                           poly(SMI_Oct_lag, 3 , raw=T) + poly(SMI_Nov_lag, 3 , raw=T) + poly(SMI_Dec_lag, 3 , raw=T) ,  data=Yield_SMI_detrend_nna, index=c("comId", "year"), model="random")
summary(OLS_allSMI_pol3_re)
# Error

## Polygon 2 with RE
## Offensichtlich kann man RE nicht mir demeand und detrended daten benutzen
names(Yield_SMI_detrend)
OLS_allSMI_pol2_re <-plm(formula=winterWheat~poly(SMI_Jan, 2 , raw=T) + poly(SMI_Feb, 2 , raw=T) + poly(SMI_Mar, 2 , raw=T) + poly(SMI_Apr, 2 , raw=T) + 
                       poly(SMI_Mai, 2 , raw=T) + poly(SMI_Jun, 2 , raw=T) + poly(SMI_Jul, 2 , raw=T) + poly(SMI_Aug, 2 , raw=T) + poly(SMI_Sep_lag, 2 , raw=T) + 
                       poly(SMI_Oct_lag, 2 , raw=T) + poly(SMI_Nov_lag, 2 , raw=T) + poly(SMI_Dec_lag, 2 , raw=T) ,  data=Yield_SMI_detrend_nna,index=c("comId", "year"), model="random")


OLS_allSMI_poltest2_re <-plm(formula=winterWheat~SMI_Jan+I(SMI_Jan^2) + SMI_Feb+I(SMI_Feb^2) + SMI_Mar+I(SMI_Mar^2) + SMI_Apr+I(SMI_Apr^2) + SMI_Mai+I(SMI_Mai^2) +
                           SMI_Jun+I(SMI_Jun^2) + SMI_Jul+I(SMI_Jul^2) + SMI_Aug+I(SMI_Aug^2) + SMI_Sep_lag+I(SMI_Sep_lag^2) + SMI_Oct_lag+I(SMI_Oct_lag^2) + SMI_Nov_lag+I(SMI_Nov_lag^2) + 
                           SMI_Dec_lag+I(SMI_Dec_lag^2)  , data=Yield_SMI_detrend, index=c("comId", "year"), model="random")
summary(OLS_allSMI_poltest2_re)


#############################################
#### Least Squares Dummy Variable Modell ####

## Variablen Special: Variablen, die bei pooled regression besonders relevant sind:  ##
## Linear Model mit dummy für comIDs ohne intercept: Wenn dummys für jedes Land berücksichtigt werden und ein Intercept, dann perfekte multicollinearität ##
lsdv_dum_special<-lm(formula=winterWheat_deZscore~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Jul+SMI_Sep_lag + factor(comId) -1, data=Yield_SMI_detrend)
summary(lsdv_dum_special)

## Step funktion with 
names(Yield_SMI_detrend)
fixed_plm_special<- plm(formula=winterWheat_deZscore~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Jul+SMI_Sep_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="within", na.action=na.omit)
summary(fixed_plm_special)
# Interpretation
'
Beim Umstieg von dummy Ansatz zu fixed effects aus plm Paket ändert sich der R-square, die Variation wird nicht mehr so gut erklärt.
'

###############################################
## level -level Specification, fixed effects ##
## LM with factor variables ##
fixed_lm_allSMI_levlev<-lm(formula=winterWheat_deZscore~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag + factor(comId) -1, data=Yield_SMI_detrend)
summary(fixed_lm_allSMI_levlev)
plot(fixed_lm_allSMI_levlev)

## Pooled with plm ##
pooled_plm_allSMI_levlev<- plm(formula=winterWheat_deZscore~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="pooling")
summary(pooled_plm_allSMI_levlev)

  
## Fixed Effects with plm and all SMI variables ##
fixed_plm_allSMI_levlev<- plm(formula=winterWheat_deZscore~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="within")
summary(fixed_plm_allSMI_levlev)

## Fixed Effect, whereby the non significant variables are dropped ##
fixed_plm_lessSMI_levlev<- plm(formula=winterWheat_deZscore~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Jun+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="within")
summary(fixed_plm_lessSMI_levlev)

## Fixed Effects whereby only strong significant variables are kept ##
fixed_plm_strongSMI_levlev<- plm(formula=winterWheat_deZscore~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, model="within")
summary(fixed_plm_strongSMI_levlev)


## Tables of the summaries ##
screenreg(list(fixed_lm_allSMI_levlev,pooled_plm_allSMI_levlev, fixed_plm_allSMI_levlev, fixed_plm_lessSMI_levlev, fixed_plm_strongSMI_levlev) , 
          custom.model.names = c("OLS FE", "PLM All Pooled", "PLM ALL FE", "PLM LESS FE", "PLM STRONG FE"),  
          caption            = "Level-Level Specification",
          scalebox= 0.4,
          omit.coef = "factor")


table_fe_levlev <- texreg(list(fixed_lm_allSMI_levlev,pooled_plm_allSMI_levlev, fixed_plm_allSMI_levlev, fixed_plm_lessSMI_levlev, fixed_plm_strongSMI_levlev) , 
                          custom.model.names = c("OLS FE", "PLM All Pooled", "PLM ALL FE", "PLM LESS FE", "PLM STRONG FE"),  
                          caption            = "Level-Level Specification",
                          scalebox= 0.4,
                          omit.coef = "factor")

### Hier fehlt noch die berücksichtigung der F-statistic, welce insgesamt interessant wäre.

table_fe_levlev


###########################################
# log-level Specification, fixed effects ##

## LM with factor variables ##
fixed_lm_allSMI_loglev<-lm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag + factor(comId) -1, data=Yield_SMI_detrend)
summary(fixed_lm_allSMI_loglev)
plot(fixed_lm_allSMI_loglev)

## Pooled with plm ##
pooled_plm_allSMI_loglev<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="pooling")
summary(pooled_plm_allSMI_loglev)
pooled_plm_allSMI_loglev$

## Fixed Effects with plm and all SMI variables ##
fixed_plm_allSMI_loglev<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="within")
summary(fixed_plm_allSMI_loglev)

## Fixed Effect, whereby the non significant variables are dropped ##
fixed_plm_lessSMI_loglev<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Jun+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="within")
summary(fixed_plm_lessSMI_loglev)


## Fixed Effects whereby only strong significant variables are kept ##
fixed_plm_strongSMI_loglev<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, model="within")
summary(fixed_plm_strongSMI_loglev)

## Fixed Effects whereby only strong significant variables are kept, twoways: individual and time fixed effect ##
fixed_plm_strongSMI_loglev_twoway<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Dec_lag, data=Yield_SMI_detrend, model="within")
# Schätzen dauert relativ lange
summary_fixed_plm_strongSMI_loglev_twoway<-summary(fixed_plm_strongSMI_loglev_twoway)


## Tables of the summaries ##
screenreg(list(fixed_lm_allSMI_loglev,pooled_plm_allSMI_loglev, fixed_plm_allSMI_loglev, fixed_plm_lessSMI_loglev, fixed_plm_strongSMI_loglev) , 
       custom.model.names = c("OLS FE", "PLM All Pooled", "PLM ALL FE", "PLM LESS FE", "PLM STRONG FE"),  
       caption            = "loglev Specification",
       scalebox= 0.4,
       omit.coef = "factor")


table_fe_loglev <- texreg(list(fixed_lm_allSMI_loglev,pooled_plm_allSMI_loglev, fixed_plm_allSMI_loglev, fixed_plm_lessSMI_loglev, fixed_plm_strongSMI_loglev) , 
                          custom.model.names = c("OLS FE", "PLM All Pooled", "PLM ALL FE", "PLM LESS FE", "PLM STRONG FE"),  
                          caption            = "loglev Specification",
                          scalebox= 0.4,
                          omit.coef = "factor")

### Hier fehlt noch die berücksichtigung der F-statistic, welce insgesamt interessant wäre.

table_fe_loglev


###############################################
## level -level Specification, random effects ##

# ## LM with factor variables ##
fixed_lm_allSMI_levlev<-lm(formula=winterWheat_deZscore~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag + factor(comId) -1, data=Yield_SMI_detrend)
summary(fixed_lm_allSMI_levlev)
plot(fixed_lm_allSMI_levlev)
step(fixed_lm_allSMI_levlev)

# Pooled with plm ##
pooled_plm_allSMI_levlev <- plm(formula=winterWheat_deZscore~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="pooling")
summary(pooled_plm_allSMI_levlev)

  
## random Effects with plm and all SMI variables ##
random_plm_allSMI_levlev<- plm(formula=winterWheat_deZscore~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="random")
summary(random_plm_allSMI_levlev)

## random Effect, whereby the non significant variables are dropped ##
random_plm_lessSMI_levlev<- plm(formula=winterWheat_deZscore~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Jun+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="random")
summary(random_plm_lessSMI_levlev)

## random Effects whereby only strong significant variables are kept ##
random_plm_strongSMI_levlev<- plm(formula=winterWheat_deZscore~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Dec_lag, data=Yield_SMI_detrend, model="random")
summary(random_plm_strongSMI_levlev)


## Tables of the summaries ##
screenreg(list(pooled_plm_allSMI_levlev, random_plm_allSMI_levlev, random_plm_lessSMI_levlev, random_plm_strongSMI_levlev) , 
          custom.model.names = c("PLM All Pooled", "PLM ALL RE", "PLM LESS RE", "PLM STRONG RE"),  
          caption            = "Level-Level Specification, random effects",
          scalebox= 0.4,
          omit.coef = "factor")


table_re_levlev <- texreg(list(pooled_plm_allSMI_levlev, random_plm_allSMI_levlev, random_plm_lessSMI_levlev, random_plm_strongSMI_levlev) , 
                          custom.model.names = c("PLM All Pooled", "PLM ALL RE", "PLM LESS RE", "PLM STRONG RE"),  
                          caption            = "Level-Level Specification, random effects",
                          scalebox= 0.4,
                          omit.coef = "factor")

############################################
# log-level Specification, random effects ##

# ## LM with factor variables ##
# random_lm_allSMI_loglev<-lm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag + factor(comId) -1, data=Yield_SMI_detrend)
# summary(random_lm_allSMI_loglev)
# plot(random_lm_allSMI_loglev)

## Pooled with plm ##
pooled_plm_allSMI_loglev<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="pooling")
summary(pooled_plm_allSMI_loglev)

  
## random Effects with plm and all SMI variables ##
random_plm_allSMI_loglev<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Feb+SMI_Mar+SMI_Apr+SMI_Mai+SMI_Jun+SMI_Jul+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="random")
summary(random_plm_allSMI_loglev)

## random Effect, whereby the non significant variables are dropped ##
random_plm_lessSMI_loglev<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Jun+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Nov_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, index=c("comId", "year"),model="random")
summary(random_plm_lessSMI_loglev)

## random Effects whereby only strong significant variables are kept ##
random_plm_strongSMI_loglev<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag-1+SMI_Dec_lag, data=Yield_SMI_detrend, model="random")
summary(random_plm_strongSMI_loglev)

## Tables of the summaries ##
screenreg(list(pooled_plm_allSMI_loglev, random_plm_allSMI_loglev, random_plm_lessSMI_loglev, random_plm_strongSMI_loglev) , 
          custom.model.names = c("PLM All Pooled", "PLM ALL RE", "PLM LESS RE", "PLM STRONG RE"),  
          caption            = "loglev Specification, random",
          scalebox= 0.4,
          omit.coef = "factor")


table_re_loglev <- texreg(list(pooled_plm_allSMI_loglev, random_plm_allSMI_loglev, random_plm_lessSMI_loglev, random_plm_strongSMI_loglev, random_plm_strongSMI_loglev_laggedAfterHarvest,
                               random_plm_strongSMI_loglev_beforeHarvest) , 
                          custom.model.names = c("PLM All Pooled", "PLM ALL RE", "PLM LESS RE", "PLM STRONG RE", "PLM STRONG RE LAGGED FALL",  "PLM STRONG RE NO FALL"),  
                          caption            = "loglev Specification, random",
                          scalebox= 0.4,
                          omit.coef = "factor")

table_re_loglev

### Hier fehlt noch die berücksichtigung der F-statistic, welce insgesamt interessant wäre.

table_re_loglev




#############################################################################
## log-level Specification, random effects, lagged variables after harvest ##

## fixed effects to set a reference for fixed effects ##
names(Yield_SMI_detrend)
fixed_plm_allSMI_loglev_laggedAfterHarvest<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Feb + SMI_Mar + SMI_Apr+ SMI_Mai + SMI_Jun+ SMI_Jul + SMI_Aug + SMI_Sep_lag+ SMI_Oct_lag + SMI_Nov_lag + SMI_Dec_lag, data=Yield_SMI_detrend, model="within",na.action=na.omit)
summary(fixed_plm_allSMI_loglev_laggedAfterHarvest)

fixed_plm_allSMI_loglev_laggedAfterHarvest<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Feb + SMI_Mar + SMI_Apr+ SMI_Mai + SMI_Jun+ SMI_Jul + SMI_Aug + SMI_Sep_lag+ SMI_Oct_lag + SMI_Nov_lag + SMI_Dec_lag, data=Yield_SMI_detrend, model="within")
summary(fixed_plm_allSMI_loglev_laggedAfterHarvest)

## random effects with all variables, but variables after harversting months are lagged ##
random_plm_allSMI_loglev_laggedAfterHarvest<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Feb + SMI_Mar + SMI_Apr+ SMI_Mai + SMI_Jun+ SMI_Jul + SMI_Aug + SMI_Sep_lag+ SMI_Oct_lag + SMI_Nov_lag + SMI_Dec_lag, data=Yield_SMI_detrend, model="random")
summary(random_plm_allSMI_loglev_laggedAfterHarvest)

## random effects with no non-signifiantet variables, but variables after harversting months are lagged ##
random_plm_lessSMI_loglev_laggedAfterHarvest<- plm(log(winterWheat_deZscore)~SMI_Jan+ SMI_Mar + SMI_Apr + SMI_Jun+ SMI_Jul + SMI_Aug +  SMI_Oct_lag + SMI_Nov_lag + SMI_Dec_lag, data=Yield_SMI_detrend, model="random")
summary(random_plm_lessSMI_loglev_laggedAfterHarvest)

## random Effects with only strong variables, but variables of months after harvest are lagged
random_plm_strongSMI_loglev_laggedAfterHarvest<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+ SMI_Mar + SMI_Apr + SMI_Jul + SMI_Aug +  SMI_Oct_lag + SMI_Nov_lag + SMI_Dec_lag, data=Yield_SMI_detrend, model="random")
summary(random_plm_strongSMI_loglev_laggedAfterHarvest)

## random Effects with Jan and Mar
random_plm_JanMarSMI_loglev_laggedAfterHarvest<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+ SMI_Mar , data=Yield_SMI_detrend, model="random")
summary(random_plm_JanMarSMI_loglev_laggedAfterHarvest)

## random Effects with Apr and Jul
random_plm_AprJulSMI_loglev_laggedAfterHarvest<- plm(formula=log(winterWheat_deZscore)~SMI_Apr+ SMI_Jul , data=Yield_SMI_detrend, model="random")
summary(random_plm_AprJulSMI_loglev_laggedAfterHarvest)

## random Effects with Aug
random_plm_AugSMI_loglev_laggedAfterHarvest<- plm(formula=log(winterWheat_deZscore)~SMI_Aug , data=Yield_SMI_detrend, model="random")
summary(random_plm_AugSMI_loglev_laggedAfterHarvest)

## random Effects with Oct_lag Nov_lag Dec_Lag
random_plm_OctNovDecSMI_loglev_laggedAfterHarvest<- plm(formula=log(winterWheat_deZscore)~SMI_Oct_lag + SMI_Nov_lag + SMI_Dec_lag , data=Yield_SMI_detrend, model="random")
summary(random_plm_OctNovDecSMI_loglev_laggedAfterHarvest)

## random Effects with only strong variables before harvest
random_plm_strongSMI_loglev_beforeHarvest<- plm(formula=log(winterWheat_deZscore)~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Aug+SMI_Sep_lag, data=Yield_SMI_detrend, model="random")
summary(random_plm_strongSMI_loglev_beforeHarvest)

## random Effects with only strong variables before harvest, no januar
random_plm_strongSMI_loglev_beforeHarvest<- plm(formula=log(winterWheat_deZscore)~SMI_Mar+SMI_Apr, data=Yield_SMI_detrend, model="random")
summary(random_plm_strongSMI_loglev_beforeHarvest)

## Tables of the summaries ##
screenreg(list(fixed_plm_allSMI_loglev_laggedAfterHarvest,random_plm_allSMI_loglev_laggedAfterHarvest, random_plm_lessSMI_loglev_laggedAfterHarvest, random_plm_strongSMI_loglev_laggedAfterHarvest,
               random_plm_JanMarSMI_loglev_laggedAfterHarvest) , 
#           custom.model.names = c( fixed_plm_allSMI_loglev_laggedAfterHarvest,random_plm_allSMI_loglev_laggedAfterHarvest, random_plm_lessSMI_loglev_laggedAfterHarvest, random_plm_strongSMI_loglev_laggedAfterHarvest,
random_plm_JanMarSMI_loglev_laggedAfterHarvest),  
          caption            = "loglev Specification, random",
          scalebox= 0.4,
          omit.coef = "factor")


table_re_loglev <- texreg(list(pooled_plm_allSMI_loglev, random_plm_allSMI_loglev, random_plm_lessSMI_loglev, random_plm_strongSMI_loglev, random_plm_strongSMI_loglev_laggedAfterHarvest,
                               random_plm_strongSMI_loglev_beforeHarvest) , 
                          custom.model.names = c("PLM All Pooled", "PLM ALL RE", "PLM LESS RE", "PLM STRONG RE", "PLM STRONG RE LAGGED FALL",  "PLM STRONG RE NO FALL"),  
                          caption            = "loglev Specification, random",
                          scalebox= 0.4,
                          omit.coef = "factor")

table_re_loglev




#######################
## LME - Estimation  ##
## random effects with all variables, but variables after harversting months are lagged ##
random_lme_allSMI_loglev_laggedAfterHarvest<- lme(log(winterWheat_deZscore)~SMI_Jan+SMI_Feb + SMI_Mar + SMI_Apr+ SMI_Mai + SMI_Jun+ SMI_Jul + SMI_Aug + SMI_Sep_lag+ SMI_Oct_lag + SMI_Nov_lag + SMI_Dec_lag, data=Yield_SMI_detrend, random = ~1 | comId, , na.action=na.omit)
summary(random_lme_allSMI_loglev_laggedAfterHarvest)
AIC(random_lme_allSMI_loglev_laggedAfterHarvest)


## Tables of summaries ##
screenreg(random_lme_allSMI_loglev_laggedAfterHarvest)







### Hier fehlt noch die berücksichtigung der F-statistic, welce insgesamt interessant wäre.

table_re_loglev

####################################################################################
#### statt log-level Specification: use demean for winterWheat_deZscore , random effects ####

pooling_plm_strongSMI_demean<- plm(formula=demeanWW~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, model="pooling")
summary(pooling_plm_strongSMI_demean)

## Funktioniert nicht mit random ##
fixed_plm_strongSMI_demean<- plm(formula=demeanWW~SMI_Jan+SMI_Mar+SMI_Apr+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Dec_lag, , data=Yield_SMI_detrend, model="within")
summary(fixed_plm_strongSMI_demean)

####################################################################################
#### statt log-level Specification: use demean for winterWheat_deZscore , random effects ####
random_plm_strongSMI_weird<- plm(formula=log(winterWheat_deZscore)~exp(SMI_Jan)+exp(SMI_Mar)+exp(SMI_Apr)+exp(SMI_Aug)+exp(SMI_Sep_lag)+exp(SMI_Oct_lag)+exp(SMI_Dec_lag), data=Yield_SMI_detrend, model="random")
summary(random_plm_strongSMI_weird)

random_plm_strongSMI_exp<- plm(formula=winterWheat_deZscore~exp(SMI_Jan+SMI_Mar+SMI_Apr+SMI_Aug+SMI_Sep_lag+SMI_Oct_lag+SMI_Dec_lag), data=Yield_SMI_detrend, model="random")
summary(random_plm_strongSMI_exp)

#### Steht noch aus ####
'
Wie sieht es denn mit lagged variablen aus, vor allem der Monate nach der Ernte

'

############################################################################################################################################################################################
#### Table with important results ####
' pooled, plm all fe, plm all re, plm strong fe, plm strong re'

## loglev Specification ##
table_important_loglev <- texreg(list(pooled_plm_allSMI_loglev, fixed_plm_allSMI_loglev, random_plm_allSMI_loglev, fixed_plm_strongSMI_loglev, random_plm_strongSMI_loglev) , 
                          custom.model.names = c("PLM All Pooled", "PLM ALL FE", "PLM ALL RE","PLM STRONG FE","PLM STRONG RE" ),  
                          caption            = "Important Results of loglev Specification",
                          scalebox= 0.4,
                          omit.coef = "factor")
table_important_loglev

## levlev Specification ##
table_important_levlev <- texreg(list(pooled_plm_allSMI_levlev, fixed_plm_allSMI_levlev, random_plm_allSMI_levlev, fixed_plm_strongSMI_levlev, random_plm_strongSMI_levlev) , 
                                 custom.model.names = c("PLM All Pooled", "PLM ALL FE", "PLM ALL RE","PLM STRONG FE","PLM STRONG RE" ),  
                                 caption            = "Important Results of levlev Specification",
                                 scalebox= 0.4,
                                 omit.coef = "factor")
table_important_levlev





############################################################################################################################################################################################
##################
#### Testing #####
##################
'
Tests for serial autocorrealtion and heteroskefasdiscitry: ->fgls approach


'

################################################
#### Fixed or Random Effects: Hausmann Test ####

phtest_levlev<-phtest(fixed_plm_strongSMI_levlev, random_plm_strongSMI_levlev)
phtest_loglev<-phtest(fixed_plm_strongSMI_loglev, random_plm_strongSMI_loglev)
'
Der Hausmann Test besagt, dass die Null, nämlich das Model ist Random Effects, nicht abgelehnt werden kann.
Daher macht wohl random effects mehr Sinn

'

############################################################################
#### Testing for random effects: Breusch-Pagan Lagrange multiplier (LM) ####
plmtest(pooled_plm_allSMI_loglev, type=c("bp"))
# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
'
Here we can reject the null, that there is no panel effect. Therefore random effects is the preferred model.
'

#############





########################################################
#### Time: seperate years; Spatial: all communities ####
#### Linear Models for the seperate years  ####
'
Dazu brauche ich die Daten der einzelnen Jahre, sowohl SMI als auch Yield
'

#### Loop über die Jahre 1999 bis 2010 ####
list1<-seq(1999,2010,1)
length(list1)
list2<-paste("lm_MaiToJul_lin", list, sep="_")
paste(list2[1])
length(list2)


lm_MaiToJul_lin_99to10<-list()
Anova_lm_MaiToJul_lin_99to10<-NULL

for (i in 1:12)
{ 
  print(list2[[i]])
  # lm_MaiToJul_lin[i]<-list2[i]
  ## Linear Model ##
  lm_MaiToJul_lin_99to10<- lm(formula=winterWheat_deZscore~SMI_Yearly_MaiToJul, year==list1[[i]], data=Yield_SMI_detrend)
  lm_MaiToJul_lin_99to10[[i]] <-summary(lm_MaiToJul_lin_99to10)
  
#   ## Plot the data and the fitted line ##
#   with(Yield_SMI_detrend[Yield_SMI_detrend$year==list1[[i]],], plot(SMI_Yearly_MaiToJul, winterWheat_deZscore, main=list1[[i]]) )
#   abline(lm_2003_MaiToJul_lin)
#   
#   ## Anova ##
#   Anova_lm_MaiToJul_lin_99to10[[i]]<- anova(lm_MaiToJul_lin_99to10[[i]])
  
  
}

lm_MaiToJul_lin_99to10
lm_MaiToJul_lin_99to10
list2[[1]]
Anova_lm_MaiToJul_lin_99to10[[1]]




lm_2003_MaiToJul_lin<-lm(formula=winterWheat_deZscore~SMI_Yearly_MaiToJul, year==2003, data=Yield_SMI_detrend)
anova(lm_2003_MaiToJul_lin)
plot(lm_2003_MaiToJul_lin)
summary(lm_2003_MaiToJul_lin)

## Plotten der fitted line in den Daten ##
with(Yield_SMI_detrend[Yield_SMI_detrend$year==2003,], plot(SMI_Yearly_MaiToJul, detrendScoreWW, main="test"))
t<-t+ abline(lm_2003_MaiToJul_lin)
t

