## Maps of Fixed Effects ##

' Mit diesem Script werden können Maps von Fixed Effects erstellt werden.
  Diese werden mit Hilfe von spplot dargestellt. Für die farbliche Auswahl nutze ich einen Clustering Algorithmus (z.B. Fisher Jenkins).
  Dadurch kommen die Cluster besser zur geltung. 
  Cluster sind erkennbar. 
'

## Input
'
aus den .._SM/WW_BIC Scripten 
'
## Output ##
'
spplot maps
'

##########################################
## Lade Datei mit räumlichen Referenzen ##
KreisPolOGR <- readOGR("./data/data_raw/4_michael/adminitrative_borders_Ger/", "vg2500_krs")
str(KreisPolOGR,3)
KreisPolOGR

## Transform RS to comId compatible references
KreisPolOGR$comId<-as.integer(str_sub(KreisPolOGR$RS,1,5))
head(KreisPolOGR)



###################
#### Silomaize ####

#########
## Mai ##

########################################################
## Map of Fixed Effects for Model plm.fit_SM_BEST_Mai ##
## Lade Datei mit Fixed Effects für Model plm.fit_SM_BEST_Mai
fixef_plm.fit_SM_BEST_Mai <- read.csv("./figures/figures_exploratory/FixedEffects/Silomaize/plm.fit_SM_BEST_Mai_FE.csv")
fixef_plm.fit_SM_BEST_Mai$X <- NULL

head(fixef_plm.fit_SM_BEST_Mai)
str(fixef_plm.fit_SM_BEST_Mai)

###################################
## Create new spatial data.frame ##
' Merge fixed Effects with Spatial Reference DataFrame via comId '
KreisPolOGR_fixef_SM_BEST_Mai <- merge(KreisPolOGR, fixef_plm.fit_SM_BEST_Mai, by = "comId")
dim(KreisPolOGR_fixef_SM_BEST_Mai)
str(KreisPolOGR_fixef_SM_BEST_Mai, 2)
head(KreisPolOGR_fixef_SM_BEST_Mai)

#############################################################################
## Plot Spatial Data Frame with colours in accordance to cluster algorithm ##
# Create Class Intervalls via cluster algorithm #
classInt <- classIntervals(KreisPolOGR_fixef_SM_BEST_Mai$FE, n=3, style = "jenks")
classInt
pal <- brewer.pal(3, "Blues")
plot(classInt, pal=pal, main="Fisher-Jenks natural breaks", xlab="", ylab="")

## Define breacks
at1 <- classInt$brks
at1
# ##  Anpassen der Breaks ##
# at1[1] <- at1[1] - at1[1]/100
# at1[length(at1)] <- at1[length(at1)] + at1[length(at1)]/100

## Plot with breaks derived Fisher - Jenins Natural Breaks Algorithm with properly assigned colors
spplot(KreisPolOGR_fixef_SM_BEST_Mai, zcol="FE", at=at1, col.regions=colorRampPalette(pal)(length(at1)-1),main="Spatial Distribution of Fixed Effects of Mai - Silo Maize" )

## Export Plot as PDF ##
pdf('./figures/figures_exploratory/FixedEffects/Silomaize/SM_BEST_Mai.pdf')
spplot(KreisPolOGR_fixef_SM_BEST_Mai, zcol="FE", at=at1, col.regions=colorRampPalette(pal)(length(at1)-1),main="Spatial Distribution of Fixed Effects of Mai - Silo Maize" )
dev.off()
