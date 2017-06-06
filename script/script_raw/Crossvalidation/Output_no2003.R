#### Produce Regression Output Tables of all Silomaize Models ####

' In diesem Script werden die Tables mit den Regressions Coefficienten für die einzelene Monate erstellt. Dabei erstelle ich jeweils einen Table für
  Best Standard Model, Best Model in General and Best Model with SMI'

'
Hier sollte ich sohl plm adjusted R-square als auch das von lm einbringen. Daran kann man sehen, wie groß der Anteil ist, welcher durch die sich
verändernden Variablen erklärt wird.

'
library("stargazer")
library(ggplot2)
library(ggthemes)
###############
## Silomaize ##
###############

labels1 <-c("May", "June","July","August","September","October")
###################################
## Driscoll Kray Standard Errors ##

##########################
## Best Standard Models ##
se_bestStandard <- list(DK.se_SM_bestStandard_May_no2003, DK.se_SM_bestStandard_Jun_no2003, DK.se_SM_bestStandard_Jul_no2003, 
                        DK.se_SM_BEST_Aug_no2003, DK.se_SM_BEST_Sep_no2003, DK.se_SM_bestStandard_Oct_no2003)

stargazer(plm.fit_SM_bestStandard_May_no2003,plm.fit_SM_bestStandard_Jun_no2003, plm.fit_SM_bestStandard_Jul_no2003, plm.fit_SM_BEST_Aug_no2003,
          plm.fit_SM_BEST_Sep_no2003,plm.fit_SM_bestStandard_Oct_no2003, 
          se = se_bestStandard,   
          title  = "Results of Regression Models of Winter Wheat with smallest BIC of Standard Configuration",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
#           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$","Precipitation","AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                                         "AverageTemperature$^{3}$", "AverageTemperature", "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormal dry", "SMI:abnormal wet", 
                                         "SMI:abundant wet", "SMI:severe wet" ),
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStandard_no2003.txt",
#          out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStandard_no2003",
          no.space=TRUE ,
          df = FALSE)

##################################################################
## Plot with significant Coefficients of Standard Configuration ##
standard_Coefficients_SM <- read.csv(file="./figures/figures_exploratory/BIC/Silomaize/Standard_Coefficients_SM.csv")
standard_Coefficients_SM 
# View(standard_Coefficients_SM)
str(standard_Coefficients_SM)
names(standard_Coefficients_SM)
levels(standard_Coefficients_SM$Anomaly)
# Change Order 
standard_Coefficients_SM$Anomaly <- factor(standard_Coefficients_SM$Anomaly, levels=c("severe drought","moderate drought","abnormal dry","abnormal wet","abundant wet","severe wet"))
standard_Coefficients_SM$Month <- factor(standard_Coefficients_SM$Month, levels=c("Mai","June","July","August","September","October"))
levels(standard_Coefficients_SM$Month)[levels(standard_Coefficients_SM$Month)=="Mai"] <- "May"
levels(standard_Coefficients_SM$Anomaly)<- c("severe drought","moderate drought","abnormally dry","abnormally wet","abundantly wet","severely wet")
cs2 <- colorRampPalette(c("tomato4","tan4", "tan1","dodgerblue1","dodgerblue4", "darkblue"))(6)

g_SM <- ggplot(standard_Coefficients_SM, aes(x = Anomaly, y=Coefficient_sig_perc, fill=Month))
g_SM + geom_bar(stat = "identity", position="dodge") + facet_grid(~Month) + theme_few() +
    theme(axis.text.x = element_text(angle=90, vjust=0, size=30, color=cs2, face="bold"),
          axis.text.y = element_text(angle=0, vjust=0, size=30),
          axis.title.y = element_text(angle=90, size=30),
          axis.title.x = element_blank(),
          strip.text.x = element_text(size = 30),
          legend.position="none") +
     geom_vline(xintercept = 3.5,  colour="grey", linetype = "longdash", size=1) +
     geom_hline(yintercept = 0,  colour="grey", linetype = "longdash", size=0.5) +
    ylab("Percentage Change of Silo Maize") +
  
    scale_fill_brewer(palette="Pastel2") 


# Exportet with height 800 and width 1600 in png

###########################
## Best Model in General ##

se_BEST <- list(DK.se_SM_BEST_May_no2003, DK.se_SM_BEST_Jun_no2003, DK.se_SM_BEST_Jul_no2003, DK.se_SM_BEST_Aug_no2003, DK.se_SM_BEST_Sep_no2003, DK.se_SM_BEST_Oct_no2003)

stargazer(plm.fit_SM_BEST_May_no2003 ,plm.fit_SM_BEST_Jun_no2003,plm.fit_SM_BEST_Jul_no2003,plm.fit_SM_BEST_Aug_no2003,plm.fit_SM_BEST_Sep_no2003,plm.fit_SM_BEST_Oct_no2003, 
          se = se_BEST,   
          title ="Results of Regression Models of Winter Wheat with smallest BIC of all Configurations",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation","Precipitation$^{2}$","Precipitation$^{3}$","Potential Evapotrans.$^{1}$",
                                         "Potential Evapotrans.$^{2}$","Potential Evapotrans.$^{3}$","Precipitation","AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                                         "AverageTemperature$^{3}$"    , "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormally dry", "SMI:abnormally wet", 
                                         "SMI:abundantly wet", "SMI:severely wet" ),
#           type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Best_no2003.txt",
          out="./figures/figures_exploratory/BIC/Silomaize/SM_Best_no2003",
          no.space=TRUE ,
          df = FALSE)

summary(plm.fit_SM_BEST_May_no2003)

## Best Model with SMI ##

se_BEST_SMI <- list( DK.se_SM_BEST_May_no2003,  DK.se_SM_bestSMI_Jun_no2003, DK.se_SM_bestSMI_Jul_no2003, DK.se_SM_BEST_Aug_no2003, DK.se_SM_bestStandard_Sep_no2003, DK.se_SM_BEST_Oct_no2003)

stargazer(plm.fit_SM_BEST_May_no2003 , plm.fit_SM_bestSMI_Jun_no2003, plm.fit_SM_bestSMI_Jul_no2003, plm.fit_SM_BEST_Aug_no2003 ,plm.fit_SM_bestStandard_Sep_no2003, plm.fit_SM_BEST_Oct_no2003,
          title="Results of Regression Models of Winter Wheat with smallest BIC of all Configurations with SMI",
          se = se_BEST_SMI,  
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
#           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$","Precipitation$^{3}$", "Potential Evapotrans.$^{1}$",
                              "Potential Evapotrans.$^{2}$","Potential Evapotrans.$^{3}$","Precipitation","AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$", "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormally dry", "SMI:abnormally wet", 
                                "SMI:abundantly wet", "SMI:severely wet" ),
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Best_SMI_no2003.txt", 
#            out="./figures/figures_exploratory/BIC/Silomaize/SM_Best_SMI_no2003" ,
          no.space=TRUE ,
          df = FALSE)

#####################################################################################################################################################################
#####################################################################################################################################################################

#################
## WinterWheat ##

###################################
## Driscoll Kray Standard Errors ##

#################
## Best Models ##
se_BEST_no2003 <- list(DK.se_WW_BEST_Oct_lag_no2003, DK.se_WW_BEST_Nov_lag_no2003, DK.se_WW_BEST_Dec_lag_no2003, 
                DK.se_WW_BEST_Jan_no2003 ,DK.se_WW_BEST_Feb_no2003, DK.se_WW_BEST_Mar_no2003, DK.se_WW_BEST_Apr_no2003, 
                       DK.se_WW_BEST_May_no2003, DK.se_WW_BEST_Jun_no2003, DK.se_WW_BEST_Jul_no2003,
                       DK.se_WW_BEST_Aug_no2003)

cov.labels1_no2003 <-c("Prec$^{1}$", 
                       "T$^{1}$", "T$^{2}$", "T$^{3}$"    ,
                       "Prec$^{1}$","Prec$^{2}$", 
                       "Pet", "Pet$^{2}$", "Pet$^{3}$"    ,
                       "Prec$^{3}$",
                       "Pet",
                       "T",
                "SMI:sev. drought", "SMI:mod. drought",  "SMI:abn. dry", "SMI:abn. wet", 
  "SMI:abun. wet", "SMI:sev. wet" )
labels1_no2003 <-c("October", "November", "December", "January", "February","March","April","May", "June","July","August")

stargazer(plm.fit_WW_BEST_Oct_lag_no2003, plm.fit_WW_BEST_Nov_lag_no2003, plm.fit_WW_BEST_Dec_lag_no2003, 
          plm.fit_WW_BEST_Jan_no2003, plm.fit_WW_BEST_Feb_no2003,plm.fit_WW_BEST_Mar_no2003,plm.fit_WW_BEST_Apr_no2003,
          plm.fit_WW_BEST_May_no2003,plm.fit_WW_BEST_Jun_no2003, plm.fit_WW_BEST_Jul_no2003, 
          plm.fit_WW_BEST_Aug_no2003, 
          se = se_BEST_no2003,  
          title  = "Results of Regression Models of Winter Wheat with smallest BIC - not considering 2003",
          dep.var.caption = "Dependent Variable: log(Winter Wheat)",
          dep.var.labels = "Model of the month",
         align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1_no2003,      
            covariate.labels = cov.labels1_no2003,          
          no.space=TRUE ,
          df = FALSE,
#           type="text", 
#           out="./figures/figures_exploratory/BIC/Winterwheat/WW_Best_no2003.txt"
                     out="./figures/figures_exploratory/BIC/Winterwheat/WW_Best_no2003"
)


##########################
## Best Standard Models ##
se_bestStandard2_no2003 <- list(DK.se_WW_BEST_Oct_lag_no2003, DK.se_WW_bestStandard_Nov_lag_no2003, DK.se_WW_BEST_Dec_lag_no2003, 
                DK.se_WW_BEST_Jan_no2003, DK.se_WW_BEST_Feb_no2003, DK.se_WW_bestStandard_Mar_no2003, DK.se_WW_bestStandard_Apr_no2003, 
                DK.se_WW_BEST_May_no2003, DK.se_WW_BEST_Jun_no2003, DK.se_WW_bestStandard_Jul_no2003,
                DK.se_WW_bestStandard_Aug_no2003)

cov.labels2_no2003 <-c("Prec", 
                       "Prec$^{1}$", "Prec$^{2}$", "Prec$^{3}$", 
                       "T$^{1}$", "T$^{2}$", "T$^{3}$"    ,
                       "T",
                "SMI:sev. drought", "SMI:mod. drought",  "SMI:abn. dry", "SMI:abn. wet", 
                "SMI:abun. wet", "SMI:sev. wet" )
labels2_no2003 <-c("October", "November", "December", "January", "February","March","April","May", "June","July","August")

stargazer(plm.fit_WW_BEST_Oct_lag_no2003, plm.fit_WW_bestStandard_Nov_lag_no2003, plm.fit_WW_BEST_Dec_lag_no2003, 
          plm.fit_WW_BEST_Jan_no2003, plm.fit_WW_BEST_Feb_no2003, plm.fit_WW_bestStandard_Mar_no2003,plm.fit_WW_bestStandard_Apr_no2003,
          plm.fit_WW_BEST_May_no2003, plm.fit_WW_BEST_Jun_no2003, plm.fit_WW_bestStandard_Jul_no2003, 
          plm.fit_WW_bestStandard_Aug_no2003, 
          se = se_bestStandard2_no2003,  
          title  = "Results of Regression Models of Winter Wheat with smallest BIC of Standard Configuration",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          #           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels2_no2003,      
           covariate.labels = cov.labels2_no2003,          
          #           no.space=TRUE ,
          df = FALSE,
#                     type="text", 
#                     out="./figures/figures_exploratory/BIC/Winterwheat/WW_BestStandard_no2003.txt"
          out="./figures/figures_exploratory/BIC/Winterwheat/WW_BestStandard_no2003"
)


##################################################################
## Plot with significant Coefficients of Standard Configuration ##
standard_Coefficients_WW <- read.csv(file="./figures/figures_exploratory/BIC/Winterwheat/Standard_Coefficients_WW.csv")
standard_Coefficients_WW 
# View(standard_Coefficients_WW)
str(standard_Coefficients_WW)
names(standard_Coefficients_WW)
levels(standard_Coefficients_WW$Anomaly)
# Change Order 
standard_Coefficients_WW$Anomaly <- factor(standard_Coefficients_WW$Anomaly, levels=c("severe drought","moderate drought","abnormal dry","abnormal wet","abundant wet","severe wet"))
standard_Coefficients_WW$Month <- factor(standard_Coefficients_WW$Month, levels=c("February", "March", "April","May","June","July","August"))
levels(standard_Coefficients_ww$Month)[levels(standard_Coefficients_SM$Month)=="Mai"] <- "May"


g_WW <- ggplot(standard_Coefficients_WW, aes(x = Anomaly, y=Coefficient_sig_perc, fill=Month))
g_WW + geom_bar(stat = "identity", position="dodge") + facet_grid(~Month) + theme_few() +
  theme(axis.text.x = element_text(angle=90, vjust=0, size=25),
        axis.text.y = element_text(angle=0, vjust=0, size=25),
        axis.title.y = element_text(angle=90, size=25),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 25),
        legend.position="none") +
  geom_vline(xintercept = 3.5,  colour="grey", linetype = "longdash", size=1) +
  geom_hline(yintercept = 0,  colour="grey", linetype = "longdash", size=0.5) +
  
  ylab("Percentage Change of Winter Wheat") +
  
  scale_fill_brewer(palette="Pastel2") 




