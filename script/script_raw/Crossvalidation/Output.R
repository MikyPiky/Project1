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
library(reshape)
library(cowplot)
library(scales)
library(RColorBrewer)
###############
## Silomaize ##
###############

labels1 <-c("May", "June","July","August","September","October")
###################################
## Driscoll Kray Standard Errors ##

##########################
## Best Standard Models ##
se_bestStandard <- list(DK.se_SM_bestStandard_May, DK.se_SM_bestStandard_Jun, DK.se_SM_bestStandard_Jul, DK.se_SM_bestStandard_Aug, DK.se_SM_bestStandard_Sep, DK.se_SM_bestStandard_Oct)

stargazer(plm.fit_SM_bestStandard_May,plm.fit_SM_bestStandard_Jun, plm.fit_SM_bestStandard_Jul, plm.fit_SM_bestStandard_Aug,
          plm.fit_SM_bestStandard_Sep,plm.fit_SM_bestStandard_Oct, 
          se = se_bestStandard,   
          title  = "Results of Regression Models of Winter Wheat with smallest BIC of Standard Configuration",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
#           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$","Precipitation","AverageTemperature", "AverageTemperature$^{2}$",
                                         "AverageTemperature$^{3}$"    , "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormal dry", "SMI:abnormal wet", 
                                         "SMI:abundant wet", "SMI:severe wet" ),
#           type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStandard.txt",
         out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStandard",
          no.space=TRUE ,
          df = FALSE)

###################################################
## Best Standard Models with highest polynomials ##
DK.se_SM_BestStdFull_Aug

se_BestStdFull <- list(DK.se_SM_BestStdFull_May, DK.se_SM_BestStdFull_Jun, DK.se_SM_BestStdFull_Jul, DK.se_SM_BestStdFull_Aug, DK.se_SM_BestStdFull_Sep, 
                        DK.se_SM_BestStdFull_Oct)

stargazer(plm.fit_SM_BestStdFull_May, plm.fit_SM_BestStdFull_Jun, plm.fit_SM_BestStdFull_Jul, plm.fit_SM_BestStdFull_Aug, plm.fit_SM_BestStdFull_Sep,
          plm.fit_SM_BestStdFull_Oct, 
          se = se_BestStdFull,   
          title  = "Results of Regression Models of Silage Maize - Standard Models highest Polynomials",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          #           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$","AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$"    , "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormal dry", "SMI:abnormal wet", 
                               "SMI:abundant wet", "SMI:severe wet" ),
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull.txt",
#                    out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull",
          no.space=TRUE ,
          df = FALSE)


############################################################
## Best Standard Models with highest polynomials - no2003 ##

se_BestStdFull_no2003 <- list(DK.se_SM_BestStdFull_May_no2003, DK.se_SM_BestStdFull_Jun_no2003, DK.se_SM_BestStdFull_Jul_no2003, 
                              DK.se_SM_BestStdFull_Aug_no2003, DK.se_SM_BestStdFull_Sep_no2003, DK.se_SM_BestStdFull_Oct_no2003)

stargazer(plm.fit_SM_BestStdFull_May_no2003, plm.fit_SM_BestStdFull_Jun_no2003, plm.fit_SM_BestStdFull_Jul_no2003, plm.fit_SM_BestStdFull_Aug_no2003, 
          plm.fit_SM_BestStdFull_Sep_no2003, plm.fit_SM_BestStdFull_Oct_no2003, 
          se = se_BestStdFull_no2003,   
          title  = "Results of Regression Models of Silage Maize - no2003 - Standard Models highest Polynomials",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          #           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$","AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$"    , "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormal dry", "SMI:abnormal wet", 
                               "SMI:abundant wet", "SMI:severe wet" ),
                    type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull_no2003.txt",
#           out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull_no2003",
          no.space=TRUE ,
          df = FALSE)


###############################################################
## Best Standard Models with highest polynomials but no SMI ##
se_BestStdFull_noSMI <- list(DK.se_SM_BestStdFull_May_noSMI, DK.se_SM_BestStdFull_Jun_noSMI, DK.se_SM_BestStdFull_Jul_noSMI, DK.se_SM_BestStdFull_Aug_noSMI, DK.se_SM_BestStdFull_Sep_noSMI, 
                       DK.se_SM_BestStdFull_Oct_noSMI)

stargazer(plm.fit_SM_BestStdFull_May_noSMI, plm.fit_SM_BestStdFull_Jun_noSMI, plm.fit_SM_BestStdFull_Jul_noSMI, plm.fit_SM_BestStdFull_Aug_noSMI, 
          plm.fit_SM_BestStdFull_Sep_noSMI, plm.fit_SM_BestStdFull_Oct_noSMI, 
          se = se_BestStdFull_noSMI,   
          title  = "Results of Regression Models of Silage Maize - Standard Models highest Polynomials",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          #           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$","AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$" ),
#                     type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull_noSMI.txt",
          out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull_noSMI",
          no.space=TRUE ,
          df = FALSE)


###############################################################
## Best Standard Models with highest polynomials but no Tavg ##
se_BestStdFull_noTavg <- list(DK.se_SM_BestStdFull_May_noTavg, DK.se_SM_BestStdFull_Jun_noTavg, DK.se_SM_BestStdFull_Jul_noTavg, DK.se_SM_BestStdFull_Aug_noTavg, DK.se_SM_BestStdFull_Sep_noTavg, 
                             DK.se_SM_BestStdFull_Oct_noTavg)

stargazer(plm.fit_SM_BestStdFull_May_noTavg, plm.fit_SM_BestStdFull_Jun_noTavg, plm.fit_SM_BestStdFull_Jul_noTavg, plm.fit_SM_BestStdFull_Aug_noTavg, 
          plm.fit_SM_BestStdFull_Sep_noTavg, plm.fit_SM_BestStdFull_Oct_noTavg, 
          se = se_BestStdFull_noTavg,   
          title  = "Results of Regression Models of Silage Maize - Standard Models highest Polynomials",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          #           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$", "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormal dry", "SMI:abnormal wet", 
                               "SMI:abundant wet", "SMI:severe wet" ),
#           type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull_noTavg.txt",
                    out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull_noTavg",
          no.space=TRUE ,
          df = FALSE)

###############################################################
## Best Standard Models with highest polynomials but no Prec ##
se_BestStdFull_noPrec <- list(DK.se_SM_BestStdFull_May_noPrec, DK.se_SM_BestStdFull_Jun_noPrec, DK.se_SM_BestStdFull_Jul_noPrec, DK.se_SM_BestStdFull_Aug_noPrec, DK.se_SM_BestStdFull_Sep_noPrec, 
                              DK.se_SM_BestStdFull_Oct_noPrec)

stargazer(plm.fit_SM_BestStdFull_May_noPrec, plm.fit_SM_BestStdFull_Jun_noPrec, plm.fit_SM_BestStdFull_Jul_noPrec, plm.fit_SM_BestStdFull_Aug_noPrec, 
          plm.fit_SM_BestStdFull_Sep_noPrec, plm.fit_SM_BestStdFull_Oct_noPrec, 
          se = se_BestStdFull_noPrec,   
          title  = "Results of Regression Models of Silage Maize - Standard Models highest Polynomials",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          #           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$", "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormal dry", "SMI:abnormal wet", 
                               "SMI:abundant wet", "SMI:severe wet" ),
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull_noPrec.txt",
#                     out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull_noPrec",
          no.space=TRUE ,
          df = FALSE)

###############################################################
## Best Standard Models - selection for senstitivity analasis##

plm.fit_SM_BestStdFull_Jul_onlyTemp
DK.se_SM_BestStdFull_Jul_onlyTemp

plm.fit_SM_BestStdFull_Jun_noPrec
DK.se_SM_BestStdFull_Jun_noPrec

plm.fit_SM_BestStdFull_Jul_noPrec
DK.se_SM_BestStdFull_Jul_noPrec

plm.fit_SM_BestStdFull_Jul_noTavg
DK.se_SM_BestStdFull_Jul_noTavg

plm.fit_SM_BestStdFull_Aug_noPrec
DK.se_SM_BestStdFull_Aug_noPrec

plm.fit_SM_BestStdFull_Aug_noTavg
DK.se_SM_BestStdFull_Aug_noTavg

se_BestStdFull_sensitivity <- list(DK.se_SM_BestStdFull_Jul_onlyTemp,
                                   DK.se_SM_BestStdFull_Jun_noPrec, 
                                   DK.se_SM_BestStdFull_Jul_noTavg,
                                   DK.se_SM_BestStdFull_Jul_noPrec,
                                   DK.se_SM_BestStdFull_Aug_noPrec,
                                   DK.se_SM_BestStdFull_Aug_noTavg)

stargazer(plm.fit_SM_BestStdFull_Jul_onlyTemp, 
          plm.fit_SM_BestStdFull_Jun_noPrec, 
          plm.fit_SM_BestStdFull_Jul_noTavg, 
          plm.fit_SM_BestStdFull_Jul_noPrec,
          plm.fit_SM_BestStdFull_Aug_noPrec, 
          plm.fit_SM_BestStdFull_Aug_noTavg,
          se = se_BestStdFull_sensitivity,   
          title  = "Results of Regression Models of Silage Maize - Standard Models highest Polynomials",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
#           dep.var.labels = "Model of the month",
          #           align=TRUE, 
          model.numbers = T,
#           column.labels = labels1,      
          covariate.labels = c("AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$", "Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$", 
                               "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormal dry", "SMI:abnormal wet", 
                               "SMI:abundant wet", "SMI:severe wet" ),
        type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull_sensitivity.txt",
#           out="./figures/figures_exploratory/BIC/Silomaize/SM_BestStdFull_sensitivity",
          no.space=TRUE ,
          df = FALSE)

##################################################################
## Plot with significant Coefficients of Standard Configuration ##
standard_Coefficients_SM <- read.csv(file="./figures/figures_exploratory/BIC/Silomaize/Std_Coefficients_SM3.csv")
standard_Coefficients_SM 
# View(standard_Coefficients_SM)
str(standard_Coefficients_SM)
names(standard_Coefficients_SM)
levels(standard_Coefficients_SM$Anomaly)
# standard_Coefficients_SM$se <- factor(standard_Coefficients_SM$se)
# Change Order 
standard_Coefficients_SM$Anomaly <- factor(standard_Coefficients_SM$Anomaly, c("C1","C2","C3","C4",
                                                                               "C5","C6"))
# standard_Coefficients_SM$Anomaly <- factor(standard_Coefficients_SM$Anomaly, c("C1: severe drought","C2: moderate drought","C3: abnormally dry","C4: abnormally wet",
#                                                                                "C5: abundantly wet","C6: severely wet"))
# standard_Coefficients_SM$Anomaly <- factor(standard_Coefficients_SM$Anomaly, c("sev. drought","mod. drought","abnorm. dry","abnorm. wet","abund. wet","sev. wet"))

# standard_Coefficients_SM$Anomaly <- factor(standard_Coefficients_SM$Anomaly, c("severe drought","moderate drought","abnormally dry","abnormally wet",
                                                                               # "abundantly wet","severely wet"))

standard_Coefficients_SM$Month <- factor(standard_Coefficients_SM$Month, levels=c("May","June","July","August","September","October"))


cs2 <- colorRampPalette(c("#a50026","#d73027","#f46d43",'#74add1','#4575b4','#313695'))(6)
# cs2 <- colorRampPalette(brewer.pal(11,"RdYlBu"))(7)
limits <- aes(ymax = Coefficient_sig_perc - se100, ymin = Coefficient_sig_perc + se100)
dodge <- position_dodge(width = 1)


g_SM <- ggplot(standard_Coefficients_SM, aes(x = Anomaly, y = Coefficient_sig_perc, fill=Month))

Std_Coefficients_SM <- g_SM + 
    ylim(-25, 10) +
    geom_bar(stat = "identity", position=dodge) + facet_grid(~Month) + 
    geom_errorbar(limits, position=dodge, width=0.5) +
    theme_few() +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, size=25),
          axis.text.y = element_text(angle=0, vjust=0, size=25),
          axis.title.y = element_text(angle=90, size=30),
          axis.title.x = element_blank(),
          strip.text.x = element_text(size = 25, vjust=-0.2),
          legend.position="none") +
     geom_vline(xintercept = 3.5,  colour="gray40", linetype = "longdash", size=1) +
     geom_hline(yintercept = c(5, 0, -5, -10, -15, -20) ,  colour="gray40", linetype = "longdash", size=0.5) +
    ylab("Change of Silage Maize Yield (%)") +
   scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a","#1f78b4","#e6ab02"))  

Std_Coefficients_SM

ggsave("Std_Coefficients_SM3.pdf", plot = Std_Coefficients_SM, device = "pdf", 
       path = "/Storage/ownCloud/Home/Klimabuero/Proj1/figures/figures_exploratory/BIC/Silomaize/",
       width = 16, height = 8,
       dpi = 300)

ggsave("Std_Coefficients_SM3.png", plot = Std_Coefficients_SM, device = "png", 
       path = "/Storage/ownCloud/Home/Klimabuero/Proj1/figures/figures_exploratory/BIC/Silomaize/",
       width = 16, height = 10,
       dpi = 300)

# Exported with height 800 and width 1600 in png

###########################
## Best Model in General ##

se_BEST <- list(DK.se_SM_BEST_May, DK.se_SM_BEST_Jun, DK.se_SM_BEST_Jul, DK.se_SM_BEST_Aug, DK.se_SM_BEST_Sep, DK.se_SM_BEST_Oct)

stargazer(plm.fit_SM_BEST_May,plm.fit_SM_BEST_Jun,plm.fit_SM_BEST_Jul,plm.fit_SM_BEST_Aug,plm.fit_SM_BEST_Sep,plm.fit_SM_BEST_Oct, 
          se = se_BEST,   
          title ="Results of Regression Models of Winter Wheat with smallest BIC of all Configurations",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
                    covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$","Precipitation$^{3}$","Potential Evapotrans.$^{1}$",
                                         "Potential Evapotrans.$^{2}$","Potential Evapotrans.$^{3}$","AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                                         "AverageTemperature$^{3}$"    , "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormally dry", "SMI:abnormally wet", 
                                         "SMI:abundantly wet", "SMI:severely wet" ),
#           type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Best.txt",
          out="./figures/figures_exploratory/BIC/Silomaize/SM_Best",
          no.space=TRUE ,
          df = FALSE)

##################################################################
## Plot with significant Coefficients of General Best Configuration ##
Best_Coefficients_SM <- read.csv(file="./figures/figures_exploratory/BIC/Silomaize/Best_Coefficients_SM.csv")
Best_Coefficients_SM 
# View(Best_Coefficients_SM)
str(Best_Coefficients_SM)
names(Best_Coefficients_SM)
levels(Best_Coefficients_SM$Anomaly)
test <- as.factor(Best_Coefficients_SM$Anomaly)
levels(test)
test
# Change Order 
Best_Coefficients_SM$Anomaly <- factor(Best_Coefficients_SM$Anomaly, c("severe drought","moderate drought","abnormally dry","abnormally wet","abundantly wet","severely wet"))
Best_Coefficients_SM$Month <- factor(Best_Coefficients_SM$Month, levels=c("June","July","August","September","October"))

cs2 <- colorRampPalette(c("tomato4","tan4", "tan1","dodgerblue1","dodgerblue4", "darkblue"))(6)

g_SM <- ggplot(Best_Coefficients_SM, aes(x = Anomaly, y=Coefficient_sig_perc, fill=Month))
g_SM + geom_bar(stat = "identity", position="dodge") + facet_grid(~Month) + theme_few() +
  theme(axis.text.x = element_text(angle=90, vjust=0, size=30, color=cs2, face="bold"),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 30),
        legend.position="none") +
  geom_vline(xintercept = 3.5,  colour="grey", linetype = "longdash", size=1) +
  geom_hline(yintercept = 0,  colour="grey", linetype = "longdash", size=0.5) +
  ylab("Percentage Change of Silage Maize Yield") +
  
  scale_fill_brewer(palette="Pastel2") 



####################################
## Best Model in General - no2003 ##

se_BEST_no2003 <- list(DK.se_SM_BEST_May_no2003, DK.se_SM_BEST_Jun_no2003, DK.se_SM_BEST_Jul_no2003, DK.se_SM_BEST_Aug_no2003, DK.se_SM_BEST_Sep_no2003, 
                       DK.se_SM_BEST_Oct_no2003)

stargazer(plm.fit_SM_BEST_May_no2003,plm.fit_SM_BEST_Jun_no2003,plm.fit_SM_BEST_Jul_no2003,plm.fit_SM_BEST_Aug_no2003,plm.fit_SM_BEST_Sep_no2003,
          plm.fit_SM_BEST_Oct_no2003, 
          se = se_BEST_no2003,   
          title ="Results of Regression Models of Winter Wheat with smallest BIC of all Configurations",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$","Precipitation$^{3}$", "Potential Evapotrans.$^{1}$",
                               "Potential Evapotrans.$^{2}$","Precipitation","AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$" , "Potential Evapotrans.$^{3}$",   "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormally dry",
                               "SMI:abnormally wet", 
                               "SMI:abundantly wet", "SMI:severely wet" ),
#           type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Best_no2003.txt",
                    out="./figures/figures_exploratory/BIC/Silomaize/SM_Best_no2003",
          no.space=TRUE ,
          df = FALSE)


#########################
## Best Model with SMI ##

se_BEST_SMI <- list( DK.se_SM_bestSMI_May,  DK.se_SM_bestSMI_Jun, DK.se_SM_bestSMI_Jul, DK.se_SM_bestSMI_Aug, 
                     DK.se_SM_bestSMI_Sep, DK.se_SM_bestSMI_Oct)

stargazer(plm.fit_SM_bestSMI_May, plm.fit_SM_bestSMI_Jun, plm.fit_SM_bestSMI_Jul, 
          plm.fit_SM_bestSMI_Aug ,plm.fit_SM_bestSMI_Sep, plm.fit_SM_bestSMI_Oct,
          title="Results of Regression Models of Winter Wheat with smallest BIC of all Configurations with SMI",
          se = se_BEST_SMI,  
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
#           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation","Precipitation$^{2}$", "Precipitation$^{3}$","Potential Evapotrans.",
                              "Potential Evapotrans.$^{2}$","Potential Evapotrans.$^{3}$","AverageTemperature", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$"  , "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormally dry", "SMI:abnormally wet", 
                                "SMI:abundantly wet", "SMI:severely wet" ),
#           type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Best_SMI.txt", 
          out="./figures/figures_exploratory/BIC/Silomaize/SM_Best_SMI" ,
          no.space=TRUE ,
          df = FALSE)

##################################
## Best Model with SMI - no2003 ##

se_BEST_SMI_no2003 <- list( DK.se_SM_bestSMI_May_no2003,  DK.se_SM_bestSMI_Jun_no2003, DK.se_SM_bestSMI_Jul_no2003, 
                            DK.se_SM_bestSMI_Aug_no2003, DK.se_SM_bestSMI_Sep_no2003,  DK.se_SM_bestSMI_Oct_no2003)

stargazer(plm.fit_SM_bestSMI_May_no2003, plm.fit_SM_bestSMI_Jun_no2003, plm.fit_SM_bestSMI_Jul_no2003, plm.fit_SM_bestSMI_Aug_no2003 ,
          plm.fit_SM_bestSMI_Sep_no2003, plm.fit_SM_bestSMI_Oct_no2003,
          title="Results of Regression Models of Winter Wheat with smallest BIC of all Configurations with SMI",
          se = se_BEST_SMI,  
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          #           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation$^{1}$","Precipitation$^{2}$", "Precipitation$^{3}$","Potential Evapotrans.$^{1}$",
                               "Potential Evapotrans.$^{2}$","Precipitation","AverageTemperature$^{1}$", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$"  ,"Potential Evapotrans.$^{3}$", "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormally dry", "SMI:abnormally wet", 
                               "SMI:abundantly wet", "SMI:severely wet" ),
#                     type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_Best_SMI_no2003.txt", 
          out="./figures/figures_exploratory/BIC/Silomaize/SM_Best_SMI_no2003" ,
          no.space=TRUE ,
          df = FALSE)


########################
## Best Models no PET ##

se_noPET <- list(DK.se_SM_noPET_May,DK.se_SM_noPET_Jun, DK.se_SM_noPET_Jul, DK.se_SM_noPET_Aug, DK.se_SM_noPET_Sep, DK.se_SM_noPET_Oct)

stargazer(plm.fit_SM_noPET_May,plm.fit_SM_noPET_Jun,plm.fit_SM_noPET_Jul,plm.fit_SM_noPET_Aug,plm.fit_SM_noPET_Sep,plm.fit_SM_noPET_Oct, 
          se = se_noPET,   
          title ="Results of Regression Models",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
                              covariate.labels = c("Precipitation","Precipitation$^{2}$", "Precipitation$^{3}$","AverageTemperature", "AverageTemperature$^{2}$",
                                                   "AverageTemperature$^{3}$"    , "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormally dry", "SMI:abnormally wet", 
                                                   "SMI:abundantly wet", "SMI:severely wet" ),
          type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_noPET.txt",
#                     out="./figures/figures_exploratory/BIC/Silomaize/SM_noPET",
          no.space=TRUE ,
          df = FALSE)

#################################
## Best Models no PET - no2003 ##

se_noPET_no2003 <- list(DK.se_SM_noPET_May_no2003,DK.se_SM_noPET_Jun_no2003, DK.se_SM_noPET_Jul_no2003, DK.se_SM_noPET_Aug_no2003, DK.se_SM_noPET_Sep_no2003, 
                 DK.se_SM_noPET_Oct_no2003)

stargazer(plm.fit_SM_noPET_May_no2003,plm.fit_SM_noPET_Jun_no2003,plm.fit_SM_noPET_Jul_no2003,plm.fit_SM_noPET_Aug_no2003,plm.fit_SM_noPET_Sep_no2003,
          plm.fit_SM_noPET_Oct_no2003, 
          se = se_noPET_no2003,   
          title ="Results of Regression Models",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = c("Precipitation","Precipitation$^{2}$", "Precipitation$^{3}$","Precipitation","AverageTemperature", "AverageTemperature$^{2}$",
                               "AverageTemperature$^{3}$"    , "SMI:severe drought", "SMI:moderate drought",  "SMI:abnormally dry", "SMI:abnormally wet", 
                               "SMI:abundantly wet", "SMI:severely wet" ),
                    type="text", out="./figures/figures_exploratory/BIC/Silomaize/SM_noPET_no2003.txt",
#           out="./figures/figures_exploratory/BIC/Silomaize/SM_noPET_no2003",
          no.space=TRUE ,
          df = FALSE)

##################################################################
## Plot with significant Coefficients of Configuration without PET ##
standard_Coefficients_SM_noPET <- read.csv("~/Documents/projects/correlation/figures/figures_exploratory/BIC/Silomaize/SM_noPET_SignCoeff_asPerc2.csv", sep=",")
standard_Coefficients_SM_noPET
# View(standard_Coefficients_SM)
str(standard_Coefficients_SM_noPET)
names(standard_Coefficients_SM_noPET)
levels(standard_Coefficients_SM_noPET$Anomaly)
levels(standard_Coefficients_SM_noPET$Month)
# Change Order 
standard_Coefficients_SM_noPET$Anomaly <- factor(standard_Coefficients_SM_noPET$Anomaly, levels=c("severe drought",
                                                                                                  "moderate drought",
                                                                                                  "abnormally dry",
                                                                                                  "abnormally wet",
                                                                                                  "abundantly wet",
                                                                                                  "severely wet"))
standard_Coefficients_SM_noPET$Month <- factor(standard_Coefficients_SM_noPET$Month, levels=c("May","June","July","August","September","October"))
# levels(standard_Coefficients_SM_noPET$Anomaly)<- c("severe drought","moderate drought","abnormally dry","abnormally wet","abundantly wet","severely wet")
cs2 <- colorRampPalette(c("tomato4","tan4", "tan1","dodgerblue1","dodgerblue4", "darkblue"))(6)


## Standard Coefficients in Percentage - no2003
g_SM_no2003 <- ggplot(standard_Coefficients_SM_noPET, aes(x = Anomaly, y=Coefficient_sig_perc_no2003, fill=Month))
g_SM_no2003 + geom_bar(stat = "identity", position="dodge") + facet_grid(~Month) + theme_few() +
  theme(axis.text.x = element_text(angle=90, vjust=0, size=30, color=cs2, face="bold"),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 30),
        legend.position="none") +
  geom_vline(xintercept = 3.5,  colour="grey", linetype = "longdash", size=1) +
  geom_hline(yintercept = 0,  colour="grey", linetype = "longdash", size=0.5) +
  ylab("Percentage Change of Silage Maize - no2003") +
  
  scale_fill_brewer(palette="Pastel2") 








#####################################################################################################################################################################
#################
## WinterWheat ##,
###################################
## Driscoll Kray Standard Errors ##

#################
## Best Models ##
se_BEST <- list(DK.se_WW_BEST_Oct_lag, DK.se_WW_BEST_Nov_lag, DK.se_WW_BEST_Dec_lag, 
                DK.se_WW_BEST_Jan ,DK.se_WW_BEST_Feb, DK.se_WW_BEST_Mar, DK.se_WW_BEST_Apr, 
                       DK.se_WW_BEST_May, DK.se_WW_BEST_Jun, DK.se_WW_BEST_Jul,
                       DK.se_WW_BEST_Aug)

cov.labels1 <-c("Prec$^{1}$", "Prec$^{2}$", "Prec", "Prec$^{3}$",
                "T$^{1}$", "T$^{2}$", "T$^{3}$"    ,
                "PET$^{1}$", "PET$^{2}$", "PET$^{3}$"    ,
                "SMI:sev. drought", "SMI:mod. drought",  "SMI:abn. dry", "SMI:abn. wet", 
  "SMI:abun. wet", "SMI:sev. wet" )
labels1 <-c("October", "November", "December", "January", "February","March","April","May", "June","July","August")

stargazer(plm.fit_WW_BEST_Oct_lag, plm.fit_WW_BEST_Nov_lag, plm.fit_WW_BEST_Dec_lag, 
          plm.fit_WW_BEST_Jan, plm.fit_WW_BEST_Feb,plm.fit_WW_BEST_Mar,plm.fit_WW_BEST_Apr,
          plm.fit_WW_BEST_May,plm.fit_WW_BEST_Jun, plm.fit_WW_BEST_Jul, 
          plm.fit_WW_BEST_Aug, 
          se = se_BEST,  
          title  = "Results of Regression Models of Winter Wheat with smallest BIC",
          dep.var.caption = "Dependent Variable: log(Winter Wheat)",
          dep.var.labels = "Model of the month",
           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
           covariate.labels = cov.labels1,          
           no.space=TRUE ,
          df = FALSE,
          type="text", 
          out="./figures/figures_exploratory/BIC/Winterwheat/WW_Best.txt"
#                      out="./figures/figures_exploratory/BIC/Winterwheat/WW_Best"
)


##########################
## Best Standard Models ##
se_bestStandard <- list(DK.se_WW_BEST_Oct_lag, DK.se_WW_bestStandard_Nov_lag, DK.se_WW_BEST_Dec_lag, 
                DK.se_WW_BEST_Jan ,DK.se_WW_bestStandard_Feb, DK.se_WW_bestStandard_Mar, DK.se_WW_BEST_Apr, 
                DK.se_WW_BEST_May, DK.se_WW_BEST_Jun, DK.se_WW_BEST_Jul,
                DK.se_WW_BEST_Aug)

cov.labels2 <-c("Prec$^{1}$", "Prec$^{2}$", "Prec", "Prec$^{3}$",
                "T", "T$^{2}$", "T$^{3}$"    ,
                "SMI:sev. drought", "SMI:mod. drought",  "SMI:abn. dry", "SMI:abn. wet", 
                "SMI:abun. wet", "SMI:sev. wet" )
labels1 <-c("October", "November", "December", "January", "February","March","April","May", "June","July","August")

stargazer(plm.fit_WW_BEST_Oct_lag, plm.fit_WW_bestStandard_Nov_lag, plm.fit_WW_BEST_Dec_lag, 
          plm.fit_WW_BEST_Jan, plm.fit_WW_bestStandard_Feb,plm.fit_WW_bestStandard_Mar,plm.fit_WW_BEST_Apr,
          plm.fit_WW_BEST_May,plm.fit_WW_BEST_Jun, plm.fit_WW_BEST_Jul, 
          plm.fit_WW_BEST_Aug, 
          se = se_bestStandard,  
          title  = "Results of Regression Models of Winter Wheat with smallest BIC of Standard Configuration",
          dep.var.caption = "Dependent Variable: log(Silomaize)",
          dep.var.labels = "Model of the month",
          #           align=TRUE, 
          model.numbers = FALSE,
          column.labels = labels1,      
          covariate.labels = cov.labels2,          
          #           no.space=TRUE ,
          df = FALSE,
                    type="text", 
                    out="./figures/figures_exploratory/BIC/Winterwheat/WW_BestStandard.txt"
#           out="./figures/figures_exploratory/BIC/Winterwheat/WW_BestStandard"
)


##########################
## Best SMI Models ##
'gleich wie best models'


##################################################################
## Plot with significant Coefficients of Standard Configuration ##
## all years ##
best_Coefficients_WW <- read.csv(file="./figures/figures_exploratory/BIC/Winterwheat/Best_Coefficients_WW.csv")
best_Coefficients_WW 
# View(best_Coefficients_WW)
str(best_Coefficients_WW)
names(best_Coefficients_WW)
levels(best_Coefficients_WW$Anomaly)
# Change Order 
best_Coefficients_WW$Anomaly <- factor(best_Coefficients_WW$Anomaly, levels=c("severe drought","moderate drought","abnormally dry","abnormally wet","abundantly wet","severely wet"))
best_Coefficients_WW$Month <- factor(best_Coefficients_WW$Month, levels=c("October","November", "December", "January","February", "March", "April","May","June","July","August"))
# levels(best_Coefficients_WW$Anomaly)<- c("severe drought","moderate drought","abnormally dry","abnormally wet","abundantly wet","severely wet")
cs2 <- colorRampPalette(c("tomato4","tan4", "tan1","dodgerblue1","dodgerblue4", "darkblue"))(6)


g_WW <- ggplot(best_Coefficients_WW, aes(x = Anomaly, y=Coefficient_sig_perc, fill=Month))
g_WW + geom_bar(stat = "identity", position="dodge") + facet_grid(~Month) + theme_few() +
  theme(axis.text.x = element_text(angle=90, vjust=0, size=20, color=cs2, face="bold"),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 30),
        legend.position="none") +
  scale_y_continuous(limits = c(-12, 10)) +
  geom_vline(xintercept = 3.5,  colour="grey", linetype = "longdash", size=1) +
  geom_hline(yintercept = 0,  colour="grey", linetype = "longdash", size=0.5) +
  ylab("Percentage Change of Winter Wheat") +
  
  scale_fill_brewer(palette="Set3") 

##################################################################
## Plot with significant Coefficients of Standard Configuration ##
## no2003 ##
best_Coefficients_WW_no2003 <- read.csv(file="./figures/figures_exploratory/BIC/Winterwheat/Best_Coefficients_WW_no2003.csv")
best_Coefficients_WW_no2003 
# View(best_Coefficients_WW_no2003)
str(best_Coefficients_WW_no2003)
names(best_Coefficients_WW_no2003)
levels(best_Coefficients_WW_no2003$Anomaly)
# Change Order 
best_Coefficients_WW_no2003$Anomaly <- factor(best_Coefficients_WW_no2003$Anomaly, levels=c("severe drought","moderate drought","abnormally dry","abnormally wet","abundantly wet","severely wet"))
best_Coefficients_WW_no2003$Month <- factor(best_Coefficients_WW_no2003$Month, levels=c("October","November", "December", "January","February", "March", "April","May","June","July","August"))
# levels(best_Coefficients_WW_no2003$Anomaly)<- c("severe drought","moderate drought","abnormally dry","abnormally wet","abundantly wet","severely wet")
cs2 <- colorRampPalette(c("tomato4","tan4", "tan1","dodgerblue1","dodgerblue4", "darkblue"))(6)


g_WW_no2003 <- ggplot(best_Coefficients_WW_no2003, aes(x = Anomaly, y=Coefficient_sig_perc, fill=Month))
g_WW_no2003 + geom_bar(stat = "identity", position="dodge") + facet_grid(~Month) + theme_few() +
  theme(axis.text.x = element_text(angle=90, vjust=0, size=20, color=cs2, face="bold"),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 30),
        legend.position="none") +
  scale_y_continuous(limits = c(-12, 10)) +
  geom_vline(xintercept = 3.5,  colour="grey", linetype = "longdash", size=1) +
  geom_hline(yintercept = 0,  colour="grey", linetype = "longdash", size=0.5) +
  ylab("Percentage Change of Winter Wheat") +
  
  scale_fill_brewer(palette="Set3") 


##################################################################
## Plot with significant Coefficients of Standard Configuration ##
## no2003 and no2004 ##
best_Coefficients_WW_no2003no2004 <- read.csv(file="./figures/figures_exploratory/BIC/Winterwheat/Best_Coefficients_WW_no2003no2004.csv")
best_Coefficients_WW_no2003no2004 
# View(best_Coefficients_WW_no2003no2004)
str(best_Coefficients_WW_no2003no2004)
names(best_Coefficients_WW_no2003no2004)
levels(best_Coefficients_WW_no2003no2004$Anomaly)
# Change Order 
best_Coefficients_WW_no2003no2004$Anomaly <- factor(best_Coefficients_WW_no2003no2004$Anomaly, levels=c("severe drought","moderate drought","abnormally dry","abnormally wet","abundantly wet","severely wet"))
best_Coefficients_WW_no2003no2004$Month <- factor(best_Coefficients_WW_no2003no2004$Month, levels=c("October","November", "December", "January","February", "March", "April","May","June","July","August"))
# levels(best_Coefficients_WW_no2003no2004$Anomaly)<- c("severe drought","moderate drought","abnormally dry","abnormally wet","abundantly wet","severely wet")
cs2 <- colorRampPalette(c("tomato4","tan4", "tan1","dodgerblue1","dodgerblue4", "darkblue"))(6)


g_WW_no2003no2004 <- ggplot(best_Coefficients_WW_no2003no2004, aes(x = Anomaly, y=Coefficient_sig_perc, fill=Month))
g_WW_no2003no2004 + geom_bar(stat = "identity", position="dodge") + facet_grid(~Month) + theme_few() +
  theme(axis.text.x = element_text(angle=90, vjust=0, size=20, color=cs2, face="bold"),
        axis.text.y = element_text(angle=0, vjust=0, size=30),
        axis.title.y = element_text(angle=90, size=30),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 30),
        legend.position="none") +
  scale_y_continuous(limits = c(-12, 10)) +
  geom_vline(xintercept = 3.5,  colour="grey", linetype = "longdash", size=1) +
  geom_hline(yintercept = 0,  colour="grey", linetype = "longdash", size=0.5) +
  ylab("Percentage Change of Winter Wheat") +
  
  scale_fill_brewer(palette="Set3") 




