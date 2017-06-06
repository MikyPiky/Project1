## Produce Boxplot of Silomais conditional on the years ##

library(ggthemes)
library(ggplot2)
## Load data.file which has log-detrended Silage Maize Yield Data ##
Yield_Covariates_SM_Oct <- read.csv(file="./data/data_raw/Yield_Covariates_SM_Oct.csv")
Yield_Covariates_SM_Oct$X <- NULL
names(Yield_Covariates_SM_Oct)
str(Yield_Covariates_SM_Oct)
Yield_Covariates_SM_Oct$year <- factor(Yield_Covariates_SM_Oct$year)

gg<- ggplot(Yield_Covariates_SM_Oct, aes(year, siloMaize_logtrend)) 
gg + 
  geom_abline(intercept = 0,slope=0, color="green",alpha = 6/10)   +
#   geom_jitter(width = 0.2) +
#   geom_smooth(span = 0.1, se=T, color="green") +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + theme_few() +
  theme(axis.text.x = element_text(angle=90, vjust=0, size=25),
        axis.text.y = element_text(angle=0, vjust=0.2,  size=25),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=25)) +
  ylab("Log(Silomais) - detrended")

ggsave(boxplot2, file=".//figures/figures_exploratory/Luis/boxplot2.pdf", width=16, height=8)
ggsave(boxplot2, file=".//figures/figures_exploratory/Luis/boxplot2.tiff", width=16, height=8)


## Load not detrended data of yield.
Yield_SMI_long <- read.csv("./data/data_processed/Yield_SMI_long" )
head(Yield_SMI_long)
table(Yield_SMI_long$year)
str(Yield_SMI_long$year)
summary(Yield_SMI_long$siloMaize)

################
## Delete NAs ##
################
sum(is.na(Yield_SMI_long) )
dim(Yield_SMI_long)
Yield_SMI_long_nna <- na.omit(Yield_SMI_long) 
dim(Yield_SMI_long)

## Check for NAs
any(is.na(Yield_SMI_long_nna))
## Reset Rownames
rownames(Yield_SMI_long) <- NULL

## Further work with DataFrame without Yield_Covariates_SM_May index ##
Yield_SMI_long <- Yield_SMI_long_nna 

mean(Yield_SMI_long$siloMaize)[1]

Yield_SMI_long$year <- factor(Yield_SMI_long$year)

gg<- ggplot(Yield_SMI_long, aes(year, siloMaize)) 

XY_SM_Year_withtrend<-gg + 
  geom_abline(intercept = mean(Yield_SMI_long$siloMaize)[1],slope=0, color="green",alpha = 6/10)   +
  #   geom_jitter(width = 0.2) +
  #   geom_smooth(span = 0.1, se=T, color="green") +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) + theme_few() +
  theme(axis.text.x = element_text(angle=90, vjust=0, size=25),
        axis.text.y = element_text(angle=0, vjust=0.2,  size=25),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=25)) +
  ylab("Log(Silomais) - not detrended")
XY_SM_Year_withtrend
ggsave(XY_SM_Year_withtrend, file=".//figures/figures_exploratory/Scatterplot_Yield_Year/XY_SM_Year_withtrend2.png", width=8, height=8)


