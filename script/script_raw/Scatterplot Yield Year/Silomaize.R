

## Load data.file which has log-detrended Silage Maize Yield Data ##

Yield_Covariates_SM_Oct <- read.csv(file="./data/data_raw/Yield_Covariates_SM_Oct.csv")
Yield_Covariates_SM_Oct$X <- NULL
names(Yield_Covariates_SM_Oct)
str(Yield_Covariates_SM_Oct)
Yield_Covariates_SM_Oct$year <- factor(Yield_Covariates_SM_Oct$year)

gg<- ggplot(Yield_Covariates_SM_Oct, aes(year, siloMaize_logtrend)) 
gg+ 
#   geom_jitter(width = 0.2) +
#   geom_smooth(span = 0.1, se=T, color="green") +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) 
boxplot
ggsave(boxplot2, file=".//figures/figures_exploratory/Luis/boxplot2.pdf", width=16, height=8)
ggsave(boxplot2, file=".//figures/figures_exploratory/Luis/boxplot2.tiff", width=16, height=8)