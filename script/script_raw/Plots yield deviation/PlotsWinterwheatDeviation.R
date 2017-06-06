###############################################################################################################################################################################################
########################
#### Plots für Luis ####
########################
## Plots, welche sich auf das Jahr 2003 beziehen und die Abweichung von Mittel der Jahre 1999 - 2010 darstellen ###

## Dependencies ##
'Scatterplot_Winterwheat_SMI: Dort werden die Variablen, welche hier relevant sind, erstellt. 
'
## Input ##
'
Yield_SMI_long_1999 <- Scatterplot_Winterwheat_SMI
'

## Packages ##
library("ggplot2")
library("plyr")
library("foreign")
library("plm")
library("car")
library("lattice")
library("zoo")
library("stringr")
library("scales")
library("mgcv")
library("apsrtable")

##############################################################################################################################################################################################
#### Load Data.Frame ####
Yield_SMI_long_1999<-read.csv("data/data_processed/Yield_SMI_long_1999.csv")




## Plots für das Jahr 2003 ##
Yield_SMI_long_1999_2003 <- subset(Yield_SMI_long_1999, year==2003)
head(Yield_SMI_long_1999_2003, 40)



### Violin Diagramms with points
violin1<- ggplot(Yield_SMI_long_1999_2003, aes(comId, demeanWW, color=comState)) +  geom_violin() + geom_point() +
  geom_abline(intercept = 0,slope=0, color="red") +
  theme_bw() + ggtitle( "Violin Diagramms of the deviation of winter wheat yield in the year 2003 from the mid-term average (1999-2003) for the federal states of Germany") +
  scale_y_continuous(labels = percent) + ylab("Deviation from mean average (1999-2010 of) of winter wheat") + xlab("Ids of counties") +  guides(colour = federalStates)
ggsave(violin1, file=".//figures/figures_exploratory/Luis/violin1.pdf", width=16, height=8)
ggsave(violin1, file=".//figures/figures_exploratory/Luis/violin1.tiff", width=16, height=8)


### Violin Diagramms no points
violon2<-ggplot(Yield_SMI_long_1999_2003, aes(comId, demeanWW, color=comState)) +  geom_violin() +
  geom_abline(intercept = 0,slope=0, color="red") +
  theme_bw() + ggtitle( "Violin Diagramms of the deviation of winter wheat yield in the year 2003 from the mid-term average (1999-2003) for the federal states of Germany") +
  scale_y_continuous(labels = percent) + ylab(("Percentage Deviation from average winter wheat yield")) + xlab("Ids of counties") +  guides(colour = federalStates)
ggsave(violin2, file=".//figures/figures_exploratory/Luis/violin2.pdf", width=16, height=8)
ggsave(violin2, file=".//figures/figures_exploratory/Luis/violin2.tiff", width=16, height=8)


### Points Geom
point<-ggplot(Yield_SMI_long_1999_2003, aes(comId, demeanWW, color=comState)) +  geom_point() +
  geom_abline(intercept = 0,slope=0, color="red") +
  theme_bw() + ggtitle( "Diagramms of the deviation of winter wheat yield in the year 2003 from the mid-term average (1999-2003) for the federal states of Germany") +
  scale_y_continuous(labels = percent) + ylab(("Percentage Deviation from average winter wheat yield")) + xlab("Ids of counties") +  guides(colour = federalStates)
ggsave(point, file=".//figures/figures_exploratory/Luis/point.pdf", width=16, height=8)
ggsave(point, file=".//figures/figures_exploratory/Luis/point.tiff", width=16, height=8)


### Boxplot
boxplot1<-ggplot(Yield_SMI_long_1999_2003, aes(comId, demeanWW, color=comState)) +  geom_boxplot() +
  geom_abline(intercept = 0,slope=0, color="red") + 
  theme_bw() + ggtitle( "Boxplots of the deviation of winter wheat yield in the year 2003 from the mid-term average (1999-2003) for the federal states of Germany") +
  scale_y_continuous(labels = percent) + ylab("Percentage Deviation from average winter wheat yield") + xlab("Ids of counties") +  guides(colour = federalStates)
ggsave(boxplot1, file=".//figures/figures_exploratory/Luis/boxplot1.pdf", width=16, height=8)
ggsave(boxplot1, file=".//figures/figures_exploratory/Luis/boxplot1.tiff", width=16, height=8)



boxplot2<-ggplot(Yield_SMI_long_1999_2003, aes(comId, demeanWW, color=comState)) +  geom_boxplot() + geom_point() +
  geom_abline(intercept = 0,slope=0, color="red") +
  theme_bw() + ggtitle( "Boxplots of the deviation of winter wheat yield in the year 2003 from the mid-term average (1999-2003) for the federal states of Germany") +
  scale_y_continuous(labels = percent) + ylab("Percentage Deviation from average winter wheat yield") + xlab("Ids of counties") +  guides(colour = federalStates)
ggsave(boxplot2, file=".//figures/figures_exploratory/Luis/boxplot2.pdf", width=16, height=8)
ggsave(boxplot2, file=".//figures/figures_exploratory/Luis/boxplot2.tiff", width=16, height=8)


### Histogram
histogram<-ggplot(Yield_SMI_long_1999_2003, aes(demeanWW)) +  geom_histogram(aes(fill=comState)) +
  geom_vline(xintercept = 0, color="red")  + 
  theme_bw() + ggtitle( "Histogram of the deviation of winter wheat yield in the year 2003 from the mid-term average (1999-2003) for the federal states of Germany") +
  xlab(("Percentage Deviation from average winter wheat yield")) + ylab("Counts") +  guides(colour = federalStates) +
  scale_x_continuous(labels = percent) 

ggsave(histogram, file=".//figures/figures_exploratory/Luis/histogram.pdf", width=16, height=8)
ggsave(histogram, file=".//figures/figures_exploratory/Luis/histogram.tiff", width=16, height=8)


### density plot
density<-ggplot(Yield_SMI_long_1999_2003, aes(demeanWW, fill=comState)) + 
  geom_density(position = "stack", aes(y = ..count..)) +
  geom_vline(xintercept = 0, color="red")  + 
  theme_bw() +
  ggtitle( "Density plot of the deviation of winter wheat yield in the year 2003 from the mid-term average (1999-2003) for the federal states of Germany") +
  xlab(("Percentage Deviation from average winter wheat yield")) + ylab("Counts") + scale_x_continuous(labels = percent)  + guides(fill = federalStates) 
density  

ggsave(density, file=".//figures/figures_exploratory/Luis/density.pdf", width=16, height=8)
ggsave(density, file=".//figures/figures_exploratory/Luis/density.tiff", width=16, height=8)
