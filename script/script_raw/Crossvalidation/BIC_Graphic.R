#### Create Graphics for BIC ####

## Description of File ##
'
In diesem Script werden die verschiedenen BIC Werte der Model-Configurationen einschließlich des Supermodels geplotted. 
Anschließend werden die einzelnen Plots in einem Panel zusammengefügt.
'
## Input ##
'
Die _BIC Files im Crossvalidation Ordner für Silage Maize und Winterwheat

'
## Output
'
BIC Panels
'


## Libraries ##
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(stringr)

####################################################################################################################################################################
## Colors ##
# colors <- colorRampPalette(c("blue4","purple1","deeppink","wheat","salmon","red3","darkred","darkorange","orange1","yellow","green","green3","darkgreen"))
# pie(rep(1,11), col=colors(11))
# colors11 = colors(11)

brewer.pal.info

display.brewer.pal(12, name= "Paired")
colors11 <- brewer.pal(11, name= "Paired")
# colors11 <- colors[c(11:1))]
##################
## Winter Wheat ##

## October_lag ##
BIC_WW_10lag <- read.csv("./data/data_raw/BIC/BIC_WW_Oct_lag.csv")
BIC_WW_10lag$index <-  rep(1:55)


which.min(BIC_WW_10lag$BIC)
BIC_WW_10lag_mintot <- BIC_WW_10lag[which.min(BIC_WW_10lag$BIC),]

g10lag_WW <- ggplot(BIC_WW_10lag,aes(y=BIC, x=index, color = model))
g10lag_WW + labs(title="BIC of various model configurations", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28,  colour=colors20[6], linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_10lag_mintot, colour=colors11[1], size=20)

## October_lag no 2003 ##
BIC_WW_10lag_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_Oct_lag_no2003.csv")
BIC_WW_10lag_no2003$index <-  rep(1:55)


which.min(BIC_WW_10lag_no2003$BIC)
BIC_WW_10lag_no2003_mintot <- BIC_WW_10lag_no2003[which.min(BIC_WW_10lag_no2003$BIC),]

g10lag_WW <- ggplot(BIC_WW_10lag_no2003,aes(y=BIC, x=index, color = model))
g10lag_WW + labs(title="BIC of various model configurations - no2003", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_10lag_no2003_mintot, colour=colors11[1], size=20)



###################################################################################################################################################################
## November_lag ##
BIC_WW_11lag <- read.csv("./data/data_raw/BIC/BIC_WW_Nov_lag.csv")
BIC_WW_11lag$index <-  rep(1:55)
head(BIC_WW_11lag)

which.min(BIC_WW_11lag$BIC) # SMIPrecPET am besten
BIC_WW_11lag_mintot <- BIC_WW_11lag[which.min(BIC_WW_11lag$BIC),]

g11lag_WW <- ggplot(BIC_WW_11lag,aes(y=BIC, x=index, color = model))
g11lag_WW + labs(title="BIC of various model configurations", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_11lag_mintot, colour=colors20[2], size=20)

#########################
## November_lag no2003 ##
BIC_WW_11lag_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_Nov_lag_no2003.csv")
levels(BIC_WW_11lag_no2003$month) <- "November_lag_no2003"
BIC_WW_11lag_no2003$index <-  rep(1:55)
head(BIC_WW_11lag_no2003)

which.min(BIC_WW_11lag_no2003$BIC)
BIC_WW_11lag_no2003_mintot <- BIC_WW_11lag_no2003[which.min(BIC_WW_11lag_no2003$BIC),]

g11lag_no2003_WW <- ggplot(BIC_WW_11lag_no2003,aes(y=BIC, x=index, color = model))
g11lag_no2003_WW + labs(title="BIC of various model configurations", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_11lag_no2003_mintot, colour=colors20[2], size=20)


####################################################################################################################################################################
## December_lag ##
BIC_WW_12lag <- read.csv("./data/data_raw/BIC/BIC_WW_Dec_lag.csv")
BIC_WW_12lag$index <-  rep(1:55)


which.min(BIC_WW_12lag$BIC)
BIC_WW_12lag_mintot <- BIC_WW_12lag[which.min(BIC_WW_12lag$BIC),]

g12lag_WW <- ggplot(BIC_WW_12lag,aes(y=BIC, x=index, color = model))
g12lag_WW + labs(title="BIC of various model configurations", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_12lag_mintot, colour=colors11[1], size=20)

## December_lag no2003##
BIC_WW_12lag_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_Dec_lag_no2003.csv")
BIC_WW_12lag_no2003$index <-  rep(1:55)
head(BIC_WW_12lag_no2003)

which.min(BIC_WW_12lag_no2003$BIC)
BIC_WW_12lag_no2003_mintot <- BIC_WW_12lag_no2003[which.min(BIC_WW_12lag_no2003$BIC),]

g12lag_WW <- ggplot(BIC_WW_12lag_no2003,aes(y=BIC, x=index, color = model))
g12lag_WW + labs(title="BIC of various model configurations", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour=="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_12lag_no2003_mintot, colour=colors11[1], size=20)

####################################################################################################################################################################
## January ##
BIC_WW_1 <- read.csv("./data/data_raw/BIC/BIC_WW_Jan.csv")
BIC_WW_1$index <-  rep(1:55)

which.min(BIC_WW_1$BIC)
BIC_WW_1_mintot <- BIC_WW_1[which.min(BIC_WW_1$BIC),]

g1_WW <- ggplot(BIC_WW_1,aes(y=BIC, x=index, color = model))
g1_WW + labs(title="BIC of various model configurations", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28,  colour=colors20[6], linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_1_mintot, colour=colors20[1], size=20)

####################################################################################################################################################################
## January no 2003 ##
BIC_WW_1_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_Jan_no2003.csv")
BIC_WW_1_no2003$index <-  rep(1:55)

which.min(BIC_WW_1_no2003$BIC)
BIC_WW_1_no2003_mintot <- BIC_WW_1_no2003[which.min(BIC_WW_1_no2003$BIC),]

g1_no2003_WW <- ggplot(BIC_WW_1_no2003,aes(y=BIC, x=index, color = model))
g1_no2003_WW + labs(title="BIC of various model configurations", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_1_no2003_mintot, colour=colors11[1], size=20)



################################################################################################################################################################
## February ##
BIC_WW_2 <- read.csv("./data/data_raw/BIC/BIC_WW_Feb.csv")
BIC_WW_2$index <-  rep(1:55)
which.min(BIC_WW_2$BIC)
BIC_WW_2_mintot <- BIC_WW_2[which.min(BIC_WW_2$BIC),]

g2_WW <- ggplot(BIC_WW_2,aes(y=BIC, x=index, color = model))
g2_WW + labs(title="BIC of various model configurations", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28,  colour=colors20[6], linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_2_mintot, colour=colors20[1], size=20)

## February - no2003 ##
BIC_WW_2_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_Feb_no2003.csv")
levels(BIC_WW_2_no2003$month) <- "February_no2003"
BIC_WW_2_no2003$index <-  rep(1:55)
which.min(BIC_WW_2_no2003$BIC)
BIC_WW_2_no2003_mintot <- BIC_WW_2_no2003[which.min(BIC_WW_2_no2003$BIC),]

g2_WW <- ggplot(BIC_WW_2_no2003,aes(y=BIC, x=index, color = model))
g2_WW + labs(title="BIC of various model configurations", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = 1) + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_2_no2003_mintot, colour=colors11[1], size=20)

#####################################################################################################################################################################
## March ##
BIC_WW_3 <- read.csv("./data/data_raw/BIC/BIC_WW_Mar.csv")
BIC_WW_3$index <-  rep(1:55)
which.min(BIC_WW_3$BIC)
BIC_WW_3_mintot <- BIC_WW_3[which.min(BIC_WW_3$BIC),]

g3_WW <- ggplot(BIC_WW_3,aes(y=BIC, x=index, color = model))
g3_WW + labs(title="BIC of various model configurations", x="") +  
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_3_mintot, colour=colors11[1], size=20)

## March no 2003 ##
BIC_WW_3_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_Mar_no2003.csv")
BIC_WW_3_no2003$index <-  rep(1:55)
which.min(BIC_WW_3_no2003$BIC)
BIC_WW_3_no2003_mintot <- BIC_WW_3_no2003[which.min(BIC_WW_3_no2003$BIC),]

g3_WW_no2003 <- ggplot(BIC_WW_3_no2003,aes(y=BIC, x=index, color = model))
g3_WW_no2003 + labs(title="BIC of various model configurations", x="") +  
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_3_no2003_mintot,  size=20)

####################################################################################################################################################################
## April ##
BIC_WW_4 <- read.csv("./data/data_raw/BIC/BIC_WW_Apr.csv")
BIC_WW_4$index <-  rep(1:55)
which.min(BIC_WW_4$BIC)
BIC_WW_4_mintot <- BIC_WW_4[which.min(BIC_WW_4$BIC),]

g4_WW <- ggplot(BIC_WW_4,aes(y=BIC, x=index, color = model))
g4_WW + labs(title="BIC of various model configurations", x="") +   
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_4_mintot,  size=20)

## April no 2003 ##
BIC_WW_4_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_Apr_no2003.csv")
BIC_WW_4_no2003$index <-  rep(1:55)
which.min(BIC_WW_4_no2003$BIC)
BIC_WW_4_no2003_mintot <- BIC_WW_4_no2003[which.min(BIC_WW_4_no2003$BIC),]

g4_WW_no2003 <- ggplot(BIC_WW_4_no2003,aes(y=BIC, x=index, color = model))
g4_WW_no2003 + labs(title="BIC of various model configurations", x="") +   
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_4_no2003_mintot,  size=20)


####################################################################################################################################################################
## May ##

BIC_WW_5 <- read.csv("./data/data_raw/BIC/BIC_WW_May.csv")
BIC_WW_5

which.min(BIC_WW_5$BIC)
BIC_WW_5[which.min(BIC_WW_5$BIC),]

g5_WW <- ggplot(BIC_WW_5,aes(y=BIC, x=index, color = model))
g5_WW + labs(title="BIC of various model configurations", x="") +
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_5_mintot,  size=20)

## May - no 2003 ##
BIC_WW_5_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_May_no2003.csv")
BIC_WW_5_no2003

which.min(BIC_WW_5_no2003$BIC)
BIC_WW_5_no2003_mintot <- BIC_WW_5_no2003[which.min(BIC_WW_5_no2003$BIC),]

g5_WW_no2003 <- ggplot(BIC_WW_5_no2003,aes(y=BIC, x=index, color = model))
g5_WW_no2003 + labs(title="BIC of various model configurations", x="") +
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_5_no2003_mintot,  size=20)


###################################################################################################################################################################
## June ##
BIC_WW_6 <- read.csv("./data/data_raw/BIC/BIC_WW_Jun.csv")
BIC_WW_6$index <-  rep(1:55)

which.min(BIC_WW_6$BIC)
BIC_WW_6_mintot <- BIC_WW_6[which.min(BIC_WW_6$BIC),]

g6_WW <- ggplot(BIC_WW_6,aes(y=BIC, x=index, color = model))
g6_WW + labs(title="BIC of various model configurations", x="") +   
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_6_mintot, size=20)


## June no 2003 ##
BIC_WW_6_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_Jun_no2003.csv")
BIC_WW_6_no2003$index <-  rep(1:55)
levels(BIC_WW_6_no2003$month) <- "June_no2003"
head(BIC_WW_6_no2003)
which.min(BIC_WW_6_no2003$BIC)
BIC_WW_6_no2003_mintot <- BIC_WW_6_no2003[which.min(BIC_WW_6_no2003$BIC),]

g6_WW_no2003 <- ggplot(BIC_WW_6_no2003,aes(y=BIC, x=index, color = model))
g6_WW_no2003 + labs(title="BIC of various model configurations - no2003", x="") +   
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_6_no2003_mintot,  size=20)



####################################################################################################################################################################
## July ##
BIC_WW_7 <- read.csv("./data/data_raw/BIC/BIC_WW_Jul.csv")
BIC_WW_7$index <-  rep(1:55)
which.min(BIC_WW_7$BIC)
BIC_WW_7_mintot <- BIC_WW_7[which.min(BIC_WW_7$BIC),]

g7_WW <- ggplot(BIC_WW_7,aes(y=BIC, x=index, color = model))
g7_WW + labs(title="BIC of various model configurations", x="") +
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_7_mintot, colour=colors20[1], size=20)
  
## July - no2003 ##
BIC_WW_7_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_Jul_no2003.csv")
BIC_WW_7_no2003$index <-  rep(1:55)
which.min(BIC_WW_7_no2003$BIC)
BIC_WW_7_no2003_mintot <- BIC_WW_7_no2003[which.min(BIC_WW_7_no2003$BIC),]

g7_WW_no2003 <- ggplot(BIC_WW_7_no2003,aes(y=BIC, x=index, color = model))
g7_WW_no2003 + labs(title="BIC of various model configurations", x="") +
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_7_no2003_mintot, size=20)

#################################################################################################################################################################
## August ##
BIC_WW_8 <- read.csv("./data/data_raw/BIC/BIC_WW_Aug.csv")
BIC_WW_8$index <-  rep(1:55)
which.min(BIC_WW_8$BIC)
BIC_WW_8_mintot <- BIC_WW_8[which.min(BIC_WW_8$BIC),]

g8_WW <- ggplot(BIC_WW_8,aes(y=BIC, x=index,color=model)) + geom_point() 
g8_WW + labs(title="BIC of various model configurations", x="") +
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_8_mintot, colour=colors20[1], size=20)


## August - no2003##
BIC_WW_8_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_Aug_no2003.csv")
BIC_WW_8_no2003$index <-  rep(1:55)
which.min(BIC_WW_8_no2003$BIC)
BIC_WW_8_no2003_mintot <- BIC_WW_8_no2003[which.min(BIC_WW_8_no2003$BIC),]

g8_WW_no2003 <- ggplot(BIC_WW_8_no2003,aes(y=BIC, x=index,color=model)) + geom_point() 
g8_WW_no2003 + labs(title="BIC of various model configurations", x="") +
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 25), 
        legend.title = element_blank(), 
        legend.text = element_text(size=25), 
        axis.text.y = element_text(angle=0, vjust=1, size=25),
        axis.title.y = element_text(angle=0, size=25),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_WW_8_no2003_mintot, colour=colors20[1], size=20)


###########
## Total ##
BIC_WW <- as.data.frame(rbind(BIC_WW_10lag,BIC_WW_11lag,BIC_WW_12lag,BIC_WW_1,BIC_WW_2, BIC_WW_3, BIC_WW_4, BIC_WW_5, BIC_WW_6, BIC_WW_7, BIC_WW_8))
#BIC_WW_10lag_mintot,BIC_WW_11lag_no2003_mintot, BIC_WW_12lag_mintot, BIC_WW_1_mintot, 
levels(BIC_WW$month)[levels(BIC_WW$month)=="Mai"] <- "May"
head(BIC_WW)
BIC_WW_mintot <- as.data.frame(rbind( BIC_WW_2_mintot, BIC_WW_3_mintot,BIC_WW_4_mintot, BIC_WW_5_mintot,BIC_WW_6_mintot, BIC_WW_7_mintot, BIC_WW_8_no2003_mintot))
BIC_WW_SMI <- as.data.frame(rbind(BIC_WW_10lag[28,],BIC_WW_11lag_no2003[28,],BIC_WW_12lag[28,],BIC_WW_1[28,],
                                  BIC_WW_2[28,], BIC_WW_3[28,], BIC_WW_4[28,],BIC_WW_5[28,], BIC_WW_6[28,],
                                  BIC_WW_7[28,], BIC_WW_8[28,]))
levels(BIC_WW_SMI$month)[levels(BIC_WW_SMI$month)=="Mai"] <- "May"


g_WW <- ggplot(BIC_WW,aes(y=BIC, x=model_index,color=model))
g_WW + labs(title="BIC of various model configurations - Winter Wheat", x="") +
    geom_point(size=2, alpha=0.0001) +     theme_few() +
#       geom_jitter(size=10) +  # scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
#    geom_point(size=4, alpha=1) +
#   geom_violin(size=2.5) +
  
   geom_boxplot(size=2, width=0.5) +

  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") +
  theme_few() +
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y = element_text(angle=0, vjust=1, size=20),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 40),
        
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  guides(fill=guide_legend(title=NULL)) +
  facet_wrap( ~ month, nrow=2) +
  geom_vline(xintercept = 6.5,  colour="black", linetype = "longdash", size=0.4)  +
#   geom_point(data=BIC_WW_SMI, colour=colors[6], size=5) +
  geom_vline(xintercept = 2.5,  colour="grey", linetype = "longdash", size=0.3) +
  geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.3)  
#  geom_point(data=BIC_SM_mintot, colour="black", size=2)


####################
## Total -2003 vs no2003 ##
BIC_WW_2003_vs_no2003 <- as.data.frame(rbind(BIC_WW_10lag, BIC_WW_10lag_no2003,
                                     BIC_WW_11lag, BIC_WW_11lag_no2003,
                                     BIC_WW_12lag, BIC_WW_12lag_no2003,
                                     BIC_WW_1, BIC_WW_1_no2003, 
                                     BIC_WW_2, BIC_WW_2_no2003, 
                                     BIC_WW_3, BIC_WW_3_no2003, 
                                     BIC_WW_4, BIC_WW_4_no2003, 
                                     BIC_WW_5, BIC_WW_5_no2003, 
                                     BIC_WW_6, BIC_WW_6_no2003, 
                                     BIC_WW_7, BIC_WW_7_no2003, 
                                     BIC_WW_8, BIC_WW_8_no2003))

levels(BIC_WW_2003_vs_no2003$month)[levels(BIC_WW_2003_vs_no2003$month)=="Mai"] <- "May"

BIC_WW_mintot_2003_vs_no2003 <- as.data.frame(rbind( BIC_WW_10lag_mintot, BIC_WW_10lag_no2003_mintot,
                                      BIC_WW_11lag_mintot, BIC_WW_11lag_no2003_mintot,
                                      BIC_WW_12lag_mintot, BIC_WW_12lag_no2003_mintot, 
                                      BIC_WW_1_mintot,  BIC_WW_10lag_no2003_mintot,
                                      BIC_WW_2_mintot, BIC_WW_2_no2003_mintot, 
                                      BIC_WW_3_mintot, BIC_WW_3_no2003_mintot,
                                      BIC_WW_4_mintot, BIC_WW_4_no2003_mintot,
                                      BIC_WW_5_mintot, BIC_WW_5_no2003_mintot,
                                      BIC_WW_6_mintot, BIC_WW_6_no2003_mintot, 
                                      BIC_WW_7_mintot, BIC_WW_7_no2003_mintot, 
                                      BIC_WW_8_mintot, BIC_WW_8_no2003_mintot))

BIC_WW_SMI_2003_vs_no2003 <- as.data.frame(rbind(BIC_WW_10lag[28,], BIC_WW_10lag_no2003[28,],
                                         BIC_WW_11lag[28,], BIC_WW_11lag_no2003[28,],
                                         BIC_WW_12lag[28,], BIC_WW_12lag_no2003[28,],
                                         BIC_WW_1[28,], BIC_WW_1_no2003[28,], 
                                         BIC_WW_2[28,], BIC_WW_2_no2003[28,], 
                                         BIC_WW_3[28,], BIC_WW_3_no2003[28,], 
                                         BIC_WW_4[28,], BIC_WW_4_no2003[28,], 
                                         BIC_WW_5[28,], BIC_WW_5_no2003[28,],
                                         BIC_WW_6[28,], BIC_WW_6_no2003[28,],
                                         BIC_WW_7[28,], BIC_WW_7_no2003[28,],
                                         BIC_WW_8[28,],  BIC_WW_8_no2003[28,]))



g_WW_2003_vs_no2003 <- ggplot(BIC_WW_no2003,aes(y=BIC, x=model_index,color=model))
g_WW_2003_vs_no2003 + labs(title="BIC of various model configurations - Winter Wheat", x="") +
  geom_point(size=2, alpha=0.0001) +     theme_few() +
  #       geom_jitter(size=10) +  # scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  #    geom_point(size=4, alpha=1) +
  #   geom_violin(size=2.5) +
  
  geom_boxplot(size=2, width=0.5) +
  
  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") +
  theme_few() +
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y = element_text(angle=0, vjust=1, size=20),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 40),
        
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  guides(fill=guide_legend(title=NULL)) +
  facet_wrap( ~ month, nrow=3) +
  geom_vline(xintercept = 6.5,  colour="black", linetype = "longdash", size=0.4)  +

  geom_vline(xintercept = 2.5,  colour="grey", linetype = "longdash", size=0.3) +
  geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.3)  


#####################
## Total - no 2003 ##
BIC_WW_no2003 <- as.data.frame(rbind( BIC_WW_10lag_no2003,
                                             BIC_WW_11lag_no2003,
                                             BIC_WW_12lag_no2003,
                                             BIC_WW_1_no2003, 
                                             BIC_WW_2_no2003, 
                                             BIC_WW_3_no2003, 
                                             BIC_WW_4_no2003, 
                                             BIC_WW_5_no2003, 
                                             BIC_WW_6_no2003, 
                                             BIC_WW_7_no2003, 
                                             BIC_WW_8_no2003))

levels(BIC_WW_2003$month)[levels(BIC_WW_2003$month)=="Mai"] <- "May"

BIC_WW_mintot_no2003 <- as.data.frame(rbind(BIC_WW_10lag_no2003_mintot,
                                           BIC_WW_11lag_no2003_mintot,
                                           BIC_WW_12lag_no2003_mintot, 
                                           BIC_WW_10lag_no2003_mintot,
                                           BIC_WW_2_no2003_mintot, 
                                           BIC_WW_3_no2003_mintot,
                                           BIC_WW_4_no2003_mintot,
                                           BIC_WW_5_no2003_mintot,
                                           BIC_WW_6_no2003_mintot, 
                                           BIC_WW_7_no2003_mintot, 
                                           BIC_WW_8_no2003_mintot))

BIC_WW_SMI_no2003 <- as.data.frame(rbind(BIC_WW_10lag_no2003[28,],
                                        BIC_WW_11lag_no2003[28,],
                                        BIC_WW_12lag_no2003[28,],
                                        BIC_WW_1_no2003[28,], 
                                        BIC_WW_2_no2003[28,], 
                                        BIC_WW_3_no2003[28,], 
                                        BIC_WW_4_no2003[28,], 
                                        BIC_WW_5_no2003[28,],
                                        BIC_WW_6_no2003[28,],
                                        BIC_WW_7_no2003[28,],
                                        BIC_WW_8_no2003[28,]))



g_WW_2003_no2003 <- ggplot(BIC_WW_no2003,aes(y=BIC, x=model_index,color=model))
g_WW_2003_no2003 + labs(title="BIC of various model configurations - Winter Wheat", x="") +
  geom_point(size=2, alpha=0.0001) +     theme_few() +
  #       geom_jitter(size=10) +  # scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  #    geom_point(size=4, alpha=1) +
  #   geom_violin(size=2.5) +
  
  geom_boxplot(size=2, width=0.5) +
  
  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") +
  theme_few() +
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y = element_text(angle=0, vjust=1, size=20),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 40),
        
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  guides(fill=guide_legend(title=NULL)) +
  facet_wrap( ~ month, nrow=2) +
  geom_vline(xintercept = 6.5,  colour="black", linetype = "longdash", size=0.4)  +
  #   geom_point(data=BIC_WW_SMI, colour=colors[6], size=5) +
  geom_vline(xintercept = 2.5,  colour="grey", linetype = "longdash", size=0.3) +
  geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.3)  
# geom_point(data=BIC_WW_mintot_2003, colour="black", size=2)



###############################################################
## Show BIC Values of Standard Configuration of Winter Wheat ##

BIC_WW_2[which.min(BIC_WW_2$BIC[BIC_WW_2$model=="01_SMIPrecTavg"]),] # -5662.206 

BIC_WW_3[which.min(BIC_WW_3$BIC[BIC_WW_3$model=="01_SMIPrecTavg"]),] # -5423.768
BIC_WW_4[which.min(BIC_WW_4$BIC[BIC_WW_4$model=="01_SMIPrecTavg"]),] # -5125.372

BIC_WW_5[which.min(BIC_WW_5$BIC[BIC_WW_5$model=="01_SMIPrecTavg"]),] # -5045.607
BIC_WW_6[which.min(BIC_WW_6$BIC[BIC_WW_6$model=="01_SMIPrecTavg"]),] # -6268.861
BIC_WW_7[which.min(BIC_WW_7$BIC[BIC_WW_7$model=="01_SMIPrecTavg"]),] # -5104.292
BIC_WW_8[which.min(BIC_WW_8$BIC[BIC_WW_8$model=="01_SMIPrecTavg"]),] # -5568.724


##################################################################################################################################################################################################
###################
#### Silomais #####

################################################################
## May ##
BIC_SM_5 <- read.csv(file="./data/data_raw/BIC/BIC_SM_May.csv")

BIC_SM_5

which.min(BIC_SM_5$BIC)
BIC_SM_5[which.min(BIC_SM_5$BIC),]

# g5_SM <- ggplot(BIC_SM_5,aes(y=BIC, x=index, color = model))
# g5_SM + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5)

## May - no PET ##
# BIC_SM_5_noPET <- read.csv(file="./data/data_raw/BIC/BIC_SM_May_noPET.csv")
# BIC_SM_5_noPET
# which.min(BIC_SM_5_noPET$BIC)


# g5_SM_noPET <- ggplot(BIC_SM_5_noPET,aes(y=BIC, x=index, color = model))
# g5_SM_noPET + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

## May - no PET, no 2003 ##
# BIC_SM_5_noPET_no2003 <- read.csv(file="./data/data_raw/BIC/BIC_SM_May_no2003_noPET.csv")
# BIC_SM_5_noPET_no2003
# which.min(BIC_SM_5_noPET_no2003$BIC)


# g5_SM_noPET_no2003 <- ggplot(BIC_SM_5_noPET,aes(y=BIC, x=index, color = model))
# g5_SM_noPET_no2003 + labs(title="BIC of various model configurations - no 2003", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)


## May - no 2003 ##
# BIC_SM_5_no2003 <- read.csv(file="./data/data_raw/BIC/BIC_SM_May_no2003.csv")
# BIC_SM_5_no2003
# which.min(BIC_SM_5_no2003$BIC)

# 
# g5_SM_no2003 <- ggplot(BIC_SM_5,aes(y=BIC, x=index, color = model))
# g5_SM_no2003 + labs(title="BIC of various model configurations - no 2003", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

#####################################################################################
## June ##
BIC_SM_6 <- read.csv(file="./data/data_raw/BIC/BIC_SM_Jun.csv")
BIC_SM_6

# g6_SM <- ggplot(BIC_SM_6,aes(y=BIC, x=index, color = model))
# g6_SM + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5)
# 

# ## June - noPET 
# BIC_SM_6_noPET <- read.csv(file="./data/data_raw/BIC/BIC_SM_Jun_noPET.csv")
# BIC_SM_6_noPET[which.min(BIC_SM_6_noPET$BIC),]

# g6_SM_noPET <- ggplot(BIC_SM_6_noPET,aes(y=BIC, x=index, color = model))
# g6_SM_noPET + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

## June - noPET, no2003 ##
# BIC_SM_6_noPET_no2003 <- read.csv(file="./data/data_raw/BIC/BIC_SM_Jun_no2003_noPET.csv")
# BIC_SM_6_noPET_no2003[which.min(BIC_SM_6_noPET_no2003$BIC),]

# g6_SM_noPET_no2003 <- ggplot(BIC_SM_6_noPET_no2003,aes(y=BIC, x=index, color = model))
# g6_SM_noPET_no2003 + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

# ## June - no2003 ##
# BIC_SM_6_no2003 <- read.csv(file="./data/data_raw/BIC/BIC_SM_Jun_no2003.csv")
# BIC_SM_6_no2003[which.min(BIC_SM_6_no2003$BIC),]

# g6_SM_no2003 <- ggplot(BIC_SM_6_no2003,aes(y=BIC, x=index, color = model))
# g6_SM_no2003 + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) 
# 
# # +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

#################################################################
## July ##
BIC_SM_7 <- read.csv("./data/data_raw/BIC/BIC_SM_Jul.csv")

BIC_SM_7

BIC_SM_7[which.min(BIC_SM_7$BIC),]

# g7_SM <- ggplot(BIC_SM_7,aes(y=BIC, x=index, color = model))
# g7_SM + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5)
# 

## July _noPET ##
# BIC_SM_7_noPET <- read.csv("./data/data_raw/BIC/BIC_SM_Jul_noPET.csv")
# 
# BIC_SM_7_noPET
# 
# BIC_SM_7_noPET[which.min(BIC_SM_7_noPET$BIC),]

# g7_SM_noPET <- ggplot(BIC_SM_7_noPET,aes(y=BIC, x=index, color = model))
# g7_SM_noPET + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

# ## July _noPET , no 2003 ##
# BIC_SM_7_noPET_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Jul_no2003_noPET.csv")
# 
# BIC_SM_7_noPET_no2003
# 
# BIC_SM_7_noPET_no2003[which.min(BIC_SM_7_noPET_no2003$BIC),]

# g7_SM_noPET_no2003 <- ggplot(BIC_SM_7_noPET_no2003,aes(y=BIC, x=index, color = model))
# g7_SM_noPET_no2003 + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

## July  , no 2003 ##
# BIC_SM_7_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Jul_no2003.csv")
# 
# BIC_SM_7_no2003
# 
# BIC_SM_7_no2003[which.min(BIC_SM_7_no2003$BIC),]

# g7_SM_no2003 <- ggplot(BIC_SM_7_no2003,aes(y=BIC, x=index, color = model))
# g7_SM_no2003 + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) 
# +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

##########################################################################
## August ##
BIC_SM_8 <- read.csv("./data/data_raw/BIC/BIC_SM_Aug.csv")
BIC_SM_8
BIC_SM_8[which.min(BIC_SM_8$BIC),]


# g8_SM <- ggplot(BIC_SM_8,aes(y=BIC, x=index, color = model))
# g8_SM + labs(title="August - Silage Maize", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5)

## August - noPET##
# BIC_SM_8_noPET <- read.csv("./data/data_raw/BIC/BIC_SM_Aug_noPET.csv")
# BIC_SM_8_noPET
# BIC_SM_8_noPET[which.min(BIC_SM_8_noPET$BIC),]


# g8_SM_noPET <- ggplot(BIC_SM_8_noPET,aes(y=BIC, x=index, color = model))
# g8_SM_noPET + labs(title="August - Silage Maize", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

## August - noPET, no2003 ##
# BIC_SM_8_noPET_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Aug_no2003_noPET.csv")
# BIC_SM_8_noPET_no2003
# BIC_SM_8_noPET_no2003[which.min(BIC_SM_8_noPET_no2003$BIC),]


# g8_SM_noPET_no2003 <- ggplot(BIC_SM_8_noPET_no2003,aes(y=BIC, x=index, color = model))
# g8_SM_noPET_no2003 + labs(title="August - Silage Maize", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)
# 
# ## August -  no2003 ##
# BIC_SM_8_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Aug_no2003.csv")
# BIC_SM_8_no2003
# BIC_SM_8_no2003[which.min(BIC_SM_8_no2003$BIC),]

# 
# g8_SM_no2003 <- ggplot(BIC_SM_8_no2003,aes(y=BIC, x=index, color = model))
# g8_SM_no2003 + labs(title="August - Silage Maize", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)



#####################################################################
## September ##
BIC_SM_9 <- read.csv("./data/data_raw/BIC/BIC_SM_Sep.csv")
BIC_SM_9
which.min(BIC_SM_9$BIC)
BIC_SM_9[which.min(BIC_SM_9$BIC),]

# g9_SM <- ggplot(BIC_SM_9,aes(y=BIC, x=index, color = model))
# g9_SM + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5)



## September - noPET##
# BIC_SM_9_noPET <- read.csv("./data/data_raw/BIC/BIC_SM_Sep_noPET.csv")
# BIC_SM_9_noPET
# which.min(BIC_SM_9_noPET$BIC)
# BIC_SM_9_noPET[which.min(BIC_SM_9_noPET$BIC),]

# g9_SM_noPET <- ggplot(BIC_SM_9_noPET,aes(y=BIC, x=index, color = model))
# g9_SM_noPET + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

## September - noPET, no 2003##
# BIC_SM_9_noPET_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Sep_no2003_noPET.csv")
# BIC_SM_9_noPET_no2003
# which.min(BIC_SM_9_noPET_no2003$BIC)
# BIC_SM_9_noPET_no2003[which.min(BIC_SM_9_noPET_no2003$BIC),]

# g9_SM_noPET_no2003 <- ggplot(BIC_SM_9_noPET_no2003,aes(y=BIC, x=index, color = model))
# g9_SM_noPET_no2003 + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

## September - no 2003##
# BIC_SM_9_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Sep_no2003.csv")
# BIC_SM_9_no2003
# which.min(BIC_SM_9_no2003$BIC)
# BIC_SM_9_no2003[which.min(BIC_SM_9_no2003$BIC),]

# g9_SM_no2003 <- ggplot(BIC_SM_9_no2003,aes(y=BIC, x=index, color = model))
# g9_SM_no2003 + labs(title="BIC of various model configurations", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)
# 

############################################################################
## October ##
BIC_SM_10 <- read.csv("./data/data_raw/BIC/BIC_SM_Oct.csv")
BIC_SM_10
which.min(BIC_SM_10$BIC)
BIC_SM_10[which.min(BIC_SM_10$BIC),]

# g10_SM <- ggplot(BIC_SM_10,aes(y=BIC, x=index, color = model))
# g10_SM + labs(title="BIC of various model configurations", x="") +   
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 28.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5)

# ## October - noPET ##
# BIC_SM_10_noPET <- read.csv("./data/data_raw/BIC/BIC_SM_Oct_noPET.csv")
# BIC_SM_10_noPET
# which.min(BIC_SM_10_noPET$BIC)
# BIC_SM_10_noPET[which.min(BIC_SM_10_noPET$BIC),]

# g10_SM_noPET <- ggplot(BIC_SM_10_noPET,aes(y=BIC, x=index, color = model))
# g10_SM_noPET + labs(title="BIC of various model configurations", x="") +   
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

## October - noPET, no2003 ##
# BIC_SM_10_noPET_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Oct_no2003_noPET.csv")
# BIC_SM_10_noPET_no2003
# which.min(BIC_SM_10_noPET_no2003$BIC)
# BIC_SM_10_noPET_no2003[which.min(BIC_SM_10_noPET_no2003$BIC),]

# g10_SM_noPET_no2003 <- ggplot(BIC_SM_10_noPET_no2003,aes(y=BIC, x=index, color = model))
# g10_SM_noPET_no2003 + labs(title="BIC of various model configurations", x="") +   
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)

# ## October - no2003 ##
# BIC_SM_10_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Oct_no2003.csv")
# BIC_SM_10_no2003
# which.min(BIC_SM_10_no2003$BIC)
# BIC_SM_10_no2003[which.min(BIC_SM_10_no2003$BIC),]

# g10_SM_no2003 <- ggplot(BIC_SM_10_no2003,aes(y=BIC, x=index, color = model))
# g10_SM_no2003 + labs(title="BIC of various model configurations", x="") +   
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 25), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=25), 
#         axis.text.y = element_text(angle=0, vjust=1, size=25),
#         axis.title.y = element_text(angle=0, size=25),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
#   geom_vline(xintercept = 16.5,  colour="grey", linetype = "longdash", size=1) +
#   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.5) +
#   geom_vline(xintercept = 22.5,  colour="grey", linetype = "longdash", size=0.5)
# 

# ## Super ##
# ## For publication worth regression output need to change data names ##
# BIC_SM_Super <- read.csv(file="./data/data_raw/BIC/BIC_SM_super.csv")
# levels(BIC_SM_Super$month)[levels(BIC_SM_Super $month)=="Mai"] <- "May"
# 
# BIC_SM_Super
# which.min(BIC_SM_Super$BIC)
# BIC_SM_Super_mintot <- BIC_SM_Super[which.min(BIC_SM_Super$BIC),]
# 
# gsuper_SM <- ggplot(BIC_SM_Super,aes(y=BIC, x=index, color = model))
# gsuper_SM + labs(title="BIC of various model configurations", x="") +   
#     geom_point(size=2) +     
#   theme_few() +
#   scale_colour_manual(values = colors(20)[12:20]) + 
#   theme(plot.title=element_text(size=15, face="bold")) + theme_few() +
#   scale_x_discrete(breaks=NULL) +
#   facet_wrap( ~ month) + 
#   geom_point(data=BIC_SM_Super_mintot, colour="black")


############################################################################
##############################
## Growing Period, no Super ##
BIC_SM_gp <- as.data.frame(rbind(BIC_SM_5, BIC_SM_6, BIC_SM_7, BIC_SM_8, BIC_SM_9, BIC_SM_10))
levels(BIC_SM_gp$model)
BIC_SM_gp$model

head(BIC_SM_gp)

BIC_SM_gp$model1 <- BIC_SM_gp$model

#################################################
#### Change names of models for publications ####
BIC_SM_gp$model1 <- str_replace_all(BIC_SM_gp$model1, "01_SMIPrecTavg", "09_SMI_P_T")
BIC_SM_gp$model1 <- str_replace_all(BIC_SM_gp$model1, "02_SMIPrecPET", "11_SMI_P_E")
BIC_SM_gp$model1 <- str_replace_all(BIC_SM_gp$model1, "03_SMIPrec", "03_SMI_P")
BIC_SM_gp$model1 <- str_replace_all(BIC_SM_gp$model1, "04_SMIPET", "07_SMI_E")
BIC_SM_gp$model1 <- str_replace_all(BIC_SM_gp$model1, "05_SMITavg", "05_SMI_T")
BIC_SM_gp$model1 <- str_replace_all(BIC_SM_gp$model1, "06_SMI", "01_SMI")
BIC_SM_gp$model1 <- str_replace_all(BIC_SM_gp$model1, "07_Prec", "02_P")
BIC_SM_gp$model1 <- str_replace_all(BIC_SM_gp$model1, "08_Tavg", "04_T")
BIC_SM_gp$model1 <- str_replace_all(BIC_SM_gp$model1, "09_PET", "06_E")
BIC_SM_gp$model1 <- str_replace_all(BIC_SM_gp$model1, "10_PrecTavg", "08_P_T")
BIC_SM_gp$model1 <- str_replace_all(BIC_SM_gp$model1, "11_PrecPET", "10_P_E")
BIC_SM_gp$model1 <- factor(BIC_SM_gp$model1)

levels(BIC_SM_gp$model1)

BIC_SM_gp$model1_number <-  BIC_SM_gp$model1

##########################################
## Only create Model Names with numbers ##
BIC_SM_gp$model1_number <- str_replace_all(BIC_SM_gp$model1_number,  "09_SMI_P_T", "09")
BIC_SM_gp$model1_number <- str_replace_all(BIC_SM_gp$model1_number,  "11_SMI_P_E", "11")
BIC_SM_gp$model1_number <- str_replace_all(BIC_SM_gp$model1_number,  "03_SMI_P", "03")
BIC_SM_gp$model1_number <- str_replace_all(BIC_SM_gp$model1_number,  "07_SMI_E", "07")
BIC_SM_gp$model1_number <- str_replace_all(BIC_SM_gp$model1_number,  "05_SMI_T", "05")
BIC_SM_gp$model1_number <- str_replace_all(BIC_SM_gp$model1_number,  "01_SMI", "01")
BIC_SM_gp$model1_number <- str_replace_all(BIC_SM_gp$model1_number,  "02_P", "02")
BIC_SM_gp$model1_number <- str_replace_all(BIC_SM_gp$model1_number,  "04_T", "04")
BIC_SM_gp$model1_number <- str_replace_all(BIC_SM_gp$model1_number,  "06_E", "06")
BIC_SM_gp$model1_number <- str_replace_all(BIC_SM_gp$model1_number,  "08_P_T", "08")
BIC_SM_gp$model1_number <- str_replace_all(BIC_SM_gp$model1_number,  "10_P_E", "10")
BIC_SM_gp$model1_number <- factor(BIC_SM_gp$model1_number)

levels(BIC_SM_gp$model1_number)




# BIC_SM_gp$model1 <-factor(BIC_SM_gp$model1, levels=c("SMI","Prec","SMIPrec","Tavg","SMITavg","PET","SMIPET","PrecTavg","SMIPrecTavg","PrecPET","SMIPrecPET" ))
levels(BIC_SM_gp$model1)

BIC_SM_gp$month1 <- BIC_SM_gp$month
BIC_SM_gp$month1 <-str_replace_all(BIC_SM_gp$month1, "May", "May")
BIC_SM_gp$month1 <-str_replace_all(BIC_SM_gp$month1, "Jun", "June")
BIC_SM_gp$month1 <-str_replace_all(BIC_SM_gp$month1, "Jul", "July")
BIC_SM_gp$month1 <-str_replace_all(BIC_SM_gp$month1, "Aug", "August")
BIC_SM_gp$month1 <-str_replace_all(BIC_SM_gp$month1, "Sep", "September")
BIC_SM_gp$month1 <-str_replace_all(BIC_SM_gp$month1, "Oct", "October")
BIC_SM_gp$month1 <-factor(BIC_SM_gp$month1, levels=c(  "May" ,    "June"   ,   "July"  ,   "August"  ,  "September",    "October" ))

BIC_SM_gp$model_index1 <-factor(BIC_SM_gp$model_index, levels=c(6,7,3,8,5,9,4,10,1,11,2 ))
levels(BIC_SM_gp$model_index1)

dim(BIC_SM_gp)


## Add variable for pairwise coloring ##
One <- rep(1, 28)
Two <- rep(2, 27)

Pairwise <- as.data.frame(append(One,Two))

names(Pairwise) <- "Pairwise"
Pairwise$Pairwise <- as.factor(Pairwise$Pairwise)      

Pairwise <- rbind(Pairwise, Pairwise, Pairwise, Pairwise, Pairwise, Pairwise)

BIC_SM_gp <- cbind(BIC_SM_gp[,1:9], Pairwise)

str(BIC_SM_gp)
# 
# View(BIC_SM_gp)


# label1 <- data.frame(x = as.numeric(c(1,2.5,4.5,6.5,8.5,10.5)), y = c(-4100,-4100,-4100,-4100,-4100,-4100), label1 = c("SMI", "SMI/no SMI + Prec", "SMI/no SMI + Tavg", "SMI/no SMI + PET", "SMI/no SMI + Prec + Tavg", "SMI/no SMI + Prec + PET") )
# label1 <- as.data.frame(do.call(rbind, replicate(5, as.matrix(label1 ), simplify=FALSE)))
# label1
# str(label1)
# dim(label1)
# model1 <- as.data.frame(sort(rep(c("May" ,    "June"   ,   "July"  ,   "August"  ,  "September",    "October" ),5)))
# dim(model1)
# label2 <- cbind(label1, model1)
# names(label2) <- c("x", "y", "label1", "month1")
# label2
# str(label2)
# label2$x <- as.numeric(rep(c(1,2,3,4,5,6),5))
# label2$y <-as.numeric(rep(c(-4100,-4100,-4100,-4100,-4100,-4100),5))

###########################################
## Growing Period, no Super, modelindex ###
g_SM <- ggplot(BIC_SM_gp,aes(y=BIC, x=model1, color=Pairwise))
g_SM_full <- g_SM + labs(title="BIC of various model configurations - Silage Maize", x="") +
  geom_hline(yintercept = -5000,  colour="grey", linetype = "longdash", size=1)  +
  geom_hline(yintercept = -5500,  colour="grey", linetype = "longdash", size=1)  +
  geom_hline(yintercept = -6000,  colour="grey", linetype = "longdash", size=1)  +
  geom_hline(yintercept = -6500,  colour="grey", linetype = "longdash", size=1)  +
  geom_hline(yintercept = -7000,  colour="grey", linetype = "longdash", size=1)  +
  geom_point(size=4) +
  #   geom_violin(size=2.5) +
  #   geom_boxplot(size=2, width=0.5) +
  scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") +
  scale_colour_grey() +
  theme_few() +
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle=90, size=30, vjust=2),
        axis.text.y = element_text(angle=0, vjust=1, size=20),
        axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5,  size=20),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_blank(), 
        legend.position="none")  + 
  #   scale_x_discrete(breaks=NULL) +
  scale_y_continuous(limit=c(-7000,-4500)) +
    guides(fill=guide_legend(title=NULL)) +
  facet_wrap( ~ month1) +
  geom_vline(xintercept = 1.5,  colour="grey", linetype = "solid", size=0.6)  +
  geom_vline(xintercept = 3.5,  colour="grey", linetype = "longdash", size=0.8) +
  geom_vline(xintercept = 5.5,  colour="grey", linetype = "longdash", size=0.8)  + 
  geom_vline(xintercept = 7.5,  colour="grey", linetype = "solid", size=0.6)   +
  geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.8)  

# 
 g_SM_full 

ggsave("BIC_distribution_withNames.png", plot = g_SM_full, device = "png", path ="./figures/figures_exploratory/BIC/Silomaize",width = 16, height = 8)

ggsave("BIC_distribution_withNames.pdf", plot = g_SM_full, device = "pdf", path ="./figures/figures_exploratory/BIC/Silomaize",width = 16, height = 8)

which.min(BIC_SM_gp$BIC)
BIC_SM_gp_mintot <- BIC_SM_gp[which.min(BIC_SM_gp$BIC),]

## Für die Publication The Effect of Soil Moisture Anomalies on Maize Yield in Germany wurde die Grafik nachträglich mit inkscape bearbeitet. Zuerst wurde eine Version mit 
## Zahlen auf der x-Achse jedes Panels ausgegeben. Anschließend mit der Variablen Bezeichnung in 90 Grad. In inskape wurden dann die Variablen Bezeichnungen in die erste 
## Grafik eingefügt. Die enstprechende Datei ist BIC_distrinbution.svg. Da beim Speichern unter als PDF die Grafik weicher aussieht wurde stattdessen eine PDF gedrucht, 
## anschließend rotiert (online) und dann mit pdfcrop beschnitten (BIC_rotated_cropped.pdf)

#  geom_point(data=BIC_SM_mintot, colour="black", size=2)
getwd()


######################################
## Growing Period, no Super, no2003 ##
BIC_SM_gp_no2003 <- as.data.frame(rbind(BIC_SM_5_no2003, BIC_SM_6_no2003, BIC_SM_7_no2003, BIC_SM_8_no2003, BIC_SM_9_no2003, BIC_SM_10_no2003))
BIC_SM_gp_no2003$model1 <-  BIC_SM_gp_no2003$model
levels(BIC_SM_gp_no2003$model1)

BIC_SM_gp_no2003$model1 <-str_replace_all(BIC_SM_gp_no2003$model1, "01_SMIPrecTavg", "09_SMIPrecTavg")
BIC_SM_gp_no2003$model1 <-str_replace_all(BIC_SM_gp_no2003$model1, "02_SMIPrecPET", "11_SMIPrecPET")
BIC_SM_gp_no2003$model1 <-str_replace_all(BIC_SM_gp_no2003$model1, "03_SMIPrec", "03_SMIPrec")
BIC_SM_gp_no2003$model1 <-str_replace_all(BIC_SM_gp_no2003$model1, "04_SMIPET", "07_SMIPET")
BIC_SM_gp_no2003$model1 <-str_replace_all(BIC_SM_gp_no2003$model1, "05_SMITavg", "05_SMITavg")
BIC_SM_gp_no2003$model1 <-str_replace_all(BIC_SM_gp_no2003$model1, "06_SMI", "01_SMI")
BIC_SM_gp_no2003$model1 <-str_replace_all(BIC_SM_gp_no2003$model1, "07_Prec", "02_Prec")
BIC_SM_gp_no2003$model1 <-str_replace_all(BIC_SM_gp_no2003$model1, "08_Tavg", "04_Tavg")
BIC_SM_gp_no2003$model1 <-str_replace_all(BIC_SM_gp_no2003$model1, "09_PET", "06_PET")
BIC_SM_gp_no2003$model1 <-str_replace_all(BIC_SM_gp_no2003$model1, "10_PrecTavg", "08_PrecTavg")
BIC_SM_gp_no2003$model1 <-str_replace_all(BIC_SM_gp_no2003$model1, "11_PrecPET", "10_PrecPET")
BIC_SM_gp_no2003$model1 <- factor(BIC_SM_gp_no2003$model1)
# BIC_SM_gp_no2003$model1 <-factor(BIC_SM_gp_no2003$model1, levels=c("SMI","Prec","SMIPrec","Tavg","SMITavg","PET","SMIPET","PrecTavg","SMIPrecTavg","PrecPET","SMIPrecPET" ))
levels(BIC_SM_gp_no2003$model1)

BIC_SM_gp_no2003$month1 <- BIC_SM_gp_no2003$month
BIC_SM_gp_no2003$month1 <-str_replace_all(BIC_SM_gp_no2003$month1, "May_no2003", "May")
BIC_SM_gp_no2003$month1 <-str_replace_all(BIC_SM_gp_no2003$month1, "Jun_no2003", "June")
BIC_SM_gp_no2003$month1 <-str_replace_all(BIC_SM_gp_no2003$month1, "Jul_no2003", "July")
BIC_SM_gp_no2003$month1 <-str_replace_all(BIC_SM_gp_no2003$month1, "Aug_no2003", "August")
BIC_SM_gp_no2003$month1 <-str_replace_all(BIC_SM_gp_no2003$month1, "Sep_no2003", "September")
BIC_SM_gp_no2003$month1 <-str_replace_all(BIC_SM_gp_no2003$month1, "Oct_no2003", "October")
BIC_SM_gp_no2003$month1 <-factor(BIC_SM_gp_no2003$month1, levels=c(  "May" ,    "June"   ,   "July"  ,   "August"  ,  "September",    "October" ))
levels(BIC_SM_gp_no2003$month1)

BIC_SM_gp_no2003$model_index1 <-factor(BIC_SM_gp_no2003$model_index, levels=c(6,7,3,8,5,9,4,10,1,11,2 ))
levels(BIC_SM_gp_no2003$model_index1)



which.min(BIC_SM_gp_no2003$BIC)
BIC_SM_gp_no2003_mintot <- BIC_SM_gp_no2003[which.min(BIC_SM_gp_no2003$BIC),]




g_SM_no2003 <- ggplot(BIC_SM_gp_no2003,aes(y=BIC, x=model1, color=model1))
g_SM_full_no2003 <- g_SM_no2003 + labs(title="BIC of various model configurations - Silage Maize", x="") +
  geom_hline(yintercept = -4400,  colour="grey", linetype = "dotted", size=0.5)  +
  geom_hline(yintercept = -4800,  colour="grey", linetype = "dotted", size=0.5)  +
  geom_hline(yintercept = -5200,  colour="grey", linetype = "dotted", size=0.5)  +
  geom_point(size=4) +
  #   geom_violin(size=2.5) +
  #   geom_boxplot(size=2, width=0.5) +
  #   scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") +
  scale_colour_grey() +
  theme_few() +
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle=90, size=30, vjust=2),
        axis.text.y = element_text(angle=0, vjust=1, size=20),
        axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5,  size=20),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 20),
        legend.position="none") + 
  #   scale_x_discrete(breaks=NULL) +
  scale_y_continuous(limit=c(-5500,-4100)) +
  #   guides(fill=guide_legend(title=NULL)) +
  facet_wrap( ~ month1) +
  geom_vline(xintercept = 1.5,  colour="grey", linetype = "solid", size=0.6)  +
  geom_vline(xintercept = 3.5,  colour="grey", linetype = "longdash", size=0.8) +
  geom_vline(xintercept = 5.5,  colour="grey", linetype = "longdash", size=0.8)  + 
  geom_vline(xintercept = 7.5,  colour="grey", linetype = "solid", size=0.6)   +
  geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.8)  +
  geom_point(data=BIC_SM_gp_no2003_mintot, aes(y=BIC, x=model1, color="red", size=20))

#   geom_vline(xintercept = 2.5,  colour="grey", linetype = "longdash", size=0.3) +
#   geom_vline(xintercept = 4.5,  colour="grey", linetype = "longdash", size=0.3)  + 
#   geom_vline(xintercept = 6.5,  colour="grey", linetype = "longdash", size=0.3)   +
#   geom_vline(xintercept = 8.5,  colour="grey", linetype = "longdash", size=0.3)  +
#   geom_vline(xintercept = 10.5,  colour="grey", linetype = "longdash", size=0.3) 

g_SM_full_no2003



#  geom_point(data=BIC_SM_mintot, colour="black", size=2)


############################
## Growing Period - noPET ##
BIC_SM_gp_noPET <- as.data.frame(rbind(BIC_SM_5_noPET, BIC_SM_6_noPET, BIC_SM_7_noPET, BIC_SM_8_noPET, BIC_SM_9_noPET, BIC_SM_10_noPET))
BIC_SM_gp_noPET$month1 <- BIC_SM_gp_noPET$month
BIC_SM_gp_noPET$month1 <-str_replace_all(BIC_SM_gp_noPET$month1, "May", "May")
BIC_SM_gp_noPET$month1 <-str_replace_all(BIC_SM_gp_noPET$month1, "Jun", "June")
BIC_SM_gp_noPET$month1 <-str_replace_all(BIC_SM_gp_noPET$month1, "Jul", "July")
BIC_SM_gp_noPET$month1 <-str_replace_all(BIC_SM_gp_noPET$month1, "Aug", "August")
BIC_SM_gp_noPET$month1 <-str_replace_all(BIC_SM_gp_noPET$month1, "Sep", "September")
BIC_SM_gp_noPET$month1 <-str_replace_all(BIC_SM_gp_noPET$month1, "Oct", "October")
BIC_SM_gp_noPET$month1 <-factor(BIC_SM_gp_noPET$month1, levels=c(  "May" ,    "June"   ,   "July"  ,   "August"  ,  "September",    "October" ))

g_SM_noPET <- ggplot(BIC_SM_gp_noPET,aes(y=BIC, x=model_index, color=model))
g_SM_noPET + 
  labs(title="BIC of various model configurations - Silage Maize", x="") +
  geom_point(size=5, alpha=1) +     
  #       geom_jitter(size=10) +  # scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  #    geom_point(size=4, alpha=1) +
  #   geom_violin(size=2.5) +
#   geom_point(size=2, width=0.5) +
  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") +
  theme_few() +
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y = element_text(angle=0, vjust=1, size=20),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 40),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  guides(fill=guide_legend(title=NULL)) +
  facet_wrap( ~ month1) +
  geom_vline(xintercept = 4.5,  colour="black", linetype = "longdash", size=0.4)  +
  geom_vline(xintercept = 1.5,  colour="grey", linetype = "longdash", size=0.3) +
  geom_vline(xintercept = 6.5,  colour="grey", linetype = "longdash", size=0.3) 

####################################
## Growing Period - noPET, no2003 ##
BIC_SM_gp_noPET_no2003 <- as.data.frame(rbind(BIC_SM_5_noPET_no2003, BIC_SM_6_noPET_no2003, BIC_SM_7_noPET_no2003, BIC_SM_8_noPET_no2003, 
                                              BIC_SM_9_noPET_no2003, BIC_SM_10_noPET_no2003))
BIC_SM_gp_noPET_no2003$month1 <- BIC_SM_gp_noPET_no2003$month
BIC_SM_gp_noPET_no2003$month1 <-str_replace_all(BIC_SM_gp_noPET_no2003$month1, "May_no2003", "May")
BIC_SM_gp_noPET_no2003$month1 <-str_replace_all(BIC_SM_gp_noPET_no2003$month1, "Jun_no2003", "June")
BIC_SM_gp_noPET_no2003$month1 <-str_replace_all(BIC_SM_gp_noPET_no2003$month1, "Jul_no2003", "July")
BIC_SM_gp_noPET_no2003$month1 <-str_replace_all(BIC_SM_gp_noPET_no2003$month1, "Aug_no2003", "August")
BIC_SM_gp_noPET_no2003$month1 <-str_replace_all(BIC_SM_gp_noPET_no2003$month1, "Sep_no2003", "September")
BIC_SM_gp_noPET_no2003$month1 <-str_replace_all(BIC_SM_gp_noPET_no2003$month1, "Oct_no2003", "October")
BIC_SM_gp_noPET_no2003$month1 <- as.factor(BIC_SM_gp_noPET$month1)
BIC_SM_gp_noPET_no2003$month1 <-factor(BIC_SM_gp_noPET$month1, levels=c(  "May" ,    "June"   ,   "July"  ,   "August"  ,  "September",    "October" ))

g_SM_noPET_no2003 <- ggplot(BIC_SM_gp_noPET_no2003,aes(y=BIC, x=model_index, color=model))
g_SM_noPET_no2003 + 
  labs(title="BIC of various model configurations - Silage Maize", x="") +
  geom_point(size=5, alpha=1) +     
  #       geom_jitter(size=10) +  # scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  #    geom_point(size=4, alpha=1) +
  #   geom_violin(size=2.5) +
  #   geom_point(size=2, width=0.5) +
  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") +
  theme_few() +
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y = element_text(angle=0, vjust=1, size=20),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 40),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  guides(fill=guide_legend(title=NULL)) +
  facet_wrap( ~ month1) +
  geom_vline(xintercept = 4.5,  colour="black", linetype = "longdash", size=0.4)  +
  geom_vline(xintercept = 1.5,  colour="grey", linetype = "longdash", size=0.3) +
  geom_vline(xintercept = 6.5,  colour="grey", linetype = "longdash", size=0.3) 



#######################
## July August Super ##
BIC_PrecJulTavgJulSMIAug <- read.csv( file="./data/data_raw/BIC/BIC_PrecJulTavgJulSMIAug.csv")
names(BIC_PrecJulTavgJulSMIAug) <- names(BIC_SM_7)
BIC_SM_JulAugSup <-NULL
BIC_SM_JulAugSup <- as.data.frame(rbind(BIC_SM_7, BIC_SM_8,BIC_PrecJulTavgJulSMIAug))
names(BIC_SM_JulAugSup)

## Subframe für SMI
BIC_SMI_JulAugSup <- as.data.frame(rbind(BIC_SM_7[28,], BIC_SM_8[28,]))

## Mark minimum Point of each month
BIC_SM_mintot_JulAugSup <- as.data.frame(rbind(BIC_SM_7_mintot, BIC_SM_8_mintot))

## July August Super
g_SM <- ggplot(BIC_SM_JulAugSup,aes(y=BIC, x=model_index, color=model))
g_SM + labs(title="BIC of various model configurations - Silage Maize", x="") +
  #    geom_jitter(size=10) +  # scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  geom_point(size=1, alpha=1) +
  geom_violin(size=2.5) +
  theme_few() +
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle=90, vjust=0, size=30, face="bold"),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 30),  
        axis.text.y = element_text(angle=0, vjust=1, size=30),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  guides(fill=guide_legend(title="Model")) +
  facet_grid( ~ month) +
 geom_vline(xintercept = 6,  linetype = "longdash", size=0.4)  +
  geom_point(data=BIC_SMI_JulAugSup, size=4) +
   geom_vline(xintercept = 2.5,  colour="grey", linetype = "longdash", size=0.3) +
   geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.3)  
#  geom_point(data=BIC_SM_mintot, colour="black", size=2)


## Show BIC Values of Standard Configuration #
BIC_SM_5[which.min(BIC_SM_5$BIC[BIC_SM_5$model=="01_SMIPrecTavg"]),] # -4266.159
BIC_SM_6[which.min(BIC_SM_6$BIC[BIC_SM_6$model=="01_SMIPrecTavg"]),] # -4938.556
BIC_SM_7[which.min(BIC_SM_7$BIC[BIC_SM_7$model=="01_SMIPrecTavg"]),] # -5262.63
BIC_SM_8[which.min(BIC_SM_8$BIC[BIC_SM_8$model=="01_SMIPrecTavg"]),] # -5107.538
BIC_SM_9[which.min(BIC_SM_9$BIC[BIC_SM_9$model=="01_SMIPrecTavg"]),] # -4464.088
BIC_SM_10[which.min(BIC_SM_10$BIC[BIC_SM_10$model=="01_SMIPrecTavg"]),] # -4562.911
