#### Create Graphics for BIC ####

## Description of File ##
'
In diesem Script werden die verschiedenen BIC Werte der Model-Configurationen einschließlich des Supermodels geplotted. 
Anschließend werden die einzelnen Plots in einem Panel zusammengefügt.
'
## Input ##
'
Die _BIC Files im Crossvalidation Ordner für Silomaize und Winterwheat

'
## Output
'
BIC Panels
'


## Libraries ##
library(ggplot2)
library(RColorBrewer)
library(ggthemes)

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

which.min(BIC_WW_11lag$BIC) # SMIPrecPet am besten
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
## Mai ##

BIC_WW_5 <- read.csv("./data/data_raw/BIC/BIC_WW_Mai.csv")
BIC_WW_5$index <-  rep(1:55)
which.min(BIC_WW_5$BIC)
BIC_WW_5_mintot <- BIC_WW_5[which.min(BIC_WW_5$BIC),]

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

## Mai - no 2003 ##
BIC_WW_5_no2003 <- read.csv("./data/data_raw/BIC/BIC_WW_May_no2003.csv")
BIC_WW_5_no2003$index <-  rep(1:55)
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
g6_WW + labs(title="BIC of various model configurations - June", x="") +   
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(
#     plot.title=element_blank(), 
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


head(BIC_WW_6_no2003)
which.min(BIC_WW_6_no2003$BIC)
BIC_WW_6_no2003_mintot <- BIC_WW_6_no2003[which.min(BIC_WW_6_no2003$BIC),]

g6_WW_no2003 <- ggplot(BIC_WW_6_no2003,aes(y=BIC, x=index, color = model))
g6_WW_no2003 + labs(title="BIC of various model configurations - no2003", x="") +   
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") + 
  theme(
#     plot.title=element_blank(), 
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
  geom_point(data=BIC_WW_8_no2003_mintot,  size=20)


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



g_WW_2003_vs_no2003 <- ggplot(BIC_WW_2003_vs_no2003,aes(y=BIC, x=model_index,color=model))
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
  geom_point(data=BIC_WW_mintot_2003_vs_no2003, colour="black", size=2)

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



###################
#### Silomais #####


## Mai ##
BIC_SM_5 <- read.csv(file="./data/data_raw/BIC/BIC_SM_Mai.csv")
str(BIC_SM_5)
levels(BIC_SM_5$month)[levels(BIC_SM_5$month)=="Mai"] <- "May"
BIC_SM_5$index <-  rep(1:55)
which.min(BIC_SM_5$BIC)
which.min(BIC_SM_5$model=="01_SMIPrecTavg")
which.min(BIC_SM_5$BIC[BIC_SM_5$model=="01_SMIPrecTavg"])
BIC_SM_5[which.min(BIC_SM_5$BIC[BIC_SM_5$model=="01_SMIPrecTavg"]),] # -4266.159
BIC_SM_5_mintot <- BIC_SM_5[which.min(BIC_SM_5$BIC),]

g5_SM <- ggplot(BIC_SM_5,aes(y=BIC, x=index, color = model))
g5_SM + labs(title="BIC of various model configurations", x="") +    
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
  geom_vline(xintercept = 28,   linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_SM_5_mintot,  size=20)

## May_no2002 ##
BIC_SM_5_no2003 <- read.csv(file="./data/data_raw/BIC/BIC_SM_May_no2003.csv")
str(BIC_SM_5_no2003)
levels(BIC_SM_5_no2003$month)[levels(BIC_SM_5_no2003$month)=="Mai"] <- "May"
BIC_SM_5_no2003$index <-  rep(1:55)
which.min(BIC_SM_5_no2003$BIC)
which.min(BIC_SM_5_no2003$model=="01_SMIPrecTavg")
which.min(BIC_SM_5_no2003$BIC[BIC_SM_5_no2003$model=="01_SMIPrecTavg"])
BIC_SM_5_no2003[which.min(BIC_SM_5_no2003$BIC[BIC_SM_5_no2003$model=="01_SMIPrecTavg"]),] # -4266.159
BIC_SM_5_no2003_mintot <- BIC_SM_5_no2003[which.min(BIC_SM_5_no2003$BIC),]

g5_SM <- ggplot(BIC_SM_5_no2003,aes(y=BIC, x=index, color = model))
g5_SM + labs(title="BIC of various model configurations", x="") +    
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
  geom_vline(xintercept = 28,   linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_SM_5_no2003_mintot, size=20)
#################################################################################################################################################################
## June ##
BIC_SM_6 <- read.csv(file="./data/data_raw/BIC/BIC_SM_Jun.csv")
levels(BIC_SM_6$month)[levels(BIC_SM_6$month)=="Mai"] <- "May"
BIC_SM_6$index <-  rep(1:55)  
which.min(BIC_SM_6$BIC)
BIC_SM_6_mintot <- BIC_SM_6[which.min(BIC_SM_6$BIC),]

g6_SM <- ggplot(BIC_SM_6,aes(y=BIC, x=index, color = model))
g6_SM + labs(title="BIC of various model configurations", x="") +    
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
  geom_point(data=BIC_SM_6_mintot, colour=colors20[11], size=20)

## June - _no2003 ##
BIC_SM_6_no2003 <- read.csv(file="./data/data_raw/BIC/BIC_SM_Jun_no2003.csv")
head(BIC_SM_6_no2003)
BIC_SM_6_no2003$index <-  rep(1:55)  
which.min(BIC_SM_6_no2003$BIC)
BIC_SM_6_no2003_mintot <- BIC_SM_6_no2003[which.min(BIC_SM_6_no2003$BIC),]

g6_SM_no2003 <- ggplot(BIC_SM_6_no2003,aes(y=BIC, x=index, color = model))
g6_SM_no2003 + labs(title="BIC of various model configurations", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired") + 
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
  geom_vline(xintercept = 28.5,   linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_SM_6_no2003_mintot,  size=20)

###################################################################################################################################################################
## July ##
BIC_SM_7 <- read.csv("./data/data_raw/BIC/BIC_SM_Jul.csv")
levels(BIC_SM_7$month)[levels(BIC_SM_7$month)=="Mai"] <- "May"
BIC_SM_7$index <-  rep(1:55)
which.min(BIC_SM_7$BIC)
BIC_SM_7_mintot <- BIC_SM_7[which.min(BIC_SM_7$BIC),]

g7_SM <- ggplot(BIC_SM_7,aes(y=BIC, x=index, color = model))
g7_SM + labs(title="BIC of various model configurations", x="") +    
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
  geom_point(data=BIC_SM_7_mintot, colour=colors20[1], size=20)

## July_no2003 ##
BIC_SM_7_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Jul_no2003.csv")
levels(BIC_SM_7_no2003$month)[levels(BIC_SM_7$month)=="Mai"] <- "May"
BIC_SM_7_no2003$index <-  rep(1:55)
which.min(BIC_SM_7_no2003$BIC)
BIC_SM_7_mintot_no2003 <- BIC_SM_7_no2003[which.min(BIC_SM_7_no2003$BIC),]

g7_SM_no2003 <- ggplot(BIC_SM_7_no2003,aes(y=BIC, x=index, color = model))
g7_SM_no2003 + labs(title="BIC of various model configurations", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired") + 
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
  geom_vline(xintercept = 28.5,  linetype = "longdash", size=1) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_SM_7_mintot_no2003, size=20)

##################################################################################################################################################################
## August ##
BIC_SM_8 <- read.csv("./data/data_raw/BIC/BIC_SM_Aug.csv")
levels(BIC_SM_8$month)[levels(BIC_SM_8$month)=="Mai"] <- "May"
BIC_SM_8$index <-  rep(1:55)
which.min(BIC_SM_8$BIC)
BIC_SM_8_mintot <- BIC_SM_8[which.min(BIC_SM_8$BIC),]
BIC_SM_8_bestSt <- subset(BIC_SM_8, model=="01_SMIPrecTavg")

g8_SM <- ggplot(BIC_SM_8,aes(y=BIC, x=index, color = model))
g8_SM + labs(title="August - Silomaize", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size=30), 
        axis.text.y = element_text(angle=0, vjust=1, size=30),
        axis.title.y = element_text(angle=90, size=30),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
#   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="black", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_SM_8_mintot, colour=colors20[1], size=20)

## August_no2003 ##
BIC_SM_8_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Aug_no2003.csv")
levels(BIC_SM_8_no2003$month)[levels(BIC_SM_8_no2003$month)=="Mai"] <- "May"
BIC_SM_8_no2003$index <-  rep(1:55)
which.min(BIC_SM_8_no2003$BIC)
BIC_SM_8_no2003_mintot <- BIC_SM_8_no2003[which.min(BIC_SM_8_no2003$BIC),]
BIC_SM_8_no2003_bestSt <- subset(BIC_SM_8_no2003, model=="01_SMIPrecTavg")

g8_no2003_SM <- ggplot(BIC_SM_8_no2003,aes(y=BIC, x=index, color = model))
g8_no2003_SM + labs(title="August - Silomaize", x="") +    
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size=30), 
        axis.text.y = element_text(angle=0, vjust=1, size=30),
        axis.title.y = element_text(angle=90, size=30),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="black", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_point(data=BIC_SM_8_no2003_mintot, size=20)

# 
# ## Best Standard Model August
# g8_SM <- ggplot(BIC_SM_8_bestSt,aes(y=BIC, x=index, color = model))
# g8_SM + labs(title="August - Silomaize", x="") +    
#   geom_point(size=10) +     
#   theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
#   theme(plot.title=element_blank(), 
#         panel.grid.major.x = element_blank(),
#         strip.text.x = element_text(size = 40), 
#         legend.title = element_blank(), 
#         legend.text = element_text(size=40), 
#         axis.text.y = element_text(angle=0, vjust=1, size=40),
#         axis.title.y = element_text(angle=0, size=40),
#         legend.position="bottom") + 
#   scale_x_discrete(breaks=NULL) +
#   #   facet_wrap( ~ month) + 
# 
#   geom_point(data=BIC_SM_8_mintot, colour=colors20[1], size=20)

###################################################################################################################################################################
## September ##
BIC_SM_9 <- read.csv("./data/data_raw/BIC/BIC_SM_Sep.csv")
levels(BIC_SM_9$month)[levels(BIC_SM_9$month)=="Mai"] <- "May"
BIC_SM_9$index <-  rep(1:55)

which.min(BIC_SM_9$BIC)
BIC_SM_9_mintot <- BIC_SM_9[which.min(BIC_SM_9$BIC),]

g9_SM <- ggplot(BIC_SM_9,aes(y=BIC, x=index, color = model))
g9_SM + labs(title="BIC of various model configurations", x="") +    
    geom_point(size=2) +     theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  theme(plot.title=element_text(size=15, face="bold")) + theme_few() +
  scale_x_discrete(breaks=NULL) +
  facet_wrap( ~ month) + 
  geom_vline(xintercept = 28,  colour=colors20[6], linetype = "longdash", size=0.2) +
  geom_point(data=BIC_SM_9_mintot, colour="black")

## September_no2003 ##
BIC_SM_9_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Sep_no2003.csv")
BIC_SM_9_no2003$index <-  rep(1:55)

which.min(BIC_SM_9_no2003$BIC)
BIC_SM_9_no2003_mintot <- BIC_SM_9_no2003[which.min(BIC_SM_9_no2003$BIC),]

g9_no2003_SM <- ggplot(BIC_SM_9_no2003,aes(y=BIC, x=index, color = model))
g9_no2003_SM + labs(title="BIC of various model configurations", x="") +  
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size=30), 
        axis.text.y = element_text(angle=0, vjust=1, size=30),
        axis.title.y = element_text(angle=90, size=30),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="black", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5)

#   geom_point(data=BIC_SM_9_no2003_mintot, size=20)

###################################################################################################################################################################
## October ##
BIC_SM_10 <- read.csv("./data/data_raw/BIC/BIC_SM_Oct.csv")
levels(BIC_SM_10$month)[levels(BIC_SM_10$month)=="Mai"] <- "May"


BIC_SM_10$index <-  rep(1:55)
which.min(BIC_SM_10$BIC)
BIC_SM_10_mintot <- BIC_SM_10[which.min(BIC_SM_10$BIC),]

g10_SM <- ggplot(BIC_SM_10,aes(y=BIC, x=index, color = model))
g10_SM + labs(title="BIC of various model configurations", x="") +   
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size=30), 
        axis.text.y = element_text(angle=0, vjust=1, size=30),
        axis.title.y = element_text(angle=90, size=30),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="black", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5)

## October_no2003 ##
BIC_SM_10_no2003 <- read.csv("./data/data_raw/BIC/BIC_SM_Oct_no2003.csv")
levels(BIC_SM_10_no2003$month)[levels(BIC_SM_10_no2003$month)=="Mai"] <- "May"


BIC_SM_10_no2003$index <-  rep(1:55)
which.min(BIC_SM_10_no2003$BIC)
BIC_SM_10_no2003_mintot <- BIC_SM_10_no2003[which.min(BIC_SM_10_no2003$BIC),]

g10_no2003_SM <- ggplot(BIC_SM_10_no2003,aes(y=BIC, x=index, color = model))
g10_no2003_SM + labs(title="BIC of various model configurations", x="") +   
  geom_point(size=10) +     
  theme_few() +  scale_colour_brewer(type = "qual", palette = "Paired") + 
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 30), 
        legend.title = element_blank(), 
        legend.text = element_text(size=30), 
        axis.text.y = element_text(angle=0, vjust=1, size=30),
        axis.title.y = element_text(angle=90, size=30),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  #   facet_wrap( ~ month) + 
  geom_vline(xintercept = 28.5,  colour="black", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 18.5,  colour="grey", linetype = "longdash", size=0.5) +
  geom_vline(xintercept = 37.5,  colour="grey", linetype = "longdash", size=0.5)

## Super ##
## For publication worth regression output need to change data names ##
BIC_SM_Super <- read.csv(file="./data/data_raw/BIC/BIC_SM_super.csv")
levels(BIC_SM_Super$month)[levels(BIC_SM_Super $month)=="Mai"] <- "May"

BIC_SM_Super
which.min(BIC_SM_Super$BIC)
BIC_SM_Super_mintot <- BIC_SM_Super[which.min(BIC_SM_Super$BIC),]

gsuper_SM <- ggplot(BIC_SM_Super,aes(y=BIC, x=index, color = model))
gsuper_SM + labs(title="BIC of various model configurations", x="") +   
    geom_point(size=2) +     
  theme_few() +
  scale_colour_manual(values = colors(20)[12:20]) + 
  theme(plot.title=element_text(size=15, face="bold")) + theme_few() +
  scale_x_discrete(breaks=NULL) +
  facet_wrap( ~ month) + 
  geom_point(data=BIC_SM_Super_mintot, colour="black")


############################
## Total - no comparision ##
BIC_SM_gp <- as.data.frame(rbind(BIC_SM_5, 
                                 BIC_SM_6, 
                                 BIC_SM_7, 
                                 BIC_SM_8, 
                                 BIC_SM_9, 
                                 BIC_SM_10))
levels(BIC_SM_gp$month)[levels(BIC_SM_gp$month)=="Mai"] <- "May"
## Subframe für SMI
BIC_SMI <- as.data.frame(rbind(BIC_SM_5[28,], 
                               BIC_SM_6[28,], 
                               BIC_SM_7[28,], 
                               BIC_SM_8[28,], 
                               BIC_SM_9[28,],
                               BIC_SM_10[28,]))

## Mark minimum Point of each month
BIC_SM_mintot <- as.data.frame(rbind(BIC_SM_5_mintot,
                                     BIC_SM_6_mintot, 
                                     BIC_SM_7_mintot, 
                                     BIC_SM_8_mintot, 
                                     BIC_SM_9_mintot, 
                                     BIC_SM_10_mintot))

## Growing Period, no Super, modelindex
g_SM <- ggplot(BIC_SM_gp,aes(y=BIC, x=model_index, color=model))
g_SM + labs(title="BIC of various model configurations - Silo Mais", x="") +
#    geom_jitter(size=10) +  # scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  geom_point(size=1, alpha=1) +
  geom_boxplot(size=1) +
  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") +
  theme_few() +
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y = element_text(angle=0, vjust=1, size=20),
        strip.text.x = element_text(size = 40), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 40),
        
             legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  guides(fill=guide_legend(title=NULL)) +
  facet_wrap( ~ month) +
  geom_vline(xintercept = 6.5,  colour="black", linetype = "longdash", size=0.4)  +
  geom_vline(xintercept = 2.5,  colour="grey", linetype = "longdash", size=0.3) +
  geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.3)  

    
########################################
## Total - Copmarision no 2003 - 2003 ##
BIC_SM_gp_no2003 <- as.data.frame(rbind(BIC_SM_5, BIC_SM_5_no2003, 
                                 BIC_SM_6, BIC_SM_6_no2003, 
                                 BIC_SM_7, BIC_SM_7_no2003, 
                                 BIC_SM_8, BIC_SM_8_no2003,
                                 BIC_SM_9, BIC_SM_9_no2003,
                                 BIC_SM_10, BIC_SM_10_no2003))
levels(BIC_SM_gp$month)[levels(BIC_SM_gp$month)=="Mai"] <- "May"
# ## Subframe für SMI
# BIC_SMI_no2003 <- as.data.frame(rbind(BIC_SM_5[28,], BIC_SM_5_no2003[28,],
#                                BIC_SM_6[28,], 
#                                BIC_SM_7[28,], 
#                                BIC_SM_8[28,], 
#                                BIC_SM_9[28,],
#                                BIC_SM_10[28,]))
# 
# ## Mark minimum Point of each month
# BIC_SM_mintot <- as.data.frame(rbind(BIC_SM_5_mintot,
#                                      BIC_SM_6_mintot, 
#                                      BIC_SM_7_mintot, 
#                                      BIC_SM_8_mintot, 
#                                      BIC_SM_9_mintot, 
#                                      BIC_SM_10_mintot))

## Growing Period, no Super, modelindex
g_SM_no2003 <- ggplot(BIC_SM_gp_no2003,aes(y=BIC, x=model_index, color=model))
g_SM_no2003 + labs(title="BIC of various model configurations - Silo Mais", x="") +
  #    geom_jitter(size=10) +  # scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  geom_point(size=1, alpha=1) +
  geom_boxplot(size=1) +
  scale_colour_brewer(type = "qual", palette = "Paired", direction = "1") +
  theme_few() +
  theme(plot.title=element_blank(), 
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y = element_text(angle=0, vjust=1, size=20),
        strip.text.x = element_text(size = 40), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 40),
        legend.position="bottom") + 
  scale_x_discrete(breaks=NULL) +
  guides(fill=guide_legend(title=NULL)) +
  facet_wrap( ~ month) +
  geom_vline(xintercept = 6.5,  colour="black", linetype = "longdash", size=0.4)  +
  geom_vline(xintercept = 2.5,  colour="grey", linetype = "longdash", size=0.3) +
  geom_vline(xintercept = 9.5,  colour="grey", linetype = "longdash", size=0.3)  





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
g_SM + labs(title="BIC of various model configurations - Silo Mais", x="") +
  #    geom_jitter(size=10) +  # scale_colour_brewer(type = "qual", palette = "Paired", direction = "-1") + 
  geom_point(size=1, alpha=1) +
  geom_violin(size=2.5) +
  scale_colour_manual(values = colors(20)) +
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
 geom_vline(xintercept = 6,  colour=colors20[6], linetype = "longdash", size=0.4)  +
  geom_point(data=BIC_SMI_JulAugSup, colour=colors20[6], size=4) +
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
