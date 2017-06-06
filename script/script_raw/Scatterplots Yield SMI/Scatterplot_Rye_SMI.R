### File Description ### 
# Produce Scatterplots for relationship per acre output of yield and SMI for the years 1999 - 2010
# Here: rye

## Input and Dependencies
# data/data_processed/Yield_SMI_long <- MergeSMI_Yield

## Output
# Scatterplots rye SMI

## Packages
library("ggplot2")

#####################################################################################################################################################
#####################################################################################################################################################

#### Load large Yield_SMI_long Dataset #### 
Yield_SMI_long<-read.csv("data/data_processed/Yield_SMI_long")

## Explore that data set
names(Yield_SMI_long)
head(Yield_SMI_long)
unique(Yield_SMI_long$year)

####  For plotting purpose only use years with yield data, i.e. 1999 -  2010 ####

## Delete years 1995 - 1998
Yield_SMI_long_1999<-Yield_SMI_long[-(1:1640),]

## Explore these Data
head(Yield_SMI_long_1999)
unique(Yield_SMI_long_1999$year)
names(Yield_SMI_long_1999)


#### Plot SMI for various months with Winterwhea yield for this year
## Linear line
qp_Jan_lin<-qplot(SMI_Jan, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("Rye, Linear, Jan")
qp_Feb_lin<-qplot(SMI_Feb, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("Rye, Linear, Feb")
qp_Mar_lin<-qplot(SMI_Mar, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("Rye, Linear, Mar")
qp_Apr_lin<-qplot(SMI_Apr, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("Rye, Linear, Apr")
qp_Mai_lin<-qplot(SMI_Mai, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("Rye, Linear, Mai")
qp_Jun_lin<-qplot(SMI_Jun, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("Rye, Linear, Jun")
qp_Jul_lin<-qplot(SMI_Jul, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("Rye, Linear, Jul")
qp_Aug_lin<-qplot(SMI_Aug, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm", shape=I(1),se=FALSE)+ ggtitle("Rye, Linear, Aug")
qp_Sep_lin<-qplot(SMI_Sep, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("Rye, Linear, Sep")
qp_Oct_lin<-qplot(SMI_Oct, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("Rye, Linear, Oct")
qp_Nov_lin<-qplot(SMI_Nov, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("Rye, Linear, Nov")
qp_Dec_lin<-qplot(SMI_Dec, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("Rye, Linear, Dec")

## Polygons
qp_Jan_Pol<-qplot(SMI_Jan, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Jan")
qp_Feb_Pol<-qplot(SMI_Feb, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Feb")
qp_Mar_Pol<-qplot(SMI_Mar, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Mar")
qp_Apr_Pol<-qplot(SMI_Apr, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Apr")
qp_Mai_Pol<-qplot(SMI_Mai, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Mai")
qp_Jun_Pol<-qplot(SMI_Jun, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Jun")
qp_Jul_Pol<-qplot(SMI_Jul, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Jul")
qp_Aug_Pol<-qplot(SMI_Aug, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Aug")
qp_Sep_Pol<-qplot(SMI_Sep, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Sep")
qp_Oct_Pol<-qplot(SMI_Oct, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Oct")
qp_Nov_Pol<-qplot(SMI_Nov, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Nov")
qp_Dec_Pol<-qplot(SMI_Dec, rye, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("Rye, Polygon, Dec")
