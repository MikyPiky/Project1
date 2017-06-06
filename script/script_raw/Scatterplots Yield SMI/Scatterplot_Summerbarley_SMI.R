### File Description ### 
# Produce Scatterplots for relationship per acre output of yield and SMI for the years 1999 - 2010
# Here: summerBarley

## Input and Dependencies
# data/data_processed/Yield_SMI_long <- MergeSMI_Yield

## Output
# Scatterplots summerBarley SMI

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
qp_Jan_lin<-qplot(SMI_Jan, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("summerBarley, Linear, Jan")
qp_Feb_lin<-qplot(SMI_Feb, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("summerBarley, Linear, Feb")
qp_Mar_lin<-qplot(SMI_Mar, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("summerBarley, Linear, Mar")
qp_Apr_lin<-qplot(SMI_Apr, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("summerBarley, Linear, Apr")
qp_Mai_lin<-qplot(SMI_Mai, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("summerBarley, Linear, Mai")
qp_Jun_lin<-qplot(SMI_Jun, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("summerBarley, Linear, Jun")
qp_Jul_lin<-qplot(SMI_Jul, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("summerBarley, Linear, Jul")
qp_Aug_lin<-qplot(SMI_Aug, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm", shape=I(1),se=FALSE)+ ggtitle("summerBarley, Linear, Aug")
qp_Sep_lin<-qplot(SMI_Sep, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("summerBarley, Linear, Sep")
qp_Oct_lin<-qplot(SMI_Oct, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("summerBarley, Linear, Oct")
qp_Nov_lin<-qplot(SMI_Nov, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("summerBarley, Linear, Nov")
qp_Dec_lin<-qplot(SMI_Dec, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"), method="lm",shape=I(1), se=FALSE)+ ggtitle("summerBarley, Linear, Dec")

## Polygons
qp_Jan_Pol<-qplot(SMI_Jan, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Jan")
qp_Feb_Pol<-qplot(SMI_Feb, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Feb")
qp_Mar_Pol<-qplot(SMI_Mar, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Mar")
qp_Apr_Pol<-qplot(SMI_Apr, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Apr")
qp_Mai_Pol<-qplot(SMI_Mai, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Mai")
qp_Jun_Pol<-qplot(SMI_Jun, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Jun")
qp_Jul_Pol<-qplot(SMI_Jul, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Jul")
qp_Aug_Pol<-qplot(SMI_Aug, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Aug")
qp_Sep_Pol<-qplot(SMI_Sep, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Sep")
qp_Oct_Pol<-qplot(SMI_Oct, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Oct")
qp_Nov_Pol<-qplot(SMI_Nov, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Nov")
qp_Dec_Pol<-qplot(SMI_Dec, summerBarley, data=Yield_SMI_long_1999, color=factor(year), geom=c("point", "smooth"),shape=I(1), se=FALSE)+ ggtitle("summerBarley, Polygon, Dec")
