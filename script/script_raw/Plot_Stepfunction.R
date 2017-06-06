library("ggplot2")

x <- c(0, 0.1, 0.2, 0.3, 0.7, 0.8, 0.9, 1 )
y <-  c(-0.5, -.3, -.2, 0.0, 0.2, 0.3, .5) 

plot(x,y,type="n") 
segments(x[-length(x)],y[-length(x)],x[-1],y[-length(x)]) 
points(x[-length(x)],y[-length(x)],pch=16) 
points(x[-1],y[-length(x)],pch=1) 

dta <- data.frame( x= x[-length(x)],y=y[-length(x)], xend=x[-1], yend=y[-length(x)] ) 
ggplot( dta, aes( x=x, y=y, xend=xend, yend=yend )) + 
  geom_segment(size=4 ) +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        strip.background = element_rect(fill = "white"),  
        axis.title=element_text(size=20 ,face="bold")) +

#   + ggtitle( "Boxplot der monatlichen Durschschnitts-SMI pro Jahr mit Jitter der Daten"))
   ylab ("Estimated Coefficients")  + 
  xlab("SMI") #
#   geom_point( shape=16, size=4 ) + 
#   geom_point( aes( x=xend, y=yend ), shape=1, size=4 ) 


x <- 1:100

dat <- data.frame(x,y+5)
f <- function(x) exp(x^3+x^2+x+5


p <- ggplot(dat, aes(x,log(y))) 
p + geom_point()
p  
p + stat_function(fun=f, colour="red")
