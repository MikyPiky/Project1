# cluster bootstrap with paralell processing
rm(list=ls())

# packages for cluster standard errors
library(lmtest)
library(lfe)
# use multicore functions
library(snow)

# set up simulation
n <- 10000 # number of observations
x <- rnorm(n)
y <- 5 + 2*x + rnorm(n, 0, 40)
# regression
m1 <- lm(y ~ x)
summary(m1)
# standard error is 0.4

# duplicate data
dpt <- 20 # dpt times
dat <- data.frame(x = rep(x, dpt) , y = rep(y, dpt), g = rep(1:n, dpt))

# regressions with no clustering
m2 <- lm(y ~ x, data = dat) # smaller StErrs
summary(m2)
# standard errors are like m1 = 0.09

# now cluster

reg_felm<-felm(y ~ x | g | 0 | g, data = dat)
summary(reg_felm)
coefficients(reg_felm)
vcv(reg_felm)
se <- function(object) tail(sqrt(diag(object)), 1)
se(vcov(reg_felm))
reg_felm$cse
reg_felm$coefficients[2]

# standard errors are like m1 = 0.4

# lets do this with a regular cluster bootstap
reps <- 5 # 50 reps in practice do more
clusters <- unique(dat$g)
boot.res1 <- matrix(NA, nrow = reps, ncol = 1)
reg_felm <- NULL

coef(lm(y ~ x, data = df.bs))[2]
summary(lm(y ~ x, data = df.bs))

# open time stamp
t1 <- Sys.time()
# set the seed 
set.seed(12345)
# do in loop
for(i in 1:reps){
  # sample the clusters with replacement
  units <- sample(clusters, size = length(clusters), replace=T)
  # create bootstap sample with sapply
  df.bs <- sapply(units, function(x) which(dat[,"g"]==x))
  df.bs <- dat[unlist(df.bs),]
  reg_felm <-felm(y ~ x | g | 0 | g, data = df.bs)
  boot.res1[i] <- coefficients(reg_felm)
}
# close time stamp
t2 <- Sys.time()
t2 - t1 

sd(boot.res1) # good bootstrap standard errors are = 0.4

# now lets speed up the sapply function from the previous example

boot.res2 <- matrix(NA, nrow = reps, ncol = 1)

# set the seed 
set.seed(12345)

cl <- makeCluster(10)
# open time stamp
t3 <- Sys.time()
# do in loop
for(i in 1:reps){
  # sample the clusters with replacement
  units <- sample(clusters, size = length(clusters), replace = T)
  # now use the 10 cores instead of 1!
  clusterExport(cl, c("dat", "units"))
  # cluster apply instead of regular apply
  df.bs = clusterApply(cl, units, function(x) which(dat$g == x))
  df.bs <- dat[unlist(df.bs),]
  boot.res2[i] <- coef(lm(y ~ x, data = df.bs))[2]
}
# close time stamp
t4 <- Sys.time()
t4 - t3 

stopCluster(cl)

sd(boot.res2) # good bootstrap standard errors are = 0.4