#
# PLOT CONFIDENCE INTERVAL OF AN LM MODEL USING
# A SPECIFIED VARIANCE-COVARIANCE MATRIX
#
# Note:
# - This calculates the confidence interval of the mean from a fitted linear regression
#   when the estimated SE of coefficients is adjusted after model fitting.
# - The confidence interval is for the fitted mean; not to be confused with
#   prediction tolerance / prediction interval for individual observations.
#
# Warnings:
# - assumes normally distributed residuals *
# - it was only tested on lm() models and a specific plm() model with linear, categorical, and poly(..., raw=TRUE) terms
# - it does NOT produce proper confidence intervals with poly(..., raw=FALSE) terms
#
# Please send suggestions / report issues to Mick Wu (mick.wu@ufz.de)
# Updated: 2016-12-21




# Function to generate confidence intervals of the mean 
# using a specific variance-covariance matrix
predictvcv <- function(mod, vcv, newdata, level=0.95){
  # mod: regression model from which to extract coefficient estimates
  # vcv: specified variance-covariance matrix for coefficients.
  #    dimensions must match the number of terms in the model
  # newdata: optional dataframe to predict new observations.
  #          * It must also contain the response variable but the value does not matter
  # level: level of confidence interval (default=95%
  #
  # returns a data frame with 3 variables: fitted mean, lower CI, upper CI

  # Coefficient estimates
  if (missing(mod)) stop("The fitted regression model 'mod' is missing")
  beta.hat <- coef(mod)
  # Variance covariance matrix
  if (missing(vcv)){
    warning("The covariance matrix 'vcv' is missing. It will be taken from 'the model'mod'")
    vcv <- vcov(mod)
  }
  if (any(length(beta.hat) != dim(vcv))) stop("'vcv' must be a square matrix with dimensions matching the number of terms in the regression model")
  # Model matrix
  if (missing(newdata)){
    Xp <- model.matrix(mod)
  } else {
    f <- formula(mod)
    Xp <- model.matrix(f,data=newdata)
  }
  # Predicted mean
  pred <- as.numeric(Xp %*% beta.hat)
  # SE (SD of mean)
  se <- sqrt(unname(rowSums((Xp %*% vcv) * Xp)))
  # quantile using the t-distribution
  quant <- abs(qt((1-level)/2, df=mod$df.residual))
  # output
  return(data.frame(fit=pred, lwr=pred-(quant*se), upr=pred+(quant*se)) ) 
}

head(Xp)
tail(Xp)
dim(Xp)
length(pred)
length(beta.hat)
length(Xp)
#____________
#   EXAMPLE

#
# Simulate data
#
x1 <- rnorm(10, mean=0, sd=150) # continuous predictor
x2 <- rep(0:1,each=5) # categorical predictor
y <- 0.01*(x1^2) + 0.00005*(x1^3) + 20*x2 + rnorm(10, mean=0, sd=500) # response variable with dependencies
# data frame
dat <- data.frame(y,x1,x2)
# new data frame for plotting a smooth confidence interval
res = 10 # resolution for plotting
newdat <- data.frame(y=0, x1 = rep(seq(min(x1),max(x1), # y is needed but values do not matter
                     length.out=res),2), x2 = rep(0:1,each=res))
rm(y,x1,x2,res) # clean up
# plot the data
plot(y~x1, col=hsv(h=(x2+0.2)/2, alpha=0.2), data=dat)

#
# Fit a linear regression with polynomials
#
mod <- lm(y ~ poly(x1,degree=3,raw = TRUE) + x2, data=dat)
summary(mod)

#
# Plot different confidence intervals for comparison
#

# Plot the data with confidence intervals of the mean
# using the normal predict function and fitted vcv
plot(y~x1, col=hsv(h=0.1+x2/2, alpha=0.3), data=dat)
prednew <- predict(mod, newdata=newdat, interval="confidence")
index <- newdat$x2==0
polygon(c(rev(newdat$x1[index]),newdat$x1[index]), c(rev(prednew[index,"lwr"]),prednew[index,"upr"]),
        border=hsv(h=0.1+newdat$x2[index]/2, v=0.9),
        col=hsv(h=0.1+newdat$x2[index]/2, alpha=0.3))
index <- newdat$x2==1
polygon(c(rev(newdat$x1[index]),newdat$x1[index]), c(rev(prednew[index,"lwr"]),prednew[index,"upr"]),
        border=hsv(h=0.1+newdat$x2[index]/2, v=0.9),
        col=hsv(h=0.1+newdat$x2[index]/2, alpha=0.3))

# Plot the data with confidence intervals of the mean
# using the predictvcv function
# model with unchanged vcv (should be the same as predict function)
predvcvnew <- predictvcv(mod, newdata=newdat) 
plot(y~x1, col=hsv(h=0.1+x2/2, alpha=0.3), data=dat)
index <- newdat$x2==0
polygon(c(rev(newdat$x1[index]),newdat$x1[index]), c(rev(predvcvnew[index,"lwr"]),predvcvnew[index,"upr"]),
        border=hsv(h=0.1+newdat$x2[index]/2, v=0.9),
        col=hsv(h=0.1+newdat$x2[index]/2, alpha=0.3))
index <- newdat$x2==1
polygon(c(rev(newdat$x1[index]),newdat$x1[index]), c(rev(predvcvnew[index,"lwr"]),predvcvnew[index,"upr"]),
        border=hsv(h=0.1+newdat$x2[index]/2, v=0.9),
        col=hsv(h=0.1+newdat$x2[index]/2, alpha=0.3))

# model with an altered vcv
vcv <- vcov(mod)
diag(vcv) <- diag(vcv)*2
predvcvnew <- predictvcv(mod, newdata=newdat, vcv=vcv) 
plot(y~x1, col=hsv(h=0.1+x2/2, alpha=0.3), data=dat)
index <- newdat$x2==0
polygon(c(rev(newdat$x1[index]),newdat$x1[index]), c(rev(predvcvnew[index,"lwr"]),predvcvnew[index,"upr"]),
        border=hsv(h=0.1+newdat$x2[index]/2, v=0.9),
        col=hsv(h=0.1+newdat$x2[index]/2, alpha=0.3))
index <- newdat$x2==1
polygon(c(rev(newdat$x1[index]),newdat$x1[index]), c(rev(predvcvnew[index,"lwr"]),predvcvnew[index,"upr"]),
        border=hsv(h=0.1+newdat$x2[index]/2, v=0.9),
        col=hsv(h=0.1+newdat$x2[index]/2, alpha=0.3))

