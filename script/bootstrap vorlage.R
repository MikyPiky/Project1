#-----------------------------------------------------------------------------
# Load packages
#-----------------------------------------------------------------------------

require(ggplot2)
require(pscl)
require(MASS)
require(boot)

#-----------------------------------------------------------------------------
# Load data
#-----------------------------------------------------------------------------

zinb <- read.csv("http://www.ats.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

#-----------------------------------------------------------------------------
# Calculate zero-inflated regression
#-----------------------------------------------------------------------------

m1 <- zeroinfl(count ~ child + camper | persons, data = zinb,
               dist = "negbin", EM = TRUE)
coef(m1)
coef(summary(m1))

#-----------------------------------------------------------------------------
# Store the original regression coefficients
#-----------------------------------------------------------------------------

original.estimates <- as.vector(t(do.call(rbind, coef(summary(m1)))[, 1:2]))

#-----------------------------------------------------------------------------
# Set the number of replications
#-----------------------------------------------------------------------------

n.sim <- 2000

#-----------------------------------------------------------------------------
# Set up a matrix to store the results
#-----------------------------------------------------------------------------

store.matrix <- matrix(NA, nrow=n.sim, ncol=12)

#-----------------------------------------------------------------------------
# The loop
#-----------------------------------------------------------------------------

set.seed(123)

for(i in 1:n.sim) {
  
  #-----------------------------------------------------------------------------
  # Draw the observations WITH replacement
  #-----------------------------------------------------------------------------
  
  data.new <- zinb[sample(1:dim(zinb)[1], dim(zinb)[1], replace=TRUE),]
  
  #-----------------------------------------------------------------------------
  # Calculate the model with this "new" data
  #-----------------------------------------------------------------------------
  
  m <- zeroinfl(count ~ child + camper | persons,
                data = data.new, dist = "negbin",
                start = list(count = c(1.3711, -1.5152, 0.879),
                             zero = c(1.6028, -1.6663)))
  
  #-----------------------------------------------------------------------------
  # Store the results
  #-----------------------------------------------------------------------------
  
  store.matrix[i, ] <- as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
  
}


#-----------------------------------------------------------------------------
# Save the means, medians and SDs of the bootstrapped statistics
#-----------------------------------------------------------------------------

boot.means <- colMeans(store.matrix, na.rm=T)

boot.medians <- apply(store.matrix,2,median, na.rm=T)

boot.sds <- apply(store.matrix,2,sd, na.rm=T)

#-----------------------------------------------------------------------------
# The bootstrap bias is the difference between the mean bootstrap estimates
# and the original estimates
#-----------------------------------------------------------------------------

boot.bias <- colMeans(store.matrix, na.rm=T) - original.estimates

#-----------------------------------------------------------------------------
# Basic bootstrap CIs based on the empirical quantiles
#-----------------------------------------------------------------------------

conf.mat <- matrix(apply(store.matrix, 2 ,quantile, c(0.025, 0.975), na.rm=T),
                   ncol=2, byrow=TRUE)
colnames(conf.mat) <- c("95%-CI Lower", "95%-CI Upper")