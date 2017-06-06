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
# 
# zinb <- read.csv("http://www.ats.ucla.edu/stat/data/fish.csv")
# zinb <- within(zinb, {

#   nofish <- factor(nofish)
#   livebait <- factor(livebait)
#   camper <- factor(camper)
# })

zinb <- temp2

#-----------------------------------------------------------------------------
# Calculate zero-inflated regression
#-----------------------------------------------------------------------------

m <- lm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = zinb)

coef(m) # gibt nur coefficienten aus
coef(summary(m)) # gibt die ganze Statistic aus


## beide liefern das gleiche
as.vector(unname(t(coef(summary(m))[,1:2]))) 
as.vector(t(do.call(rbind, list(coef(summary(m))))[, 1:2]))

length(as.vector(t(do.call(rbind, list(coef(summary(m))))[, 1:2])))

#-----------------------------------------------------------------------------
# Store the original regression coefficients
#-----------------------------------------------------------------------------

original.estimates <- as.vector(t(do.call(rbind, list(coef(summary(m))))[, 1:2]))

#-----------------------------------------------------------------------------
# Set the number of replications
#-----------------------------------------------------------------------------

n.sim <- 5

#-----------------------------------------------------------------------------
# Set up a matrix to store the results
#-----------------------------------------------------------------------------

store.matrix <- matrix(NA, nrow=n.sim, ncol=738)

#-----------------------------------------------------------------------------
# The loop
#-----------------------------------------------------------------------------

set.seed(123)

for(i in 1:n.sim) {
  
  #-----------------------------------------------------------------------------
  # Draw the observations WITH replacement
  #-----------------------------------------------------------------------------
  
  data.new <- zinb[sample(1:dim(zinb)[1], dim(zinb)[1], replace=TRUE),]
  print(head(data.new))
  #-----------------------------------------------------------------------------
  # Calculate the model with this "new" data
  #-----------------------------------------------------------------------------
 
  
  m <-glm(formula = formula_PrecTavgSmiGDM_2v3poly,  data = data.new)
#   summary(m)
#   print( as.vector(t(do.call(rbind, list(coef(summary(m))))[, 1:2])))
  length( as.vector(t(do.call(rbind, list(coef(summary(m))))[, 1:2])))
  #-----------------------------------------------------------------------------
  # Store the results
  #-----------------------------------------------------------------------------
  
  store.matrix[i, ] <- as.vector(t(do.call(rbind, list(coef(summary(m))))[, 1:2]))
  length(as.vector(t(do.call(rbind, list(coef(summary(m))))[, 1:2])))
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


########################################################################




bootstrap <- function(df, m) {
  n <- nrow(df)
  
  attr(df, "indices") <- replicate(m, sample(n, replace = TRUE) - 1, 
                                   simplify = FALSE)
  attr(df, "drop") <- TRUE
  attr(df, "group_sizes") <- rep(n, m)
  attr(df, "biggest_group_size") <- n
  attr(df, "labels") <- data.frame(replicate = 1:m)
  attr(df, "vars") <- list(quote(replicate))
  class(df) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  
  df
}
library(dplyr)
library(broom)

nrow(temp)

set.seed(2014)
bootnls <- temp2 %>% bootstrap(100) %>%
  do(tidy( plm(formula = update(formula_PrecTavgSmiGDM_2v3poly, .~. - dummy(comId)),  data = temp2,  effect="individual", model=("within"), index = c("comId","year"))  ))

class(bootnls)
labels(bootnls)
bootnls$std.error

bootnls[1:100,]

alpha = .05
bootnls %>% group_by(term) %>% summarize(low=quantile(estimate, alpha / 2),
                                         high=quantile(estimate, 1 - alpha / 2))

library(ggplot2)
ggplot(bootnls, aes(std.error)) + geom_histogram(binwidth=0.00001) + facet_wrap(~ term, scales="free")
