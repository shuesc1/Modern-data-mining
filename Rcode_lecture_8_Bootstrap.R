
######## Bootstrap  and its applications #####
# 5.2: Bootstrap


### Part I: motivation example via Weibull distribution


### Part II: Bootstrap in action 
###          Apply the technique in linear models
###          (package(boot))

### Load package boot

if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(boot)  # package boot is used. 


### Part I: A motivating example using Weibull distribution

# In real life we only have one data sample. We have nice statistics that
# can be used to estimate some unknown parameters. But we may not have standard errors handy
# for the estimates. 

# Solution: use bootstrap sample to replicate the statistic. 
# The key: bootstrap sample resembles the sample cdf. 


####  Weibull distribution: life time to an event: 
#     Ex: how long a product survives; how long a patient survives...

# 1) Get familiar with Weibull variables

# Weibull densities are defined as:

#   f(x, lambda, k)= (k/lambda) (x/lambda)^(k-1) exp(-(x/lambda)^k)
#   x>0, k>0 (controls the shape of the distribution) and lambda>0
#   k=1  => exponential disn
# Two parameters:  shape=k and scale=lambda
# Shape parameter controls the degree of skewness. 

# Take a look at Weibull shape=.5 and lambda=1
X <- rweibull(20000, shape=.5)  # very skewed to right
hist(X, breaks=100, col="blue") # Note: the sample number n=20000 is large so that we
   # get a good look at the Weibull densities.

# Four Weibull disn when shape parameters go form .5 to 2
par(mfrow=c(4, 1))
shape <- c(.5, 1, 1.5, 2)
for (i in 1:4) 
  {hist(rweibull(20000, shape=shape[i]),
        xlab = "",
        breaks = 100, 
        col = i,
        main = paste("Weibull dis with shape=", shape[i]))}
par(mfrow=c(1,1))
#####

# 2) Bootstrap Magic

# Let's study X follows Weibull(shape=1.5 and scale=1)

# i) A random sample X with n=1000 obs are available to us
  
  par(mfrow=c(3,1))
  set.seed(1)
  X <- rweibull(1000, shape=1.5)
  hist(X, breaks=100, col="blue", main="The original sample")

# ii) Now a bootstrap sample: randomly take 1000 obs'n from X with replacement.
  index <- sample(1000, 1000, replace=TRUE)
  Xboot <- X[index]
  hist(Xboot, breaks=100, col="red", main="A bootstrap sample")
  par(mfrow=c(1,1))
  
# Can you tell the difference between the two histograms??? Nope

# Implication: . We can get another sample from the population for free!
#              . We can then produce any statistics we want for free.
  
# 3) We now going to estimate theta=E(X^3) (the true third moment is 2) using the sample we have. This relates to Kurtosis. 
  # Step 1: Use sample mean 
    theta.hat <- mean(X^3)
    theta.hat  
    # but we don't have standard error for theta.hat
    
  # Step 2: Generate many bootstrap sample and obtain replicates of theta.hat. Here we produce 100
    # Bootstrap estimates.
    
    N.B <- 200   # number of bootstrap samples: needs to be large to have a stable hist. below
    theta.hat.b <- 0
    n=length(X)
    for (i in 1:N.B)
      {
      index <- sample(n, n, replace=TRUE)
      Xboot <- X[index]   # ith Bootstrap sample of size n
      theta.hat.b[i] <- mean(Xboot^3) # Bootstrap estimates
      }
    hist(theta.hat.b, breaks=20, col="red", 
         main = "bootstrap estimate of x^3")  
  #  What have you observed?
    
    #FACTS: Bootstrap estimates theta.hat.b follows a Normal dis'n (Coincident? No.) with center to be 
    # the original estimate theta.hat=mean(X^3)
    
    
  # Step 3: Construct bootstrap CI of theta=E(X^3), say 95% CI's.
    
    #Optional I: use normal CI's 
          CI.normal <- mean(X^3) + 2* sd(theta.hat.b) *c(-1, 1)
          CI.normal
    
    # Optional II: Rank theta.hat.b and find the lower .025 and upper .025 percentiles
          CI.quantil <- quantile(theta.hat.b, c(0.025, 0.975)); 
          CI.quantil
 
    # The two confidence intervals are very similar.

### Exercise for you:Construct a 95% CI for the median of X.
  # In this case the Bootstrap Percentil ci will work better since
  # the sample median is not exactly unbiased.
          
### Summary about bootstrap magic
        
    ## When it works:
        # 1) The original sample is an iid sample with large n
        # 2) Bootstrap sample will be a good approx. to a random sample
        # 3) If we are interested in estimating some unknown parameter say:
            # E(X^2), Median(X), Cor(X, Y), beta.i in linear models. 
            # As long as the estimators we use are unbiased, the bootstrap CI is valid!
          
    ## When it doesn't work
        # If the pivotal estimate is not a good estimate of the unknown parameters  
        # The lasso estimates are biased so Bootstrap CI's wont' work.
          
          
          
  
          
### Part II: Bootstrap in action          

### When the linear model shows strong heteroscedasticity, all the inference made 
### based on the model assumptions may not be trust worthy. We may try to fix it by
### transformations on y etc. But we could also do Bootstrap!!!!         
    
## Use car data to produce CI's for some linear coefficients, R^2, etc.
          
# 1) Get the data
          
# Set working directory
# rm(list=ls()) # Remove all the existing variables
# dir <- c("/Users/lzhao/Dropbox/STAT471/Data")   # my laptop
# setwd(dir)
          
          
data1 <- read.csv("car_04_regular.csv")
str(data1)
data1$GPM=1/data1$MPG_City  # transform y's
names(data1)
data2=data1[, -c(1, 3,4)]

# 2) Bootstrap CI's for coefficients

# The following example shows how to produce ci's for linear coef's and r-square
library(boot)

fit <- lm(GPM ~ Horsepower+Weight+Length, data2) # let's consider the linear model with three x's
confint(fit)
fit$coef

# Method I: Bootstrap residuals

## Step I: Use original OLS
## Step II: Bootstrap the residuals
## Step III: Update y
## Step IV: Get new OLS estimates and store them
## Step V: Repeat from Step II
## Step VI: Construct bootstrap CI

## Step I: original OLS
fit <- lm(GPM ~ Horsepower+Weight+Length, data2)

N <- nrow(data2)
Nboot <- 1000
b.estimates <- matrix(NA, ncol=length(fit$coefficients), nrow=Nboot) # output of OLS estimates
colnames(b.estimates) <- names(b.estimates)

for(iboot in 1:Nboot) {
  ## Step II: Bootstrap the residuals
  index <- sample(N, replace=T)
  b.residuals <- fit$residuals[index]
  
  ## Step III: Update y
  b.y <- predict(fit) + b.residuals
  b.data <- data2
  b.data$GPM <- b.y
  
  ## Step IV: Get new OLS estimates and store them
  b.fit <- lm(GPM ~ Horsepower+Weight+Length, data=b.data)
  b.estimates[iboot,] <- b.fit$coefficients
  if(iboot%%200==0) print(iboot)
}

## Step VI: Construct bootstrap CI
b.se <- apply(b.estimates, 2, sd)
b.CI <- data.frame(cbind(fit$coefficients - 2*b.se, fit$coefficients + 2*b.se))
colnames(b.CI) <- c("lbound (resid. bs)", "ubound (resid. bs)")
b.CI

## Compare with the original CI
confint(fit)


# Method II: Pairwise bootstrap

# a quick model diagnoses support the t-intervals and the t/F tests. 
# Nevertheless, we can produce bootstrap ci's and compare the two sets.
plot(fit, 1) # residual plot of the fit
plot(fit, 2) # qq normal of the residuals


# first define the function to produce LS est's of beta's
coef <- function(formula, data, indices) {
  data.temp <- data[indices,]
  fit <- lm(formula, data=data.temp)
  return(fit$coef)
}

# Take 100 bootstrap samples and produces b-confidence intervals for each coef's
est.boot <- boot(data=data2, statistic=coef, formula=GPM~Horsepower+Weight+Length, R=100)

# view est.boot
est.boot
plot(est.boot, index=2) # beta HP
plot(est.boot, index=3) # beta Wt
plot(est.boot, index=4) # beta Length


boot.ci(est.boot, type="perc", index=2)

### Get some details
est.b.coef <- data.frame(est.boot$t)
names(est.b.coef) <- c("\beta0", "\beta_HP", "\beta_Wt", "\beta_Length") 
fit$coefficients
est.boot$t

## pairwise bootstrap CI
b.se.2 <- apply(est.b.coef, 2, sd)
b.CI.2 <- data.frame(cbind(fit$coefficients - 2*b.se.2, fit$coefficients + 2*b.se.2))
colnames(b.CI.2) <- c("lbound (pair bs)", "ubound (pair bs)")
b.CI.2

## Compare with residual bootstrap CI and the original CI
cbind(b.CI, b.CI.2, confint(fit))


# 3) Bootstrap CI's for R^2

# function to obtain R-Square from the data 
rsq <- function(formula, data, indices) {
  data.temp <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=data.temp)
  return(summary(fit)$r.square) # return the r.square
} 

# bootstrapping with R replications 
b.rsq <- boot(data=data2, statistic=rsq, 
              R=100, formula=GPM~Horsepower+Weight+Length)

b.rsq
plot(b.rsq)
hist(b.rsq$t, breaks=20)
boot.ci(b.rsq, type="perc")
boot.ci(b.rsq, type="norm")

