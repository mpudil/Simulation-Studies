



#####################################################################################
# In the previous function, we only allowed for one iteration every time we increased
# the variance of xstar by 0.1. Let's now see what happens when we take the mean of 
# several (100) iterations per increment. This should provide us with a better a
# pproximation, which will then be determined exactly though theory.
#####################################################################################



ivsim <- function(nReps) {
  var_xstar <- 0.5   # initialize the variance of x* to t
  olsbias <- NULL
  ivbias <- NULL
  differ <- NULL
  variances <- NULL
  corrxc <- NULL
  corrxz <- NULL
  
  for(i in 1:nReps){
    olsbiasm <- NULL
    ivbiasm <- NULL
    differm <- NULL
    variancesm <- NULL
    corrxcm <- NULL
    corrxzm <- NULL
    for(j in 1:100) {
      xStarAndC <- mvrnorm(1000, c(20, 15), matrix(c(var_xstar, 0.5, 0.5, 4), 2, 2))  # Multivariate r norm
      xStar <- xStarAndC[,1]
      c <- xStarAndC[,2]
      z <- rnorm(1000)  # Mean 0 and sd 1 by default
      x <- xStar + z
      
      # using 1 makes it easy to estimate how 'wrong' an estimator is and toss
      # some noise on y
      y <- 1 + x + c + rnorm(1000, 0, 0.5)  # Note that all beta parameters are 1 here
      
      olsbiasm[j] <- summary(lm(y~x))$coefficients[2] -1
      ivbiasm[j] <- summary(ivreg(y~x|z))$coefficients[2] -1
      differm[j] <- olsbiasm[j] - ivbiasm[j]
      corrxcm[j] <- cor(x,c)
      corrxzm[j] <- cor(x,z)
      # By design, cor(z,c) will be about 0, so all we really need to determine if the IV is weak is the correlation between x and z
      
    }
    
    
    
    olsbias[i] <- mean(olsbiasm)
    ivbias[i] <- mean(ivbiasm)
    differ[i] <- mean(differm)
    variances[i] <- var_xstar
    corrxc[i] <- mean(corrxcm)
    corrxz[i] <- mean(corrxzm)
    
    var_xstar <- var_xstar + 0.1  # Increment var_xstar by 0.1
    
  }
  cbind(olsbias, ivbias, differ, variances, corrxc, corrxz)
}

bias <- ivsim(300)


##############################################################################################################################
# Plot of Correlation of X and Z vs Bias of IV and OLS (effect of weak instruments on the bias/bias difference of IV and OLS)
plot(bias[,6], bias[,1], ylim=c(-0.1,0.3), xlab="Correlation Between X and Z", ylab="Bias", col="steel blue", pch=20) 
points(bias[,6], bias[,2], xlab="Correlation Between X and Z", ylab="IV Bias", col="tomato")
legend(.2,.25, legend=c('OLS Bias', 'IV Bias'), col=c('steel blue', 'tomato'), lty=3:3, cex=0.8, lwd=3)
###############################################################################################################################
# Comments:
# With the current setup (all else held the same), as we increase the correlation between x and z, the bias of the IV estimator
# continues to stay at 0 and the consistency improves. 
# The OLS estimate, however, becomes more biased but continues to be consistent.
# The IV estimator and the OLS estimator tend to be about equal when the correlation is around 0.25, and OLS ends us being a better
# estimator when the correlation between X and Z decreases to below 0.25 or 0.2. 



################################################################################################################
# Other, less important graphs for building intuition:

# Plot of Variance of X* vs Bias of IV and OLS
plot(bias[,4], bias[,1], ylim=c(-0.1,0.3), xlab="Variance of X*", ylab="Bias", col="steel blue", pch=20) 
points(bias[,4], bias[,2], xlab="Variance of X*", ylab="IV Bias", col="tomato")
legend(25,.25, legend=c('OLS Bias', 'IV Bias'), col=c('steel blue', 'tomato'), lty=3:3, cex=0.8, lwd=3)



plot(bias[,5], bias[,1])  # Plot correlation between x and c against the ols bias
# Results: as the correlation between x and c increases, the OLS bias increases at a decreasing rate

plot(bias[,5], bias[,2]) # The IV estimates are unbiased, but consistent only when the correlation between x and c is at about 0.2. 


# Notes: as the variance of X* increases, the bias of the OLS decreases and continues to stay unbiased. 
# Meanwhile, the variance of the IV estimate increases, but stays unbiased.


plot(bias[,2], bias[,1], xlab='Variance of X*', ylab='Difference in Bias Between IV and OLS', col='steel blue', 
     main='Effect of Variance of X* on the Difference Between IV and OLS Estimates')


# Regression fit in order to find the line that approximates the relationship between the variance of x* and the 
# difference in bias between IV and OLS

xr <- 1/sqrt(bias[,2]+1)   # This is the function covariance function between 
yr <- bias[,1]

b0 <- summary(lm(yr~xr))$coefficients[1]
b1 <- summary(lm(yr~xr))$coefficients[2]

lines(bias[,2], b0 + b1*xr)


plot(bias[,2], log(bias[,1]))
############################################################################################################

# It appears that the variance of the difference between the OLS and IV estimators is related to the variance of x*.
#
# Use theory to determine where the 0.406879 and the -0.067155 come from. Do they approximate to 0.4 and 0, respectively (or some other number)?





# Other endogeneity problems besides omitted variables:
# 1. reverse causation (when changes in the dependent variable change the value of at least one of the covariates)
# 2. When the covariates are subject to non-random measurement error. 



# Question: Does the type of endogeneity problem affect the usefulness of the instrumental variable vs OLS???



################################################################################
# We will now explore what happens to  the correlation of x and z as we 
# change the correlation of c and z, for various values of z. We won't worry
# about taking the averages of the outcomes for each iteration as we did above,
# because we are already doing 2 for loops. However, we may look further into doing
# that later if necessary.
################################################################################

library("MASS")
corrxz <- seq(0,0.9,0.05)
olsbias <- NULL
ivbias <- NULL
for(i in 1:length(corrxz)){
  correlation_cz <- corrcz[i]
  varz <- 1
  varc <- 4
  covariance_cz <- correlation_cz*sqrt(varz)*sqrt(varc)
  for(j in 1:100) {
    xSzc <- mvrnorm(1000, c(20,15,0), matrix(c(4,0.5,0,0.5,4,covariance_cz,0,covariance_cz,varz),3,3))  # Multivariate r norm
    xStar <- xSzc[,1]
    z <- xSzc[,2]
    c <- xSzc[,3]  # Mean 0 and sd 1 by default
    x <- xStar + z
    
    y <- 1 + x + c + rnorm(1000, 0, 0.5)  # Note that all beta parameters are 1 here
    
    olsbiasm[i,j] <- summary(lm(y~x))$coefficients[2] -1
    ivbiasm[i,j] <- summary(ivreg(y~x|z))$coefficients[2] -1
    differm[j] <- olsbiasm[j] - ivbiasm[j]
    corrxcm[j] <- cor(x,c)
    corrxzm[j] <- cor(x,z) 
    
    
    corrcz[i,j] <- correlation_cz
    corrxz[i,j] <- cor(x,z)
    correlationcz <- correlation_cz+0.1
  }
  variance_z <- variance_z + 0.5
}
list(corrxz,corrcz)

}

correlation_sim <- ivsim2(100)




# Correlation Study -------------------------------------------------------

library("MASS")

results <- NULL
corrcz <- seq(0,0.95,0.05)


for(i in 1:length(corrcz)){
  varc <- 4
  varz <- 1
  covcz <- corrcz[i]*sqrt(varx)*sqrt(varz)
  for(j in 1:100){   # 100 datasets of 1000 rows each
    xSzc <- mvrnorm(1000, c(20,15,0), matrix(c(4,0.5,0,0.5,4,covcz,0,covcz,varz),3,3))  # Multivariate r norm
    xStar <- xSzc[,1]
    z <- xSzc[,2]
    c <- xSzc[,3]  # Mean 0 and sd 1 by default
    x <- xStar + z
    
    y <- 1 + x + c + rnorm(1000, 0, 0.5)  # Note that all beta parameters are 1 here
    
    olsbiasm[j,i] <- summary(lm(y~x))$coefficients[2] -1
    ivbiasm[j,i] <- summary(ivreg(y~x|z))$coefficients[2] -1
    
  }
}
}


# Heat Map ----------------------------------------------------------------

library("MASS")
library("AER")
install.packages("matrixcalc")
library('matrixcalc')
install.packages("corpcor")
library('corpcor')
install.packages('fields')
library('fields')

# See progress
install.packages('svMisc')
library('svMisc')
require(svMisc)

corrxz <- seq(-1,1,by=0.039)
sigmaxS <- NULL
for(i in 1:length(corrxz)){
  sigmaxS <- (1/corrxz)-1
}

corrcz <- seq(-1,1,by=0.021)
varz <- 1
varc <- 1
sigmacz <- corrcz * sqrt(varz) * sqrt(varc)


outols <- array(NA, c(length(corrxz), length(corrcz),100))
outiv <- array(NA, c(length(corrxz), length(corrcz),100))
outdiff <- array(NA, c(length(corrxz), length(corrcz),100))
outcorrcz <- array(NA, c(length(corrxz), length(corrcz),100))
outcorrxz <- array(NA, c(length(corrxz), length(corrcz),100))

for(i in 1:length(corrxz)){
  for(j in 1:length(corrcz)){
    for(k in 1:100){

      M <- make.positive.definite(matrix(c(sigmaxS[i]^2,0.5,0,0.5,varc,sigmacz[j],0,sigmacz[j],varz),3,3))
      xSzc <- mvrnorm(1000, c(20,15,0), M) 
      xStar <- xSzc[,1] 
      z <- xSzc[,2] 
      c <- xSzc[,3]   # Mean 0 and sd 1 by default
      x <- xStar + z 
      y <- 1 + x + c + rnorm(1000, 0, 0.5)  
        
      outols[i,j,k] <- summary(lm(y~x))$coefficients[2] -1 
      outiv[i,j,k] <- summary(ivreg(y~x|z))$coefficients[2] -1 
      outcorrcz[i,j,k] <- cor(c,z) 
      outcorrxz[i,j,k] <- cor(x,z) 
    } 
  }
  progress(i, progress.bar = TRUE)
  Sys.sleep(0.01)
  if(i==length(corrxz)) cat('Done!\n')
}




# OLS Heat Plot -----------------------------------------------------------


heatols <- NULL


# For OLS estimator
heatols <- matrix(0,length(corrxz),length(corrcz))
for(i in 1:100) {
  heatols <- heatols + outols[,,i]
}


avheat <- heatols/100


image.plot(corrxz, corrcz, avheat, main='OLS Bias', xlab='Correlaton Between of Endogenous X and Instrumental Variable', ylab='Correlation Between Omitted Variable and 
           Instrumental Variable')






# IV Heat Plot ------------------------------------------------------------


heativ <- NULL


# For IV estimator
heativ <- matrix(0,length(corrxz),length(corrcz))
for(i in 1:100) {
  heativ <- heativ + outiv[,,i]
}


avheativ <- heativ/100


avheativ[avheativ > 0.5] <- 0.5
avheativ[avheativ < -0.5] <- -0.5


image.plot(corrxz, corrcz, avheativ, main='Instrumental Variable Regression Bias', xlab='Correlaton Between of Endogenous X and Instrumental Variable', 
            ylab='Correlation Between\n Omitted Variable and Instrumental Variable')











# Definitional Heat Plot --------------------------------------------------



corrxz <- seq(-1,1,by=0.079)
sigmaxS <- NULL
for(i in 1:length(corrxz)){
  sigmaxS <- (1/corrxz)-1
}

corrcz <- seq(-1,1,by=0.079)
varz <- 1
varc <- 1
sigmacz <- corrcz * sqrt(varz) * sqrt(varc)

outols <- array(NA, c(length(corrxz), length(corrcz),100))
outiv <- array(NA, c(length(corrxz), length(corrcz),100))



for(i in 1:length(corrxz)){
  for(j in 1:length(corrcz)){
    for(k in 1:100){
      
      M <- make.positive.definite(matrix(c(sigmaxS[i]^2,0.5,0,0.5,varc,sigmacz[j],0,sigmacz[j],varz),3,3))
      xSzc <- mvrnorm(1000, c(20,15,0), M) 
      xStar <- xSzc[,1] 
      z <- xSzc[,2] 
      c <- xSzc[,3]   # Mean 0 and sd 1 by default
      x <- xStar + z 
      
      outols[i,j,k] <- cov(x,c) / var(x)
      outiv[i,j,k] <- sigmacz[j] / sd(z)

    } 
  }

}



# For OLS estimator
heatols <- matrix(0,length(corrxz),length(corrcz))
for(i in 1:100) {
  heatols <- heatols + outols[,,i]
}


avheat <- heatols/100


image.plot(corrxz, corrcz, avheat, main='OLS Bias', xlab='Correlaton Between of Endogenous X and Instrumental Variable', ylab='Correlation Between Omitted Variable and 
           Instrumental Variable')






# IV Heat Plot ------------------------------------------------------------


heativ <- NULL


# For IV estimator
heativ <- matrix(0,length(corrxz),length(corrcz))
for(i in 1:100) {
  heativ <- heativ + outiv[,,i]
}


avheativ <- heativ/100


image.plot(corrxz, corrcz, avheativ, main='Instrumental Variable Regression Bias', xlab='Correlaton Between of Endogenous X and Instrumental Variable', 
           ylab='Correlation Between\n Omitted Variable and Instrumental Variable')















