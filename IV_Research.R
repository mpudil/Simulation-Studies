# Linear Regression Simulation
set.seed(15)
# y = b0 + b1*x1 + b2*x2 + e 
# x1 is exogenous
# x2 is endogenous and is correlated with e
# z1 (not shown in model) is an instrumental variable (IV) and is correlated with x2 but not with e 
  # or with y in any other way



# Generate z1 
z1 = rnorm(n=100, mean=5, sd=0.1)

# Generate x2 based off of x3
x2 = z1 + rnorm(n=100, mean=1, sd=0.3)

# Generate b0, b1, b2, x1, and e
b0 = 6
b1 = 0.5
b2 = 0.2
x1 = rnorm(n=100, mean=50, sd=8)
e = rnorm(n=100, mean=0, sd=1)

# Generate y
y=NA #initialize y
for(i in 1:100){
  y[i] = b0 + b1*x1[i] + b2*x2[i] + e[i]
}

# OLS
lm(y~x1 + x2)

# IV regression

#install.packages('AER')
#library(AER)
#install.packages('systemfit')
#library(systemfit)

#Important note: Endogenous variables (X) can only appear before the vertical line; 
#instruments (Z) can only appear after the vertical line; exogenous regressors that 
#are not instruments (W) must appear both before and after the vertical line.

reg_iv0 <- ivreg(y ~ x1 + x2 | x1 + z1)
summary(reg_iv0)





####################################################

# Example: effect of education on income (n=500)
# Note that here, all values are normally distributed and there is a linear relationship 
  # between all values

#############################################################################
# Model: income = b0 + b1*years_education + b2*gpa + e
# Note that in this simulation, gpa is exogenous and years_education is 
# endogenous but related to the instrumental variable, years_mother_education
##############################################################################


education_sim <- function(nReps){
  ols_intercept_bias <- NULL
  ols_years_education_bias <- NULL
  ols_gpa_bias <- NULL
  
  iv_intercept_bias <- NULL
  iv_years_education_bias <- NULL
  iv_gpa_bias <- NULL
  
  
  # Generate years_mother_education
  for(j in 1:nReps){
    income=NA #initialize income
    years_education <- rnorm(500, 11, 3)

   # Generate years_education
    years_mother_education <- 0.2*years_education + rnorm(500, 8, 3)

    # Generate gpa
    gpa <- rnorm(500, 3, 0.5)

    # Generate error term e
    e <- rnorm(5000, 0, 10000)
  
    # Generate income
    # b0: mean of all incomes
    # b1: diminishing marginal effect of years of education on income: for every 1 more year of education, assume
      # a $1,000 increase in income

  
    for(i in 1:500){
       income[i] <- 40000 + 1000*years_education[i] + 5000*gpa[i] + e[i]
      }

  # OLS Regression
  ols_education <- lm(income ~ years_education + gpa)
  ols_intercept_bias[j] <- ols_education$coefficients[1] - 40000
  ols_years_education_bias[j] <- ols_education$coefficients[2] - 1000
  ols_gpa_bias[j] <- ols_education$coefficients[3]-5000


  # IV Regression
  iv_education <- ivreg(income ~ years_education + gpa| gpa + years_mother_education)
  iv_intercept_bias[j] <- iv_education$coefficients[1] - 40000
  iv_years_education_bias[j] <- iv_education$coefficients[2] - 1000
  iv_gpa_bias[j] <- iv_education$coefficients[3]-5000
  }
  
  all_biases <- cbind(ols_intercept_bias, ols_years_education_bias, ols_gpa_bias, iv_intercept_bias, iv_years_education_bias, iv_gpa_bias)
 
  #Create boxplot to show biases
  
  all_biases <- as.data.frame(all_bias)
  

}
  
  
df <- as.data.frame(education_sim(1000))

# Boxplot for all biases (blue is OLS, red is IV)
par(mfrow=c(2,3))
boxplot(df2[1], main="OLS Intercept", ylab="Bias", col="blue")
boxplot(df2[2], main="OLS Years Education", ylab="Bias", col="blue")
boxplot(df2[3], main="OLS GPA", ylab="Bias", col="blue")
boxplot(df2[4], main="IV Intercept", ylab="Bias", col="red")
boxplot(df2[5], main="IV Years Education", ylab="Bias", col="red")
boxplot(df2[6], main="IV GPA", ylab="Bias", col="red")


# Boxplot for only the endogenous variable of years of education
# Boxplot for only the endogenous variable of years of education
boxplot(df2[2], xlim=c(1,3), ylim=c(-2500,2500), at=1.5, col="blue", xlab="OLS bias")
boxplot(df2[5], add=TRUE, at=2.5, col="red", xlab="IV bias")



######################################################################
##### Generalization of all-normal distribution with IV and OLS #####
######################################################################


# Basically the above example, but with the betas being drawn randomly as well
# Additionally, we will include bias percentage
# Here, we will define the term "bias percentage" to be equal to:
# abs((estimated parameter - true parameter) / (true parameter))


education_sim3 <- function(nReps){
  ols_intercept_bias <- NULL
  ols_years_education_bias <- NULL
  ols_gpa_bias <- NULL
  
  ols_intercept_bias_percentage <- NULL
  ols_years_education_bias_percentage <- NULL
  ols_gpa_bias_percentage <- NULL
  
  iv_intercept_bias <- NULL
  iv_years_education_bias <- NULL
  iv_gpa_bias <- NULL
  
  iv_intercept_bias_percentage  <- NULL
  iv_years_education_bias_percentage  <- NULL
  iv_gpa_bias_percentage  <- NULL
  
  
  # Generate years_mother_education
  for(j in 1:nReps){
    income=NA #initialize income
    mean_ed <- runif(1,0,1000)
    sd_ed <- runif(1,0,0.1*mean_ed)
    years_education <- rnorm(500, mean_ed, sd_ed)
    
    # Generate years_education
    years_mother_education <- rnorm(500,0.2,0.05)*years_education + rnorm(500, 8, 3)
    
    # Generate gpa
    gpa <- rnorm(500, 3, 0.5)
    
    # Generate error term e
    e <- rnorm(5000, 0, 10000)
    
    # Generate income
    # b0: mean of all incomes
    # b1: diminishing marginal effect of years of education on income: for every 1 more year of education, assume
    # a $1,000 increase in income
    
    b0_income <- rnorm(1, 40000, 3000)
    b1_income <- rnorm(1,1000,100)
    b2_income <- rnorm(1,5000, 500)
    for(i in 1:500){
      income[i] <- b0_income + b1_income*years_education[i] + b2_income*gpa[i] + e[i]
    }
    
    # OLS Regression
    ols_education <- lm(income ~ years_education + gpa)
    ols_intercept_bias[j] <- ols_education$coefficients[1] - b0_income
    ols_years_education_bias[j] <- ols_education$coefficients[2] - b1_income
    ols_gpa_bias[j] <- ols_education$coefficients[3] - b2_income
    
    # OLS Regression Percentage
    
    ols_intercept_bias_percentage[j] <- 100*((ols_education$coefficients[1] - b0_income)/(b0_income))
    ols_years_education_bias_percentage[j] <- 100*((ols_education$coefficients[2] - b1_income)/(b1_income))
    ols_gpa_bias_percentage[j] <- 100*((ols_education$coefficients[3] - b2_income)/(b2_income))
    
    
    # IV Regression
    iv_education <- ivreg(income ~ years_education + gpa| gpa + years_mother_education)
    iv_intercept_bias[j] <- iv_education$coefficients[1] - b0_income
    iv_years_education_bias[j] <- iv_education$coefficients[2] - b1_income
    iv_gpa_bias[j] <- iv_education$coefficients[3] - b2_income
    
    # IV Regression Percentage
    iv_education <- ivreg(income ~ years_education + gpa| gpa + years_mother_education)
    iv_intercept_bias_percentage[j] <- 100*(iv_intercept_bias[j]/b0_income)
    iv_years_education_bias_percentage[j] <- 100*(iv_years_education_bias[j]/b1_income)
    iv_gpa_bias_percentage[j] <- 100*(iv_gpa_bias[j]/b2_income)
  }
  # Create dataframe with all bias columns (normal and percent) included from IV and OLS regression
  all_biases <- as.data.frame(cbind(ols_intercept_bias, ols_years_education_bias, 
                                    ols_gpa_bias, ols_intercept_bias_percentage, 
                                    ols_years_education_bias_percentage, 
                                    ols_gpa_bias_percentage, iv_intercept_bias, 
                                    iv_years_education_bias, iv_gpa_bias, 
                                    iv_intercept_bias_percentage, iv_years_education_bias_percentage, 
                                    iv_gpa_bias_percentage))
  
  
  
}


results3 <- as.data.frame(education_sim3(100))


# Boxplot for all bias percentages (blue is OLS, red is IV)
par(mfrow=c(2,3))
boxplot(results3[4], main="OLS Intercept Percentage Bias", ylim=c(-100,100), ylab="Bias (%)", col="blue")
boxplot(results3[5], main="OLS Years Education Percentage Bias", ylim=c(-10,10), ylab="Bias (%)", col="blue")
boxplot(results3[6], main="OLS GPA Percentage Bias Percentage Bias", ylim=c(-50,50), ylab="Bias (%)", col="blue")
boxplot(results3[10], main="IV Intercept Percentage Bias", ylim=c(-200,400), ylab="Bias (%)", col="red")
boxplot(results3[11], main="IV Education Percentage Bias", ylim=c(-50,50), ylab="Bias (%)", col="red")
boxplot(results3[12], main="IV GPA Percentage Bias", ylim=c(-100,100), ylab="Bias (%)", col="red")



# Boxplot for only the endogenous variable of years of education (percentage bias)
boxplot(results3[5], xlim=c(1,3), ylim=c(-50,50), at=1.65, col="steel blue", main="OLS Bias                        IV Bias")
boxplot(results3[11], add=TRUE, at=2.5, col="tomato")


# It appears in these situations, OLS is a considerably better estimator than IV. 





####################################################################################################
# Now let's introduce omitted variable bias into our regression by omitting the exogegnous x.
# Note how the instrumental variableregression compares to the OLS estimation. Let's revert back to
# fixing our betas. We will also be exclusively looking at only the bias of the endogeneous variable,
# but will be looking at 
####################################################################################################

education_sim4 <- function(nReps){
  ols_years_education_bias <- NULL
  ols_years_education_bias_percentage <- NULL

  iv_years_education_bias <- NULL
  iv_years_education_bias_percentage <- NULL
  
  
  # Generate years_mother_education
  for(j in 1:nReps){
    income=NA #initialize income
    years_education <- rnorm(500, 11, 3)
    
    # Generate years_education
    years_mother_education <- 0.2*years_education + rnorm(500, 8, 3)
    
    # Generate gpa
    gpa <- rnorm(500, 3, 0.5)
    
    # Generate error term e
    e <- rnorm(5000, 0, 10000)
    
    # Generate income
    # b0: mean of all incomes
    # b1: diminishing marginal effect of years of education on income: for every 1 more year of education, assume
    # a $1,000 increase in income
    
    
    for(i in 1:500){
      income[i] <- 40000 + 1000*years_education[i] + 5000*gpa[i] + e[i]  # Note that income now follows a bivariate normal distribution
    }
    
    # OLS Regression
    ols_education <- lm(income ~ years_education)
    ols_years_education_bias[j] <- ols_education$coefficients[2] - 1000
    ols_years_education_bias_percentage[j] <- 100*((ols_years_education_bias[j])/(ols_education$coefficients[2]))
    
    # IV Regression
    iv_education <- ivreg(income ~ years_education|years_mother_education)
    iv_years_education_bias[j] <- iv_education$coefficients[2] - 1000
    iv_years_education_bias_percentage[j] <- 100*(iv_years_education_bias[j]/(iv_education$coefficients[2]))

  }
  
  all_biases <- cbind(ols_years_education_bias, iv_years_education_bias, ols_years_education_bias_percentage,  iv_years_education_bias_percentage)
  
  #Create boxplot to show biases
  
  all_biases <- as.data.frame(all_biases)
  
  
}


results4 <- as.data.frame(education_sim4(1000))

# Boxplot for all biases (blue is OLS, red is IV)
boxplot(results4[1], xlim=c(1,3), at=1.5, main="OLS Years Education", ylab="Bias", col="blue")
boxplot(results4[2], add=TRUE, at=2.5, atmain="IV Years Education", ylab="Bias", col="red")


boxplot(results4[3], xlim=c(1,3), at=1.7, ylim=c(-200,200), main="OLS Bias                  IV Bias", ylab="Bias (%)", col="blue")
boxplot(results4[4], add=TRUE, at=2.5, col="red")





















####################################################################################################
# Now let's use a Poison distribution rather than a normal distribution to see if there is a
# difference between using years of education as a continuous random variable or a discrete variable
####################################################################################################

education_sim5 <- function(nReps){
  ols_years_education_bias <- NULL
  ols_years_education_bias_percentage <- NULL
  
  iv_years_education_bias <- NULL
  iv_years_education_bias_percentage <- NULL
  
  
  # Generate years_mother_education
  for(j in 1:nReps){
    income=NA #initialize income
    years_education <- rpois(500, 11)
    
    # Generate years_education
    years_mother_education <- 0.2*years_education + rnorm(500, 8, 3)
    
    # Generate gpa
    gpa <- rnorm(500, 3, 0.5)
    
    # Generate error term e
    e <- rnorm(5000, 0, 10000)
    
    # Generate income
    # b0: mean of all incomes
    # b1: diminishing marginal effect of years of education on income: for every 1 more year of education, assume
    # a $1,000 increase in income
    
    
    for(i in 1:500){
      income[i] <- 40000 + 1000*years_education[i] + 5000*gpa[i] + e[i]
    }
    
    # OLS Regression
    ols_education <- lm(income ~ years_education)
    ols_years_education_bias[j] <- ols_education$coefficients[2] - 1000
    ols_years_education_bias_percentage[j] <- 100*((ols_years_education_bias[j])/(ols_education$coefficients[2]))
    
    # IV Regression
    iv_education <- ivreg(income ~ years_education|years_mother_education)
    iv_years_education_bias[j] <- iv_education$coefficients[2] - 1000
    iv_years_education_bias_percentage[j] <- 100*(iv_years_education_bias[j]/(iv_education$coefficients[2]))
    
  }
  
  all_biases <- cbind(ols_years_education_bias, iv_years_education_bias, ols_years_education_bias_percentage,  iv_years_education_bias_percentage)
  
  #Create boxplot to show biases
  
  all_biases <- as.data.frame(all_biases)
  
  
}


results4 <- as.data.frame(education_sim4(1000))

# Boxplot for all biases (blue is OLS, red is IV)
boxplot(results4[1], xlim=c(1,3), at=1.5, main="OLS Years Education", ylab="Bias", col="blue")
boxplot(results4[2], add=TRUE, at=2.5, atmain="IV Years Education", ylab="Bias", col="red")


boxplot(results4[3], xlim=c(1,3), at=1.7, ylim=c(-200,200), main="OLS Bias                     IV Bias", ylab="Bias (%)", col="blue")
boxplot(results4[4], add=TRUE, at=2.5, col="red")








