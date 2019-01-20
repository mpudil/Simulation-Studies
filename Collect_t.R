# Collect t 
# Problem: A company wants to know how changing the number of toys to collect in a cereal 
# box would affect the number of boxes the average customer would need to purchase before 
# attaining all the toys. The arguments included in the function will be:

# 1. The number (N) of customers that successfully complete the challenge
# 2. The number of toys (t) in the challenge
# 3. The probability (p) of selecting each toy

# Part 1

# For the first part, you will use a Monte Carlo simulation to approximate the effect that adding 
# an additional toy into the prize affects the number of attempts necessary to complete
# the challenge. For the regression, hold N constant at 1000 and always use equal probability of 
# collecting toys, but vary the number of toys needed from 1 to 30. 

# You will then plot the number of toys to collect against the number of tries needed to collect
# all toys. The plot will contain the mean number of tries (your Monte Carlo estimate) and a 
# regression line that goes through the points for reference. To assure similar results, set the 
# seed to 12. Hint: the command coef(lm(y~x)) will return the intercept and slope of a line
# (be sure to replace y and x with the appropriate dependent and independent variable, respectively).

# Solution to Part 1:
set.seed(12)

cereal <- function(N, t, p=NULL){
  
  number_tries <- numeric()
  for(i in 1:N){
    all_prizes <- numeric()
    number_toys <- 0
    while(number_toys < t){
      box <- sample(1:t,1, replace=TRUE, prob = p)
      all_prizes <- c(all_prizes,box)
      number_toys <- length(unique(all_prizes))
    }
    number_tries[i] <- length(all_prizes)
  }
  
  list(mc.est=mean(number_tries), mc.error=sd(number_tries)/sqrt(N), results = number_tries)

}

toy_data <- matrix(nrow=30, ncol=2)

for(i in 1:30){
  toy_data[i,1] <- i
  toy_data[i,2] <- cereal(1000,i)$mc.est
}

toy_data <- data.frame(toy_data)
colnames(toy_data) <- c('Toys', 'Tries')

coefs <- coef(lm(toy_data$Tries ~ toy_data$Toys))

plot(toy_data$Toys, toy_data$Tries, col='steel blue', main='Average Number of Tries Until All Toys Collected, by Number of Toys',
     xlab='Number of Toys to Collect', ylab='Number of Tries Needed to Collect All Toys')

abline(reg=coefs, col='tomato')


# Answer: On average, one more toy to collect leads to about 4.2 more attempts needed to collect all



# Part 2:

# Now we explore how many attempts are needed when the probability of collecting each toy differs
# (for example, when the chance of getting one toy is only 1% and all other are equally probable)

cereal(1000, 5, c(0.01,0.99/4,0.99/4,0.99/4,0.99/4))$mc.est
# On average, it takes about 100 tries to get all 5 toys in this scenario (setting N=1000, and t=5)




