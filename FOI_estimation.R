##Simple catalytic model estimation
# assumes foi is homogenous and constant
# P(a) = 1 - e^(-lambda*t)
# P(a) - 1 = - e^(-lambda*t) 
# 1 - p(a) = e^(-lambda*t)
# log(1 - P(a)) = -lambda * t
# -log(1 - P(a)) = lambda * t

mc_seroneg <- c(79, 49, 37, 12, 11) #mexico city seronegativity by age 
gua_seroneg <- c(39, 39, 46, 0, 0) #guadalajara seronegativity by age 
mont_seroneg <- c(75, 62, 56, 25, 19) #mont. seronegativity by age 

mean_seroneg <- colMeans(rbind(mc_seroneg, gua_seroneg, mont_seroneg)) #estimate the mean of each age band
mean_seroprev <- 100 - mean_seroneg #estimate seroprevalence
mean_seroprev <- mean_seroprev/100 #convert to proportion
mean_seroprev

sero_data <- tibble(age = c(0.75, 1.5, 2.5, 3.5, 4.5),
                    sero_prev = mean_seroprev)


sero_fit <- lm(-log(1 - sero_prev) ~ 0 + age, data = sero_data) #remove intercept--sero = 1 -exp(-lambda_hat * t) 

lambda_hat1 <- coef(sero_fit) 
lambda_hat1 #foi

lambda_prob1 <- 1 - exp(-lambda_hat1) #convert foi hazard to yearly infection probability
lambda_prob1


#Simple SIR approximation: R0-1/L
#assumes endemic equilibrium

# Re = (S/N)*R0 = 1 at equilibrium
#S/N = 1/R0

# dS/dt = mN - mS - lambdaS = 0
# S(m+lambda) = mN
# S/N = m/m+lambda
# 1/R0 = m/m+lambda
# R0m = m + lambda
# lambda = m(R0-1)
#lambda = r0-1/L


lambda_hat2 <- (11-1)/55 #assuming R0 = 11 and life expectancy is 55 in Nigeria
lambda_hat2
lambda_prob2 <- 1 - exp(-lambda_hat2) # convert to yearly probability
lambda_prob2
