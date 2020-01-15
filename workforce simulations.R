#create some random attrition probabilities by employee
#if you already have annual employee attrition probabilities you can ignore the code below and read them into a field called "p_term_annual" 

data <- data.frame("EE" <- c(1:10000), p_term_annual <- 0)
data$p_term_annual <- 3*rnorm(nrow(data), .1, .025)*runif(nrow(data), 0, 1)^2

#set parameters (number of months to forecast and number of simulations)

no_months <- 12
n_sims <- 5000

#load ggplot library

library(ggplot2)

#view summary statistics

min(data$p_term_annual)
max(data$p_term_annual)
mean(data$p_term_annual)

#adjust termination rates based on time period input

data$p_term_adjusted <- 1 - (1 - data$p_term_annual)^(no_months/12)

#view summary statistics for adjusted data

min(data$p_term_adjusted)
max(data$p_term_adjusted)
mean(data$p_term_adjusted)

#run sims

sim_results <- vector("numeric", n_sims)

for (i in 1:n_sims)
  
{
  data$rand <- runif(nrow(data), 0, 1)
  
  data$term <- 0
  data$term[data$rand < data$p_term_adjusted] <- 1
  
  sim_results[i] <- sum(data$term)
  
  next
}

#put output in a data frame

output <- data.frame(sim_results)

#plot histogram

f <- ggplot(output, aes(x = sim_results)) +
  geom_histogram(color = "black", fill = "blue2", binwidth = 1) +
  labs(title = "Workforce Simulations", x = "Number of terminations", "Number of occurences")
f