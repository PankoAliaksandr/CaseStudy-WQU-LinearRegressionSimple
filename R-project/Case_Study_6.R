# Case Study 6

# Description:
#===============================================================================================================
# Task 1: Download data for last 1 year for the DJIA and all its 30 constituent stocks.
# Task 2: Calculate daily returns of the DJIA index and the downloaded stocks over the period under study
# Task 3: Considering the equation form provided above and matching the Index returns vs.
#         the returns of one of its constituent stocks at a time, perform linear regression fits and
#         calculate α(alpha) and β values for each stock
# Task 4: Graphically represent the distribution of α(alpha) and β values for the constituents of DJIA
#===============================================================================================================

library(quantmod)
library(PerformanceAnalytics)


# Task 1: Download data
global_data_frame <- read.csv(file = "d:/DJprices_d.csv")

#Convert Date into "YYYY-MM-DD" format
global_data_frame$Date <- as.Date(global_data_frame$Date, "%m/%d/%Y")

#Create time series for all data frame
time_series_character <- xts(x = global_data_frame,order.by = global_data_frame$Date )

#Remove "Date" column
time_series_character$Date <- NULL

#Convert character time series into numeric
time_series_numeric <-as.numeric(time_series_character)
attributes(time_series_numeric) <- attributes(time_series_character)

# Task 2: Daily returns

daily_returns <- Return.calculate(prices = time_series_numeric, method = "discrete")
daily_returns <- daily_returns[-1,]

# Task 3: linear regression

number_of_stocks <- ncol(daily_returns)

names <-colnames(daily_returns)
names <- names[-1]
matrix_coeff <- c()

for (i in 2: number_of_stocks){
  index_returns_vector <- as.vector(daily_returns[,1])
  stock_rerurns_vector <-as.vector(daily_returns[,i])
  t_line <-lm(stock_rerurns_vector ~ index_returns_vector)
  coeff <- coef(t_line)
  r_squared <- summary(t_line)$r.squared
  p_value_intercept <- summary(t_line)$coefficients['(Intercept)',4]
  p_value_beta <- summary(t_line)$coefficients['index_returns_vector',4]
  coeff <-append(coeff, c(p_value_intercept, p_value_beta))
  coeff <- append(coeff, r_squared)
  matrix_coeff <- rbind(matrix_coeff, coeff,deparse.level = 0)
}

colnames(matrix_coeff) <- c('alpha', 'beta', 'p.value_b0', 'p.value_b1', 'R.squared')
df <- data.frame(matrix_coeff)
rownames(df) <- names

# Find how many alpha is significant
v <-as.vector(df$p.value_b0)
y <- v[ v < 0.05 ]
number_of_sing_alpha <- length(y)
number_of_sing_alpha

# Task 4. Graphical representation
hist(df$beta, xlab = 'beta values', ylab = 'beta frequencies', main = 'beta distribution', col = c('red', 'blue', 'yellow','cyan','magenta','green'))
hist(df$alpha, xlab = 'alpha values', ylab = 'alpha frequencies',main = 'alpha distribution', col = c('red', 'blue', 'yellow','cyan','magenta','green'))

