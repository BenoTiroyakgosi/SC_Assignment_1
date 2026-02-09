# Author : Beno Tiroyakgosi
# Date   : 07 February 2026
# Subject: Statistical Computing Practical 1 


#==============================================
#  Load Packages
#==============================================
library(car)
library(tidyverse)
library(dplyr)
library(nycflights13)



#==============================================
#  Load Data
#==============================================

data("airquality")
data("cars")




#===============================================
#  Practical 1
#===============================================


#-----------------------------------------------
#  Question 1
#-----------------------------------------------


# This checks for NA in all the entries of the data frame
logical_df <- is.na(airquality)       
  
# Summing across the rows. If NA exists its recorded as 1 in logical_df
check_na <-  apply(logical_df,1,sum)

# creates a vector with the rows which have NA
rows_with_na <- which(check_na>0)
print(rows_with_na)

#-------------------------------------------------
#  Question 2
#-------------------------------------------------

statistics_fun <- function(input){
  # This function computes the mean, sd, min and max of a numerical vector input
  # This function returns a vector of the statistics 
  # NAs are removed during computation and results are rounded to 2 decimal places
  
  average   <- round(mean(input, na.rm = TRUE), 2)
  stand_dev <- round(sd(input, na.rm = TRUE), 2)
  min_val   <- round(min(input, na.rm = TRUE), 2)
  max_val   <- round(max(input, na.rm = TRUE), 2)
  
  return(c(average, stand_dev, min_val, max_val))
  
  }

# calling function to compute stats
temp_stats  <- statistics_fun(airquality$Temp)    
ozone_stats <- statistics_fun(airquality$Ozone)


# Creating data frame of statistics 
stats_df <- data.frame(temp_stats,
                       ozone_stats,
                       row.names = c("Mean", "Standard Deviation", "Minimum", "Maximum")
                       )

print(stats_df)

#---------------------------------------------
# Question 3
#---------------------------------------------



# Binding 1s to regressor/X which accounts for intercept

regressor_x <- cbind(1, cars$speed) 

# Storing dependent variable/ Y
dependent_var_y <- cars$dist


# Beta computation
beta <- solve(t(regressor_x) %*% regressor_x) %*% t(regressor_x) %*% dependent_var_y
print(beta)



#-----------------------------------------------
#  Question 4
#-----------------------------------------------

# Fit linear regression model where distance depends on speed
linear_model <- lm(dist ~ speed, data = cars)
summary(linear_model)



#===============================================
#  Day 3: Question 4 and 5
#===============================================


#-----------------------------------------------
#  Question 4
#-----------------------------------------------

# Generate a sequence of 10000 numbers from -2 to 2
nums <- seq(-2, 2 , length.out = 10000)

# Obtain the respective sin(x) values
sin_values <- sin(nums)

plot(nums, sin_values,
     pch  = 19,        # plot character
     main = "Graph of sin(x) for x in [-2,2]",
     xlab = "x",
     ylab = "sin(x)",
     lwd  = 3,         # line width
     col  = "red",
     las  = 1  ,       # axis 
     cex.lab = 1.4,    # axis titles
     cex.axis = 1.2   # axis tick labels
     )


#------------------------------------------------
#  Question 5
#------------------------------------------------


# sampling from a t-distribution
# setting seed makes simulation reproducible 
set.seed(123)          
t_sample <- rt(1000, df = 1)

# The QQ-plot compares ordered values so the values sampled are sorted

t_sorted <- sort(t_sample)
num_of_t <- length(t_sorted)

# Approximate normal quantiles to plot against t sample quantiles
set.seed(456)
sim_norm        <- rnorm(10000)
sim_norm_sorted <- sort(sim_norm)

# relative position to be used for quantile matching
relative_pos_cal <- (1:num_of_t) / num_of_t   

#relative position multiplied by total number of sim norm  to exctract 1000 associated values
expected_q <- sim_norm_sorted[round(relative_pos_cal * length(sim_norm_sorted))] 

#Beginning of 95% envelope
set.seed(789)
n_sims <- 1000

# simulate 1000x1000 matrix with columns being from the same normal sample. So 1000 samples. 
# The idea is to obtain the 2.5th and 97.5th quantile from each nth sample to plot

sim_matrix <- matrix(rnorm(num_of_t*n_sims), nrow = num_of_t, ncol = n_sims)

# sorting columns then obtaining quantiles
sim_matrix <- apply(sim_matrix, 2, sort)

lower_2_point_5 <- function(input){
  #This function finds and returns the 2.5th percentile 
  
  return(quantile(input, probs = 0.025))
}

upper_97_point_5 <- function(input){
  #This function finds and returns the 97.5th percentile
  
  return(quantile(input, probs = 0.975))
}


lower <- apply(sim_matrix, 1, lower_2_point_5)
upper <- apply(sim_matrix, 1, upper_97_point_5)

# Manual QQ-plot
plot(expected_q, t_sorted,
     xlab = "Standard Normal Quantiles", 
     ylab = "Sample Quantiles (t)", 
     main = "Manual QQ-plot vs Standard Normal",
     type = "p",
     col = "black",
     ylim = c(-5,5)                              # adjusting y axis limits to match presentation of qqPlot
     )
grid(col = "grey")
lines(expected_q, lower, col = "red", lty = 1)   # lower bound (2.5th quantile)
lines(expected_q, upper, col = "red", lty = 1)   # upper bound (97.5th quantile)
abline(0, 1, col = "blue", lwd = 1)  # 45 degree line


# QQ-plot against standard normal for verification
qqPlot(t_sorted,
       envelope = 0.95,
       ylim = c(-5, 5),
       main = "QQ-plot using R function",
       xlab = "Standard Normal Quantiles",
       ylab = "Sample Quantiles(t)"
       )

#===================================================
#  Practical 4
#===================================================


#---------------------------------------------------
#  Question 1
#---------------------------------------------------

# This function displays columns and notes NA values

UtilsDataRSV::view_cols(flights)


#---------------------------------------------------
# Question 2
#---------------------------------------------------



flight1 <- flights%>%
  filter(month == 1)%>%      # choosing all rows with a month value of 1
  group_by(carrier)%>%       # grouping by carrier to show carrier stats
  summarise(mean_distance = mean(distance),
            sd_distance = sd(distance))
flight1


#--------------------------------------------------
# Question 3
#--------------------------------------------------


# From the output, the carriers with mean 0 and NA are AS, HA, F9, YV and OO


odd_carriers <- tibble(carrier = c("AS","HA", "F9", "OO", "YV"))


flights%>%
  filter(month == 1 )%>%
  semi_join(odd_carriers, join_by(carrier))%>%    # To leave desired carriers in tibble
  distinct(carrier, distance)                     # Shows the different values carriers have

#Showing how many entries of OO exist

flights%>%
  filter(carrier == "OO" & month == 1 )%>%
  select(carrier, month)

#----------------------------------------------------
# Question 4
#----------------------------------------------------

flights %>%
  group_by(month, carrier) %>%   # grouping by two variables so they appear in final output
  summarise(
    avg_dep_delay = mean(dep_delay, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = carrier, values_from = avg_dep_delay)

#----------------------------------------------------
# Question 5
#----------------------------------------------------


# tibble which meets conditions

flight_condition<-  flights%>%
  filter(dep_delay > 0 & arr_delay <= 0)

total_flights <- nrow(flights)           # number of total flights
met_condition <- nrow(flight_condition)  # number of flights which met condition

prop_of_flights <- met_condition / total_flights 
print(prop_of_flights)

            








































