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

#-----------------------------------------------------
#  Question 6 (Part 1 and 2)
#-----------------------------------------------------


multi_routes <- flights %>%            
  group_by(origin, dest) %>%            # grouping allowing for count in the next line  
  filter(n_distinct(carrier) > 1) %>%   # filtering out non-repeat routes 
  ungroup()

avg_arr_delay <- multi_routes %>%
  group_by(origin, dest, carrier) %>%   
  summarise(
    arr_del_avg = mean(arr_delay, na.rm = TRUE),
    .groups = "drop"
  )

avg_arr_delay

#----------------------------------------------------
# Question 6 part 3
#----------------------------------------------------


# filter is used to keep the carrier with lowest delay time per route

avg_arr_delay %>% 
  group_by(origin, dest) %>%            
  filter(arr_del_avg == min(arr_del_avg, na.rm = TRUE)) %>%  
  ungroup()


#--------------------------------------------------
# Question 6 part 4
#--------------------------------------------------


avg_arr_delay %>%
  group_by(origin, dest) %>%
  summarise(
    best_delay  = min(arr_del_avg, na.rm = TRUE), # smallest delay by route
    worst_delay = max(arr_del_avg, na.rm = TRUE), # largest delay by route
    diff_del    = worst_delay - best_delay,        
    .groups = "drop"
  )%>%
  filter(diff_del == max(diff_del, na.rm =TRUE))  # retain only the largest delay


#---------------------------------------------------
# Question 6 part 5
#---------------------------------------------------

# Observing JFK ATL flights with their carriers

avg_arr_delay %>%
  filter(origin =="JFK", dest =="ATL")

# Filter flights for JFK → ATL and carriers 9E, DL, EV 

flights %>%
  filter(origin == "JFK", dest == "ATL", carrier %in% c("9E", "DL", "EV"))%>%
  group_by(carrier) %>%
  summarise(
    num_flights = n(),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    median_arr_delay = median(arr_delay, na.rm = TRUE),
    max_arr_delay = max(arr_delay, na.rm = TRUE),
    min_arr_delay = min(arr_delay, na.rm = TRUE),
    .groups = "drop"
  )           # obtaining flight data for assessment 




#---------------------------------------------------
#  Question 7
#---------------------------------------------------

# Storing into data frame

error_data <- structure(list(
  id = c("id_1", "id_2", "id_3", "id_4", "id_5", "id_6", "id_7", "id_8", "id_9", "id_10",
         "id_11", "id_12", "id_13", "id_14", "id_15", "id_16", "id_17", "id_18", "id_19", "id_20",
         "id_21", "id_22", "id_23", "id_24", "id_25", "id_26", "id_27", "id_28", "id_29", "id_30",
         "id_31", "id_32", "id_33", "id_34", "id_35", "id_36", "id_37", "id_38", "id_39", "id_40",
         "id_41", "id_42", "id_43", "id_44", "id_45", "id_46", "id_47", "id_48", "id_49", "id_50"),
  age = c(50L, 34L, 70L, 33L, 22L, 61L, 69L, 73L, 62L, 56L, 71L, 33L, 73L, 44L, 45L, 46L, 24L,
          70L, 46L, 76L, 47L, 76L, 28L, 48L, 54L, 27L, 45L, 26L, 61L, 28L, 38L, 55L, 33L, 36L,
          62L, 58L, 72L, 31L, 34L, 51L, 61L, 64L, 26L, 28L, 60L, 29L, 42L, 46L, 79L, 72L),
  gender = c("male", "male", "male", "female", "female", "male", "female", "male", "male", "female",
             "female", "male", "male", "female", "male", "male", "male", "male", "female", "male",
             "male", "male", "male", "female", "femal", "male", "female", "female", "female", "female",
             "male", "female", "female", "female", "male", "male", "female", "male", "female", "female",
             "male", "female", "female", "male", "male", "female", "male", "male", "male", "female"),
  height = c(174.4, 197.7, 174.1, 194.5, NA, 180.4, 170.5, 157.4, 196.8, 165.1,
             153, 197.4, 186, 157.1, 177.5, 197.7, 179.3, 170.2, 182.4, NA,
             165.4, 161, 168.5, 199.2, 157.7, 154.6, 157.1, 184.5, 181, 194.6,
             183.6, 186.9, 176.1, 183, 191.1, 189.3, 199, 172, 165.6, 170.5,
             150.5, 159.2, 192.1, 161.6, 162, 153.8, 162.3, 186.6, 192.4, 174.9),
  weight = c(69.4, 62.3, 55.6, 69.5, 78.6, 60.8, 72.2, 60.9, 75.1, 67.7,
             82.5, 68.7, 67.8, 76.7, 87, 61.1, 70.6, 63.3, 81.5, 59.2, 93.2,
             87.3, 83.4, 80.9, 68.6, 76.5, 93.7, 79.1, 92, 65.6, 85.4, 63.3,
             79.7, 74.1, 63.3, 78.2, 95.7, 95.1, 63.7, 66.1, 99.3, 81, 96.9,
             73.3, 70.3, 83, 57.6, 78.6, 61.9, 98.1),
  disease_status = c("diseased", "healthy", "healthy", "healthy", "healthy", "healthy", "diseased",
                     "healthy", "diseased", "Healthy", "diseased", "healthy", "diseased", "healthy",
                     "diseased", "healthy", "healthy", "healthy", "healthy", "healthy", "healthy",
                     "diseased", "healthy", "diseased", "healthy", "healthy", "healthy", "healthy",
                     "diseased", "diseased", "healthy", "healthy", "healthy", "diseased", "diseased",
                     "diseased", "healthy", "diseased", "healthy", "healthy", "healthy", "healthy",
                     "healthy", "diseased", "diseased", "diseased", "healthy", "healthy", "diseased",
                     "diseased"),
  glucose = c(96, 78, 101, 119, 103, 91, 86, NA, 77, 80, 115, 85, 88, 109, NA, 71, 90, 94, 91,
              87, 113, 93, 97, 118, 109, 80, 85, 119, 99, 108, 89, 108, 97, 116, 79, 84, 75, 81,
              119, NA, 106, 109, 75, 82, 84, 75, 76, 120, 119, 77)
), row.names = c(NA, -50L), class = c("tbl_df", "tbl", "data.frame"))

# This viewing option shows details of the columns

UtilsDataRSV::view_cols(error_data)
















