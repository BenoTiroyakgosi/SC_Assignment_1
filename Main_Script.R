# Author : Beno Tiroyakgosi
# Date   : 07 February 2026
# Subject: Statistical Computing Practical 1 


#==============================================
#  Load Packages
#==============================================

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





