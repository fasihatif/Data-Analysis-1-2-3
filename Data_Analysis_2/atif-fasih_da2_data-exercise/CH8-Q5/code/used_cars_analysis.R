library(rvest)
library(tidyverse)
library(data.table)

database <- read_html('https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_2/atif-fasih_da2_data-exercise/CH8-Q5/data/used_cars_ford.csv')

###################################################################################
# Checking scatterplots for Transformation of variables
###################################################################################

# Level - Level
database %>%
  ggplot(aes(x = age, y = car_price)) + geom_point() +geom_smooth(method = 'loess')

# log - log
database %>%
  ggplot(aes(x = age, y = car_price)) + geom_point() +geom_smooth(method = 'loess') + 
  scale_y_continuous(trans = log_trans()) + scale_x_continuous(trans = log_trans()) 

# level - log
database %>%
  ggplot(aes(x = age, y = car_price)) + geom_point() +geom_smooth(method = 'loess') + 
  scale_y_continuous(trans = log_trans(),breaks = c(0,2500,5000,10000,15000,20000,25000)) 

### Level log shows best pattern of association

#################################################################
# Convert price to log price
#################################################################

database <- database %>% mutate( ln_price = log( car_price ),
                                 age_sq = age*age)

#################################################################
# Regression Analysis
#################################################################

# Simple Linear Regression
linear_reg <- lm_robust( ln_price ~ age , data = database , se_type = "HC2" )
linear_reg
ggplot( data = database, aes( x = age, y = ln_price ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

# Quadratic Regression
quad_reg <- lm_robust( ln_price ~ age + age_sq , data = database )
summary( quad_reg )
ggplot( data = database, aes( x = age, y = ln_price) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

# Piecewise linear Spline Regression

cutoff <- 8

# Use simple regression with the lspline function
lspline_reg <- lm_robust(ln_price ~ lspline( age, cutoff ), data = database )
summary( lspline_reg  )
ggplot( data = database, aes( x = age, y = ln_price ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff) , method = lm , color = 'red' )


data_out <- 'C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_2/atif-fasih_da2_data-exercise/CH8-Q5/docs/'
  
htmlreg( list(linear_reg , quad_reg , lspline_reg),
         type = 'html',
         custom.model.names = c("ln_price - linear","ln_price - quadratic",
                                "ln_spline - PLS"),
         caption = "Modelling Price vs Age of Cars",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)



#################################################################
# Residual analysis.
#################################################################

# Get the predicted y values from the model
database$linear_reg_y_pred <- linear_reg$fitted.values
# Calculate the errors of the model
database$linear_reg_res <- database$ln_price - database$linear_reg_y_pred 

# Find countries with largest negative errors
best_deal <- database%>% top_n( -5 , linear_reg_res ) %>% 
  select( model_name , make_year , car_price, ln_price,linear_reg_y_pred , linear_reg_res )

library(knitr)
kable(best_deal)

# Find countries with largest positive errors
database %>% top_n( 5 , linear_reg_res ) %>% 
  select( model_name , make_year , car_description, car_colour,location, car_price, ln_price, linear_reg_y_pred , linear_reg_res )





