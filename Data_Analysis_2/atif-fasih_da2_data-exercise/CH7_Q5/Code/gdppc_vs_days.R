
library(tidyverse)


# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
#install.packages("lspline")
library(lspline)
# Estimate robust SE
#install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
#install.packages("texreg")
library(texreg)
# For different themes
#install.packages(ggthemes)
library(ggthemes)
# Non parametric Regression

ggplot(df, aes(x = business_days, y = gdppc)) + geom_point() + scale_y_continuous() + scale_x_continuous()


  
ggplot(df, aes(x=business_days,y=gdppc)) + 
  stat_summary_bin(fun='mean', bins = 4, color='red', size=2, geom='point')

# Lowess
ggplot(df, aes(x=business_days,y=gdppc)) + geom_point() + geom_smooth(method = "loess") 

# log transformation
ggplot(df, aes(x = business_days, y = gdppc)) + geom_point() +  scale_y_continuous( trans = log_trans()) + 
  scale_x_continuous( trans = log_trans(),breaks = c(5,10,20,40,80,160))

df <- df %>% mutate( ln_gdppc = log( gdppc ),
                     ln_bus_days = log( business_days ) )


# Bin summary 4 bins + loess

ggplot(df, aes(x= ln_bus_days,y=ln_gdppc)) + stat_summary_bin(fun='mean', bins = 4, color='red', size=2, geom='point')+
  geom_smooth(method = "loess")  

scatterplot + loess
ggplot(df, aes(x= ln_bus_days,y=ln_gdppc)) + geom_point() 
+ geom_smooth(method = "loess") 


#Linear Regression

reg_linear <- lm_robust(ln_gdppc ~ ln_bus_days, data = df)
summary(reg_linear)


ggplot( data = df, aes( x = ln_bus_days, y = ln_gdppc )) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )







