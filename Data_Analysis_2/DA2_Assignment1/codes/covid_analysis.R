# Packages to use
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
# For table transformations
library(data.table)
# For table creations
library(knitr)

# Set working directory
setwd('C:/Users/abc/OneDrive/Business_Analytics/Data-Analysis-1-2-3/Data_Analysis_2/DA2_Assignment1/data/clean/')

# Assign imported data to df_clean which acts as our backup
df_clean <- read_csv('covid_pop_09_24_2020_clean.csv')

# We select relevant columns for analysis
df <- df_clean %>% select(country,confirmed,death,population)

### Data Transformation

# For possible log transformations, we remove rows with '0' values
df <- df %>% filter(death != 0)

# Divide population by 100,000 for easier measurements and interpretation
df <- df %>% mutate(population = (population/1000000)) 

# Finding deaths per capita 
df <- df %>% mutate(deaths_per_capita = round((death / population),digits = 2))

# Finding confirmed cases per capita
df <- df  %>% mutate(cases_per_capita = round((confirmed / population),digits = 2))

df <-  df %>% select(country,death, confirmed,cases_per_capita, deaths_per_capita, population)

df_histogram <- df


kable(head(df,3))


## **Summary statistics for X and Y variables**

summary_cases_per_capita <- df %>% summarise(
  mean     = mean(cases_per_capita),
  median   = median(cases_per_capita),
  std      = sd(cases_per_capita),
  min      = min(cases_per_capita),
  max      = max(cases_per_capita))

summary_deaths_per_capita <- df %>% summarise(
  mean     = mean(deaths_per_capita),
  median   = median(deaths_per_capita),
  std      = sd(deaths_per_capita),
  min      = min(deaths_per_capita),
  max      = max(deaths_per_capita))
 
summary_population <- df %>% summarise(
  mean     = mean(population),
  median   = median(population),
  std      = sd(population),
  min      = min(population),
  max      = max(population))

summary_stats2 <- summary_cases_per_capita %>% add_row(summary_deaths_per_capita) %>% add_row(summary_population)

row.names(summary_stats2) <- c("cases_per_capita", "deaths_per_capita", "population")

# Histogram visualization
df_histogram %>%
  select(cases_per_capita,deaths_per_capita) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()


#Preview table
kable(summary_stats2)


## **Investigation of the transformation of our variables**

# Level - Level
ggplot( df , aes(x = cases_per_capita, y = deaths_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Registered cases per capita (2019)",y = "Deaths per capita (2019)") 

# level - Log
ggplot( df , aes(x = cases_per_capita, y = deaths_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Registered cases per capita (2019), ln scale",y = "Deaths per capita (2019)")  +
  scale_x_continuous( trans = log_trans(), breaks = c(1,2,5,10,20,50,100,200,500 ,1000,5000, 15000,30000) )

# Log - Level
ggplot( df , aes(x = cases_per_capita, y = deaths_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Registered cases per capita (2019), ln scale",y = "Deaths per capita (2019)")  +
  scale_y_continuous( trans = log_trans(), breaks = c(1,10,100,250,500,1000))

# Log - Log
ggplot( df , aes(x = cases_per_capita, y = deaths_per_capita))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Total registered cases per capita (2019),ln scale",y = "Total deaths per capita (2019), ln scale")  +
  scale_x_continuous( trans = log_trans(),   breaks = c(1,2,5,10,20,50,100,200,500,1000,7000,15000,30000) )+
  scale_y_continuous( trans = log_trans() )


# Take Log of death/capita and log cases/capita total
df <- df %>% mutate( ln_deaths_per_capita = log( deaths_per_capita ),
                     ln_cases_per_capita = log( cases_per_capita) )


df <- df %>% mutate( ln_cases_per_capita_sq = ln_cases_per_capita^2)

## Estimating different Regression models

# Linear Regression
reg_linear <- lm_robust( ln_deaths_per_capita ~ ln_cases_per_capita , data = df , se_type = "HC2" )

#Linear Regression Visualization**

ggplot( data = df, aes( x = ln_cases_per_capita, y = ln_deaths_per_capita ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' ) +
  ggtitle("Linear Regression") +
  labs(x = "Registered cases per capita per million, ln scale",y = "Deaths per capita per million,ln scale")

# Quadratic Regression
reg_quad <- lm_robust( ln_deaths_per_capita ~ ln_cases_per_capita + ln_cases_per_capita_sq , data = df )

# Quadratic Regression Visualization**

ggplot( data = df, aes( x = ln_cases_per_capita, y = ln_deaths_per_capita ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' ) +
  ggtitle("Quadratic Regression") +
  labs(x = "Registered cases per capita per million, ln scale", y = "Deaths per capita per million,ln scale")


# Regression with piecewise linear spline
# 1st define the cutoff for gdp per capita
cutoff <- c(4000,7500,25000)
# 2nd we use a log transformation -> cutoff needs to be transformed as well
cutoff_ln<- log( cutoff )
# Use simple regression with the lspline function
reg_pls <- lm_robust(ln_deaths_per_capita ~ lspline( ln_cases_per_capita , cutoff_ln ), data = df )

# Piecewise Linear Spline Regression
ggplot( data = df, aes( x = ln_deaths_per_capita, y = ln_cases_per_capita ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'red' ) +
  ggtitle("Piecewise Linear Spline Regression") +
  labs(x = "Registered cases per capita per million, ln scale",y = "Deaths per capita per million,ln scale")


# Weighted OLS Regression
reg_wgt_ols <- lm_robust(ln_deaths_per_capita ~ ln_cases_per_capita, data = df , weights = population)
# Weighted OLS Regression Visualization

ggplot(data = df, aes(x = ln_cases_per_capita, y = ln_deaths_per_capita)) +
  geom_point(data = df, aes(size=population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='red') +
  ggtitle("Weighted OLS Regression") + 
  labs(x = "Registered cases per capita per million, ln scale",y = "Deaths per capita per million,ln scale") +
  scale_size(range = c(1, 15)) +
  annotate("text", x = 4.0, y = 1, label = "China", size=5)+
  annotate("text", x = 8.5, y = 3.5, label = "India", size=5)+
  annotate("text", x = 10.5,  y = 7, label = "United States", size=5)


# Creating model summary with texreg
data_out <- "C:/Users/abc/OneDrive/Business_Analytics/DA1-BA/Assignment_2/out/"
htmlreg( list(reg_linear,reg_quad,reg_pls,reg_wgt_ols),
         type = 'html',
         custom.model.names = c("cases/capita - linear","cases/capita - quadratic",
                                "cases/capita - pls", "cases/capita- weighted linear"),
         caption = "Modelling deaths per capita and confirmed cases per capita",
         file = paste0( data_out ,'model_comparison1.html'), include.ci = FALSE)


## Residual Analysis


# Get the predicted y values from the model
df$reg_linear_y_pred <- reg_linear$fitted.values
# Calculate the errors of the model
df$reg_linear_res <- df$ln_deaths_per_capita - df$reg_linear_y_pred

# Find countries with largest negative errors
df %>% top_n( -5 , reg_linear_res ) %>% 
      select( country ,death, ln_deaths_per_capita , reg_linear_y_pred, reg_linear_res )

# Find countries with largest positive errors
df %>% top_n( 5 , reg_linear_res ) %>% 
       select( country, death,  ln_deaths_per_capita , reg_linear_y_pred, reg_linear_res )

## Testing hypothesis

#We test the following hypothesis:

#H0 - B= 0 : There is no relationship between ln\_deaths\_per\_capita and ln\_cases\_per\_capita

#HA - B != 0 : There is a relationship between between ln\_deaths\_per\_capita and ln\_cases\_per\_capita



# 1) Coefficient is equal to 0:
summary( reg_linear)

# We will use a significant value of 0.05 to check whether we can reject null hypothesis. Our results show that we received an estimated p value of 5.255e-48 which is very close to zero and t value of 20.66 which is greater than 2. Since the p value is less than 0.05, we reject null hypothesis and conclude that there is a relationship between between ln\_deaths\_per\_capita and ln\_cases\_per\_capita.

