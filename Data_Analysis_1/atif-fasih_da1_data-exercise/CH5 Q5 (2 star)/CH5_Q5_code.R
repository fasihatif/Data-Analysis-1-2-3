library(tidyverse)
# Import raw data
data_in <- "C:/Users/abc/OneDrive/Business_Analytics/DA1-BA/Data_Analysis_1_Data_Exercise/atif-fasih_da1_data-exercise/CH5 Q5 (2 star)/"
co2_per_capita <- read_csv(paste0(data_in,"CO2_per_capita.csv"))

# Import gdp_per_capita
data_in <- "C:/Users/abc/OneDrive/Business_Analytics/DA1-BA/Data_Analysis_1_Data_Exercise/atif-fasih_da1_data-exercise/CH5 Q5 (2 star)/"
gdp_per_capita <- read_csv(paste0(data_in,"GDP_per_capita.csv"))

# Rename columns
co2_per_capita <- rename(co2_per_capita, 'country' = 'Country Name' ,'co2' = '2016')
gdp_per_capita <- rename(gdp_per_capita, 'country' = 'Country Name' , 'gdp' = '2016')

# Merge the two tables
gdp_co2_data <- merge(gdp_per_capita,co2_per_capita, by = 'country')

# Sort table by GDP
gdp_co2_data <- gdp_co2_data %>% arrange(gdp)

# Remove countries missing values
gdp_co2_data <- gdp_co2_data[complete.cases(gdp_co2_data),]

# Calculate median for splitting into 2 groups
median(gdp_co2_data$gdp)

# Split into 2 groups 
gdp_co2_data <- gdp_co2_data %>%
mutate('group' = ifelse(gdp >= 5555.335, 'group2','group1'))

# No of observations in each group
table(gdp_co2_data$group)


# calculate means of each group
group1_mean <- mean(gdp_co2_data$co2[gdp_co2_data$group == 'group1'])
group2_mean <- mean(gdp_co2_data$co2[gdp_co2_data$group == 'group2'])

# Calculate the difference in mean of the 2 groups
mean_diff <- group2_mean - group1_mean

# Bootsrap

set.seed(13456)
n_group1 <- 119
n_group2 <- 119

# No of boostrap samples
B <- 10000

# Convert Boot-sample into a column

boot_group1 <- matrix(sample(gdp_co2_data$co2[gdp_co2_data$group == 'group1'],
size = B*n_group1,
replace = TRUE),ncol = B,
nrow = n_group1)

boot_group2 <- matrix(sample(gdp_co2_data$co2[gdp_co2_data$group == 'group2'],
size = B*n_group2,
replace = TRUE),ncol = B,
nrow = n_group2)

dim(boot_group1);dim(boot_group2)

# Calculate the difference in MEANS for each of the bootsamples
Boot_diff_in_means <- colMeans(boot_group1) - colMeans(boot_group2)

# The "PERCENTILE" bootstrap confidence interval
quantile(Boot_diff_in_means, prob = 0.025)
quantile(Boot_diff_in_means, prob = 0.975)

