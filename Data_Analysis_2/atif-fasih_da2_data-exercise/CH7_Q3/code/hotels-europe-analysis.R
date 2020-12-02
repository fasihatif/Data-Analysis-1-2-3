
library(tidyverse)
library(ggthemes)

df <- read_csv(paste0(dir,"/clean/hotels-europe_clean.csv"))

df_bp_filter <- df %>% filter(city_actual == "Budapest" &
                                accommodation_type == "Hotel" &
                                weekend == 0 & 
                                year == 2017 & 
                                month == 11 & 
                                stars %in% c(3.0,3.5,4.0))

df_bp <- df_bp_filter %>% select(city_actual,price,distance,stars)

# Examining the distribution of the distance variable

df_bp %>%  ggplot(aes(distance)) + 
  geom_histogram(color = 'black') + 
  theme_wsj() + scale_fill_wsj() + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6))

df_bp <- df_bp %>% mutate(dist_category = ifelse(distance < 2,"close","far"))
df_bp <- df_bp %>% mutate(dist_category = factor(dist_category))
                          
df_bp %>% group_by(dist_category) %>%
  summarise("mean_price" = mean(price)) %>%
  ggplot(aes(dist_category, mean_price)) + geom_point()

                          
ggplot(df_bp, aes(x=distance,y=price)) + 
  stat_summary_bin(fun='mean', bins = 4, color='orange', size=2, geom='point' ,breaks = c(0,1,2,3,6))
                          

ggplot(df_bp, aes(x=distance,y=price)) + 
  geom_point(alpha = 0.4,) + 
  stat_summary_bin(fun.y='mean', bins = 4, color='orange', size=2, geom='point' ,breaks = c(0,1,2,3,6))



ggplot(df_bp, aes(x=distance,y=price)) + 
  geom_point(alpha = 0.4,) +
  geom_smooth(method = "loess")

ggplot(df_bp, aes(x=distance,y=price)) + 
  geom_point() +
  geom_smooth(method = "loess")

# Linear Regression

df_bp_reg <- lm(price ~ distance, data = df_bp)
summary(df_bp_reg)

ggplot( data = df_bp, aes( x = distance, y = price) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' ) +
  ggtitle("Linear Regression") +
  labs(x = "Distance to city center (Miles)",y = "Price($)")


 df_histogram %>%
  select(cases_per_capita,deaths_per_capita) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()