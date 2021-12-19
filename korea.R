# World Bank GDP and CO2 Data

library(dplyr)
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
library(lubridate)
library(ggpubr)

options(scipen=1)
a <- read_csv("data/korea.csv", na = "..") %>%
  clean_names() %>%
  select(-series_code, -country_name, -country_code) 


long <- a %>% pivot_longer(x1960_yr1960:x2020_yr2020,
                           names_to="year",
                           values_to="value")

long$year <- substr(long$year, 2,5)

gdp <- long %>% filter(series_name=="GDP (current US$)") %>%
  mutate(value=value/1000000)
co2 <- long %>% filter(series_name=="Total greenhouse gas emissions (kt of CO2 equivalent)") 

gdp_plot <- ggplot(gdp, aes(x=year, y=value)) + 
  geom_line(group=1) +
  theme_linedraw() + 
  labs(x="", y="million USD$",
       title="South Korea's GDP") +
  scale_x_discrete(breaks=seq(1960, 2020, 5)) 

co2_plot <- ggplot(co2, aes(x=year, y=value)) + 
  geom_line(group=1, color="blue") +
  theme_linedraw() + 
  labs(x="", y="kt of CO2 equivalent",
       title="Total GHG emissions") + 
  scale_x_discrete(breaks=seq(1960, 2020, 5)) 

t <- ggarrange(gdp_plot, co2_plot, ncol=1)

ggsave("korea.png")
