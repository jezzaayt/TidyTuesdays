library(tidyverse)
library(gghalves)
library(ggdist)
library(ggthemes)
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')


unique(drought$state_abb)
df <- drought %>% filter(state_abb == "TX")

df <- df %>% filter(map_date >= 20210101)
df

df$month <- months(df$valid_end)
df


max_drought <- aggregate(df$area_pct, by= list(df$drought_lvl), max )
min_drought <- aggregate(df$area_pct, by= list(df$drought_lvl), min )
max_drought

min_drought
ggplot(df, aes(x=drought_lvl, y= area_pct, fill=drought_lvl))+ stat_halfeye() + 
  geom_half_point(side= 1) +labs(title = "Drought Area Percentage levels in Texas 2021", 
                                 subtitle = "TidyTuesday July 20th 2021\nData from U.S Drought Monitor", 
                                 caption="Vizusalistion by Jeremy Aytoun", y = "Percentage of state in drought", 
                                 x = "Drought Levels", fill="Drought Levels") +theme_calc() +
  annotate("text" , label = "27.84", x = 1, y = 46) +
  annotate("segment", x = 1, xend = 1, y = 28, yend = 40, color = "#f8766d")+ 
  annotate("text" , label = "35.84", x = 2, y = 46) +
  annotate("segment", x = 2, xend = 2, y = 36, yend = 40, color = "#a3a500")+
  annotate("text" , label = "18.69", x = 3, y = 46) +
  annotate("segment", x = 3, xend = 3, y = 20, yend = 40, color = "#39b600") +
  annotate("text" , label = "15.77", x = 4, y = 46) +
  annotate("segment", x = 4, xend = 4, y = 20, yend = 40, color = "#00c1aa") + 
  annotate("text" , label = "9.74", x = 5, y = 46) +
  annotate("segment", x = 5, xend = 5, y = 10, yend = 40, color = "#00abfd") + 
  annotate("text" , label = "91.38", x = 6, y = 105) +
  annotate("segment", x = 6, xend = 6, y = 90, yend = 98, color = "#DC71FA") + # end of max
  annotate("text" , label = "3.28", x = 1, y = -3 ) +
  annotate("text" , label = "3.33", x = 2, y = -5) +
  annotate("text" , label = "1.16", x = 3, y = -5) +
  annotate("text" , label = "0.00", x = 4, y = -5) +
  annotate("text" , label = "0.00", x = 5, y = -5) +
  annotate("text" , label = "8.22", x = 6, y = -3) 

ggsave("drought_level.png")
