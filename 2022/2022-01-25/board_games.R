library(tidyverse)
library(ggpubr)
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')
df <- merge(ratings,details, by=c("id"))


df_ <- df %>%filter (year >=1990 & year <= 2022) %>% group_by(year)%>% summarise(bayes = mean(bayes_average) ,user_rated  = mean(users_rated))

ggplot(df_, aes(x=year, y = bayes))+ geom_point(aes(size=user_rated, color=user_rated))  + geom_smooth() +theme_pubr() + 
  labs(title="Bayes Average of Board games since 1990 to present",subtitle = "Average of the Bayes Average for per year basis same for the users rating", x = "Year", y = "Bayes Average", 
       caption = "TidyTuesday week 25th January 2022\nData: Board Games Geek/Kaggle\nBy:Jeremy A\n@Jezzaayt", size="Users Rated") + 
  scale_y_continuous(breaks = seq(from = 5.5,to = 5.8, by = 0.025))+
  scale_x_continuous(breaks = seq(1990, 2022, 2)) + geom_curve(aes( x  = 2020, y = 5.54, xend = 2022, yend = 5.523 ), arrow = arrow(length = unit(0.2,"cm"))) + 
  annotate("text", x = 2020, y = 5.545, label = "Lowest Bayes Average: 5.52\n90 average users rated")+
  geom_curve(x = 2019.3, y = 5.785, xend = 2019, yend = 5.766, arrow=arrow(length = unit(0.2,"cm")))+
  annotate("text", x = 2021, y = 5.78, label = "Highest Bayes Average: 5.76\n\n676 average users rated")  + guides(color = "none")
