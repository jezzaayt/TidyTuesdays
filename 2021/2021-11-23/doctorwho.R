install.packages("tidyverse")
library(tidyverse)
library(ggpubr)

tuesdata <- tidytuesdayR::tt_load('2021-11-23')
tuesdata
imdb <- tuesdata$imdb
imdb
ggplot(imdb, aes(x = ep_num, y = rating, fill = rating)) + geom_col() +
  facet_wrap(. ~season) + scale_x_continuous(breaks = round(seq(min(imdb$ep_num), max(imdb$ep_num), by = 1), 1))+
  scale_y_continuous(breaks = round(seq(min(0), max(10), by = 1), 1)) +
  scale_fill_gradient(low ="#FF0000", high = "#0000FF") + theme_cleveland() + 
  labs(title = "Doctor Who Ratings per season", x="Episode Number", y = "Ratings", caption = "Jeremy Aytoun\nTidyTuesday 23/11/2021")


ggsave("doctor_who_ratings.png", last_plot(), scale = 5)
