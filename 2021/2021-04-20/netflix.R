library(tidyverse)
library(janitor)
library(gganimate)
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')


head(netflix_titles)
# Remove NA 
netflix <- na.omit(netflix_titles)

head(netflix)

as.data.frame(table(netflix$release_year))
#looking at the data
netflix_movie <- netflix %>% filter(type == "Movie")
netflix_tv <- netflix %>% filter(type == "TV Show")
netflix_18 <- netflix %>% filter(rating == "R") # No tv shows

netflix
d<- netflix %>% get_dupes(director)
tail(d)

netflix %>% group_by(release_year) %>% add_tally()


net_dup <- netflix %>% group_by(release_year, type) %>% add_tally(name = "count_per_release_year") %>%
  filter(release_year >= 2010) 
net_ani <- ggplot(net_dup, aes(x = release_year, y= count_per_release_year, label = count_per_release_year ) )+ 
  geom_bar(stat = "identity", position="dodge", fill = "#E50914") +
  #geom_text_repel() + 
  geom_text(aes(label= as.character(count_per_release_year)), position=  position_dodge(width =1), vjust = -0.5 ) +
  scale_x_continuous(labels=as.character(net_dup$release_year), breaks=net_dup$release_year) +
  theme_minimal() + facet_wrap(~type) + labs(title = "Total Number of Netflix Movies/TV Shows released from 2010 onwards",
                                             y = "Releases Per Year ", 
                                             caption= "Data: https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-04-20 \n By: @JezzaAyt | https://github.com/jezzaayt/") + transition_time(release_year) +
  shadow_mark() 

animate(net_ani, fps = 25, duration = 30, width = 700, height = 500, end_pause = 10)

anim_save("netflix_gif.gif", animation = last_animation())
