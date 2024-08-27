library(tidyverse)
library(tm)
library(tidytext)
library(ggthemes)


power_rangers_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-27/power_rangers_episodes.csv')

# copy desc column into a description column

power_rangers_episodes <- power_rangers_episodes %>%
  mutate(description = desc)

# convert to lower case

# tokenize the description column

tokens <- power_rangers_episodes %>%
  unnest_tokens(word, description)

# remove stop words

pr <- tokens %>%
  anti_join(stop_words)


count_pr <- pr %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) 
# list all season titles by number of word occurrences

countwords <- pr %>% 
  anti_join(stop_words) %>% 
  count(word, season_title, sort = TRUE) %>% 
  group_by(season_title) %>% 
  summarise(total_words = sum(n)) 


pr %>% 
  anti_join(stop_words) %>% 
  count(word, season_title, sort = TRUE) %>% 
  filter(n > 8) %>% 
  ggplot(aes(reorder(word,n), n,fill = season_title)) + 
  geom_col() + 
  coord_flip()  +
  facet_wrap(~factor(season_title,levels=c("Mighty Morphin (Season 1)","Mighty Morphin (Season 2)", "Dino Thunder",
"Time Force", "S.P.D.", "Operation Overdrive","Zeo", "Jungle Fury","Mighty Morphin (Season 3)","Turbo","Mystic Force",
"In Space","Ninja Storm", "Wild Force","R.P.M.", "Lost Galaxy","Lightspeed Rescue", "Samurai",
"Megaforce", "Dino Super Charge", "Dino Charge", "Ninja Steel", "Super Samurai", "Super Megaforce", 
"Beast Morphers (Season 2)", "Beast Morphers (Season 1)", "Super Ninja Steel")), scales = "free_y") +
  labs(title = "Count of words greater than 8 times within the description per episode", 
subtitle = "TidyTuesday 27th August 2024\nVisualisation By https://github.com/jezzaayt", 
y = "Number of word occurrences", x ="Word in description of episode", fill = "Season Title") +
  theme_clean()

ggsave("PR.png", units)





