library(tidyverse)
library(tidytuesdayR)
library(urbnthemes)
tuesdata <- tidytuesdayR::tt_load('2021-12-14')
tuesdata
studio_album_tracks <- tuesdata$studio_album_tracks
studio_album_tracks


ggplot(studio_album_tracks, aes(x = loudness, y = reorder(track_name,-loudness))) + geom_col() + facet_wrap(album_name~ ., scale="free_y") +
    geom_label(aes(label=key_mode), hjust = 0.2, nudge_x =  .5)

keys <- studio_album_tracks %>% count(key_mode, album_name, track_name, sort = TRUE) %>% rename(key_count= n )
keys

ggplot(keys, aes(x = key_mode, y = track_name, fill = album_name)) + geom_tile() + urbnthemes::theme_urbn_print() +
  labs(title = "Musical Keys in Spice Girls by Track and Album", subtitle = "TidyTuesday for week of 14th December 2021", 
       x=  "Key", y = "Track Name", caption = "Visualisation by Jeremy A\n https://github.com/jezzaayt")

ggsave(plot = last_plot(),  "spice_girls_keys.png", scale = 4.25)

