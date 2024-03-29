library(tidytuesdayR)
library(tidyverse)
library(ggrepel)
library(patchwork)
library("viridis")
library(tidytext)
library(ggpubr)
tuesdata <- tidytuesdayR::tt_load('2021-12-07')

spiders <- tuesdata$spiders
table(spiders$distribution)
family_count <- spiders %>% count(family, sort = TRUE)
family_count <- head(family_count, 20 )

fs <- ggplot(family_count, aes ( x = reorder(family, -n), y = n, fill = family, label = n)) + geom_col() +
  geom_label(colour="white") +labs(fill = "Family", x = "Family Name", y = "Count per Family", title = "Top 20 Spider Family Names with Counts" ) + 
  scale_fill_viridis(option="D", discrete = TRUE) + scale_x_discrete(guide=guide_axis(n.dodge=2))+ theme_pubr()
fs
spiders
species <- spiders %>% count(species, family , sort = TRUE)
species <- head(species, 100)
species
ggplot(species, aes ( x = reorder(family, -n), y = n, fill = species, label = n)) + geom_col()

ss<- species%>% drop_na() %>% mutate(species = reorder_within(species, n, family)) %>% ggplot(aes( x = reorder(species, n), y = n, fill = family , label = n)) + geom_col() + 
  scale_y_reordered() + facet_wrap( ~family, scales="free_x", nrow = 7)+
  geom_label(vjust = 2, colour="white") +labs(fill = "Family", x = "Species Name", y = "Count per Species", title = "Top 100 Spider Species per Family with Counts" ) +
  scale_x_discrete(guide=guide_axis(n.dodge=3))  + scale_fill_viridis(option="E", discrete = TRUE) + theme_pubr()

ss
width = 15
height = (9/16) * width

fs + ss + plot_annotation(title = "World Spider Database\nTidyTuesday 7th December 2021", caption = "Visualisation by Jeremy A")
ggsave("spiders.png", width = width, height = height, scale = 2)
