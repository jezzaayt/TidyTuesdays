library(tidyverse)
library(patchwork)
library(ggpubr)
tuesdata <- tidytuesdayR::tt_load('2021-05-25')


drivers <- tuesdata$drivers
records <- tuesdata$records

alpha_value = 0.5
size_value = 0.3


all <- ggplot(records, aes(x = track, y = time, fill = track), ) + geom_boxplot(alpha=0.5) + 
  geom_jitter(alpha = alpha_value, size=size_value) + coord_flip() + labs(title = "Mario Kart 64 World Records Track times",
                                                           y = "Time (seconds)", x = "Track") +
  scale_y_continuous(breaks = seq (0,400, 20)) + theme_classic2() + theme_cleveland()

records_shortcuts <- filter(records, shortcut == "Yes")
records_shortcuts


shortcuts <- ggplot(records_shortcuts, aes(x = track, y = time, fill = track)) + geom_boxplot(alpha=0.5) + 
  geom_jitter(alpha = alpha_value, size=size_value) + coord_flip() + labs(title = " including shortcuts only",
                                                           y = "Time (seconds)", x = "Track")+
  scale_y_continuous(breaks = seq (0,400, 20)) + theme_classic2() +theme_cleveland()


all
shortcuts
all2 <- all+ labs(title="", y = "")
all2 / shortcuts + plot_annotation(title = "Mario Kart 64 World Records Track Times", 
                                   subtitle = "Data originally from MKWRS.com - Visualisation TidyTuesday 25/05/2021",
                                   caption = "Visualisation by: @Jezzaayt | https://github.com/jezzaayt  ")
ggsave(filename = "mario_times.png" , last_plot(), scale = 1.5)
