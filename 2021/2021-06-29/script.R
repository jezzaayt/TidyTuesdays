library(tidyverse)
library(hrbrthemes)
library(utils)
library(lubridate)
library(ggrepel)
library(ggthemr)
#TidyTuesday 26-06-29
animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')


head(animal_rescues)
animal_rescues$animal_group_parent

# count by type of animal per year 
# remove unknowns 


df <- with(animal_rescues, subset(animal_rescues, subset = !grepl(glob2rx("Unknown*"), animal_rescues$animal_group_parent)))
unique(df$animal_group_parent)

df$animal_group_parent[df$animal_group_parent == "cat"] <- "Cat"
df$animal_group_parent[df$animal_group_parent == "Budgie"] <- "Bird"
df$animal_group_parent[df$animal_group_parent == "Pigeon"] <- "Bird"
df <- df %>% mutate(across(where(is.character),toupper))
df$date_time_of_call


# convert cal year to date 
df$years <- ymd(df$cal_year, truncated= 2L)
year(df$years)
class(df$years)
df
df <- df %>% group_by(cal_year, years, animal_group_parent) %>% add_count()
df_tally<-  df %>% group_by(cal_year, animal_group_parent) %>% count(name = "tally_animal_by_borough_year") 
df_tally  <- na.omit(df_tally) 
df_tally <- df_tally %>% filter(cal_year >= 2020) 
df_tally <- df_tally
df_tally$tally_animal_by_borough_year

unique(df_tally$borough)
df_tally$cal_year <- as.character(df_tally$cal_year)
as.character(df_tally$cal_year)
class(df_tally$cal_year)

ggthemr("solarized")

ggplot(df_tally, aes(animal_group_parent, tally_animal_by_borough_year, fill = cal_year, label=tally_animal_by_borough_year)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip()+ labs(title="Animals rescues in London boroughs during 2020 and 2021", subtitle="Data: London.Gov\nTidyTuesday 29th June 2021",
                     x="Animal type", y="Number of animals rescued", fill = "Year",
                     caption= "Vizualization by Jeremy Aytoun") +
  geom_label_repel(position=position_nudge_repel())

ggsave("animal_rescues.png", scale= 2.5)

df_tally$tally_animal_by_borough_year
df_tally
