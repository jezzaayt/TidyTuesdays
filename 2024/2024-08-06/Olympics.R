library(tidyverse)
library(plotly)
olympics <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-06/olympics.csv") # nolint: line_length_linter.
olympics

unique(olympics$event)

df <- olympics %>%
  filter(!str_detect(event, ",|team")) %>%
  filter(across(everything(), ~!is.na(.))) %>%
  filter(year >= 2000)

df <- df %>%
  mutate(team = str_replace_all(team, "-|2|1", "")) %>%
  mutate(team = str_trim(team))
#Split the games between summer and winter games and count by medals won by team in df # nolint

df_summer <- df %>%
  filter(str_detect(season, "Summer"))
df_winter <- df %>%
  filter(str_detect(season, "Winter"))

# count the total number of medals won by team in each game total
# medals is not a column count in the medal column which has strings
df_summer <- df_summer %>%
  group_by(year, team) %>%
  summarise(total_medals = sum(str_count(medal, "Gold|Silver|Bronze")))
df_winter <- df_winter %>%
  group_by(year, team) %>%
  summarise(total_medals = sum(str_count(medal, "Gold|Silver|Bronze")))
# Define Olympic colors
olympic_colors <- c("#0081C8", "#FCB131", "#000000", "#00A651", "#EE334E")

# make two ggplots with df summer and winter games
ggplot(df_summer, aes(x = year, y = total_medals, color = team)) +
  geom_line() +
  labs(title = "Total Medals Won by Team for Summer Games (2000-2016)",
       x = "Year", y = "Total Medals", color = "Team")

#create a ggplot with no theme
# Create a scatter plot for the summer games data frame
ggplot(df_summer, aes(x = year, y = total_medals, color = team)) +
  geom_point() +
  geom_line()+
  labs(title = "Total Medals Won by Team for Summer Games (2000-2016)",
       x = "Year", y = "Total Medals", color = "Team")

# Create a scatter plot for the winter games data frame
ggplot(df_winter, aes(x = year, y = total_medals, color = team)) +
  geom_point() +
  labs(title = "Total Medals Won by Team for Winter Games (2000-2016)",
       x = "Year", y = "Total Medals", color = "Team")
