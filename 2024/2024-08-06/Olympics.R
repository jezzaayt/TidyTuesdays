library(tidyverse)
library(ggtext)
library(magick)
library(showtext)
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext_auto(enable = TRUE)
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

# filter the both summer and winter dataframes by the top 20 medal winners

top_summer <- df_summer %>%
  group_by(year, team) %>%
  summarise(total_medals = sum(total_medals)) %>%
  arrange(desc(total_medals)) %>%
  slice(1:9)
top_summer



theme_set(theme_bw(base_size = 7, base_family = "text"))                

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    plot.background       = element_rect(fill = "white", color = "white"),
    panel.background      = element_rect(fill = "white", color = "white"),
    plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(1.1), color = "black", family = "text", face = "bold", hjust = 0.5),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(2), color = "black", family = "text", face = "bold", hjust = 0.5), # nolint
    axis.text             = element_text(size = rel(0.8), color = "black", family = "text"), # nolint
    axis.line.x           = element_line(color = "gray40", linewidth = .15),
    panel.grid.minor.y    = element_blank(),
    panel.grid.major.y    = element_line(linetype = "dotted", linewidth = 0.1, color = '#f80000'), # nolint: line_length_linter.
    panel.grid.minor.x    = element_blank(),
    panel.grid.major.x    = element_blank(),
    strip.text            = element_textbox(size     = rel(1),
                                            face     = 'bold', # nolint
                                            color    = "black",
                                            hjust    = 0.5,
                                            halign   = 0.5,
                                            r        = unit(5, "pt"),
                                            width    = unit(5.5, "npc"),
                                            padding  = margin(3, 0, 3, 0),
                                            margin   = margin(3, 3, 3, 3),
                                            fill     = "transparent"),
    panel.spacing       = unit(1, 'lines')
)  


#create a ggplot with no theme
# Create a scatter plot for the summer games data frame
twttr <-  str_glue("<span style='font-family:fa-brands'>&#xf081; </span> ")
github <- str_glue("<span style='font-family:fa-brands'>&#xf09b; </span> ")
captiont  <- str_glue("<br>{twttr}    @JezzaAyt 
{github}Jezzaayt")

# obtained some ideas from other users in TidyTuesday for these 

ggplot(top_summer, aes(x = year, y = total_medals, color = team)) +
  geom_point(show.legend = TRUE) +
  geom_line()+
  labs(title = "Total Medals Won by Team for Summer Games (2000-2016)",
       x = "Year", y = "Total Medals", color = "Team")+
       # Add a text label to highlight the top 5 teams
       geom_text(aes(label = total_medals), nudge_y = 2, nudge_x = .5, check_overlap = TRUE) +
       labs(caption = captiont ) +theme(legend.position = "right", 
       plot.caption = element_markdown(
         size = rel(2 ),
            color = "#5e5d5d" ,
            lineheight = -.5,
            hjust = 0.1,
            halign = 0,
            margin = margin(t = 10, b = 1)
       ))
       



ggsave("Medals.png")
