library(tidyverse)
library(tm)
library(tidytext)
library(ggthemes)
library(tidyverse)
library(ggtext)
library(magick)
library(showtext)
library(patchwork)
library(png)
library(extrafont)
library(data.tree)
library(ggraph)
library(igraph)


font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext_auto()

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/master/data/2024/2024-10-08/most_visited_nps_species_data.csv')


threshold = 200
view(df)

# new df count the amount of commonnames per species per parkname
new_df <- df %>%
  group_by(ParkName, CategoryName, Order) %>%
  summarize(CommonNamesCount = n(), .groups = "drop")%>%
  filter(CommonNamesCount > threshold) %>%
  arrange(desc(CommonNamesCount))

view(new_df)


theme_set(theme_minimal(base_size = 7, base_family = "text"))                
rel_size = 2.5
theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.background       = element_rect(fill = "white", color = "white"),
    panel.background      = element_rect(fill = "white", color = "white"),
    plot.title            = element_text(margin = margin(10, 0, 0, 0), size = rel(rel_size), color = "black", family = "text", face = "bold", hjust = 0.5),

    axis.title.x          = element_text(margin = margin(10, 0, 0, 0), size = rel(rel_size), color = "black", family = "text", face = "bold", hjust = 0.5),
    axis.title.y          = element_text(margin = margin(0, 10, 0, 0), size = rel(rel_size), color = "black", family = "text", face = "bold", hjust = 0.5), # nolint
    axis.text             = element_text(size = rel(2), color = "black", family = "text"), # nolint
    axis.line.x           = element_line(color = "gray40", linewidth = .15),
    
    panel.spacing = unit(1.5, 'lines'),
)  


#create a ggplot with no theme
about <- str_glue("National Park Species — TidyTuesday— 2024-10-08")
twttr <-  str_glue("<span style='font-family:fa-brands'>&#xf081; </span> ")
github <- str_glue("<span style='font-family:fa-brands'>&#xf09b; </span> ")
linkedin <- str_glue("<span style='font-family:fa-brands'>&#xf0e1; </span> ")
captiont  <- str_glue("<br> {about} 
<br>{twttr}    @JezzaAyt 
<br>
{github}Jezzaayt
<br>{linkedin}JeremyAytoun")



ggplot(new_df, aes(x =  reorder(ParkName, -CommonNamesCount), y = CommonNamesCount, fill = CategoryName)) +
  geom_bar(stat = "identity", position = "dodge") +
  #facet_wrap(~Order) +
  labs(title = about, x = "Park Names", caption = captiont,
   subtitle = "Counts of Common Names by Park and Category", fill = "Category Names") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.caption = ggtext::element_textbox(  size = rel(1.2),
  margin = margin(t = 5, r = 5, b = 5, l = 5),
  padding = margin(t = 2, r = 2, b = 2, l = 2))
  ) + guides(fill = guide_legend(override.aes = list(alpha = 1))) 


ggsave("2024/2024-10-08/nationalparks.png", last_plot(),dpi = 300, width = 4, height = 6)
