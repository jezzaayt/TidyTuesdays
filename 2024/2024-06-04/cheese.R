library(tidyverse)
library(readr)
library(leaflet)
library(sf)
library(tmap)   
library(spData)
library(maps)
library(ggtext)
library(magick)
library(showtext)
library(patchwork)
library(png)
library(extrafont)
library(NLP)
library(plotly)

font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')

showtext_auto(enable = TRUE)
current_dir = as.String(dirname(rstudioapi::getSourceEditorContext()$path))
current_dir


# Import the CSV file
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv"
cheese_data <- read_csv(url)


# Basic data analysis
head(cheese_data)            # View the first few rows
str(cheese_data)             # Check the structure of the data
summary(cheese_data)         # Summary statistics
colSums(is.na(cheese_data))  # Check for missing values

# Data cleaning only france from the country columne

# Filter out non-France data 
cheese_data_fr <- cheese_data %>% filter(country == "France") %>% drop_na(region)
cheese_data_fr


# Example dataframe with location names
locations_df <- data.frame(
  region = c(
    "Amou", "Anjou", "Aquitaine", "Aube", "Auvergne", "Auvilliers", "Averyon",
    "Avesnes", "Aveyron", "Banon", "Bas-Languedoc", "Basque", "Bergues", "Berry",
    "Bourgogne", "Brittany", "Burgund", "Burgundy", "Béarnaise in Pyrénées-Atlantique",
    "Centre", "Centre-Val de Loire", "Cevenes", "Charentes", "Charentes-Poitou",
    "Chirac", "Comtat Venaissin", "Corsica", "Croisy-sur-Eure", "Franche Comté",
    "French Basque Country", "Gascony", "Gevrey-Chambertin", "Gâtinais", "Haute Vienne",
    "Haute-Savoie / Upper Savoy", "Herault", "Ile de France", "Ile-de-France/Champagne",
    "Illoud (Haute-Marne)", "Isere", "Laguiole", "Languedoc", "Languedoc-Roussillon",
    "Laqueuille", "Laruns", "Larzac", "Loire", "Loire Valley", "Lower Normandy",
    "Midi-Pyrenees", "Midi-Pyrénées", "New York", "Nord-Pas-de-Calais", "Normandy",
    "Pays Basque", "Pays d’Auge", "Poitou-Charentes", "Provencale", "Provence",
    "Puimichel in Provence Alpes", "Pyrenees", "Pyrenees Mountains", "Pyrenees-Atlantiques",
    "Pyrénées", "Pyrénées-Atlantiques", "Rhone Valley", "Rhone-Alps", "Rhône-Alpes",
    "Roncq", "Salers", "Savoie", "St Antoine", "Troyes", "Upper Corsica", "massif des Causses",
    "province of Brittany", "the department of Loiret"
  )
)

# Correctly applying mutate and case_when
locations_df <- locations_df %>%
  mutate(
    location = case_when(
      region %in% c("Amou", "Aquitaine", "Basque", "French Basque Country", 
                      "Gascony", "Laruns", "Pays Basque", "Pyrénées-Atlantiques", 
                      "Pyrenees-Atlantiques") ~ "Nouvelle-Aquitaine",
      region %in% c("Anjou", "Charentes", "Charentes-Poitou", "Poitou-Charentes", 
                      "Haute Vienne") ~ "Nouvelle-Aquitaine",
      region %in% c("Aube", "Illoud (Haute-Marne)", "Troyes") ~ "Grand Est",
      region %in% c("Auvergne", "Haute-Savoie / Upper Savoy", "Isere", 
                      "Laqueuille", "Salers", "Savoie", "Rhone-Alps", 
                      "Rhône-Alpes") ~ "Auvergne-Rhône-Alpes",
      region %in% c("Bourgogne", "Burgund", "Burgundy", "Gevrey-Chambertin", 
                      "Franche Comté") ~ "Bourgogne-Franche-Comté",
      region %in% c("Brittany", "province of Brittany") ~ "Bretagne",
      region %in% c("Béarnaise in Pyrénées-Atlantique") ~ "Nouvelle-Aquitaine",
      region %in% c("Centre", "Centre-Val de Loire", "Gâtinais", 
                      "the department of Loiret") ~ "Centre-Val de Loire",
      region %in% c("Cevenes", "Languedoc", "Languedoc-Roussillon", 
                      "Midi-Pyrenees", "Midi-Pyrénées", "Aveyron", "Larzac", 
                      "Bas-Languedoc", "Herault") ~ "Occitanie",
      region %in% c("Corsica", "Upper Corsica") ~ "Corse",
      region %in% c("Comtat Venaissin", "Provence", "Provencale", 
                      "Puimichel in Provence Alpes", "Banon") ~ "Provence-Alpes-Côte d'Azur",
      region %in% c("Croisy-sur-Eure", "Lower Normandy", "Normandy", 
                      "Pays d’Auge") ~ "Normandy",
      region %in% c("Ile de France", "Ile-de-France/Champagne") ~ "Île-de-France",
      region %in% c("Loire", "Loire Valley") ~ "Centre-Val de Loire",
      region %in% c("Nord-Pas-de-Calais", "Bergues", "Roncq") ~ "Hauts-de-France",
      region == "New York" ~ "Outside France",
      TRUE ~ "Unclassified"  # Assigns "Unclassified" to unmatched locations
    )
  )

# Display the updated dataframe
print(locations_df)

str(cheese_data_fr)
cheese_data_fr <- cheese_data_fr %>%
  separate_rows(milk, sep = ",\\s*") 


cheese_data_fr <- cheese_data_fr %>%
  separate_rows(region, sep = ",\\s*") 
cheese_data_fr

cheese_data_fr  <- left_join(cheese_data_fr,locations_df,  by="region")
# make new colume to make regions to the new named regions 




# count how many types of milk per region

cheese_data_fr <- cheese_data_fr %>% group_by(milk, location) %>% summarise(count = n())

cheese_data_fr




# Create a map using leaflet
# Using this france geojson 
geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions.geojson"

map <- st_read(geojson_url)
#rename column to match the prev
map  <- map  %>% rename(location = nom)
map
table(map$region)



merge_df  <- left_join(map, cheese_data_fr, by ="location")
merge_df
str(merge_df)

# filter to only one unique location 

merge_df <- merge_df %>% group_by(location) %>% filter(row_number() == 1)
merge_df


theme_set(theme_void(base_size = 12, base_family = "Sans-Serif"))                

theme_update(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'right',
    plot.background       = element_rect(fill = "white", color = "white"),
    panel.background      = element_rect(fill = "white", color = "white"),
    plot.margin           = margin(t = 10, r = 10, b = 10, l = 10),
     axis.text             = element_text(size = rel(2.5), color = "black", family = "text"), # nolint
    strip.text            = element_textbox(size     = rel(1),
                                            face     = 'bold', # nolint
                                            color    = "black",
                                            hjust    = 0.5,
                                            halign   = 0.5,
                                            r        = unit(5, "pt"),
                                            width    = unit(5.5, "npc"),
                                            padding  = margin(3, 0, 0, 0),
                                            margin   = margin(3, 0, 3, 3),
                                            fill     = "transparent")
)

about <- str_glue("Cheese — TidyTuesday— 2024-06-04")
twttr <-  str_glue("<span style='font-family:fa-brands'>&#xf081; </span> ")
github <- str_glue("<span style='font-family:fa-brands'>&#xf09b; </span> ")
caption  <- str_glue("<br> {about} 
<br>{twttr}    @JezzaAyt 
{github}Jezzaayt")

title <- str_glue("Most amount of Cheese made by animal per region in France") 

p <- ggplot(merge_df) +
  geom_sf(color = "red", aes(fill =count)) +
  geom_sf_text(aes(label = location), nudge_y = -.2, 
  color ="#ff5a30", size  = 3.7) + 
  geom_sf_label(aes(label = milk, fill=count),color="#ff3281", nudge_y= .2,) +
  geom_sf_label(aes(label = count, fill=count),color="#ff3281",nudge_y= .2 ,nudge_x = .5)+
  labs(title=title, caption = caption) +
  scale_fill_viridis_c(name = "count", begin = 0.1, end = 1)+
  theme( legend.position='bottom', 
   axis.title.y = element_blank(),
   axis.title.x = element_blank(), 
   axis.text = element_blank(), ,
       plot.caption = element_markdown(
         size = rel(2 ),
            color = "#5e5d5d" ,
            lineheight = -.5,
            hjust = 0.1,
            halign = 0,
            margin = margin(t = 10, b = 1)
       ))
merge_df$count
class(current_dir)
filename =current_dir  +"/Milk In French Regions.png"
filename
ggsave(filename, last_plot(), dpi = 300, width = 4, height = 7)


# trying to add interactivity with the plot 

interactive_map <- ggplotly(p)
interactive_map

# Will have to edit and improve this plotly but it works kind of... 
# It does not have all the information listed as just the ggplot 