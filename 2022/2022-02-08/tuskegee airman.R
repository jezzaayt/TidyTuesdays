library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(rgeos)

file <- dir("../../data", recursive = TRUE, full.names = TRUE, pattern = "\\.geojson")
spdf <- geojson_read(file,  what = "sp")
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

  
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fort <- broom::tidy(spdf, region="google_name")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

airmen
df <- airmen %>% count(state)
df <- na.omit(df) # remove NA
df
#remove Non US States
df <- df[df$state != "Unk" & df$state !="Haiti" & df$state != "VI" & df$state != "TD" & df$state != "HT"& df$state != "KN" & df$state != "DE"
         &df$state!= "CN" & df$state != "In", ]
df <- df %>% mutate(state.name = state.name[match(state, state.abb)])
df[is.na(df)] = "District of Columbia" # this is bad but works as only one NA and its DC 
df

spdf_fortified <- spdf_fort %>%
  left_join(. , df, by=c("id"="state.name")) 


ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group, fill = n))+ 
  scale_fill_gradient(low = "#009e73", high = "#d55e00", name = "Number of people\nper State")+
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map() + labs(title = "Tuskegee Airman", 
                     subtitle = "Tuskegee Airman Challenge \nTidyTuesday February 8th 2022\nGrey means no person from this state", caption = "Visualisation by @Jezzaayt") 
