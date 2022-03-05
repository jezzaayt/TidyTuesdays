library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(cowplot)

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

stations

df <- stations #%>% summarise(.data = stations, STATE)
df <- df %>% count(STATE) 
spdf <- geojson_read("./../../data/us_states_hexgrid.geojson",  what = "sp")


spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

df_state_names <- df
df_state_names
df_state_names
df_state_names$STATE <- stateConversion(df_state_names$STATE, )
df_state_names <- na.omit(df_state_names)

library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))


spdf_fortified <- spdf_fortified %>% 
  left_join(., df_state_names, by=c("id"="STATE"))

spdf_fortified
library(viridis)
my_palette <- rev(magma(8))[c(-1,-8)]
plot(spdf)

ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = n, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="red", size=3.5, alpha=0.6) +
  theme_map()  +
 scale_fill_viridis(discrete = FALSE, option = "A",limits=c(2,15661), breaks = seq(min(spdf_fortified$n),max(spdf_fortified$n),
                                                                                   (max(spdf_fortified$n)-min(spdf_fortified$n))/4))+
  labs(title = "Alterative Fuel stations", subtitle = "Source: Tuesday 1st March TidyTuesday\nUS DOT", fill ="Stations per State", caption = "Visualisation by Jeremy A\nhttps://github.com/jezzaayt/")+
  theme(
    legend.direction = "horizontal",
   legend.position = c(0.2, 0),
   legend.key.width = unit(2,"cm"),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(color = NA),
    plot.title = element_text(size= 18, hjust=0.5, color = "#FF4500", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) 

ggplot() +geom_col(data = df, aes (x = reorder(STATE,-n), y = n)) + geom_text(data = df, aes (x =STATE, y = n,  label = n), nudge_x =  -0.5)

# found via https://favorableoutcomes.wordpress.com/2014/10/28/updated-r-function-state-conversion-for-converting-state-codes-to-full-state-name/

stateConversion <- function(x, faclevs = 'selected') {
  
  st.codes <- data.frame(state = as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
                                             "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN",
                                             "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH",
                                             "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT",
                                             "WA", "WI", "WV", "WY")),
                         full = as.factor(c("Alaska","Alabama" ,  "Arkansas", "Arizona","California" , "Colorado" ,
                                            "Connecticut", "District of Columbia","Delaware" ,  "Florida" , "Georgia" ,
                                            "Hawaii","Iowa" ,"Idaho" , "Illinois" , "Indiana" ,  "Kansas" ,
                                            "Kentucky" , "Louisiana" , "Massachusetts", "Maryland" ,"Maine" ,
                                            "Michigan" , "Minnesota" , "Missouri" ,"Mississippi" ,  "Montana" ,
                                            "North Carolina","North Dakota", "Nebraska" , "New Hampshire" , "New Jersey" ,  "New Mexico" ,
                                            "Nevada" ,"New York" , "Ohio" , "Oklahoma" ,
                                            "Oregon" , "Pennsylvania" , "Puerto Rico", "Rhode Island" , "South Carolina", "South Dakota" ,
                                            "Tennessee" , "Texas" , "Utah" ,  "Virginia","Vermont" ,
                                            "Washington" , "Wisconsin", "West Virginia" , "Wyoming"))
  )
  
  if (nchar(x[1]) == 2) { st.x <- data.frame(state = x); refac.x <- st.codes$full[match(tolower(st.x$state), tolower(st.codes$state))] }
  else { st.x <- data.frame(full = x); refac.x <- st.codes$state[match(tolower(st.x$full), tolower(st.codes$full))] }
  
  if(faclevs == 'all') {return(refac.x)}
  else {return(factor(refac.x))}
  
}
  


