library(tidyverse)
library(scales)
library(ggpubr)

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')
colony <- colony[!(colony$state=="United States"),]

cali <- colony %>% filter(state == "California")
cali
stress_cali <- stressor %>% filter(state == "California")
stress_cali <- stress_cali %>% group_by(year, state, stressor) %>% summarise(sum(stress_pct)) 
stress_cali <- stress_cali%>% rename("yearly_pct" = "sum(stress_pct)")
stress_cali
cali <- cali %>% left_join(stress_cali)
cali
cali <- cali %>% filter( year == c("2020", "2021"))
colony_year <- colony %>% group_by(year,  state ) %>% summarize(sum(colony_lost),sum(colony_added)) %>% rename(year_colony_lost='sum(colony_lost)' )   %>% rename(year_colony_added = 'sum(colony_added)')
colony_year
ggplot(stress_cali, aes(x = year, y = yearly_pct, fill = stressor, color = stressor)) + geom_point() + geom_line() + geom_area() + facet_grid(~stressor)

ggplot(colony_year, aes(x = year, year_colony_lost, fill = year)) + geom_col() + coord_flip() + 
  geom_text(aes(label = year_colony_lost), hjust = 1) + facet_wrap(~state, scales="free_x") + scale_x_continuous(labels=as.character(colony_year$year),
                                                                                                      breaks=colony_year$year) + 
  scale_fill_distiller(palette = "Oranges")

# get the values in negative so can plot with negative and positive graph
colony_year$year_colony_lost <-  colony_year$year_colony_lost - colony_year$year_colony_lost - colony_year$year_colony_lost
colony_year

ggplot(colony_year)+ geom_bar(stat="identity", aes(x =year, y = year_colony_added), fill="#648FFF") + geom_bar( aes( x = year, y = year_colony_lost), fill = "#FE6100", stat="identity") + 
  facet_wrap(~state, scales="free_x", ncol = 9) + coord_flip() + scale_y_continuous(labels=comma)   +scale_x_continuous(labels=as.character(colony_year$year),
                                                                                                             breaks=colony_year$year) +
  geom_text(data = colony_year, aes(x = year, y = year_colony_lost, label = year_colony_lost), position = position_stack(vjust=-.5), size = 3)+
  geom_text(data = colony_year, aes(x = year, y = year_colony_added, label = year_colony_added), position = position_stack(vjust=1), size = 3) + 
  labs(title = "Bees loss and gains in the United States", subtitle = "TidyTuesday 11th January 2022\nFrom USDA", x = "Years", y = "Colony differences", caption  = "Visualsiation by Jeremy A") + 
  theme_cleveland()
       
