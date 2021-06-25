library(tidyverse)
library(ggrepel)
library(ggridges)
library(ggsci)
parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')

head(parks)

#parks$spend_per_resident_data <- as.numeric(parks$spend_per_resident_data)
parks$spend_per_resident_data
class(parks$spend_per_resident_data)

df <- parks 
df <-df %>% mutate(city=replace(city, city == "Washington, DC", "Washington, D.C."))

df$spend_per_resident_data <- gsub("\\$","",as.character(df$spend_per_resident_data))

df$spend_per_resident_data <- as.numeric(df$spend_per_resident_data)
df_20 <- df %>% group_by(rank) %>% filter(year == 2020) %>% arrange(desc(city)) %>% slice(1:10)
df_20$text <- paste0("$", df_20$spend_per_resident_data)
df_20 <- head(df_20, 20)

ggplot(df_20, aes(y = city, x= spend_per_resident_data, label = text, fill=spend_per_resident_data)) +
  geom_density_ridges_gradient(stat="binline", bins=50)+
  scale_fill_material()+
  geom_label_repel(
    nudge_y = .1) + labs(title="Park spending per resident in USD", subtitle = "TidyTuesday June 22nd 2021 \nPark Access",
                         x = "Spending Per Resident", 
                         y = "City",
                         caption = "By https://github.com/jezzaayt",
                         fill = "Spending per resident")

