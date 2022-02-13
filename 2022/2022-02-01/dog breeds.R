library(tidyverse)
library(CGPfunctions)
library(formattable)
library(patchwork)
library(webshot)
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')

# wanted to try animating but nop failed so went with something else, ill try another animation in another tidytuesday due to time during week decided not to
# when I went to finish this weeks off
'df <- breed_rank_all %>% select(-c(links, Image))


df<- melt(df, id=c("Breed"))  %>% separate(variable, "year")
df$year <- as.integer(as.character(df$year)) %>% as.integer(as.character(df$value))
df <- df %>% mutate(rank = rank(value))
df <- df %>%filter(df$value<=25) 
class(df$value)
df <- drop_na(df)
df
ggplot(df, aes(y = value, x = Breed, fill  = Breed, group = Breed)) + geom_col() +
  transition_time(year) + labs(title = "Year: {frame_time}") 
sort(df$value)

unique(df$Breed)

table(df$Breed)'


df_traits <- breed_traits
df_rank <- breed_rank_all  
df <- inner_join(df_rank, df_traits, by = c("Breed" = "Breed"))
df <- df %>% filter(`Mental Stimulation Needs` ==  5)
df 

df_pivot <- df %>% pivot_longer(cols =c(`2013 Rank`,`2014 Rank`, `2015 Rank`, `2016 Rank`, `2017 Rank`, `2018 Rank`, `2019 Rank` , `2020 Rank`),
                                values_to = "values", names_to = "years_rank", values_drop_na = TRUE)

df_pivot %>% gsub("Rank", "", df_pivot)

df_pivot <- df_pivot %>%
  mutate_at("years_rank", str_replace, " Rank", "")
#df_pivot$years_rank <- as.numeric(df_pivot$years_rank)
#df_pivot <- df_pivot %>% arrange(df_pivot, years_rank)
df_pivot
slope <- newggslopegraph(data = df_pivot, years_rank, values, Breed,
                Title = "Breeds ranking who have the most Mental Stimulated Need", 
                SubTitle = "Data from American Kennel Club\nTidyTuesday 2nd February 2022",
                Caption = "Visualisation by Jeremy A")

df_pivot
customOne = "#96DED1"

customTwo = "#6495ED"

customRed = "#F88379"

unique(df_pivot$Breed)
table <- df_pivot %>% group_by(Breed) %>% slice(1) %>% arrange(table, values)
table
table <- table[-c(2,3,12:21)]
table
table_complete <- formattable(table, align=c("l", "c", "c", "c", "c", "c", "c", "l"),
            list(`Affectionate With Family` = color_tile(customOne, customTwo),
                 `Good With Young Children` = color_tile(customOne, customTwo),
                 `Good With Other Dogs` = color_tile(customOne, customTwo),
                 `Shedding Level` = color_tile(customTwo, customOne),
                 `Coat Grooming Frequency` = color_tile(customOne, customTwo),
                 `Drooling Level` = color_bar(customRed)
                 )
            )
table_complete 
slope
widget1 <- as.htmlwidget(table_complete)
htmlwidgets::saveWidget(widget1, "dog_breeds.html")

webshot(url="dog_breeds.html" , file="table_breeds.png", vheight= 265, zoom = 5)
table_breeds = png::readPNG("table_breeds.png", native=TRUE, info = TRUE)
table_breeds                          
slope + cowplot::draw_image(table_breeds, x = 4, scale = 60, y = 90)
