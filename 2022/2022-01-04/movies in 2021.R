library(tidyverse)
library(RColorBrewer)
library(urbnthemes)
# Tidy Tuesday 2021
#Bring own data 
# so I am using my movies watched which I record in Google Sheets 

movies_2021 <- read_csv("2021 movies.csv")
movies_2021 <- subset(movies_2021, select = c("Name", "When Watched", "Where did I watch it?", "Movie
TV show", "Year Released", "Genre", "Rating out of 5", "Rating"))
#did have some Netflix in january of 2021 but didn't watch more than 10 so did not include 
movies_2021 <- movies_2021 %>% filter(movies_2021$`Where did I watch it?` %in% c("Amazon Prime", "Disney+"))
movies_2021
series <- movies_2021 %>% filter(movies_2021$`Movie
TV show` == "Series")
movies <- movies_2021 %>% filter(movies_2021$`Movie
TV show`== "Movie") 
movies<- movies %>% add_count(Genre, `Where did I watch it?`)
movies <- movies %>% add_count(Genre,`Where did I watch it?`, `Rating out of 5`, name = "TotalCount")
movies
#ggplot(series,aes(x = Genre, y = reorder(`Rating out of 5`, Genre), fill = `Rating out of 5`)) + geom_col(position="dodge2")+
 # facet_wrap(`Where did I watch it?`~Genre, scales = "free") +scale_fill_distiller(palette="RdBu")

# Chose to only focus on movies

ggplot(movies,aes(x = Genre, y = reorder(`Rating out of 5`, Genre), fill = TotalCount)) + geom_col(position= position_dodge(width = 1.2))+
  facet_grid(`Where did I watch it?`~Genre, scales = "free") +scale_fill_distiller(palette="Dark2")  + 
  labs(title = "My most watched film genres split between Amazon Prime (38) and Disney+ (17)\nTidyTuesday 04/01/2021 Bring own Data ",
       subtitle = " I record data everytime I watch via Google Sheets and this is just for 2021", caption="https://github.com/jezzaayt",
       fill = "Film Count", y = "Rating out of 5") +
  theme_urbn_print() + theme(legend.key.size =  unit(1, "cm"), legend.title = element_text("Film Count")) + 
  geom_text(aes(label=paste0("Rating:",`Rating out of 5`, "\nFilms:",`TotalCount` )),
                                                                           position=position_dodge(width=1.2), vjust=-0.2)
