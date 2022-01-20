library(tidyverse)
library(janitor)
library(viridis)
library(ggpubr)
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')
chocolate
glimpse(chocolate)
choc_names <- sort(table(chocolate$company_location), decreasing=TRUE)[1:5]  %>% names()
choc_names
sort(table(chocolate$ingredients), decreasing=TRUE)

ratingmean <- chocolate %>% group_by(company_location) %>% summarise(mean(rating)) %>% rename("avg_rating" = 'mean(rating)' ) 
uni<- unique(ratingmean)
distinct(ratingmean,avg_rating)
dup_rating <- ratingmean %>% group_by(round(avg_rating, digits = 1)) %>% filter(n()>=1) %>% summarise(n=n())

 

duplicated(ratingmean$avg_rating)
sum(duplicated(ratingmean$avg_rating))
dup<- ratingmean %>% get_dupes(avg_rating) 
dup <- dup[!duplicated(dup$avg_rating),]
dup$mylabel = paste0("Countries with same average rating: ", dup$dupe_count)
dup

ggplot(dup, aes( x= reorder(company_location,avg_rating), y = avg_rating, fill = avg_rating, label = mylabel))  +geom_col() +
  coord_flip() + geom_label(fill="grey20", hjust= 1.5, color="white") + labs(x = "Countries", y= "Average Rating", fill = "Average Rating", title ="Average chocolate rating by company location", 
                                                              subtitle = "Counted the amount of different company locations with same rating. Ignoring ratings that only had one company location  \nTidyTuesday 18th January 2022", caption = "Graphic by Jeremy A\nhttps://github.com/jezzaayt") + theme_pubclean() +
  scale_y_continuous(limits = c(0,3.5), breaks = seq(0,3.5,by=0.25)) + scale_fill_viridis(discrete=FALSE, option = "D", limits = c(2,4)) 
