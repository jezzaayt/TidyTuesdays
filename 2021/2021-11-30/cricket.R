library(tidyverse)
library(tidytuesdayR)
library(ggrepel)
library(ggpubr)
library(ggimage)
tuesdata <- tidytuesdayR::tt_load('2021-11-30')
matches <- tuesdata$matches
matches
winners <- matches %>% count(winner, sort=TRUE) 
winners
winners$winner <- factor(winners$winner, levels = winners$winner)

winners <- head(winners,5)
winners
cc <- countrycode(winners$winner, origin = "country.name", destination = "iso2c")
winners$flags <-paste0(".png", winners$flags)
countrycode(winners$winner, origin = "country.name", destination = "iso2c")
winners$flags <- paste0(cc, winners$flags)
winners
winners$flags

 ggplot(winners, aes(x = winner, y = n , label=n, fill = winner))+geom_col()  + theme_cleveland()+
   geom_image(y = -2, aes( image = flags)) + geom_label(colour = "white") + scale_fill_manual(values = c("Australia" = "#012169","South Africa" = "#007749",
                                                                                                  "Pakistan" = "#115740", 
                                                                                         "India" =  "#FF9933","Sri Lanka"= "#8D153A"))+
   scale_y_continuous(limits=c(0,200))+
   labs(title = "Top 5 One Day Test Winners from 1996 - 2005", x = "Winners", y = "Matches Won", 
        subtitle = "TidyTuesday 30th November 2021\nSubmitted by Hassannasir",
        caption = "Visualisation by Jeremy Aytoun\nhttps://github.com/jezzaayt") 
   