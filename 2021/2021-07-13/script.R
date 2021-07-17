library(tidyverse)
library(ggthemes)
library(viridis)
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

scoobydoo

scoobydoo <- scoobydoo[!scoobydoo$motive == "NULL",] # removes NULL motives so only motive
scoobydoo <- scoobydoo %>% filter(str_detect(series_name, "Scooby*"))# only series that had scooy in the name

df <- scoobydoo %>% count(series_name, motive, sort = TRUE)
df

df1 <-  scoobydoo %>% count(motive, sort = TRUE)
df1

sg <- ggplot(df, aes(x=reorder(motive, n),label= n,  y= n, fill=series_name)) + geom_bar(stat="identity") + coord_flip() +
  geom_text(position=position_stack(0.5), color="white") + theme_fivethirtyeight() + 
  labs(title = "Most Common Scooby-Doo Motives by Series", x=  "Motives", y = "Number of Motives",
       subtitle = "TidyTuesday July 13th 2021 - Scooby Doo Episodes", caption = "By Jeremy Aytoun \nhttps://github.com/jezzaayt") +
  guides(fill=guide_legend("Series Name")) + scale_fill_viridis(option="turbo", discrete = TRUE)
sg


sg2 <- ggplot(df1, aes(x=reorder(motive, n),label= n,  y= n, fill=motive)) + geom_bar(stat="identity") + coord_flip() +
  geom_text(position=position_stack(0.5), color="white", size = 2.5) + theme_fivethirtyeight() + 
  labs(title = "Most Common Scooby-Doo Motives", x=  "Motives", y = "Number of Motives", 
       subtitle = "TidyTuesday July 13th 2021 - Scooby Doo Episodes", caption = "By Jeremy Aytoun\nhttps://github.com/jezzaayt") + 
  guides(fill=guide_legend("Motives"))+ scale_fill_viridis(option="turbo", discrete = TRUE)
sg2


#sg + geom_text(aes(x = reorder(df1$motive, df1$n) , df1$n, y=df1$n))
