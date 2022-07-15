library(tidyverse)
library(ggthemes)
flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

flights

df <- flights |> group_by(YEAR, flights$`Pivot Label`)|> summarize(FLT_TOT_IFR_2 = sum(FLT_TOT_IFR_2))  
df <- na.omit(df)
df
names(df)[2] <- "Airports"


df_graph <- df |> group_by(df$Airports) |> filter(n()>5)

ggplot(df_graph, aes(x = YEAR,y = FLT_TOT_IFR_2, fill= FLT_TOT_IFR_2)) + geom_col() +facet_wrap(~Airports, nrow = 5) + theme_fivethirtyeight() + theme(legend.position = "top", legend.key.size = unit(2.5,"cm")) +
     labs(title ="Flight activity between 2016-2022", fill =  "Amount of Flights", x = "Years", y = "Number of Flights", subtitle = "TidyTuesday 2022-07-12\nAll Airports that had flights between 2016-2022", 
          caption = "By Jeremy A\nhttps://github.com/jezzaayt") 

