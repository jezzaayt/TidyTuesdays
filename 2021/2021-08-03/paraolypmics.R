library(tidyverse)
library(CGPfunctions)
library(patchwork)
library(ggpubr)
athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')


df <- athletes %>% filter(year >= 2000)
abb <- df %>% select(medal, country,  abb, year, event, type)
abb <- abb %>% filter(abb %in% c("GBR", "RSA", "GER", "AUT", "SWE", "USA"))
df1 <- abb %>% count(medal,abb, year , name = "medals") %>% filter(medal == "Gold")
df1 <-  df1[order(df1$year),] 

df1$year <- as.character(df1$year)

gold<- newggslopegraph(dataframe = df1, Times = year, Measurement = medals, Grouping = abb,
                Title = "Gold Medals per country", SubTitle ="Data from International Paralympic Committee\nTidyTuesday August 3rd 2021",
                Caption = "")
df2 <- abb %>% count(medal,abb, year , name = "medals") %>% filter(medal == "Silver")
df2 <-  df2[order(df2$year),] 

df2$year <- as.character(df2$year)

silver<- newggslopegraph(dataframe = df2, Times = year, Measurement = medals, Grouping = abb,
                Title = "Silver Medals per country", SubTitle ="Data from International Paralympic Committee\nTidyTuesday August 3rd 2021",
                Caption = "")

df3 <- abb %>% count(medal,abb, year , name = "medals") %>% filter(medal == "Bronze")
df3 <-  df3[order(df2$year),] 

df3$year <- as.character(df3$year)

bronze <- newggslopegraph(dataframe = df3, Times = year, Measurement = medals, Grouping = abb,
                Title = "Bronze Medals per country", SubTitle ="Data from International Paralympic Committee\nTidyTuesday August 3rd 2021",
                Caption = "")


oscar <- athletes %>% filter(athlete == "PISTORIUS Oscar")
oscar
df_oscar <- oscar %>% select(event, medal, year)
df_oscar<- df_oscar %>% count(medal, year, name = "medals") 
df_oscar$year <-  as.character(df_oscar$year)
df_oscar
oscar <- ggplot(df_oscar, aes(x = year, y = medals, fill = medal)) + geom_bar(stat="identity", position = "dodge") + 
  labs(title="Winning medals by Oscar Pistorius (RSA)", subtitle =  "Data from International Paralympic Committee") + theme_pubr() +
  scale_fill_manual("legend", values=c("Bronze" = "#824A02", "Silver" = "#A7A7AD", "Gold" = "#D6AF36"))

ggsave("oscar_medals.png")


df_rsa <- athletes %>% filter(abb == "RSA" & year >= 2004)
df_rsa <- df_rsa %>% count(medal, year, name = "medals")
df_rsa$year <- as.character(df_rsa$year)

rsa<- ggplot(df_rsa, aes(x = year, y = medals, fill = medal)) + geom_bar(stat="identity", position = "dodge") + 
  labs(title="Winning medals by South Africa (RSA)", subtitle =  "Data from International Paralympic Committee") + theme_pubr() +
  scale_fill_manual("legend", values=c("Bronze" = "#824A02", "Silver" = "#A7A7AD", "Gold" = "#D6AF36"))
ggsave("RSA_medals.png")

gold  + silver + bronze + plot_annotation(title="Medals won per United States, Great Britain, South Africa, Sweden, Germany and Austria",
                                         caption = "Jeremy Aytoun")
ggsave("medals_slope.png")


rsa+oscar + plot_annotation(title="Medals won per South Africa x Oscar Pistorius",
                          caption = "TidyTuesday 3rd August 2021 | Jeremy Aytoun")
