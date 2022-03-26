library(tidyverse)
library(ggrepel)
library(ggpubr)
babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

babynames

nz_names <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/nz_names.csv")


df_us <- babynames %>% filter(name=="Jeremy" & sex == "M")
df_nz <- nz_names%>% filter(Name=="Jeremy" & Sex == "Male")  %>% rename(n = Count)%>% rename_with(tolower)

df <- full_join(df_us, df_nz)

          
ggplot() + geom_line(df_us, mapping=aes(x= year, y= n), color="#B22234")+ geom_line(df_nz, mapping = aes(x = year, y=n), color = "#00AFBB")  + 
  labs(title="Births in America and New Zealand with the name Jeremy", subtitle = "TidyTuesday Week of 22nd March 2022\n",
       caption = "")+
  annotate("text", x = 1977, y = 22000, label="Highest in US was 1977 with 21614 Births")+
  annotate("text", x = 1979, y = 900, label="Highest in NZ was 1977 with 220 Births")+
  annotate("segment", x = 1991, xend=1991, y = 0, yend=22000) + annotate("text", x = 1991, y =18000, size=4, label = "Jeremy by Pearl Jam was released in 1991")+
  annotate("segment", x = 1993, xend=1993, y = 0, yend=22000) + annotate("text", x = 2002, y =14000, size=4, label = "Me in 1993")+
  annotate("text", x = 2012, y =14000, size=4, label = "not in these countries")+
  annotate("segment", x = 1999, xend = 1993, y = 13900, yend=13000, colour = "#CC5500", arrow=arrow())+
  theme_pubclean()+scale_x_continuous(limits = c(1923,2020), breaks = seq(1923,2020,10)) +
  annotate("text", x = 2005, y = 21100, label ="Visualisation by Jeremy A\n@Jezzaayt\nhttps://github.com/jezzaayt/", color = "#bababa")+
  annotate("text", x = 1932, y = 20300, label ="Source: Hadley Wickham and Emily Kothe", color = "#bababa")

