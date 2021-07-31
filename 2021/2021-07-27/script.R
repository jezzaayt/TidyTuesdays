library(tidyverse)
library(patchwork)
library(ggpubr)
## TidyTuesday 27th July 2021
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

# Looking at the shortest / tallest per event


olympics 

df <- olympics %>% group_by(name, sex, height, year, sport )
df <- df %>% distinct(height)
df <- na.omit(df)
df<- df[!duplicated(df$name),]
df
df_m <- df %>% filter(sex == "M")
df_f <- df %>% filter(sex =="F")


df_f_tall <- df_f %>% arrange(desc(height))
df_f_short <- df_f %>% arrange(-desc(height))
df_m_tall <- df_m %>% arrange(desc(height))
df_m_short <- df_m %>% arrange(-desc(height))
df_f_short <- head(df_f_short)
df_f_tall <- head(df_f_tall)
df_m_short <- head(df_m_short)
df_m_tall <- head(df_m_tall)
addline_format <- function(x){
  gsub('\\s','\n',x)
}


df_m_short
mp1 <-  ggplot(df_m_short, aes( x = reorder(name, height), 
                                y = height, fill = sport)) + geom_col() +labs(x = "Names", y = "Height (cm)")+
  geom_text(aes(label= paste(height, "cm\nYear:",  year)), nudge_y=-15) + theme_pubr()
mp1 <- mp1  + scale_x_discrete(breaks=unique(df_m_short$name), labels=addline_format(df_m_short$name))
mp2 <- ggplot(df_m_tall, aes( x = reorder(name, height), y = height, fill = sport)) + geom_col()+
  labs(x = "Names", y = "Height (cm)") +
  geom_text(aes(label  = paste(height, "cm\nYear:",  year)), nudge_y=-20) + theme_pubr()
mp2 <- mp2 + scale_x_discrete(breaks=unique(df_m_tall$name), labels=addline_format(df_m_tall$name))
fp1 <- ggplot(df_f_short, aes( x = reorder(name, height), y = height, fill = sport)) + geom_col()+
  labs(x = "Names" ,y = "Height (cm)") +
  geom_text(aes(label  = paste(height, "cm\nYear:",  year)), nudge_y=-15) + theme_pubr()
fp2 <- ggplot(df_f_tall, aes( x = reorder(name, height), y = height, fill = sport)) + geom_col()+
  labs(x = "Names", y = "Height (cm)") +
  geom_text(aes(label= paste(height, "cm\nYear:",  year)), nudge_y=-20) + theme_pubr()
fp1 <- fp1 + scale_x_discrete(breaks=unique(df_f_short$name), labels=addline_format(df_f_short$name))
fp2 <- fp2 + scale_x_discrete(breaks=unique(df_f_tall$name), labels=addline_format(df_f_tall$name))

(mp1 + mp2) / (fp1 + fp2) + 
  plot_annotation(title="Tallest and shortest male and female players all time in Olypmics",
                                            caption = "Source: Kaggle / TidyTuesday 27th July 2021 \nVisusalisation by Jeremy Aytoun",
                tag_levels = list(c("Male short", "Male tall", "Female short", "Female tall"), 1))

ggsave("olympical_heights.png", scale = 2.5)
