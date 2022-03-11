library(tidyverse)
library(ggalluvial)
library(textclean)
library(janitor)

erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')


df <- erasmus 
df <- df %>% remove_empty("rows")
df$sending_city
df$sending_city <- replace_non_ascii(df$sending_city)
df$receiving_city <- replace_non_ascii(df$receiving_city)
df <- df[!(df$sending_city == "*�*"),]
df <- df[!grepl("1/4", df$sending_city),]
df <- df[!grepl("1/4", df$receiving_city),]

df <- df[!grepl("3/4", df$sending_city),]
df <- df[!grepl("3/4", df$receiving_city),]
df <- df[!df$sending_city=="",]
df <- df[!df$receiving_city=="",]

df <- df[!df$sending_city==".",]
df <- df[!df$receiving_city==".",]

df <- df[!df$sending_city=="/",]
df <- df[!df$receiving_city=="/",]



df <- df[!grepl("\\?", df$sending_city),]
df <- df[!grepl("\\?", df$receiving_city),]
df$sending_city <- str_to_title(df$sending_city)
df$receiving_city <- str_to_title(df$receiving_city)
df_ <- df

#checking if going to same city, remove if true
df_ <- df_ %>% mutate(test = as.numeric(df_$sending_city == df_$receiving_city))
df_ <- df_[!grepl("1", df_$test),]


df_<- df_ %>% group_by(sending_city, receiving_city) %>% summarise(freq = n())

df_plot <- df_
df_plot <- df_plot %>% filter(freq >= 95)


ggplot(df_plot, aes(y = freq ,axis1 = sending_city, axis2 = receiving_city))+ ggsankey::theme_sankey()  + 
  geom_alluvium(aes(fill = freq)) + geom_stratum() + geom_label(stat = "stratum", aes(label = after_stat(stratum)))+ 
  scale_x_discrete(limits = c("From", "To"), expand = c(.05, .05))+ labs(title = "Frequency of where ERAMUS Students go to and from where", 
                                                                         caption = "Data: Data.Europa\nVisualisation:Jeremy A\nhttps://github.com/jezzaayt")  + 
  scale_fill_gradient2(low = "#abdbe0",  mid = "#1C8200" , high= "#802417") 

