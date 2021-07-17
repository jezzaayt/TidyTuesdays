library(tidyverse)
library(janitor)
library(lubridate)
library(viridis)
library(ggthemes)
library(plotly)
library(ggrepel)
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

holidays <- holidays %>% clean_names()
holidays

df <- holidays

date <- holidays$date_mdy
as.Date(date, format("%m/%d/%Y"))
df$date_mdy <- mdy(df$date_mdy)
class(holidays$date_mdy)
df$date_mdy
df$day <- lubridate::day(df$date_mdy)

df$month_day <-  format(df$date_mdy, format="%m-%d")
df$month_day
sort(df$month_day)
df <- df[order(df$month_day),]
df
df$month_day
class(day)
days(df$month_day)
df$month
# 
df_grouped <- df %>% group_by(month_day, month, day)
df_grouped
df_summarized <- df_grouped %>% summarise(n =n())
df_summarized <- na.omit(df_summarized)
df_summarized <- df_summarized[order(df_summarized$month)]
df_summarized$month = factor(df_summarized$month, levels = month.abb)
df_summarized



ggplot(df_summarized, aes(x = month, y = day, fill= n)) + geom_tile(color="white", size=0.2) + 
  scale_fill_viridis(option="C") + ggthemes::theme_tufte() + scale_y_reverse() + 
  labs(title = "Most Days of Independence", subtitle = "TidyTuesday: 06/07/2021\n" )

# try and get it in a calendar as previous looks messy

df_cal <- df 
df_cal <- na.omit(df_cal)
df_cal <- df_cal[c(2,3,4,5)]

df_cal$month_day <-  format(df$date_parsed, format="%m-%d")
df_cal
df_cal <- df_cal %>% group_by(date_parsed,weekday,  day, month)
df_cal
df_cal <- df_cal %>% summarise(n =n())
df_cal <- df_cal %>% summarise(n =n(.[[day]]))
df_cal <- df_cal[order(df_cal$month),]
df_cal$month = factor(df_cal$month, levels = month.abb)
#df_cal$weekday = factor(df_cal$weekday, levels = wday_vec)
df_cal$weekday = ordered(df_cal$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
df_cal

ind_cal <- ggplot(df_cal, aes(x=weekday, y = day)) + geom_tile(aes(fill=n), height = 5) +scale_fill_viridis(option="C") +
  facet_wrap(~month) + scale_y_reverse(breaks = seq(0,30, 5)) + 
  labs(title = "Most Days of Independence", subtitle = "TidyTuesday: 06/07/2021\nEach Independence day is labelled per year ",
       x = "",fill =  "Number of\nIndependence days", caption = "Vizusalisation By Jeremy A") +theme_pander() + # theme_tufte() + 
  theme(panel.border = element_rect(colour = "black", size = 1, fill = NA)) + geom_text(aes(label=day, color = "white") ) + 
  guides(color=FALSE )
ind_cal
class(df_cal$weekday)
class(wday_vec)

cal <- df %>% na.omit(df) 
cal <- cal[c(2,3,4,5)]
#cal <- cal %>% group_by(day, month ,weekday)
#cal <- cal  %>% summarise(n =n())
cal$month = factor(cal$month, levels = month.abb)
cal <- cal %>% count(weekday, day, month)

d <- cal %>% count(month, day)

cal <- aggregate(data = cal, weekday+n~ day +month, FUN = first)
cal

cal<- cal %>% group_by(day, month) %>% summarise_all(funs(trimws(paste(., collapse = '.'))))
cal <- cal %>% separate(n, c("n", "n2"))
cal
#cal <- cal  %>% summarise(n =n)
cal$n <- as.numeric(as.character(cal$n))
cal$n2 <- as.numeric(as.character(cal$n2))
cal[is.na(cal)] = 0
cal
cal$n <- cal$n + cal$n2
#cal <-
cal <- cal %>% separate(weekday,c("weekday"), extra="drop")
cal$weekday = ordered(cal$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
cal
class(cal$n)
#cal <- cal  %>% summarise(n =n())

# not the best due to some are overlapping due to like august 10 11 overlap a bit 
indc <- ggplot(cal, aes(x=weekday, y = day, label = day)) + geom_tile(aes(fill=n), position = position_dodge(width=2, preserve="total"),  height = 2) +scale_fill_viridis(option="C") +
  facet_wrap(~month, ncol=4 ) + scale_y_reverse(breaks = seq(0,31, 5)) + 
  labs(title = "Independence around the world", subtitle = "TidyTuesday: 06/07/2021\nThough as some data it over years some dates were on the same day number but different week day.\nThis has been collasped into unique day number per month. ",
       x = "Week Day", fill =  "Number of\nIndependence days", caption = "Vizusalisation By Jeremy Aytoun") +theme_pander() + # theme_tufte() + 
  theme(axis.text.x = element_text(angle=45, hjust=1), panel.border = element_rect(colour = "black", size = 1, fill = NA)) + 
  geom_text(aes(label=day, color = "white" ), size = 2, vjust=.5 ) + 
  guides(color=FALSE, fill = guide_colourbar(barheight = 5, barwidth = 1.5 )) 
indc
ggsave("independence_days.png", height = 8, width =10)


#cal$date_parsed <- NULL
#cal$weekday <- NULL
cal$day

ggplotly(indc)

'#cal <- cal %>% group_by(day, month) %>% summarise(multiple = n_distinct(day, month))
cal 

# plotly did not work as closely as I wanted perhaps if everyday was filled in or did the calendar in another method sure 
ggplotly(ind_cal)

class(df_cal$date_parsed)
df_summarized
