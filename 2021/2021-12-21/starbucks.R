library(tidyverse)
library(ggpubr)
starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')
starbucks

coffee <- starbucks %>% filter(grepl("coffee|Coffee", product_name))  %>% filter(size=="grande")
coffee <- coffee %>% arrange(caffeine_mg)
coffee

milk_coffee <- coffee %>% filter(milk ==5)

tea <- starbucks %>% filter(grepl("tea|Tea", product_name), size=="grande") %>% arrange(caffeine_mg)
tea 
milk_tea <- tea %>% filter(whip == 0,milk == 5)
milk_tea
hot_choc <- starbucks %>% filter(grepl("chocolate|Chocolate", product_name), size=="grande") %>% arrange(caffeine_mg)
milk_choc <- hot_choc %>% filter(whip == 0,milk == 5,)
milk_choc
ggplot(coffee, aes(x = product_name, y = caffeine_mg)) + geom_segment(aes(x=product_name, xend = product_name, y=0, yend=caffeine_mg)) + 
  facet_grid(~size, scales="free") + geom_point()


mcoffee <-  ggplot(milk_coffee) + geom_hline(aes(yintercept = length(milk_coffee))) +geom_col(aes(x= reorder(str_wrap(product_name, 5),calories), y = calories, fill = calories))+ 
  geom_point(aes(x = reorder(str_wrap(product_name,5),caffeine_mg), y = caffeine_mg, color = product_name, size = caffeine_mg),  show.legend=TRUE, alpha=.9) +
  guides(colour= FALSE)+
  coord_polar()


mcoffee <- mcoffee+  scale_fill_gradientn(colours=c("#F9E79F", "#B9770E"))+ 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "gray12", size=7),
        plot.background = element_rect(fill ="transparent", color=NA),
        legend.position = "bottom")  +
  scale_color_manual(values=c("#E65100",  "#F9A825")) + 
  labs(title = "Coffee ")
mcoffee


mtea<- ggplot(milk_tea) + geom_hline(aes(yintercept = length(milk_tea))) +geom_col(aes(x= reorder(str_wrap(product_name, 10),calories), y = calories, fill = calories))+ 
  geom_point(aes(x = reorder(str_wrap(product_name,10),caffeine_mg), y = caffeine_mg, color = product_name, size = caffeine_mg),  show.legend=TRUE, alpha=.9) +
  guides(colour= FALSE)+
  coord_polar() 

mtea <- mtea+  scale_fill_gradientn(colours=c("#F9E79F", "#B9770E"))+ 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "gray12", size=10),
        plot.background = element_rect(fill ="transparent", color=NA),
        legend.position = "bottom")  +
  scale_color_manual(values=c("#B9770E",  "#E65100", "#E65100",  "#FFD54F", "#FF6F00" , "#F9A825", "#E65100")) + 
  labs(title = "Teas  ")
mtea

mchoc <- ggplot(milk_choc) + geom_hline(aes(yintercept = length(milk_choc))) +geom_col(aes(x= reorder(str_wrap(product_name, 10),calories), y = calories, fill = calories))+ 
  geom_point(aes(x = reorder(str_wrap(product_name,10),caffeine_mg), y = caffeine_mg, size = caffeine_mg, color = product_name), show.legend=TRUE, alpha=.9) +
  guides(colour= FALSE)+
  coord_polar() 
  

mchoc <- mchoc+  scale_fill_gradientn(colours=c("#F9E79F", "#B9770E"))+ 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "gray12", size=10),
        plot.background = element_rect(fill ="transparent", color=NA),
        legend.position = "bottom")  +
  scale_color_manual(values=c("#E65100", "#E65100",  "#FFD54F", "#F9A825", "#ffffff")) + 
  labs(title = "Hot Chocolate  ")
mchoc




starbucks_milk<- ggarrange(mchoc, mtea, mcoffee, common.legend = TRUE, nrow=1, legend="bottom")
starbucks_milk<- annotate_figure(starbucks_milk, top=
                  text_grob("\nStarbucks Whole Milk in Coffee, Hot Chocolate and Tea in Grande size (most popular)\nBy the calories and how much caffeine per mg",
                            color = "#00704A")
                )
annotate_figure(starbucks_milk, bottom=text_grob("Visualisation by Jeremy A\nTidyTuesday week of 21st December 2021"))

