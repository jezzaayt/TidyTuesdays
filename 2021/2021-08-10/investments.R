library(tidyverse)
library(hrbrthemes)
library(scales)
library(ggrepel)
investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/investment.csv')
chain_investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')
ipd <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/ipd.csv')

head(ipd)


ipd_year_start_end <- ipd %>% group_by(category) %>% slice(c(1,n())) %>% ungroup()
investment_year_start_end <- investment %>% group_by(category) %>% slice(c(1,n())) %>% ungroup()
chain_investment_year_start_end <- chain_investment %>% group_by(category) %>% slice(c(1,n())) %>% ungroup()
# Only using investment to plot looked at others 
# Only using the investment due to the double of gross investment in million and not chained. 
# ipd is implicit price deflators
investment_year_start_end
# as the group num is not unique for each category but for each meta category give each their own unique id

investment_year_start_end$uni_id <- investment_year_start_end %>% group_indices(category)

ggplot(investment_year_start_end, aes(x = gross_inv, y = reorder(category, gross_inv), label = gross_inv)) + geom_line(aes(group = uni_id)) +
  geom_point(aes(color=as.factor(year)), size = 4) + theme_ipsum_pub() +  scale_x_continuous(labels= comma, breaks=pretty_breaks(n=13)) +
  labs(title = "Investment pricing between a 70 year difference\nFrom the first year (1947) and last year (2017)",
       subtitle = "Data: Bureau of Economic Analysis U.S Department of Commerce\nTidyTuesday 10th August 2021",
       caption = "Visualisation by Jeremy Aytoun", color= "Years") + xlab("Gross Investment (USD)") + ylab("Categories")
  
ggsave("investments.png", scale=3)                                     
                                                                                             