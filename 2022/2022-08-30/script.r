library(tidyverse)
library(dplyr)

#downloaded it from 
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-08-30/pell.csv
pell <- read.csv('pell.csv')

head(pell)
pell$state

df <- pell %>% group_by(YEAR, STATE)
df
ggplot(pell, aes(x = YEAR, y = AWARD, fill = STATE)) + geom_col()

install_version("tidy", version = "0.9.1", repos = "http://cran.us.r-project.org")