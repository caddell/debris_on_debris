library(tidyverse)

events <- read_csv("DebrisOnDebris_with_names.csv")

ggplot(events, aes(days_to_tca))+
  geom_histogram(binwidth = .25)+
  theme_minimal()

ggplot(events, aes(pc_best, days_to_tca))+
  geom_point()+
  theme_minimal()
