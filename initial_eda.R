library(tidyverse)

events <- read_csv("DebrisOnDebris_with_names.csv")

ggplot(events, aes(days_to_tca))+
  geom_histogram(binwidth = .25)+
  theme_minimal()
