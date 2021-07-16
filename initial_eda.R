library(tidyverse)
library(ggthemes)

events <- read_csv("DebrisOnDebris_with_names.csv")

ggplot(events, aes(days_to_tca))+
  geom_histogram(binwidth = .25)+
  theme_minimal()

ggplot(events, aes(pc_best, days_to_tca))+
  geom_point()+
  theme_minimal()

event_summary <- events %>% 
  group_by(event_number) %>% 
  summarize(days_tracked = n(), 
            last_notice = min(days_to_tca), 
            first_notice = (max(days_to_tca)),
            lead = first_notice - last_notice) %>% 
  filter(days_tracked < 11) #some weird event history
  
ggplot(event_summary, aes(x = days_tracked))+
  geom_histogram(binwidth = 1)+
  theme_minimal()

ggplot(event_summary, aes(last_notice))+
  geom_histogram(binwidth = .25)+
  theme_minimal()

ggplot(event_summary, aes(first_notice))+
  geom_histogram(binwidth = .25)+
  theme_minimal()

event_summary %>% 
  filter(last_notice < 1.5) %>% #pull out events that went away
  pivot_longer(cols = c(last_notice, first_notice), names_to = "notice", values_to = "days") %>% 
  ggplot(aes(days, event_number))+
  geom_line(aes(group = event_Snumber), color = 'lightgrey')+
  geom_point(aes(color = notice), size = 3)+
  scale_color_colorblind()+
  theme_minimal()
  