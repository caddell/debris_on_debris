library(tidyverse)
library(ggthemes)

events <- read_csv("DebrisOnDebris_with_names.csv")

ggplot(events, aes(days_to_tca))+
  geom_histogram(binwidth = .25)+
  theme_minimal()

events %>% 
  filter(pc_best < .001) %>% 
ggplot(aes(pc_best, days_to_tca))+
  geom_point()+
  theme_minimal()

events %>% 
  filter(pc_nom < .001) %>% 
  ggplot(aes(pc_nom, days_to_tca))+
  geom_point()+
  theme_minimal()

events %>% 
  filter(pc_nom < .001,
         event_number < 55) %>%
  arrange(event_number, desc(days_to_tca)) %>% 
  ggplot(aes(days_to_tca, pc_nom))+
  geom_point()+
  geom_line(aes(group = event_number), arrow = arrow())+
  scale_x_reverse()+
  theme_minimal()

event_summary <- events %>% 
  group_by(event_number) %>% 
  summarize(days_tracked = n(), 
            last_notice = min(days_to_tca), 
            first_notice = (max(days_to_tca)),
            max_pc_best = max(pc_best),
            min_pc_best = min(pc_best),
            max_pc_nom = max(pc_nom),
            min_pc_nom = min(pc_nom),
            lead = first_notice - last_notice) %>% 
  filter(days_tracked < 11) #some weird event history
  
ggplot(event_summary, aes(x = days_tracked))+
  geom_density(adjust = 2)+
  theme_minimal()

ggplot(event_summary, aes(last_notice))+
  geom_density(adjust = 2)+
  theme_minimal()

event_summary %>% 
  summarise(stopped_tracking = sum(last_notice > 2.5, na.rm = TRUE), stopped_tracking_pct = stopped_tracking/n())

ggplot(event_summary, aes(first_notice))+
  geom_density(adjust = 2)+
  theme_minimal()

event_summary %>% 
  filter(last_notice < 1.5) %>% #pull out events that went away
  pivot_longer(cols = c(last_notice, first_notice), names_to = "notice", values_to = "days") %>% 
  ggplot(aes(days, event_number))+
  geom_line(aes(group = event_number), color = 'lightgrey')+
  geom_point(aes(color = notice), size = 1.5)+
  scale_color_colorblind()+
  theme_minimal()
