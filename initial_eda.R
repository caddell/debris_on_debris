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
  filter(event_number < 5) %>%
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
            lead = first_notice - last_notice,
            single_notice = as.factor(if_else(first_notice == last_notice, "Single Notice", "Multiple-Notices"))) %>% 
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
  filter(last_notice < 2,
         max_pc_nom > .0001) %>% 
  count()

event_summary %>% 
  arrange(desc(first_notice,last_notice)) %>% 
  mutate(order = row_number()) %>% 
  pivot_longer(cols = c(last_notice, first_notice), names_to = "notice", values_to = "days") %>%
  select(days, order, notice, single_notice) %>% 
  drop_na() %>% 
  ggplot(aes(days, order))+
  geom_line(aes(group = order), color = 'lightgrey')+
  geom_point(aes(color = notice), size = 1.5)+
  scale_color_colorblind()+
  scale_x_continuous(breaks = seq(from = 0 , to = 10, by = 1))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(y = "Event",
       x = "Days",
       color = "Notice")+
  facet_grid(rows = vars(single_notice))

table(event_summary$single_notice)
