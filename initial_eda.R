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
  filter(event_number < 500,
         days_to_tca < 5) %>%
  arrange(event_number, desc(days_to_tca)) %>% 
  ggplot(aes(days_to_tca, pc_nom))+
  geom_point(alpha = .5)+
  geom_line(aes(group = event_number), alpha = .5, arrow = arrow(angle = 25))+
  scale_x_reverse()+
  theme_minimal()

event_summary <- events %>% 
  group_by(event_number) %>% 
  summarize(days_tracked = n(), 
            last_notice = min(days_to_tca), 
            first_notice = (max(days_to_tca)),
            max_pc_best = max(pc_best),
            min_pc_best = min(pc_best),
            pc_best_diff = max_pc_best - min_pc_best,
            max_pc_nom = max(pc_nom),
            min_pc_nom = min(pc_nom),
            pc_nom_diff = max_pc_nom - min_pc_nom,
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

#suggested analysis from Matt
events <- events %>% 
  mutate(pc = if_else(is.na(pc_best), pc_nom, pc_best))


events %>% 
  filter(pc < .005,
         !is.na(days_to_tca)) %>% 
ggplot(aes(log10(pc)))+
  stat_ecdf()+
  #scale_x_continuous(limits = c(0,.0000025))+
  facet_wrap(~floor(days_to_tca), drop = TRUE, ncol = 1)+
  theme_minimal()

events %>% 
  filter(!is.na(days_to_tca)) %>% 
ggplot(aes(log10(pc), color= as.factor(floor(days_to_tca))))+
 geom_density()+
  theme_minimal()

events %>% 
  filter(!is.na(days_to_tca)) %>% 
  ggplot(aes(pc, color= as.factor(floor(days_to_tca))))+
  stat_ecdf()+
  scale_x_continuous(limits = c(0,.00001))+
  theme_minimal()+
  labs(color = "Days to TCA",
       y = "CDF")

#only 392 events with pc > .0001 or 
events %>% 
  filter(pc > .0001) %>% 
  count()

#that's .09% of all events
392/410889

#there is prob_cat that makes the number of fragments harder to use
#here I'll use the .5 as yes to make it easy
events <- events %>% 
  mutate(frag = if_else(prob_cat_if_coll > .5, num_frag_if_catcol, num_frag_no_catcol),
         frag_breaks = cut(frag, breaks = c(0,10, 50, 100, 200, Inf)))

events %>% 
  filter(!is.na(days_to_tca)) %>% 
  ggplot(aes(pc, color = frag_breaks))+
  stat_ecdf()+
  theme_minimal()+
  labs(color = "Fragment",
       y = "CDF")

table(events$frag_breaks)

#with a pc set of .0001 and only less than 5 days to collislion, there would have been 392 events
events %>% 
  filter(pc > .0001,
         days_to_tca < 5)

#If I'm reading this right, there was never a PC above.004
events %>% 
  arrange(desc(pc)) %>% 
  select(pc, pc_nom, pc_best)

#its because all pc_nom above that threshold had small values for pc_best