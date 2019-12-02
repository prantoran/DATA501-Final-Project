### Encoding: UTF-8


library(knitr)
opts_chunk$set(fig.path='figure/beamer-',fig.align='center',fig.show='hold',size='footnotesize')

library(dplyr)

library(cowplot)
library(ggplot2)

results <- read.csv('/home/prantoran/datasets/survey_results_public.csv')

results %>% group_by(Student)

results %>% 
  group_by(Student) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(Student,n), stat = 'identity', fill = "#FFB935") +
  labs(
    title = 'Number of Students'
  ) -> s1


results %>% 
  group_by(Employment) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(Employment,n), stat = 'identity', fill = "#FFB935") +
  labs(
    title = 'Employement Status'
  ) +
  coord_flip() -> s2



plot_grid(s2,
  s1,
  #labels = c('Number of Students','Employement Status'),
  label_x = 0.2,
  ncol = 2,
  rel_widths = c(1.5,1))


results %>% filter(Employment %in% 'Employed full-time') %>% 
  filter(Gender %in% c('Male','Female')) %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 500) %>%
  arrange(desc(n)) %>%
  summarise(med_sal = median(ConvertedSalary, na.rm = T)) %>% 
  arrange(med_sal) %>% 
  select(Country) %>% mutate(Country = factor(Country)) -> countries_salary


countries_salary

options(scipen=999)
results %>% filter(Employment %in% 'Employed full-time') %>% 
  filter(Gender %in% c('Male','Female')) %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 500) %>% 
  arrange(desc(ConvertedSalary)) %>% 
  ungroup(Country) %>% 
  ggplot() +
  geom_boxplot(aes(Country,ConvertedSalary, fill = Gender))  +
  scale_fill_manual(name = "Gender", values = c("#a64dff", "#99ccff")
                    , labels = waiver()) +
  scale_x_discrete(limits = countries_salary$Country) +
  coord_flip() +
  scale_y_log10() + 
  labs(x = "Country", 
       y = "Log of Annual Salary in USD",
       title = "Annual Salary in USD - by Country") 

