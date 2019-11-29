### Encoding: UTF-8


library(knitr)
opts_chunk$set(fig.path='figure/beamer-',fig.align='center',fig.show='hold',size='footnotesize')

library(dplyr)

library(cowplot)
library(ggplot2)

results <- read.csv('C:\Users\prantor\work\src\github.com\prantoran\DATA501-Final-Project/survey_results_public.csv')

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


install.packages("devtools")
devtools::install_github("rstudio/shiny")

results %>% filter(Employment %in% 'Employed full-time') %>% 
  filter(!is.na(DevType)) %>%
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  select(DevType,ConvertedSalary,Gender) %>%
  mutate(DevType = stringr::str_split(DevType, pattern = ";")) %>%
  tidytext::unnest(DevType) %>%
  group_by(DevType,Gender) %>%
  summarise(Median_Salary = median(ConvertedSalary,na.rm = TRUE)) %>%
  arrange(desc(Median_Salary)) %>%
  ungroup() %>%
  mutate(DevType = reorder(DevType,Median_Salary)) %>%
  head(20) %>% 
  hchart('column',hcaes('DevType','Median_Salary', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>%  
  hc_title(text = "Median Salary by Developer Type wrt Gender")
