# Tidy Tuesday - 29th June 2021

# Animal Rescues in London Data set

rm(list = ls())
setwd("C:/Users/vamsi/Desktop/UNL/Pet_Projects/tidytuesdays/animal_rescues/")

library(tidyverse)
library(DataExplorer)
library(lubridate)
library(RColorBrewer)
library(waffle)

data = read_csv("animal_rescues.csv")

# Inspect data

glimpse(data)
plot_intro(data)
plot_bar(data)
plot_histogram(data)

data = data %>% 
  mutate(across(where(is.character), factor)) #change all character variables to factor variables

data = data[which(data$cal_year!="2021"),] # Restrict only to full years 2009-2020
  

glimpse(data)

# EDA

data %>% 
  count(animal_group_parent) %>% arrange(desc(n))

# Some animals groups messed: 
# - cat and  Cat
# - bird and budgie and pigeon should be grouped

data = data %>% 
  mutate(animal_group_parent = recode(animal_group_parent, cat = "Cat",
                                      Budgie = "Bird",
                                      Pigeon = "Bird"))

data %>% count(animal_group_parent) %>% View

case_count_by_animal = data %>% count(animal_group_parent, sort = T)


data %>% 
  ggplot(aes(animal_group_parent)) + geom_bar() #does not look good


data %>% 
  count(animal_group_parent, sort = T) %>% 
  ggplot(aes(reorder(animal_group_parent, n), n, fill = animal_group_parent)) + 
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() #much better

# Top 10 animal groups
data %>% 
  count(animal_group_parent, sort = T) %>% 
  top_n(n= 10) %>% 
  ggplot(aes(reorder(animal_group_parent, n), n, fill = animal_group_parent)) + 
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() #much better

data %>% 
  count(cal_year)

data %>% 
  count(animal_group_parent, sort = T) %>% 
  top_n(n =  10)

selected_animals = data %>% 
  count(animal_group_parent, sort = T) %>% 
  top_n(n =  10) %>% .$animal_group_parent %>% as.character

data1 = data[data$animal_group_parent %in% selected_animals,]


# Trend of number of rescues per animal group over the years
data1 %>%
  group_by(cal_year) %>% 
  count(animal_group_parent) %>% 
  ggplot(aes(cal_year, n)) + geom_line() +
  facet_wrap(~animal_group_parent, nrow=5)
  

# Heat map of number of rescues over years

data1 %>% 
  select(animal_group_parent, cal_year) %>% 
  group_by(cal_year) %>% 
  count(animal_group_parent) %>% 
  pivot_wider(names_from = cal_year, values_from= n) %>% View #Visualize in a matrix form

data1 %>% 
  select(animal_group_parent, cal_year) %>% 
  group_by(cal_year) %>% 
  count(animal_group_parent) %>% 
  ggplot(aes(as.factor(cal_year), animal_group_parent, fill= n)) + geom_tile() +
  scale_fill_viridis_b() + theme_ipsum()  


# Number of rescues by month within years


data1 =  data1 %>% 
  mutate(cal_month = month(as.POSIXct(date_time_of_call, format = "%d/%m/%Y %H:%M")))
  
  
data1 %>% 
  select(animal_group_parent, cal_year, cal_month) %>% 
  group_by(cal_year, cal_month) %>% 
  count(animal_group_parent) %>% 
  ggplot(aes(cal_month, n, color = animal_group_parent)) + geom_line()+
  facet_wrap(~cal_year, nrow = 4) + scale_x_continuous(name = "Month", breaks = seq(1,12,1),
                                                       labels = c("J", "F", "M", "A", "M", "J", "J",
                                                                  "A", "S", "O", "N", "D")) + 
  scale_y_continuous(name = "Incident Count") + 
  ggtitle("Incident count trends over the years for different animals") +
  labs(color = "Animal Type") 
  

  
  
  
  
  
  
  
  


