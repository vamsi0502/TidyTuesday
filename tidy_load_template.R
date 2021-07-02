# Load Tidy Data from source

library(tidytuesdayR)
tuesdata = tt_load(2021, week= 27)

animal_rescues = tuesdata$animal_rescues

setwd("C:/Users/vamsi/Desktop/UNL/Pet_Projects/tidytuesdays/animal_rescues/")
write.csv(animal_rescues, "animal_rescues.csv")
