library(tidyverse)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(summarytools)

#import data
sgcarmart <- read.csv("sgcarmart.csv")

#remove rows that are null and select necessary columns
avail_cars <- sgcarmart %>%
    select(-1, -2, - contains('href'), - contains('link'), - contains('electric')) %>%
    filter(!grepl('null', Current_Price))

#clean up data and remove artefacts
clean_cars <- avail_cars %>%
  #remove dollar signs, commas, and units
  mutate_all(str_replace_all, "[$]|km/h|km/L|/yr|/mth|,|cc", "") %>%
  #weird character so had to mutate separately
  mutate(Road_Tax = str_replace_all(Road_Tax, "[Â /yr]", "")) %>%
  mutate(ARF = str_replace_all(ARF, "[(after VES rebate)]", "")) %>%
  #Needed the '.%' since the numbers were not the same
  mutate(Down_Payment = str_replace_all(Down_Payment, "(Maximum .*% loan)", "")) %>%
  mutate(Down_Payment = stri_replace_all_fixed(Down_Payment, "()", "")) %>%
  mutate(VES = str_replace_all(VES, "\\(.*\\)", "")) %>%
  mutate(tank_cap = str_replace_all(tank_cap, "L", ""))

#encode dummy variables
cleandum_cars <- clean_cars %>%
  mutate(KeylessEntry = ifelse(KeylessEntry == "Yes", 1, 0)) %>%
  mutate(AutoHeadlights = ifelse(AutoHeadlights == "Yes", 1, 0)) %>%
  mutate(AutoWipers = ifelse(AutoWipers == "Yes", 1, 0)) %>%
  mutate(PaddleShift = ifelse(PaddleShift == "Yes", 1, 0)) %>%
  mutate(SmartKey = ifelse(SmartKey == "Yes", 1, 0)) %>%
  mutate(RemoteBoot = ifelse(RemoteBoot == "Yes", 1, 0)) %>%
  mutate(ReverseCam = ifelse(ReverseCam == "Yes", 1, 0))

#convert numbers from strings to numeric
intcleandum_cars <- cleandum_cars %>%
  mutate_at(vars(-one_of(c('categories','openindiv','Vehicle.Name','veh_type','engine_type'))), as.numeric)

intcleandum_cars %>% filter(Installment<=1700) %>% ggplot(aes(Installment,top_speed,color=veh_type)) + geom_point() + theme_classic()




