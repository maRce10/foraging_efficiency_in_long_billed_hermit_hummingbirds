rm(list= ls())
library(tidyverse)
library(readxl)

# To get birds id for set 2014
coglogs14 <- read_excel("C:/Users/Kasia/Dropbox/LBH pooled data/Cognition-personality/Cognitive experiments 2013-2015.xls", sheet = 3)
coglogs14 <- coglogs14 %>% 
  select(`Entry ID`, `Color code`, `ID #`) %>% 
  mutate(`Entry ID`= as.character(`Entry ID`),
         `ID #` = as.character(`ID #`))

id14 <- data.frame(ID = list.files("C:/Users/Kasia/Dropbox/FF_fear and foraging/Tracker output_cuts from cog/csv_tracker_2014"))
id14 <- id14  %>% 
  mutate(ID = substr(ID, 1, 4),
         `Entry ID` = gsub("\\.", "", ID)) %>% 
  select(`Entry ID`)

id14 <- left_join(id14, coglogs14) 
colnames(id14) <- c("EntryID","ColorCode","BirdID") 

setwd("C:/Users/Kasia/Dropbox/FF_fear and foraging/Tracker output_cuts from cog")
write.csv(id14, "id.14.csv", row.names = FALSE)
