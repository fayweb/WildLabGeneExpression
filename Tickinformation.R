#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(stringr)

#import sota to gain information on the wild data
SOTA <- read.csv("https://raw.githubusercontent.com/derele/Mouse_Eimeria_Field/master/data_products/SOTA_Data_Product.csv")

#get a list of columnn names of SOTA
colnames(SOTA)

#make a new data set for the parasites
Ectoparasites <- 
  SOTA %>%
    select(Mouse_ID, Ectoparasites_Logical, Ticks, Fleas) %>%
   #replace all NAs with False
    replace_na(list(Ectoparasites_Logical = "FALSE", Ticks = "FALSE", Fleas = "FALSE")) %>% 
   #now select relevant mice (Starting with AA)
    filter(grepl("AA_", Mouse_ID)) 

Ectoparasites$Fleas <- as.factor(Ectoparasites$Fleas)
Ectoparasites$Ticks <- as.factor(Ectoparasites$Ticks)
Ectoparasites$Ectoparasites_Logical <- as.factor(Ectoparasites$Ectoparasites_Logical)


Ectoparasites %>%
  group_by(Fleas) %>%
  summarize(Total = n()) %>%
  ggplot(x = Fleas, y = Total) + 
  geom_bar()

#plot for fleas
ggplot(Ectoparasites, aes(Fleas, Mouse_ID, color = Fleas)) +
  geom_bar(stat = "identity")
