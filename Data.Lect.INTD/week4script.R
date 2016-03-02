pnorm(-2) + (1 - pnorm(2))
library(dplyr)

dat <- read.csv("mice_pheno.csv") #We downloaded this file in a previous section

controlPopulation <- filter(dat,Sex == "F" & Diet == "chow") %>%
  dplyr::select(Bodyweight) %>% unlist

hfPopulation <- filter(dat,Sex == "F" & Diet == "hf") %>%
  dplyr::select(Bodyweight) %>% unlist
library(rafalib)

mypar(1,2)

hist(hfPopulation)

hist(controlPopulation)
mypar(1,2)

qqnorm(hfPopulation)

qqline(hfPopulation)

qqnorm(controlPopulation)

qqline(controlPopulation)
