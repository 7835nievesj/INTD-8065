# Population Samples Estimates
# 2. Understanding the Population
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- "mice_pheno.csv"
if (!file.exists(filename)) download(url,destfile=filename)
dat <- mice_pheno
View(dat)
library(dplyr)
controlPopulation <- filter(dat,Sex == "F" & Diet == "chow") %>% 
  dplyr::select(Bodyweight) %>% unlist
length(controlPopulation)
hfPopulation <- filter(dat,Sex == "F" & Diet == "hf") %>% 
  dplyr::select(Bodyweight) %>% unlist
length(hfPopulation)
summary(controlPopulation)
summary(hfPopulation)
mean(controlPopulation)-mean(hfPopulation)
t=(mean(controlPopulation)-mean(hfPopulation))/(sqrt(1/length(controlPopulation)+1/length(hfPopulation)) * (224*var(controlPopulation)+199*var(hfPopulation)/423)
)
t
