# Clase INTD 8065 Statistical Inference
# We set seed to avoid 
set.seed(1)
install.packages("downloader")##use install.packages to install
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if (!file.exists(filename)) download(url, destfile=filename)
dat <-read.csv("femaleMiceWeights.csv")
head(dat)
View(dat)
install.packages("dplyr")
library(dplyr)
control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist
print( mean(treatment) )
print( mean(control) )
obsdiff <- mean(treatment) - mean(control)
print(obsdiff)
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- "femaleControlsPopulation.csv" 
##check if file exists and if it does not, download it:
if (!file.exists(filename)) download(url,destfile=filename)
population <- femaleControlsPopulation
##use unlist to turn it into a numeric vector
population <- unlist(population) 
# Sample without replacement How does it vary?
control <- sample(population,12)
mean(control)

control <- sample(population,12)
mean(control)

control <- sample(population,12)
mean(control)
##12 control mice
control <- sample(population,12)
##another 12 control mice that we act as if they were not
treatment <- sample(population,12)
print(mean(treatment) - mean(control))
#Sample from the Null Hypothesis
n <- 10000
null <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  null[i] <- mean(treatment) - mean(control)
}
#So what percent of the 10,000 are bigger than obsdiff?

mean(null >= obsdiff)
# Heights of Men in a Population
install.packages("UsingR")
library(UsingR)
data(father.son,package="UsingR")
x <- father.son$fheight
# Take a Sample
round(sample(x,10),1)
# Empirical Distribution
smallest <- floor( min(x) )
largest <- ceiling( max(x) )
values <- seq(smallest, largest,len=300)
heightecdf <- ecdf(x)
plot(values, heightecdf(values), type="l",
     xlab="a (Height in inches)",ylab="Pr(x <= a)")
# Histogram
hist(x)
# Specify sequence of Bins, better Labels
bins <- seq(smallest, largest)
hist(x,breaks=bins,xlab="Height (in inches)",main="Adult men heights")
install.packages("rafalib")
n <- 100
library(rafalib)
nullplot(-5,5,1,30, xlab="Observed differences (grams)", ylab="Frequency")
totals <- vector("numeric",11)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  nulldiff <- mean(treatment) - mean(control)
  j <- pmax(pmin(round(nulldiff)+6,11),1)
  totals[j] <- totals[j]+1
  text(j-6,totals[j],pch=15,round(nulldiff,1))
  ##if(i < 15) Sys.sleep(1) ##You can add this line to see values appear slowly
}
# Values as large as Observed Difference are relatively rare
hist(null, freq=TRUE)
abline(v=obsdiff, col="red", lwd=2)
# Normal approximation works very well here
1 - pnorm(obsdiff,mean(null),sd(null))
