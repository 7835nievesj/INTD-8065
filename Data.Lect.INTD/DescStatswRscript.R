# mtcars contains data of  fuel consumption and 10 aspects of automobile design 
# and performance for 32 automobiles (1973-74 models).
# Source: 1974 Motor Trend US magazine.
data(mtcars)
head(mtcars)

# One way of using the information inside de data frame is attaching it
attach(mtcars)
search()
objects(2)
summary(mpg)

# Once the data frame is not needed anymore, it can be detached
detach(mtcars)
search()
summary(mpg)

# Data inside a data frame can be also used with command with
with(mtcars,summary(mpg))

# Finding and changing working directories
# Get actual working directory
getwd()
setwd("C:/Users/Maria Eglee//Dropbox//Grants//U54//R workshop//Slides")
getwd()

# Working directory can also be set using menus, both in R and in RStudio

# Reading data from delimited files

# We will use a dataset included in the Agresti and Franklin (2012) CD, 
# containing data on human development for year 2004 (PNUD)

human_development=read.table("./Datasets/human_development.txt",header=T,sep="\t")

# For avoiding the error, substitute the separator by a tab

human_development=read.table("./Datasets//human_development.txt",header=T,sep="\t")
class(human_development)
head(human_development,5)
human_development[24,]

# read.delim already assumes tabs as separators and expects column names
human_development=read.delim("./Datasets/human_development.txt")
head(human_development)

# read.csv allows to read .csv values (as produced by Excel)
HumanDevelopCsv=read.csv("./Datasets/human_development.csv")
head(HumanDevelopCsv,5)


# EXPLORING CATEGORICAL DATA

# Data frame survey in library MASS contains the responses of 237 Statistics I students 
# at the University of Adelaide to a questionaire including sex, 
# age, height, frequency of smoking, frequency of exercising and others.

require(MASS)
data(survey)
names(survey)
attach(survey)

# Consider the frequency of smoking
summary(Smoke)

# Redefine variable assigning different order to the levels
Smoke=factor(Smoke, levels=c("Never","Occas","Regul","Heavy"))
table(Smoke)

# Consider exercising frequency. Redefine to change order of levels
table(Exer)
Exer=factor(Exer,levels=c("Freq","Some", "None"))
table(Exer)

# Create a contingency table for Smoke and Exer
ExerSmoke.tab=table(Exer,Smoke)
ExerSmoke.tab

# Calculate row total, column total and table total
# Totals by row
apply(ExerSmoke.tab,1,sum)

# Totals by column
apply(ExerSmoke.tab,2,sum)

# Table total
sum(ExerSmoke.tab)

# Calculate a table of proportions by row
ExerSmoke.prop=ExerSmoke.tab/apply(ExerSmoke.tab,1,sum)
round(ExerSmoke.prop,3)

# BARPLOTS

# Barplot for the frequency of smoking
barplot(table(Smoke))

# A better presented version
 
barplot(table(Smoke),names=c("Never","Ocassionally", "Regularly","Heavily"),  
        xlab="Frequency of smoking", ylab="No. of responses",cex.names=0.8,col="lightblue")

# For obtaining a list of available colors
colors()
 
# Horizontal boxplot with class order reverted 
barplot(rev(table(Smoke)),names=rev(c("Never","Ocassionally","Regularly", "Heavily")), 
        ylab="Frequency of smoking", cex.names=0.7, col="lightblue",horiz=T)

#Side by side plot for contingency tables
 barplot(ExerSmoke.tab,beside=T,xlab="Frequency of smoking",col=terrain.colors(3))

# Add a legend to the plot
 legend(locator(1),fill=terrain.colors(3),legend=c("None","Some", "Frequent"), title="Frequency of excercise")

# Same graph in grayscale and locating the legend in the top right angle
barplot(ExerSmoke.tab,beside=T,xlab="Frequency of smoking", col=gray(seq(0,1,len=3)))
legend("topright",fill=gray(seq(0,1,len=3)),legend=c("None","Some","Frequent"), title="Frequency of excercise")

# MOSAIC PLOTS
# Mosaic plot for the contingency table of exercise vs smoking
mosaicplot(ExerSmoke.tab,col=terrain.colors(4),main=" Exercise frequency vs Smoking frequency",las=1,xlab="Excercise")
end{semiverbatim}

# EXPLORING QUANTITATIVE VARIABLES
 
# Read dataset
latam.frm <- read.csv("latam2011.csv")
attach(latam.frm)
latam.frm=latam2011
# Cleveland's dotchart for population
dotchart(TotalPop,labels=rownames(latam.frm),main="Population for Latin America \n Year 2011")
end{semiverbatim}

# Cleveland's dotchart with sorted data
dotchart(sort(TotalPop),labels=rownames(latam.frm)[order(TotalPop)],main="Population for Latin America \n Year 2011")

# \n represents a line break.
 
# strip chart for population
stripchart(TotalPop,xlab="Total Population (thousands)", pch=16)

# strip chart for BirthRate with data rounded
stripchart(round(BirthRate,0), xlab="BirthRate (x1,000)", pch=1)

# Same graph with points stacked
stripchart(round(BirthRate,0),method="stack",xlab="BirthRate (x1,000)", pch=16)

# Strip charts for comparing birth rates for South America and Central America
SouthAm=c(1,0,1,1,1,1,0,0,0,1,0,0,0,0,0,0,1,1,0,1,1)
stripchart(round(BirthRate,0)~SouthAm,method="stack", xlab="Birth Rate (x1,000)")

# Add text to the graph
text(locator(),c("South America","Central America and Caribbean"))

# HISTOGRAMS

# Histogram for population using Sturges' method
hist(TotalPop,main="Total Population")

# Histogram for population using Friedman-Diaconis method
hist(TotalPop,breaks="FD",main="Total Population")

# Histogram for life expectation. It is not plotted       
lehist = hist(LifeExp,plot=F)
lehist

# Use lehist for ploting a histogram with cumulative curve
sum(lehist$counts)
plot(lehist,ylim=c(0,21),xlab="Life Expectancy (Years)",main="Life Expectancy \n Latin America 2011",col="gray")
lines(lehist$breaks,c(0,cumsum(lehist$counts)))

# NUMERICAL DESCRIPTIVE MEASURES
# Centrality measures
mean(TotalPop) # mean
median(TotalPop) # median
mean(TotalPop,trim=0.05) # trimmed mean, 5% at each end
mean(TotalPop,trim=0.1) # trimmed mean, 10% at each end

# Dispersion measures
quantile(TotalPop) # minimum, maximum and quartiles
quantile(TotalPop,.75)-quantile(TotalPop,.25) # Interquartile range
sd(TotalPop) # Standard deviation

# summary produces five number summaries + means for single variables or data frames
summary(TotalPop)
summary(latam.frm[,1:4])

#BOXPLOT

# boxplot for the population
boxplot(TotalPop,ylab="Total Population (Thousands)")           

# boxplots for birth rate and death rate in the same graph
boxplot(BirthRate,DeathRate,names=c("Birth Rate","Death Rate"))

           
#Using boxplots for comparing variables for different levels of a factor           
class(Pulse)
class(Exer)


# split Pulse according to the levels of Exer and summarize each level
lapply(split(Pulse,Exer),summary)

# Produce boxplots for Pulse for each level of Exer
plot(Exer,Pulse)

# Add notches to boxplots for comparing medians
plot(Exer,Pulse,notch=T)

# EXPLORING RELATIONSHIPS BETWEEN QUANTITATIVE VARIABLES.

# Produce scatterplot of birth rate vs life expectancy
plot(LifeExp,BirthRate,xlab="Life Expectancy (Years)",ylab="Birth Rate (x 1000pop)")
 
# Ploting character can be changed
plot(LifeExp,BirthRate,xlab="Life Expectancy (Years)",ylab="Birth Rate (x 1000pop)",pch=16)
plot(LifeExp,BirthRate,xlab="Life Expectancy (Years)",ylab="Birth Rate (x 1000pop)",pch="X")

# pairs produces a matrix of scatterplots 
pairs(latam.frm)

# Correlation matrix for the columns of latam.frm
round(cor(latam.frm),3)

# It is possible to interact with graphs for identifying points
plot(LifeExp,DeathRate,xlab="Life Expectancy (Years)",ylab="Death Rate (x 1000 pop.)")
identify(LifeExp,DeathRate,labels=rownames(latam.frm))

# Graphs can be drawn in different stages for showing different features
# Select a subset of countries
countries=rownames(latam.frm)
selec=countries=="Bolivia"|countries=="Colombia"|countries=="PuertoRico"|countries=="Uruguay"|countries=="Venezuela"
selec

# Plot the two subsets of countries with different colors
# First create empty graph
plot(DeathRate,LifeExp,xlab="Death Rate (x1000)",ylab="Life Expectancy (years)",type="n")
# Add points
points(DeathRate[selec],LifeExp[selec],pch=16,col="red")
points(DeathRate[!selec],LifeExp[!selec],pch=16)
# Identify points and add labels
identify(DeathRate[selec],LifeExp[selec],pch=16,col="red",labels=countries[selec])

