# Examples of mathematical operations and functions in R
7+2*3
(7+2)*3
12/2+4
5+2^3
4*sqrt(2)
log(sqrt(2)+1)

# Numbers, characters or results of mathematical operations 
# can be assigned to variables
x<-5+6^2
x
y=8+2
y
x=9*sqrt(20)
x
name1="Maria"
name1

# Listing workspace and erasing variables
ls()
objects()
rm(x,y)
ls()

# Creating vectors by concatenating
x=c(1,2,3,4,5)
x
x=c(x,6)
x
y=c(20,40,60)
y
z=c(x,y)
z

# Creating vectors using regular sequences
seq(1,10)
seq(1960,2000,5)
seq(0,1,.1)
seq(1,0,-0.1)

# Short versions for increment=1 or -1
seq(10)
1:10
10:1

# Creating vectors repeating regular patterns
rep(1,10)
rep(1:5,2)
ind=rep(c("yes","no"),3)
ind
# vector with logical values
logic=rep(c(TRUE,FALSE),5)
logic
rep(1:3,1:3)
rep(1:3,length=10)

# Mathematical operations and functions for vectors
a=1:10
b=a+5
b
a+b
c=sqrt(a)
c
d=10^-a
d

# Extracting elements from a vector
c
c[3]
c[10]
c[11]

# Classes
class(c)
class(ind)
class(logic)
as.numeric(logic)
as.character(a)

# Data frames
data(mtcars)
?mtcars
head(mtcars)
names(mtcars)
head(rownames(mtcars),5)
dim(mtcars) # Getting the size of the data frame

# Extracting rows and columns of a data frame
mtcars$mpg
mtcars[2,3]
mtcars["Mazda RX4 Wag","disp"]
mtcars["Mazda RX4 Wag",]
mtcars[10:15,"hp"]
class(mtcars[10:15,1:2])

# Attaching data frames
attach(mtcars)
search()
objects(2)
summary(mpg)
detach(mtcars)
search()
summary(mpg)


# Using with
with(mtcars,summary(mpg))

# Manipulating data frames with dplyr
library(dplyr)
mech4c=filter(mtcars,cyl==4&am==1)
View(mech4c)
class(mech4c)
mpg.4cm=select(mech4c,mpg)
View(mpg.4cm)
class(mpg.4cm)
mpg.4cm=unlist(mpg.4cm)
class(mpg.4cm)
mph.4cm=filter(mtcars,cyl==4&am==1) %>% select(mpg) %>% unlist()

#Reading data: Reading from the keyboard
x=scan()
x
y=scan(,what=list(0,""))
y
class(y)
unlist(y[[1]])
unlist(y[[2]])

# Reading data: Reading delimited files
HumanDevelopTxt=read.table("./Datasets/human_development.txt",header=T,sep="")
HumanDevelopTxt=read.table("./Datasets/human_development.txt",header=T,sep="\t")
class(HumanDevelopTxt)
head(HumanDevelopTxt,5)
HumanDevelopTxt[24,]

HumanDevelopTxt=read.delim("./Datasets/human_development.txt")
head(HumanDevelopTxt)

HumanDevelopCsv=read.csv("./Datasets/human_development.csv")
head(HumanDevelopCsv,5)

require(foreign)
HumanDevelopSpss=read.spss("./Datasets/human_development.sav",to.data.frame=T)
head(HumanDevelopSpss,5)





