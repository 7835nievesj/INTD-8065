#Use the data set VA in library MASS (removing those patients who survived) for answering the following questions.

VA <- filter(VA, status == 1) 

##1.	Perform a Student-t test comparing survival times for different treatments, with and without the assumption of equal variances. 
##What is your conclusion? How does this compare with the side by side boxplots ?

# barplot
plot(VA$treat,VA$stime,
     main = "Survival Time by Treatment Type", 
     names = c("Standard","Test"),
     las = 1,
     xlab = "Treatment Type", 
     ylab = "Survival Time (Days)",
     col = 3)

treat <- as.numeric(VA$treat)
t.test(treat,stime, var.equal = TRUE) #t.test variance = True

t.test(treat,stime, var.equal = FALSE) #t.test variance = False

##2.	Repeat the analysis in previous question for the Total Time from Diagnosis to Death (this quantity was used in Homework #3).

diag.time.days <- VA$diag.time * 30
diag.death <- diag.time.days + VA$stime

#barplot
plot(VA$treat,diag.death,
     main = "Time From Diagnosis to Death by Treatment Type",
     names = c("Standard","Test"),
     las = 1,     
     xlab = "Treatment Type", 
     ylab = "Time From Diagnosis (Days)",
     col = 3)

t.test(treat, diag.death, var.equal = TRUE) #t.test variance = True

t.test(treat, diag.death, var.equal = FALSE) #t.test variance = False


##3.	Replicate questions 1 and 2, but now with the logarithm of the data. Compare the results you obtained with your previous answers.

## log of data
stime.log <- log(stime)
diag.death.log <- log(diag.death)

## barplot stime.log

plot(VA$treat, stime.log,
     main = "Log Survival Time by Treatment Type",
     names = c("Standard","Test"),
     las = 1,     
     xlab = "Treatment Type", 
     ylab = "Log Survival Time",
     col = 3)

## t.test stime.log

t.test(treat,stime.log, var.equal = TRUE) #t.test variance = True

t.test(treat,stime.log, var.equal = FALSE) #t.test variance = False

## barplot diag.death.log

plot(VA$treat, diag.death.log,
     main = "Log Time From Diagnosis to Death by Treatment Type",
     names = c("Standard","Test"),
     las = 1,     
     xlab = "Treatment Type", 
     ylab = "Log Time From Diagnosis",
     col = 3)

##t.test diag.death.log

t.test(treat,diag.death.log, var.equal = TRUE) #t.test variance = True

t.test(treat,diag.death.log, var.equal = FALSE) #t.test variance = False
                                                                                        