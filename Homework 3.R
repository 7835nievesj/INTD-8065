#INTD 8065 Data Analysis for Cancer Research
#Homework 3

#Use dataset VA in library MASS (removing those patients who survived) to answer the following questions:
attach(VA)
VA <- filter(VA, status == 1) 

#1.	Compare survival times for different treatments using side by side boxplots. Make the same comparison for the total time from diagnosis to death.

# Produce boxplots for Survival Time to death for each level of Treatment

plot(treat,stime,
        main = "Survival Time by Treatment Type", 
        names = c("Standard","Test"),
        las = 1,
        xlab = "Treatment Type", 
        ylab = "Survival Time (Days)",
        col = 3)

# Produce boxplots from Diagnostic Time to death for each level of Treatment (Sum stime & diag.time) # change months to days

diag.time.days <- diag.time * 30
diag.death <- diag.time.days + stime
plot(treat,diag.death,
        main = "Time From Diagnosis to Death by Treatment Type",
        names = c("Standard","Test"),
        las = 1,     
        xlab = "Treatment Type", 
        ylab = "Time From Diagnosis (Days)",
        col = 3)


#2.	Explore the existence of association between the Karnofsky score and the survival time using graphs and numerical measures. 

plot(Karn,stime,
     main = "Association Between Karnofsky Score and Survival Time",
     ylab = "Survival Time (days)",
     xlab = "Karnofsky score",
     las = 1,
     col = 3)
cor(Karn,stime, method = 'pearson') #0.4090833
cor(Karn,stime, method = 'spearman') #0.5986425

#Hint: Consider both the survival time and its logarithm. Do you find differences?

stime.log <- log(stime)
plot(Karn,stime.log,
     main = "Association Between Karnofsky Score and Log Survival Time",
     ylab = "Log Survival Time (days)",
     xlab = "Karnofsky score",
     las = 1,
     col= 3)
cor(Karn, stime.log, method = 'pearson') #0.584689
cor(Karn, stime.log, method = 'spearman') #0.5986425

#3.	Comment any other result you consider relevant. Write a general conclusion of your analysis.
pairs(VA, col = 3)
