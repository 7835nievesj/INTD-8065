
# HOMEWORK 7

# Interpret the coefficients of the selected model for the prostate cancer 
# data in terms of odds. You can follow the analysis in the rotavirus 
# vaccine example.


# Make generalized linear model of prostate data frame
prostate.lm <- glm(Nodes ~ Xray + Stage, family = binomial)
summary(prostate.lm)

# The odds ratio corresponding to the Xray is
exp(2.1194)
#8.326

# The odds ratio corresponding to the Stage is
exp(1.5883)
#4.895

predictions <- cbind(Xray, Stage, 
                + round(predict(prostate.lm, type = "response"), 4))
