# R Demo Worked Example Script
setwd("C:/Users/bradleyw/Dropbox/GitHub/HDRSeminar_Programming")

# Load in Data
library(haven)
diabetes <- read_sav("data/diabetes.sav")

# Allocate labels to categorical variable
diabetes$Diabetes <- factor(diabetes$Diabetes,levels = c(2,1))
levels(diabetes$Diabetes) <- c("No Diabetes","Has Diabetes")

# Proportion of patients with and without diabetes
table(diabetes$Diabetes)/nrow(diabetes)

# Difference between groups
aggregate(gluc ~ Diabetes,FUN = summary,data=diabetes)
aggregate(bmi ~ Diabetes,FUN = summary,data=diabetes)
aggregate(age ~ Diabetes,FUN = summary,data=diabetes)

boxplot(gluc ~ Diabetes,data=diabetes)
boxplot(bmi ~ Diabetes,data=diabetes)
boxplot(age ~ Diabetes,data=diabetes)

# Plot of data 
library(ggplot2)
ggplot(diabetes) + 
  geom_point(aes(x=bmi,y=gluc,col=Diabetes))

# Fit a model (we are after a logistic regression)

diabetes_model <- glm(Diabetes ~ gluc + bmi + age,
                      data=diabetes,
                      family = binomial(link = "logit"))
summary(diabetes_model)

# Compute Odds Ratios and 95% CI
exp(coef(diabetes_model))
exp(confint(diabetes_model))