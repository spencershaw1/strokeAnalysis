#
# Stroke Prediction
# Spencer Shaw
# December 16, 2021
#
# An R Script to create models, plots, and learning algorithms that will 
# assist in predicting whether a given patient will have a stroke 


rm(list = ls())
library(ggplot2)
library(caTools)
require(ggiraph)
require(ggiraphExtra)
require(plyr)

# MAC
setwd("/Users/spencershaw/OneDrive\ -\ Clemson\ University/desktop/projects/strokeAnalysis/")

# PC
setwd("C:/Users/jspen/OneDrive\ -\ Clemson\ University/desktop/projects/strokeAnalysis/")


# Create a dataframe to work with
strokeData <- read.csv("data/healthcare-dataset-stroke-data.csv")
strokeData <- subset(strokeData, bmi != "N/A")
strokeData$bmi <- as.numeric(strokeData$bmi)

strokeData <- subset(strokeData, select = -c(id))

str(strokeData)

# EXPLORATORY DATA ANALYSIS

# p = 10 (exclude id and stroke - they aren't predictors)
names(strokeData)
ncol(strokeData)

# n = 4909
nrow(strokeData)

# 1) Average Glucose vs. Smoking Status
ggplot(strokeData, aes(smoking_status, avg_glucose_level)) + 
  geom_boxplot(color = "#2ed573") + ggtitle("Average Glucose Level vs. Smoking Status") + 
  ylab("Average Glucose Level") + xlab("Smoking Status")

# 2) Average Glucose vs. Smoking Status (split on stroke and no stroke)
ggplot(strokeData, aes(smoking_status, avg_glucose_level, color = factor(stroke))) +
  geom_boxplot() + ggtitle("Avereage Glucose Level vs. Smoking Status") + 
  ylab("Average Glucose Level") + xlab("Smoking Status") + labs(color = "Stroke")

# 3) Average Glucose vs. Stroke
ggplot(strokeData, aes(stroke, avg_glucose_level, group = stroke, color = factor(stroke))) + 
  geom_boxplot(size = 1) + ggtitle("Average Glucose Level vs. Stroke") + 
  ylab("Average Glucose Level") + xlab("Stroke") + labs(color = "Stroke")

# 4) Average Glucose vs. Stroke (with jitter)
ggplot(strokeData, aes(stroke, avg_glucose_level, group = stroke, color = factor(stroke))) + 
  geom_boxplot(size = 1) + geom_jitter() + ggtitle("Average Glucose Level vs. Stroke") + 
  ylab("Average Glucose Level") + xlab("Stroke") + labs(color = "Stroke")

# 5) Heart Disease and Stroke
ggplot(strokeData, aes(stroke, heart_disease, color = factor(stroke))) + geom_jitter() + 
  ggtitle("Heart Disease and Stroke") + ylab("Heart Disease") + xlab("Stroke") + 
  labs(color = "Stroke")

# 6) Smoking Status and Stroke
ggplot(strokeData, aes(stroke, smoking_status, color = factor(stroke))) + geom_jitter() + 
  ggtitle("Smoking Status and Stroke") + ylab("Smoking Status") + xlab("Stroke") + 
  labs(color = "Stroke")

# 7) Hypertension and Stroke
ggplot(strokeData, aes(stroke, hypertension, color = factor(stroke))) + geom_jitter() + 
  ggtitle("Hypertension and Stroke") + ylab("Hypertension") + xlab("Stroke") + 
  labs(color = "Stroke")

# 8) Ever Married and Stroke
ggplot(strokeData, aes(stroke, ever_married, color = factor(stroke))) + geom_jitter() + 
  ggtitle("Ever Married and Stroke") + ylab("Ever Married") + xlab("Stroke") + 
  labs(color = "Stroke")

# 9) Residence Type and Stroke
ggplot(strokeData, aes(stroke, Residence_type, color = factor(stroke))) + geom_jitter() + 
  ggtitle("Residence Type and Stroke") + ylab("Residence Type") + xlab("Stroke") + 
  labs(color = "Stroke")

# 10) Age vs BMI with no Stroke separation and no regression lines
ggplot(strokeData, aes(x = age, y = bmi)) + geom_point(size = 0.65, color = "#2ed573") +
  ylim(0, 65) + ggtitle("BMI vs. Age") + ylab("BMI") + xlab("Age")

# 11) Age vs BMI with no Stroke separation and linear regression
ggplot(strokeData, aes(x = age, y = bmi)) + geom_point(size = 0.65, color = "#2ed573") +
  ylim(0, 65) + geom_smooth(method = "lm", color = "#1e90ff") + ggtitle("BMI vs. Age") + 
  ylab("BMI") + xlab("Age")

# 12) Age vs BMI with no Stroke separation and loess regression
ggplot(strokeData, aes(x = age, y = bmi)) + geom_point(size = 0.65, color = "#2ed573") +
  ylim(0, 65) + geom_smooth(method = "loess", color = "#1e90ff") + 
  ggtitle("BMI vs. Age") + xlab("BMI") + ylab("Age")

# 13) Age vs BMI with Stroke separation and no regression lines
ggplot(strokeData, aes(x = age, y = bmi, color = factor(stroke))) + geom_point(size = 0.65) +
  ylim(0, 65) + ggtitle("BMI vs. Age") + ylab("BMI") + xlab("Age") + 
  labs(color = "Stroke")

# 14) Age vs BMI with Stroke separation and linear regression
ggplot(strokeData, aes(x = age, y = bmi, color = factor(stroke))) + geom_point(size = 0.65) +
  ylim(0, 65) + geom_smooth(method = "lm") + ggtitle("BMI vs. Age") + 
  ylab("BMI") + xlab("Age") + labs(color = "Stroke")

# 15) Age vs BMI with Stroke separation and loess regression
ggplot(strokeData, aes(x = age, y = bmi, color = factor(stroke))) + geom_point(size = 0.65) +
  ylim(0, 65) + geom_smooth(method = "loess") + ggtitle("BMI vs. Age") + 
  ylab("BMI") + xlab("Age") + labs(color = "Stroke")

# 16) Stroke occurrences
ggplot(strokeData, aes(stroke, fill = factor(stroke))) + geom_bar(width = 0.75) + 
  scale_x_discrete() + ggtitle("Stroke Occurences") + ylab("Counts") + 
  xlab("Stroke Status") + labs(fil = "Stroke")



# PREDICTION / ANALYSIS

# Converting qualitative data to numeric data
strokeData$ever_married <- as.factor(strokeData$ever_married)
strokeData$MARRIED <- as.numeric(strokeData$ever_married)

strokeData$work_type <- as.factor(strokeData$work_type)
strokeData$WORK <- as.numeric(strokeData$work_type)

strokeData$Residence_type <- as.factor(strokeData$Residence_type)
strokeData$RESIDENCE <- as.numeric(strokeData$Residence_type)

strokeData$smoking_status <- as.factor(strokeData$smoking_status)
strokeData$SMOKE <- as.numeric(strokeData$smoking_status)


# Logistic Regression
split = sample.split(strokeData$stroke, SplitRatio = 0.5)
train = subset(strokeData, split == TRUE)
test = subset(strokeData, split == FALSE)

glm.fit <- glm(stroke ~ age + hypertension + heart_disease +
                 MARRIED + WORK + RESIDENCE + avg_glucose_level + 
                 bmi + SMOKE, data = train, family = binomial)
summary(glm.fit)

glm.fit2 <- glm(stroke ~ avg_glucose_level * hypertension, data = train,
               family = binomial)
summary(glm.fit2)


ggPredict(glm.fit2, se = TRUE, digits = 3) + 
  ggtitle("Probability of Stroke") + ylab("Probability") + xlab("Average Glucose Level")

ggplot(strokeData, aes(x=avg_glucose_level, y=stroke, color = factor(hypertension))) + 
  geom_point() + geom_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  ggtitle("Probability of Stroke") + ylab("Probability") + xlab("Average Glucose Level") +
  labs(color = "Hypertension")


# Training Set Accuracy - Fit 1
predictTrain <- predict(glm.fit, data = train, type = "response")
table(train$stroke, predictTrain > 0.5)
(2348 + 3) / (2348 + 2 + 101 + 3)

# Test Set Accuracy - Fit 1
predictTest <- predict(glm.fit, newdata = test, type = "response")
table(test$stroke, predictTest > 0.5)
(2348 + 2) /(2348 + 2 + 103 + 2)

# Training Set Accuracy - Fit 2
predictTrain <- predict(glm.fit2, data = train, type = "response")
table(train$stroke, predictTrain > 0.5)
(2350 + 0) / (2350 + 0 + 104 + 0)

# Test Set Accuracy - Fit 2
predictTest <- predict(glm.fit2, newdata = test, type = "response")
table(test$stroke, predictTest > 0.5)
(2350 + 0) /(2348 + 0 + 105 + 0)




