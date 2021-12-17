#
# Stroke Prediction
# Spencer Shaw
# December 16, 2021
#
# An R Script to create models and learning algorithms that will assist in
# predicting whether a patient will have a stroke 


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

# Average Glucose vs. Smoking Status
ggplot(strokeData, aes(smoking_status, avg_glucose_level)) + geom_boxplot(color = "#2ed573")

# Average Glucose vs. Smoking Status (split on stroke and no stroke)
ggplot(strokeData, aes(smoking_status, avg_glucose_level, color = factor(stroke))) +
  geom_boxplot()

# Average Glucose vs. Stroke
ggplot(strokeData, aes(stroke, avg_glucose_level, group = stroke, color = factor(stroke))) + 
  geom_boxplot(size = 1)

# Average Glucose vs. Stroke (with jitter)
ggplot(strokeData, aes(stroke, avg_glucose_level, group = stroke, color = factor(stroke))) + 
  geom_boxplot(size = 1) + geom_jitter()

# Heart Disease and Stroke
ggplot(strokeData, aes(stroke, heart_disease, color = factor(stroke))) + geom_jitter()

# Smoking Status and Stroke
ggplot(strokeData, aes(stroke, smoking_status, color = factor(stroke))) + geom_jitter()

# Hypertension and Stroke
ggplot(strokeData, aes(stroke, hypertension, color = factor(stroke))) + geom_jitter()

# Ever Married and Stroke
ggplot(strokeData, aes(stroke, ever_married, color = factor(stroke))) + geom_jitter()

# Residence Type and Stroke
ggplot(strokeData, aes(stroke, Residence_type, color = factor(stroke))) + geom_jitter()

# Age vs BMI with no Stroke separation and no regression lines
ggplot(strokeData, aes(x = age, y = bmi)) + geom_point(size = 0.65, color = "#2ed573") +
  ylim(0, 65)

# Age vs BMI with no Stroke separation and linear regression
ggplot(strokeData, aes(x = age, y = bmi)) + geom_point(size = 0.65, color = "#2ed573") +
  ylim(0, 65) + geom_smooth(method = "lm", color = "#1e90ff")

# Age vs BMI with no Stroke separation and loess regression
ggplot(strokeData, aes(x = age, y = bmi)) + geom_point(size = 0.65, color = "#2ed573") +
  ylim(0, 65) + geom_smooth(method = "loess", color = "#1e90ff")

# Age vs BMI with Stroke separation and no regression lines
ggplot(strokeData, aes(x = age, y = bmi, color = factor(stroke))) + geom_point(size = 0.65) +
  ylim(0, 65)

# Age vs BMI with Stroke separation and linear regression
ggplot(strokeData, aes(x = age, y = bmi, color = factor(stroke))) + geom_point(size = 0.65) +
  ylim(0, 65) + geom_smooth(method = "lm")

# Age vs BMI with Stroke separation and loess regression
ggplot(strokeData, aes(x = age, y = bmi, color = factor(stroke))) + geom_point(size = 0.65) +
  ylim(0, 65) + geom_smooth(method = "loess")

# Stroke occurences
ggplot(strokeData, aes(stroke, fill = factor(stroke))) + geom_bar(width = 0.75) + 
  scale_x_discrete()



# PREDICTION / ANALYSIS
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

ggPredict(glm.fit2, se = TRUE, digits = 3)

ggplot(strokeData, aes(x=avg_glucose_level, y=stroke, color = factor(hypertension))) + 
  geom_point() + geom_smooth(method="glm", se=FALSE, method.args = list(family=binomial))


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




