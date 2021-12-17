#
# Stroke Prediction
# Spencer Shaw
# December 16, 2021
#
# An R Script to create models and learning algorithms that will assist in
# predicting whether a patient will have a stroke 


rm(list = ls())
library(ggplot2)

# MAC
setwd("/Users/spencershaw/OneDrive\ -\ Clemson\ University/desktop/projects/strokeAnalysis/")

# PC
setwd("C:/Users/jspen/OneDrive\ -\ Clemson\ University/desktop/projects/strokeAnalysis/")


# Create a dataframe to work with
strokeData <- read.csv("data/healthcare-dataset-stroke-data.csv")
strokeData <- subset(strokeData, bmi != "N/A")

strokeData$bmi <- as.numeric(strokeData$bmi)

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




# PREDICTION / ANALYSIS