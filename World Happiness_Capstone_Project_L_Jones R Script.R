setwd("~/R/Dataset")
WorldHappiness<- read.csv("~/R/Dataset/WorldHappiness Capstone.csv")
View(WorldHappiness)
WorldHappiness<- read.csv("~/R/Dataset/WorldHappiness Capstone.csv")
View(WorldHappiness)
attach(WorldHappiness)

#load libraries
library(glm2)
library(caret)
library(rpart)
library(caret)
library(dplyr)
library(MASS)
library(ggplot2)
library(reshape2)

#DATA EXPLORATION

#View the headers
 
head(WorldHappiness)
 

#View the dimension of the dataset
 
dim(WorldHappiness)
 

#View the dataset attributes - class
 
sapply(WorldHappiness,class)
 

#Summary of WorldHappiness 2017 dataset
 
summary(WorldHappiness)
 

#Review the structure of the WorldHappines 2017 dataset
 
str(WorldHappiness)
 
#METHODS & ANALYSIS

#Multiple Linear Regression 
#Create a scatterplot with freedom and family as the x value and happiness.score as the y value to visualize the data
 
plot(WorldHappiness$Freedom+WorldHappiness$Family,WorldHappiness$Happiness.Score,main="Happiness Score and Freedom",col="dark green", xlab="Freedom & Family /n Figure 1")
 

#Correlation the relationsip between Freedom and Family and happiness.score
 
cor(WorldHappiness$Freedom+WorldHappiness$Family,WorldHappiness$Happiness.Score)
 

#Fit the linear regression model. Using this to scale the relationship between the freedom,family and generosity (x) and happiness.Score (y)
 
lr_model<-lm(WorldHappiness$Happiness.Score~WorldHappiness$Freedom+WorldHappiness$Family)
lr_model
 

#Correlation test of freedom and family
 
cor.test(WorldHappiness$Freedom,WorldHappiness$Family)
 
#Plot Pearson's product-moment correlation
 
cor(WorldHappiness[,6:12])
plot((WorldHappiness[,6:12]))
 


#summary of the model results
 
summary(lr_model)
 


#Scatterplot with Freedom
 
plot(WorldHappiness$Freedom+WorldHappiness$Family,WorldHappiness$Happiness.Score,main="Scatterplot of Freedom & Happiness Score",pch=17, col="orange",xlab="Freedom \n Figure 1")
abline(lr_model)
 

par(mfrow=c(1,2))
plot(WorldHappiness$Freedom,WorldHappiness$Happiness.Score,main="Scatterplot of Freedom & Happiness Score",pch=17, col="orange",xlab="Freedom \n Figure 1")
abline(lr_model)
plot(WorldHappiness$Family,WorldHappiness$Happiness.Score,main="Scatterplot of Freedom & Happiness Score",pch=7, col="blue",xlab="Freedom \n Figure 2")
abline(lr_model)
 

#Identify the coefficient intercept
 
confint(lr_model)
 

#Identify the model coefficient
 
coef(lr_model)
anova(lr_model)
 

#Plot the lr_model  
 
plot(lr_model)
abline(lr_model)
par(mfrow=c(2,2))
 

#Make a prediction
 
predict(lr_model)
 
