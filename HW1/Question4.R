#####################################################################################
## HW1 question 4
## Author: Sai Lone
## Created: 09/15
## Edited: 09/16
#####################################################################################


rm(list = ls())

setwd("/Users/sailone/Desktop/fall19/EAS506/HW1")
# install.packages("MASS")
# install.packages("ggplot2")
libary(ggplot2)
library(MASS)


############################
# Load the data
############################
data(Boston)
?Boston
names(Boston)

############################
# a) Make pairwise scatterplots of the predictors, and describe your findings.
############################

boston <- Boston
plot(boston)

############################
# b) Are any of the predictors associated with per capita crime rate?
############################

correlation <- cor(boston$crim, boston.-crim)

large_model <- lm(crim ~ ., data= boston)
summary(large_model)

# strong correlation can be seen in dis, rad, medv

############################
# c) Do any of the suburbs of Boston appear to have particularly high crime rates?
#    Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.
############################

par(mfrow = c(1,3))
attach(boston)
hist(boston$crim)
hist(boston$ptratio)
hist(boston$tax)

############################
# d) In this data set, how many of the suburbs average more than seen rooms per dwelling? 
#    More than eight rooms per dwelling? Comment on the suburbs that average more than 
#  	 eight rooms per dwelling.Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.
############################

dim(subset(boston, rm>7))
# [1] 64 14
# There are 64 suburbs with more than 7 rooms per dwelling.

subs_more_then_8 <- subset(boston, rm>8)
dim(subs_more_then_8)
# [1] 13 14
# There are 13 suburbs with more than 8 rooms per dwelling.

par(mfrow = c(1,3))
attach(subs_more_then_8)
hist(subs_more_then_8$crim)
hist(subs_more_then_8$ptratio)
hist(subs_more_then_8$tax)





















