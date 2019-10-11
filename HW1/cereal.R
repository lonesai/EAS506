#####################################################################################
## Pre_mining examples
## Author: Sai Lone
## Created: 08/31
## Edited:  09/16
#####################################################################################

rm(list = ls())

# set working director
setwd("/Users/sailone/Desktop/fall19/EAS506/HW1")

install.packages("ggplot2")
install.packages("ggvis")

library(ggplot2)
library(ggvis)
library(readr)

# read cereal.cvs 
cereal <- read.csv("cereal.csv", header = TRUE, sep=',')

#####################################################
######### 1)  Exploratory data analysis
#####################################################
# get info on the cereal data set
names(cereal)
dim(cereal)
summary(cereal)

# Pre- process this data (e.g. transformations, elimination of outliers, noise removal, imputation for missing data)
#####################################################

# Convert cereal$mfr, cereal$type and cereal$shelf to factor
cereal$mfr <- factor(cereal$mfr)
cereal$type <- factor(cereal$type)
cereal$shelf <- factor(cereal$shelf)
summary(cereal)

# Plots on different field that correlate with ratings

plot(cereal)
boxplot(cereal)
plot(cereal$rating, cereal$calories, 
	main="rating vs calories", 
	ylab = "calories", xlab = "ratings")
plot(cereal$rating, cereal$sugars , 
	main="rating vs sugars", 
	ylab = "sugars", xlab = "ratings")

# Remove outliers
cereal <- subset(cereal, sugars > 0)
cereal <- subset(cereal, calories > 0)

# Save an object to a file
save(cereal, file = "cleaned_data.RData")

rate_sug_cal <- cbind(cereal$rating, cereal$sugars, cereal$calories)
colnames(rate_sug_cal) <- c("Rating", "Sugar","Calories")

# Converting matrix to data_frame
rate_sug_cal <- as.data.frame(rate_sug_cal)

summary(rate_sug_cal)

#####################################################
######### 2)  Perform a multiple regression on the dataset
#####################################################


large_model <- lm(rating ~. - name - mfr, data=cereal)
summary(large_model)

plot(rate_sug_cal)
small_model <- lm(Rating ~ Sugar + Calories, data=rate_sug_cal)
summary(small_model)

model <- lm(rating ~. - name - mfr *calories:sugars, data=cereal)
summary(model)





