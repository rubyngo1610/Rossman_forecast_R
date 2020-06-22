#Import data
train <- read.csv("train.csv")

#Check if there's any NA
table (complete.cases (train))
#no NULL values found

summary(train)
#Sales and Customers have outliers - Max values are much higher than the mean
boxplot.default(train$Customers,horizontal = TRUE)
#Verified: Customers have outliers
boxplot.default(train$Sales,horizontal = TRUE)
#Verified: Sales have outliers
