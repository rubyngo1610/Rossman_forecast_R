####################
#Linear regression 
####################

#Import data
train <- read.csv("train.csv")
store <- read.csv("store.csv")
train_store <- merge(train,store,by="Store")

#Split data
index = sample(2,nrow(train_store),replace = TRUE, prob=c(0.7,0.3))
train70 <- train_store[index==1,]
train30 <- train_store[index==2,]

#Build the model
lrMod <- lm(Sales ~ Store + Open + DayOfWeek + StateHoliday + Promo + StoreType + Assortment + Promo2, data=train70)  
pred <- predict(lrMod, train30)
summary (lrMod) #Adjusted R-squared:  0.562

#Calculating Prediction Accuracy
actuals_preds <- data.frame(cbind(actuals=train30$Sales, predicteds=pred))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

#Accuracy: 74.9%

#Apply the model
test <- read.csv("test.csv")
test_store <- merge(test,store,by="Store")

lrMod <- lm(Sales ~ Store + Open + DayOfWeek + StateHoliday + Promo + StoreType + Assortment + Promo2, data=train_store)  
pred <- predict(lrMod, test_store)
summary (lrMod) #Adjusted R-squared:  0.5626

#Save the predicted sales to file .csv
sales_forecast <- data.frame(Id=test_store$Id, Sales=pred)
write.csv(sales_forecast,"LinearRegressionRossmann.csv")