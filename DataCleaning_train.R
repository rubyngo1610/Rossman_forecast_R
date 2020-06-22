#Import data
train <- read.csv("train.csv")

#Structure of data
str(train)

#Check if there's any NA
table (complete.cases (train))
#no NULL values found

summary(train)
#Sales and Customers have outliers - Max values are much higher than the mean
boxplot.default(train$Customers,horizontal = TRUE)
#Verified: Customers have outliers
boxplot.default(train$Sales,horizontal = TRUE)
#Verified: Sales have outliers

#Assigning column names for store
colNames <- c ("Store", "StoreType",	"Assortment",	"CompetitionDistance",	
              "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear",	
              "PromoParticipation",	"PromoParticipationSinceWeek",	
              "PromoParticipationSinceYear", "PromoInterval")

#Read store data
store <- read.table ("store.csv", header = TRUE, sep = ",",
                       strip.white = TRUE, col.names = colNames,
                       na.strings = "?", stringsAsFactors = TRUE)
#Display structure of the data
str (store)

#NA cases
table (complete.cases (store))

#Table summary
summary(store)

#Replace the NAs in Competition Distance by its median
store$CompetitionDistance[is.na(store$CompetitionDistance)] <- median(store$CompetitionDistance, na.rm=TRUE)

#Replace the remaining NA's by 0
store[is.na(store)] <- 0
