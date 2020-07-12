#Load package
library(dplyr)

#Import data
train <- read.csv("train.csv")

#Structure of data
str(train)

#Change datatype of Date from "char" to "date"
train$Date <- as.Date(as.character(train$Date))
class(train$Date) #Changed to Date: Checked.
#Change StateHoliday to factor
train$DayOfWeek <- as.factor(as.integer(train$DayOfWeek))
train$StateHoliday <- as.factor(as.character(train$StateHoliday))
train$Open <- as.factor(as.character(train$Open))
train$Promo <- as.factor(as.character(train$Promo))
train$SchoolHoliday <- as.factor(as.character(train$SchoolHoliday))

#Check if there's any NA
table (complete.cases (train))
#no NULL values found

summary(train)

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

#Join table using dplyr
train_store <- merge(train, store, by = "Store")

