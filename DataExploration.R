#EXPLORATION
options("scipen" = 10) #for graph to show full number
library(ggplot2)
library(sqldf)
library(dplyr)

##Sales ~ Store
#Min,Max of mean sales
AA <- vector(mode = "numeric",length = 1115)
for (i in 1:1115) {
  AA[i] <- mean(train_store$Sales[train_store$Store==i])
}
match(max(AA),AA) #--> Store 262 has the max mean sales
match(min(AA),AA) #--> Store 307 has the min mean sales
#Number of stores in each range of sales
A <- vector(mode = "numeric",length = 1115)
for (i in 1:1115) {
  A[i] <- mean(train_store$Sales[train_store$Store==i])
}
summary(A) #get value of 1st quadrant and 3rd quadrant
#--> Sales vary from stores to stores --> Strong predictor value

##Sales ~ DayOfWeek
ggplot(train_store, aes(x=Date,y=Sales,color=DayOfWeek)) + geom_smooth()
#--> Day 7 is extremely low compared to other dates 
#Look closer at Day 7
train$DayOfWeek <- as.integer(as.factor(train$DayOfWeek))
Day7Sales <- subset(train_store,DayOfWeek==7)
summary(Day7Sales) #-> Many stores have 0 sales on Sunday
Day7Sales$Open <- as.integer(as.factor(Day7Sales$Open))
nrow(subset(Day7Sales,Open==0)) #--> All stores are open on Day 7, but some of them don't have any sales and customers at all
nrow(Day7Sales) - nrow(subset(Day7Sales,Sales==0))
#--> 3593/144730 records have sales on Day 7
sqldf("select Sales, sum(StateHoliday), sum(SchoolHoliday) from Day7Sales where Sales==0 group by Sales")
#On School Holiday that fell on Sunday, most stores saw no sales.
#However, on Sunday's State Holiday, every stores had sales (If it's not State Holiday, all Sunday's sales would be 0)

##Sales ~ Date
ggplot(train_store, aes(x=Date,y=Sales)) + geom_smooth()
#-->Sales increased from 2013 to 2015
ggplot(train_store, aes(x=Date,y=Customers)) + geom_smooth()
avgsalespercustomerbydate <- sqldf("select Date, sum(Sales)/Customers as Average from train_store where Sales>0 group by Date")
ggplot(avgsalespercustomerbydate, aes(x=Date, y=Average))+ geom_smooth()
#Near the end of 2014, Sales increased but Sales per Customers decreased.
#Thus, both Sales and Customers increased but they bought less than in the past. 
#--> Look closer into the end of 2014.

##Sales ~ Customers
C <- vector(mode = "numeric",length = 7)
for (i in 1:7) {
  C[i] <- sum(train_store$Customers[train_store$DayOfWeek==i])
}
barplot(C,main="Total customers by Day of Week",names.arg = dput(1:7))
#--> Day 7 saw a significantly lower number of customers, just like sales.
# Check for positive correlation
D <- matrix(1:1115,nrow=1115,ncol=1)
for (i in 1:1115) {
  D[i] <- sum(train_store$Customers[train_store$Store==i])
}
qplot(AA,D) + xlab("Sales") + ylab("Customers") + geom_smooth(method="lm")
#--> Store and Customers are positively related

##Sales ~ StateHoliday
OpenOnHoliday <- subset(train_store,StateHoliday != 0 & Open==1)
NotOpenOnHoliday <- subset(train_store,StateHoliday != 0 & Open==0)
nrow(OpenOnHoliday) 
nrow(NotOpenOnHoliday) #--> 910/31320 records open on holidays
a1 <- sum(OpenOnHoliday$Sales[OpenOnHoliday$StateHoliday=="a"])
b1 <- sum(OpenOnHoliday$Sales[OpenOnHoliday$StateHoliday=="b"])
c1 <- sum(OpenOnHoliday$Sales[OpenOnHoliday$StateHoliday=="c"])
barplot(c(a1,b1,c1),main="Total sales on each holiday",names.arg = c("a","b","c"))
#--> Within 910 stores that are open on holiday, public holiday (a) saw the 
# highest sale (5890305), significantly more than Easter (b) (1433744) and
# Christmas (c) (691806)

#Try to compare sales between dates with holiday and non-holiday --> Ask chi Phuong

##Sales ~ SchoolHoliday
m <- 0
n <- 0
for (j in 1:1115) {
  sales1 <- mean(train_store$Sales[train_store$Store==j & train_store$SchoolHoliday==1])
  sales2 <- mean(train_store$Sales[train_store$Store==j & train_store$SchoolHoliday==0])
  if (sales2-sales1 > 0) { m = m + 1 }
  else if (sales2-sales1 < 0) { n = n + 1 }
}
m
n
#--> Sales was significantly lower on school holidays
#--> Strong predictor value

##Sales ~ StoreType
a2 <- mean(train_store$Sales[train_store$StoreType=="a"])
b2 <- mean(train_store$Sales[train_store$StoreType=="b"])
c2 <- mean(train_store$Sales[train_store$StoreType=="c"])
d2 <- mean(train_store$Sales[train_store$StoreType=="d"])
barplot(c(a2,b2,c2,d2),main="Mean sales of each store type",names.arg = c("a","b","c","d"))

#--> Type b has the highest sales

#Determine the sales of each PromoInterval
boxplot(Sales ~ PromoInterval, data = train_store,
        main = "Sales based on the PromoInterval",
        xlab = "PromoInterval", ylab = "Sales", col = "blue")

# Take a closer look at the JAJO interval
summary(train_store[train_store$PromoInterval == "Jan,Apr,Jul,Oct",]$Sales)

sqldf("SELECT PromoInterval, COUNT(PromoInterval), FROM train_store WHERE Sales > 7614 + 1.5 * (7614-3680) GROUP BY PromoInterval")

#No interval stands out but Jan, Apr, Jun, Oct had the highest mean and number of outliers -> predictor value

#Determine the sales of each assortment
ggplot(train_store["Sales" != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(Assortment))) + 
  geom_smooth(size = 2)

#Average sales per assortment
a <- mean(train_store$Sales[train_store$Assortment == "a"])
b <- mean(train_store$Sales[train_store$Assortment == "b"])
c <- mean(train_store$Sales[train_store$Assortment == "c"])
barplot(c(a,b,c), main = "Average sales per assortment", names.arg = c("a","b","c"))

#Customers per assortment
a1 <- sum(train_store$Customers[train_store$Assortment == "a"])
b1 <- sum(train_store$Customers[train_store$Assortment == "b"])
c1 <- sum(train_store$Customers[train_store$Assortment == "c"])
barplot(c(a1,b1,c1), main = "Customers per assortment", names.arg = c("a","b","c"))
#Assortment b (b>c>a) had the best sales and always had higher sales than the other assortments, even its average sales. Furthermore, the number of customers who bought b are extremely low compare to other assortments'
#--> B's sales performance was good.So why the sales performance of a and c are worse? What qualities did b have?

#Correlation between Competition Distance and Sales
plot(Sales ~ CompetitionDistance, train_store)
#There is a clear correlation between sales and competition distance. The plot indicates that the closer the competitor, the lower the sales.

# Days since start of promo2
#Need to fix and try to find a way to leverage the data

# PromoContinuation vs Sales
boxplot(Sales ~ PromoContinuation, data = train_store,
        main = "Sales based on the PromoContinuation",
        xlab = "PromoContinuation", ylab = "Sales", col = "yellow")
# Sales when having a 2nd Promo were less than without a 2nd Promo but not significant
#Consider the promo data on Open days
row_to_keep = which(train_store$Open > 0)
openday <- train_store[row_to_keep,]
summary(openday)
#The numbers of PromoContinuation are nearly equal

#Sales between promo day and not promo day
ggplot(openday["Sales" != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(Promo))) + 
  geom_smooth(size = 2)
promoY <- mean(train_store$Sales[train_store$Promo == 1])
promoN <- mean(train_store$Sales[train_store$Promo == 0])
barplot(c(promoY,promoN), main = "Average sales per Promo", names.arg = c("1","0"))
#Sales nearly doubled when there was a promo on that day
