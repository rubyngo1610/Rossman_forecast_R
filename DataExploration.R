#EXPLORATION
options("scipen" = 10) #for graph to show full number
library(ggplot2)
library(sqldf)

##Sales ~ Store
#Min,Max of total sales
AA <- vector(mode = "numeric",length = 1115)
for (i in 1:1115) {
  AA[i] <- sum(train_store$Sales[train_store$Store==i])
}
match(max(AA),AA) #--> Store 262 has the max total sales
match(min(AA),AA) #--> Store 307 has the min total sales
#Number of stores in each range of sales
A <- vector(mode = "numeric",length = 1115)
for (i in 1:1115) {
  A[i] <- mean(train_store$Sales[train_store$Store==i])
}
summary(A) #get value of 1st quadrant and 3rd quadrant
firstquantile <- A[A<=4412] 
middle <- A[A>4412 & A<6634]
thirdquantile <- A[A>=6634] 
barplot(c(length(firstquantile),length(middle),length(thirdquantile))
,main="Number of stores by sales range",names.arg = c("1st quantile",
"Middle","3rd quantile"),xlab="Sales",ylab="No. of stores")
#--> Sales vary from stores to stores --> Strong predictor value

##Sales ~ DayOfWeek
ggplot(train_store, aes(x=Date,y=Sales,color=DayOfWeek)) + geom_smooth()
#--> Day 7 is extremely low compared to other dates --> Strong predictor value
#Look closer at Day 7
train$DayOfWeek <- as.integer(as.factor(train$DayOfWeek))
Day7Sales <- subset(train_store,DayOfWeek==7)
summary(Day7Sales) #-> Many stores have 0 sales on Sunday
Day7Sales$Open <- as.integer(as.factor(Day7Sales$Open))
nrow(subset(Day7Sales,Open==0)) #--> All stores are open on Day 7,
#but some of them don't have any sales and customers at all
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
barplot(C,main="Total customers by Day of Week",names.arg = dput(b))
#--> Day 1 the highest volume, 
# Check for positive correlation
D <- matrix(1:1115,nrow=1115,ncol=1)
for (i in 1:1115) {
  D[i] <- sum(train_store$Customers[train_store$Store==i])
}
qplot(AA,D) + xlab("Sales") + ylab("Customers") + geom_smooth(method="lm")
#--> Store and Customers are positively related

##Sales ~ Open
#Some stores were opened but didn't have any sales

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
