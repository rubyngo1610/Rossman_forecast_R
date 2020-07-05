mergedata <- read.csv("merge.csv")
options("scipen" = 10) #for graph to show full number
library(ggplot2)

##Sales ~ Store
#Min,Max of total sales
AA <- vector(mode = "numeric",length = 1115)
for (i in 1:1115) {
  AA[i] <- sum(mergedata$Sales[mergedata$Store==i])
}
match(max(AA),AA) #--> Store 262 has the max total sales
match(min(AA),AA) #--> Store 307 has the min total sales
#Look at total sales for the first 100 stores
A <- vector(mode = "numeric",length = 100)
for (i in 1:100) {
  A[i] <- sum(mergedata$Sales[mergedata$Store==i])
}
a <- as.character(as.numeric(1:100))
barplot(A,main="Total sales by Store",names.arg = dput(a),xlab="Store")
#--> Sales vary from stores to stores --> Strong predictor value

##Sales ~ DayOfWeek
mergedata$DayOfWeek <- as.factor(as.integer(mergedata$DayOfWeek))
B <- vector(mode = "numeric",length = 7)
for (i in 1:7) {
  B[i] <- sum(mergedata$Sales[mergedata$DayOfWeek==i])
}
b <- as.character(as.numeric(1:7))
barplot(B,main="Total sales by Day of Week",names.arg = dput(b))
#--> Day 7 is extremely low compared to other dates --> Strong predictor value

##Sales ~ Date
Day1 <- subset(mergedata,Date == "2015-07-31",select = c(Store,Sales))
E <- vector(mode = "numeric",length = 50)
for (i in 1:50) {
  E[i] <- Day1$Sales[Day1$Store == i]
}
e <- as.character(as.numeric(1:50))
barplot(E,main="Sales each store on 7/31/2015",names.arg = dput(e))
#--> Sales on the same date varies significantly --> Check for correlations
# with StoreType, Assortments, CompetitorDistance, Promo, Promo2, PromoInterval

##Sales ~ Customers
C <- vector(mode = "numeric",length = 7)
for (i in 1:7) {
  C[i] <- sum(mergedata$Customers[mergedata$DayOfWeek==i])
}
barplot(C,main="Total customers by Day of Week",names.arg = dput(b))
#--> Day 1 the highest volume, 
# Check for positive correlation
D <- matrix(1:StoreTotal,nrow=StoreTotal,ncol=1)
for (i in 1:StoreTotal) {
  D[i] <- sum(mergedata$Customers[mergedata$Store==i])
}
qplot(A,D) + xlab("Sales")+ ylab("Customers") +geom_smooth(method="lm")
#--> Store and Customers are positively related

##Sales ~ Open
##Sales ~ StateHoliday
OpenOnHoliday <- subset(mergedata,StateHoliday != 0 & Open==1)
NotOpenOnHoliday <- subset(mergedata,StateHoliday != 0 & Open==0)
nrow(OpenOnHoliday) 
nrow(NotOpenOnHoliday) #--> 910/31320 records open on holidays
a1 <- sum(OpenOnHoliday$Sales[OpenOnHoliday$StateHoliday=="a"])
b1 <- sum(OpenOnHoliday$Sales[OpenOnHoliday$StateHoliday=="b"])
c1 <- sum(OpenOnHoliday$Sales[OpenOnHoliday$StateHoliday=="c"])
barplot(c(a1,b1,c1),main="Total sales on each holiday",names.arg = c("a","b","c"))
#--> Within 910 stores that are open on holiday, public holiday (a) saw the 
# highest sale (5890305), significantly more than Easter (b) (1433744) and
# Christmas (c) (691806)

##Sales ~ SchoolHoliday
m <-  0
for (n in 1:1115) {
  sales1 <- mean(mergedata$Sales[mergedata$Store==n & mergedata$SchoolHoliday==1])
  sales2 <-mean(mergedata$Sales[mergedata$Store==n & mergedata$SchoolHoliday==0])
  if (abs(sales2-sales1) > 500) { m = m + 1 }
}
m
#--> 866/1115 stores that was significantly affected by school holidays
#--> Strong predictor value

##Sales ~ StoreType
a2 <- sum(mergedata$Sales[mergedata$StoreType=="a"])
b2 <- sum(mergedata$Sales[mergedata$StoreType=="b"])
c2 <- sum(mergedata$Sales[mergedata$StoreType=="c"])
d <- sum(mergedata$Sales[mergedata$StoreType=="d"])
barplot(c(a2,b2,c2,d),main="Total sales of each store type",names.arg = c("a","b","c","d"))
#--> Type a has the highest sales, with b is the lowest (a>d>c>b)