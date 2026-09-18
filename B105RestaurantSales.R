#import the required dataset from system
tips <- read.csv("tipsn.csv")
# Display first few rows from the dataset
head(tips)
# Display dimensions
dim(tips)
# Display summary
summary(tips)
#Checking Missing Values
colSums(is.na(tips))
#checking duplicates
sum(duplicated(tips))
#removing duplicates
tipsnew<- tips[!duplicated(tips), ]
dim(tipsnew)
#converting categorical to factors
tipsnew$gender <- as.factor(tipsnew$gender)
tipsnew$smoker <- as.factor(tipsnew$smoker)
tipsnew$day <- as.factor(tipsnew$day)
tipsnew$time <- as.factor(tipsnew$time)
str(tipsnew)
#sampling for illustration
set.seed(111)
sampldata<- tipsnew[  sample(
    1:nrow(tipsnew),
    size = round(0.85*nrow(tipsnew))
  ),
]
dim(sampldata)
#Descriptive statistics
mean(tipsnew$total_bill)
median(tipsnew$total_bill)
sd(tipsnew$total_bill)
min(tipsnew$total_bill)
max(tipsnew$total_bill)
quantile(tipsnew$total_bill)
#Descriptive statistics tip
mean(tipsnew$tip)
median(tipsnew$tip)
sd(tipsnew$tip)
min(tipsnew$tip)D
max(tipsnew$tip)
quantile(tipsnew$tip)
#Descriptive statistics size
mean(tipsnew$size)
median(tipsnew$size)
sd(tipsnew$size)
min(tipsnew$size)
max(tipsnew$size)
quantile(tipsnew$size)
#frequency of different type based on gender
table(tipsnew$gender)
#frequency of different smoker or non smoker
table(tipsnew$smoker)
#frequency of different in lunch/dinner
table(tipsnew$time)
#frequency of different days
table(tipsnew$day)
hist(
  tipsnew$total_bill,
  main = "Toal Bill distribution of restaruant data",
  xlab = "Total Bill (???)",
  ylab = "Frequency",
  col = "red",
  border = "black"
)

hist(
  tipsnew$tip,
  main = "Tip Amount Distribution",
  xlab = "Tip (???)",
  ylab = "Frequency",
  col = "green",
  border = "black"
)

plot(
  tipsnew$total_bill,tipsnew$tip,main="Total Bill vs Tip",col='red',xlab="Total Bill (???)",ylab="Tip (???)",pch=18)

cor.test(  tipsnew$total_bill,   tipsnew$tip,   method = "pearson" )

tm_test<-t.test(tip ~ time,   data = tipsnew,   var.equal = FALSE)

tm_test

gender_test<- t.test( tip ~ gender,   data = tipsnew,   var.equal = FALSE)

gender_test

plot(   tipsnew$size,   tipsnew$tip,   main = "Table Size vs Tip",   xlab = "Number of People",
  ylab = "Tip (???)",   pch = 18 )

cor.test(  tipsnew$size,  tipsnew$tip,  method = "pearson")


multiple_model<-lm(   tip ~ total_bill +     size +     gender +     smoker +     time,   data = tipsnew )

summary(multiple_model)