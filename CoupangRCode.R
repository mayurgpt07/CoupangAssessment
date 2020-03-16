setwd('D:/InterviewPrep/')

#Load Libraries Required
library(dplyr)

#Read Data from CSV file
salesData <- read.csv('sample_sales_data_original.csv', header = T)
View(salesData)

#Convert Sales price to its absoluteValue
salesData <- salesData %>% mutate(sales_price_modified = abs(sale_price))

plot(salesData$sales_price_modified, ylab = "Sales Price per Order($)", main = "Scatter Plot for Sales Price per Order")
summary(salesData)
quantile(salesData$sales_price_modified, 0.99)
  
#Calculation of spend per buyer
SpendingPerBuyer <- salesData %>%group_by(user_id, test_option) %>% summarise(SpendingPerBuyer = sum(sales_price_modified, na.rm = T))
#SpendingPerBuyer <- salesData %>% filter(user_id != 13616) %>%group_by(user_id, test_option) %>% summarise(SpendingPerBuyer = sum(sales_price_modified, na.rm = T))



#Observe the distribution
hist(SpendingPerBuyer$SpendingPerBuyer,xlab = "Spend Per Buyer($)", ylab = "Number of Buyers", main = "Distribution of Spending Per Buyer")
options(scipen = 7)

#Seperating the two cases
SpendingOption1 = SpendingPerBuyer[SpendingPerBuyer$test_option == 1, ]
hist(SpendingOption1$SpendingPerBuyer,xlab = "Spend Per Buyer($)", ylab = "Number of Buyers", main = "Distribution of Spending Per Buyer for Treatment Group")

meanOption1 = SpendingOption1%>% group_by(test_option) %>% summarise(MeanOfGroup = mean(SpendingPerBuyer, na.rm = T))

SpendingOption0 = SpendingPerBuyer[SpendingPerBuyer$test_option == 0, ]
hist(SpendingOption0$SpendingPerBuyer,xlab = "Spend Per Buyer($)", ylab = "Number of Buyers", main = "Distribution of Spending Per Buyer for Control Group")


meanOption0 = SpendingOption0%>% group_by(test_option) %>% summarise(MeanOfGroup = mean(SpendingPerBuyer, na.rm = T))


#Null Hypothesis Average Spend per Buyer 1 = Average Spend per Buyer 0
#Alternate Hypothesis Average Spend per Buyer 1 = Average Spend per Buyer 0

#Check for difference in variance
var(SpendingOption0$SpendingPerBuyer)
var(SpendingOption1$SpendingPerBuyer)


#Difference in the variance of the two columns, therefore it is preferable to use welch T test 
t.test(SpendingOption1$SpendingPerBuyer, SpendingOption0$SpendingPerBuyer, var.equal = F)

#p-value = 0.5312, which means we do not have enough evidence to reject the null hypothesis

#Using ANOVA
summary(aov(SpendingPerBuyer ~ factor(test_option), data = SpendingPerBuyer))

#p-value = 0.532, F Value < 1, which means we do not have enough evidence to reject the null hypothesis

#Check for power
Zalpha = qnorm(0.975)

VarA = var(SpendingOption0$SpendingPerBuyer, na.rm = T)/nrow(SpendingOption0)
VarB = var(SpendingOption1$SpendingPerBuyer, na.rm = T)/nrow(SpendingOption1)

totalVar = sqrt(VarA + VarB)

powerForTest = pnorm((-Zalpha + (abs(meanOption0$MeanOfGroup-meanOption1$MeanOfGroup))/totalVar))*100

