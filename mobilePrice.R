library(readr)
data <- read_csv("Data.csv")
View(data)

library(outliers)


#detecting and removing 
boxplot(data$fc)
boxplot(data$fc)$out
outliers <- boxplot(data$fc ,plot=FALSE)$out
print(outliers)
data = data[-which(data$fc %in% outliers),]
boxplot(data$fc)

#detecting and removing outlier
boxplot(data$px_height)
boxplot(data$px_height)$out
outliers <- boxplot(data$px_height, plot=FALSE)$out
print(outliers)
data = data[-which(data$px_height %in% outliers),]
boxplot(data$px_height)

str(data)

#normalization
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

data$battery_power <- normalize(data$battery_power)
data$clock_speed <- normalize(data$clock_speed)
data$fc <- normalize(data$fc)
data$int_memory <- normalize(data$int_memory)
data$m_dep <- normalize(data$m_dep)
data$mobile_wt <- normalize(data$mobile_wt)
data$n_cores <- normalize(data$n_cores)
data$pc <- normalize(data$pc)
data$px_height <- normalize(data$px_height)
data$px_width <- normalize(data$px_width)
data$ram <- normalize(data$ram)
data$sc_h <- normalize(data$sc_h)
data$sc_w <- normalize(data$sc_w)
data$talk_time <- normalize(data$talk_time)

str(data)

#correlation
cor(data)
cor(data$three_g , data$four_g)
cor(data$pc ,data$fc)
cor(data$px_height , data$px_width)
cor(data$sc_w , data$sc_h)

#End prepossessing


#____________Classification___________
#Loading required packages:
library(party)
library(caret)

#we used set.seed() to get reproducible result
set.seed(1234)

#______________decision tree #1________

#Split the dataset into two subsets: Training(90%) , Testing(10%)
ind <- sample(2,nrow(data), replace=TRUE, prob=c(0.9,0.1))

#Training(90%)
trainData <- data[ind==1,]

#Testing(10%)
testData <- data[ind==2,]


#Build and create the training model
formula <- price_range ~ battery_power + blue+ clock_speed + dual_sim+ fc + four_g + int_memory + m_dep + 
mobile_wt + n_cores + pc + px_height + px_width + ram + sc_h + sc_w + talk_time+ three_g + touch_screen + wifi
data.ctree <- ctree(formula, data=trainData)
table(predict(data.ctree), trainData$price_range)


#plot the tree
print(data.ctree)
plot(data.ctree)
plot(data.ctree, type="simple")

#test prediction
testPred <- format(round(predict(data.ctree , newdata=testData)))

#Evaluate the model
CM <- table(testPred, testData$price_range)
confusionMatrix(CM)


#______________decision tree #2________

#Split the dataset into two subsets: Training(80%) , Testing(20%)
ind <- sample(2,nrow(data), replace=TRUE, prob=c(0.8,0.2))

#Training(80%)
trainData <- data[ind==1,]

#Testing(20%)
testData <- data[ind==2,]


#Build and create the training model
formula <- price_range ~ battery_power + blue+ clock_speed + dual_sim+ fc + four_g + int_memory + m_dep + 
  mobile_wt + n_cores + pc + px_height + px_width + ram + sc_h + sc_w + talk_time+ three_g + touch_screen + wifi
data.ctree <- ctree(formula, data=trainData)
table(predict(data.ctree), trainData$price_range)


#plot the tree
print(data.ctree)
plot(data.ctree)
plot(data.ctree, type="simple")

#test prediction
testPred <- format(round(predict(data.ctree , newdata=testData)))

#Evaluate the model
CM <- table(testPred, testData$price_range)
confusionMatrix(CM)

#______________decision tree #3________

#Split the dataset into two subsets: Training(70%) , Testing(30%)
ind <- sample(2,nrow(data), replace=TRUE, prob=c(0.7,0.3))

#Training(70%)
trainData <- data[ind==1,]

#Testing(30%)
testData <- data[ind==2,]


#Build and create the training model
formula <- price_range ~ battery_power + blue+ clock_speed + dual_sim+ fc + four_g + int_memory + m_dep + 
  mobile_wt + n_cores + pc + px_height + px_width + ram + sc_h + sc_w + talk_time+ three_g + touch_screen + wifi
data.ctree <- ctree(formula, data=trainData)
table(predict(data.ctree), trainData$price_range)


#plot the tree
print(data.ctree)
plot(data.ctree)
plot(data.ctree, type="simple")

#test prediction
testPred <- format(round(predict(data.ctree , newdata=testData)))

#Evaluate the model
CM <- table(testPred, testData$price_range)
confusionMatrix(CM)

