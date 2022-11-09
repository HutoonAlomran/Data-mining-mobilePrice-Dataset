data <- read.csv("/Users/hutoo/Documents/KSU/IT 6/DATA MAINING/Project/train.csv")

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

## Loading required packages:
library(party)
library(rpart)
library(caret)
library(rpart.plot)

#To get reproducible result
set.seed(1234)

#Split the dataset into two subsets: Training(90%) , Testing(10%)
ind <- sample(2,nrow(data), replace=TRUE, prob=c(0.9,0.1))

#Training(90%)
trainData <- data[ind==1,]

#Testing(10%)
testData <- data[ind==2,]

#crating the model
model <- rpart(price_range ~ battery_power + blue+ clock_speed + dual_sim+ fc + four_g + int_memory + m_dep + mobile_wt + n_cores + pc + px_height + px_width + ram + sc_h + sc_w + talk_time+ three_g + touch_screen + wifi)

