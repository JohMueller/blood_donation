# install.packages(c("readr", "caret", "ggplot2")

library(readr)
library(caret)
library(ggplot2)

# Load and clean the data
data <- read_csv("~/projects/blood_donation/data.csv")
names(data) <- c("ID", "months_since_last", "number_donations",
                 "total_volume", "months_since_first", "made_donation")
data[2:5] <- sapply(data[2:5], as.numeric)
data$made_donation <- as.factor(data$made_donation)
data$ID <- NULL

# split into training and test data
Index <- createDataPartition(data$made_donation, p=0.80, list=FALSE)
data_train <- data[Index,]
data_test <- data[-Index,]

# explore the data

nrow(data_train)
ncol(data_train)
summary(data_train)

plot(density(data_train$total_volume))

ggplot(data_train, aes(x=total_volume, 
                       y=number_donations))+
  geom_point(aes(col =as.factor(made_donation)))

# train the model

fit.glm <- train(made_donation ~ months_since_last + number_donations + months_since_first,
                 data=data_train, method="glm", metric="Accuracy")

summary(fit.glm)
predict(fit.glm)

# check accuracy on test data
predict(fit.glm, newdata = data_test)
sum(predict(fit.glm, newdata = data_test) == data_test$made_donation)/nrow(data_test)

