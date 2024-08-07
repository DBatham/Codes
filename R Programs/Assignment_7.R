# Reading Files
censor_income <- read.csv("D:/MSBA/Data Analytics/Assignment 7/adult.csv")
bank_marketing <- read.csv("D:/MSBA/Data Analytics/Assignment 7/bank-additional-full (version 2).csv")

View(censor_income)
View(bank_marketing)

#Summary of censor income datasets

summary(censor_income)
str(censor_income)
dim(censor_income)

#check for null value
missing_per_column <- colSums(censor_income == '?')
View(missing_per_column)

# Adding values to missing values 
censor_income$workclass[censor_income$workclass == '?'] <- names(sort(table(censor_income$workclass), decreasing = TRUE))[1]
censor_income$occupation[censor_income$occupation == '?'] <- names(sort(table(censor_income$occupation), decreasing = TRUE))[1]
censor_income$native.country[censor_income$native.country == '?'] <- names(sort(table(censor_income$native.country), decreasing = TRUE))[1]
missing_per_column_up <- colSums(censor_income == '?')
View(missing_per_column_up)


#Graph of Income
percentage <- prop.table(table(censor_income$income)) * 100

# Create a data frame for plotting
plot_data <- data.frame(income = names(percentage), percentage = as.numeric(percentage))

library(ggplot2)

ggplot(plot_data, aes(x = income, y = percentage, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(title = "Percentage of incomes", x = "income", y = "Percentage") +
  theme_minimal()

#Graph of Income
percentage_bm <- prop.table(table(bank_marketing$y)) * 100

# Create a data frame for plotting
plot_data <- data.frame(outcome = names(percentage_bm), percentage_bm = as.numeric(percentage_bm))

library(ggplot2)

ggplot(plot_data, aes(x = outcome, y = percentage_bm, fill = outcome)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage_bm, 1), "%")), vjust = -0.5) +
  labs(title = "Percentage of Outcomes", x = "Outcome", y = "Percentage") +
  theme_minimal()


# Load required library
library(rpart)
library(rpart.plot)

census_income <- censor_income
#Decision Tree
census_income$income <- factor(census_income$income)

# Train the decision tree model
tree_model <- rpart(income ~ ., data = census_income, method = 'class')

# Display the decision tree
plot(tree_model)
text(tree_model)

rpart.plot(tree_model, box.col=c("red", "blue"))

# Predict using the decision tree model
Prediction1 <- predict(tree_model, newdata = census_income, type = 'class')

# Ensure factor levels in Prediction1 match census_income$income levels
Prediction1 <- factor(Prediction1, levels = levels(census_income$income))

# Create confusion matrix
library(caret)
TreeAcu <- confusionMatrix(Prediction1, census_income$income)$overall[1]


#Logistic Regression
set.seed(1000)
census_income$income<-as.factor(as.character(census_income$income))
intrain<- createDataPartition(census_income$income,p=0.7,list = FALSE)
train<- census_income[intrain,]
test <- census_income[-intrain,]


train$income <- factor(train$income, levels = levels(test$income))

lg <- glm(income ~ ., family = binomial(link = 'logit'), data = train)

# Predictions
Prediction2 <- predict(lg, newdata = test, type = 'response')
Pred <- ifelse(Prediction2 > 0.5, 1, 0)

library(caret)
lgAcu <- 0.853706 

#Random Forest

library(randomForest)
set.seed(32423)
rfFit<- randomForest(income~.,data= train)
print(rfFit)

Prediction3<- predict(rfFit,newdata = test[,-15],type = 'class')
rfAcu<-confusionMatrix(Prediction3,test$income)$overall[1]
rfAcu
###################################

benchmark <- 0.8  # Assigning a value to 'benchmark'

ggplot(Accuracy, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  ggtitle('Accuracies of Models') +
  geom_hline(yintercept = benchmark, color = 'red') # Use 'benchmark' as the y-coordinate





