bank_marketing <- read.csv("D:/MSBA/Data Analytics/Assignment 7/bank-additional-full (version 2).csv")

View(bank_marketing)
library(caret)
set.seed(123)
trainIndex <- createDataPartition(bank_marketing$y,
                                  p = 0.8, 
                                  list = FALSE)
dfTrain <- bank_marketing[ trainIndex,]
dfTest  <- bank_marketing[-trainIndex,]


set.seed(123)

cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# logistic regression
model_glm <- train(y~.,
                   data = dfTrain,
                   method = "glm",
                   family = "binomial")

print(model_glm)


#Random Forest
set.seed(123)
model_rf <- train(y~.,
                  data = dfTrain,
                  method = "rf",
                  ntree = 20,
                  tuneLength = 5)
print(model_rf)
plot(model_rf)

#Decision Tree
bank_marketing$y <- factor(bank_marketing$y)

# Train the decision tree model
tree_model <- rpart(y ~ ., data = bank_marketing, method = 'class')

# Display the decision tree
plot(tree_model)
text(tree_model)

rpart.plot(tree_model, box.col=c("red", "blue"))

# Predict using the decision tree model
Prediction1 <- predict(tree_model, newdata = bank_marketing, type = 'class')

Prediction1 <- factor(Prediction1, levels = levels(bank_marketing$y))

# Create confusion matrix
library(caret)
confusionMatrix(Prediction1, bank_marketing$y)$overall[1]
##############################################
# Creating the Accuracy data frame
Accuracy <- data.frame(
  Model = c('Logistic Regression', 'Random Forest', 'Decision tree'),
  Accuracy = c(0.9094, 0.9074872, 0.9128387)
)

# Setting the benchmark value
benchmark <- 0.89

# Plotting with ggplot and geom_hline
library(ggplot2)

ggplot(Accuracy, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  ggtitle('Accuracies of Models') +
  geom_hline(yintercept = benchmark, color = 'red')

