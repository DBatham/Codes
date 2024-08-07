Queens = read.csv("D:/MSBA/Data Analytics/Assigment 4/NYC_Citywide_Annualized_Calendar_Sales_Update.csv")
View(Queens)

# Calculate the average sales price for each area
NEIGHBORHOOD <- Queens$NEIGHBORHOOD
SALE.PRICE <- Queens$SALE.PRICE
avg_sales_price <- Queens %>% group_by(NEIGHBORHOOD) %>% summarise(avg_sales_price = mean(SALE.PRICE))

library(ggplot2)

ggplot(avg_sales_price, aes(x = NEIGHBORHOOD, y = avg_sales_price/1000)) + 
  geom_bar(stat = "identity", fill = "skyblue") + 
  labs(title = "Average Sales Prices in Queens Neighborhoods", x = "Neighborhood", y = "Average Sales Price in Thousands") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#b) 
#to find outliers using Cook's distance
Land_sq <- Queens$LAND.SQUARE.FEET
model <- lm(SALE.PRICE ~ SALE.PRICE , data = Queens)

cooksd <- cooks.distance(model)

# Identify influential points with high Cook's Distance
influential_points <- Queens[which(cooksd > 4 * mean(cooksd)), ]
View(influential_points)


#to find outliers using Interquartile
SALE.PRICE_Q1 <- quantile(SALE.PRICE, 0.25)
SALE.PRICE_Q3 <- quantile(SALE.PRICE, 0.75)

# Calculate the IQR
IQR <- SALE.PRICE_Q3 - SALE.PRICE_Q1

lower_bound <- SALE.PRICE_Q1 - 1.5 * IQR
upper_bound <- SALE.PRICE_Q3 + 1.5 * IQR

# Identify outliers
outliers <- SALE.PRICE[SALE.PRICE < lower_bound | SALE.PRICE > upper_bound]
print(outliers)


mean_b <- mean(Queens$SALE.PRICE, na.rm = TRUE)
Queens$SALE.PRICE[is.na(Queens$SALE.PRICE)] <- mean_b


mean_g <- mean(Queens$GROSS.SQUARE.FEET, na.rm = TRUE)
Queens$GROSS.SQUARE.FEET[is.na(Queens$GROSS.SQUARE.FEET)] <- mean_g
Queens$GROSS.SQUARE.FEET <- as.numeric(Queens$GROSS.SQUARE.FEET)


mean_l <- mean(Queens$LAND.SQUARE.FEET, na.rm = TRUE)
Queens$LAND.SQUARE.FEET[is.na(Queens$LAND.SQUARE.FEET)] <- mean_l
Queens$LAND.SQUARE.FEET <- as.numeric(Queens$LAND.SQUARE.FEET)



# Split the dataset into training and test sets
set.seed(123)
train_index <- sample(1:nrow(Queens), 0.8 * nrow(Queens))
train_set <- Queens[train_index, ]
test_set <- Queens[-train_index, ]

model_m <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET + LAND.SQUARE.FEET, data = train_set)
predictions <- predict(model_m, newdata = test_set)
mse <- mean((predictions - test_set$SALE.PRICE)^2)
print(mse)

#2
set.seed(123)
train_index <- sample(1:nrow(Queens), 0.9 * nrow(Queens))
train_set <- Queens[train_index, ]
test_set <- Queens[-train_index, ]

model_m <- lm(SALE.PRICE ~ GROSS.SQUARE.FEET + LAND.SQUARE.FEET, data = train_set)
predictions <- predict(model_m, newdata = test_set)
mse <- mean((predictions - test_set$SALE.PRICE)^2)
print(mse)



#d)
df = read.csv("D:/MSBA/Data Analytics/Assigment 4/NYC_Citywide_Annualized_Calendar_Sales_Update.csv")
# Convert the date variable to a factor variable
df$SALE.DATE <- factor(df$SALE.DATE)

# Set the seed for reproducibility
set.seed(123)

# Split the data into training and test sets in a 70:30 ratio
training_set <- sample(1:nrow(df), 0.7 * nrow(df))
test_set <- setdiff(1:nrow(df), training_set)

# Create the training and test data frames
training_df <- df[training_set, ]
test_df <- imputeMean(test_df)
test_df <- df[test_set, ]

df$borough_region <- factor(paste(df$BOROUGH, df$BUILDING.CLASS.CATEGORY, sep = "_"))


# Fit the model using the new borough_region factor variable
model <- lm(df$SALE.PRICE ~ df$borough_region, data = training_df)

# Make predictions on the test set
test_predictions <- predict(model, newdata = test_df)

# Calculate the mean squared error (MSE) on the test set
mse <- mean((test_predictions - test_df$AnnualizedSales)^2)

# Print the MSE
print(mse)



# Load the necessary libraries
library(dplyr)
library(lmtest)

# Fit the linear regression model
model <- lm(df$SALE.PRICE ~ SALE.DATE, data = df)

# Examine the model fit
summary(model)
