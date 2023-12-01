library(tidyverse)
library(ggplot2)
df <- read.csv('Online_sales.csv')
head(df)

stats <- table(summary(df))


plot(stats)
#Checking for Null values
sum(is.na(df))

# 1. User Engagement
avg_pages_visited <- mean(df$ProductRelated)
avg_duration <- mean(df$ProductRelated_Duration)

print(avg_duration)
print(avg_pages_visited)

# 2. Bounce and Exit Rates
overall_bounce_rate <- mean(df$BounceRates)
exit_rates_by_page <- tapply(df$ExitRates, df$PageValues, mean)

# 3. Conversion and Revenue
conversion_rate <- sum(df$Revenue) / nrow(df)
page_value_correlation <- cor(df$PageValues, df$Revenue)


# 4. Visitor Behavior by Month
monthly_visits <- table(df$Month)
monthly_revenue <- tapply(df$Revenue, df$Month, sum)

# 5. Traffic Sources
traffic_type_visits <- table(df$TrafficType)



# 6. Visitor Types and Weekend Impact
visitor_types <- table(df$VisitorType)
weekend_revenue <- sum(df$Revenue[df$Weekend])

# 7. Browser and Operating System Impact
browser_preferences <- table(df$Browser)
os_preferences <- table(df$OperatingSystems)

# 8. Regional Patterns
region_visits <- table(df$Region)
region_revenue <- tapply(df$Revenue, df$Region, sum)


# 9. Special Day Impact
special_day_visits <- sum(df$SpecialDay > 0)
special_day_revenue <- sum(df$Revenue[df$SpecialDay > 0])

# 10. Retention and Customer Loyalty
returning_visits <- sum(df$VisitorType == "Returning_Visitor")
returning_frequency <- table(df$VisitorType[df$VisitorType == "Returning_Visitor"])

cat("1. Average Pages Visited:", avg_pages_visited, "\n")
cat("  Average Duration:", avg_duration, "\n\n")

cat("2. Overall Bounce Rate:", overall_bounce_rate, "\n")
cat("  Exit Rates by Page:", exit_rates_by_page, "\n\n")

cat("4. Monthly Visits:", monthly_visits, "\n")
cat("  Monthly Revenue:", monthly_revenue, "\n\n")

cat("5. Traffic Type Visits:", traffic_type_visits, "\n\n")

cat("6. Visitor Types:", visitor_types, "\n")
cat("  Weekend Revenue:", weekend_revenue, "\n\n")

cat("7. Browser Preferences:", browser_preferences, "\n")
cat("  OS Preferences:", os_preferences, "\n\n")

cat("8. Regional Visits:", region_visits, "\n")
cat("  Regional Revenue:", region_revenue, "\n\n")

cat("9. Special Day Visits:", special_day_visits, "\n")
cat("  Special Day Revenue:", special_day_revenue, "\n\n")

cat("10. Returning Visits:", returning_visits, "\n")
cat("   Returning Visitor Frequency:", returning_frequency, "\n")


# Add some plots

# Plot 1: Monthly Visits
ggplot(df, aes(x = Month, fill = Revenue)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Month", y = "Visits", title = "Monthly Visits")

# Plot 2: Monthly Revenue
ggplot(df, aes(x = Month, y = Revenue, fill = Revenue)) +
  geom_bar(stat = "summary", fun = sum) +
  theme_minimal() +
  labs(x = "Month", y = "Revenue", title = "Monthly Revenue")


# Plot 3: Visitor Types
ggplot(df, aes(x = VisitorType, fill = Revenue)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Visitor Type", y = "Visits", title = "Visitor Types")


# Plot 5: OS Preferences
ggplot(df, aes(x = as.character (OperatingSystems), fill = Revenue)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Operating System", y = "Visits", title = "OS Preferences")

# Plot 6: Regional Visits
ggplot(df, aes(x = as.character(Region), fill = Revenue)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Region", y = "Visits", title = "Regional Visits")

# Plot : Special Day Visits
ggplot(df, aes(x = as.character(SpecialDay), fill = Revenue)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Special Day", y = "Visits", title = "Special Day Visits")

# The decision tree

library(rpart)
library(rpart.plot)

#converting income to numeric

df$Region <- df$Region
decision_tree_model <- rpart(Revenue~., data= df, method = 'class')

par(mar= c(1,1,1,1)) #adjust margins
rpart.plot(decision_tree_model, main='Decision Tree Model Base on Revenue', box.palette = 'BuBn')

library(gridExtra)


# Create a grid of multiple plots for different aspects
grid.arrange(
  ggplot(df, aes(x = Revenue, y = Informational, fill = Revenue)) +
    geom_boxplot() +
    ggtitle("Informational Pages Visited by Revenue"),
  
  ggplot(df, aes(x = Revenue, y = ProductRelated, fill = Revenue)) +
    geom_boxplot() +
    ggtitle("Product Related Pages Visited by Reveue"),
  
  
  ncol = 2
)



# Install the package if it's not already installed
if (!require(caret)) {
  install.packages("caret")
}

# Load the package
library(caret)


# Split the data
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(df$Revenue, p = .8, list = FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]

head(test)

# Fit a linear regression model
model <- lm(Revenue ~ ProductRelated + ProductRelated_Duration, data = train)

# Check the summary of the model
summary(model)



# Make predictions
predictions <- predict(model, data = test)

# Check the predictions
head(predictions)

plot(model)

# Predict the values for the test set
predictions <- predict(model, newdata = test)

# Calculate the Mean Squared Error
mse <- mean((test$Revenue - predictions)^2)

# Calculate the Root Mean Squared Error
rmse <- sqrt(mse)

# Calculate the R-squared
r_squared <- summary(model)$r.squared

print(paste("Mean Squared Error: ", mse))
print(paste("Root Mean Squared Error: ", rmse))
print(paste("R-squared: ", r_squared))

# Fit the logistic regression model
model2 <- glm(Revenue ~ ProductRelated + ProductRelated_Duration, data = train, family = "binomial")

# Generate predictions for the test set
predictions <- predict(model2, newdata = test, type = "response")


# Classify the predictions
predicted_classes <- ifelse(predictions >= 0.5, 1, 0)

# Calculate the accuracy score
accuracy <- sum(predicted_classes == test$Revenue) / nrow(test)

print(paste("Accuracy: ", accuracy))


grid.arrange(
  ggplot(df, aes(x = Revenue, y = BounceRates, fill = Revenue)) +
    geom_boxplot() +
    ggtitle("Impact of Bounce Rates on Revenue"),
  
  ggplot(df, aes(x = Revenue, y = PageValues, fill = Revenue)) +
    geom_boxplot() +
    ggtitle("Imppact of PageValues on Revenue"),
  
  ncol=2
)


ggplot(df, aes(x = Revenue, fill = Revenue)) +
    geom_bar() +
    ggtitle("Revenue Chart")
  
 
