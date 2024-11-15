setwd("C:/Users/ml30r/Downloads")
install.packages("readxl")
library(readxl)
bike_data = read_excel("BikeRentals.xlsx")
bike_data

# Task 1: Exploratory data analysis
head(bike_data)
# Convert columns to appropriate types 
bike_data$season = as.factor(bike_data$season)
bike_data$yr = as.factor(bike_data$yr)
bike_data$mnth = as.factor(bike_data$mnth)
bike_data$holiday = as.factor(bike_data$holiday)
bike_data$weekday = as.factor(bike_data$weekday)
bike_data$workingday = as.factor(bike_data$workingday)
bike_data$weathersit = as.factor(bike_data$weathersit)

# Convert 'dteday' to Date type
bike_data$dteday <- as.Date(bike_data$dteday)

# View data structure to confirm types
str(bike_data)

install.packages("ggplot2")
library(ggplot2)


# Plot monthly distribution of the total number of bikes rented
ggplot(bike_data, aes(x = mnth, y = cnt)) +
  geom_boxplot() +
  labs(title = "Monthly Distribution of Total Number of Bikes Rented",
       x = "Month", y = "Total Number of Bikes Rented") +
  theme_minimal()

# Plot yearly distribution of the total number of bikes rented
ggplot(bike_data, aes(x = yr, y = cnt)) +
  geom_boxplot() +
  labs(title = "Yearly Distribution of Total Number of Bikes Rented",
       x = "Year", y = "Total Number of Bikes Rented") +
  theme_minimal()

# Boxplot for outliers analysis (for temp, atemp, hum, windspeed)
ggplot(bike_data) +
  geom_boxplot(aes(x = factor(0), y = temp)) +
  labs(title = "Boxplot of Normalized Temperature", y = "Normalized Temperature (temp)") +
  theme_minimal()

ggplot(bike_data) +
  geom_boxplot(aes(x = factor(0), y = atemp)) +
  labs(title = "Boxplot of Normalized Feeling Temperature", y = "Normalized Feeling Temperature (atemp)") +
  theme_minimal()

ggplot(bike_data) +
  geom_boxplot(aes(x = factor(0), y = hum)) +
  labs(title = "Boxplot of Normalized Humidity", y = "Normalized Humidity (hum)") +
  theme_minimal()

ggplot(bike_data) +
  geom_boxplot(aes(x = factor(0), y = windspeed)) +
  labs(title = "Boxplot of Normalized Wind Speed", y = "Normalized Wind Speed (windspeed)") +
  theme_minimal()


# Split data into training and test sets (80% train, 20% test)
set.seed(123)
# For reproducibility
install.packages("caret")
library(caret)
install.packages("lattice")
library(lattice)
trainIndex = createDataPartition(bike_data$cnt, p = 0.8, list = FALSE)
trainData = bike_data[trainIndex, ]
testData = bike_data[-trainIndex, ]

# Train the Random Forest model
install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(cnt ~ season + yr + mnth + holiday + weekday + workingday + 
                           weathersit + temp + atemp + hum + windspeed, 
                         data = trainData, 
                         importance = TRUE)

# Print model summary
print(rf_model)

# Predict on test data
predictions <- predict(rf_model, newdata = testData)

# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - testData$cnt)^2))
cat("RMSE: ", rmse, "\n")

# Plot predicted vs actual values
ggplot(testData, aes(x = cnt, y = predictions)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual Bike Rentals",
       x = "Actual Bike Rentals", y = "Predicted Bike Rentals") +
  theme_minimal()
