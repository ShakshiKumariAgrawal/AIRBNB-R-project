# 1. Data Importing
listings<-read.csv("C:/Users/ishak/Downloads/listing_r.csv")
View(listings)

# 2. Data Cleaning and Transformation
library("dplyr")
library("tidyr")
library("ggplot2")
library("lubridate")

colSums(is.na(listings)) # N/A are in -> price, reviews_per_month

# Replace NA in reviews_per_month with 0
listings <- mutate(listings,reviews_per_month = ifelse(is.na(reviews_per_month), 0, reviews_per_month))

# Converting last_review as date format 
listings <- mutate(listings,last_review = parse_date_time(last_review, orders = c("Ymd", "Y-m-d", "dmy", "d-m-y")))

# Drop rows with missing values in other crucial columns
listings <- drop_na(listings,price, minimum_nights, room_type, latitude, longitude,last_review)

# Transforming data
listings <- mutate(listings,room_type = as.factor(room_type))
listings <- mutate(listings,reviews_per_year = reviews_per_month * 12)

# 3. Exploratory Data Analysis (EDA)
# Summary
summary(listings)

# Visualizations
ggplot(listings, aes(x = price)) + 
  geom_histogram(binwidth = 50) + 
  ggtitle("Distribution of Prices")

ggplot(listings, aes(x = room_type, y = price)) + 
  geom_boxplot() + 
  ggtitle("Price by Room Type")

# 4. Feature Engineering
# Creating a variable which contains distance of a place from the given landmark 
landmark_lat <- 40.748817
landmark_lon <- -73.985428

# Define a function to calculate the Haversine distance
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert degrees to radians
  radians <- pi / 180
  lat1 <- lat1 * radians
  lon1 <- lon1 * radians
  lat2 <- lat2 * radians
  lon2 <- lon2 * radians
  
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # Radius of Earth in kilometers
  R <- 6371
  distance <- R * c
  return(distance)
}

listings <- mutate(listings,distance_from_landmark = haversine_distance(latitude, longitude, landmark_lat, landmark_lon))

# 5. Modeling
library(caret)  # for splitting data
set.seed(123)
train_indices <- sample(seq_len(nrow(listings)), size = 0.7*nrow(listings))
train_data <- listings[train_indices, ]
test_data <- listings[-train_indices, ]

# Exclude 'name' and 'host_name' variables from the model
train_data <- select(train_data, -name, -host_name)
test_data <- select(test_data, -name, -host_name)

# Building a regression model
model <- lm(price ~ ., data = train_data)
summary(model)

# 6. Model Evaluation
# Predicting on test set
predictions <- predict(model, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(mean((predictions - test_data$price)^2))
print(paste("RMSE: ", rmse))

# Visualizing model performance
ggplot(data.frame(Predicted = predictions, Actual = test_data$price), aes(x = Predicted, y = Actual)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = 'blue') +
  ggtitle("Predicted vs Actual Prices")

tinytex::install_tinytex()