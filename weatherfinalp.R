

#####..............................import data set
setwd("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/final project materials")

Weather.Data <- read.csv("D:/11th semmester/INTRODUCTION TO DATA SCIENCE [C]/final project materials/Weather Data.csv")

View(Weather.Data)
Weather.Data
str(Weather.Data)
head(Weather.Data)
names(Weather.Data)
###..........................................data set preparation

Weather.Data$Weather <- factor(Weather.Data$Weather, levels=c("Fog","Freezing Drizzle,Fog","Mostly Cloudy","Cloudy","Rain","Rain Showers","Mainly Clear","Snow Showers","Clear","Snow","Freezing Rain,Fog","Freezing Rain","Freezing Drizzle","Rain,Snow","Moderate Snow","Freezing Drizzle,Snow","Freezing Rain,Snow Grains","Snow,Blowing Snow","Freezing Fog","Haze","Rain,Fog","Drizzle,Fog","Rain,Snow","Freezing Drizzle,Haze","Freezing Rain,Haze","Snow,Haze","Drizzle","Snow,Fog","Snow,Ice Pellets","Thunderstorms,Rain Showers","Thunderstorms,Rain","Rain,Haze","Thunderstorms,Rain Showers,Fog","Thunderstorms","Thunderstorms,Rain,Fog","Thunderstorms,Moderate Rain Showers,Fog","Thunderstorms,Heavy Rain Showers","Rain Showers,Fog","Rain Showers,Snow Showers","Snow Pellets","Rain,Snow,Fog","Moderate Rain,Fog","Freezing Rain,Ice Pellets,Fog","Drizzle,Ice Pellets,Fog","Drizzle,Snow","Rain,Ice Pellets","Drizzle,Snow,Fog","Rain,Snow Grains","Snow Showers,Fog","Moderate Snow,Blowing Snow","Rain,Snow,Ice Pellets"), labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51))
Weather.Data

Weather.Data <- Weather.Data[, -which(names(Weather.Data) == "Date.Time")]
print(Weather.Data)


number_of_missing_value=colSums(is.na(Weather.Data))
number_of_missing_value




###.............................................................................................................................
##..................hoyyyyyy normalization with weather collumn ..........................

library(dplyr)


Weather.Data <- as.data.frame(sapply(Weather.Data, as.numeric))

min_max_norm <- function(x) { 
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

normalized_data <- Weather.Data %>%
  mutate(across(everything(), min_max_norm))


print(normalized_data)




###.......................again co rlt ........................ata hoice.....................................

library(dplyr)


selected_attributes <- c("Temp_C", "Dew.Point.Temp_C","Rel.Hum_.", "Wind.Speed_km.h", "Visibility_km", "Press_kPa")

correlation_matrix <- cor(Weather.Data$Weather, Weather.Data[, selected_attributes])

print(correlation_matrix)

##...........................co rlt using normalised data ..........

library(dplyr)

selected_attributes <- c("Temp_C", "Dew.Point.Temp_C","Rel.Hum_.", "Wind.Speed_km.h", "Visibility_km", "Press_kPa")

correlation_matrix <- cor(normalized_data$Weather, normalized_data[, selected_attributes])

print(correlation_matrix)

####.........................
library(corrplot)
a<-cor(normalized_data)
corrplot(a,method="color")



#
##############........................again................



####............................agein     2..................
install.packages("class")
library(class)



important_attributes <- c("Temp_C", "Dew.Point.Temp_C", "Rel.Hum_", "Wind.Speed_km.h")

# Create a new dataset with only the selected attributes
selected_data <- Weather.Data[, c(important_attributes, "Weather")]

# Convert the "Weather" column to a factor (if not already)
selected_data$Weather <- as.factor(selected_data$Weather)

# Set seed for reproducibility
set.seed(123)

# Create a random sample of row indices for training data
random <- sample(1:nrow(selected_data), 0.7 * nrow(selected_data))

# Split the dataset into training and testing sets
weather_train <- selected_data[random, ]
weather_test <- selected_data[-random, ]


# Extract the labels
weather_train_labels <- weather_train$Weather
weather_test_labels <- weather_test$Weather

# Perform KNN classification
k <- 3  # Set the value of 'k'
predicted_labels <- knn(train = weather_train[, -which(names(weather_train) == "Weather")],
                        test = weather_test[, -which(names(weather_test) == "Weather")],
                        cl = weather_train_labels,
                        k = k)





# Calculate accuracy
accuracy <- sum(predicted_labels == weather_test_labels) / length(weather_test_labels)

# Calculate confusion matrix
conf_matrix <- table(predicted = predicted_labels, actual = weather_test_labels)

# Calculate recall and precision
recall <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
precision <- conf_matrix[1, 1] / sum(conf_matrix[, 1])

# Print results
cat("Accuracy:", accuracy, "\n")
cat("Confusion Matrix:\n", conf_matrix, "\n")
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")

###...................................................stop...............................

#..................................diagram correlation ar

library(dplyr)
library(corrplot)


selected_attributes <- c("Temp_C", "Dew.Point.Temp_C","Rel.Hum_.", "Wind.Speed_km.h", "Visibility_km", "Press_kPa")

correlation_matrix <- cor(Weather.Data$Weather, Weather.Data[, selected_attributes])

corrplot(correlation_matrix, method = "color", type = "full", tl.col = "black")

#........................................................

install.packages("colorspace")
library(ggplot2)

ggplot(Weather.Data, aes(x = Weather, y = Temp_C)) +
  geom_point() +
  labs(x = "Weather", y = "Temp_C") +
  theme_minimal()


###................positive corelation dia gram (weather and tem c attributes ar )

library(ggplot2)


ggplot(Weather.Data, aes(x = Weather, y = Temp_C)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Weather", y = "Temp_C") +
  theme_minimal()

#.............negative...........naigeria.............................
# Load the required libraries
# install.packages("ggplot2")
library(ggplot2)

# Create a scatter plot between "Weather" and "Temp_C" with negative correlation
ggplot(Weather.Data, aes(x = Weather, y = Temp_C)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Weather", y = "Temp_C") +
  theme_minimal()

#..........................................(weather and Dew.Point.Temp_C) positive correlation diagram..

library(ggplot2)


ggplot(Weather.Data, aes(x = Weather, y = Dew.Point.Temp_C)) +
  geom_point(color = "blue") +
  labs(x = "Weather", y = "Dew.Point.Temp_C") +
  theme_minimal()

##.................training and testing..........................................................................

###........................................hoiceeee
# Load necessary libraries
library(caret)

# Create a random sample of row indices for training data
random <- sample(1:nrow(normalized_data), 0.7 * nrow(normalized_data))

# Split the dataset into training and testing sets
weather_train <- normalized_data[random, ]
weather_test <- normalized_data[-random, ]

# Extract the labels (assuming "Weather" column is the label)
weather_train_labels <- weather_train$Weather
weather_test_labels <- weather_test$Weather

#.........................................
weather_train

weather_test
###............................for accuracy.............................

install.packages("class")
library(class)

set.seed(123)


random <- sample(1:nrow(normalized_data), 0.7 * nrow(normalized_data))

weather_train <- normalized_data[random, ]
weather_test <- normalized_data[-random, ]

# Extract the labels
weather_train_labels <- weather_train$Weather
weather_test_labels <- weather_test$Weather

# Perform KNN classification
k <- 3  # Set the value of 'k'
predicted_labels <- knn(train = weather_train[, -which(names(weather_train) == "Weather")],
                        test = weather_test[, -which(names(weather_test) == "Weather")],
                        cl = weather_train_labels,
                        k = k)

# Calculate accuracy
accuracy <- sum(predicted_labels == weather_test_labels) / length(weather_test_labels)

# Print accuracy
cat("Accuracy:", accuracy, "\n")


###............................................10 fold ..........................................
#..............final ...............10 fold cross validation .......................

install.packages("class")
library(class)
install.packages("caret")
library(caret)

set.seed(123)


num_folds <- 10

fold_indices <- createFolds(normalized_data$Weather, k = num_folds)

accuracies <- numeric(num_folds)

for (i in 1:num_folds) {

  test_indices <- fold_indices[[i]]
  train_indices <- setdiff(1:nrow(normalized_data), test_indices)
  
  weather_train <- normalized_data[train_indices, ]
  weather_test <- normalized_data[test_indices, ]
  

  input_features_train <- weather_train[, -which(names(weather_train) == "Weather")]
  input_features_test <- weather_test[, -which(names(weather_test) == "Weather")]
  weather_train_labels <- weather_train$Weather
  weather_test_labels <- weather_test$Weather
  

  k <- 3  # Set the value of 'k'
  predicted_labels <- knn(train = input_features_train,
                          test = input_features_test,
                          cl = weather_train_labels,
                          k = k)
  
 
  accuracies[i] <- sum(predicted_labels == weather_test_labels) / length(weather_test_labels)
}

mean_accuracy <- mean(accuracies)

cat("Mean Accuracy (10-Fold Cross-Validation):", mean_accuracy, "\n")





###..................................final Confusion matrix..................................................



install.packages("class")
library(class)
install.packages("caret")
library(caret)

set.seed(123)

num_folds <- 10


fold_indices <- createFolds(normalized_data$Weather, k = num_folds)


confusion_matrices <- list()
recalls <- numeric(num_folds)
precisions <- numeric(num_folds)

calculate_metrics <- function(cm) {
  recall <- cm[1, 1] / sum(cm[1, ])
  precision <- cm[1, 1] / sum(cm[, 1])
  return(list(recall = recall, precision = precision))
}

for (i in 1:num_folds) {
 
  test_indices <- fold_indices[[i]]
  train_indices <- setdiff(1:nrow(normalized_data), test_indices)
  
  weather_train <- normalized_data[train_indices, ]
  weather_test <- normalized_data[test_indices, ]
  
  input_features_train <- weather_train[, c("Temp_C", "Dew.Point.Temp_C", "Rel.Hum_.", 
                                            "Wind.Speed_km.h", "Visibility_km", "Press_kPa")]
  input_features_test <- weather_test[, c("Temp_C", "Dew.Point.Temp_C", "Rel.Hum_.", 
                                          "Wind.Speed_km.h", "Visibility_km", "Press_kPa")]
  weather_train_labels <- weather_train$Weather
  weather_test_labels <- weather_test$Weather
  

  k <- 3  # Set the value of 'k'
  predicted_labels <- knn(train = input_features_train,
                          test = input_features_test,
                          cl = weather_train_labels,
                          k = k)
  
  confusion_matrices[[i]] <- table(predicted = predicted_labels, actual = weather_test_labels)
  
 
  metrics <- calculate_metrics(confusion_matrices[[i]])
  recalls[i] <- metrics$recall
  precisions[i] <- metrics$precision
}

mean_recall <- mean(recalls)
mean_precision <- mean(precisions)


cat("Mean Recall:", mean_recall, "\n")
cat("Mean Precision:", mean_precision, "\n")

for (i in 1:num_folds) {
  cat("Confusion Matrix (Fold", i, "):\n")
  print(confusion_matrices[[i]])
  cat("\n")
}



