# Specify the file path
file_path <- "D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Airline Passenger Satisfaction/test.csv"

# Read the CSV file into a data frame
dataset <- read.csv(file_path)

# Display the first few rows of the dataset to verify the import
head(dataset)


# Summary statistics
summary(dataset)

# Define the path to your dataset
file_path <- "D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Airline Passenger Satisfaction/test.csv"

# Read the dataset into a data frame
df <- read.csv(file_path)

# Check for null values in each attribute
missing_values <- sapply(df, function(x) sum(is.na(x)))

# Display the null values
print("Null values in each attribute:")
print(missing_values)


# Mean of Flight Distance
mean_distance <- mean(df$`Flight Distance`)
print(paste("Mean Flight Distance: ", mean_distance))

# Median of Age
median_age <- median(df$Age)
print(paste("Median Age: ", median_age))

# Standard Deviation of Departure Delay in Minutes
sd_departure_delay <- sd(df$`Departure Delay in Minutes`)
print(paste("Standard Deviation of Departure Delay: ", sd_departure_delay))

# Install and load the dplyr package if not already installed
# install.packages("dplyr")
library(dplyr)

# Install and load the dplyr package if not already installed
# install.packages("dplyr")
library(dplyr)

# Define the path to your dataset
file_path <- "D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Airline Passenger Satisfaction/test.csv"

# Read the dataset into a data frame
df <- read.csv(file_path)

# Remove the first "id" column
df <- select(df, -id)

# Display the updated dataset without the "id" column
print(df)

# Define the path to your dataset
file_path <- "D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Airline Passenger Satisfaction/test.csv"

# Read the dataset into a data frame
df <- read.csv(file_path)

# Apply label encoding to the "satisfaction" column
df$satisfaction <- as.numeric(factor(df$satisfaction))

# Display the updated dataset with label-encoded "satisfaction" column
print(df)


# Count the occurrences of each level in the "satisfaction" column
satisfaction_counts <- table(df$satisfaction)

# Create a bar plot
barplot(satisfaction_counts, main = "Satisfaction Distribution", xlab = "Satisfaction Level", ylab = "Count", col = c("skyblue", "salmon"))


# Count the occurrences of each level in the "satisfaction" column
satisfaction_counts <- table(df$satisfaction)

# Create a pie plot
pie(satisfaction_counts, main = "Satisfaction Distribution", col = c("skyblue", "salmon"))





# Install caret package
install.packages("caret")

# Load caret package
library(caret)

# Define the path to your dataset
file_path <- "D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Airline Passenger Satisfaction/test.csv"

# Read the dataset into a data frame
df <- read.csv(file_path)

# Set a seed for reproducibility
set.seed(123)

# Create an index for the training set (80%)
train_index <- createDataPartition(df$satisfaction, p = 0.8, list = FALSE)

# Create the training and testing sets
train_set <- df[train_index, ]
test_set <- df[-train_index, ]

# Display the dimensions of the training and testing sets
cat("Dimensions of the training set:", dim(train_set), "\n")
cat("Dimensions of the testing set:", dim(test_set), "\n")


# Install and load necessary packages if not already installed
# install.packages("randomForest")
# install.packages("ggplot2")
library(randomForest)
library(ggplot2)

# Define the path to your dataset
file_path <- "D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Airline Passenger Satisfaction/test.csv"

# Read the dataset into a data frame
df <- read.csv(file_path)

# Apply label encoding to the "satisfaction" column
df$satisfaction <- as.numeric(factor(df$satisfaction))

# Set a seed for reproducibility
set.seed(123)

# Create an index for the training set (80%)
train_index <- createDataPartition(df$satisfaction, p = 0.8, list = FALSE)

# Create the training set
train_set <- df[train_index, ]

# Train a Random Forest model
rf_model <- randomForest(satisfaction ~ Gender, data = train_set)

# Create a data frame for plotting
plot_data <- data.frame(Gender = levels(df$Gender), Satisfaction = predict(rf_model, newdata = data.frame(Gender = levels(df$Gender))))

# Create a bar plot using ggplot2
ggplot(plot_data, aes(x = Gender, y = Satisfaction, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Satisfaction by Gender", x = "Gender", y = "Satisfaction") +
  scale_fill_manual(values = c("skyblue", "salmon"))



# Define the path to your dataset
file_path <- "D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Airline Passenger Satisfaction/test.csv"

# Read the dataset into a data frame
df <- read.csv(file_path)

# Apply label encoding to the "satisfaction" column
df$satisfaction <- as.numeric(factor(df$satisfaction))

# Set a seed for reproducibility
set.seed(123)

# Create an index for the training set (80%)
train_index <- createDataPartition(df$satisfaction, p = 0.8, list = FALSE)

# Create the training set
train_set <- df[train_index, ]

# Train a logistic regression model
log_reg_model <- glm(satisfaction ~ Gender, data = train_set, family = "binomial")

# Display the summary of the logistic regression model
summary(log_reg_model)







# Install and load necessary packages if not already installed
# install.packages("caret")
# install.packages("pROC")
library(caret)
library(pROC)

# Define the path to your dataset
file_path <- "D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Airline Passenger Satisfaction/test.csv"

# Read the dataset into a data frame
df <- read.csv(file_path)

# Apply label encoding to the "satisfaction" column
df$satisfaction <- as.numeric(factor(df$satisfaction))

# Set a seed for reproducibility
set.seed(123)

# Create an index for the training set (80%)
train_index <- createDataPartition(df$satisfaction, p = 0.8, list = FALSE)

# Create the training set
train_set <- df[train_index, ]

# Train a logistic regression model
log_reg_model <- glm(satisfaction ~ Gender, data = train_set, family = "binomial")

# Make predictions on the testing set
predictions <- predict(log_reg_model, newdata = df[-train_index, ], type = "response")

# Convert predicted probabilities to binary (1 or 2)
predicted_satisfaction <- ifelse(predictions > 0.5, 1, 2)

# Create a confusion matrix
conf_matrix <- confusionMatrix(factor(predicted_satisfaction), factor(df$satisfaction[-train_index]))

# Display the confusion matrix
print("Confusion Matrix:")
print(conf_matrix)




# Extract precision and recall from the confusion matrix
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]

# Display precision and recall
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")





