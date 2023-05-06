# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install required packages
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)

# Read data from CSV
filename = "../data/covid-19-symptoms.tab"
data <- read.csv(file = filename, sep =" ", header = TRUE)

# Convert columns to factors
index <- 1:ncol(data)
data[ , index] <- lapply(data[ , index], as.factor)

# Percentaje of training examples
training_p <- 0.8

# Generate data partition 80% training / 20% test. The result is a vector with the indexes 
# of the examples that will be used for the training of the model.
training_indexes <- createDataPartition(y = data$TARGET, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_indexes, ]  # Extract training data using training_indexes
test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes 

# Create Linear Model using training data. Formula = all the columns except Salary
model <- rpart(formula = TARGET ~., data = training_data)

# Make the prediction using the model and test data
prediction <- predict(model, test_data, type = "class")

# Calculate accuracy using Confusion Matrix
prediction_results <- table(test_data$TARGET, prediction)
matrix <- confusionMatrix(prediction_results)
accuracy <- matrix$overall[1]

# Print the accuracy
accuracy <- paste0("Accuracy = ", round(100*accuracy, digits = 2), "%")
print(accuracy, quote = FALSE)

# Print attributes in descending relevance
attrs <- names(model$variable.importance)

print("Attributes in descending order of relevance")

for (i in 1:length(attrs)) {
  print(paste0("  ", attrs[i]), quote = FALSE)
}

# Plot tree (this method is slow, wait until pot is completed)
rpart.plot(model, 
           type = 2,
           extra = 102,
           tweak = 1.1,
           box.palette = "GnYlRd",
           shadow.col = "darkgray",
           main = "Go to hospital or stay at home?", 
           sub = accuracy)

# Print the rules that represent the Decision Tree
rpart.rules(model, 
            style="wide", 
            cover = TRUE, 
            eq = "=", 
            when = "IF", 
            and = "&&", 
            extra = 4)