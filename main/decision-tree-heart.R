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
filename = "../data/heart.csv"
data <- read.csv(file = filename, sep =",", header = TRUE)

# Convert columns to factors
index <- c(2, 3, 6, 7, 9, 11, 12)
data[ , index] <- lapply(data[ , index], as.factor)

ggplot(data, aes(x=Age, y=HeartDisease)) + geom_point()
ggplot(data, aes(x=RestingBP, y=HeartDisease)) + geom_point()

data$Cholesterol <- ifelse(data$Cholesterol <= 200, "Normal", "High")
data$Age <- cut(data$Age, breaks = 4)

ggplot(data, aes(x=ST_Slope, fill=ST_Slope)) + geom_bar() +
  geom_text(stat="count", aes(label=..count..), vjust=-0.25) +
  labs(x = "ST_Slope", y = "Frequency")

# Percentaje of training examples
training_p <- 0.8

# Generate data partition 80% training / 20% test. The result is a vector with the indexes 
# of the examples that will be used for the training of the model.
training_indexes <- createDataPartition(y = data$HeartDisease, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_indexes, ]  # Extract training data using training_indexes
test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes 

best_model <- NULL
best_acuraccy <- 0
for (i in 1:10) {
  # Create Linear Model using training data. Formula = all the columns except HeartDisease
  model <- rpart(formula = HeartDisease ~., data = training_data)
  
  # Make the prediction using the model and test data
  prediction <- predict(model, test_data, type = "class")
  
  # Calculate accuracy using Confusion Matrix
  prediction_results <- table(test_data$HeartDisease, prediction)
  matrix <- confusionMatrix(prediction_results)
  accuracy <- matrix$overall[1]
  
  if (accuracy > best_acuraccy) {
    best_model <- model
    best_acuraccy <- accuracy
  }
}

# Print the accuracy
best_acuraccy <- paste0("Accuracy = ", round(100*best_acuraccy, digits = 2), "%")
print(best_acuraccy, quote = FALSE)

# Print attributes in descending relevance
attrs <- names(best_model$variable.importance)

print("Attributes in descending order of relevance")

for (i in 1:length(attrs)) {
  print(paste0("  ", attrs[i]), quote = FALSE)
}

# Plot tree (this method is slow, wait until pot is completed)
rpart.plot(best_model, 
           type = 2,
           extra = 102,
           tweak = 1.1,
           box.palette = "GnYlRd",
           shadow.col = "darkgray",
           main = "Go to hospital or stay at home?", 
           sub = best_acuraccy)

# Print the rules that represent the Decision Tree
rpart.rules(best_model, 
            style="wide", 
            cover = TRUE, 
            eq = "=", 
            when = "IF", 
            and = "&&", 
            extra = 4)