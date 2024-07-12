#Setting directory
getwd()
setwd("C:/Users/anish/OneDrive/Desktop/codsoft")

#Installing packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("reshape2")
install.packages("tidyverse")
install.packages("caret")
install.packages("ROCR")
install.packages("pROC")

library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(tidyverse)
library(caret)
library(ROCR)
library(pROC)


#Read dataset
df <- read.csv("Titanic-Dataset.csv")

# Cheking data quality issues
summary(df)

colSums(is.na(df))

# Count the number of NA values and blank strings in each column
na_blank_summary <- sapply(df, function(x) sum(is.na(x) | x == ""))
na_blank_summary

# Boxplot for Age
boxplot(df$Age, main = "Boxplot of Age")

# Boxplot for Fare
boxplot(df$Fare, main = "Boxplot of Fare")


## Converting to categorical columns
df$Embarked <- as.factor(df$Embarked)
df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)
df$Sex <- as.factor(df$Sex)

#Addressing data quality issues

##1. Drop unncessary columns without useful data
# Drop columns PassengerId, Name, and Cabin
df <- df %>%
  select(-PassengerId, -Name, -Cabin, -Ticket)

##2..Replaced Embarked blanks using mode


# Replace blank strings with NA
df[df$Embarked == "", "Embarked"] <- NA

# Function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode of the Embarked column
mode_embarked <- get_mode(df$Embarked)

print(mode_embarked)

# Replace NA values in Embarked with the mode
df$Embarked[is.na(df$Embarked)] <- mode_embarked

# Verify that there are no missing values in the Embarked column
final_missing_embarked <- sum(is.na(df$Embarked))
print(paste("Missing values in Embarked column after imputation:", final_missing_embarked))

#Updated summary
na_blank_summary <- sapply(df, function(x) sum(is.na(x) | x == ""))
print(na_blank_summary)


##3. Replace Age values with the average age
average_age <- mean(df$Age, na.rm = TRUE)  # Calculate the average age
# Replace NAs with average age
df$Age[is.na(df$Age)] <- average_age
# Replace values above 65 with average age
df$Age[df$Age > 65] <- average_age  # Replace values above 60 with the average

sum(df$Age > 65, na.rm = TRUE)
sum(is.na(df$Age))



##4. Replace Fare values above 400 with the average fare
average_fare <- mean(df$Fare)  # Calculate the average fare
df$Fare[df$Fare > 400] <- average_fare  # Replace values above 400 with the average

sum(df$Fare > 400, na.rm = TRUE)


##5. Correct no of family members
# Add a new column TotalMembers
df$TotalMembers <- df$SibSp + df$Parch

df <- df %>%
  select(-SibSp,-Parch)

## Data exploratory visualization
# Stacked bar plot of Pclass vs. Survived
ggplot(df, aes(x = as.factor(Pclass), fill = as.factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival Proportion by Passenger Class", x = "Passenger Class", y = "Proportion", fill = "Survived") +
  scale_fill_manual(values = c("red", "green"), labels = c("No", "Yes")) +
  theme_minimal()

# Density plot of Age by Survived
ggplot(df, aes(x = Age, fill = as.factor(Survived))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age by Survival Status", x = "Age", y = "Density", fill = "Survived") +
  scale_fill_manual(values = c("red", "green"), labels = c("No", "Yes")) +
  theme_minimal()

# Bar plot of Sex vs. Survived
ggplot(df, aes(x = Sex, fill = as.factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Sex", x = "Sex", y = "Count", fill = "Survived") +
  scale_fill_manual(values = c("red", "green"), labels = c("No", "Yes")) +
  theme_minimal()

## Modelling
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(df$Survived, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

model <- glm(Survived ~ Pclass + Sex + Age + Fare,
             data = train_data, family = "binomial")
summary(model)

model <- glm(Survived ~ Pclass + Sex + Age + Embarked,
             data = train_data, family = "binomial")
summary(model)

model <- glm(Survived ~ Pclass + Sex + Age + TotalMembers,
             data = train_data, family = "binomial")
summary(model)


predictions <- predict(model, newdata = test_data, type = "response")
predictions_class <- ifelse(predictions > 0.5, 1, 0)  # Convert probabilities to class labels

# Confusion matrix
confusion_matrix <- confusionMatrix(table(predictions_class, test_data$Survived))
confusion_matrix

# Calculate accuracy
accuracy <- confusion_matrix$overall['Accuracy']
cat(accuracy)

# Create prediction object
pred <- prediction(predictions, test_data$Survived)

# Calculate performance
perf <- performance(pred, "tpr", "fpr")

# Plot ROC curve
plot(perf, colorize = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Calculate AUC
auc_perf <- performance(pred, measure = "auc")
auc <- auc_perf@y.values[[1]]
cat("AUC:", auc, "\n")

