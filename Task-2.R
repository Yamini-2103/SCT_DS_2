# -----------------------------------------------------
# TASK 02 : DATA CLEANING & EDA ON TITANIC DATASET (R)
# -----------------------------------------------------

# Install packages (if needed)
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("dplyr")

library(tidyverse)
library(ggplot2)
library(dplyr)

# ---------------------------
# 1. Load Titanic Dataset
# ---------------------------

url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
titanic <- read.csv(url)

head(titanic)
dim(titanic)

# ---------------------------
# 2. Data Cleaning
# ---------------------------

colSums(is.na(titanic))

# Fill Age NA with median
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

# Fill Embarked NA with mode
mode_embarked <- names(sort(table(titanic$Embarked), decreasing = TRUE))[1]
titanic$Embarked[is.na(titanic$Embarked)] <- mode_embarked

# Remove Cabin
titanic <- titanic %>% select(-Cabin)

colSums(is.na(titanic))

# ---------------------------
# 3. Summary Statistics
# ---------------------------

summary(titanic)

# ---------------------------
# 4. UNIVARIATE ANALYSIS
# ---------------------------

# Age distribution with labels
ggplot(titanic, aes(Age)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(
    title = "Age Distribution of Passengers",
    x = "Age of Passenger",
    y = "Count of Passengers"
  )

# Survived count with labels
ggplot(titanic, aes(factor(Survived))) +
  geom_bar(fill = "orange", color = "black") +
  labs(
    title = "Passenger Survival Count",
    x = "Survival Status (0 = No, 1 = Yes)",
    y = "Number of Passengers"
  )

# ---------------------------
# 5. BIVARIATE ANALYSIS
# ---------------------------

# Survival by Gender with labels
ggplot(titanic, aes(Sex, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival Count by Gender",
    x = "Gender",
    y = "Number of Passengers",
    fill = "Survived (1 = Yes)"
  )

# Survival by Passenger Class with labels
ggplot(titanic, aes(factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival Count by Passenger Class",
    x = "Passenger Class",
    y = "Number of Passengers",
    fill = "Survived (1 = Yes)"
  )

# Age vs Fare scatterplot with labels
ggplot(titanic, aes(Age, Fare, color = factor(Survived))) +
  geom_point() +
  labs(
    title = "Relationship Between Age and Fare",
    x = "Age of Passenger",
    y = "Fare Paid",
    color = "Survived (1 = Yes)"
  )

# ---------------------------
# 6. CORRELATION ANALYSIS
# ---------------------------

numeric_data <- titanic %>% select_if(is.numeric)
cor_matrix <- cor(numeric_data)

heatmap(
  cor_matrix,
  Colv = NA, Rowv = NA,
  col = heat.colors(256),
  main = "Correlation Heatmap of Numeric Variables",
  xlab = "Variables",
  ylab = "Variables"
)

