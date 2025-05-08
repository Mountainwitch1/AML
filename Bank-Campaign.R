# Load Data
# Load the data from the CSV file for both test and train datasets
test <- read.csv("Bank Marketing Campaign.csv", sep = ",")
train <- read.csv("Bank Marketing Campaign.csv", sep = ",")


# Check the structure of both test and train datasets
str(test)
str(train)

# Combine datasets
# Add a column to label which dataset each row comes from (test or train)
test$dataset <- "test"
train$dataset <- "train"

# Combine both datasets into one big dataset
combine <- rbind(test, train)

# Explore the data
# Check the structure, dimensions, and summary of the combined dataset
str(combine)
dim(combine)
summary(combine)

# Look at how many different jobs, education levels, and marital statuses exist in the data
table(combine$job)
table(combine$education)
table(combine$marital)

# Visualize features
# Create a bar chart showing the distribution of the target variable (y)
ggplot(combine, aes(x = y)) +
  geom_bar(fill = "rosybrown") +
  labs(title = "Target Variable Distribution", x = "Subscription to Term Deposits", y = "Count")

# Create a bar chart showing the distribution of marital status
ggplot(combine, aes(x = marital)) +
  geom_bar(fill = "salmon", color = "black") +
  labs(title = "Marital Status Distribution", x = "Marital Status", y = "Count")

# Create a bar chart showing the distribution of different jobs
ggplot(combine, aes(y = job)) +
  geom_bar(fill = "plum") +
  labs(title = "Job Distribution", x = "Count", y = "Job")

# Create a density plot to show the distribution of call durations
ggplot(combine, aes(x = duration)) +
  geom_density(fill = "lightblue", alpha = 0.7, na.rm = TRUE) +
  labs(title = "Call Duration", x = "Duration (seconds)", y = "Density")

# Create a histogram to show how many contacts were made during the campaign
ggplot(combine, aes(x = campaign)) +
  geom_histogram(binwidth = 1, fill = "peru", color = "black") +
  labs(title = "Campaign Contacts", x = "Number of Contacts", y = "Frequency")


# Principal Component Analysis (PCA)
# Select only numeric columns for PCA
numeric_data <- combine[sapply(combine, is.numeric)]

# Scale the data (standardize it) so that all features have similar ranges
scaled_data <- scale(numeric_data)

# Remove rows with missing values in the scaled data
scaled_data <- na.omit(scaled_data)

# Perform PCA to reduce the data to fewer dimensions (principal components)
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Show the summary of the PCA result (explains the variance captured by each component)
summary(pca_result)

# Create a scatterplot of the first two principal components (PC1 and PC2)
pc1_scores <- pca_result$x[, 1]
pc2_scores <- pca_result$x[, 2]
pca_plot_data <- data.frame(PC1 = pc1_scores, PC2 = pc2_scores)
ggplot(pca_plot_data, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.7) +
  labs(x = "Principal Component 1", y = "Principal Component 2",
       title = "PCA Scatterplot of First Two Components")

# Handle Missing Values
# Check the percentage of missing values in each column
missing_summary <- colSums(is.na(combine)) / nrow(combine) * 100
print(missing_summary)

# Use a method called Multiple Imputation to fill in missing values
library(mice)
imputed_data <- mice(combine, method = "pmm", m = 1, maxit = 5, seed = 500)

# Update the dataset with the imputed values (missing values filled)
combine <- complete(imputed_data)

# Detect Outliers
# Calculate the first and third quartiles for the 'duration' column to find outliers
Q1 <- quantile(combine$duration, 0.25, na.rm = TRUE)
Q3 <- quantile(combine$duration, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define the range for valid 'duration' values, using the IQR rule to detect outliers
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

# Remove any rows where 'duration' is an outlier (too high or too low)
combine <- combine[combine$duration >= lower & combine$duration <= upper, ]

# Boxplot After Outlier Removal
# Create a boxplot of 'duration' to show how removing outliers affects the data
ggplot(combine, aes(x = "", y = duration)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red") +
  labs(title = "Boxplot After Removing Outliers", x = "Duration", y = "Values")

# Multicollinearity
# Select a few numeric columns and check how correlated they are with each other
num_vars <- combine[, c("age", "duration", "balance", "pdays", "previous")]
correlation_matrix <- cor(num_vars, use = "complete.obs")
print(correlation_matrix)

# Create a heatmap to visualize the correlation between numeric variables
library(ggcorrplot)
ggcorrplot(correlation_matrix, hc.order = TRUE, type = "lower", lab = TRUE)

# Remove Low Variance Columns
# Remove columns that don't have much variation (e.g., if a column has the same value for most rows)
combine <- combine[, apply(combine, 2, function(col) length(unique(col)) > 1)]

# Scale Numeric Variables
# Standardize the 'age' and 'duration' columns to have a mean of 0 and a standard deviation of 1
combine[, c("age", "duration")] <- scale(combine[, c("age", "duration")])

# Compare the original 'duration' values with the scaled (standardized) ones
original_duration <- numeric_data$duration[rownames(numeric_data) %in% rownames(combine)]
scaled_duration <- combine$duration
filtered_data <- na.omit(data.frame(Original = original_duration, Scaled = scaled_duration))

# Create a scatterplot to show how scaling changes the 'duration' values
ggplot(filtered_data, aes(x = Original, y = Scaled)) +
  geom_point(alpha = 0.7, color = "blue") +
  labs(title = "Impact of Scaling on Duration", x = "Original Values", y = "Scaled Values")

#Part 2 from here 

