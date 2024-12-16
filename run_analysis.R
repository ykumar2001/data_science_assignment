# Load required libraries
library(dplyr)

# Step 1: Download and unzip the dataset if it doesn't exist
data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data_dir <- "UCI HAR Dataset"

if (!file.exists(data_dir)) {
  download.file(data_url, destfile = "dataset.zip")
  unzip("dataset.zip")
}

# Step 2: Load the training and test datasets
# Load features and activity labels
features <- read.table(file.path(data_dir, "features.txt"), col.names = c("index", "feature"))
activities <- read.table(file.path(data_dir, "activity_labels.txt"), col.names = c("code", "activity"))

# Load training data
train_subject <- read.table(file.path(data_dir, "train", "subject_train.txt"), col.names = "subject")
train_x <- read.table(file.path(data_dir, "train", "X_train.txt"), col.names = features$feature)
train_y <- read.table(file.path(data_dir, "train", "y_train.txt"), col.names = "activity_code")

# Load test data
test_subject <- read.table(file.path(data_dir, "test", "subject_test.txt"), col.names = "subject")
test_x <- read.table(file.path(data_dir, "test", "X_test.txt"), col.names = features$feature)
test_y <- read.table(file.path(data_dir, "test", "y_test.txt"), col.names = "activity_code")

# Step 3: Merge the training and test datasets
subject <- rbind(train_subject, test_subject)
features_data <- rbind(train_x, test_x)
activity <- rbind(train_y, test_y)

# Combine all into one dataset
merged_data <- cbind(subject, activity, features_data)

# Step 4: Extract measurements on the mean and standard deviation for each measurement
selected_columns <- grepl("mean\\(\\)|std\\(\\)", features$feature)
data_mean_std <- merged_data[, c(1, 2, selected_columns)]

# Step 5: Use descriptive activity names to name the activities in the data set
data_mean_std$activity_code <- activities[data_mean_std$activity_code, 2]
colnames(data_mean_std)[2] <- "activity"

# Step 6: Appropriately label the dataset with descriptive variable names
colnames(data_mean_std) <- gsub("^t", "Time", colnames(data_mean_std))
colnames(data_mean_std) <- gsub("^f", "Frequency", colnames(data_mean_std))
colnames(data_mean_std) <- gsub("Acc", "Accelerometer", colnames(data_mean_std))
colnames(data_mean_std) <- gsub("Gyro", "Gyroscope", colnames(data_mean_std))
colnames(data_mean_std) <- gsub("Mag", "Magnitude", colnames(data_mean_std))
colnames(data_mean_std) <- gsub("BodyBody", "Body", colnames(data_mean_std))

# Step 7: Create a second, independent tidy dataset with the average of each variable for each activity and each subject
tidy_data <- data_mean_std %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean))

# Step 8: Save the tidy dataset to a file
write.table(tidy_data, file = "tidy_data.txt", row.names = FALSE)
