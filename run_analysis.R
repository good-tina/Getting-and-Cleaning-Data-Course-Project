# Getting and Cleaning Data course project
# Data files were retrieved on https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# Tasks
## 1. Merge the raining and the test sets to create one data set.
## 2. Extract only the measurements on the mean and standard deviation for each measurement.
## 3. Use descriptive activity names t name the activities in the data set.
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Task 1 - Merge the raining and the test sets to create one data set.
setwd("./UCI HAR Dataset")

# Reading train data
features <- read.table("./features.txt", header = FALSE);
activityLabels <- read.table("./activity_labels.txt", header = FALSE, col.names = c("ActivityId", "ActivityType"));
subjects <- read.table("./train/subject_train.txt", header = FALSE, col.names = c("SubjectId"));
x <- read.table("./train/x_train.txt", header = FALSE, col.names = c(features[,2]));
y <- read.table("./train/y_train.txt", header = FALSE, col.names = c("ActivityId"));

# Adding test data
subjects <- rbind(subjects, read.table("./test/subject_test.txt", header = FALSE, col.names = c("SubjectId")));
x <- rbind(x, read.table("./test/x_test.txt", header = FALSE, col.names = c(features[,2])));
y <- rbind(y, read.table("./test/y_test.txt", header = FALSE, col.names = c("ActivityId")));

# Mergin data
mergeDataSet <- cbind(subjects,x,y);

## Task 2 - Extract only the measurements on the mean and standard deviation for each measurement.
columnNames <- colnames(mergeDataSet)
# Creating identifier vector with relevant columns (ids, means and standard deviation)
vectorOfRelevantColumns <- (grepl("ActivityId",columnNames) | grepl("SubjectId",columnNames) 
                            | grepl("mean",columnNames) |  grepl("std",columnNames));

# Extracting relevant columns
relevantColumnsDataSet <- mergeDataSet[vectorOfRelevantColumns == TRUE];

## Task 3 - Use descriptive activity names t name the activities in the data set.
# Adding ActivityName column based on Activity Id of the row
relevantColumnsDataSet$ActivityLabel <- activityLabels[,2][match(relevantColumnsDataSet$ActivityId, activityLabels[,1])] 


## Task 4 - Appropriately labels the data set with descriptive variable names.
columnNames <- colnames(relevantColumnsDataSet);
# Getting modified column names
columnNames <- gsub("std$","StandardDeviation",columnNames);
columnNames <- gsub("mean","Mean",columnNames);
columnNames <- gsub("^t","Time",columnNames);
columnNames <- gsub("^f","Frequency",columnNames);
columnNames <- gsub("([Gg]ravity)","Gravity",columnNames);
columnNames <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columnNames);
columnNames <- gsub("[Gg]yro","Gyro",columnNames);
columnNames <- gsub("AccMag","AccelerationMagnitude",columnNames);
columnNames <- gsub("([Bb]ody[Aa]cc[Jj]erkmag)","BodyAccelerationJerkMagnitude",columnNames);
columnNames <- gsub("JerkMag","JerkMagnitude",columnNames);
columnNames <- gsub("GyroMag","GyroMagnitude",columnNames);
# Substituting column names 
colnames(relevantColumnsDataSet) <- columnNames;

## Task 5 - From the data set in task 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Importing Library
library(reshape2)
# Melting data set to make it easier to get means
meltedDataSet <- melt(relevantColumnsDataSet, id = c("SubjectId", "ActivityId"));
meltedDataSet$value <- as.numeric(meltedDataSet$value);
# Getting means data set
meanDataSet <- dcast(meltedDataSet, ActivityId + SubjectId ~ variable, mean);

# Saving tidy data set
write.table(meanDataSet, file = "finalTidyMeanDataSet.txt", row.name=FALSE, sep="\t")
