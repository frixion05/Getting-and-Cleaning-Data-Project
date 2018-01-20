# Peer-graded Assignment: Getting and Cleaning Data Course Project
# Ryan
# 2018-01-20

# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each 
# measurement.
# 3. Use descriptive activity names to name the activities in the data set.
# 4. Appropriately label the data set with descriptive variable names.
# 5. From the data set in step 4, create a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# -1. Clear environment and console.
rm(list = ls(all = TRUE))
cat("\014")

# 0. Download and unzip data.
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/Dataset.zip")
unzip(zipfile = "./data/Dataset.zip", exdir = "./data")


# 1. Merge the training and the test sets to create one data set.
## Read files.
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

features <- read.table('./data/UCI HAR Dataset/features.txt')

activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

## Assign column names. 
colnames(x_train) <- features[,2] 
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

## Merge dataset into one.
train <- cbind(y_train, subject_train, x_train)
test <- cbind(y_test, subject_test, x_test)
merged <- rbind(train, test)


# 2. Extract only the measurements on the mean and standard deviation for each 
# measurement.
## Read column names.
colNames <- colnames(merged)

## Create vector for ID, mean, and standard deviation.
meanAndSTD <- (grepl("activityId" , colNames) | 
                         grepl("subjectId" , colNames) | 
                         grepl("mean.." , colNames) | 
                         grepl("std.." , colNames) 
)

## Subset from merged dataset
subsetmeanAndSTD <- merged[ , meanAndSTD == TRUE]


# 3. Use descriptive activity names to name the activities in the data set.
subsetWithActivityNames <- merge(subsetmeanAndSTD, activityLabels,
                              by='activityId',
                              all.x=TRUE)


# 4. Appropriately label the data set with descriptive variable names.
## Done in #s 1 and 2.


# 5. From the data set in step 4, create a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
## Create tidy data.  
TidyData <- aggregate(. ~subjectId + activityId, subsetWithActivityNames, mean)
TidyData <- TidyData[order(TidyData$subjectId, TidyData$activityId),]

## Write to a csv txt file.
write.table(TidyData, "TidyData.txt", row.name = FALSE)
