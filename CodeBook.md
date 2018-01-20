# Getting and Cleaning Data Project
This is *code book* of the course project for Getting and Cleaning Data module of the Data Science Specialization track of Johns Hopkins University in Coursera.  
By Ryan  
2018-01-20

## Description
This document describes the data set and the analysis performed.

## Data
One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## Data set information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Attribute information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### Variables
- Data from the downloads are stored in these variables: `x_train`, `y_train`, `x_text`, `y_test`, `subject_train` and `subject_test`. 
- The list of all features is stored in `features` taken from features.txt while the link of class labels with their activity name is stored in `activityLabels`.  


## Analysis
-1. Clear environment and console.

0. Download and unzip data.

1. Merge the training and the test sets to create one data set.
Read files.
Assign column names. 
Merge dataset into one.

2. Extract only the measurements on the mean and standard deviation for each measurement.
Read column names.
Create vector for ID, mean, and standard deviation.
Subset from merged dataset

3. Use descriptive activity names to name the activities in the data set.

4. Appropriately label the data set with descriptive variable names.
Done in #s 1 and 2.

5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
Create tidy data.  
Write to a csv txt file.
