# Coursera JHU Getting and Cleaning Data Course Project
# Casey St. Pierre
# March 15, 2022

# runAnalysis.r will:

# Input UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

###### 1. Here I am merging test and training sets to make one data set.
setwd('C:/Users/cjstp/Desktop/RStudioTour/UCI HAR Dataset');

# Import training data from files & Name the columns 
features <- read.table('./features.txt',header=FALSE);
activityLabels <- read.table('./activity_labels.txt',header=FALSE); 
colnames(activityLabels) <- c("activityId","activityType");
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); 
colnames(subjectTrain) <- "subjectId";
xTrain <- read.table('./train/x_train.txt',header=FALSE); colnames(xTrain) <- 
  features[,2];
yTrain <- read.table('./train/y_train.txt',header=FALSE); colnames(yTrain) <- 
  "activityId";

# Merge Data into complete training set
trainingSet = cbind(yTrain,subjectTrain,xTrain);

# Import test data from files & Name columns
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); 
colnames(subjectTest) <- "subjectId";
xTest <- read.table('./test/x_test.txt',header=FALSE); colnames(xTest) <- 
  features[,2];
yTest <- read.table('./test/y_test.txt',header=FALSE); colnames(yTest) <- 
  "activityId";

# Merge Data into complete test set
testSet = cbind(yTest,subjectTest,xTest);

# Combine Training Data Set and Test Data Set into one Merged Data Set
MergedDataSet = rbind(trainingSet,testSet);

# Create columns vector to prepare data for subsetting
columns <- colnames(MergedDataSet);

###### 2. Now I am extracting only the mean and standard dev for each measurement.

# Create a vector that indentifies the ID, mean & stddev columns as TRUE
vector <- (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) &
             !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | 
             grepl("-std..",columns) & !grepl("-std()..-",columns));

# Update MergedDataSet based on previously identified columns
MergedDataSet <- MergedDataSet[vector==TRUE];

###### 3. And here I am using descriptive activity names for the activities in the data set.

# Add in descriptive activity names to MergedDataSet & update columns vector
MergedDataSet <- merge(MergedDataSet,activityLabels,by='activityId',all.x=TRUE);
MergedDataSet$activityId <-activityLabels[,2][match(MergedDataSet$activityId, activityLabels[,1])] 

columns <- colnames(MergedDataSet);

###### 4. Applying appropriate labels to the data set, using descriptive variable names.

# Tidy column names
for (i in 1:length(columns)) 
{
  columns[i] <- gsub("\\()","",columns[i])
  columns[i] <- gsub("-std$","StdDev",columns[i])
  columns[i] <- gsub("-mean","Mean",columns[i])
  columns[i] <- gsub("^(t)","time",columns[i])
  columns[i] <- gsub("^(f)","freq",columns[i])
  columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
  columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
  columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
  columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
  columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
  columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
  columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
};

# Update MergedDataSet with new descriptive column names
colnames(MergedDataSet) <- columns;

# Remove activityType column
MergedDataSet <- MergedDataSet[,names(MergedDataSet) != 'activityType'];

###### 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Averaging each activity and each subject as Tidy Data
tidyData <- aggregate(MergedDataSet[,names(MergedDataSet) 
                                    != c('activityId','subjectId')],by=list
                      (activityId=MergedDataSet$activityId,
                        subjectId=MergedDataSet$subjectId),mean);

# Export tidyData set 
write.table(tidyData, './FinalTidyData.txt',row.names=FALSE,sep='\t')