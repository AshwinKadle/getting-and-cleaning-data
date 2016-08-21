##########################################################################################################
## Coursera Getting and Cleaning Data Course Project
## 
# tidydata.r File Description:
# This script will perform the following steps on the UCI HAR Dataset downloaded from 
#
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#
# Assumption ###
# 1. It is assumed that the user running the script has changed to the appropriate folder where the dataset is unzipped 
#
##########################################################################################################


# Clean up workspace
rm(list=ls())

# 1. Merge the training and the test sets to create one data set.

# Read in the data from files
features     <- read.table('./features.txt',header=FALSE); #imports features.txt
activitylabel <- read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjecttrain <- read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xtrain       <- read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
ytrain       <- read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

# Assigin column names to the data imported above
colnames(activitylabel)  <- c('activityid','activitytype');
colnames(subjecttrain)  <- "subjectid";
colnames(xtrain)        <- features[,2]; 
colnames(ytrain)        <- "activityid";

# Read in the test data
subjecttest <- read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xtest       <- read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
ytest       <- read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names to the test data imported above
colnames(subjecttest) <- "subjectid";
colnames(xtest)       <- features[,2]; 
colnames(ytest)       <- "activityid";

# Create the final training set by merging ytrain, subjecttrain, and xtrain
trainingData <- cbind(ytrain,subjecttrain,xtrain);

# Create the final test set by merging the xtest, ytest and subjecttest data
testData <- cbind(ytest,subjecttest,xtest);


# Combine training and test data to create a final data set
combinedData <- rbind(trainingData,testData);

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# Create a vector for the column names from the combinedData, which will be used
# to select the desired mean() & stddev() columns

colnames  <- colnames(combinedData); 
logicvector <- (grepl("activity..",colnames) | grepl("subject..",colnames) | grepl("-mean..",colnames) & !grepl("-meanFreq..",colnames) & !grepl("mean..-",colnames) | grepl("-std..",colnames) & !grepl("-std()..-",colnames));

# Subset CombinedData to finalData based on the logicalVector to keep only desired columns
finalData <- combinedData[logicvector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the activitylabel table to include descriptive activity names
finalData <- merge(finalData,activitylabel,by='activityid',all.x=TRUE);

# Updating the colnames vector to include the new column names after merge
colnames  <- colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colnames)) 
{
  colnames[i] <- gsub("\\()","",colnames[i])
  colnames[i] <- gsub("-std$","StdDev",colnames[i])
  colnames[i] <- gsub("-mean","Mean",colnames[i])
  colnames[i] <- gsub("^(t)","time",colnames[i])
  colnames[i] <- gsub("^(f)","freq",colnames[i])
  colnames[i] <- gsub("([Gg]ravity)","Gravity",colnames[i])
  colnames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colnames[i])
  colnames[i] <- gsub("[Gg]yro","Gyro",colnames[i])
  colnames[i] <- gsub("AccMag","AccMagnitude",colnames[i])
  colnames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colnames[i])
  colnames[i] <- gsub("JerkMag","JerkMagnitude",colnames[i])
  colnames[i] <- gsub("GyroMag","GyroMagnitude",colnames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) <- colnames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activitytype column
finalDataNoActivityType  <- finalData[,names(finalData) != 'activitytype'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityid','subjectid')],by=list(activityid=finalDataNoActivityType$activityid,subjectid = finalDataNoActivityType$subjectid),mean);

# Merging the tidyData with activitytype to include descriptive acitvity names
tidyData    <- merge(tidyData,activitylabel,by='activityid',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData-WithoutRowNames.txt',row.names=FALSE,sep='\t');

