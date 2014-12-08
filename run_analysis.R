# Clean slate
#rm(list=ls())
#setwd("C:/~")

########################################################################################
### Step 1 ### Merges the training and the test sets to create one data set.   
# Loading all necessary data and assigning column names
sub_train <- read.table("subject_train.txt")
x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
sub_test <- read.table("subject_test.txt")
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
features <- read.table("features.txt")
actLabels <- read.table("activity_labels.txt")
allData <- rbind(x_train, x_test)
allLabel <- rbind(y_train, y_test)
allSubject <- rbind(sub_train,sub_test)
colnames(actLabels) <- c('activityId','activityType')
colnames(sub_train) <- "subjectId"
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityId"
colnames(sub_test) <- "subjectId"
colnames(x_test) <- features[,2]
colnames(y_test) <- "activityId"
# Combining different train and test data
finalData <- rbind(x_train, x_test)
finalSub <- rbind(sub_train, sub_test)
finalAct <- rbind(y_train, y_test)

### Step 2 ### Extract only the measurements on the mean and standard deviation for each measurement.
# Getting rid of all variables except those having something to do with mean and std
meanStd <- grep("mean\\(\\)|std\\(\\)", features[,2]) 
joinData <- finalData[,meanStd]


### Steps 3 and 4 ###
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names.
# Starting by labeling those variables
# Merging the finalData set with the acitivityType table to include descriptive activity names
# And storing dataset into memory

names(joinData) <- gsub("\\(\\)", "", features[meanStd, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names
finalData <- cbind(finalAct, finalSub, joinData)
finalData <- merge(finalData,actLabels,by='activityId',all.x=TRUE)
write.table(finalData, 'finalData.txt', row.names=F)

### Step 5 ### Create a second, independent tidy data set with the average of each variable for each activity 
# and each subject.
# Dismissing activity type from dataset
finalDataNoActivityType <- finalData[,names(finalData) != 'activityType']
# Summarizing the finalDataNoActivityType table to include just the mean of each variable 
# for each activity and each subject
tidyData <- aggregate(finalDataNoActivityType[,-c(1,2)], by = list(
        activityId = finalDataNoActivityType$activityId,
        subjectId = finalDataNoActivityType$subjectId), mean)
# Merging the tidyData with activityType to include descriptive acitvity names
tidyData <- merge(tidyData,actLabels,by='activityId',all.x=TRUE)
# Export the tidyData set
write.table(tidyData, 'tidyData.txt',row.names=F)






