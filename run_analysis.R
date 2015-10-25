##Course Project Getting and Cleanining Data
## Author: Enrique Ponte

# Clean up workspace
rm(list=ls())

##set my working directory
setwd("C:/Users/admin/Desktop/Coursera")

##Download the file and put the file in the Samsung folder
if(!file.exists("./Samsung")){dir.create("./Samsung")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Samsung/Dataset.zip")

##Unzip the file
unzip(zipfile="./Samsung/Dataset.zip",exdir="./Samsung")

##unzipped files are in the folder UCI HAR Dataset. Get the list of the files
path_rf <- file.path("./Samsung" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)

##Read data from the files into the variables

##Read the Activity files
dataActivityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ),header = FALSE)
dataActivityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"),header = FALSE)

##Read the Subject files
dataSubjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)
dataSubjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)

##Read Feartures files
dataFeaturesTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)


##Merges the training and the test sets to create one data set
##Concatenate the data tables by rows
dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
dataActivity<- rbind(dataActivityTrain, dataActivityTest)
dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)

##set names to variables
names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
dataFeaturesNames <- read.table(file.path(path_rf, "features.txt"),head=FALSE)
names(dataFeatures)<- dataFeaturesNames$V2

##Merge columns to get the data frame Data for all data
dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, dataCombine)

##Extracts only the measurements on the mean and standard deviation for each measurement
##Subset Name of Features by measurements on the mean and standard deviation
##taken Names of Features with "mean()" or "std()"

subdataFeaturesNames<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]

##Subset the data frame Data by seleted names of Features
selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity" )
Data<-subset(Data,select=selectedNames)
colnames(Data) <- tolower(colnames(Data))

##Uses descriptive activity names to name the activities in the data set
##Read descriptive activity names from "activity_labels.txt"
activityLabels = read.table(file.path(path_rf, "activity_labels.txt"), sep="", header=FALSE)
currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
    Data$activity <- gsub(currentActivity, currentActivityLabel, Data$activity)
    currentActivity <- currentActivity + 1
}
##Appropriately labels the data set with descriptive variable names
##In the former part, variables activity and subject and names of the activities have been labelled using descriptive names.
##In this part, Names of Feteatures will labelled using descriptive variable names.

names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

##Creates a second,independent tidy data set and ouput it
##In this part,a second, independent tidy data set will be created with the average of each variable
##for each activity and each subject based on the data set in step 4.

library(dplyr);
Data2<-aggregate(. ~subject + activity, Data, mean)
Data2<-Data2[order(Data2$subject,Data2$activity),]
write.table(Data2, file = "tidy.txt",row.name=FALSE)



