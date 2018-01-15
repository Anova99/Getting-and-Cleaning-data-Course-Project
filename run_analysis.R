rm(list=ls())

getwd()

setwd("C:/Users/Julio/Desktop/Maestría Energía 2017/Coursera/Data Science Specialization JH/Curso 3 - Getting and cleaning data/Project/UCI HAR Dataset")

#Read the Train Data, the subjeact id, the features and the activities. 

train_data <- read.table("./train/X_train.txt",sep="",header = F)

var_names <- read.table("features.txt",sep="",header = F)

id_train <- read.table("./train/subject_train.txt",sep="",header = F)

activity_train <- read.table("./train/y_train.txt",sep="",header = F)

#Name the columns 

colnames(train_data) = var_names[,2]
colnames(id_train) <- "Subject ID"
colnames(activity_train) <- "Activity"

#Cbind to generate the train data frame

train_df <- cbind(id_train,activity_train,train_data)

View(train_df)

#Read the Test Data, the subjeact id, the features and the activities. 

test_data <- read.table("./test/X_test.txt",sep="",header = F)

id_test <- read.table("./test/subject_test.txt",sep="",header = F)

activity_test <- read.table("./test/y_test.txt",sep="",header = F)

#Name the columns

colnames(test_data) = var_names[,2]
colnames(id_test) <- "Subject ID"
colnames(activity_test) <- "Activity"

#Cbind to generate the test data frame

test_df <- cbind(id_test,activity_test,test_data)

View(test_df)

#Merge the train and test sets to create one data set 

tidy_data <- rbind(train_df,test_df)

View(tidy_data)

#Extracts only the measurements on the mean and standard deviation for each measurement. First we
#subset the columns with the words "mean" and "std" and afterwards we take out the ones that measure
#the meanFreq

std_mean <- tidy_data[,grep("*(mean|std)",colnames(tidy_data))]
std_mean <- std_mean[,-(grep("*meanFreq",colnames(std_mean)))]

View(std_mean)

std_mean_data <- cbind(tidy_data[,1:2],std_mean)

View(std_mean_data)

#Appropriately labels the data set with descriptive variable names.

Activities <- read.table("activity_labels.txt",sep="",header = F)

library(dplyr)

tidy_df <- merge(std_mean_data,Activities,by.x = "Activity",by.y = "V1",sort = FALSE)

View(tidy_df)

#We take out the column with the activity number and we replace it with the one with the activity names

tidy_df <- tidy_df[,-1]
tidy_df <- rename(tidy_df,Activity = V2)
tidy_df <- tidy_df[,c(1,68,2:67)]

#Create a second, independent tidy data set with the average 
#of each variable for each activity and each subject

tidy_df_2 <- group_by(tidy_df,`Subject ID`,Activity)

tidy_df_2 <- summarise_all(tidy_df_2,funs(mean))

View(tidy_df_2)

write.table(tidy_df_2,file = "tidy_df_2.txt",row.names = FALSE)

