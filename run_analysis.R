#read the data sets from the downloaded zip file

setwd("F://Data Science//ื๗าต//Getting and Cleaning Data//UCI HAR Dataset")
X_train <- read.table(".//train//X_train.txt")
y_train <- read.table(".//train//y_train.txt")
subject_train <- read.table(".//train//subject_train.txt")
X_test <- read.table(".//test//X_test.txt")
y_test <- read.table(".//test//y_test.txt")
subject_test <- read.table(".//test//subject_test.txt")
activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

#Merges the training and the test sets to create one data set by rbind() function
X_all <- rbind(X_train, X_test)

#Extracts only the measurements on the mean and standard deviation for each measurement
#Using the grep() function to extract the ...mean() and ...std() features from the "feature.txt"

colnames(X_all) <- as.character(features[,2])
mean <- grep("mean()", colnames(X_all),fixed = TRUE)
std <- grep("std()", colnames(X_all),fixed = TRUE)
X_all_meansd <- X_all[, c(mean, std)]

#Uses descriptive activity names to name the activities in the data set
y_all<- rbind(y_train,y_test)
activity <- cbind(y_all, X_all_meansd)
colnames(activity)[1] <- "Activity" #name those activities

#Appropriately labels the data set with descriptive variable names
#Here I used a for-loop to label those variables

activity_labels[,2] <- as.character(activity_labels[,2])
for(i in 1:length(activity[,1])){
    activity[i,1] <- activity_labels[activity[i,1],2]
}

#creates a second, independent tidy data set with the average of each variable for each activity and each subject
subject_all <- rbind(subject_train,subject_test)
all <- cbind(subject_all,activity)
colnames(all)[1] <- "Subject"
tidy_data <- aggregate(all[,3] ~ Subject + Activity,data = all, FUN = "mean")
for(i in 4 : ncol(all)){
    tidy_data[ ,i] <- aggregate(all[,i] ~ Subject + Activity,data = all, FUN = "mean")[,3]
    colnames(tidy_data)[3: ncol(tidy_data)] <- colnames(X_all_meansd)
}
write.table(tidy_data, file = "Final_data.txt")

#test the Final_data
final <- read.table("Final_data.txt")
head(final)
