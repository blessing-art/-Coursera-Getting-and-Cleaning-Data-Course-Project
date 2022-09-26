library(dplyr)

filename <- "getuci_data.zip"
# checking if folder exists.
if (!file.exists(filename)) {
  fileURL <-  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"  
  
  download.file(fileURL, filename, method = "curl")
}

# checking if folder exists
if (!file.exists("UCI HAR Dataset")) {
  unzip(filename)
}

features <- read.table("UCI HAR Dataset/features.txt", 
                       col.names = c("n", "functions"))
activityLabel <- read.table("UCI HAR Dataset/activity_labels.txt",
                            col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", 
                           col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", 
                            col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", 
                      col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#quest1
## Merges the training and the test sets to create one data sets.

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)

Subjects <- rbind(subject_train, subject_test)
merge_data <- cbind(Subjects, Y, X)

#Quest2
#Extracts only the measurements on the mean and standard deviation 
#for each measurement.

Tidy <- merge_data %>% select(subject, code, 
                              contains("mean"), contains("std"))

#quest3
#Uses descriptive activity names to name the activities in the data set.
Tidy$code <- activityLabel[Tidy$code, 2]

#quest4
#Appropriately labels the data set with descriptive variable names.
names(Tidy)[2] = "activity"
names(Tidy)<-gsub("BodyBody", "Body", names(Tidy))
names(Tidy)<-gsub("Mag", "Magnitude", names(Tidy))
names(Tidy)<-gsub("^t", "Time", names(Tidy))
names(Tidy)<-gsub("^f", "Frequency", names(Tidy))
names(Tidy)<-gsub("tBody", "TimeBody", names(Tidy))
names(Tidy)<-gsub("-mean()", "Mean", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("-std()", "STD", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("-freq()", "Frequency", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("angle", "Angle", names(Tidy))

#Quest5
#From the data set in quest 4, creates a second, 
#independent tidy data set with the average of each variable
#for each activity and each subject.

FinalData <- Tidy %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)
