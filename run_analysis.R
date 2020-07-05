library(dplyr)

###Pulling in test data
##Pull in test data
test_raw <- read.table(file = "./UCI HAR Dataset/test/X_test.txt",
                   header = FALSE)
##Pull in test data feature names
y_test <- read.table(file = "./UCI HAR Dataset/test/y_test.txt",
                   header = FALSE)
##Pull in test data subject identifier
test_subject <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt",
                       header = FALSE)

###Pulling in training data
##Pull in training data feature names
y_train <- read.table(file = "./UCI HAR Dataset/train/y_train.txt",
                      header = FALSE)
##Pull in training data
train_raw <- read.table(file = "./UCI HAR Dataset/train/X_train.txt",
                        header = FALSE)
##Pull in train data subject identifier
train_subject <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt",
                            header = FALSE)

##Pull in feature names overall
features <- read.table(file = "./UCI HAR Dataset/features.txt",
                       header = FALSE)

##Renaming test and train data
colnames(test_raw) <- features$V2
colnames(train_raw) <- features$V2

##Renaming test and train labels
y_test <- rename(y_test, activity = "V1")
y_train <- rename(y_train, activity = "V1")

##Renaming subject label
test_subject <- rename(test_subject, subject = "V1")
train_subject<- rename(train_subject, subject = "V1")

##Combining test and labels and subjects
test_data <- cbind(test_subject, y_test, test_raw)

##Combining train and labels and subjects
train_data <- cbind(train_subject, y_train, train_raw)

##Combing training and testing data
data <- rbind(test_data, train_data)
datatemp1 <- grep("mean\\()", names(data))
datatemp2 <- grep("std", names(data))
data2 <- data[,c(datatemp1, datatemp2)]

##Don't forget the subject and activity values!
data2 <- cbind(data[,1:2], data2)

##Renaming activities in new variable
data2$activityname[data2$activity == 1] <- "walking"
data2$activityname[data2$activity == 2] <- "walking_upstairs"
data2$activityname[data2$activity == 3] <- "walking_downstairs"
data2$activityname[data2$activity == 4] <- "sitting"
data2$activityname[data2$activity == 5] <- "standing"
data2$activityname[data2$activity == 6] <- "laying"

##Removing old activity column
data2 <- select(data2, -activity)

##Moving new activityname column to the front
###If all we want is a tidy version of the previous dataset, and not a new dataset entirely, this is where we would stop
data2 <- data2[, c(1, 68, 2:67)]

##grouping subject and activityname, averaging by the mean
data3 <- data2 %>%
      group_by(subject, activityname) %>%
      summarize_all("mean")

##Doing a whole heck of a lot of work to transform the names
newnames <- gsub("tBodyAcc", "BodyAccel_Time", names(data3))
newnames <- gsub("tGravityAcc", "GravityAccel_Time", newnames)
newnames <- gsub("tBodyGyro", "BodyGyro_Time", newnames)
newnames <- gsub("fBodyAcc", "BodyAccel_Freq", newnames)
newnames <- gsub("fBodyGyro", "BodyGyro_Freq", newnames)
newnames <- gsub("fBodyBodyGyro", "BodyGyro_Freq", newnames)
newnames <- gsub("fBodyBodyAcc", "BodyAccel_Freq", newnames)
newnames <- gsub("-mean()", "_avg", newnames)
newnames <- gsub("\\()", "", newnames)

##Ending up with data4, which is the final version of the transformed dataset, taking averages for all the relevant variabels
data4 <- data3 %>%
      rename_at(names(data3), ~ newnames)

