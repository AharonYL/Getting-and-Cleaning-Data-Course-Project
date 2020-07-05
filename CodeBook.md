Codebook
========

The goal today is to transform the Human Activity Recognition Using
Smartphones Data Set from the UCI Machine Learning Repository into a
more readable, more easily understandable, summarized format. We can
begin by noting the features and observations as they were displayed in
the original dataset, before describing the transformations each
variable underwent.

Feature Transformations
-----------------------

### Original Features

The original features of the dataset are as follows:

-   tBodyAcc-XYZ
-   tGravityAcc-XYZ
-   tBodyAccJerk-XYZ
-   tBodyGyro-XYZ
-   tBodyGyroJerk-XYZ
-   tBodyAccMag
-   tGravityAccMag
-   tBodyAccJerkMag
-   tBodyGyroMag
-   tBodyGyroJerkMag
-   fBodyAcc-XYZ
-   fBodyAccJerk-XYZ
-   fBodyGyro-XYZ
-   fBodyAccMag
-   fBodyAccJerkMag
-   fBodyGyroMag
-   fBodyGyroJerkMag

In each case, the 't' stands for time, the 'f' stands for frequency
domain signals, 'Body' refers to body movement, 'Gravity' refers to
gravitational movement, 'Acc' refers to acceleration, 'Jerk' refers, as
best I understand it, to jerking motions, and the 'Mag' refers to the
magnitude of each motion.

Further, each of the above features is given a wide variety of summary
variables to help explain it. Everything from mins and maxes to
autoregression coefficients are included. For the purpose of this
assignment, I'll be pulling out the average and standard deviation for
each feature, with the X, Y, and Z variables separated to make
alternative calculations easier for whoever uses this dataset.

### Transformed Features

Outside of simply subsetting the above variables so that only the
averages and standard deviations were included, I changed the variable
names in various ways to increase legibility. The new variable names are
as follows:

-   BodyAccel\_Time-XYZ
-   GravityAccel\_Time-XYZ
-   BodyAccel\_TimeJerk-XYZ
-   BodyGyro\_Time-XYZ
-   BodyGyro\_TimeJerk-XYZ
-   BodyAccel\_TimeMag
-   GravityAccel\_TimeMag
-   BodyAccel\_TimeJerkMag
-   BodyGyro\_TimeMag
-   BodyGyro\_TimeJerkMag
-   BodyAccel\_Freq-XYZ
-   BodyAcc\_FreqJerk-XYZ
-   BodyGyro\_Jerk-XYZ
-   BodyAccel\_FreqMag
-   BodyAccel\_FreqJerkMag
-   BodyGyro\_FreqMag
-   BodyGyro\_FreqJerkMag

As already mentioned, each of these features is, in the final dataset,
summarized via averages and standard deviations, written as "avg" and
"std" within the datasets variable names. The transformations shown
above increase clarity in two ways: first, by making explicit that "t"
is for "time" and "f" is for "frequency", and second by adding
underscores to more easily differentiate between the types of
measurement being described.

Transforming Selected Observations
----------------------------------

### Prevoius observations, need for improvement

Over the course of the project, several of the variable names needed to
be revised for clarity. I'm referring specifically to the activity names
which, in the original dataset, are stored as numbers in the following
way:

-   1 = WALKING
-   2 = WALKING\_UPSTAIRS
-   3 = WALKING\_DOWNSTAIRS
-   4 = SITTING
-   5 = STANDING
-   6 = LAYING

To better communicate these observations in the final, tidy dataset, a
new column with the acitvity names written explicitly was created to
replace the activity numbers of the original dataset.

Further, numerous calculations were made for each patient (labeled in
the "subject" field), which had to be averaged. This was accomplished
using the "group\_by" function, allowing, in the final dataset, for each
observation to be of a particular subject, within a particular activity

The Cleaning Process
--------------------

This section will explain how I achieved the transformations described
above, and created a clean, viable dataset for future use.

I began by pulling in the test and training datasets, along with the
other files necessary for complete analysis within each. Example code
for the test dataset is as follows:

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

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

I then pulled in the features dataset common to both training and
testing datasets with the following code:

    features <- read.table(file = "./UCI HAR Dataset/features.txt",
                           header = FALSE)

I then made some minor name changes to the original data to make
variable transformations easier later on, though this step seems
exceptionally optional.

    ##Renaming test and train data
    colnames(test_raw) <- features$V2
    colnames(train_raw) <- features$V2

    ##Renaming test and train labels
    y_test <- rename(y_test, activity = "V1")
    y_train <- rename(y_train, activity = "V1")

    ##Renaming subject label
    test_subject <- rename(test_subject, subject = "V1")
    train_subject<- rename(train_subject, subject = "V1")

Finally, I combined each of the datasets with both the feature name and
subject files.

    ##Combining test and labels and subjects
    test_data <- cbind(test_subject, y_test, test_raw)

    ##Combining train and labels and subjects
    train_data <- cbind(train_subject, y_train, train_raw)

I then combined the test and train datasets as follows:

    data <- rbind(test_data, train_data)

However, given that I needed only the average and standard deviation
measurements for each of the necessary variables, I simply pulled out
the required variables and placed them into a newdataset to simplify
later transformations

    ##Combing training and testing data
    data <- rbind(test_data, train_data)
    datatemp1 <- grep("mean\\()", names(data))
    datatemp2 <- grep("std", names(data))
    data2 <- data[,c(datatemp1, datatemp2)]

But can't forget the subject and activity columns! So I just bind those
onto the newdataset without a problem:

    data2 <- cbind(data[,1:2], data2)

Next come the observation name changes for activities. I took the long
way to do this but, using a character vector with the proper names would
work just as well.

    data2$activityname[data2$activity == 1] <- "walking"
    data2$activityname[data2$activity == 2] <- "walking_upstairs"
    data2$activityname[data2$activity == 3] <- "walking_downstairs"
    data2$activityname[data2$activity == 4] <- "sitting"
    data2$activityname[data2$activity == 5] <- "standing"
    data2$activityname[data2$activity == 6] <- "laying"

    data2 <- select(data2, -activity)

I then moved this new variable to the front of the dataset, removed the
old activity variable, and moved on to the final transformations.

First, I need to use group\_by on the subject column to more easily
average the results of each test for each subject and each exercise.

    data2 <- data2[, c(1, 68, 2:67)]

    data3 <- data2 %>%
          group_by(subject, activityname) %>%
          summarize_all("mean")

Then comes the long and annoying process of changing the feature names,
which I did as follows:

    newnames <- gsub("tBodyAcc", "BodyAccel_Time", names(data3))
    newnames <- gsub("tGravityAcc", "GravityAccel_Time", newnames)
    newnames <- gsub("tBodyGyro", "BodyGyro_Time", newnames)
    newnames <- gsub("fBodyAcc", "BodyAccel_Freq", newnames)
    newnames <- gsub("fBodyGyro", "BodyGyro_Freq", newnames)
    newnames <- gsub("fBodyBodyGyro", "BodyGyro_Freq", newnames)
    newnames <- gsub("fBodyBodyAcc", "BodyAccel_Freq", newnames)
    newnames <- gsub("-mean()", "_avg", newnames)
    newnames <- gsub("\\()", "", newnames)

    data4 <- data3 %>%
          rename_at(names(data3), ~ newnames)

And that's that! The final dataset contains separate columns for the
averaged results of each subject and each test. Took a little while, but
that's one tidy dataset.
