
library(data.table)
library(dplyr)
library(tidyr)

### Required Files are downloaded to Working directory and unzipped before starting
path <- file.path(getwd(), "UCI HAR Dataset") ## get path to Dataset
################ Merge the training and the test sets to create one data set######
###### Read all required files into R
#### merge rows according to file name
### rename column

# 1) Subject
train_subject <- read.table(file.path(path, "train", "subject_train.txt"))
test_subject <- read.table(file.path(path, "test", "subject_test.txt"))
merge_subject <- bind_rows(train_subject, test_subject)
names(merge_subject) <- "subject"  

# 2) Labels
train_label <- read.table(file.path(path, "train", "Y_train.txt"))
test_label <- read.table(file.path(path, "test", "y_test.txt"))
merge_label <- bind_rows(train_label, test_label)
names(merge_label) <- "activity" 


# 3) Set
train_set <- read.table(file.path(path, "train", "x_train.txt"))
test_set <- read.table(file.path(path, "test", "x_test.txt"))
merge_set <- bind_rows(train_set, test_set)


#### merge all column

merge_data <- bind_cols(merge_subject, merge_label, merge_set)

### order by subject and label
merge_data <- merge_data[order(merge_subject, merge_label),]
merge_data <- as.data.table(merge_data)
setkey(merge_data, subject, activity)
merge_data <- data.table(melt(merge_data, key(merge_data), variable.name="variable"))


#######Extracts only the measurements on the mean and standard deviation 
#####for each measurement

###### Read "features.text" file which contains List of all features.
##### create character vector and rename merge_data column names

features <- read.table(file.path(path, "features.txt"))
names(features) <- c("variable", "features") # Rename column
features$variable<- paste0("V", features$variable)
merge_data2 <- merge(merge_data, features, by = "variable") 

## subset mean and std
subset_data <- merge_data2[grepl("subject|label|mean\\(\\)|std\\(\\)", merge_data2$features),]


###########Use descriptive activity names to name the activities in the data set
###### Read "activity_labels.txt" file which contains decription for label.
activity_label <- read.table(file.path(path, "activity_labels.txt"))
names(activity_label) <- c("activity", "activitiName")
subset_data_description<- merge(subset_data, activity_label, by = "activity")



###############Appropriately labels the data set with descriptive variable names.
#### used to check features
###unique_features<- unique(subset_data_description$features)

#### get domain, instrument, acceleration, mean and std 
f<- function (A) {
  grepl(A, subset_data_description$features)
}

y <- matrix(1:2)
x <- matrix(c(f("^t"), f("^f")), ncol=nrow(y))
subset_data_description$domain <- factor(x %*% y, labels=c("Time", "Freq"))


x <- matrix(c(f("Acc"), f("Gyro")), ncol=nrow(y))
subset_data_description$instrument <- factor(x %*% y, labels=c("accelerometer", "gyroscope"))

x <- matrix(c(f("BodyAcc"), f("GravityAcc")), ncol=nrow(y))
subset_data_description$acceleration <- factor(x %*% y, labels=c(NA, "body", "gravity"))

x <- matrix(c(f("mean()"), f("std()")), ncol=nrow(y))
subset_data_description$variable <- factor(x %*% y, labels=c("mean", "std"))

## Jerk and magnitude
subset_data_description$jerk <- factor(f("Jerk"), labels=c(NA, "jerk"))
subset_data_description$magnitude <- factor(f("Mag"), labels=c(NA, "magnitude"))

## XYZ axis
y <- matrix(1:3, nrow=3)
x <- matrix(c(f("-X"), f("-Y"), f("-Z")), ncol=nrow(y))
subset_data_description$axis <- factor(x %*% y, labels=c(NA, "x", "y", "z"))

##### Rearrange collumn and remove unnecessary column
finaldt <- select(subset_data_description, subject, activitiName, instrument, acceleration, magnitude, jerk, variable, value, -activity, -features)


##ndependent tidy data set with the average of each variable for each activity and each subject.
tidydata <-
finaldt %>% 
  group_by_(.dots=c("subject", "activitiName", "instrument", "acceleration", "magnitude", "jerk", "variable")) %>% 
  summarize(average=mean(value))  
View(tidydata)
#### create txt file
write.table(tidydata, "cleaningDataProject.txt", row.names = FALSE)
