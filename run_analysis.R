
#data from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#download the data and extract it

if (!file.exists("dataset.zip")){
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "dataset.zip")
}

data_path <- unzip("dataset.zip")
# some preparation for the lable and name of columns
activity_label <- readLines(unz("C:/Users/iowner/Downloads/coursera/Getting and Cleaning Data/project/dataset.zip",
                                filename = "UCI HAR Dataset/activity_labels.txt"))

activity_label <- gsub("^[0-9]\\s*", "", activity_label)

features <- read.table(unz("C:/Users/iowner/Downloads/coursera/Getting and Cleaning Data/project/dataset.zip",
                           filename = "UCI HAR Dataset/features.txt"))

#Handle with test data
label_test <- read.table(unz("C:/Users/iowner/Downloads/coursera/Getting and Cleaning Data/project/dataset.zip",
                             filename = "UCI HAR Dataset/test/y_test.txt"))

data_test <- read.table(unz("C:/Users/iowner/Downloads/coursera/Getting and Cleaning Data/project/dataset.zip",
                            filename = "UCI HAR Dataset/test/X_test.txt"))

subject_test <- read.table(unz("C:/Users/iowner/Downloads/coursera/Getting and Cleaning Data/project/dataset.zip",
                               filename = "UCI HAR Dataset/test/subject_test.txt"))

label_test$V1 <- factor(label_test$V1, labels = activity_label)
names(label_test) <- "activity"

names(subject_test) <- "subject"

features_name <- features$V2 

names(data_test) <- features_name

data_test <- cbind(subject_test,label_test,data_test)

#Handle with train data
label_train <- read.table(unz("C:/Users/iowner/Downloads/coursera/Getting and Cleaning Data/project/dataset.zip",
                             filename = "UCI HAR Dataset/train/y_train.txt"))

data_train <- read.table(unz("C:/Users/iowner/Downloads/coursera/Getting and Cleaning Data/project/dataset.zip",
                            filename = "UCI HAR Dataset/train/X_train.txt"))

subject_train <- read.table(unz("C:/Users/iowner/Downloads/coursera/Getting and Cleaning Data/project/dataset.zip",
                               filename = "UCI HAR Dataset/train/subject_train.txt"))

label_train$V1 <- factor(label_train$V1, labels = activity_label)
names(label_train) <- "activity"

names(subject_train) <- "subject"

names(data_train) <- features_name

data_train <- cbind(subject_train,label_train,data_train)

#merge data
df <- rbind(data_test, data_train)

#Extracts only the measurements on the mean and standard deviation
# column name include mean() and std() at the end
col_true <- sapply(names(df), function(x) 
  ifelse((grepl("mean",x) | grepl("std",x)) & !(grepl("meanFreq", x)),TRUE,FALSE))
col_true[1] <- TRUE
col_true[2] <- TRUE

#subset the data for columns
df <- df[, col_true]

#tidy data (created with write.table() using row.name=FALSE)
#independent tidy data set with the average of each variable for each activity and each subject.
temp <- sapply(df[3:ncol(df)],function(x)
  aggregate(x, by = list(df$subject,df$activity), FUN = mean, na.rm = TRUE))
seq_sub <- seq(from = 3, to = length(temp), by = 3)
temp <- temp[c(1,2,seq_sub)]

tidy_data <- as.data.frame(temp) 
names(tidy_data) <- names(df)


write.table(tidy_data, file = "tidydata.txt", row.names = FALSE)




