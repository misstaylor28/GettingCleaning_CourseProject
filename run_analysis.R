library(plyr)
library(dplyr)
library(stringr)
library(data.table)

features <- read.table("./features.txt",
                       col.names= c("number", "feature"), 
                       fill=FALSE, 
                       strip.white=TRUE)

activity_labels <- read.table("./activity_labels.txt",
                              col.names=c("activity_id","activity_name"),
                              fill=FALSE,
                              strip.white=TRUE)

X_train <- read.table("./train/X_train.txt", 
                      col.names = features[,2],
                      fill=FALSE,
                      strip.white=TRUE)

X_test <- read.table("./test/X_test.txt", 
                      col.names = features[,2],
                      fill=FALSE,
                      strip.white=TRUE)

y_train <- read.table("./train/y_train.txt", 
                      col.names = "label",
                      fill=FALSE,
                      strip.white=TRUE)

y_test <- read.table("./test/y_test.txt", 
                      col.names = "label",
                      fill=FALSE,
                      strip.white=TRUE)
sub_train <- read.table("./train/subject_train.txt",
                        col.names = "subject",
                        fill = FALSE,
                        strip.white = TRUE)

sub_test <- read.table("./test/subject_test.txt",
                        col.names = "subject",
                        fill = FALSE,
                        strip.white = TRUE)

## 1.  Merges the training and the test sets to create one data set
mergedData <- tbl_df(merge(X_train, X_test, all=TRUE))
labels <- rbind(y_train, y_test)
subjects <- rbind(sub_train, sub_test)
labeledData <- cbind(subjects, mergedData, labels)

## 2.  Extracts only the measurements on the mean and standard deviation for 
##     each measurement. 

meanMeasures <- grep("mean", features[,2])
stdMeasures <- grep("std", features[,2])
keep <- sort(append(meanMeasures,stdMeasures))
keep <- append(keep, length(labeledData))
subset <- select(labeledData, keep)

## 3.  Uses descriptive activity names to name the activities in the data set
revalue(as.character(subset$label), c("1" = "WALKING", "2" = "WALKING_UPSTAIRS",
                                      "3" = "WALKING_DOWNSTAIRS", "4" = "SITTING",
                                      "5" = "STANDING", "6" = "LAYING") )

## 4.  Appropriately labels the data set with descriptive variable names
subColNames <- names(subset)
# Remove non alpha-numeric characters
newColNames <- str_replace_all(subColNames, "[[:punct:]]"," ")
setnames(subset, old = subColNames, new = newColNames)

## 5.  From the data set in step 4, creates a second, independent tidy data set with the average of each 
##     variable for each activity and each subject.  
by_subject <- group_by(subset, subject, label) %>%
        summarise_each(funs(mean))

write.table(by_subject, file = "tidyData.txt", sep=",",row.names = FALSE)

