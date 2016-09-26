library(dplyr)
library(data.table)

# # common tasks
# Working directory should be set upfront

# Read features file to get variable names for the observations files
columnnamesfile <-
  read.csv(
    "./UCI HAR Dataset/features.txt",
    header = FALSE,
    sep = " ",
    col.names = c("idx", "vars")
  )
columnnames <-
  as.character(paste0(columnnamesfile$idx, columnnamesfile$vars))
# create a list of variables without special characters
cn <- lapply(columnnames, function (x) {
  gsub("[-|(|)]|,", "", x)
})
# Get a list of "mean" and "StD" variables as valid ones
validvars <- intersect(grep("^[^angle]", cn),
                       setdiff(grep("([Mm]ean|[Ss]td)", cn),
                               grep(".Freq.*", cn)))

# read the labels file for activities so it will be possible to merge it with the main dataset
activitylabelsfile <-
  read.csv(
    "./UCI HAR Dataset/activity_labels.txt",
    header = FALSE,
    sep = " ",
    col.names = c("activity_id", "activity_desc"),
    stringsAsFactors = FALSE
  )


# Process train data
# Read train file as table considering only valid variables
trainfile <-
  read.table("./UCI HAR Dataset/train/X_train.txt")[validvars]
names(trainfile) <- cn[validvars]

# Read subjects file for train dataset
subjectsfile <-
  read.csv("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)

# Add subjects column to dataset
trainfile$subjects <- subjectsfile$V1

# Read activities file for train dataset
activitiesfile <-
  read.csv("./UCI HAR Dataset/train/y_train.txt", header = FALSE)

# Add activity id column to the dataset
trainfile$activities <- activitiesfile$V1

# Process test data
# Read test file as table considering only valid variables
testfile <-
  read.table("./UCI HAR Dataset/test/X_test.txt")[validvars]
names(testfile) <- cn[validvars]

# Read subjects file for test dataset
subjectsfile <-
  read.csv("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)

# Add subjects column to dataset
testfile$subjects <- subjectsfile$V1

# Read activities file for test dataset
activitiesfile <-
  read.csv("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

# Add activity id column to the dataset
testfile$activities <- activitiesfile$V1

# Merge both datasets
mergedtraintestfiles <- rbind(testfile, trainfile)

# merge with activitiy_labels file to get activities description instead if code
mergedUCI <- merge(
  mergedtraintestfiles,
  activitylabelsfile,
  by.x = "activities",
  by.y = "activity_id",
  all.x = TRUE
)

# Create a tidy dataset with the mean of every variable, by activity and subject
tidyDS <-
  aggregate(
    mergedUCI[, 2:74],
    by = list(
      activities = mergedUCI$activity_desc,
      subjects = mergedUCI$subjects
    ),
    FUN = mean,
    na.action = na.omit
  )

write.table(tidyDS, './tidyDS.txt', sep = ',')
