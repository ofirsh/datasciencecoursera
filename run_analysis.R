## source("run_analysis.R")
## run_analysis()

run_analysis <- function() 
{
  ## Reading test and train data sets  
  
  ## Test
  test <- read.table("X_test.txt",header=FALSE)
  subject.test <- read.table("subject_test.txt",header=FALSE)[[1]]
  activity.test <- read.table("y_test.txt",header=FALSE)[[1]]
  
  ## Train
  train <- read.table("X_train.txt",header=FALSE)
  subject.train <- read.table("subject_train.txt",header=FALSE)[[1]]
  activity.train <- read.table("y_train.txt",header=FALSE)[[1]]
  
  ## Activity labels
  activity.labels <- read.table("activity_labels.txt",header=FALSE,stringsAsFactors=FALSE)[[2]]
  
  ## Merges the training and the test sets to create one data set
  merged <- rbind(train,test)
  subject.merged  <- c(subject.train,subject.test) 
  activity.merged <- c(activity.train,activity.test)
  
  ## Transform the activity code to readable formats (WALKING,LAYING etc)
  readable.activity <- function(x) { activity.labels[x] }
  activity.merged.readable = sapply(activity.merged,readable.activity, simplify=TRUE)
  

  ## Identify the measurements on the mean and standard deviation using regular expression
  col.names <- as.list(read.table("./project/features.txt",stringsAsFactors=FALSE,header=FALSE)[2])[[1]]
  col.index.std.mean <- grep("std()|mean()",col.names)
  
  ## Extracts only the measurements on the mean and standard deviation for each measurement.
  merged.std.mean <- merged[,col.index.std.mean]
  col.names.std.mean <- col.names[col.index.std.mean]
  
  ## Appropriately labels the data set with descriptive variable names
  col.names.std.mean.clean <- tolower(gsub("[[:punct:]]", "", col.names.std.mean))
  
  names(merged.std.mean) <- col.names.std.mean.clean
  
  aggdata <-aggregate(merged.std.mean, by=list(activity.merged.readable,subject.merged),FUN=mean, na.rm=TRUE)
  
  names(aggdata)[1] <- "activity"
  names(aggdata)[2] <- "subject"
  
  write.table(aggdata,file="tidyset.txt",sep = " ",col.names = TRUE)
}