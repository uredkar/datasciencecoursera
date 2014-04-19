library(reshape2)
library(data.table)

# this is where the files are stored
setwd("C:/Sources/cousera/getdata-002/Assignment1/UCI HAR Dataset")
features = read.table("features.txt",sep= "")
activities = read.table("activity_labels.txt",sep= "")

# load test
x_test = read.table("test/X_test.txt",sep= "")
y_test = read.table("test/y_test.txt",sep= "")
subject_test = read.table("test/subject_test.txt",sep= "")
# buld the test dataset
x_test2 = cbind(y_test,subject_test,x_test)
# load train
x_train = read.table("train/x_train.txt",sep= "")
y_train = read.table("train/y_train.txt",sep= "")
subject_train = read.table("train/subject_train.txt",sep= "")

# buld the train dataset
y_train2 = cbind(y_train,subject_train,x_train)

#combine test and train dataset
alldt <- rbind(x_test2, y_train2)

# add Activities and Subject columns to all the features
colnames = c("Activities","Subject",as.character(features$V2))

# assign column name to the dataframe/dataset
names(alldt) = colnames

# convert the activity id to label
activitylabel = sapply(alldt$Activities,FUN = function(x )  activities$V2[x])

#assign activity label to common dataset
alldt$Activities = activitylabel

# clear a function to filter columns
matchMeanAndStd = function (s)
{
  if (length(grep("*mean()*",s)) > 0)
  {
    return(TRUE)
  }
  if (length(grep("*std()*",s)) > 0)
  {
    return(TRUE)
  }
  if (length(grep("*Activities *",s)) > 0)
  {
    return(TRUE)
  }
  if (length(grep("*Subject *",s)) > 0)
  {
    return(TRUE)
  }
  
  return(FALSE)
}

#use the above function to get columns that match the filer
matched = lapply(colnames,matchMeanAndStd)

# create the first data set which has the required columns
meanvarDFFirst = alldt[,colnames[which(c(unlist(matched)))] ]

# convert the first dataset to a table 
meanvarSecond = as.data.table(meanvarDFFirst)

# do a group by Subject, Activities and get a mean i.e average for all columns
group = meanvarSecond[, lapply(.SD, mean), by = c("Subject","Activities")]

# save the tidy data table for export
write.table(group, file = "tidy.txt")

# check if saved correctly
tidy = read.table("tidy.txt")
