## here we go. some house cleaning first
library(dplyr)
setwd("~/learning/coursera-datascience/course3/project")
features = read.table("features.txt")
activity_labels = read.table("activity_labels.txt")

## now read test data
setwd("~/learning/coursera-datascience/course3/project/test")
subject_test = read.table("subject_test.txt")
x_test = read.table("X_test.txt")
y_test = read.table("y_test.txt")

## now read training data
setwd("~/learning/coursera-datascience/course3/project/train")
subject_train = read.table("subject_train.txt")
x_train = read.table("X_train.txt")
y_train = read.table("Y_train.txt")

##now convert to data frames to make life easier
xtestdf <- as.data.frame(x_test)
ytestdf <- as.data.frame(y_test)
xtraindf <- as.data.frame(x_train)
ytraindf <- as.data.frame(y_train)
featuresdf <- as.data.frame(features)
labelsdf <- as.data.frame(activity_labels)


## now merge
xcompdf <- rbind(xtestdf, xtraindf)
ycompdf <- rbind(ytestdf, ytraindf)
df <- xcompdf
colnames(df) <- featuresdf$V2
df$y <- ycompdf$V1

##add subject data
subtestdf <- as.data.frame(subject_test)
subtraindf <- as.data.frame(subject_train)
subcompdf <- rbind(subtestdf, subtraindf)
df$Subject <- subcompdf$V1

## now get only mean and std dev data
mergeddf = merge(df,labelsdf, by.x="y", by.y = "V1")
mergedYcomp = merge(ycompdf,labelsdf, by.x="V1", by.y = "V1")
df$Labels <- mergedYcomp$V2
filterdf <- df[ , grepl( "mean|Mean|std|Std|Labels|Subject" , names( df ) ) ]

## now rearrange
ordered_df <- arrange(filterdf,Labels,Subject)

## now split by activity
od1 <- ordered_df[grep("LAYING",ordered_df$Labels),]
od2 <- ordered_df[grep("WALKING",ordered_df$Labels),]
od3 <- ordered_df[grep("WALKING_UPSTAIRS",ordered_df$Labels),]
od4 <- ordered_df[grep("WALKING_DOWNSTAIRS",ordered_df$Labels),]
od5 <- ordered_df[grep("SITTING",ordered_df$Labels),]
od6 <- ordered_df[grep("STANDING",ordered_df$Labels),]

## and split by subject
os1 <- split(od1,od1$Subject)
os2 <- split(od2,od2$Subject)
os3 <- split(od3,od3$Subject)
os4 <- split(od4,od4$Subject)
os5 <- split(od5,od5$Subject)
os6 <- split(od6,od6$Subject)

## now find the averages

tidy1 <- lapply(os1, function(x) colMeans(x[1:86]))
tidy2 <- lapply(os2, function(x) colMeans(x[1:86]))
tidy3 <- lapply(os3, function(x) colMeans(x[1:86]))
tidy4 <- lapply(os4, function(x) colMeans(x[1:86]))
tidy5 <- lapply(os5, function(x) colMeans(x[1:86]))
tidy6 <- lapply(os6, function(x) colMeans(x[1:86]))

##now recombine
joined <- c(tidy1, tidy2, tidy3, tidy4, tidy5, tidy6)
dfinal <- do.call("rbind", joined)

## remove duplicates
dfinalvals <- unique(dfinal)

##and save to disk
write.table(dfinalvals,"finalMeansByLabelAndSubject.txt",row.names = FALSE)














































