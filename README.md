run_analysis.R
#Merges the training and the test sets to create one data set

#load train data
x_train <- read.table("X_train.txt")
x_names <- read.table("features.txt")
x_names_vect <- as.vector(as.matrix(x_names[,2]))
names(x_train) <- x_names_vect
#load train activities
y_train <- read.table("Y_train.txt")
names(y_train) <- c("Activity")
#load train subjects
s_train <- read.table("subject_train.txt")
names(s_train) <- c("Subjects")
#load test data   
x_test <- read.table("X_test.txt")
names(x_test) <- x_names_vect
#load test activities
y_test <- read.table("Y_test.txt")
names(y_test) <- c("Activity")
#load test subjects
s_test <- read.table("subject_test.txt")
names(s_test) <- c("Subjects")

#combine train and test data  
x_set <- rbind(x_train, x_test)
y_set <- rbind(y_train, y_test)
s_set <- rbind(s_train, s_test)
train_test_data <- cbind(x_set,y_set,s_set)
#Extracts only the measurements on the mean and standard deviation for each measurement. 

#extract columns with mean() and std() in the name
ms_set <- regexpr("*mean\\(\\)*",x_names_vect)>0 | regexpr("*std\\(\\)*",x_names_vect)>0
mean_SD_set <- cbind(x_set[,ms_set],y_set,s_set)
#Uses descriptive activity names to name the activities in the data set

#obtain Activity names and create a factor variable 
y_factornames <- read.table("activity_labels.txt")
y_fact <- factor(as.vector(as.matrix(y_set[,1])),labels=y_factornames[,2])

#replace the y-variable data in the overall and the Means-and-SDs datasets with the new factor variable
alldata <- cbind(x_set,s_set,y_fact)
names(alldata)[length(names(alldata))] <- "Activity"

mean_ds_set <- cbind(x_set[,ms_set],s_set,y_fact)   
names(mean_ds_set)[length(names(mean_ds_set))] <- "Activity"
temp_data <- x_set[,ms_set]
original_n <- names(temp_data)
original_n <- gsub("\\)","",gsub("\\(","",original_n))
original_n <- strsplit(original_n,"-")
names_new = rep("colum name",length(original_n));

for (i in 1 : length(original_n)) {
  temp <- original_n[[i]][1]
  temp1 <- original_n[[i]][2]
  temp2 <- original_n[[i]][3]
  
  if (substr(temp,1,1)=="t") {
    var_name <- substr(temp, 2, nchar(temp))
    domain <-"(time domain)" 
  } else if (substr(temp,1,1)=="f") {
    var_name <- substr(temp, 2, nchar(temp))
    domain <-"(frequency domain)"  
  } else {
    var_name <- temp
    domain <- ""
  }
  
  if (!is.null(temp2) & !is.na(temp2)) {
    temp2 <- paste("along the", temp2, "axis",sep=" ")
  } else {
    temp2 <- ""
  }
  
  if (temp1=="mean") {
    temp1 <- "Mean of" 
  } else if (temp1 =="std") {
    temp1 <- "Standard Deviation of"
  }
  
  colname <- paste(temp1, var_name, temp2, domain, sep=" ")
  names_new[i] <- colname
}

#apply new names to the dataset
names(temp_data) <- names_new
mean_ds_set <- cbind(temp_data,s_set,y_fact)  
names(mean_ds_set)[length(names(mean_ds_set))] <- "Activity"
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

split_up <- split(x_set,list(alldata$Activity,alldata$Subjects))
s_x <- length(split_up)
n_x <- names(split_up)
s_y <- length(x_set)
n_y <- names(x_set)

avg_act_sub = matrix(rep(0,s_x*s_y),nrow=s_x,ncol=s_y)
rownames(avg_act_sub) <- n_x
colnames(avg_act_sub) <- n_y

for (i in 1:length(split_up)) {
  avg_act_sub[i,] <- as.vector(sapply(split_up[[i]],mean,rm.na=TRUE))
}


==============
