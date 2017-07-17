######## Load data information
#Set up the working directory
setwd("F:/Data Science Specialization/data cleaning and exploratory/week4/project  Dataset/")

features=read.table("./UCI HAR Dataset/features.txt",as.is=T)
activity=read.table("./UCI HAR Dataset/activity_labels.txt",as.is=T)


#Load train dataset
x_train=read.table("./UCI HAR Dataset/train/x_train.txt",as.is=T)
y_train=read.table("./UCI HAR Dataset/train/y_train.txt",as.is=T)
subject_train=read.table("./UCI HAR Dataset/train/subject_train.txt",as.is=T)


body_acc_x_train=read.table("./UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt",as.is=T)
body_acc_y_train=read.table("./UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt",as.is=T)
body_acc_z_train=read.table("./UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt",as.is=T)
body_gyro_x_train=read.table("./UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt",as.is=T)
body_gyro_y_train=read.table("./UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt",as.is=T)
body_gyro_z_train=read.table("./UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt",as.is=T)
total_acc_x_train=read.table("./UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt",as.is=T)
total_acc_y_train=read.table("./UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt",as.is=T)
total_acc_z_train=read.table("./UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt",as.is=T)


#Load test dataset
x_test=read.table("./UCI HAR Dataset/test/x_test.txt",as.is=T)
y_test=read.table("./UCI HAR Dataset/test/y_test.txt",as.is=T)
subject_test=read.table("./UCI HAR Dataset/test/subject_test.txt",as.is=T)

body_acc_x_test=read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt",as.is=T)
body_acc_y_test=read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt",as.is=T)
body_acc_z_test=read.table("./UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt",as.is=T)
body_gyro_x_test=read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt",as.is=T)
body_gyro_y_test=read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt",as.is=T)
body_gyro_z_test=read.table("./UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt",as.is=T)
total_acc_x_test=read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt",as.is=T)
total_acc_y_test=read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt",as.is=T)
total_acc_z_test=read.table("./UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt",as.is=T)

################################################################
##select the required features and change the names for naming the colum names of measurments for activities.

dim(features)
#number of features
n_f=dim(features)[1]
head(features) 
names(features)=c("Feature_id","Feature_nm")
#Extracts only the measurements on the mean and standard deviation for each measurement
Mean=grepl("mean()",features$Feature_nm)
Stardard_deviation=grepl("std()",features$Feature_nm)
meanFreq=grepl("meanFreq()",features$Feature_nm)

features$Mean=Mean
features$Std=Stardard_deviation
features$meanFreq=meanFreq
  
#features$Feature_name=gsub("()","",features$Feature_nm)
#require(stringr)
#features$Feature_name=str_replace(features$Feature_nm, "()", " ")
#Above functions didn't work, we try another in package "stringi"
require(stringi)
features$Feature_name=stri_replace_all(features$Feature_nm, "", fixed="()")

Selected_features=features$Feature_id[(features$Mean==TRUE|features$Std==TRUE)&features$meanFreq==FALSE]

dim(activity); head(activity)
names(activity)=c("Activity_Label","Activity")

#########################################################################
###############     Clean train dataset   ########################

dim(subject_train)
unique(subject_train)
table(subject_train)
names(subject_train)="Subject_id"
#number of stujects in train data set
m_train=dim(unique(subject_train))[1]

dim(x_train); head(x_train) 
names(x_train)=features$Feature_name
n_train=dim(x_train)[1]

#Recall that we extracts only the measurements on the mean and standard deviation for each measurement
Selected_x_train= x_train[,Selected_features]
names(Selected_x_train)

dim(y_train); head(y_train); unique(y_train)
names(y_train)="Activity_Label"


Activity_nm_train=y_train
names(Activity_nm_train)="Activity"

for(i in 1:n_train){
  Activity_nm_train$Activity[i]=activity$Activity[y_train$Activity_Label[i]]
}

table(Activity_nm_train$Activity)

Activity_train=cbind(Activity_nm_train,Selected_x_train)



dim(body_acc_x_train) 
dim(body_acc_y_train)
dim(body_acc_z_train)
dim(body_gyro_x_train)
dim(body_gyro_y_train)
dim(body_gyro_z_train)
dim(total_acc_x_train)
dim(total_acc_y_train)
dim(total_acc_z_train)

#number of readings/window
n_w=dim(body_acc_x_train)[2] 
#train data size
n_train=dim(body_acc_x_train)[1]


for(i in 1:n_w){
  names(body_acc_x_train)[i]=paste0("body_acc_x_Window_",i)
  names(body_acc_y_train)[i]=paste0("body_acc_y_Window_",i)
  names(body_acc_z_train)[i]=paste0("body_acc_z_Window_",i)
  names(body_gyro_x_train)[i]=paste0("body_gyro_x_Window_",i)
  names(body_gyro_y_train)[i]=paste0("body_gyro_y_Window_",i)
  names(body_gyro_z_train)[i]=paste0("body_gyro_z_Window_",i)
  names(total_acc_x_train)[i]=paste0("total_acc_x_Window_",i)
  names(total_acc_y_train)[i]=paste0("total_acc_y_Window_",i)
  names(total_acc_z_train)[i]=paste0("total_acc_z_Window_",i)
}


#Create a Varable for labeling the subjects belonging to test or train sets
Group_Label=nrow(Activity_nm_train)
Group_Label="Train"

#Merge train data


#Complete data
#train_data=cbind(Group_Label,subject_train,Activity_train,
#                  body_acc_x_train,body_acc_y_train,body_acc_z_train,
#                  body_gyro_x_train,body_gyro_y_train,body_gyro_z_train,
#                  total_acc_x_train,total_acc_y_train,total_acc_z_train
#                  )

##We only need measurements on the mean and standard deviation for each measurement
#Don't need the detailed data in each readings/window

train_data=cbind(Group_Label,subject_train,Activity_train)
dim(train_data)
names(train_data)
head(train_data)

table(train_data$Activity)
#########################################################################
###############     Clean test dataset   ########################

dim(subject_test)
unique(subject_test)
table(subject_test)
names(subject_test)="Subject_id"

#number of stujects in test data set
m_test=dim(unique(subject_test))[1]

dim(x_test); head(x_test) 
names(x_test)=features$Feature_name
n_test=dim(x_test)[1]
#Recall that we extracts only the measurements on the mean and standard deviation for each measurement
Selected_x_test= x_test[,Selected_features]
names(Selected_x_test)

dim(y_test); head(y_test); unique(y_test)
names(y_test)="Activity_Label"

Activity_nm_test=y_test
names(Activity_nm_test)="Activity"

for(i in 1:n_test){
  Activity_nm_test$Activity[i]=activity$Activity[y_test$Activity_Label[i]]
}


Activity_test=cbind(Activity_nm_test,Selected_x_test)



dim(body_acc_x_test) 
dim(body_acc_y_test)
dim(body_acc_z_test)
dim(body_gyro_x_test)
dim(body_gyro_y_test)
dim(body_gyro_z_test)
dim(total_acc_x_test)
dim(total_acc_y_test)
dim(total_acc_z_test)
#number of readings/window
n_w=dim(body_acc_x_test)[2] 
#test data size
n_test=dim(body_acc_x_test)[1]


for(i in 1:n_w){
  names(body_acc_x_test)[i]=paste0("body_acc_x_Window_",i)
  names(body_acc_y_test)[i]=paste0("body_acc_y_Window_",i)
  names(body_acc_z_test)[i]=paste0("body_acc_z_Window_",i)
  names(body_gyro_x_test)[i]=paste0("body_gyro_x_Window_",i)
  names(body_gyro_y_test)[i]=paste0("body_gyro_y_Window_",i)
  names(body_gyro_z_test)[i]=paste0("body_gyro_z_Window_",i)
  names(total_acc_x_test)[i]=paste0("total_acc_x_Window_",i)
  names(total_acc_y_test)[i]=paste0("total_acc_y_Window_",i)
  names(total_acc_z_test)[i]=paste0("total_acc_z_Window_",i)
}


#Create a Varable for labeling the subjects belonging to test or train sets
Group_Label=nrow(Activity_nm_test)
Group_Label="test"

#Merge test data


#Complete data
#test_data=cbind(Group_Label,subject_test,Activity_test,
#                 body_acc_x_test,body_acc_y_test,body_acc_z_test,
#                 body_gyro_x_test,body_gyro_y_test,body_gyro_z_test,
#                 total_acc_x_test,total_acc_y_test,total_acc_z_test
#)

#We only need measurements on the mean and standard deviation for each measurement
#Don't need the detailed data in each readings/window

test_data=cbind(Group_Label,subject_test,Activity_test)
dim(test_data)

names(test_data)

table(test_data$Activity)
# Merge the train and test datasets into a comb dataset

Comb_data=rbind(train_data,test_data)
names(Comb_data)

Comb_data=data.frame(Comb_data)


#export in to txt file
write.table(Comb_data,"./SamSung data_merged tainning and test sets.txt",row.names = F)



############################################################################################################
## From the Comb_data above, creates a second, independent tidy data set with the average of each variable
## for each activity and each subject.
Comb_data$Group_Label=factor(Comb_data$Group_Label)
Comb_data$Subject_id=factor(Comb_data$Subject_id)
Comb_data$Activity=factor(Comb_data$Activity)

Tidy_data=aggregate(Comb_data[,!names(Comb_data)%in%c("Group_Label","Subject_id","Activity")],by=Comb_data[,c("Group_Label","Subject_id","Activity")],FUN=mean)
library(dplyr)
Tidy_data=arrange(Tidy_data,Group_Label,Subject_id,Activity)

write.table(Tidy_data,"./Tiday SamSung data_merged tainning and test sets.txt",row.names = F)

