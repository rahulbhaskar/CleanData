run_analysis<-function()
{
  #Read the source files into R
  xtest<-read.table("x_test.txt")
  ytest<-read.table("y_test.txt")
  subjecttest<-read.table("subject_test.txt")
  xtrain<-read.table("x_train.txt")
  ytrain<-read.table("y_train.txt")
  subjecttrain<-read.table("subject_train.txt")
  
  #Tie the observations with Activies and Subjects to prepare for merge.
  
  train<-cbind(xtrain,ytrain,subjecttrain)
  test<-cbind(xtest,ytest,subjecttest)
  
  #Combine the Training and Test Data
  combined<-rbind(test,train)
  
  #Extract only the Mean and Standard Deviation Data.
  #The subject and activity are pulled to the beginning for readability
  extract<-combined[,c(563,562,1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,121,122,123,124,125,126,161,162,163,164,165,166,201,202,214,215,227,228,240,241,253,254,266,267,268,269,270,271,294,295,296,345,346,347,348,349,350,373,374,375,424,425,426,427,428,429,452,453,454,503,504,513,516,517,526,529,530,539,542,543,552)]
  
  #Appropriate values inserted for activities (Column 2)
  
  for(i in 1:nrow(extract))
  {
    if(extract[i,2]==1)
    {
      extract[i,2]<-"WALKING"
    }
    else if(extract[i,2]==2)
    {
      extract[i,2]="WALKING_UPSTAIRS"
    }
    else if(extract[i,2]==3)
    {
      extract[i,2]="WALKING_DOWNSTAIRS"
    }
    else if(extract[i,2]==4)
    {
      extract[i,2]="SITTING"
    }
    else if(extract[i,2]==5)
    {
      extract[i,2]="STANDING"
    }
    else if(extract[i,2]==6)
    {
      extract[i,2]="LAYING"
    }
    
  }
  
  ##Appropriate labelling done for the field names of the extract
  colnames(extract)<-c("Subject","Activity","BodyAccMeanX",
                          "BodyAccMeanY","BodyAccMeanZ","BodyAccStdX","BodyAccStdY",
                          "BodyAccStdZ","GravityAccMeanX","GravityAccMeanY",
                          "GravityAccMeanZ","GravityAccStdX","GravityAccStdY",
                          "GravityAccStdZ","tBodyAccJerkMeanX","tBodyAccJerkMeanY",
                          "tBodyAccJerkMeanZ","tBodyAccJerkStdX","tBodyAccJerkStdY",
                          "tBodyAccJerkStdZ","BodyGyroMeanX","BodyGyroMeanY",
                          "BodyGyroMeanZ","BodyGyroStdX","BodyGyroStdY",
                          "BodyGyroStdZ","BodyGyroJerkMeanX","BodyGyroJerkMeanY",
                          "BodyGyroJerkMeanZ","BodyGyroJerkStdX","BodyGyroJerkStdY",
                          "BodyGyroJerkStdZ","BodyAccMagMean","BodyAccMagStd",
                          "GravityAccMagMean","GravityAccMagStd","BodyAccJerkMagMean",
                          "BodyAccJerkMagStd","BodyGyroMagMean","BodyGyroMagStd",
                          "BodyGyroJerkMagMean","BodyGyroJerkMagStd","fBodyAccMeanX",
                          "fBodyAccMeanY","fBodyAccMeanZ","fBodyAccStdX",
                          "fBodyAccStdY","fBodyAccStdZ","fBodyAccMeanFreqX",
                          "fBodyAccMeanFreqY","fBodyAccMeanFreqZ","fBodyAccJerkMeanX",
                          "fBodyAccJerkMeanY","fBodyAccJerkMeanZ","fBodyAccJerkStdX",
                          "fBodyAccJerkStdY","fBodyAccJerkStdZ",
                          "fBodyAccJerkMeanFreqX","fBodyAccJerkMeanFreqY",
                          "fBodyAccJerkMeanFreqZ","fBodyGyroMeanX","fBodyGyroMeanY",
                          "fBodyGyroMeanZ","fBodyGyroStdX","fBodyGyroStdY",
                          "fBodyGyroStdZ","fBodyGyroMeanFreqX","fBodyGyroMeanFreqY",
                          "fBodyGyroMeanFreqZ","fBodyAccMagMean","fBodyAccMagStd",
                          "fBodyAccMagMeanFreq","fBodyBodyAccJerkMagMean",
                          "fBodyBodyAccJerkMagStd","fBodyBodyAccJerkMagMeanFreq",
                          "fBodyBodyGyroMagMean","fBodyBodyGyroMagStd",
                          "fBodyBodyGyroMagMeanFreq","fBodyBodyGyroJerkMagMean",
                          "fBodyBodyGyroJerkMagStd","fBodyBodyGyroJerkMagMeanFreq")

 #Calculate means for each of the columns and merge data
  
  for(i in 3:ncol(extract))
  {
    if(i==3)
    {
      finaldata1<-aggregate(extract[,i]~Subject+Activity,data=extract,mean)
    }
    else
    {
      finaldata2<-aggregate(extract[,i]~Subject+Activity,data=extract,mean)
      finaldata1<-merge(finaldata1,finaldata2,by=c("Subject","Activity"))
    }
  }
 
 #Reassgining the variable names to the data
  colnames(finaldata1)<-c("Subject","Activity","BodyAccMeanX",
                       "BodyAccMeanY","BodyAccMeanZ","BodyAccStdX","BodyAccStdY",
                       "BodyAccStdZ","GravityAccMeanX","GravityAccMeanY",
                       "GravityAccMeanZ","GravityAccStdX","GravityAccStdY",
                       "GravityAccStdZ","tBodyAccJerkMeanX","tBodyAccJerkMeanY",
                       "tBodyAccJerkMeanZ","tBodyAccJerkStdX","tBodyAccJerkStdY",
                       "tBodyAccJerkStdZ","BodyGyroMeanX","BodyGyroMeanY",
                       "BodyGyroMeanZ","BodyGyroStdX","BodyGyroStdY",
                       "BodyGyroStdZ","BodyGyroJerkMeanX","BodyGyroJerkMeanY",
                       "BodyGyroJerkMeanZ","BodyGyroJerkStdX","BodyGyroJerkStdY",
                       "BodyGyroJerkStdZ","BodyAccMagMean","BodyAccMagStd",
                       "GravityAccMagMean","GravityAccMagStd","BodyAccJerkMagMean",
                       "BodyAccJerkMagStd","BodyGyroMagMean","BodyGyroMagStd",
                       "BodyGyroJerkMagMean","BodyGyroJerkMagStd","fBodyAccMeanX",
                       "fBodyAccMeanY","fBodyAccMeanZ","fBodyAccStdX",
                       "fBodyAccStdY","fBodyAccStdZ","fBodyAccMeanFreqX",
                       "fBodyAccMeanFreqY","fBodyAccMeanFreqZ","fBodyAccJerkMeanX",
                       "fBodyAccJerkMeanY","fBodyAccJerkMeanZ","fBodyAccJerkStdX",
                       "fBodyAccJerkStdY","fBodyAccJerkStdZ",
                       "fBodyAccJerkMeanFreqX","fBodyAccJerkMeanFreqY",
                       "fBodyAccJerkMeanFreqZ","fBodyGyroMeanX","fBodyGyroMeanY",
                       "fBodyGyroMeanZ","fBodyGyroStdX","fBodyGyroStdY",
                       "fBodyGyroStdZ","fBodyGyroMeanFreqX","fBodyGyroMeanFreqY",
                       "fBodyGyroMeanFreqZ","fBodyAccMagMean","fBodyAccMagStd",
                       "fBodyAccMagMeanFreq","fBodyBodyAccJerkMagMean",
                       "fBodyBodyAccJerkMagStd","fBodyBodyAccJerkMagMeanFreq",
                       "fBodyBodyGyroMagMean","fBodyBodyGyroMagStd",
                       "fBodyBodyGyroMagMeanFreq","fBodyBodyGyroJerkMagMean",
                       "fBodyBodyGyroJerkMagStd","fBodyBodyGyroJerkMagMeanFreq")
 
 #Output written to the file 
 write.table(finaldata1,"Course_Assignment.txt",row.names=FALSE)
  
}