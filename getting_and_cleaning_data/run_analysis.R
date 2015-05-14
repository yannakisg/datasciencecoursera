library(data.table)

getY <- function(filename) {
  y <- read.table(filename)
  
  y$V1[y$V1 == "1"] <- "WALKING"
  y$V1[y$V1 == "2"] <- "WALKING_UPSTAIRS"
  y$V1[y$V1 == "3"] <- "WALKING_DOWNSTAIRS"
  y$V1[y$V1 == "4"] <- "SITTING"
  y$V1[y$V1 == "5"] <- "STANDING"
  y$V1[y$V1 == "6"] <- "LAYING"
  
  colNames <- c ("Activity")
  colnames(y) <- colNames
  return (y)
}

getX <- function(filename) {
  remCol <- c(7:40, 47:80, 87:120, 127:160, 167:200, 203:213, 216:226, 229:239, 242:252, 255:265, 272:344, 351:423, 430:502, 505:515, 518:528, 531:541, 544:561)
  
  colNames <- c ( "BodyAccelerationMeanX", "BodyAccelerationMeanY",      "BodyAccelerationMeanZ", "BodyAccelerationStdX",
                  "BodyAccelerationStdY",  "BodyAccelerationStdZ",      "GravityAccelerationMeanX","GravityAccelerationMeanY",
                  "GravityAccelerationMeanZ",    "GravityAccelerationStdX",    "GravityAccelerationStdY",    "GravityAccelerationStdZ",
                  "BodyAccelerationJerkMeanX",    "BodyAccelerationJerkMeanY",    "BodyAccelerationJerkMeanZ",    "BodyAccelerationJerkStdX",
                  "BodyAccelerationJerkStdY",  "BodyAccelerationJerkStdZ",    "BodyGyroscopeMeanX",  "BodyGyroscopeMeanY",
                  "BodyGyroscopeMeanZ",    "BodyGyroscopeStdX",    "BodyGyroscopeStdY",    "BodyGyroscopeStdZ",
                  "BodyGyroscopeJerkMeanX",    "BodyGyroscopeJerkMeanY",    "BodyGyroscopeJerkMeanZ",    "BodyGyroscopeJerkStdX",
                  "BodyGyroscopeJerkStdY",    "BodyGyroscopeJerkStdZ",    "BodyAccelerationMagnitudeMean",    "BodyAccelerationMagnitudeStd",
                  "GravityAccelerationMagnitudeMean",    "GravityAccelerationMagnitudeStd",    "BodyAccelerationJerkMagnitudeMean",    "BodyAccelarationJerkMagnitudeStd",
                  "BodyGyroscopeMagnitudeMean",    "BodyGyroscopeMagnitudeStd",    "BodyGyroscopeJerkMagnituteMean",    "BodyGyroscopeJerkMagnituteStd",
                  "FFTBodyAccelerationMeanX",    "FFTBodyAccelerationMeanY",    "FFTBodyAccelerationMeanZ",    "FFTBodyAccelerationStdX",
                  "FFTBodyAccelerationStdY",    "FFTBodyAccelerationStdZ",    "FFTBodyAccelerationJerkMeanX",    "FFTBodyAccelerationJerkMeanY",
                  "FFTBodyAccelerationJerkMeanZ",    "FFTBodyAccelerationJerkStdX",    "FFTBodyAccelerationJerkStdY",    "FFTBodyAccelerationJerkStdZ",
                  "FFTBodyGyroscopeMeanX",    "FFTBodyGyroscopeMeanY",    "FFTBodyGyroscopeMeanZ",    "FFTBodyGyroscopeStdX",    "FFTBodyGyroscopeStdY",    "FFTBodyGyroscopeStdZ",
                  "FFTBodyAccelerationMagnitudeMean",    "FFTBodyAccelerationMagnitudeStd",    "FFTBodyBodyAccelerationJerkMagnitudeMean",    "FFTBodyBodyAccelerationJerkMagnitudeStd",
                  "FFTBodyBodyGyroscopeMagnitudeMean",    "FFTBodyBodyGyroscopeMagnitudeStd",    "FFTBodyBodyGyroscopeJerkMagnitudeMean",    "FFTBodyBodyGyroscopeJerkMagnitudeStd"   
  )
  
  x <- read.table(filename)
  set(x, j = remCol, value=NULL )
  
  colnames(x) <- colNames
  
  return (x)
}

runAnalysis <- function(folder) {
  
  fileTrainX <- "train/X_train.txt"
  fileTrainY <- "train/Y_train.txt"
  fileTestX <- "test/X_test.txt"
  fileTestY <- "test/Y_test.txt"
  
  trainX <- getX(paste(folder, fileTrainX, sep = "/"))
  trainY <- getY(paste(folder, fileTrainY, sep = "/"))
  testX <- getX(paste(folder, fileTestX, sep = "/"))
  testY <- getY(paste(folder, fileTestY, sep = "/"))
  
  
  resTrain <- cbind(trainY, trainX)
  
  resTest <- cbind(testY, testX)
  
  resTrain <- do.call("rbind",as.list(by(resTrain[,3:ncol(resTrain)], resTrain$Activity, colMeans)))
  resTrain <- cbind(rownames(resTrain), resTrain)
  colnames(resTrain)[1] <- "Activity"
  rownames(resTrain) <- NULL
  FileType <- rep("TRAIN", nrow(resTrain))
  resTrain <- cbind(FileType, resTrain)
  
  resTest <- do.call("rbind", as.list(by(resTest[,3:ncol(resTest)], resTest$Activity, colMeans)))
  resTest <- cbind(rownames(resTest), resTest)
  colnames(resTest)[1] <- "Activity"
  rownames(resTest) <- NULL
  FileType <- rep("TEST", nrow(resTest))
  resTest <- cbind(FileType, resTest)
  
  res <- rbind(resTrain, resTest)
  write.table(res, "tidy_data.txt", row.names =  FALSE)
  
  
}