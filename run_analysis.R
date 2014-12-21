# Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now
# possible to collect a large amount of data about personal activity relatively
# inexpensively. These type of devices are part of the quantified self movement
# – a group of enthusiasts who take measurements about themselves regularly to
# improve their health, to find patterns in their behavior, or because they are
# tech geeks. One thing that people regularly do is quantify how much of a
# particular activity they do, but they rarely quantify how well they do it. In
# this project, your goal will be to use data from accelerometers on the belt,
# forearm, arm, and dumbell of 6 participants. They were asked to perform
# barbell lifts correctly and incorrectly in 5 different ways. More information
# is available from the website here: http://groupware.les.inf.puc-rio.br/har
# (see the section on the Weight Lifting Exercise Dataset). 

## Data 
# 
# The training data for this project are available here: 
#   https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
# The test data are available here: 
#   https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
# The data for this project come from this source:
#   http://groupware.les.inf.puc-rio.br/har. 
# If you use the document you create for this class for any purpose please cite
# them as they have been very generous in allowing their data to be used for
# this kind of assignment. 

library(caret)
library(randomForest)

# Download files
trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainFile <- "./data/pml-training.csv"
testFile  <- "./data/pml-testing.csv"

if (!file.exists("./data")) {
  dir.create("./data")
}
if (!file.exists(trainFile)) {
  download.file(trainUrl, destfile=trainFile, method="curl")
}
if (!file.exists(testFile)) {
  download.file(testUrl, destfile=testFile, method="curl")
}
# Read the files
data <- read.csv(trainFile, na.string=c("NA", ""))
check <- read.csv(testFile, na.string=c("NA", ""))

# list of not full columns
classe <- data$classe
data <- data[,names(data[,colSums(is.na(data))==0])]
data <- data[, sapply(data, is.numeric)]
remove <- grepl("^X|timestamp", names(data))
data <- data[, !remove]
pr_is <- check$problem_id
check <- check[, names(data)]
check$problem_id <- pr_is
data$classe <- classe

set.seed(19634)
#Split data
inTrain <- createDataPartition(data$classe, p = 0.75, list = F)
training <- data[inTrain,]
testing <- data[-inTrain,]

# Build model
ctrlRf <- trainControl(method = "oob")
modelRf <- train(classe ~ ., training, method = "rf", ntree = 200, trControl = ctrlRf)

# Check the error
fitRf <- predict(modelRf, testing)
confusionMatrix(testing$classe, fitRf)
accur <- postResample(fitRf, testing$classe)
accur

# Final predition
find <- which(names(check) == "problem_id")
finalRf = predict(modelRF, check[, -find])
finalRf

# Write the files to submission                                                                                                  number = 4), importance = TRUE)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(finalRf)

