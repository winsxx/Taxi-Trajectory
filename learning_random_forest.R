# Taxi Trajectory, By LapanDua

#####
# Depedency
#####
source("feature.R")
source("evaluation_script.R")
library(randomForest)

#####
# Script to Run
#####
getMostImportantFeature <- function(rfObject, numOfFeatures){
  imp <- importance(rfObject)
  imp <- data.frame(imp)
  imp <- imp[order(-imp$MeanDecreaseGini),,drop=F]
  return(row.names(imp)[1:numOfFeatures])
}

set.seed(0)

trees.amount <- 200

class.index <- ncol(train.feature)
rf <- randomForest(x = train.feature[,c(!train.feature.var0,FALSE)],
                   y = droplevels(train.feature[,class.index]),
                   ntree = trees.amount,
                   nodesize = 3,
                   do.trace = T)

prediction <- predict(rf, test.feature[,!train.feature.var0])

outside.grid.index <- is_outside_grid(prediction)
predicted.coords.list <-lapply(as.character(prediction), classToCoordinate)
predicted.coords <- do.call("rbind",predicted.coords.list)
predicted.coords <- as.data.frame(predicted.coords)

options(digits=9)
submission <- cbind(TRIP_ID = test[,1],
                    LATITUDE = predicted.coords[,2],
                    LONGITUDE = predicted.coords[,1])
submission <- as.data.frame(submission)
submission$LATITUDE <- format(round(as.numeric(as.character(submission$LATITUDE)),6), nsmall=6)
submission$LONGITUDE <- format(round(as.numeric(as.character(submission$LONGITUDE)),6), nsmall=6)
submission[outside.grid.index,2] <- format(round(last.location.benchmark[outside.grid.index,2],6), nsmall=6)
submission[outside.grid.index,3] <- format(round(last.location.benchmark[outside.grid.index,1],6), nsmall=6)

write_csv(submission, "output/submission.csv") 

if(TEST_OWN){
  destinationMining.Evaluation("output/submission.csv", "output/test_own.csv")
}
