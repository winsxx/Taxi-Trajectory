# Taxi Trajectory, By LapanDua

#####
# Depedency
#####
source(feature.R)
library(e1071)

#####
# Script to Run
#####
set.seed(0)

class.index <- ncol(train.feature)
model <- naiveBayes(x=train.feature[,-class.index], 
                    y=as.factor(train.feature[,class.index]),
                    laplace=1.5)

prediction <- predict(model, test.feature)

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

