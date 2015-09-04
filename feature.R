# Taxi Trajectory, By: LapanDua

#####
# Depedency
#####

library(readr)
library(rjson)
library(Rcpp)

#####
# Script to Run
#####
# Remove all objects
rm(list=ls())

# Sampling train set
set.seed(0)
resample <- FALSE
train.sample.path <- "data/train_sample.csv"
if(resample){
  train.all  <- read_csv("data/train.csv")
  train.sample.size <- 100000
  train.sample.index <- sample(1:nrow(train.all), train.sample.size)
  train.sample <- train.all[train.sample.index,]
  rm(train.all)
  rm(train.sample.size)
  rm(train.sample.index)
  gc(TRUE)
  save(train.sample, file=train.sample.path)
}
load(train.sample.path)
rm(train.sample.path)
rm(resample)

# Process data type
train.sample$CALL_TYPE <- factor(train.sample$CALL_TYPE, levels=c("A","B","C"))
train.sample$DAY_TYPE <- factor(train.sample$DAY_TYPE, levels=c("A","B","C"))
train.sample$MISSING_DATA <- train.sample$MISSING_DATA == "True"

# Process trajectory
sourceCpp("cpp/pre_post_process.cpp")

positions <- function(row){
  as.data.frame(do.call(rbind, fromJSON(row["POLYLINE"])))  
}

last_position <- function(row){
  tail(positions(row), 1)  
} 

last_position_from_matrix <- function(pos){
  temp <- as.matrix(pos)
  return (temp[nrow(pos),])
}

first_position <- function(row){
  head(positions(row), 1)
}

coordinate_count <- function(pos) nrow(pos)

partial_position <- function(pos){
  boundary <- sample.int(nrow(pos),1)
  return (pos[1:boundary,])
}

is_not_outlier <- function(row){
  pos <- positions(row)
  
  #remove outlier
  new_pos <- outlierRemovedCoords(as.matrix(pos));
  n_pos <- coordinate_count(new_pos)
  
  if(n_pos < 5 || n_pos > 1000 || row["MISSING_DATA"] == TRUE){
    return (FALSE);
  } else {
    return (TRUE);
  }
}

# Trajectory start coords 
trajectory.start.points <- apply(train.sample, 1, first_position)
trajectory.start.points <- do.call("rbind", trajectory.start.points)

# Trajectory end coords
trajectory.end.points <- apply(train.sample, 1, last_position)
trajectory.end.points <- do.call("rbind", trajectory.end.points)

# Construct structured training data
train.sample.outlier.index <- apply(train.sample,1,is_not_outlier)
train.sample.clean <- train.sample[train.sample.outlier.index,]
rm(train.sample.outlier.index)

train.points.list <- apply(train.sample.clean, 1, function(row){
  pos <- positions(row)
  return(outlierRemovedCoords(as.matrix(pos)))
})
rm(train.sample.clean)
train.end.points <- lapply(train.points.list, last_position_from_matrix)
train.end.points <- do.call("rbind",train.end.points)
initializeClassMapping(as.matrix(train.end.points))
train.class <- apply(train.end.points,1,CoordinateToClass)
train.class <- as.data.frame(train.class)
names(train.class) <- c("class")
train.class$class <- factor(train.class$class, getClassLevel())
train.points.partial <- lapply(train.points.list, partial_position)
train.points.partial <- lapply(train.points.list, as.matrix)
rm(train.points.list)
train.feature.list <- lapply(train.points.partial, coordsToFeature)
rm(train.points.partial)
train.feature <- do.call("rbind",train.feature.list)
rm(train.feature.list)
train.feature <- as.data.frame(train.feature)
names(train.feature) <- getEdgesFeatureLabel()
train.feature <- cbind(train.feature, train.class)
rm(train.class)

# Construct structured test data
test  <- read_csv("data/test.csv")
test$CALL_TYPE <- factor(test$CALL_TYPE, levels=c("A","B","C"))
test$DAY_TYPE <- factor(test$DAY_TYPE, levels=c("A","B","C"))
test$MISSING_DATA <- test$MISSING_DATA == "True"
test.points.list <- apply(test, 1, function(row){
  pos <- positions(row)
  return(outlierRemovedCoords(as.matrix(pos)))
})
test.feature.list <- lapply(test.points.list, coordsToFeature)
rm(test.points.list)
test.feature <- do.call("rbind",test.feature.list)
rm(test.feature.list)
test.feature <- as.data.frame(test.feature)
names(test.feature) <- getEdgesFeatureLabel()