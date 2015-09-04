# Taxi Trajectory, By: LapanDua

#####
# Depedency
#####

source("feature.R")
library(ggplot2)
library(ggmap)

#####
# Script to Run
#####

# General
dim(train.sample)
names(train.sample)
str(train.sample)
head(train.sample)
summary(train.sample)

png("output/calltype_piechart.png")
pie(table(train.sample$CALL_TYPE), main="Call Type")
legend("bottomleft", 
       c("A: From central","B: Spesific stand","C: Random street"), 
       pch = 1)
dev.off()
table(train.sample$DAY_TYPE)
table(train.sample$MISSING_DATA)

# Trajectory
png("output/trajectory_boxplot_longitude.png")
boxplot(data.frame(longitude.start=trajectory.start.points[,1], longitude.end=trajectory.end.points[,1]),
        ylim=c(-8.9,-8.0),
        main="Start and End Trajectory Longitude",
        ylab="longitude (degree)")
dev.off()
summary(data.frame(longitude.start=trajectory.start.points[,1], longitude.end=trajectory.end.points[,1]))

png("output/trajectory_boxplot_latitude.png")
boxplot(data.frame(latitude.start=trajectory.start.points[,2], latitude.end=trajectory.end.points[,2]),
        ylim=c(40.8,41.8),
        main="Start and End Trajecotry Latitude",
        ylab="latitude (degree)")
dev.off()
summary(data.frame(latitude.start=trajectory.start.points[,2], latitude.end=trajectory.end.points[,2]))

trajectory.points.all <- cbind(trajectory.start.points, trajectory.end.points)
names(trajectory.points.all) <- c("trajectory.start.longitude", "trajectory.start.latitude","trajectory.end.longitude","trajectory.end.latitude")
summary(trajectory.points.all)
rm(trajectory.points.all)

# Drawing map
set.seed(0)
map.points.index <- sample(1:nrow(trajectory.start.points), 1000)
map.points.start <- trajectory.start.points[map.points.index,]
map.points.end <- trajectory.end.points[map.points.index,]

map <- get_map(location = 'Porto', zoom = 12)
map.detail <- get_map(location = 'Porto', zoom = 13)
# Wide
png("output/map_wide_start.png")
ggmap(map) +
  geom_point(aes(x=map.points.start[,1],
               y=map.points.start[,2]),
             alpha=0.3,
             color="blue",
             fill="blue",
             size=2,
             shape=21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) + 
  ggtitle('Porto Trajectory Start Points') +
  theme(title=element_text(size=14,face="bold"))
dev.off()

png("output/map_wide_end.png")
ggmap(map) +
  geom_point(aes(x=map.points.end[,1],
                 y=map.points.end[,2]),
             alpha=0.3,
             color="red",
             fill="red",
             size=2,
             shape=21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) + 
  ggtitle('Porto Trajectory End Points') +
  theme(title=element_text(size=14,face="bold"))
dev.off()

# Detail
png("output/map_narrow_start.png")
ggmap(map.detail) +
  geom_point(aes(x=map.points.start[,1],
                 y=map.points.start[,2]),
             alpha=0.3,
             color="blue",
             fill="blue",
             size=3,
             shape=21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) + 
  ggtitle('Porto Trajectory Start Points') +
  theme(title=element_text(size=14,face="bold"))
dev.off()

png("output/map_narrow_end.png")
ggmap(map.detail) +
  geom_point(aes(x=map.points.end[,1],
                 y=map.points.end[,2]),
             alpha=0.3,
             color="red",
             fill="red",
             size=3,
             shape=21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) + 
  ggtitle('Porto Trajectory End Points') +
  theme(title=element_text(size=14,face="bold"))
dev.off()

rm(map.points.start)
rm(map.points.end)
rm(map.points.index)
