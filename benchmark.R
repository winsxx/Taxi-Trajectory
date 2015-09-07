library(readr)
library(rjson)

submission <- test["TRIP_ID"]

submission[, "LATITUDE"] <- last.location.benchmark[,2]
submission[, "LONGITUDE"] <- last.location.benchmark[,1]

write_csv(submission, "output/last_location_benchmark.csv")

destinationMining.Evaluation("output/last_location_benchmark.csv", "output/test_own.csv")
