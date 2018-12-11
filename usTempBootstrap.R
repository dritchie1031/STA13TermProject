# Generates a bootstrap distribution for the data from PRISM and computes a 
# confidence interval for that distribution.

getSummaryStats = function(group) {
  cat("Mean: ", mean(group), "\n")
  cat("Standard Dev: ", sd(group), "\n")
  cat("Sample Size: ", length(group), "\n")
  print('Five number summary: ')
  summary(group)
}


setwd("C:/Users/dritc/Documents/R/STA13TermProject")
northData = read.csv("STA13ProjectData-North.csv", header=TRUE)
southData = read.csv("STA13ProjectData-South.csv", header=TRUE)

nDiffs = northData$Diff
sDiffs = southData$Diff
getSummaryStats(nDiffs)
getSummaryStats(sDiffs)

regions = c("North", "South")
boxplot(nDiffs, sDiffs, names=regions,ylab="Difference in Mean Temp",
        main="Climate Change in North vs. South U.S.")

b = 10000
boot.dist = rep(NA, b)

for (i in 1:b) {
  boot.northSample = sample(length(nDiffs),replace=TRUE)
  boot.southSample = sample(length(sDiffs),replace=TRUE)
  boot.dist[i] = mean(nDiffs[boot.northSample]) - mean(sDiffs[boot.southSample])
}

hist(boot.dist, main="Bootstrap Histogram", xlab="Difference in Means")

confInterval = quantile(boot.dist, c(0.025, 0.975))
stderror = sd(boot.dist)
boot.mean = mean(boot.dist)

confInterval
stderror
boot.mean