# Generates a randomization distribution for my data, plots the 
# distribution on a histogram, and performs a two-tailed test for the 
# p-value

setwd("C:/Users/dritc/Documents/R/STA13TermProject")
northData = read.csv("STA13ProjectData-North.csv", header=TRUE)
southData = read.csv("STA13ProjectData-South.csv", header=TRUE)

nDiffs = northData$Diff
sDiffs = southData$Diff

n = length(nDiffs)
totalDiffs = rep(NA, n*2)
for (i in 1:n) {
  totalDiffs[i] = nDiffs[i]
  totalDiffs[i+n] = sDiffs[i]
}


b = 10000
rand.dist = rep(NA, b)
for (i in 1:b) {
  rand.sample = sample(totalDiffs)
  rand.diffs1 = rand.sample[1:n]
  rand.diffs2 = rand.sample[(n+1):(n*2)]
  rand.dist[i] = mean(rand.diffs1) - mean(rand.diffs2)
}

hist(rand.dist, main="Randomization Distribution", xlab="Difference in Means")

sampleStat = mean(nDiffs) - mean(sDiffs)
moreExtremeCount = 0
for (i in 1:length(rand.dist)) {
  if (sampleStat > 0) {
    if (rand.dist[i] >= sampleStat) {
      moreExtremeCount = moreExtremeCount + 1
    }
  } else {
    if (rand.dist[i] <= sampleStat) {
      moreExtremeCount = moreExtremeCount + 1
    }
  }
}

pval = (moreExtremeCount/b) * 2
pval