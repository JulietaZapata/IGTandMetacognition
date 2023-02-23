library(tidyverse)
library(lsr)
source('computeCI.R')
source('graphs.R')
par(bty='l', pty='s')

# load data
data   <- read.csv2('data.csv')
hpData <- read.csv('hp-binom.csv')
lpData <- read.csv('lp-binom.csv')

nsubj  <- max(data$nsubj)
nsubjh <- max(hpData$nsubj)
nsubjl <- max(lpData$nsubj)

# number of blocks (5 or 10)
nblocks <- 5
# n trials per block
ntrials <- 100/nblocks

#------- performance by blocks (all data) --------
perfBlocks <- matrix(NA, nsubj, nblocks) 

for(i in 1:nsubj){
  subj <- data[data$nsubj == i,]
  indx1 <- 1
  indx2 <- ntrials
  temp <- c()
  for(j in 1:nblocks){
    perfBlocks[i,j] <- mean(subj$adv_choice[indx1:indx2])
    indx1 <- indx1 + ntrials
    indx2 <- indx2 + ntrials
  }
  
}

ciPerf <- apply(perfBlocks, 2, CI)

makePlot(flag=T,perfBlocks, ciPerf, c('Performance', 'ALL DATA'),
         'springgreen3')


# t-tests --> when is performance above chance?
for(i in 1:nblocks){
  print(t.test(perfBlocks[,i], mu=0.5, alternative='g'))
  cat("Cohen's d:", cohensD(perfBlocks[,i], mu=0.5), '\n')
}


#------- performance by blocks (subjects with low perf) --------
perfBlocksLP <- matrix(NA, nsubjl, nblocks)

for(i in 1:nsubjl){
  subj <- lpData[lpData$nsubj == i,]
  indx1 <- 1
  indx2 <- ntrials
  for(j in 1:nblocks){
    perfBlocksLP[i,j] <- mean(subj$adv_choice[indx1:indx2])
    indx1 <- indx1 + ntrials
    indx2 <- indx2 + ntrials
  }
}

ciPerfLP <- apply(perfBlocksLP, 2, CI)

makePlot(flag=T, perfBlocksLP, ciPerfLP, 
         c('Performance','LOW PERFORMANCE SUBJECTS'),'springgreen3')

for(i in 1:nblocks){
  print(t.test(perfBlocksLP[,i], mu=0.5, alternative='g'))
  cat("Cohen's d:", cohensD(perfBlocksLP[,i], mu=0.5), '\n')
}


#------- performance by blocks (subjects with high perf) --------
perfBlocksHP <- matrix(NA, nsubjh, nblocks)

for(i in 1:nsubjh){
  subj <- hpData[hpData$nsubj == i,]
  indx1 <- 1
  indx2 <- ntrials
  for(j in 1:nblocks){
    perfBlocksHP[i,j] <- mean(subj$adv_choice[indx1:indx2])
    indx1 <- indx1 + ntrials
    indx2 <- indx2 + ntrials
  }
}

ciPerfHP <- apply(perfBlocksHP, 2, CI)

makePlot(flag=T, perfBlocksHP, ciPerfHP,
         c('Performance','HIGH PERFORMANCE SUBJECTS'),'springgreen3')

for(i in 1:nblocks){
  print(t.test(perfBlocksHP[,i], mu=0.5, alternative='g'))
  cat("Cohen's d:", cohensD(perfBlocksHP[,i], mu=0.5), '\n')
}
