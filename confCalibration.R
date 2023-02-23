library(scales)
library(tidyverse)
library(lsr)
library('ggpubr')

hp <- read.csv('hp-binom.csv')
lp <- read.csv('lp-binom.csv')

# confidence interval function
CI <- function(x, ci=0.95){
  
  interval <- qt(ci + (1 - ci)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
  
  df <- data.frame(y = mean(x), ymin = mean(x)-interval, ymax = mean(x)+interval)
  
  return(df)
  
}


# get mean level of confidence
lpConf <- rep(NA, max(lp$nsubj))
hpConf <- rep(NA, max(hp$nsubj))

for(i in 1:max(lp$nsubj)){
  lpConf[i] <- mean(lp[lp$nsubj == i,]$confidence)
}
for(i in 1:max(hp$nsubj)){
  hpConf[i] <- mean(hp[hp$nsubj == i,]$confidence)
}


# get mean level of confidence after block 3
lpConfThird <- rep(NA, max(lp$nsubj))
hpConfThird <- rep(NA, max(hp$nsubj))

for(i in 1:max(lp$nsubj)){
  lpConfThird[i] <- mean(lp[lp$nsubj == i & lp$block > 2,]$confidence)
}
for(i in 1:max(hp$nsubj)){
  hpConfThird[i] <- mean(hp[hp$nsubj == i & hp$block > 2,]$confidence)
}




### over vs underconfidence
# lp players
lpConfRes  <- rep(NA,max(lp$nsubj))
lpPerf <- rep(NA,max(lp$nsubj))
lpConfRes.secmethod <- rep(NA,max(lp$nsubj))
for(i in 1:max(lp$nsubj)){
   lpConfRes[i]  <- mean(rescale(lp[lp$nsubj==i,]$confidence, to=0:1))
   lpPerf[i] <- mean(lp[lp$nsubj==i,]$adv_choice)
}
  
# hp players
hpConfRes  <- rep(NA,max(hp$nsubj))
hpPerf <- rep(NA,max(hp$nsubj))

for(i in 1:max(hp$nsubj)){
    hpConfRes[i]  <- mean(rescale(hp[hp$nsubj==i,]$confidence, to=0:1))
    hpPerf[i] <- mean(hp[hp$nsubj==i,]$adv_choice)
}


## diff between conf and perf (calibrated == 0)
lpCalibration <- lpConfRes - lpPerf
hpCalibration <- hpConfRes - hpPerf

print(t.test(x=lpCalibration,y=hpCalibration))
cat("Cohen's d=", cohensD(x=lpCalibration,y=hpCalibration),'\n')
print(t.test(x=lpCalibration,mu=0))
cat("Cohen's d=", cohensD(x=lpCalibration,mu=0),'\n')
print(t.test(x=hpCalibration,mu=0))
cat("Cohen's d=", cohensD(x=hpCalibration,mu=0),'\n')



calibration <- data.frame(calibration=c(lpCalibration,hpCalibration),
                          group=c(rep('Low performance\ngroup',max(lp$nsubj)),
                                  rep('High performance\ngroup',max(hp$nsubj))))

a <- calibration %>%
      ggplot(aes(x=group,y=calibration))+
      geom_hline(yintercept = 0, linetype='dashed',col='darkgray')+
      geom_jitter(position=position_jitter(0.2))+
      stat_summary(fun.data=CI, pch=19,cex=2,
                   geom="pointrange", color="cadetblue")+
      ylab('Calibration of confidence')+
      ggtitle('All blocks')+
      xlab('')+
      ylim(c(-.35,.7))+
      theme_classic()+
      theme(title=element_text(size=16),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            axis.ticks.length=unit(.25, "cm"))



### over vs underconfidence SINCE ONSET OF AWARENESS
# lp players
lpConfRes  <- rep(NA,max(lp$nsubj))
lpPerf <- rep(NA,max(lp$nsubj))

for(i in 1:max(lp$nsubj)){
  lpConfRes[i]  <- mean(rescale(lp[lp$nsubj==i & lp$block > 2,]$confidence, to=0:1))
  lpPerf[i] <- mean(lp[lp$nsubj==i & lp$block > 2,]$adv_choice)
}

# hp players
hpConfRes  <- rep(NA,max(hp$nsubj))
hpPerf <- rep(NA,max(hp$nsubj))

for(i in 1:max(hp$nsubj)){
  hpConfRes[i]  <- mean(rescale(hp[hp$nsubj==i & hp$block > 2,]$confidence, to=0:1))
  hpPerf[i] <- mean(hp[hp$nsubj==i & hp$block > 2,]$adv_choice)
}

## diff between conf and perf (calibrated == 0)
lpCalibration <- lpConfRes - lpPerf
hpCalibration <- hpConfRes - hpPerf


print(t.test(x=lpCalibration,y=hpCalibration))
cat("Cohen's d=", cohensD(x=lpCalibration,y=hpCalibration),'\n')
print(t.test(x=lpCalibration,mu=0))
cat("Cohen's d=", cohensD(x=lpCalibration,mu=0),'\n')
print(t.test(x=hpCalibration,mu=0))
cat("Cohen's d=", cohensD(x=hpCalibration,mu=0),'\n')


calibration <- data.frame(calibration=c(lpCalibration,hpCalibration),
                          group=c(rep('Low performance\ngroup',max(lp$nsubj)),
                                  rep('High performance\ngroup',max(hp$nsubj))))


b <- calibration %>%
      ggplot(aes(x=group,y=calibration))+
      geom_hline(yintercept = 0, linetype='dashed',col='darkgray')+
      geom_jitter(position=position_jitter(0.2))+
      stat_summary(fun.data=CI, pch=19,cex=2,
                   geom="pointrange", color="cadetblue")+
      ylab('Calibration of confidence')+
      ggtitle('Since block 3')+
      xlab('')+
      ylim(c(-.35,.7))+
      theme_classic()+
      theme(title=element_text(size=16),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            axis.ticks.length=unit(.25, "cm"))


fig <- ggarrange(a, b, ncol=2, nrow=1)

fig

