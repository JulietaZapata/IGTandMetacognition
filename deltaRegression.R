library(tidyverse)
library(lmtest)

# load data
data <- read.csv2('data.csv')
nsubj <- max(data$nsubj)


# perform substraction to chance level
deltameta <- rep(NA,nsubj)
deltaperf <- rep(NA,nsubj)

for(i in 1:nsubj){
  subj <- data[data$nsubj==i,]
  deltameta[i] <- unique(subj[subj$block == 5,]$metacogbyblock5) - 0.5
  deltaperf[i] <- mean(subj[subj$block==5,]$adv_choice) - 0.5

}

d <- data.frame(deltametacog = deltameta, deltaperf= deltaperf)

# plot regression
ggplot(data=d, mapping=aes(x=deltametacog,y=deltaperf))+
  geom_point()+
  geom_smooth(method='glm', col='cadetblue', fill='cadetblue')+ 
  ylab('\u0394 performance (last block \u2212 chance level)')+
  xlab('\u0394 metacognition (last block \u2212 chance level)')+
  theme_classic()+
  xlim(c(-0.45, 0.45))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        axis.ticks.length=unit(.25, "cm"))

m <- lm(deltaperf ~ deltametacog, data=d)

# check normality
ggplot(data= as.data.frame(m$residuals),aes(x=m$residuals)) + 
  geom_histogram()+
  xlab('Residuals')+
  ylab('Frequency')+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        axis.ticks.length=unit(.25, "cm"))

shapiro.test(m$residuals)

# check homocedasticidy in standardized residuals
bptest(m)
std_residuals <- (m$residuals - mean(m$residuals)) / sd(m$residuals)
assumptions <- data.frame(res = m$residuals, std_res = std_residuals, 
                          fitval = m$fitted.values, pred = deltameta)

ggplot(data=assumptions, aes(x=fitval, y=std_res))+
  geom_point()+ 
  geom_abline(slope = 0, intercept = 0, lty=3)+
  xlab('Fitted values')+
  ylab('Standardized residuals')+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        axis.ticks.length=unit(.25, "cm"))

# check need for non-linear model
ggplot(data=assumptions, aes(x=pred, y=res))+
  geom_point()+
  geom_abline(slope = 0, intercept = 0, lty=3)+
  xlab('\u0394 metacognition (last block \u2212 chance level)')+
  ylab('Residuals')+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        axis.ticks.length=unit(.25, "cm"))

