library(ggplot2); library(dplyr); library(ggpubr); library(lsr)

# load data
data <- read.csv('filtereddata.csv')
raw  <- 0 # if you're loading not filtered data (rawdata.csv)
if(raw){
  # add block column
  data %>%
    mutate(block = case_when(
      trialNumber <= 20 ~ 1, 
      trialNumber > 20 & trialNumber <= 40 ~ 2, 
      trialNumber > 40 & trialNumber <= 60  ~ 3, 
      trialNumber > 60 & trialNumber <= 80  ~ 4, 
      trialNumber > 80 ~ 5
    )) -> data
  
  
  # compute metacognition
  source('auroc2.R')
  data$metacog <- NA; data$perfBlock <- NA
  for(i in unique(data$nsubj)){
    for(j in 1:5){
      # block correct rate
      data[data$nsubj==i & data$block==j,]$perfBlock <- mean(data[data$nsubj==i & data$block==j,]$correct)
      
      # metacog
      data[data$nsubj==i & data$block==j,]$metacog <- type2roc(data[data$nsubj==i & data$block==j,]$correct,
                                                               data[data$nsubj==i & data$block==j,]$confidence,
                                                               4)
    }
  }
}

### perform classic analyses: performance and metacog by block.
data %>%
  group_by(nsubj,block) %>%
  summarise(perf = mean(correct)) %>%
  ggplot(aes(x=block,y=perf))+
  geom_hline(yintercept = 0.5, lty= 2, col='gray')+
  geom_jitter(height=0, width=.1,col='springgreen3',alpha=.2)+
  stat_summary(geom='errorbar', col='springgreen3', width=.5, lwd=1)+
  xlab('Block')+ylab('Performance')+
  theme_classic(base_size=14) -> a

data %>%
  group_by(nsubj,block) %>%
  summarise(metacog = mean(metacog)) %>%
  ggplot(aes(x=block,y=metacog))+
  geom_hline(yintercept = 0.5, lty= 2, col='gray')+
  geom_jitter(height=0, width=.1,col='blue',alpha=.2)+
  stat_summary(geom='errorbar', col='blue', width=.5, lwd=1)+
  xlab('Block')+ylab('Metacognition')+ylim(0:1)+
  theme_classic(base_size=14) -> b

ggarrange(a,b,ncol=2,nrow=1)

# t-tests
data %>%
  group_by(nsubj,block) %>%
  summarise(metacog=mean(metacog),
            perf   =mean(correct)) -> sumdata

pblocks <- list(); mblocks <- list(); pd <- rep(NA,5); pm <- rep(NA,5)
for(i in 1:5){
  pblocks[[i]] <- t.test(sumdata[sumdata$block==i,]$perf, mu=0.5); print(pblocks[[i]])
  pd[i] <- cohensD(sumdata[sumdata$block==i,]$perf, mu=0.5); print(pd[i])
  mblocks[[i]] <- t.test(sumdata[sumdata$block==i,]$metacog, mu=0.5); print(mblocks[[i]])
  pm[i] <- cohensD(sumdata[sumdata$block==i,]$metacog, mu=0.5); print(pm[i])
}



### perform linear regression analysis
data %>%
  group_by(nsubj,block) %>%
  summarise(metacog=mean(metacog),
            perf   =mean(correct)) -> sumdata

m <- list(); p <- list()
for(i in 1:5){
  m[[i]] <- summary(lm(perf~metacog, sumdata[sumdata$block==i,]))
  sumdata %>%
    filter(block==i) %>%
    ggplot(aes(x=metacog,y=perf))+
    geom_hline(yintercept = .5, col='gray', lty=2)+
    geom_vline(xintercept = .5, col='gray', lty=2)+
    geom_point()+
    geom_smooth(method='glm',fill='cadetblue',col='cadetblue')+
    ylab('Performance')+xlab('Metacognition')+
    coord_cartesian(ylim=0:1, xlim=0:1)+
    ggtitle(paste('Block ',i))+
    theme_classic(base_size=14) -> p[[i]]
}

# plot betas and ci 
betas <- data.frame(block=c('Block 1', 'Block 2', 'Block 3', 'Block 4', 'Block 5'),
                    betaval=sapply(m,function(l){l$coefficients[2,1]}),
                    se=sapply(m,function(l){l$coefficients[2,2]}))
betas$cisup <- betas$betaval + 2*betas$se
betas$ciinf <- betas$betaval - 2*betas$se

betas %>%
  ggplot(aes(x=block,y=betaval))+
  geom_point(shape=1,size=4)+
  geom_errorbar(aes(x=block,ymin=ciinf,ymax=cisup),width=0,col='cadetblue')+
  ylab('Beta value')+xlab('Block')+
  geom_hline(yintercept = 0, lty=2, col='gray')+
  theme_classic(base_size=14)+
  scale_x_discrete(labels=1:5)-> p[[6]]
  

ggarrange(plotlist = p, ncol=3, nrow=2)


### confidence by decks
data %>%
  group_by(nsubj, block, cardSelected) %>%
  summarise(conf = mean(confidence),
            n = n()) %>%
  mutate(prop = n/sum(n)) -> confdata

confdata %>%
  ggplot(aes(x=block,y=conf,col=cardSelected))+
  stat_summary(geom='line',lwd=1, position=position_dodge(width=.4))+
  stat_summary(geom='errorbar', lwd=1, width=.4, position = position_dodge(width=.4))+
  theme_classic(base_size=14)+
  ggtitle('Experiment 1')+
  xlab('Block')+ylab('Confidence')+
  coord_cartesian(ylim=c(1.5,3.5))+labs(col='')+
  scale_color_discrete(type=c('red','darkred','green','darkgreen'))

# anovas 
summary(aov(conf~block*cardSelected, data=confdata))
summary(aov(conf~block*cardSelected, data=confdata[confdata$cardSelected == 'A'
                                                   | confdata$cardSelected == 'B',]))
summary(aov(conf~block*cardSelected, data=confdata[confdata$cardSelected == 'C'
                                                   | confdata$cardSelected == 'D',]))


### prop of choices
confdata %>%
  ggplot(aes(x=block, y=prop, col=cardSelected))+
  stat_summary(geom='line',lwd=1, position=position_dodge(width=.4))+
  stat_summary(geom='errorbar', lwd=1, width=.4, position = position_dodge(width=.4))+
  theme_classic(base_size=14)+
  ggtitle('Experiment 1')+
  xlab('Block')+ylab('Proportion of choices')+
  coord_cartesian(ylim=c(0,.5))+labs(col='')+
  scale_color_discrete(type=c('red','darkred','green','darkgreen'))


# anova 
summary(aov(prop~block*cardSelected, data=confdata))
summary(aov(prop~block*cardSelected, data=confdata[confdata$cardSelected == 'A'
                                                   | confdata$cardSelected == 'B',]))
summary(aov(prop~block*cardSelected, data=confdata[confdata$cardSelected == 'C'
                                                   | confdata$cardSelected == 'D',]))
