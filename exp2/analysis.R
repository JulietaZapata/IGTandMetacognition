library(ggplot2); library(dplyr); library(ggpubr)

# load data
data <- read.csv('filtereddata.csv')

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
  summarise(metacog= mean(metacog),
            perf   = mean(correct)) -> sumdata


pblocks <- list(); mblocks <- list(); pd <- rep(NA,5); pm <- rep(NA,5)
for(i in 1:5){
  pblocks[[i]] <- t.test(sumdata[sumdata$block==i,]$perf, mu=0.5); print(pblocks[[i]])
  pd[i] <- cohensD(sumdata[sumdata$block==i,]$perf, mu=0.5); print(pd[i])
  mblocks[[i]] <- t.test(sumdata[sumdata$block==i,]$metacog, mu=0.5); print(mblocks[[i]])
  pm[i] <- cohensD(sumdata[sumdata$block==i,]$metacog, mu=0.5); print(pm[i])
}


### perform pre-registered analysis
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


### confidence & proportion of selections by decks
# get new variable of deck choices given deck ordering
originalDecks <- c('A', 'B', 'C', 'D')
data$deckChoice <- NA
for(i in unique(data$nsubj)){
  o <- data[data$nsubj==i,]$deckOrder[1]
  o <- toupper(o)
  o <- c(substr(o,1,1), substr(o,2,2), substr(o,3,3), substr(o,4,4))
  data[data$nsubj==i & data$cardSelected=='A',]$deckChoice <- originalDecks[which(o=='A')]
  data[data$nsubj==i & data$cardSelected=='B',]$deckChoice <- originalDecks[which(o=='B')]
  data[data$nsubj==i & data$cardSelected=='C',]$deckChoice <- originalDecks[which(o=='C')]
  data[data$nsubj==i & data$cardSelected=='D',]$deckChoice <- originalDecks[which(o=='D')]
}


data %>%
  group_by(nsubj, block, deckChoice) %>%
  summarise(conf = mean(confidence),
            n = n()) %>%
  mutate(prop = n/sum(n)) -> confdata

confdata %>%
  ggplot(aes(x=block,y=conf,col=deckChoice))+
  stat_summary(geom='line',lwd=1, position=position_dodge(width=.4))+
  stat_summary(geom='errorbar', lwd=1, width=.4, position = position_dodge(width=.4))+
  theme_classic(base_size=14)+
  ggtitle('Experiment 2')+
  xlab('Block')+ylab('Confidence')+
  coord_cartesian(ylim=c(1.5,3.5))+labs(col='')+
  scale_color_discrete(type=c('red','darkred','green','darkgreen'))

# anovas 
summary(aov(conf~block*deckChoice, data=confdata))
summary(aov(conf~block*deckChoice, data=confdata[confdata$deckChoice == 'A'
                                                   | confdata$deckChoice == 'B',]))
summary(aov(conf~block*deckChoice, data=confdata[confdata$deckChoice == 'C'
                                                   | confdata$deckChoice == 'D',]))


### prop of choices
confdata %>%
  ggplot(aes(x=block, y=prop, col=deckChoice))+
  stat_summary(geom='line',lwd=1, position=position_dodge(width=.4))+
  stat_summary(geom='errorbar', lwd=1, width=.4, position = position_dodge(width=.4))+
  theme_classic(base_size=14)+
  ggtitle('Experiment 2')+
  xlab('Block')+ylab('Proportion of choices')+
  coord_cartesian(ylim=c(0,.5))+labs(col='')+
  scale_color_discrete(type=c('red','darkred','green','darkgreen'))


# anova 
summary(aov(prop~block*deckChoice, data=confdata))
summary(aov(prop~block*deckChoice, data=confdata[confdata$deckChoice == 'A'
                                                 | confdata$deckChoice == 'B',]))
summary(aov(prop~block*deckChoice, data=confdata[confdata$deckChoice == 'C'
                                                 | confdata$deckChoice == 'D',]))

