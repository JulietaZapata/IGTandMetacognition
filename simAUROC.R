# inspired on the code by Iair Embon 
# https://github.com/iair-embon/Metacognition.ASDtraits/blob/main/Figures/Figuras_en_R/Code/Fig_2.R

library(ggplot2); library(ggpubr); library(dplyr)

# low metacog participant
lowMeta <- data.frame(p=c(0.255,0.244,0.245,0.2560,0.260,0.245,0.240,0.255),
                      conf=c(1,2,3,4,1,2,3,4),
                      correct=c(1,1,1,1,0,0,0,0))

lowMetaAUC <- data.frame(pcor = c(0,cumsum(rev(lowMeta[lowMeta$correct==1,]$p))),
                         pinc = c(0,cumsum(rev(lowMeta[lowMeta$correct==0,]$p))))

# high metacog participant
highMeta <- data.frame(p=c(0.06,0.14,0.30,0.5,0.45,0.30,0.15,0.1),
                       conf=c(1,2,3,4,1,2,3,4),
                       correct=c(1,1,1,1,0,0,0,0))
highMetaAUC <- data.frame(pcor = c(0,cumsum(rev(highMeta[highMeta$correct==1,]$p))),
                          pinc = c(0,cumsum(rev(highMeta[highMeta$correct==0,]$p))))

# plot
lowMeta %>%
  ggplot(aes(x=conf,y=p,fill=factor(correct)))+
  geom_col(position=position_dodge(),show.legend=F)+
  xlab('Confidence')+ylab('Proportion')+
  geom_segment(aes(x=1.5,y=0,xend=1.5,yend=0.5))+
  geom_segment(aes(x=2.5,y=0,xend=2.5,yend=0.5))+
  geom_segment(aes(x=3.5,y=0,xend=3.5,yend=0.5))+
  geom_point(aes(x=1.5,y=0.52),shape=15,cex=3)+
  geom_point(aes(x=2.5,y=0.52),shape=16,cex=3)+
  geom_point(aes(x=3.5,y=0.52),shape=17,cex=3)+
  theme_classic(base_size=12)+
  scale_fill_manual(values=c('black','darkgray'))+
  theme(legend.position='none',aspect.ratio=1) -> a

lowMetaAUC %>%
  ggplot(aes(x=pinc,pcor))+
  geom_line()+
  geom_point(aes(x=pinc[2],y=pcor[2]),shape=17,cex=3)+
  geom_point(aes(x=pinc[3],y=pcor[3]),shape=16,cex=3)+
  geom_point(aes(x=pinc[4],y=pcor[4]),shape=15,cex=3)+
  xlab('P(confidence|incorrect)')+
  ylab('P(confidence|correct)')+
  theme_classic(base_size=12)+
  theme(aspect.ratio=1) -> b
  
  
highMeta %>%
  ggplot(aes(x=conf,y=p,fill=factor(correct)))+
  geom_col(position=position_dodge(),show.legend=F)+
  xlab('Confidence')+ylab('Proportion')+
  geom_segment(aes(x=1.5,y=0,xend=1.5,yend=0.5))+
  geom_segment(aes(x=2.5,y=0,xend=2.5,yend=0.5))+
  geom_segment(aes(x=3.5,y=0,xend=3.5,yend=0.5))+
  geom_point(aes(x=1.5,y=0.52),shape=15,cex=3)+
  geom_point(aes(x=2.5,y=0.52),shape=16,cex=3)+
  geom_point(aes(x=3.5,y=0.52),shape=17,cex=3)+
  theme_classic(base_size=12)+
  scale_fill_manual(values=c('black','darkgray'))+
  theme(legend.position='none',aspect.ratio=1) -> c

highMetaAUC %>%
  ggplot(aes(x=pinc,pcor))+
  geom_line()+
  geom_point(aes(x=pinc[2],y=pcor[2]),shape=17,cex=3)+
  geom_point(aes(x=pinc[3],y=pcor[3]),shape=16,cex=3)+
  geom_point(aes(x=pinc[4],y=pcor[4]),shape=15,cex=3)+
  xlab('P(confidence|incorrect)')+
  ylab('P(confidence|correct)')+
  theme_classic(base_size=12)+
  theme(aspect.ratio=1) -> d

ggarrange(a,b,c,d,ncol=2,nrow=2)
