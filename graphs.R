makePlot <- function(flag=F, meandata, cidata, title, col4plot){
  
  if(flag==T){
    if(length(meandata[1,])==5){
      blocks <- c('Block 1','Block 2','Block 3','Block 4', 'Block 5')
      pos <- seq(from=1.1,to=4.9, length.out=5)
      lab <- ''
    }
    else{
      blocks <- 1:10
      pos <- 1:10
      lab <- 'Blocks'
    }
    
    # plot(pos,colMeans(metacognition), ylim=c(0,1), col='blue',pch=19, xaxt='n',
    #      ylab='Mean value', xlab=lab, main=title, type='n')
    # for(i in 1:length(metacognition[,1])){
    #   points(jitter(pos,0.5), metacognition[i,], pch=16, col=alpha('blue',0.2))
    # }
    # for(i in 1:length(metacognition[,1])){
    #   points(jitter(pos,0.5), performance[i,], pch=16, col=alpha('cadetblue3',0.2))
    # }
    # points(pos,colMeans(metacognition),col='blue',pch=19,xaxt='n')
    # lines(pos,colMeans(metacognition),col='blue', lwd=2,xaxt='n')
    # arrows(x0=pos, y0=cimetacog[1,], y1=cimetacog[2,], code=3, angle=90, length=0.07, col='blue',lwd=2)
    # points(pos,colMeans(performance), col='cadetblue3',pch=19)
    # lines(pos,colMeans(performance), col='cadetblue3', xaxt='n', lwd=2)
    # arrows(x0=pos, y0=ciperf[1,], y1=ciperf[2,], code=3, angle=90, length=0.07, col='cadetblue3',lwd=2) 
    # abline(a=0.5, b=0, col='grey', lty=2)
    # axis(1,at=pos,labels=blocks)
    # legend(2,0.3, legend=c('Metacognition', 'Performance'), col=c('blue', 'cadetblue3'), lwd=3, bty='n')
  }
  
  plot(pos,colMeans(meandata), ylim=c(0,1), pch=19, xaxt='n',
       ylab='Mean value', xlab=lab, main=title, type='n')
  abline(a=0.5, b=0, col='black', lty=2)
  axis(1,at=pos,labels=blocks)
  for(i in 1:length(meandata[,1])){
    points(jitter(pos,0.5), meandata[i,], pch=16, col=alpha(col4plot,0.2))
  }
  points(pos,colMeans(meandata),col=col4plot,pch=19,xaxt='n',cex=1.5)
  lines(pos,colMeans(meandata),col=col4plot, lwd=4,xaxt='n')
  arrows(x0=pos, y0=cidata[1,], y1=cidata[2,], code=3, angle=90, length=0.07, col=col4plot,lwd=4)
  
}