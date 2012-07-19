generateVector <- function(n, min, max)
{
  array(runif(n, min, max), dim = c(n))
}

testFit <- function(dist){
    h<-hist(dist,breaks=15)
    xhist<-c(min(h$breaks),h$breaks)
    yhist<-c(0,h$density,0)
    xfit<-seq(min(dist),max(dist),length=40)
    yfit<-dnorm(xfit,mean=mean(dist),sd=sd(dist))
    plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)))
    lines(xfit,yfit, col="red")
}

