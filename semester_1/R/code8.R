# reminder: #####
x = runif(100,1,10)
y = rnorm(length(x),x^2)
plot(y) #creates new plot
# same as 
plot(1:length(y),y)

plot(x,y)
plot(x,y,t='p') # same
lines(x,y)
points() #same as lines(,t='p')
#parameters:
plot(x,y,type='b',pch=19,cex=1.4,col='red',lty=2,lwd=3)
# plot functions
segments
text
abline
polygon
rect

# plot annotation
legend
plot(1)
plot(1,xaxt='n')
plot(1,xaxt='n',yaxt='n')

# layout
par(mfrow=c(2,2))
layout(rbind(c(1,1,2),
             c(1,1,3)))
layout.show(3)
#par(fig=c())
dev.off()

# use axis to make own scale:
x=0:10
y = exp(x)-1
plot(x,y)
plot(x,y,log='y')

f = function(x){log(x+1)}
plot(x,f(y),yaxt='n')
at = c(0,10,100,1000,10000)
axis(2,f(at),at)
axis(2,f(at),at,las=2)

# barplot ######
par(mfrow=c(1,3),tck=-0.02,mgp=c(1.1,0.2,0),mar=c(2,3,1.0,0),oma=c(0,0,0,1))
tab = table(mtcars$cyl,mtcars$am)

b = barplot(tab)
b
(b = barplot(tab,beside=T))
# barplot output can be used to add smth to the plot. Errorbars for instance
segments(b,tab-1,b,tab+1)
(barplot(t(tab),beside=T,col=rainbow(ncol(tab))))

# piechart #####
pie(tab[,1])
pie(tab[,2])

# image ######
x = rnorm(10000,0)
y = round(rnorm(10000,x)*2,0)
x = round(x*2,0)
tab=table(x,y)
tab

image(tab,col=heat.colors(100))
image(tab,col=rev(heat.colors(100)))
image(tab,col=topo.colors(100))

# how to make correct coordinates?
xx = 1:nrow(tab)
yy = 1:ncol(tab)
image(x = xx,y=yy,z = tab,col=topo.colors(100))
image(x = xx,y=yy,z = tab,col=topo.colors(100),xaxt='n',yaxt='n')
axis(1,xx,rownames(tab))
axis(2,yy,colnames(tab))


# heatmap ######
dev.off()
library(RColorBrewer)
c = cor(mtcars)
image(c)
heatmap(c)
heatmap(c,symm = T)
heatmap(1-c,symm = T,col=brewer.pal(11,'RdYlGn'))
heatmap(1-c,symm = T,col=brewer.pal(11,'RdYlGn'),distfun = function(x)as.dist(x))


# smoothing
x = seq(from=0,5,length.out=100)
y = 1-exp(-x)
y = rnorm(length(y),y,0.1)
plot(x,y,pch=19)
lines(x,y,col='gray',lwd=3)
lines(x,smooth(y),col='black',lwd=3)
m = smooth.spline(x,y,df=2)
p = predict(m)
lines(p,col='red',lwd=3)
lines(predict(smooth.spline(x,y,df=3)),col='orange',lwd=3)
lines(predict(smooth.spline(x,y,df=7)),col='green',lwd=3)

x2 = x^2
m = lm(y ~ x + x2)
lines(x,predict(m),col='blue',lwd=3)

# by default predict predicts in original x points
x = seq(from=0,5,length.out=10)
y = 1-exp(-x)
y = rnorm(length(y),y,0.1)
plot(x,y,pch=19)
lines(predict(smooth.spline(x,y,df=7)),col='red',lwd=3)
# so line is not smooth
# lets add more points
lines(predict(smooth.spline(x,y,df=7),seq(min(x),max(x),length.out=100)),col='blue',lwd=3)
# bit more complicated for lm
plot(x,y,pch=19)
m = lm(y ~ x + I(x^2) + I(x^3) + I(x^4))
lines(x,predict(m),col='red',lwd=3)
xx = seq(min(x),max(x),length.out=100)
lines(xx,predict(m,newdata = list(x=xx)),col='blue',lwd=3)


# coordinate systems #####
dev.off()

plot(1,xlim=c(0,1),ylim=c(0,1))#,yaxs='i',xaxs='i')
y1=grconvertY(1,'npc','user')
x1=grconvertX(0,'npc','user')
text(x1,y1,'text',adj=c(0,1))

y1=grconvertY(1,'nfc','user')
x1=grconvertX(0,'nfc','user')
text(x1,y1,'A',adj=c(0,1),xpd=T)

# expressoins to make labels ######
plot(1,t='n',
		 main='first\nsecond row',
		 xlab=expression(frac(sqrt(x+y),z^2)),
		 ylab=expression(x %=~% y))

mtext('on the right',4,0)
# how to make one title for whole page? use outer margins!
par('oma')
par(mfrow=c(1,2),oma=c(0,0,1,0))
plot(1)
plot(2)
mtext('on the top',3,0)
mtext('on the top!',3,0,outer = T)

