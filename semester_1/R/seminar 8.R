# seminar 8
######################## praktika 7 #########################
d = rpois(1e4,100)
h = hist(d,seq(floor(min(d)),ceiling(max(d))+2,by = 2)) # step 2 for numbers
den = density(d,bw=0.7)
coef = (h$breaks[2]*h$breaks[1])*length(d) # поправка
lines(den$x,den$y*coef,col='red',lwd=3)
    #addd colors
c = h$counts
c = 1-(c-min(c))/(max(c)-min(c))
c = rgb(c,c,c)
class(h)
plot(h,col=c)

abline(v=mean(d),col='red')
abline(v=median(d),col='green')

inx=which.max((h$counts))
#h$breaks[inx]/2+h$breaks[inx]/2
abline(v=h$breaks[inx]/2+h$breaks[inx]/2,col='blue')

#
plot(h,col=c)
den = density(d,bw=0.7)
coef = (h$breaks[2]*h$breaks[1])*length(d) # поправка
lines(den$x,den$y*coef,col='red',lwd=3) 
i = 2:(length(den$x)-1)
#den$y[i]>den$y[i-1] & den$y[i]>den&y[i-1]
inx = which(den$y[i]>den$y[i-1] & den$y[i]>den&y[i+1])
den$y[inx]
abline(v = den$x[inx],col = 'yellow')
########################### hw 7 #####################################
t = split(mtcars,mtcars$cyl)
t(sapply(t,function(x)c(mean=mean(x$disp),sd=sd(x$disp))))

sapply(split(mtcars$disp,mtcars$cyl),mean)
#

oval = function(cx,cy,xd,yd,n=1000,...){
  t = seq(0,2*pi,length.out=n)
  x = cx + cos(t)*xd
  y = cy + sin(t)*yd
  polygon(x,y,...)
}

plot(1,t='n',xlim=c(0,100),ylim=c(0,100))
oval(30,40,10,30)
 ##
cyl.ch = mtcars$cyl
cyl.ch = as.character(mtcars$cyl)
cyls = sort(unique(cyl.ch))
cyl.col = RColorBrewer::brewer.pal(length(cyls),'Dark2')
names(cyl.col) = cyls
cyl.col[cyl.ch]

cyl.col[mtcars$cyl]
plot(mtcars$cyl,col=cyl.col[cyl.ch],pch=19,cex=2)
 ##
#################### seminar 8 ######################
x = runif(100,1,10)
y = rnorm(length(x),x^2)

plot(x)
plot(1:length(y),y)

plot(x,y)

d = cbind(x,y)
dev.off()
plot(d)
plot(d[,1],d[,2])

d=list(x=x,y=y)
dev.off()
plot(d)
plot(d$x,d$y)

#par(mfrow=c())
#par(fig=)

x  = 0:10
y = exp(x)-1
plot(x,y)
plot(x,y,log='y')
plot(x,y+1,log='y')
plot(x,log2(x+1))

# Засечки на оси
f = function(x)log(x+1)
f = function(x)x^0.2
plot(x,f(y),yaxt='n') 
at = c(0,10,100,1000,10000)
axis(2,f(at),at)
dev.off()

#### barplot
tab = table(mtcars$cyl,mtcars$am)
barplot(tab,beside = T,legend.text = T
        ,args.legend = list(x='topleft',title='# cyl'))
segments(b,tab-1,b,tab+1)
#text(mtcars$cyl)

pie(tab[,1])
##
x = rnorm(10000,0)*2
y = round(rnorm(10000,x),0)
x = round(x,0)
tab = table(x,y)
image(tab)
image(tab,col=rev(heat.colors(100)))
colnames(tab)
dev.off()

xx = 1:nrow(tab)
yy = 1:ncol(tab)
image(xx,yy,tab,xaxt='n',yaxt='n')
axis(1,xx,rownames(tab))
axis(2,yy,colnames(tab))

#cor matrix
c = cor(mtcars)
image(c)
#группирование признаков
heatmap(c)
heatmap(c,symm = T)
col = ifelse(c[1,]>0,'red','blue')
heatmap(c,symm = T,distfun = function(x)as.dist(1-x))
heatmap(c,symm = T,distfun = function(x)as.dist(1-x)
        ,ColSideColors =  col)

##
x = seq(0,5,length.out=100)
y = 1-exp(-x)
y = rnorm(length(y),y,0.1)
plot(x,y)
lines(x,y,col='gray',lwd=3)
lines(x,smooth(y),col='black',lwd=3)

m = smooth.spline(x,y,df = 6)
p = predict(m)
lines(p,col='yellow',lwd=3)
dev.off()
##
x = seq(0,5,length.out=100)
y = 1-exp(-x)
y = rnorm(length(y),y,0.1)
plot(x,y)

xx = seq(min(x),max(x),length.out=100)
lines(predict(smooth.spline(x,y,df=7),xx),col='red',lwd=3)
