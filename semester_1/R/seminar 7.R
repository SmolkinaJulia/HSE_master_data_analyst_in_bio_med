# seminar №7 13.11
library(plyr)

########################################## praktika 6 ###############################################
bs = sapply(split(baseball$year,baseball$team),function(x)c(min(x),max(x),length((x))))
bs = t(bs) 
bs[1:2,]
bs = bs[order(bs[,1],bs[,2]),]
#hist(bs[,3])
c = bs[,3]/max(bs[,3])
bgr = colorRamp(c('blue','gray','red'))
cols = apply(bgr(c),1,function(x)rgb(x[1],x[2],x[3],maxColorValue =  255))
plot(1,xlim = range(bs[,1:2]),ylim=c(1,nrow(bs)),t='n',bty='n',xlab='year',ylab = 'team')
segments(bs[,1],1:nrow(bs),bs[,2],1:nrow(bs),col=cols,lwd=c*4+0.4)

########################################## hw 6 ##########################################################
tnames = table(baseball$team)
tnames = names(tnames)[tnames>199]
bs = baseball[baseball$team %in% tnames,]

##### %in%
1:5 %in% 2:3
x = c('a','b','c','4','f','A','N','qwer')
f = x %in% letters
x[f]
intersect(x,letters)

# переход игроков из команды в команду
m = matrix(0,ncol=length(tnames),nrow=length(tnames),dimnames = list(tnames,tnames))
m[1:5,1:5]
p2t = lapply(split(bs$team,bs$id),unique)
p2t[1:2]

for(ts in p2t){
  for(a in ts){
    for(b in ts){
      m[a,b] = m[a,b] + 1
    }
  }
}
m[1:4,1:4]
cnt = diag(m)
for(a in tnames){
  for(b in tnames){
    m[a,b] = m[a,b]/min(cnt[a],cnt[b])
  }
}
hist(m)

mds = cmdscale(1-m,k=2)
mds[1:2,]
#plot(mds[,1],mds[,2])
plot(mds,cex = cnt/(cnt)*1.5,pch=19)
text(mds,rownames(mds),col='red',adj = c(1.2,1.2),xpd = T)

lg = sapply(split(baseball$lg,baseball$team)[rownames(mds)],unique) # разбивка команд по лигам
#sapply(split(baseball$lg,baseball$team),unique)[rownames(mds)]
year = sapply(split(baseball$year,baseball$team)[rownames(mds)],mean)
table(lg)

pchs = c(AL=18,NL=19)
year = (year-min(year))/(max(year)-min(year))
cols = apply(bgr(year),1,function(x)rgb(x[1],x[2],x[3],maxColorValue =  255))

plot(mds,cex = cnt/(cnt)*1.5,pch=pchs[lg],col=cols)
text(mds,rownames(mds),col='red',adj = c(1.2,1.2),xpd = T)
legend('bottomright',pch=pchs,legend = names(pchs))

plot(year,col=cols)#чем краснее тем старше

########################################## seminar 7  #############################################################
plot(1,t='n',xlim=c(0,100),ylim=c(0,100))
lines(0:100,90+sin(0:100/10)*10)
x = 0:10*10
y= 90+cos(0:10)*10
points(x,y,pch = 19)
points(x,y,pch = 19,type = 'l')
segments(x,y+3,x,y-3)

arrows(x,y+3,x,y-3,code=3,angle = 90,length = 0.1)
text(x,y,labels = paste0('x=',x),adj=c(0,0),col = 'red')

polygon(c(0,10,20),c(0,20,0),col ='blue',border = 'red')# триугольник
t = seq(0,2*pi,length.out =  100)
polygon(30+sin(t)*10,30+cos(t)*10,col ='red',density = 30)# круг
rect(40,40,60,60,col = 'green',density = 30, angle = -65)# квадрат
rect(60,60,80,80,col = '#12567830',density = 80, angle = 45)# Прямоугольник

abline(h = 0:5*20,lty=3,col='gray')
abline(v = 0:5*20,lty=3,col='gray')
#grid()
abline(a = 40, b = -1,lty=3,col='magenta')

par('mar')
par(mar = c(3.1,3.1,2.1,10),tcl = -0.2, mgp=c(1.5,0.4,0),las = 0)#bty='n'
plot(1:10,xlab = 'X',ylab = 'Y',main = 'Plot')
usr = par('usr')
legend(usr[2],usr[4],pch=1:6,legend = 1:5,xpd = T)
# размеры легенды не рисуя ее
l = legend(usr[2],usr[4],pch=1:6,legend = 1:5,xpd = T,plot = FALSE)
l$rect

par(mfrow=c(2,3),mar = c(3.1,3.1,2.1,5),tcl = -0.2, mgp=c(1.5,0.4,0),las = 0)#bty='n'
for(i in 1:6){
  plot(1,1,pch=as.character(i),xaxt='n',yaxt='n',cex=10,xlab='',ylab='')
}
par(mfcol=c(2,3),mar = c(3.1,3.1,2.1,5),tcl = -0.2, mgp=c(1.5,0.4,0),las = 0,bty='n')
for(i in 1:6){
  plot(1,1,pch=as.character(i),xaxt='n',yaxt='n',cex=10,xlab='',ylab='')
}

par(cex=3)
layout.show(15)

m = matrix(c(1,1,2,1,1,2,3,4,4),ncol=3)
m = rbind(c(1,1,3),
          c(1,1,4),
          c(2,2,4))

getOption("device")
options(device = "windows")


layout(m,widths = c(1,2,1),heights = c(1,1,1))
plot(1)
barplot(1:3)
boxplot(mtcars)
pie(1:4)

par(cex=1)
layout.show(3)

#карманы
hist(mtcars$mpg)
hist(mtcars$mpg,breaks = 10)
hist(mtcars$mpg,breaks = seq(min(mtcars$mpg),max(mtcars$mpg),length.out = 11))
x = rnorm(100)
y = rnorm(100,mean = 1)

range(x,y)

br = seq(round(range(x,y)[1])-1
        ,round(range(x,y)[2])+1,by =0.3)
hx = hist(x,br,plot =F)
hy = hist(y,br,add = T,plot =F)

ymax = max(hx$counts,hy$counts)
hist(x,br,col = '#0000FF80', ylim=c(0,ymax),border = NA)
hist(y,add = T,br,col = '#FF000080',border = NA)

boxplot(list(vx=x,vy=y))










