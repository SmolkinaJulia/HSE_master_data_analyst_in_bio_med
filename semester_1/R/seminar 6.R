## seminar 6 (30.10)
install.packages('ape')
library(ape)
# ape::alex() name_of_packege:: function
# devtools::  - install from github
# install.packeges('BiocManager')  - install from biomcontacter CRAN bioconductor
# BiocManager::install('edgeR')

#####
plotSin = function(x,t,...){  # ... for adding additional parameters
  plot(x,sin(x),t=t,...)
}

plotSin(1:100/10,t='l',lwd = 3,bty='n',col='blue')

#########
sapply(mtcars,min) #by col
#in each column we find cars wich has min value
lapply(mtcars,function(x)rownames(mtcars)[x==min(x)]) #by columns 

x = runif(20)
a = 1:20
b = -(1:20)^2
ifelse(x>0.5,a,b) #ifelse(x>0.5,true,false)
ifelse(x>0.5,'s','f')

############### apply it is sapply for matrix and n-mer massives
scale() #normirovanie for columns
m = matrix(rnorm(110,10+rep(1:10,each=10)),ncol = 10)
dim(m)
boxplot(m,col='green')
cm = apply(m,2,mean) #apply(where,col=2 string = 1,what to count)
length(cm)
nm = sweep(m,2,cm,'-') #sweep(where,col = 2 string = 1,what to delete,funvtion likr '-' '/' '*')
length(nm)
boxplot(nm)
boxplot(t(t(m)-cm)) #transporting matrix

#######
do.call(plot,list(x=1:10)) #=plot(x=1:10))

m = matrix(1:12,ncol=3)
m
s = apply(m,1,paste,collapse=',')
s
is.vector(s)
sapply(strsplit(s,','),as.numeric)
r = lapply(strsplit(s,','),as.numeric)
do.call(rbind,r)

#######
#to get variable by name
get('m')
get('nm')

m1 = rnorm(10)
m2 = rnorm(11)
m3 = rnorm(100)

sapply(1:3,function(i)get(paste0('m',i)))

###
paste('a','b') #by space
paste0('a','b') #net razdelitelya
###

sex = ifelse(runif(20)>0.5,'m','f')
height = ifelse(sex == 'm',rnorm(20,185,5),rnorm(20,165,5))
mean(height[sex =='f'])

sh = split(height,sex) #split(what to split,by which group split)
wilcox.test(sh$f,sh$m)
sapply(sh,mean)

data = data.frame(sex=sex,height=height)

sort(data$height)
o = order(data$sex,data$height)#from min to max by index
data$height[o]

sdata = split(data,data$sex)
sdata$f
lapply(sdata,function(x)x[order(x$height,decreasing = T)[1],])
do.call(rbind,lapply(sdata,function(x)x[order(x$height,decreasing = T)[1],]))
#inx = which.max(x$height)
#x[inx,])

adata = as.matrix(data)
adata
split(data,data$sex)
split(adata,adata[,'sex'])
split.data.frame(adata,adata[,'sex'])

###Plyr
library('plyr')
laply(baseball,typeof)
par(mfrow=c(3,3))
l_ply(1:9,plot,col='red')

m = matrix(rnorm(1e5),ncol=5)
z = aaply(m,1,mean,.progress = 'text') 

library(doMC)
registerDoMC(2) #number of score-2 of your laptope
system.time({l_ply(1:100,function(i)mean(rnorm(1e6)))})
system.time({l_ply(1:100,function(i)mean(rnorm(1e6)),.parallel = TRUE)})

### graphs
dev.off()
x = seq(0,5,length.out=20)
y = 1 - exp(-x)
y = rnorm(length(y),y,0.03)

plot(x,y)
par(mfrow=c(2,3),bty='n') #bty='n'- no frame at plot
plot(x,y,type='l')
plot(x,y,type='b')
plot(x,y,type='s')
plot(x,y,type='S')
plot(x,y,type='h')
# osi and itle
plot(1:20,y) #plot(title for x = 1:20,y)
plot(x,y,type='s',xlab = 'x-axis',ylab = 'y-axis',main = 'someplot')
plot(x,y,log='x')
plot(x,y,log='yx')
plot(x+1,y+1,log='yx')

par(mfrow=c(1,1))
plot(1,xlim=c(0,1),ylim=c(0,6),t='n')
for(i in 1:6){
  lines(0:1,c(i,i),lty=i,lwd=i) #lty - line type
  
}

#lines and pch
plot(1:2,1:2,pch=c('d','0')) #pch - point character
plot(1:2,1:2,pch=3)
plot(x = rep(1:5,times=5),y =rep(1:5,each=5),pch=0:24,cex=3)
plot(x = rep(1:5,times=5),y =rep(1:5,each=5),pch=0:246,cex=1:25/4)

#colors
col = colors()
col
plot.new()
legend('topleft',fill=col,legend = col,ncol=6,cex = 0.7)
#  '#000000' '#FF00FF'
plot(1:10,col='#FF00FF',pch=19,cex=3)
# прозрачность
plot(1:10,col='#FF00FF88',pch=19,cex=30)
plot(1:10,col='#FF00FF10',pch=19,cex=30)

rgb(1,0,1)
rgb(255,0,155,maxColorValue = 255)
col2rgb('red')
col2rgb('#FF00FF')
col2rgb(c('#FF00FF','#0F00FF'))

par(mfrow=c(3,3))
plot(x,y,col=c('blue','#FF00FF'),pch=19)
cols = rgb(y^2,0,max(y^2)-y^2,maxColorValue = max(y^2)) #gradient
plot(x,y,col=cols,pch=19)
plot(x,y,col=heat.colors(40),pch=19)
plot(x,y,col=terrain.colors(20),pch=19)
plot(x,y,col=topo.colors(20),pch=19)
plot(x,y,col=rainbow(20),pch=19)

plot(x,y,col=ifelse(y>0.6,'red','blue'))
plot(x,y,col=ifelse(round(x)%%2==0,'red','blue'))

sex =sample(c('m','f'),20,replace=T)
scol = c(m='blue', f= 'red')
plot(x,y,col=scol[sex],pch=19)
legend('bottomright',col=scol,pch=19,legend=names(scol))

#gradients
bbgor = colorRamp(c('blue','black','green','orange','red'))
sy = (y-min(y)/(max(y)-min(y)))
bbgor(0.5)
bbgor(sy)
cols = apply(bbgor(sy),1,function(x)rgb(x[1],x[2],x[3],maxColorValue = 255))
plot(x,y,col=cols,pch=19)

###
x = seq(0,5,length.out=20)
y = 1 - exp(-x)
y = rnorm(length(y),y,0.03)
bbgor = colorRamp(c('blue','black','green','orange','red'))
sy = (y-min(y))/(max(y)-min(y))
cols = apply(bbgor(sy),1,function(x)rgb(x[1],x[2],x[3],maxColorValue = 255))
plot(x,y,col=cols,pch=19)
####

#choose palitra
library(RColorBrewer)
brewer.pal.info
par(cex=0.7,bty = "o")
display.brewer.all()
brewer.pal(5,'Set1')
