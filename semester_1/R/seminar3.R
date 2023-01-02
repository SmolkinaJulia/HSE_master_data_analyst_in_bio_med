# seminar 3 smolkina

getwd()
setwd('C:/Users/j.smolkina/OneDrive - CRITEO/Desktop/HSE/R/class3')
getwd()

N = 10000
d = runif(N,-1,1)^2 + runif(N,-1,1)^2 <1
mean(d)*4

sum(d)/N
length(d[d])
#
piest = cumsum(d)/1:N*4
plot(piest,pch = '.')
plot(piest, type = 'l',log = 'x')
plot(abs(piest-pi), type = 'l',log = 'xy')
inx = round(seq(1,N,length.out = 200))
plot(abs(piest[inx]), type = 'l',log = 'xy')
inx = round(10^seq(0,log10(N),length.out = 200))
plot(abs(piest[inx]-pi), type = 'b',log = 'y')
#####

x = 1:10
x[-1] # delete 1 el
x[-c(5,9,1)]# delete 5,9,1 el
x[-length(x)]

#######
sex = c('m','m','f','f')
sex1 = factor(sex)
sex
sex1
sex2 = factor(sex,levels = c('m','f','u'))
sex2
table(sex)
table(sex1)
table(sex2)

levels(sex2)
attributes(sex2)
attr(sex2,'levels')

typeof(sex1)
as.integer(sex1)

z = c('02','3')
as.numeric(z)
as.numeric(factor(z))
as.numeric(as.character(factor(z)))

###### lists

x = list(int = 1:3,'a',c(T,F,T),c(2.3,5.9),sum)
x
str(x)
x[[5]](1:5)

names(x) = c('int','a','log','last')

x[1:2]
x[-1]
x[c(4,1)]
x[c(T,F,F,T)]

x[2]#list contains vector, returns list
x[[2]]# element of lists
x$log # element of lists

# add element into list
length(x)
x[[6]] = 1:10
str(x)
# add by name
x$onemore = 4:10
str(x)

# delete element
x[[6]] = NULL

x = list(list(1,2),c(3,4),onemore = list(a='1'))
typeof(x)
str(x)
x[5:7]=as.list(1:3)

y = c(list(1,2),c(3,4))
typeof(y)
str(y)
#special element in structire in the list
x[[2]][2]
x[[c(2,2)]]
x$onemore[1]
x$onemore[1]$a

#### матрицы  matrix
a = matrix(1:12,ncol = 3)
a
a = matrix(1:12,ncol = 3, byrow = T)
a
dim(a) # size of matrix
ncol(a)
nrow(a)
colnames(a) = c('a','b','c')
rownames(a) = c('r1','r2','r3','r4')
a
dimnames(a) # names of rows and cols of matrix
attributes(a)
typeof(a)
length(a)
a[1:3]

# create matrix of vector
x = 1:10
dim(x) = c(2,5)
x
x[5] = 4.2
x[6] = 'stroka'
typeof(x)

matrix(NA,ncol = 3,nrow = 6)
class(a)

b=a
b[1,1]='a'
b

a[1:2,c(1,1)]
a[,3:2]
a[,3]#turns into vector
a[,2,drop=F]#stays matrix
a[-2,2:3]
a[c('r2','r4'),2:3]
a[a[,1]>2,]

r=a[1,] #transponation
t(r)
x = 1:10
which(x%%2==1)# transform logical index into integer
x[which(x%%2==1)[1]]
inx = which(a>2,arr.ind = T)# return vector of indexes with 2 columns
a[inx]
a[c(2,3)]
a[matrix(c(2,3),ncol=2)]

## n-mer
b = array(1:12,dim=c(2,3,2))
b
length(b)
typeof(b)
dim(b)
b[1,2:3,2]
b[1,2:3,2,drop=F]
dimnames(b) = list(c('a1','a2'),c('b1','b2','b3'),c('c1','c2'))
b
b[T,'b2',1]
which(b>2,arr.ind = T)

#### dataframe can contain different types
x = data.frame(a=1:3,b=c(T,T,F),c=c('a','b','c'))
x
cbind(a=1:3,b=c(T,T,F),c=c('a','b','c'))
as.matrix(x)
x
typeof(x)
class(x)

rownames(x)=c('r1','r2','r3') # requires unique names
colnames(x)=c('c1','c2','c3')
x
x[1:2,3]
x['r1',]# it is always data.frame
x[,1]# vector
x[,1,drop=F]#data frame with 1 col
x$c1 #by name of col
x[[1]]

#as.matrix(read.csv())
############ we need the same type by columns
plot(a[1,])
plot(x[,1])
plot(x[1,])
plot(as.numeric(x[1,]))

#
t.test(1:10)$p.value
#
#### apply,sapply,lapply
x
lapply(x,typeof) # to each element of x by col ( as list), returns list
sapply(x,typeof) # to each element of x by col ( as list), return to easier 
                                                #type like vector and others
lapply(x,summary)
sapply(1:10,log) # =log(1:10)
lapply(1:10,log)

apply(a,1,mean) # mean by raws
apply(a,2,mean) # mean by columns
apply(a,2,summary)
rver = apply(a,1,mean)
rver
a[rver>1]
a[apply(a,1,mean)]

colSums(a)
colMeans(a)
rowMeans(a)
apply(a,1,sd)
