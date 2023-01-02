# seminar 02.10
getwd()
setwd('C:/Users/j.smolkina/OneDrive - CRITEO/Desktop/HSE/R/class4')
getwd()

d=data.frame(surname=c('T','F','G'),
            age = c(10,20,50),
            sex = c('f','f','m'),check.names = F)
rownames(d) = d$surname
d$surname = NULL
d

d['F',] # разрешает частичные совпадения
d[rownames(d)=='F',]

d[d$sex=='m',]
d$'sex'
d[,2,drop=F]

d = matrix(rnorm(5*24,36.6,sd=10),nrow=5)
which(d[2,]>40) # возращает индекс в векторе где true

v = runif(100,-1,3)
v[sin(v)>0]

m = matrix(v,ncol=10)
m[m[,6]>1,]
m[,colSums(m< -0.5)>0]
m[,apply(m< -0.5,2,any)]
m[,apply(m,2,min)<  -0.5]
#####

typeof(mtcars[,2])
rownames(mtcars)[mtcars$cyl==mtcars['Fiat 128','cyl']]
min(mtcars$cyl)
c = cor(mtcars)
typeof(c)
c[,'mpg']< -0.7
rownames(c)[c['mpg',]< -0.7]

v=rnorm(100,40,sqrt(10))

v[c(F,F,T)] # each 3rd
v[seq(3,length(v),by=3)]
v[1:length(v) %% 3==0]

#all beside 5th
v[c(F,F,F,F,T)]
v[-seq(5,length(v),by=5)]
v[1:length(v) %% 5!=0]

v[floor(v) %%2 ==0]

t = list(list('a',list('b','c')),list('d','e'))
t[[1]]
t[[1]][[2]]
t[[1:2]]
#########

x = runif(1,-1,1)
x
if(x> 0.5){
  print(paste(x,'>0.5'))
}else{
  print(paste(x,'<0.5'))
}

if(x>0.5) print(x)

if(x> 0.5){
  print(paste(x,'>0.5'))
}else if(x>0){
  print(paste('x>0',x,'<0.5'))
}else{print(paste(x,'<0'))}

for(i in 1:5){
  j = i^2
  print(j)
}


for(j in c(3,1,6,7)){
  cat(j)
}

for(j in letters)
  cat(j)

for(j in mtcars)
  print(summary(j))

for(f in list(min,max,sd)){
  print(f(mtcars$cyl))
}

for(i in 1:ncol(mtcars)){
  print(mean(mtcars[,i]))
}

for(i in colnames(mtcars)){
  cat(i,' ',mean(mtcars[,i]),'\n')
}

j = colnames(mtcars)[1]
#

for(i in 1:10){
  if(i %% 2 !=0){
    next
  } 
  print(i)
  if(i>7){
    break
  } 
}
#
a = rnorm(3e6)
b = rnorm(3e6)
r = numeric(length(a))
r = c()

system.time({r = a+b})
system.time({
  for(i in 1:length(a)){
  r[i] = a[i]+b[i]}
})



###
#for practika
n=10
# cod
x=1:50
coesfsc(3,1,6,3)
  













