#seminar 09.10
getwd()
setwd('C:/Users/j.smolkina/OneDrive - CRITEO/Desktop/HSE/R/class5')
getwd()
n=5
r=1
#factorial
prod(1:n)
for(i in 1:n) r=r*i
r
# sred geom
r =nrow(mtcars)/colSums((1/mtcars))
r
r=c()
for(i in 1:ncol(mtcars)) r[i] = 1/mean(1/mtcars[,i])
r

# count polinom
x = rnorm(100)
coefs = rnorm(10)
r1=c()
for(i in 1:length(x)) 
  r1[i]=sum(x[i]^(0:length(coefs)-1)*coefs)
r1

r2=rep(0,length(x))
r2 = x*0
for(j in 1:length(coefs))
  r2 = r2 + x^(j-1)*coefs[j]
r2
plot(r1,r2)
abline(a=0,b=1)
hist(r1-r2)

#hw
#f = ape::read.dna('adres',as.character=TRUE,format='fasta',as.matrix=FALSE)
#length(f[[1]])
#f = paste(f[[1]],collapse='')

#f= seqinr::read.fasta('adres',as.string = T)
#nchar(f[[1]])
#f = f[[1]]

f = readline('C:/Users/j.smolkina/OneDrive - CRITEO/Desktop/HSE/R/class4/ecoli.fasta')
f[1:10]
f=paste(f[-1],collapse = '')#without 1rst string
length(f)
nchar(f)
hex = substring(f,1:(nchar(f)-5),6:nchar(f))
hex= sort(table(nchar(hex)))
head(hex)#most rare
tail(hex)#most common

pal = c()
tr=c(A="T",T="A",G='C',C='G')
tr['A']
hexl = strsplit(names(hex),'')
i = 1

for(i in 1:length(hexl)){
  pal[i] = all(hexl[[i]] == rev(tr[hexl[[i]]]))
}
table(pal)
wilcox.test(hex[pal],hex[!pal],alternative = 'l')
table(strsplit())

#####################################################
r = c()
for(i in 1:5)
  r[i] = i^2

r = 1:5
for(i in length(r))
  r[i]=r[i]^2
r

par(mfrow=c(4,3))#to draw several pictures on 1 
for(i in 1:ncol(mtcars))
  plot(mtcars[,1],mtcars[,i])


my.rbinom = function(n,p=0.5){       #p default = 0.5
  x = sum(runif(n,0,1)<p)
  return(x)
}
  
my.rbinom(100)  
my.rbinom(100,0.1)  
my.rbinom(p=0.1,100)  

plot(x=1:10,y=sin(1:10),col='red')  

x = 10
a = 5  
myFun = function(x){
  print(a)
  a = 100
  x = paste(a,x)
  x
}
myFun(x)
x
a

f = function(abc,abd,ttt){
  cat(abc,abd,ttt)
}
f(1,2,3)
f(ttt=1,abc=2,3)
f(abd=2,ab=5,3)
f(ab=2,4,4)


f = function(...)print(list(...))
f(a=1,b=10)  

plotSin = function(x,...){plot(x,sin(x),...)}
plotSin(1:100/2,t='b',col='red',pch=19)

###class
x = 1:10
class(x)
length(x)
class(x) = 'my'#creation of attribute of x

length.my = function(x)runif(1)
length(x)
class(x) = 'integer'
length(x)

class(x) = c('my','myy')

t = table(1:10,1:10)
plot(t)
class(t)
?plot.table


x = 1:10
y = x^2
m = lm(x~y)
plot(m)
?plot.lm

## text function
text =c('hello','world')
substr(text,2,4) #taking subwords from 2 to 4th position in each
substr(text,2:3,4:6)
substr(text[1],2:3,4:5) # ignore 2d word
substring(text,2:3,4:5)
substring(text[1],2:3,4:5)

strsplit(text,'l')
strsplit(text,'l|r')
strsplit(text,'l|r',perl = F)

paste(text[1],text[2])
paste0(text[1],text[2])
paste0('rho=',cor(mtcars$mpg,mtcars$cyl))
paste(text[1],' ',text[2])
paste(text,1:2,5:8)
paste(text,collapse=',')

grep('ll',text) #return number of world with this 'll' pattern
grepl('ll',text) #logical
gregexpr('l',text)
gregexpr('l+',text)

gsub('hello','goodbye',text)

#
myApply = function(x,fun) fun(x)
myApply(1:10,sd)

funcs = list(mean,median,var,sd)
for(i in funcs) print(i(1:10))

powerFactory = function(p){
  function(x) x^p
}

p5 = powerFactory(5)
p5(3)


'+'(1,2)
'['(10:20,2:3)


"%y%" = function(a,b) a+b
"%y%"(10,11)

#
sapply(mtcars,min)
rownames(mtcars)[sapply(mtcars,which.min)]

sapply(mtcars,function(x)rownames(mtcars)[which.min(x)])

sapply(mtcars,function(x){c(rownames(mtcars)[which.min(x)],
                            rownames(mtcars)[which.max(x)])})
