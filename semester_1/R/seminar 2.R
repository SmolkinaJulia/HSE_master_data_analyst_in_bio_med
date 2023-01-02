#seminar2

getwd()
setwd('C:/Users/j.smolkina/OneDrive - CRITEO/Desktop/HSE/R/class2')
getwd()


##
# creating variable
a <- 1
a = 'hefuh'
.a = 'r'
#my.var
ls()

length(a) # to know how many items in a
a = c(1,2,'ee')
length(a)
a[4] = 5
a
# type of vectors variable int, double, text, complex
TRUE
FALSE

log.var = c(TRUE,FALSE)
int.var = c(1L,4L) # L shows that it is int
num.var = c(1,5,6.7) #double
chr.var = c('s','re')

typeof(int.var)
typeof(2)
typeof(2L)

round(2.3)
floor(3.9) # round down
ceiling(2.2)# round up

c(chr.var,'f') # insert in vack new item
c(chr.var,c('new1','new2'))
1:10
typeof(-5.1:6)
seq(-5,6,by = 0.3) # FROM -5 TO 6 WITH STEP 0.3

#
T
F
c(T,F)
T = FALSE
T
TRUE = FALSE
T = TRUE

#
1/0
log(-10)
NA # miss data might be in vector
NULL # 
length(NA)
length(NULL)

x = 1:10
x[1] = NA
x
x = NULL
x
length(x)

a = 1:10
b = a # copy of a no pointers
a
b

x = integer(10) # also exist for char and others
x[] = NA
x
length(x)

y[] = 0
rep(0,length(x))# what to repeat and how many times
rep(1:3,times = 5) #repetion of 1:3  5 times
rep(1:2, each = 4) # each item of vector repeats 4 times
#

c(T,F,1L)
typeof(c(T,F,1L))
typeof(c(T,F,1L,9.7))
typeof(c(T,F,1L,9.7,'err'))

as.numeric(c('1','2')) # exists as.integer as.character as.logical
as.logical(Inf)

typeof(1L+2)
1+ TRUE
x = rnorm(100)
x>0
sum(x>0) # as true = 1 then we will know number of items >0
all(x>0)

1 + '2' # error
1 + as.integer('2')

# attributes
x = 1:10
attr(x,'my.attr') = 'attr'
x
attributes(x)

letters
attr(x,'names') = letters[1:10]
x # almost dictionary
names(x)
names(x) = LETTERS[1:10]
x
x = c(a=1,b=2,r=-3,r = 0)# can use the same neme several times

# slices of vectors
x[c(4,1)]
x[c(4.1,1)]
x[3]

# logical indexes
x[x>0]
inx = x>0
x[inx]
x[c(T,F,T)]

y = is.na(c(NA,1))
y[!is.na(c(NA,1))] #reverse of is.na
na.omit(y)

#
x[c('a')]
x['a']
x[c('b','a')]
x['r']# takes first of doublicates
names(x)
t = table(names(x))
t[t>1]

# operators + - * ^ (**) /
2^2
2**2
9 %% 2 # ostatok ot delenia
9 %/% 2 # celochiclennoe  delenie
2>=2
!TRUE # otrizanie
!(2>3)
2 !=2
T | F # logical or
T & T # ogical and

# out of range indexws
x = 1:10
x[100]
x[100] = 10

#
x = rep(1,10)
y = 1:10
# element by element
x+y
y+y
y - x
y**2
# if lengths are different
x = 1:5
x
y
y+x
x = 1:4
y+x
y+2
y^c(1:2)

#each second element
y[T]
y[c(T,F)]
y[c(T,F)] + y[c(F,T)]

#
x <- 5
6 -> x
x
x < -5
x <- 5
x< -6

#
1  + NA
T & NA
T | NA

