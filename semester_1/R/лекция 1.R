#'лекция 1 11.09'

print("hello!")
d = rnorm(300)
d
d[1:10]
plot(d)
?rnorm
summary(d)
hist(d)

#'d вектор'

plot(density(d),ylim = c(0,0.5),xlim = c(-4.5,4.5))

x = seq(-3,3,length.out = 100) # нач знач,кон знач, кол-во точек (и нач знач и конечные входят)
y = dnorm(x) #плотность распределения Для данной точки 
#( какая вероятность получить значение в этом интервале)
x
lines(x,y,col = 'red') # draw current plot

mean(d)
var(d) # выборочная дисп
sd(d) # стандартное отклонение
median(d)
quantile(d,probs = c(0.5,0.75))

c(1,2) #crate vektor
y = rnorm(length(d),d+1, abs(d)) #generate каждый итый элемент имеет знач из среднего распр
d = cbind(d,y) # table
d[1:4,] #[strings,columns]
plot(d[,1],d[,2])
plot(d)

# линия НМК наим квадр
abline(lm(d[,2]~d[,1]), col = 'red' ) # сначала y потом x

# parcing lis(plot)
par(mfrow = c(1,3))

# compare plots
boxplot(d) #ящик с усами

plot(density(d[,1]),col = 'red')
lines(density(d[,2]),col = 'green')
plot(d[,1],d[,2])
abline(lm(d[,2]~d[,1]),col='red')

# p-value
t.test(d[,1],d[,2])
pv = wilcox.test(d[,1],d[,2])$p.value
pv # pv$p.value

# сравнение
d[,1]>0
table(d[,1]>0)           
table(d[,1]>0,d[,2]>0)  
table(first = d[,1]>0,second = d[,2]>0)  
t = table(first = d[,1]>0,second = d[,2]>0) 

#  нахождение зависимостей с помощью ФИшера - Inf = очень большое положительное число
fisher.test(t)

# проверка корреляции
cor(d[,1],d[,2])
cor(d[,1],d[,2],method = "sp") #коэф корреляции Спирмена
cor.test(d[,1],d[,2]) # p-value для коэф корреляции ( тк высокией p-value то есть связь между ними)

# Reading files
getwd() # current directory
#setwd('') # задать траекторию или задать папку
write.table(d,file = 'lec1.txt') # write table into file
ls() # all variable
rm(d) # delet
d
d = read.table('lec1.txt')
d


# save grafic into file
