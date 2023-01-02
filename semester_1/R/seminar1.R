# seminar 1 Smolkina Julia

# set parametrs
data1 = rpois(200,lambda = 0.5)
data1

mean(data1)
var(data1) 
sd(data1) 
median(data1)

data2 = rpois(200,lambda = 3)
data2

mean(data2)
var(data2) 
sd(data2) 
median(data2)

# grafics
summary(data1, col = 'blue')
hist(data1)

summary(data2)
hist(data2, col = 'red')

plot(density(data1),col = 'blue')
plot(density(data2),col = 'red')

# count correlation
cor(data1,data2)
cor(data1,data2,method = "sp") 
cor.test(data1,data2)

#table
t = table(d1 = data1,d2 = data2) 
t
t_new = cbind(data1,data2)
t_new

boxplot(t_new,col = 'pink')

cor(t[,1],t[,2])
cor(t[,1],t[,2],method = "sp") 
cor.test(t[,1],t[,2])

cor(t_new[,1],t_new[,2])
cor(t_new[,1],t_new[,2],method = "sp") 
cor.test(t_new[,1],t_new[,2])
#getwd()

# save grafic into file
pdf('seminar1_new.pdf',width = 4,height = 4)
plot(density(data1),col = 'blue')
plot(density(data2),col = 'red')
hist(data1)
hist(data2)
boxplot(t_new,col = 'pink')
dev.off()

saveRDS(data1,'data1.rds')
saveRDS(data2,'data2.rds')
saveRDS(t_new,'t_new.rds')