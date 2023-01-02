#praktika4 smolkina
#●Напишите код вычисляющий значение факториала числе
#записанного в переменную n (n задается в начале скрипта), не
#использую функции prod и factorial
#● посчитайте среднее гармоническое для всех колонок в mtcars и
#сохраните результат в вектор


getwd()
####1
n = 4
factorial = 1
for (i in 1:n){
  factorial = factorial*i
  print(factorial)
}
####2 H = n/(1/n * sum(1/x))
sumj = c()
for (i in 1:ncol(mtcars)){
  sumj[length(sumj)+1] =1/ mean(1/mtcars[,i])
}
print(sumj)
####3
#● Напишите код вычисляющий значения полинома по коэффициентам
#и значениям х.
#х — вектор, для каждого значения xi из вектора х надо посчитать
#sum(xi^(j-1)*coefs[j]) где j от 1 и до length(coefs)
x = rnorm(5, mean = 0, sd = 1)
x
coef = round(rnorm(5, mean = 2, sd = 6))
coef

x= c(10,15)
coef=1:3
pol = matrix(ncol = length(x),nrow = length(coef))
sum_pol = c()
sum_xi = 0

for(i in 1:length(x)){
  for(j in 1:length(coef)){
    pol[j,i] = sum(x[i]^(j-1)*coef[j])
    sum_xi = sum_xi + pol[j,i]
  }
  print(sum_xi)
  sum_pol[i] = sum(pol[,i])
  sum_xi=0
}
sum_pol[1:length(x)]
pol[,1]
pol[,2]
#sum_pol[1] = sum(pol[,1])
#sum_pol[2] = sum(pol[,2]) 

#for(i in 1:length(x)){print(i)} # 1 2
#for(j in 1:length(coef)){print(j)} #1 2 3
