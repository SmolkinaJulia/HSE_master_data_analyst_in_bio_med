# practika2 Smolkina Julia

# Сгенирируйте случайное число, посчитайте его синус при помощи ряда Тейлора (вокруг х=0). 
# считать по формуле из задания ( ряд Тейлора)
# генерировать случайное х и считать его синус, для этого считать вектор из элементв ряда тейлора. 
# Потестировать разные верхние границы (10, 20 и тд). К от 0 до 20, x = rmorm, для факториала есть функция факториал

#ДЗ :runif()

#factorial(1:10)
#sum(1:10)
#cumsum(1:10) #  вектор чатсичных сумм 
#  наричовать частичный суммы  по х кол-во частичных сумм, по у значение суммы и линией (горизонтальной)
#  провести истинное значение sin(x)
#abline(h = 6, col = 'red') # h = Горизонтальная линия
#abline(h = sin(0.4), col = 'blue')

x = rnorm(1, mean = 4)
x
true_sin = cbind('x' = x,'sin(x)' = sin(x))
true_sin
# teylor
n = 25
k = 0:n
teylor = cumsum(((-1)^k)*((x^(2*k+1))/factorial(2*k+1)))
plot(teylor)
real_true_sin = cbind('teylor' = teylor,'sin(x)' = sin(x))
real_true_sin
plot(k,teylor,
     main = "plot teylor",
     xlab = "k",
     ylab = "teylor",
     xlim = c(-1,n+1),
     ylim = c(min(teylor)-0.1,max(teylor)+0.1))
abline(h = sin(x), col = 'blue')
