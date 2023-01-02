x = runif(1, 1, 8)

k = c(0:10)
k

elements = (-1)^k * x^(2*k+1)/factorial(2*k+1)
elements

result = cumsum(elements[1:11])
par(mfrow=c(1,2))
plot(k, result, xlab = 'k', ylab = 'part_sum')
abline(h = sin(x), col = 'red')

plot(k, log(abs((result)-sin(x))))

