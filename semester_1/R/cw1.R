# 13 
get_poisiton_dynamics = function(x, k, f, t, n) {
  v = rep(0 , length(x))
  result = list(speed=matrix(v, ncol=length(x)), pos=matrix(x, ncol=length(x)))
  for(i in 1:n){
    f_result = -x*k - v*f
    x = x + v*t + f_result*(t**2)/2
    v = v + f_result*t
    result$speed = rbind(result$speed, v)
    result$pos = rbind(result$pos, x)
  }
  return(result)
}

res = get_poisiton_dynamics(c(10, 2, 1), 1, 2, 0.1, 100)

plot(1:101*0.1, res$pos[,1] , type='l', xlab='time iteration', ylab='position x')
lines(1:101*0.1, res$pos[,2] , type='l', col="red")
lines(1:101*0.1, res$pos[,3], type='l', col="green")

legend("bottomleft", 
       legend = c("x0=10", "x0=2", "x0=1"), 
       col = c('black', 'red', 'green') , 
       pch = c(19,19, 19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

# 15


generate_prime_numbers = function(n) {
  primes = c(2)
  for(number in 3:n){
    if(all( number%%primes != 0)){
      primes = c(primes, number)
    }
  }
  return(primes)
}

generate_prime_numbers(3)
generate_prime_numbers(4)
generate_prime_numbers(5)

generate_prime_numbers(100)
