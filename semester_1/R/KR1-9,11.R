#9

add.value = function(m, y, x, v, a) {
  answer = m
  for (i in 1:ncol(m)) {
    for (j in 1:nrow(m)) {
      answer[(i - 1) * ncol(m) + j] = m[i * ncol(m) + j] + v / (1 + (i - y)^2 + (j - x)^2) ** a
    }
  }
  answer = matrix(answer, ncol = ncol(m), byrow = T)
}

n = 100
fill = 0
matr = matrix(rep(rep(fill, n), n), ncol = n, byrow = T)
v = -100
a = 0.1
matr = add.value(matr, 10, 10, v, a)
matr = add.value(matr, 90, 90, v, a)
matr = add.value(matr, 60, 20, v, a)
matr = add.value(matr, 30, 70, v, a)
matr = add.value(matr, 20, 10, v, a)
image(matr)

#11

integral = function(fun, from = 0, to = 1, diff = 1e-10, max.iter = 1e4) {
  beg = from + c(0:1) * (to - from) / 2
  end = from + c(1:2) * (to - from) / 2
  old_square = sum((fun(beg) + fun(end)) * (to - from) / (2 * length(beg)))
  for (i in c(2:1e4)) {
    beg = from + c(0:(2**i - 1)) * (to - from) / 2**i
    end = from + c(0:(2**i - 1) + 1) * (to - from) / 2**i
    square = sum((fun(beg) + fun(end)) * (to - from) / (2 * length(beg)))
    if (abs(square - old_square) < diff) {
      break
    }
    old_square = square
  }
  return(square)
}

max.iter = 500
diff = 1e-8
integral(function(x){x^2+x^3},from=0,to=1,diff=diff,max.iter=max.iter)
integrate(function(x){x^2+x^3}, 0, 1)
