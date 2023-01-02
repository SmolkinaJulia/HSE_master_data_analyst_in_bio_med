#practika 5 smolkina

#creation of x0 н0

#xrow = (-2) + c(0:999) / 333
#x0 = matrix(rep(xrow, 1000), ncol = 1000, byrow = T)
#ycol = (-1) + c(0:999) / 499.5
#y0 = matrix(rep(ycol, 1000), ncol = 1000)

dx = seq(from = -2, to = 1, by = 3/999)
dy = seq(from = -1, to = 1, by = 2/999)
x0 = matrix(dx, nrow = 1000, ncol = 1000, byrow = TRUE)
y0 = matrix(dy, nrow = 1000, ncol = 1000, byrow = FALSE)


x0[1:8,1:8]
y0[1:8,1:8]

#save x and y as x0 and y0
x = x0
x[1:8,1:8]
y = y0
y[1:8,1:8]


#x_safe = x
#x = x0^2-y0^2 + x0
#y = 2*x0 + y0

for(i in 1:21) {
  x_safe= x
  x = x^2-y^2 + x0
  y = 2*x_safe*y + y0
}

z = t(abs(x^2 +y^2))
z[!is.na(z)] = rank(z[!is.na(z)])
image(z^3,col = rev(terrain.colors(1000)))