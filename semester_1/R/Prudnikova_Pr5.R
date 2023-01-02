#1
dx = seq(from = -2, to = 1, by = 3/999)
dy = seq(from = -1, to = 1, by = 2/999)
x0 = matrix(dx, nrow = 1000, ncol = 1000, byrow = TRUE)
y0 = matrix(dy, nrow = 1000, ncol = 1000, byrow = FALSE)

#2
x = x0
y = y0

#3-4
for(i in 1:20) {
  x_ = x
  x = x^2-y^2 + x0
  y = 2*x_*y + y0
}

#5
z = t(abs(x^2 + y^2))
z[!is.na(z)] = rank(z[!is.na(z)])
image(z^3,col=rev(terrain.colors(1000)))

