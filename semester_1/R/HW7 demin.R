dev.off()
m=rbind(c(1,1,2),
        c(1,1,3))
layout(m)

par(mar=c(2.5, 2.5, 1.5, 1), tcl = -0.2, mgp = c(1.5, 0.4, 0), bty = 'n', las=0)

#first
pink.d = rgb(228, 39, 137, maxColorValue = 255)
orange.d = rgb(226, 171, 1, maxColorValue = 255)
grey.d = rgb(100, 100, 100, maxColorValue = 255)
cylcolor = c('4'=pink.d, '6'=orange.d, '8' = grey.d)
plot(mtcars$disp, mtcars$mpg, bg = cylcolor[as.character(mtcars$cyl)], pch=21,
     xlab='Displacement', ylab='Miles/(US) gallon', cex = 2.5)

legend('topright', title = '# of cylinders', col=cylcolor, pch=19, legend=c(4, 6, 8),
       pt.cex = 2,  cex = 1.2)

plot.cyl = function(data.h, data.v, n, ...) {
  x = (max(data.h) + min(data.h)) / 2
  x.r = (max(data.h) - min(data.h)) / 2
  y = (max(data.v) + min(data.v)) / 2
  y.r = (max(data.v) - min(data.v)) / 2
  t = seq(0, 2*pi, length.out = 100)
  # distances = (data.h - x)** 2 + (data.v - y)** 2
  # furthest = which.max(distances) # index
  # 
  # k = sqrt((data.h[furthest] - x) ** 2 / x.r ** 2 + (data.v[furthest] - y) ** 2 / y.r ** 2)
  # x.r = k * x.r
  # y.r = k * y.r
  x.r = sd(data.h) * 2
  y.r = sd(data.v) * 2
  polygon(x + sin(t) * x.r, y + cos(t)* y.r, ...)
  text(x, y, labels = n, col='black', cex=2.2)
}

first.d = mtcars$disp[mtcars$cyl == 4]
second.d = mtcars$disp[mtcars$cyl == 6]
third.d = mtcars$disp[mtcars$cyl == 8]
first.mpg = mtcars$mpg[mtcars$cyl == 4]
second.mpg = mtcars$mpg[mtcars$cyl == 6]
third.mpg = mtcars$mpg[mtcars$cyl == 8]

#second
plot(1, xlim = c(70, 500), ylim = c(10, 38), 
     xlab='Displacement', ylab='Miles/(US) gallon')
pink.t = rgb(241, 134, 188, 180, maxColorValue = 255)
plot.cyl(first.d, first.mpg, 4, col = pink.t)
orang.t = rgb(255, 165, 0, 160, maxColorValue = 255)
plot.cyl(second.d, second.mpg, 6, col = orang.t)
grey.t = rgb(128, 128, 128, 128, maxColorValue = 255)
plot.cyl(third.d, third.mpg, 8, col = grey.t)

#third
carbs = as.numeric(names(table(mtcars$carb)))
means = unlist(lapply(c(1:6),
                      function(x) mean(mtcars$mpg[mtcars$carb == carbs[x]])))
sds = unlist(lapply(c(1:6),
                    function(x) sd(mtcars$mpg[mtcars$carb == carbs[x]])))
plot(carbs, means,xlim = c(1, 8), ylim = c(8, 38),
     type='b', pch=19, cex=1.5, 
     xlab='# carburetors', ylab='Miles/(US) gallon')

segments(carbs, means + 2 * sds, carbs, means - 2 * sds, xpd = T)

