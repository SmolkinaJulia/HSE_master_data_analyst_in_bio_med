# hw 7 smolkina
library(plyr)
Sys.sleep(time)
data(mtcars)
dev.off()

m=rbind(c(1,1,2),
        c(1,1,3))
layout(m)
par(mar=c(2.5, 2.5, 1.5, 1), tcl = -0.2, mgp = c(1.5, 0.4, 0), bty = 'n', las=0)
############################ 1 ###################3
main = "#cylinders"
location = "topright"
labels = c(4, 6, 8)
cylcolor = c('4'= 'pink', '6'= 'yellow', '8' = 'grey')
plot(mtcars$disp, mtcars$mpg, pch=21,
     xlab='Displacement', ylab='Miles/(US) gallon'
     ,cex = 1.5
     ,bg = cylcolor[as.character(mtcars$cyl)]
     )
legend(location ,labels ,title = main
       ,legend =labels,pt.cex = 2,cex = 1.2
       ,pch =19,col = cols)
########################## 2 ####################################
#t = seq(0,2*pi,length.out =  100)
#polygon(30+sin(t)*10,30+cos(t)*10,col ='red',density = 30)
# d = data.frame()
# mpg = data.frame()
#  
# for(i in c(4,6,8)){
#   for(j in 1:3){
#     d[i,j] = mtcars$disp[mtcars$cyl == i][j]
#     
#   }
# 
# }



d_4 = mtcars$disp[mtcars$cyl == 4]
d_6 = mtcars$disp[mtcars$cyl == 6]
d_8 = mtcars$disp[mtcars$cyl == 8]
mpg_4 = mtcars$mpg[mtcars$cyl == 4]
mpg_6 = mtcars$mpg[mtcars$cyl == 6]
mpg_8 = mtcars$mpg[mtcars$cyl == 8]


elipsdraw = function(d, mgp,labels, ...) {
  x_max = (max(d) + min(d)) / 2
  x_min = (max(d) - min(d)) / 2
  y_max = (max(mgp) + min(mgp)) / 2
  y_min = (max(mgp) - min(mgp)) / 2
  t = seq(0, 2*pi, length.out = 100)
  x_min = sd(d) * 2
  y_min = sd(mgp) * 2
  polygon(x_max + sin(t) * x_min, y_max + cos(t)* y_min, ...)
  text(x_max, y_max, labels = labels, col = 'black', cex=1.0)
}
plot(1, xlim = c(70, 500), ylim = c(10, 38), 
     xlab='Displacement', ylab='Miles/(US) gallon')
elipsdraw (d_4, mpg_4, 4, col = 'pink')
elipsdraw (d_6, mpg_6, 6, col = 'yellow')
elipsdraw (d_8, mpg_8, 8, col = 'gray')
#################### 3 ###############################

carbs = as.numeric(names(table(mtcars$carb)))
means = unlist(lapply(c(1:6),
                      function(x) mean(mtcars$mpg[mtcars$carb == carbs[x]])))
sds = unlist(lapply(c(1:6),
                    function(x) sd(mtcars$mpg[mtcars$carb == carbs[x]])))
plot(carbs, means
     ,xlim = c(1, 8), ylim = c(8, 38)
     ,type='b', pch=19, cex=1.5, 
     xlab='# carburetors', ylab='Miles/(US) gallon'
     ,bty='n')
arrows(carbs, means + 2 * sds, carbs, means - 2 * sds,angle = 90
      #,length = 0.0
      ,code = 0
       ,xpd = F)





