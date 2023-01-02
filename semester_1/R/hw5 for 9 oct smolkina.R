# hw 5 smolkina for 9 oct

#1
vector_test = c(1,2,3,4,5,6,7)
n = 3
find_n_max = function(n,vector){
  for(i in 1:(n-1)){ 
    max_el = (which.max(vector))
    vector = vector[-max_el]
  }
  max_el = which.max(vector)
  return(max_el)
}
find_n_max(n,vector_test)

#2

require(ggplot2)

#### as function ######

plot_mendelbrot = function(n,di_x,di_y,size){
  size$d <- complex(re = size$dre, im = size$dim)
  size$z <- 0
  for (k in 1:n) size$z <- size$z^2 + size$d
  size$h <- exp(-abs(size$z))
  qplot(x = dre,y = dim, fill = h, data = size
        , geom = "tile"
        , xlim = c(-2, 1)
        , ylim = c(-1.3, 1.3)
         ,main = 'mendelbrot'
        , xlab = 'dre'
        , ylab = 'dim'
        )
}

plot_mendelbrot(n = 20, di_x=seq(from = -2, to = 1, by = 3/999)
                ,di_y=seq(from = -1, to = 1, by = 2/999),
                size=expand.grid(dre = seq(from = -2, to = 1, by = 3/999), dim = seq(from = -1, to = 1, by = 2/999)))






