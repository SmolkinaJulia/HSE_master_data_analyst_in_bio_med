# praktika 7 smolkina

p = rpois(1:10000, 100)
br = seq(min(p)-1
         ,max(p)+1,by = 2)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

bgr = colorRamp(c('white','gray','black'))
d = density(p)
h = hist(p,br,xpd = F)
count = h$counts
p_norm = (p-min(p))/(max(p)-min(p))
cols = apply(bgr(p_norm),1,function(x)rgb(x[1],x[2],x[3],maxColorValue =  255))
cols = rgb(1 - count/max(count), 1 - count/max(count), 1 - count/max(count), maxColorValue = 1)

hist(p, breaks = br, freq = FALSE,
     xlab = "Переменная X",
     ylab = "Плотность вероятности",
     main = "Гистограмма, совмещенная с кривой плотности",
     #lwd = c*4+0.4,
     col = cols)
lines(d, col = "red", lwd = 2)
abline(v = mean(p),lty=3,col = "green")
abline(v = median(p),lty=3,col = "orange")
abline(v = getmode(p),lty=3,col = "violet")

