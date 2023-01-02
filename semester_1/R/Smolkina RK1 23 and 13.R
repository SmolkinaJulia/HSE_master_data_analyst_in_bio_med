####### kontrolnaya 1 Smolkina 13,23

## 23
#Напишите функцию для генерации сиракузской последовательности (ni+1=ni/2 если ni
# четное и ni*3+1 если не четное) и проверьте при помощи нее гипотезу Коллатца (что все
#такие ряды сходятся к 1) для чисел от 1 до 10000. Нарисуйте зависимость длины ряда
#додостижения единицы от начального значения.

ruad = list()
n_0_val = c(19:28)
steps = c()

sirakuz = function(n_0){

  if(n_0 < 2 | n_0 > 10000){
    return('choose n_0 again')
  }
  else{
    ruad[1] = n_0
    i = 1
    while(ruad[[i]] != 1){
      if((ruad[[i]] %% 2) == 0){ #если четно то
        ruad[i+1] = ruad[[i]]/2
      }else{
        ruad[i+1] = ruad[[i]]*3 + 1
      } 
      i = i+1
    }
  }
  return(i-1)
  #return(c(steps,(i-1)))
}

for (j in n_0_val){
  steps = append(steps,sirakuz(n_0 = j))
}
steps

plot(steps,n_0_val
     ,type = 'p'
     ,main = 'dependence steps on n_0_val'
     ,xlab = 'steps'
     ,ylab = 'n_0')

########## 13
#Напишите функцию моделирующую движение грузика подвешенного на пружинке в
#невесомости. На грузик действует сила упругости пружинки (-x*k, где x — смещение а k
# — коэффициент упругости) и сила сопротивления среды (-v*f, где v — скорость, а f —                                                                             коэффициент сопротивления среды). Функция моделирует движение временными шагами
#постоянной протяжённости t. Функция возвращает положение и скорость в каждый
#момент времени. Начальное смещение x, k, f, t и n — число шагов — параметры
#функции. Нарисуйте зависимость положения груза от времени.
 
dvij = function(x, k, f, t, n) {
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
m =0.5
num = 200
res = dvij(c(3, 10, 25), 1, 2, m, num)

plot(1:(num+1)*m, res$pos[,1] , type='l'
     , xlab='time iteration'
     , ylab='position x'
     ,main = 'pos on time')
lines(1:(num+1)*m, res$pos[,2] , type='l', col="red")
lines(1:(num+1)*m, res$pos[,3], type='l', col="blue")

legend("bottomleft", 
       legend = c("x0=3", "x0=10", "x0=25"), 
       col = c('black', 'red', 'blue') , 
       pch = c(10,10,10), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

