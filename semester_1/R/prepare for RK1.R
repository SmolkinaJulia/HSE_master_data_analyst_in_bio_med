# prepare for RK1
#  написать функцию, которая находит точку пересечения функции и оси х

# Это тожесамое что решать уравнение x^3-6x+2=0 тк у=0
#####################1

y = function(x,k=3,b=-3,n=1){
  y = k*(x**n) + b
  return(y)
}

acc =  0.1
left = -1.5
right = 4
count = 0
m = 1000000000
func_y = function(y,acc,l,r){
  while((r - l) > acc){
    if(sign(y(x = l)) != sign(y(x = r))){
      m = (l+r)/2
      count = count + 1
      if (y(m)+acc == 0 | y(m)-acc == 0){
        print('count=')
        print(count)
        print('left =')
        print(l)
        print('right =')
        print(r)
        print('x=')
        return(m)
        }
      else{ #два равных отрезка: [l,m]  [m,r]
          if(sign(y(x = l)) != sign(y(x = m))){
            count = count + 1
            r = m
            m = (l+r)/2
            }
          else{
            count = count + 1
            l = m
            m = (l+r)/2
            }
        }
    }
  }
  print('count=')
  print(count)
  print('left =')
  print(l)
  print('right =')
  print(r)
  print('x=')
  return(m)
}

func_y(y,acc,left,right)

########################2
#численно посчитать производную : заданная функция,диапазон значений и
#число точек в которых надо посчитать производную – параметры функции.
#y'= (fun(x[i])-fun(x[i-1]))/(x[i]-x[i-1])

y = function(x,k=1,b=0,n=1){
  y = k*(sin(x)**n) + b
  return(y)
}



left = 0
right = 8
n = 7               # number of points for counting
step = abs(right-left)/(n+1)
i = 0
dif = c()
m = left

differens = function(y,a = left,b = right,num = n){
  while(m!=b){
    dif[i] = (y(m)-y(a))/(m-a)
    a = m
    m = m + step
    i = i+1
  }
  
  return(dif)
}

differens(y)







