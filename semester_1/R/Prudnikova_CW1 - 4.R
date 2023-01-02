#4 Функция, которая сравнивает набор слов одинаковой длины и возвращает матрицу попарных
#расстояний - долю несовпадающих букв

words = function(sample_words){
  mx <- matrix(nrow = length(sample_words), ncol = length(sample_words))
  for (i in 1:length(sample_words)) { 
    for (j in 1:length(sample_words)) { 
      mx[i, j] <- adist(sample_words[i], sample_words[j])/nchar(sample_words)[i]
    }
  }
  return(mx)
}

a = c('micky', 'micke', 'mocke', 'mocce', 'apple', 'carle')
words(a)  


#8 Функцию, получающую на вход квадратное уравнение в текстовом виде и возвращает его корни и экстремум

gregexpr('[\d\.]*', s)
#ax2+bx+c
solveQ = function(eq) {
  a = gregexpr(#pattern,eq)
  b = #pattern for b
  c = #pattern for c
  d = (b^2 - 4*a*c)^(1/2)
    if d < 0 {
      return('no roots', c(1:3))
    } 
    elif d = 0 {
      roots = -b/(2*a)
      extremum = -b/(2*a)
      return(roots, extremum)
    }
    elif d > 0 {
      roots = c()
      roots = append(roots, (-b + d)/(2*a))
      roots = append(roots, (-b - d)/(2*a))
      extremum = -b/(2*a)
      return(roots, extr)
    }
   
}
  
s = "10.3+16x-18x2"
solveQ(s)
