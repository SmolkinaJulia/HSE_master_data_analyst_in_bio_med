# praktika 8 smolkina
dev.off()

my.barplot = function(d,t,e,...){
  b = barplot(d)
  b = barplot(d,ylim = c(0,max(b)+1),...)
  segments(b,tab,b,tab+e)
  text(b,tab-0.2,LETTERS[1:5])
}
my.barplot(d=1:5,t=LETTERS[1:5],e=runif(5,max = 3),col='red',border=NA)
# значения усиков меняются от запуска к запуска из-за runif