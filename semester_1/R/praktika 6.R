# praktika 6 smolkina 30.10
#Постройте график где по оси х отложено время, по оси у — команды. Команды отсортированы по
#самой ранней дате наблюдения. Для каждой команды нарисуйте горизонтальную указывающую
#диапазон времен для которого у этой команды есть данные. Толщина и цвет линии должны быть
#соответствовать количеству записей у данной команды.
data(baseball, package="plyr") 
library(plyr)

team <- baseball$team
year <- baseball$year
df <- data.frame(team, year)
df
unique_names = c(unique(df$team))
list_of_df = list()
list_of_df = split(df,df$team)

#############################
new_ = function(df) {
  k = list(num.records=nrow(df),
    first.year=min(df$year),
    last.year=max(df$year))
  k = as.data.frame(k)
}
data_ = lapply(list_of_df, new_)
data_ = do.call(rbind, data_)
data_[,1]
data_

sorteddata = data_[order(data_[,2], data_[,3]),]
sorteddata 

##########"The oldest team still rstill existing in 2005 is"
print(rownames(sorteddata [sorteddata [,3] > 2004,])[1])     
###########################

plot(8
     ,ylim = c(1,nrow(data_))
     ,xlim = c(min(data_[2]),max(data_[3])) 
     ,xlab = 'Year'
     ,ylab = 'Team'
    ,type = 'l'
    ,bty='n'
    )
for (i in 1:length(sorteddata[,1])) {
  norm = sorteddata[i,1] / max(sorteddata[,1])
  segments(col=rgb(norm, 1-norm, 0.3),sorteddata[i,2], i, sorteddata[i,3], i,
           lwd = 6 * norm) 
}




