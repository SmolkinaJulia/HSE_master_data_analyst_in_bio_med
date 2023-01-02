# HW 6 Smolkina
data(baseball, package="plyr") 
library(plyr)
library(dplyr)


######  1 ##################
#Отберите из baseball записли для команд у которых есть хотябы 200 записей.

count_write = function(df){
  
  df_new = data.frame()
  unique_names = c(unique(df$team))
  for( i in unique_names){
    sub = subset(baseball, team == i)
    sub_count = count(sub)
    sub_count = count(sub_count$freq)
    counted = sub_count$freq
    if(counted >=200){
      df_new = rbind(df_new,sub)
    }
  }
  return(df_new)
}

df_new = count_write(baseball)
team_200 = unique(df_new$team)
print('teams with more then 200 :')
print(team_200)

znamenatel = data.frame(team = character(),number_players_in_team = numeric())

for(i in team_200){
  sub_cur_team = subset(baseball, team == i)
  id_all = sub_cur_team$id
  uniq_id = unique(id_all)
  znamenatel[nrow(znamenatel)+1,'team'] = i
  znamenatel[nrow(znamenatel),'number_players_in_team'] = length(uniq_id)
}

znamenatel


##################  2 ##################

#Рассчитайте матрицу переходов игроков между командами — для каждой пары
#команд в этой матрице указано сколько есть игроков игравших в обоих командах.


##########
#tran = data.frame(team_1 = character(),
#                    team_2 = character(),
#                    number_mutual_players = numeric())
#trans_matrix = data.frame(team_1 = character(),
##                            team_2 = character(),
#                            number_mutual_players = numeric())
#inter =  data.frame(team_1 = character(),
#                     team_2 = character(),
#                     number_mutual_players = numeric())
#for(i in team_200){
#  df_team_1 = subset(baseball, team == i)
#  id_1 = unique(df_team_1$id)
#  for(j in id_1){
#    df_team_2 = subset(baseball, id == j & team != i)
#  }

#  number_players = count(df_team_2$team)
#  names(number_players)[names(number_players) == 'x'] <- 'team_2'
#  names(number_players)[names(number_players) == 'freq'] <- 'number_mutual_players'
#  for(k in 1:nrow(number_players)){
#    tran[nrow(tran)+1,'team_1'] = i
#  }
#  sub_i = subset(tran, team_1 == i)
#  sub_i$team_2 = number_players$team_2
#  sub_i$number_mutual_players = number_players$number_mutual_players
#  trans_matrix = rbind(trans_matrix,sub_i)
#}
#trans_matrix
#trans_matrix = trans_matrix[!duplicated(trans_matrix), ]
#trans_matrix
#################


df = data.frame(team_1 = character(),team_2= character()
                ,number_mutual_players = numeric())

for(i in 1:length(team_200)){
  team_1 = subset(baseball,team == team_200[i])
  id_1 = unique(team_1$id)
  for(j in (i+1):length(team_200)){
    team_2 = subset(baseball,team == team_200[j])
    id_2 = unique(team_2$id)
    t = intersect(id_1, id_2)
    #inter[length(inter)+1] = length(t)
    df[nrow(df)+1,'team_1'] = team_200[i]
    df[nrow(df),'team_2'] = team_200[j]
    df[nrow(df),'number_mutual_players'] = length(t)
  }
  
}
trans_matrix = df
trans_matrix
trans_matrix = trans_matrix[!duplicated(trans_matrix),]

#trans_matrix[trans_matrix$team_2 == "NA" ,]#& trans_matrix$team_2 == "SEA" ,3]
trans_matrix <-trans_matrix[-497, ]

x = matrix(0,nrow = 32,ncol=32,
           dimnames = list(c(unique(team_200))
                           ,c(unique(team_200))))

for(i in rownames(x)){
  for( j in colnames(x)){
    for(k in  1:length(trans_matrix[[1]])){
      if(trans_matrix$team_1[k] == i & trans_matrix$team_2[k] == j){
        curr = subset(trans_matrix,team_1 == i & team_2 == j)
        x[i,j] = curr$number_mutual_players
      }else{
        if(trans_matrix$team_1[k] == j & trans_matrix$team_2[k] == i){
          curr = subset(trans_matrix,team_1 == j & team_2 == i)
          x[i,j] = curr$number_mutual_players
        }
        
      }
      if(i == j){x[i,j] = 0}
    }
  }
}

x

#trans_matrix[trans_matrix$team_1 == "SEA" & trans_matrix$team_2 == "SEA" ,3]

#for(i in rownames(x)){
  for( j in colnames(x)){
    if(trans_matrix[trans_matrix$team_1 == i & trans_matrix$team_2 == j ,3] != 0){
      curr = subset(trans_matrix,team_1 == i & team_2 == j)
      x[i,j] = curr$number_mutual_players
      
    }
  }
}


dist_x = x
dist_x
############  3  ##############
#Поделите каждую ячейку полученной матрицы на размер (количество игроков
#которые есть в baseball) меньшей (из пары) команды.

for(i in rownames(x)){
  for( j in colnames(x)){
    if(dist_x[i,j] != 0){
      team_i = subset(znamenatel, team == i)
      team_j = subset(znamenatel, team == j)
      
      if(team_i$number_players_in_team >= team_j$number_players_in_team){
        dist_x[i,j] = x[i,j]/team_j$number_players_in_team
        dist_x[i,j] = round (dist_x[i,j], digits = 4)
      }else{
        dist_x[i,j] = x[i,j]/team_i$number_players_in_team
        dist_x[i,j] = round (dist_x[i,j], digits = 4)
      }
      
    }else{ dist_x[i,j] = round (dist_x[i,j], digits = 4)}
  }
}

dist_x

###### 1-dist_x ######

m = 1-dist_x
m

###### шкалирование и рисунок ######

mds = cmdscale(m,k=2)
mds
plot(mds,pch=19)
text(mds,rownames(mds),adj=c(1.2,1.2),col='red')
  
####### размер точек был пропорционален размеру команд #########

for(i in rownames(mds)){
  tocka = subset(znamenatel,team = i)
  tocka$number_players_in_team
  plot(mds,pch=19,
       cex=tocka$number_players_in_team/mean(tocka$number_players_in_team))
  text(mds,rownames(mds),adj=c(1.2,1.2),col='red')
}




