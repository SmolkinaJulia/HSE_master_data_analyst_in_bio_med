# HW 6 Smolkina
data(baseball, package="plyr") 
library(plyr)

######  1 ##################
#Отберите из baseball записли для команд у которых есть хотябы 200 записей.

#subset(mtcars, cyl == 6 & hp > 110)
i = subset(baseball, team == 'DTN' )#34
sub = subset(baseball, team == 'DTN')
#typeof(sub)
#unique(sub$team)
#sub$team
sub_count = count(i)
sub_count
sub_count = count(i_count$freq)
sub_count
counted = sub_count$freq
counted
#dataset <- data.frame(month = character(),
#                      temperature = numeric(),
#                      rain = numeric(),
#                      humidity = numeric())
#dataset
#df_new = data.frame(id = character(),
#                    year = numeric(),
#                    team = character(),
#                    lg = numeric(),
#                    g = numeric())
#df_new = data.frame()
#df_new = rbind(df_new,sub)

baseball
num_of_players = data.frame(team = character(),
                  num_of_players = numeric())



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
      num_of_players[i] <<- count(sub$id)
    }
  }
  return(df_new)
}

df_new = count_write(baseball)
team_200 = unique(df_new$team)
team_200
num_of_players

############### 2
# "CHN" "BSN" "NY1" "PHI" "PIT" "BRO" "CIN" "SLN" "BOS" "PHA" "DET" "CHA" "CLE" "WS1"
#  "SLA" "NYA" "BAL" "SFN" "LAN" "MIN" "NYN" "HOU" "CAL" "ATL" "OAK" "KCA" "MON" "SDN"
#  "ML4" "TEX" "TOR" "SEA"

tran = data.frame(team_1 = character(),
                          team_2 = character(),
                          number_mutual_players = numeric())

trans_matrix = data.frame(team_1 = character(),
                          team_2 = character(),
                          number_mutual_players = numeric())
tran
#i = 'MIN'
i = 'BSN'

df_team_1 = subset(baseball, team == i)
#df_team_1
id_1 = df_team_1$id
#id_1
for(j in id_1){
  #df_team_2 = rbind(df_team_2,subset(baseball, id == j & team != df_team_1$team ))
  df_team_2 = subset(baseball, id == j & team != df_team_1$team)
}
df_team_2
df_team_2$team
count(df_team_2,team)
#number_players = count(df_team_2$team)
number_players = count(df_team_2$team)
number_players
names(number_players)
names(number_players)[names(number_players) == 'x'] <- 'team_2'
names(number_players)[names(number_players) == 'freq'] <- 'number_mutual_players'
number_players


for(k in 1:nrow(number_players)){
  tran[nrow(tran)+1,'team_1'] = i
}

tran

tran[,c('team_1','team_2')]

sub_i = subset(tran, team_1 == i)
sub_i$team_2 = number_players$team_2
sub_i$number_mutual_players = number_players$number_mutual_players

sub_i

trans_matrix = rbind(trans_matrix,sub_i)
trans_matrix
####################################33
for(i in team_200){
  df_team_1 = subset(baseball, team == i)

  id_1 = df_team_1$id

  for(j in id_1){

    df_team_2 = subset(baseball, id == j & team != df_team_1$team)
  }
  number_players = count(df_team_2,team)
  names(number_players)[names(number_players) == 'x'] <- 'team_2'
  names(number_players)[names(number_players) == 'n'] <- 'number_mutual_players'
  for(k in 1:nrow(number_players)){
    tran[nrow(tran)+1,'team_1'] = i
  }
  sub_i = subset(tran, team_1 == i)
  sub_i$team_2 = number_players$team_2
  sub_i$number_mutual_players = number_players$number_mutual_players
  trans_matrix = rbind(trans_matrix,sub_i)
}

trans_matrix
duplicated(trans_matrix)
trans_matrix = trans_matrix[!duplicated(trans_matrix), ]
trans_matrix


count(baseball[1:100,], vars = "id")
count(baseball[1:100,], vars = "id", wt_var = "team")
count(baseball[1:100,], c("id", "year"))



#       ,dimnames = list(c("CHN", "BSN", "NY1", "PHI", "PIT"
#                          ,"BRO" ,"CIN" ,"SLN", "BOS" ,"PHA" ,"DET" ,"CHA"
#                          ,"CLE", "WS1", "SLA", "NYA", "BAL" ,"SFN", "LAN"
#                          ,"MIN" ,"NYN", "HOU", "CAL"),
#                            ,"MIN", "SFN" ,"CHN", "HOU" ,"KCA" ,"ATL" ,"CLE" ,"LAN"
#                            ,"TBA" ,"KC1","PHA", "WS1", "BAL" ,"ANA","CAL" ,"ARI" 
#                             ,"COL", "MIL", "SLN" ,"TEX", "TOR"
#                            ,"WAS" ,"BRO", "SDN" ,"SEA" ,"ML4")))




#col_names = unique(trans_matrix$team_1)
#col_names
#row_names = unique(trans_matrix$team_2)
#row_names
matrix(1:32, nrow = length(team_200), ncol = length(team_200), byrow=TRUE
       ,dimnames = list(c("CHN", "BSN" ,"NY1", "PHI", "PIT", "BRO" ,"CIN"
                          ,"SLN","BOS", "PHA", "DET" ,"CHA", "CLE"
                          ,"WS1", "SLA", "NYA", "BAL" ,"SFN" ,"LAN"
                          ,"MIN", "NYN","HOU", "CAL" ,"ATL" ,"OAK"
                          ,"KCA" ,"MON" ,"SDN", "ML4", "TEX" ,"TOR" ,"SEA"),
                        c("CHN", "BSN" ,"NY1", "PHI", "PIT", "BRO" ,"CIN"
                          ,"SLN","BOS", "PHA", "DET" ,"CHA", "CLE"
                          ,"WS1", "SLA", "NYA", "BAL" ,"SFN" ,"LAN"
                          ,"MIN", "NYN","HOU", "CAL" ,"ATL" ,"OAK"
                          ,"KCA" ,"MON" ,"SDN", "ML4", "TEX" ,"TOR" ,"SEA")))

name = c("CHN", "BSN" ,"NY1", "PHI", "PIT", "BRO" ,"CIN"
         ,"SLN","BOS", "PHA", "DET" ,"CHA", "CLE"
         ,"WS1", "SLA", "NYA", "BAL" ,"SFN" ,"LAN"
         ,"MIN", "NYN","HOU", "CAL" ,"ATL" ,"OAK"
         ,"KCA" ,"MON" ,"SDN", "ML4", "TEX" ,"TOR" ,"SEA")


matrix[1:3]

matrix(1:9, nrow=3, byrow=TRUE)    # fill matrix row-wise
matrix[,1]
