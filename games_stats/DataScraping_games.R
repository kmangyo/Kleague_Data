library(rvest)
library(reshape2)
library(ggplot2)

url<-'http://www.kleague.com/KOR_2016/classic/result_detail.asp?gameId='

game.num<-c(1:174)

url<-paste0(url, game.num)

url_html<-list()
for (i in 1:length(url)){
  url_html[[i]]<-read_html(url[i])
}

round<-list()
audi<-list()
dateLoc<-list()
club<-list()
score<-list()
goal.time<-list()
goal.player<-list()

for (i in 1:length(url)){
  round[[i]] <- url_html[[i]] %>% html_nodes(".reviewTeamRound p") %>% html_text()
  audi[[i]] <- url_html[[i]] %>% html_nodes(".audi") %>% html_text()
  dateLoc[[i]] <- url_html[[i]] %>% html_nodes(".dateLoc") %>% html_text()
  club[[i]] <- url_html[[i]] %>% html_nodes(".club") %>% html_text()
  score[[i]] <- url_html[[i]] %>% html_nodes(".score") %>% html_text()
  goal.time[[i]] <- url_html[[i]] %>% html_nodes(".reviewGoalTime") %>% html_text()
  goal.player[[i]] <- url_html[[i]] %>% html_nodes(".reviewGoalPlayer") %>% html_text()
}

round<-melt(round)
audi<-melt(audi)
dateLoc<-melt(dateLoc)
club<-melt(club)
score<-melt(score)
goal.time<-melt(goal.time)
goal.player<-melt(goal.player)

club$seq<-1
club$seq<-with(club, ave(seq,L1,FUN=cumsum))
club<-subset(club, seq<=2)

club$team<-with(club, ifelse(seq==1, c('home'), c('away')))

names(round)[1]<-'round'
names(audi)[1]<-'audi'
names(dateLoc)[1]<-'location'
names(club)[1]<-'team.name'
names(score)[1]<-'score'
names(goal.time)[1]<-'goal.time'
names(goal.player)[1]<-'goal.player'

#round= the number of round
#audi = the number of audience in the game
#location = location date, and time of the game
#team.name = the name of hone team
#score = the score of game (home:away)
#goal.time = the goal time in the game
#goal.player = the player of the goal in the game

goal.player$seq<-1
goal.player$seq<-with(goal.player, ave(seq,L1,FUN=cumsum))
goal.player$team<-goal.player$seq %% 2
goal.player$team<-with(goal.player, ifelse(team==1,c('home'),c('away')))
goal.player$goal.player<-as.character(goal.player$goal.player)
goal.player$nchar<-nchar(goal.player$goal.player)
goal.player<-subset(goal.player, nchar>1)
goal<-cbind(goal.player, goal.time)
goal$goal.time<-as.numeric(as.character(goal$goal.time))

club<-reshape(club, idvar="L1", timevar="team", direction="wide")
club<-club[c(-3,-5)]

game.df<-merge(round, audi, c('L1'))
game.df<-merge(game.df, dateLoc, c('L1'))
game.df<-merge(game.df, club, c('L1'))
game.df<-merge(game.df, score, c('L1'))

game.df$audi<-gsub("\r", "", game.df$audi)
game.df$audi<-gsub("\n", "", game.df$audi)
game.df$audi<-gsub("\t", "", game.df$audi)
game.df$audi<-gsub(",", "", game.df$audi)

game.df$audi<-as.numeric(as.character(game.df$audi))
