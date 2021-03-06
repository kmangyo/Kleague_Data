library(rvest)
library(httr)
library(reshape2)
library(stringi)
library(stringr)
library(ggplot2)

#k리그 팀 정보 페이지
url<-'http://www.kleague.com/KOR_2016/classic/clubIntro.asp'
team <- read_html(url) %>% html_nodes('a')

#k리그 팀 id 획득
team.id <- team[89:100]

team.id.txt <- list()
for(i in 1:length(team.id)){
  team.id.txt[i]<-as.character(team.id[i])
}

team.id.txt<-unlist(team.id.txt)
team.id.txt<-data.frame(team.id.txt)

team.id.txt<-data.frame(do.call('rbind', strsplit(as.character(team.id.txt$team.id.txt),'>',fixed=TRUE)))
team.id.txt<-team.id.txt[c(1)]
team.id.txt<-data.frame(do.call('rbind', strsplit(as.character(team.id.txt$X1),'title=',fixed=TRUE)))
names(team.id.txt)<-c('url','name')

team.id.txt$id<-str_sub(team.id.txt$url, -5,-3)

#k리그 팀 id 로 찾아들어간 url에서 선수 id 획득
url.team<-'http://www.kleague.com/KOR_2016/classic/clubIntro.asp?teamId='
url.team<-paste0(url.team, team.id.txt$id)

player.id <- list()
for (i in 1:length(url.team)) {
  player.id[[i]] <- read_html(url.team[i]) %>% html_nodes('a')
}

player.id.txt <- list()
for(i in 1:length(player.id)){
  player.id.txt[[i]]<-as.character(player.id[[i]])
}

player.id.txt<-data.frame(melt(player.id.txt))

player.id.txt$count<- str_count(player.id.txt$value, "player_id")
player.id.txt<-subset(player.id.txt, count>0)
player.id.txt.num<-data.frame(do.call('rbind', strsplit(as.character(player.id.txt$value),'>',fixed=TRUE)))
player.id.txt.num$id<-str_sub(player.id.txt.num$X1, -9,-2)

#k리그 선수 id로 선수 기록 획득
url.player<-'http://www.kleague.com/KOR_2016/classic/playerInfo.asp?player_id='
url.player<-paste0(url.player, player.id.txt.num$id)

name.player<-list()
bio.player<-list()
stats.player<-list()

for (i in 1:length(url.player)) {
  name.player[i]<-read_html(url.player[i]) %>% html_nodes('.name') %>% html_text()
  bio.player[i]<-read_html(url.player[i]) %>% html_nodes('.box2') %>% html_text()
  stats.player[i]<-read_html(url.player[i]) %>% html_nodes('table') %>% html_table()
}

bio.player.df<-data.frame(bio=unlist(bio.player))
name.player.df<-data.frame(name=unlist(name.player))
player.df<-cbind(name.player.df, bio.player.df)
player.df$id<-1
player.df$id<-cumsum(player.df$id)

player.df$bio <- gsub("\r\n\t\t\t", "", player.df$bio)
player.df.bio<-strsplit(as.character(player.df$bio),'\t\t') 
player.df.bio<-do.call(rbind, player.df.bio)
player.df.bio<-data.frame(player.df.bio)

#k리그 선수의 기본 정보 (팀, 포지션, 생년월일 등)
player.df.bio<-player.df.bio[c(3,6,9,12,15,18,21,25,29,33)]
#3 = team, 6 = position, 9 = number, 12 = nation, 15 = birth, 18 = height, 21 = weight, 25 = blood, 29 = debut, 33 = school
names(player.df.bio)<-c('team','pst','num','nation','birth','height','weight','blood','debut','school')
player.df<-cbind(player.df, player.df.bio)
player.df<-player.df[c(-2)]

#k리그 선수의 경기 기록 정보
stats.player.df<-rbind.fill(stats.player)
stats.player.df$id<-with(stats.player.df, ifelse(연도==c('계'),1,0))
stats.player.df$id<-cumsum(stats.player.df$id)

#k리그 선수의 개인 신상 정보와 기록정보 병합
stats.player.df_sum<-merge(stats.player.df, player.df, c('id'),all.x=T)
stats.player.df_sum<-stats.player.df_sum[complete.cases(stats.player.df_sum[,3]),]
stats.player.df_sum$birth.year<-str_sub(stats.player.df_sum$birth, 1,4)
stats.player.df_sum$age<-with(stats.player.df_sum, as.numeric(as.character(연도))-as.numeric(as.character(birth.year)))
stats.player.df_sum$age.game<-with(stats.player.df_sum, as.numeric(as.character(연도))-as.numeric(as.character(debut)))

names(stats.player.df_sum)[4]<-'game'

#나이와 게임 출장수의 관계
#k리그 데뷔 연차와 게임 출장수의 관계
#위의 관계에 대한 포지션별 차이 등
with(stats.player.df_sum, plot(age, game))
with(stats.player.df_sum, plot(age.game, game))
ggplot(data = stats.player.df_sum, aes(x = age, y = game, colour = pst)) + geom_point()
ggplot(data = subset(stats.player.df_sum, pst==c('FW')), aes(x = age, y = game)) + geom_point()

ggplot(data = stats.player.df_sum, aes(x = age.game, y = game, colour = pst)) + geom_point()
ggplot(data = subset(stats.player.df_sum, pst==c('FW')), aes(x = age.game, y = game)) + geom_point()

#2015시즌 데이터만 보기 (현재 등록선수 현황이므로 과거에 등록되었던 선수 정보는 없음)
ggplot(subset(stats.player.df_sum, 연도==2015), aes(factor(age), game)) + geom_boxplot() + ggtitle("Age VS. # of Games")
ggplot(subset(stats.player.df_sum, 연도==2015), aes(factor(age.game), game)) + geom_boxplot() +ggtitle("Game.age VS. # of Games")

