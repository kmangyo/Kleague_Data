# Install docker in Mac OS for RSelenium
# https://docs.docker.com/docker-for-mac/
# Follow the methods in the docs
# Relenuium packages docs are below.
vignette("RSelenium-docker", package = "RSelenium") 
vignette("RSelenium-basics", package = "RSelenium")

# https://hub.docker.com/r/selenium/node-firefox/tags/
# Game Info.(HT, Goal time, etc.) in K-League officail page 
# http://www.kleague.com/KOR_2016/classic/match_record.asp?meet_year=2016&meet_seq=1&gameId=223
library(RSelenium)
library(rvest)
library(dplyr)
library(reshape2)
library(stringi)
library(ggplot2)

# Activate the virtual server
remDr <- remoteDriver(port = 4445L)
remDr$open()
remDr$navigate("http://comments.sports.naver.com/template/vs.nhn?category=kleague&gameId=201611060509223#")

getsource <-remDr$getPageSource()

comment<- read_html(getsource[[1]]) %>% html_nodes(".cbox_desc") %>% html_text()
date<- read_html(getsource[[1]]) %>% html_nodes(".cbox_date") %>% html_text()
id<- read_html(getsource[[1]]) %>% html_nodes(".cbox_user_id") %>% html_text()
team<- read_html(getsource[[1]]) %>% html_nodes(".cbox_user_thumb")

# Comment web pages from 1p to 348p
# Create the element names and actions in order. This is a kind of two dimension matrix

seq_num<-seq(2,348,by=10)

seq_nums<-list()
for(i in 1:length(seq_num)){
  seq_nums[[i]]<-seq(seq_num[i],seq_num[i]+8)
}

for(i in 1:length(seq_nums)){
seq_nums[[i]][10]<-'cbox_next'
}

seq_name<-rep(c(rep("link text",9),'class name'),length(seq_nums))
seq_nums<-unlist(seq_nums)

seq_nums_df<-data.frame(seq_name,seq_nums)
seq_nums_df<-seq_nums_df[1:347,1:2]

comment_list<-list()
date_list<-list()
id_list<-list()
team_list<-list()

# Supporting team is pic. so, we have got image url.
for(i in 1:nrow(seq_nums_df)) {
  page_click <- remDr$findElement(using=as.character(seq_nums_df[i,1]), value=as.character(seq_nums_df[i,2]))
  page_click$clickElement()
  getsource <-page_click$getPageSource()

  comment_list[[i]]<- read_html(getsource[[1]]) %>% html_nodes(".cbox_desc") %>% html_text()
  date_list[[i]]<- read_html(getsource[[1]]) %>% html_nodes(".cbox_date") %>% html_text()
  id_list[[i]]<- read_html(getsource[[1]]) %>% html_nodes(".cbox_user_id") %>% html_text()
  team_list[[i]]<- read_html(getsource[[1]]) %>% html_nodes(".cbox_user_thumb")
}

comment_list<-melt(comment_list)
date_list<-melt(date_list)
id_list<-melt(id_list)

# Create the list element number in order. This is also a kind of two dimension matrix
team_num<-data.frame(len=rep(1:length(team_list), each=10),seq=rep(1:10,times=length(team_list)))

team_list_cha<-list()

for(i in 1:nrow(team_num)){
  team_list_cha[[i]] <-as.character(team_list[[team_num[i,1]]][team_num[i,2]])
}

team_list_cha<-melt(team_list_cha)

comment_df<-cbind(comment_list, date_list, id_list, team_list_cha)
comment_df<-comment_df[c(-2,-4,-6,-8)]
names(comment_df)<-c('comment','date','id','team')

# Getting supporting team name from URL
comment_df$team<-stri_sub(comment_df$team, -5, -4)

comment_df$date<-as.POSIXlt(comment_df$date)
comment_df$day<-comment_df$date$mday

# subset comments data in the game day (11/6)
comment_df<-subset(comment_df, day==6)

# subset comments data during the game (15:00~17:00)
comment_df$hour<-comment_df$date$hour
comment_df<-subset(comment_df, hour>=15&hour<17)

comment_df$min<-comment_df$date$min

# divided the time by 5 min.
comment_df$min<-floor(comment_df$min/5)*5

# some data manipulation works
comment_df$time<-with(comment_df, paste0(hour,c(':'),min))
comment_df$date<-as.character(comment_df$date)
comment_df_freq<-comment_df %>% group_by(time) %>% summarise(count=n())
comment_df_freq_team <- comment_df %>% group_by(team, time) %>% summarise(count=n())

# Viz the number of comments during the game time
theme_set(theme_gray(base_family='NanumGothic'))

comment_df_freq$time<- 
  with(comment_df_freq, ifelse(time==c('15:0'), c('15:00'),
                               ifelse(time==c('15:5'), c('15:05'),
                                      ifelse(time==c('16:5'), c('16:05'),
                                             ifelse(time==c('16:0'), c('16:00'),time)))))

ggplot(comment_df_freq, aes(x=as.factor(time), y=count)) + geom_bar(stat="identity") + geom_vline(xintercept = which(comment_df_freq$time == '15:50')) + geom_vline(xintercept = which(comment_df_freq$time == '16:10')) +geom_vline(xintercept = which(comment_df_freq$time == '16:20'),colour = "red") + xlab("Time") + ylab("Count") + ggtitle("Num. of comments during the Game")

comment_df_freq_team$time<- 
  with(comment_df_freq_team, ifelse(time==c('15:0'), c('15:00'),
                               ifelse(time==c('15:5'), c('15:05'),
                                      ifelse(time==c('16:5'), c('16:05'),
                                             ifelse(time==c('16:0'), c('16:00'),time)))))
ggplot(subset(comment_df_freq_team,team!=('기타')), aes(x=as.factor(time), y=count, fill=team)) + geom_bar(stat="identity",position=position_dodge()) + geom_vline(xintercept = which(comment_df_freq$time == '15:50')) + geom_vline(xintercept = which(comment_df_freq$time == '16:15')) +geom_vline(xintercept = which(comment_df_freq$time == '16:25'),colour = "red") + xlab("Time") + ylab("Count") + ggtitle("Num. of comments during the Game") + scale_fill_manual(values = c('#D55E00','#009E73') )  
