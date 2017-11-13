library(RSelenium)
library(rvest)
library(plyr)
library(dplyr)
library(reshape2)
library(stringi)
library(stringr)
library(ggplot2)

# comments data is from http://sports.news.naver.com/gameCenter/textRelayFootball.nhn?category=amatch&tab=player_stats&gameId=20171010A01A001618

# in mac terminal: docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
rD <- rsDriver(port=4444L,browser="chrome")
remDr <- rD$client

remDr$open()
remDr$navigate("http://comments.sports.naver.com/template/vs.nhn?category=amatch&gameId=20171010A01A001618")

# over 50k comments
seq_num<-seq(2,5827,by=10)

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

comment_list<-list()
date_list<-list()
id_list<-list()

for(i in 1:nrow(seq_nums_df)) {
  page_click <- remDr$findElement(using=as.character(seq_nums_df[i,1]), value=as.character(seq_nums_df[i,2]))
  page_click$clickElement()
  getsource <-page_click$getPageSource()
  
  comment_list[[i]]<- read_html(getsource[[1]]) %>% html_nodes(".cbox_desc") %>% html_text()
  date_list[[i]]<- read_html(getsource[[1]]) %>% html_nodes(".cbox_date") %>% html_text()
  id_list[[i]]<- read_html(getsource[[1]]) %>% html_nodes(".cbox_user_id") %>% html_text()
}

comment_df<-melt(comment_list)
date_df<-melt(date_list)
id_df<-melt(id_list)

cmt_df<-cbind(comment_df, date_df, id_df)
cmt_df<-cmt_df[c(1,3,5)]
names(cmt_df)<-c('cmt','time','id')

cmt_df$time<-as.POSIXlt(as.character(cmt_df$time))

# during game
cmt_df$num.time<-as.numeric(cmt_df$time)
cmt_df<-subset(cmt_df, num.time>=1507642200 & num.time<=1507649400)

ggplot(data=cmt_df, aes(time)) + geom_histogram(bins=120)

# using google semantic analysis API
# install.packages('googleLanguageR')
# devtools::install_github("ropensci/googleLanguageR")
library(googleLanguageR)

# random sampling 1k
cmt_df_sample <- sample_n(cmt_df, 1000)
hist(cmt_df_sample$time, breaks = 120)

cmt_df_sample$cmt<-as.character(cmt_df_sample$cmt)
cmt_df_sample$cmt<- gsub("ㄱ|ㄴ|ㄷ|ㄹ|ㅁ|ㅂ|ㅅ|ㅇ|ㅈ|ㅊ|ㅋ|ㅌ|ㅍ|ㅎ|ㅏ|ㅑ|ㅓ|ㅕ|ㅗ|ㅛ|ㅜ|ㅠ|ㅡ|ㅣ|ㅃ|ㅉ|ㄲ|ㅆ|\n|\t", " ", cmt_df_sample$cmt)

texts <- c(cmt_df_sample$cmt)
nlp_result <- gl_nlp(texts, language = "ko")

for (i in 1:length(nlp_result$sentences)){
  nlp_result$sentences[[i]]$seq<-i
}

nlp_result_sent<-ldply(nlp_result$sentences, data.frame)

cmt_df_sample$seq<-1
cmt_df_sample$seq<-cumsum(cmt_df_sample$seq)
cmt_df_sample<- cmt_df_sample %>% arrange(num.time)

cmt_df_sample$time<-as.POSIXct(cmt_df_sample$time)
cmt_df_sample<-left_join(cmt_df_sample, nlp_result_sent, c('seq'))
cmt_df_sample$moroco<-str_count(cmt_df_sample$content, "모로코")
cmt_df_sample<-subset(cmt_df_sample, moroco==0)

hist(cmt_df_sample$score, breaks = 20)
mean(cmt_df_sample$score)
ggplot(data=cmt_df_sample, aes(score)) + geom_histogram(bins=20)

# by time
ggplot(cmt_df_sample, aes(time, score)) + geom_point() + geom_smooth()

mav <- function(x,n){stats::filter(x,rep(1/n,n), sides=2)}
# ref) https://druedin.com/2012/08/11/moving-averages-in-r/

mav_df<-mav(cmt_df_sample$score,100)
mav_df<-data.frame(mav_df)
mav_df$seq <-c(1:nrow(mav_df))
names(mav_df)[1]<-'score'
ggplot(data=mav_df, aes(x=seq, y=score, group=1)) + geom_line() + geom_path()
