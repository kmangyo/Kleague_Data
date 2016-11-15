library(rvest)
library(httr)
library(reshape2)
library(stringi)
library(stringr)
library(ggplot2)

#Getting data info in the web-site.
url<-'http://www.kleague.com/kr/sub.asp?avan=1008050000'
name<-read_html(url, encoding='euc-kr') %>% html_nodes("option") %>% html_text()
name<-iconv(name,"UTF-8", "CP949")

id<-read_html(url, encoding='euc-kr') %>% html_nodes("option")
id<-iconv(id,"UTF-8", "CP949")
name.id<-data.frame(cbind(name,id))

#Getting K league classic data
classic<-c('전북','수원','포항','서울','성남','제주','인천','울산','전남','광주','부산','대전')

classic<-c('전북','수원','포항','서울','제주','울산') #2015 
classic<-c('서울','전북','제주','울산','포항','수원') #2016

name.id<-subset(name.id, name %in% classic)
name.id$value<-stri_sub(name.id$id,16,18)
name.id<-name.id[c(-2)]

#Function for web scarping
kleague<-function(team.id) {
kleague<-'http://www.kleague.com/kr/sub.asp?avan=1008050000'
post <- list(
submit = "조회",
"iptTeamid" = as.character(team.id),
"iptMeetYear" = 2015 #or 2016
)
resp<-POST(kleague, body=post, encode="form")
resp_iconv<-iconv(resp,"UTF-8", "CP949")
resp_iconv<-read_html(resp_iconv)
data<-resp_iconv %>% html_nodes(xpath='//*[@id="content_s"]/div[2]/div[2]/table[1]')
club<-data %>% html_text() %>% iconv("UTF-8", "CP949")
return(club)
}
kteam<-lapply(name.id$value,kleague)

#Data cleaning
kteam_df<-gsub("\r\n\t\t\t\t\r\n\t\t\t\t\t\r\n\t\t\t\t\t\t성명\r\n\t\t\t\t\t\t출장\r\n\t\t\t\t\t\t교체\r\n\t\t\t\t\t\tGL\r\n\t\t\t\t\t\tAS\r\n\t\t\t\t\t\tGK\r\n\t\t\t\t\t\tCK\r\n\t\t\t\t\t\tFC\r\n\t\t\t\t\t\tOS\r\n\t\t\t\t\t\tST\r\n\t\t\t\t\t\tPK\r\n\r\n\t\t\t\t\t\t경고\r\n\t\t\t\t\t\t퇴장\r\n\t\t\t\t\t\t실점\r\n\t\t\t\t\t\t자책\r\n\t\t\t\t\t\r\n\t\t\t\t\t\r\n\t\t\t\t\t\tI\r\n\t\t\t\t\t\tO\r\n\t\t\t\t\t\t합\r\n\t\t\t\t\t\t전\r\n\t\t\t\t\t\t후\r\n\t\t\t\t\t\t연\r\n\t\t\t\t\t\t합\r\n\t\t\t\t\t\t득\r\n\t\t\t\t\t\t실\r\n\t\t\t\t\t\t%\r\n\t\t\t\t\t\r\n\t\t\t\t\r\n\r\n\t\r\n\t\t", "", kteam)
kteam_df<-strsplit(kteam_df,"\r\n\t\r\n\t\r\n\t\r\n\t\t")
kteam_df<-melt(kteam_df)
kteam_df <-cbind(kteam_df,data.frame(str_split_fixed(kteam_df$value, "\r\n\t\t", 22)))
kteam_df<-kteam_df[c(-1)]
names(kteam_df )<-c('team','name','App','Sub_i','Sub_o','Sub_t','GL_1','GL_2','GL_a','GL_t','AS','GK','CK','FC','OS','ST','PK_s','PK_l','PK_r','Y','R','L','M')
kteam_df$M<-gsub("\r\n\t\r\n\t\r\n", "", kteam_df$M)

name.id$team<-1
name.id$team<-cumsum(name.id$team)

kteam_df<-merge(kteam_df, name.id, c('team'), all.x=T)

#Player game appearances in Top 4 team
kteam_df$App<-as.numeric(as.character(kteam_df$App))

#2015 season
kteam_df_2015<-kteam_df
ggplot(subset(kteam_df_2015,name.y ==c('전북')|name.y ==c('수원')|name.y ==c('포항')|name.y ==c('서울'))) + geom_density(aes(x = App, colour = name.y)) + labs(x = c('경기출장'))

#2016 season
kteam_df_2016<-kteam_df
ggplot(subset(kteam_df_2016,name.y ==c('전북')|name.y ==c('서울')|name.y ==c('제주')|name.y ==c('울산'))) + geom_density(aes(x = App, colour = name.y)) + labs(x = c('경기출장'))

#2015/16 season
kteam_df_2016$season<-2016
kteam_df_2015$season<-2015
kteam_df_2016$id<-with(kteam_df_2016, paste0(season,"_",name.y))
kteam_df_2015$id<-with(kteam_df_2015, paste0(season,"_",name.y))
kteam_df_sum<-rbind(kteam_df_2015, kteam_df_2016)

#Viz
ggplot(subset(kteam_df_sum,id ==c('2015_서울')|id ==c('2016_서울')|id ==c('2015_전북')|id ==c('2016_전북')|id ==c('2015_포항')|id ==c('2015_수원')|id ==c('2016_제주')|id ==c('2016_울산'))) + geom_density(aes(x = App, colour = id)) + labs(x = c('경기출장')) + facet_wrap(~ season)
ggplot(subset(kteam_df_sum,id ==c('2015_서울')|id ==c('2016_서울')|id ==c('2015_전북')|id ==c('2016_전북')|id ==c('2015_포항')|id ==c('2015_수원')|id ==c('2016_제주')|id ==c('2016_울산')),aes(App, fill = id)) + geom_density(alpha = 0.2) + labs(x = c('경기출장')) + facet_wrap(~ season)

ggplot(subset(kteam_df_sum,id ==c('2015_서울')|id ==c('2016_서울')|id ==c('2015_전북')|id ==c('2016_전북')),aes(App, fill = id)) + geom_density(alpha = 0.5) + labs(x = c('경기출장')) + facet_wrap(~ name.y)
ggplot(subset(kteam_df_sum,id ==c('2015_수원')|id ==c('2016_수원')|id ==c('2015_포항')|id ==c('2016_포항')),aes(App, fill = id)) + geom_density(alpha = 0.5) + labs(x = c('경기출장')) + facet_wrap(~ name.y)
ggplot(subset(kteam_df_sum,id ==c('2015_제주')|id ==c('2016_제주')|id ==c('2015_울산')|id ==c('2016_울산')),aes(App, fill = id)) + geom_density(alpha = 0.5) + labs(x = c('경기출장')) + facet_wrap(~ name.y)

ggplot(subset(kteam_df_sum,id ==c('2016_서울')|id ==c('2016_전북')|id ==c('2016_수원')|id ==c('2016_포항')),aes(App, fill = id)) + geom_density(alpha = 0.3) + labs(x = c('경기출장')) + facet_wrap(~ season)

#Numeirc info.
kteam_df_sum %>% group_by(id) %>% summarise(count=n())
kteam_df_sum %>% group_by(id) %>% summarise(mean=mean(App),median=median(App))
