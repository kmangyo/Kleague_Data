# 데이터는 케이리그 홈페이지에 있는 관중데이터를 활용, 2016시즌 1~29라운드.

library(ggplot2)
library(MASS)

# 맥에서 한글 셋팅 
theme_set(theme_gray(base_family='NanumGothic'))

# K리그 구단의 평균 관중 수
audi.mean <- game.df %>% group_by(team.name.home) %>% summarise(audi.mean=mean(audi)) %>% arrange(-audi.mean)
ggplot (audi.mean, aes(x = factor(team.name.home), y = audi.mean)) + geom_bar(stat = "identity") +  ggtitle('K리그 평균 관중') + xlab('팀') + ylab('관중 수')

# 평균 관중수 순서로 홈팀 순서를 조정
game.df$team.name.home<-factor(factor(game.df$team.name.home), levels = audi.mean$team.name.home)

# K리그 관중수 추이, 관중수가 15,000명이 넘는 경우 상대팀 표기
ggplot (game.df, aes(x = factor(round), y = audi, label=team.name.away)) 
  + geom_bar(stat = "identity") 
  + geom_text(aes(label=ifelse(audi>15000, team.name.away,''),family="NanumGothic"),size=2,hjust=0,vjust=0,angle=45) 
  + facet_wrap(~ team.name.home) 
  + ggtitle('K리그 관중 추이') + xlab('라운드') + ylab('관중 수')

# K리그 관중수 추이, 주말과 주중 색으로 나눠서 보기
ggplot(game.df, aes(x = factor(round), y = audi, fill=as.factor(weekend))) 
  + geom_bar(stat = "identity") 
  + facet_wrap(~ team.name.home) 
  + theme(legend.position="bottom", axis.text=element_text(size=7)) 
  + ggtitle('K리그 관중 추이') + xlab('라운드') + ylab('관중 수') + scale_fill_discrete(name="주중/주말", breaks=c("0", "1"),labels=c("주중", "주말")

# 서울의 관중 수 예측, 주말 데이터만 활용
seoul<-subset(game.df, team.name.home==c('FC서울')&weekend==1)

# 전북, 수원 빅클럽과의 경기 관충수는 제외
audi.prior<-c(19318, 13190, 17530, 17466, 15851, 18372, 17140)

# 음이항 분포 파리미터 추정
fitdistr (audi.prior,"negative binomial")
# size = 79.17555
# mu = 16981.00000

# 위의 결과값을 바탕으로 시뮬레이션
audi.post<-rnegbin(1000,16981,79)
hist(audi.post)
median(audi.post)
quantile(audi.post, c(1:20)*.05)

# 기대 관중수 (중앙값) = 16776.5
# 25% ~ 75% 범위, 15491.50 ~ 17991.25  
# 실제 관중수 = 15516

audi.post_df<-data.frame(audi=audi.post,name=c('seoul'))

# 서울 관중수의 예측값범위와 실제 값 
seoul_plot<-ggplot(audi.post_df, aes(x=audi)) +
  geom_histogram(binwidth=100, colour="black", fill="white") +
  geom_vline(aes(xintercept=median(audi)),color="blue", linetype="dotted", size=1) + 
  geom_vline(aes(xintercept=quantile(audi, .25)),color="blue", linetype="dotted", size=1) + 
  geom_vline(aes(xintercept=quantile(audi, .75)),color="blue", linetype="dotted", size=1) + ggtitle('FC서울 관중수 예측') + xlab('관중수')
 + geom_vline(aes(xintercept=15516),color="red", linetype="solid", size=1) 
  
# 성남의 관중 수 예측, 주말 데이터만 활용
sungnam<-subset(game.df, team.name.home==c('성남FC')&weekend==1)
audi.prior.sung<-sungnam[2:nrow(sungnam),3]

# 음이항 분포 파리미터 추정
fitdistr(audi.prior.sung,"negative binomial")
# size = 16.70473
# mu = 8155.11111

# 위의 결과값을 바탕으로 시뮬레이션
audi.post.negbin<-rnegbin(1000,8155.11111,16.70473)
quantile(audi.post.negbin, c(1:10)*.1)

# 기대 관중수 (중앙값) = 7947.0
# 25% ~ 75% 범위, 6673.5 ~ 9321.5 
# 실제 관중수 = 4828

audi.post.negbin_df<-data.frame(audi=audi.post.negbin,name=c('sungnam'))

# 성남 관중수의 예측값범위와 실제 값 
sungnam_plot<-ggplot(audi.post.negbin_df, aes(x=audi)) +
  geom_histogram(binwidth=100, colour="black", fill="white") +
  geom_vline(aes(xintercept=median(audi)),color="blue", linetype="dotted", size=1) + 
  geom_vline(aes(xintercept=quantile(audi, .25)),color="blue", linetype="dotted", size=1) + 
  geom_vline(aes(xintercept=quantile(audi, .75)),color="blue", linetype="dotted", size=1) + ggtitle('성남FC 관중수 예측') + xlab('관중수')
 + geom_vline(aes(xintercept=4828),color="red", linetype="solid", size=1) 

