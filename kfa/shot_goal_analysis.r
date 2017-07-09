library(dplyr)
library(stringr)
library(ggplot2)
library(ebbr)
library(tidyr)
library(gamlss)
library(broom)
theme_set(theme_gray(base_family='NanumGothic'))

# Read the file
kfa<-read.csv(file.choose())
head(kfa)
names(kfa)

# Col. name
names(kfa)<-c('league','location','home','away','date','time','name','position','no','half','time1','time2','x','y','event','TF')

# Data cleaning
table(kfa$home) %>% data.frame()
kfa_a_man<-subset(kfa, home==c('대한민국 남자국가대표팀')|home==c('알제리 남자국가대표팀'))
head(kfa_a_man)
table(kfa_a_man$home) %>% data.frame()
kfa_a_man_away<- kfa_a_man %>% filter(home==c('알제리 남자국가대표팀'))
kfa_a_man_home<- kfa_a_man %>% filter(home==c('대한민국 남자국가대표팀'))
kfa_a_man_away$home<-'대한민국 남자국가대표팀'
kfa_a_man_away$away<-'알제리 남자국가대표팀'
kfa_a_man <- rbind(kfa_a_man_away, kfa_a_man_home)

## Except outlier, 
score <- kfa_a_man %>% group_by(date, away) %>% summarise(goal=sum(freq[event==c('골인')]))
# date == 20030929
# away == 네팔 남자국가대표팀
kfa_a_man <- kfa_a_man %>% filter(date!=20030929)

# Year and success of events
kfa_a_man$year <- with(kfa_a_man, substr(date, 1,4))
kfa_a_man$freq <- with(kfa_a_man, ifelse(TF==c('S'),1,0))

# shot and pass events by name
kfa_a_man %>% group_by(name) %>% summarise(n=n()) %>% arrange(-n)

table(kfa_a_man$event) %>% data.frame()

# Shot & Goal
# '골인', '슈팅', '위협슈팅', '유효슈팅' 
# Pass
# '어시스트', '얼리크로스', '역습전개패스', '컷백', '크로스', '킬패스'

# Num. of events by stats
kfa_a_man %>% filter(event==c('골인')) %>% group_by(name) %>% summarise(n=n()) %>% arrange(-n)
kfa_a_man %>% filter(event==c('슈팅')) %>% group_by(name) %>% summarise(n=n()) %>% arrange(-n)
kfa_a_man %>% filter(event==c('유효슈팅')) %>% group_by(name) %>% summarise(n=n()) %>% arrange(-n)
kfa_a_man %>% filter(event==c('킬패스')) %>% group_by(name) %>% summarise(n=n()) %>% arrange(-n)

# Num. of shot and goal by year & player 
kfa_a_man_shot_goal_year <- kfa_a_man %>% group_by(year, name) %>% 
  summarise(shot=sum(freq[event==c('슈팅')]),shot.t=sum(freq[event==c('유효슈팅')]),shot.r=sum(freq[event==c('위협슈팅')]),goal=sum(freq[event==c('골인')]))

kfa_a_man_shot_goal_year$shot.all<-with(kfa_a_man_shot_goal_year, shot+shot.t+shot.r)
kfa_a_man_shot_goal_year$goal.shot <- with(kfa_a_man_shot_goal_year, goal/shot.all)

kfa_a_man_shot_goal_year %>%
  filter(shot.all >= 5 ) %>%
  ggplot(aes(year, goal.shot)) +
  geom_boxplot() 

# Num. of shot and goal by player
kfa_a_man_shot_goal <- kfa_a_man %>% group_by(name) %>% 
  summarise(shot=sum(freq[event==c('슈팅')]),shot.t=sum(freq[event==c('유효슈팅')]),shot.r=sum(freq[event==c('위협슈팅')]),goal=sum(freq[event==c('골인')]))

kfa_a_man_shot_goal$shot.all<-with(kfa_a_man_shot_goal, shot+shot.t+shot.r)
kfa_a_man_shot_goal$goal.shot <- with(kfa_a_man_shot_goal, goal/shot.all)

# Data cleaning
kfa_a_man_shot_goal<-subset(kfa_a_man_shot_goal, goal.shot<=1)

# Distribution of shots
ggplot(data=kfa_a_man_shot_goal, aes(shot.all)) + geom_histogram(breaks=seq(min(kfa_a_man_shot_goal$shot.all), max(kfa_a_man_shot_goal$shot.all), by =5)) + xlab("슛팅수")
quantile(kfa_a_man_shot_goal$shot.all, c(1:10)*.1)

# goal/shot
sum(kfa_a_man_shot_goal$goal)/sum(kfa_a_man_shot_goal$shot.all) # 0.215119

# Distribution of goal/shot
nrow(kfa_a_man_shot_goal) # 186
ggplot(data=kfa_a_man_shot_goal, aes(goal.shot)) + geom_histogram(breaks=seq(min(kfa_a_man_shot_goal$goal.shot), max(kfa_a_man_shot_goal$goal.shot), by =.05))  + xlab("골 결정력")

# Distribution of goal/shot >= 5 shots
kfa_a_man_shot_goal %>% filter(shot.all>=5) %>% nrow() # 93
kfa_a_man_shot_goal %>% filter(shot.all>=5) %>% group_by() %>% summarise(shot=sum(shot.all),goal=sum(goal), goal/shot) # 0.2246673
ggplot(data=subset(kfa_a_man_shot_goal, shot.all>=5), aes(goal.shot)) + geom_histogram(breaks=seq(min(kfa_a_man_shot_goal$goal.shot), max(kfa_a_man_shot_goal$goal.shot), by =.05))  + xlab("골 결정력")

# Relationship btw shot and goal/shot
# Opta data -- https://fivethirtyeight.com/features/lionel-messi-is-impossible/
with(data=subset(kfa_a_man_shot_goal, shot.all>=5), plot(shot.all, goal.shot))

kfa_a_man_shot_goal %>%
  filter(shot.all >= 5 ) %>%
  ggplot(aes(log(shot.all), goal.shot)) +
  geom_point() +
  geom_smooth(method = "lm") + xlab("슈팅") + ylab("골 결정력")

# w/o other info.
eb_kfa_a_man_shot_goal <- kfa_a_man_shot_goal %>%
  add_ebb_estimate(goal, shot.all, prior_subset = shot.all >= 5)
eb_kfa_a_man_shot_goal %>% arrange(-.fitted) %>% head(10) 

# w/ the num. of shot info.
eb_kfa_a_man_shot_goal <- kfa_a_man_shot_goal %>%
  add_ebb_estimate(goal, shot.all, method = "gamlss", mu_predictors = ~ log(shot.all))
eb_kfa_a_man_shot_goal %>% arrange(-.fitted) %>% head(20) 

# Change by info
eb_kfa_a_man_shot_goal %>%
  rename(Raw = .raw, Shrunken = .fitted) %>%
  gather(type, estimate, Raw, Shrunken) %>%
  ggplot(aes(shot.all, estimate)) +
  geom_point() +
  facet_wrap(~ type)+ xlab("슈팅") + ylab("골 결정력") + scale_x_log10()

# Num. of game by player
match <- kfa_a_man %>% group_by(name) %>% summarise(n=n_distinct(date))
eb_kfa_a_man_shot_goal<-merge(eb_kfa_a_man_shot_goal, match, c('name'),all.x=T)
eb_kfa_a_man_shot_goal %>% arrange(-.fitted) %>% head(20)

# Top 20 player plot
eb_kfa_a_man_shot_goal %>% arrange(-.fitted) %>% head(20) %>%
  mutate(name = reorder(name, .fitted)) %>%
  ggplot(aes(.fitted, name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = .low, xmax = .high)) +
  labs(x = "Estimated shooting efficiency (w/ 95% confidence interval)",
       y = "Player")

# Export
export <- eb_kfa_a_man_shot_goal %>% arrange(-.fitted) %>% head(20)
write.csv(export, "expot.csv")
