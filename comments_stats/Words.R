library(KoNLP)
library(reshape2)
library(dplyr)
library(wordcloud2)
library(ggplot2)
theme_set(theme_gray(base_family='NanumGothic'))

comment_df_seoul<-subset(comment_df, team==c('서울'))
comment_df_jb<-subset(comment_df, team==c('전북'))

# Seoul commnets data cleaning
comment_df_seoul$comment<- gsub("은 |는 |이 |가 |의 |ㄱ|ㄴ|ㄷ|ㄹ|ㅁ|ㅂ|ㅅ|ㅇ|ㅈ|ㅊ|ㅋ|ㅌ|ㅍ|ㅎ|ㅏ|ㅑ|ㅓ|ㅕ|ㅗ|ㅛ|ㅜ|ㅠ|ㅡ|ㅣ|ㅃ|ㅉ|ㄲ|ㅆ", " ", comment_df_seoul$comment)
comment_df_seoul$comment<- gsub("[[:punct:]]", " ", comment_df_seoul$comment)
comment_df_seoul$comment<- gsub("\\w", " ", comment_df_seoul$comment)
comment_df_seoul$comment<- gsub("\\s+", " ", comment_df_seoul$comment)

# JB commnets data cleaning
comment_df_jb$comment<- gsub("은 |는 |이 |가 |의 |ㄱ|ㄴ|ㄷ|ㄹ|ㅁ|ㅂ|ㅅ|ㅇ|ㅈ|ㅊ|ㅋ|ㅌ|ㅍ|ㅎ|ㅏ|ㅑ|ㅓ|ㅕ|ㅗ|ㅛ|ㅜ|ㅠ|ㅡ|ㅣ|ㅃ|ㅉ|ㄲ|ㅆ", " ", comment_df_jb$comment)
comment_df_jb$comment<- gsub("[[:punct:]]", " ", comment_df_jb$comment)
comment_df_jb$comment<- gsub("\\w", " ", comment_df_jb$comment)
comment_df_jb$comment<- gsub("\\s+", " ", comment_df_jb$comment)

# Use dictionary
useNIADic()

# Extract noun in Seoul comments data
text.noun_seoul<- list()
for (i in 1:nrow(comment_df_seoul) ) {
  text.noun_seoul[i]<-melt(extractNoun(comment_df_seoul[i,1]))
}

# Select nouns with +1 length and frequency in Seoul comments data
text.noun_seoul<-melt(text.noun_seoul)
text.noun_seoul<-subset(text.noun_seoul, nchar(as.character(text.noun_seoul$value))>1)
text.noun_seoul<-table(text.noun_seoul$value)
text.noun_seoul<-subset(data.frame(text.noun_seoul), Freq > 1)
text.noun_seoul<-text.noun_seoul %>% arrange(-Freq)

# Extract noun in JB comments data
text.noun_jb<- list()
for (i in 1:nrow(comment_df_jb) ) {
  text.noun_jb[i]<-melt(extractNoun(comment_df_jb[i,1]))
}

# Select nouns with +1 length and frequency in JB comments data
text.noun_jb<-melt(text.noun_jb)
text.noun_jb<-subset(text.noun_jb, nchar(as.character(text.noun_jb$value))>1)
text.noun_jb<-table(text.noun_jb$value)
text.noun_jb<-subset(data.frame(text.noun_jb), Freq > 1)
text.noun_jb<-text.noun_jb %>% arrange(-Freq)

# Create world cloud with above nouns
# wordcloud2(data = text.noun_jb, size = 1, fontFamily='NanumGothic',color = "white",backgroundColor = "darkgreen")
# wordcloud2(data = text.noun_seoul, size = 1, fontFamily='NanumGothic',color = "darkred",backgroundColor = "black")

# Combine Seoul with JB data
text.noun<- rbind(text.noun_jb, text.noun_seoul)
text.noun<- text.noun %>% group_by(Var1) %>% summarise(Freq=sum(Freq))
text.noun<-text.noun %>% arrange(-Freq)

# Top 10 Nouns
ggplot(head(text.noun,10), aes(x=reorder(keyword, freq), y=freq)) + geom_bar(stat = "identity") + coord_flip()+ xlab("keyword")

# Create world cloud with above nouns
# wordcloud2(data = text.noun, size = 1, fontFamily='NanumGothic',color = "skyblue",backgroundColor = "darkblue")

# Top 10 Nouns by team
ggplot(head(text.noun_jb,10), aes(x=reorder(Var1, Freq), y=Freq)) + geom_bar(stat = "identity") + coord_flip()+ xlab("keyword") +labs(title="전북")
ggplot(head(text.noun_seoul,10), aes(x=reorder(Var1, Freq), y=Freq)) + geom_bar(stat = "identity") + coord_flip()+ xlab("keyword")+labs(title="서울")

# chi-square test for diffrent nouns
text.noun_jb$team<-'JB'
text.noun_seoul$team<-'Seoul'

text.noun_byteam<-rbind(text.noun_jb, text.noun_seoul)
text.noun_byteam$jb.sum<-sum(text.noun_jb$Freq)
text.noun_byteam$seoul.sum<-sum(text.noun_seoul$Freq)
text.noun_byteam$all<-sum(text.noun_byteam$Freq)
names(text.noun)[2]<-'key.freq'
text.noun_byteam<-merge(text.noun_byteam, text.noun, c('Var1'),all.x=T)

# Calculate LL
text.noun_byteam$E1<- with(text.noun_byteam, (seoul.sum * key.freq)/all)
text.noun_byteam$E2<- with(text.noun_byteam, (jb.sum * key.freq)/all)
text.noun_byteam$LL<- with(text.noun_byteam, 2 *(Freq*log(Freq/E1)+(key.freq-Freq)*log((key.freq-Freq)/E2)))

text.noun_byteam$jb.ngram<-nrow(subset(text.noun_byteam,team==c('JB')))
text.noun_byteam$seoul.ngram<-nrow(subset(text.noun_byteam,team==c('Seoul')))
  
text.noun_byteam$rank<- with(text.noun_byteam, ifelse(team==c('JB'),LL*jb.ngram, LL*seoul.ngram))
text.noun_byteam<-text.noun_byteam %>% arrange(-LL,team)

# Top 5 different nouns by team
ggplot(head(subset(text.noun_byteam,team==c('JB')&Freq>=10),5), aes(x=reorder(Var1, LL), y=LL)) + geom_bar(stat = "identity") + coord_flip()+ xlab("keyword") +labs(title="전북")
ggplot(head(subset(text.noun_byteam,team==c('Seoul')&Freq>=10),5), aes(x=reorder(Var1, LL), y=LL)) + geom_bar(stat = "identity") + coord_flip()+ xlab("keyword") +labs(title="서울")

