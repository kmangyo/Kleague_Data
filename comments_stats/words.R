library(KoNLP)
library(reshape2)
library(dplyr)

comment_df_seoul<-subset(comment_df, team==c('서울'))
comment_df_jb<-subset(comment_df, team==c('전북'))

comment_df_seoul$comment<- gsub("은 |는 |이 |가 |의 |ㄱ|ㄴ|ㄷ|ㄹ|ㅁ|ㅂ|ㅅ|ㅇ|ㅈ|ㅊ|ㅋ|ㅌ|ㅍ|ㅎ|ㅏ|ㅑ|ㅓ|ㅕ|ㅗ|ㅛ|ㅜ|ㅠ|ㅡ|ㅣ|ㅃ|ㅉ|ㄲ|ㅆ", " ", comment_df_seoul$comment)

comment_df_seoul$comment<- gsub("[[:punct:]]", " ", comment_df_seoul$comment)
comment_df_seoul$comment<- gsub("\\w", " ", comment_df_seoul$comment)
comment_df_seoul$comment<- gsub("\\s+", " ", comment_df_seoul$comment)

comment_df_jb$comment<- gsub("은 |는 |이 |가 |의 |ㄱ|ㄴ|ㄷ|ㄹ|ㅁ|ㅂ|ㅅ|ㅇ|ㅈ|ㅊ|ㅋ|ㅌ|ㅍ|ㅎ|ㅏ|ㅑ|ㅓ|ㅕ|ㅗ|ㅛ|ㅜ|ㅠ|ㅡ|ㅣ|ㅃ|ㅉ|ㄲ|ㅆ", " ", comment_df_jb$comment)

comment_df_jb$comment<- gsub("[[:punct:]]", " ", comment_df_jb$comment)
comment_df_jb$comment<- gsub("\\w", " ", comment_df_jb$comment)
comment_df_jb$comment<- gsub("\\s+", " ", comment_df_jb$comment)

useNIADic()

text.noun_seoul<- list()
for (i in 1:nrow(comment_df_seoul) ) {
  text.noun_seoul[i]<-melt(extractNoun(comment_df_seoul[i,1]))
}

text.noun_seoul<-melt(text.noun_seoul)
text.noun_seoul<-subset(text.noun_seoul, nchar(as.character(text.noun_seoul$value))>1)
text.noun_seoul<-table(text.noun_seoul$value)
text.noun_seoul<-subset(data.frame(text.noun_seoul), Freq > 1)
text.noun_seoul<-text.noun_seoul %>% arrange(-Freq)

text.noun_jb<- list()
for (i in 1:nrow(comment_df_jb) ) {
  text.noun_jb[i]<-melt(extractNoun(comment_df_jb[i,1]))
}

text.noun_jb<-melt(text.noun_jb)
text.noun_jb<-subset(text.noun_jb, nchar(as.character(text.noun_jb$value))>1)
text.noun_jb<-table(text.noun_jb$value)
text.noun_jb<-subset(data.frame(text.noun_jb), Freq > 1)
text.noun_jb<-text.noun_jb %>% arrange(-Freq)

