rm(list=ls())

# Two-Step Task -----------------------------------------------------------
library(dplyr)
library(magrittr)
load('03_data/02_aggregated_data/task_raw.Rdata')


df_task%<>%
  mutate(mapping1=as.numeric(stim_3),rt1=rt,key1=(key!=c(-1))*1,ch1=(stim_selected_1%in%c(0,3))*1,
         rt2=rt_2nd,key2=(key_2nd!=c(-1))*1,sec_stg_banditA=stim_order_1_2nd,sec_stg_banditB=stim_order_1_2nd,
         sec_stg_state=stage_2nd,ch2=stim_selected_2nd,reward=unlist(outcome))%>%
  select(subj,trial_num,block,rt1,key1,ch1,mapping1,
         rt2,key2,sec_stg_banditA,sec_stg_banditB,sec_stg_state,ch2,reward)

df_task%<>%filter(block == 'test')%>%filter(trial_num != '0')%>%filter(trial_num != '84')%>%filter(trial_num != '168')

#add columns
df_task%<>%
  mutate(reward_oneback=lag(reward),
         key1_oneback =lag(key1),
         key2_oneback =lag(key2),
         stay1_bandit =(ch1==lag(ch1))*1,stay2_bandit=(ch2==lag(ch2))*1,
         stay1_key    =(key1==lag(key1))*1,stay2_key =(key2==lag(key2))*1,
         stay1_mapping=((mapping1==lag(mapping1))*1),stay2_mapping=((sec_stg_banditA==lag(sec_stg_banditA))*1),
         stay2_state =(sec_stg_state==lag(sec_stg_state)*1))%>%
  na.omit()

#visual check rt distrbution
hist(df_task$rt1[df_task$rt1>0])
hist(df_task$rt2[df_task$rt2>0])

#precent of go
df_task_go<-df_task%>%group_by(subj)%>%summarise(pGo1=mean(key1),pGo2=mean(key2))
colMeans(df_task_go)

#checking is rt of Go steps < 200ms 
df_task_rt1<-df_task%>%group_by(subj)%>%filter(key1==1)%>%summarise(rt1_good=mean((rt1>200)*1))
df_task_rt2<-df_task%>%group_by(subj)%>%filter(key2==1)%>%summarise(rt2_good=mean((rt2>200)*1))
df_task_rt_good = data.frame(
  subj = df_task_rt1$subj,
  good = (df_task_rt1$rt1_good>0.9 & df_task_rt2$rt2_good>0.9)*1
)
sum(df_task_rt_good$good)/length(df_task_rt_good$subj)==1

df_task_clean <- df_task%>%filter(key1==0 | key1==1 & rt1>200)%>%filter(key2==0 |key2==1 & rt2>200)

average_rt1<-df_task_clean%>%group_by(subj)%>%filter(key1==1)
mean(average_rt1$rt1)
average_rt2<-df_task_clean%>%group_by(subj)%>%filter(key2==1)
mean(average_rt2$rt2)

save(df_task_clean,file='03_data/02_aggregated_data/task_clean.Rdata')

#####BIS-------------------------------------------------------------------
load('03_data/02_aggregated_data/bis_raw.Rdata')
bis<-bis%<>%
  mutate(item_9=5-item_9,item_20=5-item_20,item_30=5-item_30, item_1=5-item_1,
         item_7=5-item_7,item_8=5-item_8,item_10=5-item_10,item_12=5-item_12,
         item_13=5-item_13,item_15=5-item_15,item_29=5-item_29)


bis$bis<-apply(bis[,1:30],1,sum)
bis%<>%
  mutate(attention  =rowMeans(cbind(item_6, item_5, item_9, item_11, item_20, item_24, item_26, item_28)),
         motor      =rowMeans(cbind(item_2, item_3, item_4, item_16, item_17, item_19, item_21, item_22, item_23, item_25, item_30)),
         nonplanning=rowMeans(cbind(item_1, item_7, item_8, item_10, item_12, item_13, item_14, item_15, item_18, item_27, item_29)))%>%
  select(subj,attention,motor,nonplanning,bis)
head(bis)
hist(bis$bis)

save(bis,file='03_data/02_aggregated_data/bis.Rdata')

#####WURS-------------------------------------------------------------------
load('03_data/02_aggregated_data/wurs_raw.Rdata')
wurs$wurs<-apply(wurs[,1:25],1,sum)
wurs%<>%select(subj,wurs)
save(wurs,file='03_data/02_aggregated_data/wurs.Rdata')


#####ASRS-------------------------------------------------------------------
load('03_data/02_aggregated_data/asrs_raw.Rdata')
asrs$asrs6     <-apply(asrs[,1:6],1,sum)
asrs$asrs.partb<-apply(asrs[,7:20],1,sum)
asrs%<>% select(subj,asrs6,asrs.partb)
save(asrs,file='03_data/02_aggregated_data/asrs.Rdata')


#####BISBAS-------------------------------------------------------------------
load('03_data/02_aggregated_data/bisbas_raw.Rdata')

#bisbas$bisbas<-apply(bisbas[,1:24],1,sum)
#hist(bisbas$bisbas)

bisbas%<>%
  mutate(BIS_1  =rowMeans(cbind(item_2, item_8, item_13, item_16, item_19, item_22, item_24)),
         BAS_Drive      =rowMeans(cbind(item_3, item_9, item_12, item_21)),
         BAS_Fun_Seeking=rowMeans(cbind(item_5, item_10, item_15, item_20)),
         BAS_Reward_Responsiveness=rowMeans(cbind(item_4, item_7, item_14, item_18, item_23)))%>%
  select(subj,BIS_1,BAS_Drive,BAS_Fun_Seeking, BAS_Reward_Responsiveness)
head(bisbas)

hist(bisbas$BIS_1)
hist(bisbas$BAS_Drive)
hist(bisbas$BAS_Fun_Seeking)
hist(bisbas$BAS_Reward_Responsiveness)

save(bisbas,file='03_data/02_aggregated_data/bisbas.Rdata')

# starter -----------------------------------------------------------
load('03_data/02_aggregated_data/starter_raw.Rdata')
df_starter%<>%
  mutate(subj = df_starter[,1],rt= df_starter[,3],num_tries= df_starter[,2])%>%
  select(subj,rt,num_tries)

head(df_starter) 
hist(df_starter$rt)
mean(df_starter$rt)/60

hist(df_starter$rt[df_starter$rt<5000])
mean(df_starter$rt[df_starter$rt<5000])/60

hist(as.integer(df_starter$num_tries))
mean(as.integer(df_starter$num_tries))

hist(as.integer(df_starter$num_tries[df_starter$num_tries<8]))
mean(as.integer(df_starter$num_tries[df_starter$num_tries<8]))

#average reading+quiz time =11.58869 minutes , average number of tries = 1.727273
#without outlier :  average reading+quiz time = 10.06273 minutes , average number of tries = 1.839286


save(df_starter,file='03_data/02_aggregated_data/starter.Rdata')

# post task open questions -----------------------------------------------------------
load('03_data/02_aggregated_data/questions_raw.Rdata')
hist(df_questions$rt[(df_questions$rt)<795213]/60)
mean(df_questions$rt[(df_questions$rt)<795213]/60)/60

df_questions$age <-as.integer(gsub("[{\"Q\":\"]", '', df_questions$age))
df_questions$age[df_questions$age=="0"]<-NA

hist(df_questions$age)
mean(na.omit(df_questions$age))
#mean age = 25.92727

gender <-gsub("[{\"Q1\":\"]", '', df_questions$gender[df_questions$age!="{\"Q1\":\""])
df_questions$gender <- (gender=="male"|gender=="Male")*1
#18 male 12 female 
sum(as.integer(na.omit(df_questions$gender)))

save(df_questions,file='03_data/02_aggregated_data/open_questions.Rdata')


df_questions$comprehension
#28 participants understood it very well 
#participant 26 and 9 had difficulties 

df_questions$misunderstanding
#most participants had no misunderstandings 
#sub 29 -  preferred pressing a key then waiting 
#sub 4,19,20 had difficulty around the probability of preducing a pearl

df_questions$difficulty1
#20 participants experienced the task difficulty level as easy  
#4 participants experienced the task difficulty level as medium  
#6 participants experienced the task difficulty level as difficult  

df_questions$difficulty2
#sub 10 didnt enjooy the waiting.. 
#sub 6,11 it was just luck
#sub 2,13 it was long 
#sub 26 the repetitiveness
#sub 27 bored 
#sub 28 - tried to remember the ships position 
# 18 participants experienced difficulty by the changing reward probibilty and understanding the pattern 

df_questions$strategy
# sub 6,14,26 used random strategy / no strategy  
#sub 20 is a computer science student :)
# sub 27 switched his choices (regardless of the outcome)
#sub 28 did press>no press followed by a no press>press and so on (regardless of the outcome)
#most of the subjects used a strategy of finding an benefital oyster and then sticking to it until it loses then changing a selection 


