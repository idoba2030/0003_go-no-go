rm(list=ls())
load('03_data/02_aggregated_data/wurs_raw.Rdata')
load('03_data/02_aggregated_data/asrs_raw.Rdata')
load('03_data/02_aggregated_data/bis_raw.Rdata')
load('03_data/02_aggregated_data/bisbas_raw.Rdata')
load('03_data/02_aggregated_data/task_raw.Rdata')
load('03_data/02_aggregated_data/questions_raw.Rdata')
load('03_data/02_aggregated_data/starter_raw.Rdata')


# two-step task -----------------------------------------------------------
library(dplyr)
library(magrittr)


df_task%<>%
  mutate(mapping1=stim_3,rt1=rt,key1=(key!=c(-1))*1,ch1=(stim_selected_1%in%c(0,3))*1,
         rt2=rt_2nd,key2=(key_2nd!=c(-1))*1,sec_stg_banditA=stim_order_1_2nd,sec_stg_banditB=stim_order_1_2nd,
         sec_stg_state=stage_2nd,ch2=stim_selected_2nd,reward=unlist(outcome))%>%
  select(subj,trial_num,block,rt1,key1,ch1,mapping1,
         rt2,key2,sec_stg_banditA,sec_stg_banditB,sec_stg_state,ch2,reward)
df_task%<>%filter(block == 'test')

#add columns
df_task%<>%
  mutate(reward_oneback=lag(reward),
         stay1_bandit=(ch1==lag(ch1))*1,stay2_bandit=(ch2==lag(ch2))*1,
         stay1_key   =(key1==lag(key1))*1,stay2_key =(key2==lag(key2))*1)%>%
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

df_task_clean <- df_task%>%filter(key1==0 | key1==1 & rt1>200)%>%filter(key2==0 |key2==1 & rt2>200)


#reward effect
df_task_reward_effect_bandit<-df_task_clean%>%group_by(subj,reward_oneback)%>%summarise(pStay_badnit1=mean(stay1_bandit),pStay_bandit2=mean(stay2_bandit))
df_task_reward_effect_resp1<-df_task_clean%>%group_by(subj,reward_oneback,key1)%>%summarise(pStay_key1=mean(stay1_key))
df_task_reward_effect_resp2<-df_task_clean%>%group_by(subj,reward_oneback,key2)%>%summarise(pStay_key2=mean(stay2_key))
colMeans()

