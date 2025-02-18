library(R.matlab)
library(parallel)

rm(list=ls())
detectCores()
source('01_functions/01_gong_functions.R')

#### Simulate data ----

#rndwlk=readMat('myfiles/nspn_rndwlk_frcXstateXtrlXcounter.mat')[[1]]
rndwlk=read.csv('rndwlk_v1.csv')
Nsubj=50*2
Nexp =10
Ntrls=1000

#simulate parameters
source('01_functions/01_gong_rand_params.R')
x<-lapply(1:Nsubj,function(s) {rand_params()})
x<-do.call(rbind,x)


#simulate data
source('02_models/01_gong_simme.R')

df<-
  mclapply(1:Nsubj,function(s) {
    sngdf   <-lapply(1:Nexp, function(experiment) {gong_simme(x[s,],rndwlk=rndwlk[,,],subj=s,experiment,Ntrls)})
    sngdf   <-do.call(rbind,sngdf)}
    ,mc.cores =2)


####model agnostic--------------------------------------------------------
df2<-do.call(rbind,df)
apply(df2, 2, function(x) any(is.na(x))) #are there any NA cells 

library(data.table)
library(dplyr)
library(tidyr)

df2<-df2%>%
  mutate(rw_pv=lag(rw,default=1),
         resp1_oneback=lag(resp1,default=1),
         resp2_oneback=lag(resp2,default=1),
         stay1=(ch1==lag(ch1,default=1))*1,
         stay2=(ch2==lag(ch2,default=1))*1)

apply(df2, 2, function(x) any(is.na(x))) #are there any NA cells 


df_2 <-
  df2%>%
  filter(trl>1)%>%
  group_by(subj,resp1_oneback,resp2_oneback,rw_pv)%>%
  summarise(pStay=mean(stay1))%>%
  pivot_wider(names_from=c(resp1_oneback,resp2_oneback,rw_pv),values_from=pStay)%>%
  colMeans()

  plot(df_2[2:9])
  
  df_3 <-
    df2%>%
    filter(trl>1)%>%
    group_by(gamma1,resp1_oneback,resp2_oneback,rw_pv)%>%
    summarise(pStay=mean(stay1))%>%
    pivot_wider(names_from=c(resp1_oneback,resp2_oneback,rw_pv),values_from=pStay)

#rewarded = 1 , punished= 0
# GO = 1 , Nogo = 2 
  
    df_3_effect <- data.frame(
      gamma1 <- df_3$gamma1,
      reward_effect_first_resp_Go = df_3$'1_1_1'-df_3$'1_1_0',
      reward_effect_first_resp_Nogo = df_3$'2_1_1'-df_3$'2_1_0',
      reward_effect_second_resp = df_3$'1_1_1'-df_3$'1_1_0',
      first_resp_rewarded = df_3$'1_1_1'-df_3$'2_1_1',
      second_resp_rewarded = df_3$'1_1_1'-df_3$'1_2_1',
      both_resp_rewarded = df_3$'1_1_1'-df_3$'2_2_1',
      first_resp_unrewarded = df_3$'1_1_0'-df_3$'2_1_0',
      second_resp_unrewarded = df_3$'1_1_0'-df_3$'1_2_0',
      both_resp_unrewarded = df_3$'1_1_0'-df_3$'2_2_0'
    )

  x=  df_3_effect$gamma1
  y=  df_3_effect$reward_effect_first_resp_Go
  fit <- lm(y ~ x)   ## polynomial of degree 3
  plot(x, y,xlab = 'gamma1',ylab = 'Reward effect size (first response)')  ## scatter plot (colour: black)

  x0 <- seq(min(x), max(x), length = 100)  ## prediction grid
  y0 <- predict.lm(fit, newdata = list(x = x0))  ## predicted values
  lines(x0, y0, col = 2)  ## add regression curve (colour: red)
  par(new=TRUE)
  y=  df_3_effect$reward_effect_first_resp_Nogo
  fit <- lm(y ~ x)   ## polynomial of degree 3
  plot(x, y,xlab = 'gamma1',ylab = 'Reward effect size (first response)',col = 5)  ## scatter plot (colour: black)
  y0 <- predict.lm(fit, newdata = list(x = x0))  ## predicted values
  lines(x0, y0, col = 3)  ## add regression curve (colour: red)
  

# parameter recovery ------------------------------------------------------
library(parallel)
source('02_models/01_gong_fitme.R')
fitparms<-data.frame()
Nparms<-dim(x)[2]
start_time <- Sys.time()

#optim
y<-
  lapply(1:Nsubj,function(s)
  {
    fit<- optim(par = runif(Nparms,-3,3),
                fn = gong_fitme,
                df = df[[s]],
                Ntrls = dim(df[[s]])[1],
                lower = rep(-3,Nparms),
                upper = rep(3,Nparms),
                method = "L-BFGS-B")$par})
  #mc.cores =2)
mytime <- Sys.time() - start_time

#transform and copy best fitted params
y<-do.call(rbind,y)
#y<-transform_params(y,c('logit','exp','logit','logit','logit','logit','logit','logit','logit','logit'))
#y[,5:8]<-y[,5:7]-.5

#model parameters
y2<-data.frame(
  sim.alpha1        =mylogit(y[,1]),
#  sim.alpha2        =mylogit(y[,1]),
  sim.gamma1        =mylogit(y[,2]),
  sim.gamma2        =mylogit(y[,3]),
  sim.beta1         =exp(y[,4]),
  sim.lambda        =mylogit(y[,5]),
  sim.persv         =mylogit(y[,6])-.5,
  sim.go_bias       =mylogit(y[,7])-.5
)

cor(x,y2)
diag(cor(x,y2))
mean(diag(cor(x,y2)))




####model agnostic
apply(df[[3]], 2, function(x) any(is.na(x))) #are there ant NA cells 

df2<-do.call(rbind,df)
apply(df2, 2, function(x) any(is.na(x))) #are there ant NA cells 

library(data.table)
df2$rw_pv            <-shift(df2$rw, n=1, fill=1, type=c("lag"), give.names=FALSE) # was the previous trail rewarded(=1)/unrewarded(=0) 
df2$stay1           <-(df2$ch1==shift(df2$ch1, n=1, fill=1, type=c("lag"), give.names=FALSE))*1 #did the agent repeated the same first stage action 
#df2$change1           <-(df2$ch1=!shift(df2$ch1, n=1, fill=1, type=c("lag"), give.names=FALSE))*1 #did the agent changed his first stage action 
df2$stay2           <-(df2$ch2==shift(df2$ch2, n=1, fill=1, type=c("lag"), give.names=FALSE))*1 #did the agent repeated the same second stage action 
df2$resp1_oneback           <-shift(df2$resp1, n=1, fill=1, type=c("lag"), give.names=FALSE) #was the previous trails' first stage action type Go(=1)/Nogo(=0)
df2$resp2_oneback           <-shift(df2$resp2, n=1, fill=1, type=c("lag"), give.names=FALSE) #was the previous trails' first stage action type Go(=1)/Nogo(=0)

library(reshape2)
df3<-dcast(df2,subj  ~ resp1_oneback+rw_pv,mean, value.var = c('stay1'),subset=(trl>1))
apply(df3, 2, function(x) any(is.na(x))) #are there ant NA cells 
df3[(is.na(df3))]<- 0 #converet them to 0
df3

df4<-colMeans(df3)
df4
plot(df4[2:9])


##################################################
df51_p =data.frame(row.names = c("action_2_go","action_2_nogo","1st_MA"))
df51_p[1,1] = mean(df3$`-1_1_1`)
df51_p[1,2] = mean(df3$`-1_0_1`)
df51_p[2,1] = mean(df3$`-1_1_0`)
df51_p[2,2] = mean(df3$`-1_0_0`)
df51_p[3,1] = mean(c(df51_p[1,1],df51_p[2,1])) 
df51_p[3,2] = mean(c(df51_p[1,2],df51_p[2,2]))
df51_p[1,3] = mean(c(df51_p[1,1],df51_p[1,2]))
df51_p[2,3] = mean(c(df51_p[2,1],df51_p[2,2]))
colnames(df51_p)<-c("action_1_go","action_1_nogo","2nd_MA")

df51_r =data.frame(row.names = c("action_2_go","action_2_nogo","1st_MA"))
df51_r[1,1] = mean(df3$`1_1_1`)
df51_r[1,2] = mean(df3$`1_0_1`)
df51_r[2,1] = mean(df3$`1_1_0`)
df51_r[2,2] = mean(df3$`1_0_0`)
df51_r[3,1] = mean(c(df51_r[1,1],df51_r[2,1])) 
df51_r[3,2] = mean(c(df51_r[1,2],df51_r[2,2]))
df51_r[1,3] = mean(c(df51_r[1,1],df51_r[1,2]))
df51_r[2,3] = mean(c(df51_r[2,1],df51_r[2,2]))
colnames(df51_r)<-c("action_1_go","action_1_nogo","2nd_MA")

ME_act_s2_p = df51_p[1,3] - df51_p[2,3]
ME_act_s1_p = df51_p[3,1] - df51_p[3,2]
ME_act_s2_r = df51_r[1,3] - df51_r[2,3]
ME_act_s1_r = df51_r[3,1] - df51_r[3,2]

paste("ME_act_s2_r = ",ME_act_s2_r,"ME_act_s1_r = ",ME_act_s1_r)
paste("ME_act_s2_p = ",ME_act_s2_p,"ME_act_s1_p = ",ME_act_s1_p)


df6<-dcast(df2,subj  ~ act1_pv+resp2_pv,mean, value.var = c('stay1'))
apply(df6, 2, function(x) any(is.na(x))) #are there any NA cells 

df61 =data.frame(row.names = c("action_2_go","action_2_nogo","1st_MA"))
df61[1,1] = mean(df6$`1_1`)
df61[1,2] = mean(df6$`0_1`)
df61[2,1] = mean(df6$`1_0`)
df61[2,2] = mean(df6$`0_0`)
df61[3,1] = mean(c(df61[1,1],df61[2,1])) 
df61[3,2] = mean(c(df61[1,2],df61[2,2]))
df61[1,3] = mean(c(df61[1,1],df61[1,2]))
df61[2,3] = mean(c(df61[2,1],df61[2,2]))
colnames(df61)<-c("action_1_go","action_1_nogo","2nd_MA")

ME_act_s1 = df61[3,1] - df61[3,2]
ME_act_s2 = df61[1,3] - df61[2,3]
paste("ME_act_s2 = ",ME_act_s2,"ME_act_s1 = ",ME_act_s1)

df61
plot(c(df61[1,3],df61[2,3],df61[3,1],df61[3,2]))


########

#corr between true params and model-agnostic effects
library(lme4)
m<-glmer(stay1 ~ resp1_oneback*resp2_oneback*rw_pv + (resp1_oneback*resp2_oneback*rw_pv|subj),
         data=df2,family=binomial(link="logit"),nAGQ = 0)

library(gridExtra)
grid.arrange(
  plot(x[,'gamma1'],ranef(m)$subj[,'resp1_oneback:rw_pv']),
  plot(x[,'gamma1'],ranef(m)$subj[,'resp2_oneback:rw_pv']),
  plot(x[,'gamma2'],ranef(m)$subj[,'resp1_oneback:rw_pv']),
  plot(x[,'gamma2'],ranef(m)$subj[,'resp2_oneback:rw_pv'])
)



###fit model----
library(parallel)
source('myfunc/00_gong_null_fitme.R')
fitparms<-data.frame()
Nparms<-dim(x)[2]
start_time <- Sys.time()

#optim
y<-
  mclapply(1:Nsubj,function(s)
  {
    fit<- optim(par = runif(Nparms,-3,3),
                fn = gong_null_fitme,
                df = df[[s]],
                Ntrls = dim(df[[s]])[1],
                lower = rep(-3,Nparms),
                upper = rep(3,Nparms),
                method = "L-BFGS-B")$par},
  mc.cores =2)
mytime <- Sys.time() - start_time

#transform and copy best fitted params
y<-do.call(rbind,y)
#y<-transform_params(y,c('logit','exp','logit','logit','logit','logit','logit','logit','logit','logit'))
#y[,5:8]<-y[,5:7]-.5

#model parameters
y2<-data.frame(
  sim.alpha1        =mylogit(y[,1]),
 # sim.alpha2        =mylogit(y[,2]),
  sim.beta1         =exp(y[,3]),
  sim.lambda        =mylogit(y[,4]),
  sim.persv         =mylogit(y[,5])-.5,
  sim.go_bias       =mylogit(y[,6])-.5
)


cor(x,y2)
diag(cor(x,y2))
mean(diag(cor(x,y2)))
