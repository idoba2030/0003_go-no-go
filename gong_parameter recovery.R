library(R.matlab)
library(parallel)

rm(list=ls())
detectCores()
source('myfunc/00_gong_null_functions.R')

#### Simulate data ----

rndwlk=readMat('myfiles/nspn_rndwlk_frcXstateXtrlXcounter.mat')[[1]]
Nsubj=10*2
Nexp =10
Ntrls=200

#simulate parameters
source('myfunc/00_gong_null_rand_params.R')
x<-lapply(1:Nsubj,function(s) {rand_params()})
x<-do.call(rbind,x)


#simulate data
source('myfunc/00_gong_null_simme.R')

df<-
  mclapply(1:Nsubj,function(s) {
    sngdf   <-lapply(1:Nexp, function(experiment) {gong_null_simme(x[s,],rndwlk=rndwlk[,,,sample(1:2,size=1)],subj=s,experiment,Ntrls)})
    sngdf   <-do.call(rbind,sngdf)}
    ,mc.cores =2)


####model agnostic
df2<-do.call(rbind,df)
library(data.table)
df2$rw_pv            <-shift(df2$rw, n=1, fill=1, type=c("lag"), give.names=FALSE) # was the previous trail rewarded(=1)/unrewarded(=0) 
df2$stay1           <-(df2$ch1==shift(df2$ch1, n=1, fill=1, type=c("lag"), give.names=FALSE)) #did the agent repeated the same first stage action 
df2$stay2           <-(df2$ch2==shift(df2$ch2, n=1, fill=1, type=c("lag"), give.names=FALSE)) #did the agent repeated the same second stage action 
df2$go1st_pv           <-shift(df2$go1st, n=1, fill=1, type=c("lag"), give.names=FALSE) #was the previous trails' first stage action type Go(=1)/Nogo(=0)
df2$go2st_pv           <-shift(df2$go2st, n=1, fill=1, type=c("lag"), give.names=FALSE) #was the previous trails' first stage action type Go(=1)/Nogo(=0)

library(reshape2)
df3<-dcast(df2,subj  ~ rw_pv+go1st_pv+go2st_pv,mean, value.var = c('stay1'))
apply(df3, 2, function(x) any(is.na(x))) #are there ant NA cells 
df3[(is.na(df3))]<- 0 #converet them to 0

df4<-colMeans(df3)
df4
mean(df4[2:9])
ME_p1_a1 = mean((df3$`1_1_1`+df3$`1_1_0`)-(df3$`1_0_1`+df3$`1_0_0`)) #first main effect: first stage stay probability for rewarded previous trail in the first stage action condition   
ME_m1_a1 = mean((df3$`-1_1_1`+df3$`-1_1_0`)-(df3$`-1_0_1`+df3$`-1_0_0`)) #first main effect: first stage stay probability for avoiding punishment at the previous trail in first stage action condition   
ME_p1_a2 = mean((df3$`1_1_1`+df3$`1_0_1`)-(df3$`1_1_0`+df3$`1_0_0`)) #second main effect: first stage stay probability for rewarded previous trail in the second stage action condition   
ME_m1_a2 = mean((df3$`-1_1_1`+df3$`-1_0_1`)-(df3$`-1_1_0`+df3$`-1_0_0`)) #second main effect: first stage stay probability for avoiding punishment at the previous trail in the second stage action condition   

df5 = data.frame(row.names = c("action_1","action_2"))
colnames(df5)<-c("reward","punishment")
df5[1,1] = ME_p1_a1
df5[1,2] = ME_m1_a1
df5[2,1] = ME_p1_a2
df5[2,2] = ME_m1_a2



ME_p1_go1_go2=((df3$`1_1_1`+df3$`-1_1`)-(df3$`1_0`+df3$`1_1`))
plot(x[,'w'],mf1_go)
cor(x[,'w'],mf1_go)
mb1_go=((df3$`1_0`-df3$`1_1`)-(df3$`0_0`-df3$`0_1`)) #interaction effect for Go action
plot(x[,'w'],mb1_go)
cor(x[,'w'],mb1_go)
mf1_go=((df3$`1_0`+df3$`1_1`)-(df3$`0_0`+df3$`0_1`)) #main effect for Go action
plot(x[,'w'],mf1_go)
cor(x[,'w'],mf1_go)

mb1_ng=((df3$`1_0`-df3$`1_1`)-(df3$`0_0`-df3$`0_1`)) #interaction effect for Nogo action
plot(x[,'w'],mb1_ng)
cor(x[,'w'],mb1_ng)
mf1_ng=((df3$`1_0`+df3$`1_1`)-(df3$`0_0`+df3$`0_1`)) #main effect for Nogo action
plot(x[,'w'],mf1_ng)
cor(x[,'w'],mf1_ng)

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
