# this function simulate an agent in the two-step go/nogo task Ntrls times

Shira_simme<-function(x,rndwlk,subj,experiment,Ntrls) {
  
  # set up the models' parameters - 10 free parameters (α_go, α_ng, α2, β1, β2, λ, w, p_obj, p_act, go_bias)
  alpha_go       =x[1] 
  #alpha_ng       =x[2]
  alpha_ng       = 0.00000001
  alpha2         =x[3]
  beta1          =x[4]
  beta2          =x[5] #might be neglected
  lambda         =x[6]
  w              =x[7]
  persv_obj      =x[8]
  persv_act      =x[9]
  go_bias        =x[10]
    
  #pre allocate data frame
  df=data.frame()
  
  for (t in 1:Ntrls) {
    if (t==1){
      #pre allocate vars
      Qmf            =matrix(0,3,2)     #state (3) x fractal at each state (2)
      Qmb=Qnet       =c(0,0)
      perCh1         =c(0,0) #repeating the same first stage choice
      perAct         =c(0,0) #repeating the same action go | nogo
      go=ng          =0
      stmat          =rbind(c(2,3),c(3,2)) #state transition map
      }  
    
#neglected r30-31
    #mapping : setting (a1,s1,s1) as go or nogo
    if (sample(1:0,size=1)==1){ #if =1 assigns Go to ch1
      alpha1        = c(alpha_go,alpha_ng)
      go_bias1      = c(go_bias,0)
      perAct        =c(go,ng)
      }else{
        alpha1      = c(alpha_ng,alpha_go)
        go_bias1    = c(0,go_bias)
        perAct      =c(ng,go)
      }
    
    # make a first stage choice
    Qmb[1] =.7*max(Qmf[2,1:2])+.3*max(Qmf[3,1:2])
    Qmb[2] =.3*max(Qmf[2,1:2])+.7*max(Qmf[3,1:2])
    
    Qnet[1]=(1-w)*Qmf[1,1] + (w)*Qmb[1]+persv_obj*perCh1[1]+persv_act*perAct[1]+go_bias1[1] #added the perservation to here 
    Qnet[2]=(1-w)*Qmf[1,2] + (w)*Qmb[2]+persv_obj*perCh1[2]+persv_act*perAct[2]+go_bias1[2]
    
    #softmax first choice
    p1  =exp(beta1*Qnet)/sum(exp(beta1*Qnet)) #Qnet[1]? #p1 = probability for action1 at state1 
    ch1 =sample(1:2,size=1,prob=c(p1[1],p1[2])) #where is p1[2] defined? shouldn't it be 1-p1? #the choice that actually occurred
    #i neglected the ket1 r46
    
    #neglected rows 48-52
    # make transition
    trn  =sample(0:1,size=1,prob=c(.7,.3)) #0-common 1-rare
    state=stmat[ch1,trn+1]
    #neglected row 57 
    
    # second stage choice
    #neglected rows 60-61, 65
    p2  =exp(beta2*Qnet)/sum(exp(beta2*Qnet))
    ch2 =sample(1:2,size=1,prob=c(p2[1],p2[2]))
    
    #generate outcome
    rw.prob=rndwlk[ch2,(state-1),t]
    rw     =sample(0:1,size=1,prob=c(1-rw.prob,rw.prob)) # reward =1 
    
    #update fractal model-free Qvalues
    PE1=(Qmf[state,ch2]-Qmf[1,ch1])
    PE2=(rw-Qmf[state,ch2])
    
    Qmf[1,ch1]    =Qmf[1,ch1]+alpha1[ch1]*PE1+alpha1[ch1]*lambda*PE2
    Qmf[state,ch2]=Qmf[state,ch2]+alpha2*PE2
    
    #neglected rows 82-87
    #update choice perservation parameter
    perCh1[ch1]     =1
    perCh1[3-ch1]   =0
    
    #update action perservation parameter
    if (alpha1[1]==alpha_go){
    go           =(ch1==1)*1 #action perseveration "switch":a1 is the first state object selected at the previos trial. the perseveration value will be added to the Q-value choosen at the previos trial.  will be used at the first state softmax
    ng           =(ch1==2)*1
    } else{
    go           =(ch1==2)*1
    ng           =(ch1==1)*1
    } 
    
    
    #save data
    df=rbind(df,
             data.frame(
               subj =subj,
               experiment  =experiment,
               trl  =t,
               ch1  =ch1,
               ch2  =ch2,
               state=state,
               rw   =rw,
               trn  =trn,
               act = go,
               alpha1_ratio = alpha_go/alpha_ng
#neglected r101-104
             ))
  }
  return(df)
}
