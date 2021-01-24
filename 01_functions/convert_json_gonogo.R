
########################################################################
################ convert json to Rdata  ################################
####################################################################
#this converts choice reaction time task from jason to data.frame

#note that here prolific id is taken from the starter file

con_gonogo_json<-function(task_name,datafile,curnfolder,files,subnum) {

    if (sum(grepl(task_name,files))>0) {
    #x - will hold the raw converted json 
    x <- fromJSON(fromJSON(file=paste(curnfolder,'/',files[grepl(task_name,files)],sep=""))$data)
    
    #prolific id
    prolific_id <- fromJSON(file=paste(curnfolder,'/',files[grepl('starter',files)],sep=""))$`data[prolific-id]`

    #create first_stage
    i <-sapply(1:length(x), function(i) {(x[[i]]$trial_id=='first_stage')}) 
    df_1st<-
      data.frame(
      subj       =rep(subnum,sum(i)),
      prolific_id=rep(prolific_id,sum(i)),
      trial_num  =unlist(lapply(x[i],function(xx) {xx[names(xx)=='trial_num']})),
      block      =unlist(lapply(x[i],function(xx) {xx[names(xx)=='exp_stage']})),
      stim_1       =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stimulus']}))[seq(1,816,3)],
      stim_2       =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stimulus']}))[seq(2,816,3)],
      stim_3       =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stimulus']}))[seq(3,816,3)],
      rt         =unlist(lapply(x[i],function(xx) {xx[names(xx)=='rt']})),
      key        =unlist(lapply(x[i],function(xx) {xx[names(xx)=='key_press']})),
      trial_id   =unlist(lapply(x[i],function(xx) {xx[names(xx)=='trial_id']})),
      stim_order_1   =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stim_order']}))[seq(1,544,2)],
      stim_order_2   =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stim_order']}))[seq(2,544,2)],
      stim_selected_1 =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stim_selected']})),
      stage =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stage']}))
      
)
   
    #create second_stage
    i <-sapply(1:length(x), function(i) {(x[[i]]$trial_id=='second_stage')}) 
    df_2nd<-
      data.frame(
        trial_num_2nd  =unlist(lapply(x[i],function(xx) {xx[names(xx)=='trial_num']})),
        block_2nd      =unlist(lapply(x[i],function(xx) {xx[names(xx)=='exp_stage']})),
        stim_2nd       =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stimulus']})),
        rt_2nd         =unlist(lapply(x[i],function(xx) {xx[names(xx)=='rt']})),
        key_2nd        =unlist(lapply(x[i],function(xx) {xx[names(xx)=='key_press']})),
        trial_id_2nd   =unlist(lapply(x[i],function(xx) {xx[names(xx)=='trial_id']})),
        stim_order_1_2nd   =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stim_order']}))[seq(1,544,2)],
        stim_order_2_2nd   =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stim_order']}))[seq(2,544,2)],
        stim_selected_2nd =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stim_selected']})),
        stage_2nd =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stage']}))
      )
    
    #feedback
    i <-sapply(1:length(x), function(i) {(x[[i]]$trial_id=='feedback_stage')}) 
    df_fb<-
      data.frame(
        trial_num_fb  =unlist(lapply(x[i],function(xx) {xx[names(xx)=='trial_num']})),
        block_fb      =unlist(lapply(x[i],function(xx) {xx[names(xx)=='exp_stage']})),
        stim_fb       =unlist(lapply(x[i],function(xx) {xx[names(xx)=='stimulus']})),
        rt_fb         =unlist(lapply(x[i],function(xx) {xx[names(xx)=='rt']})),
        trial_id_fb   =unlist(lapply(x[i],function(xx) {xx[names(xx)=='trial_id']}))
      )
 
    
    return(cbind(df_1st,df_2nd,df_fb))
    #return(rbind(datafile,df_1st))
  }
  else{
    return(datafile)
  }
}


