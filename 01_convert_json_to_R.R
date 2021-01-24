rm(list=ls())
library("rjson")
library(data.table)
library(dplyr)
library(tidyr)
library(parallel)
library(tidyverse)

source('01_functions/convert_json_gonogo.R')
source('01_functions/convert_json_gonogo_questions.R')
source('01_functions/convert_json_gonogo_starter.R')
source('01_functions/convert_json_self_reports.R')

mainfolder<-paste(getwd(),'/03_data/01_raw_data',sep="")
subfolder <-dir(mainfolder)
asrs<-wurs<-bis<-bisbas<-data.frame()

print(length(subfolder))

for (i in 1:length(subfolder)){
  print(paste('folder #',i,': ',subfolder[i],sep=""))
  files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))
  asrs    <-con_sr_json('asrs',asrs,curnfolder,files,i)
  wurs    <-con_sr_json('wurs',wurs,curnfolder,files,i)
  bis     <-con_sr_json('bis-',bis,curnfolder,files,i)
  bisbas  <-con_sr_json('bis_bas',bisbas,curnfolder,files,i)
  
  df_task      <-con_gonogo_json('test',df,curnfolder,files,i)
  df_task$outcome <- lapply(df_task$stim_fb,function(x) grepl('_r.png', x)*1)
  df_questions <- con_gonogo_questions_json('test',df,curnfolder,files,i)
  df_starter <- con_gonogo_starter_json('starter',df,curnfolder,files,i)
  
}  


save(wurs,file='03_data/02_aggregated_data/wurs_raw.Rdata')
save(asrs,file='03_data/02_aggregated_data/asrs_raw.Rdata')
save(bis,file='03_data/02_aggregated_data/bis_raw.Rdata')
save(bisbas,file='03_data/02_aggregated_data/bisbas_raw.Rdata')
save(df_task,file='03_data/02_aggregated_data/task_raw.Rdata')
save(df_questions,file='03_data/02_aggregated_data/questions_raw.Rdata')
save(df_starter,file='03_data/02_aggregated_data/starter_raw.Rdata')


