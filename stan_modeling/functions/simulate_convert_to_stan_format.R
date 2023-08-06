simulate_convert_to_standata <-function (path,cfg,var_toinclude){

  source('./functions/make_mystandata.R')


  #load artificial data
  load(paste0(path$data,'/artificial_data.Rdata'))


  #convert
  df$fold=df$block
  df=df%>%mutate(first_trial_in_block=(block!=lag(block,default=1))*1)
  df$first_trial_in_block[1]=1
  data_for_stan<-make_mystandata(data                 = df, 
                                 subject_column       = df$subject,
                                 block_column         = df$block,
                                 var_toinclude        = var_toinclude,
                                 additional_arguments = list(
                                   Narms  = cfg$Narms, 
                                   Noptions= cfg$Noptions))

  #save
  save(data_for_stan,file=paste0(path$data,'/artificial_standata.Rdata'))
  cat(paste0('[stan_modeling]:  "artificial_standata.Rdata" was saved at "',path$data,'"'))
  
}