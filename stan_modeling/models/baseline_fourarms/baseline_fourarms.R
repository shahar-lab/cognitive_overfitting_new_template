#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
#pre-allocation
  
  #set parameters
  alpha = parameters['alpha']
  beta  = parameters['beta']

  
  #set initial var
  Narms              = cfg$Narms
  Noptions            = cfg$Noptions
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues          = cfg$rndwlk
  rownames(expvalues)=c('ev1','ev2','ev3','ev4')
  Qval               = as.matrix(t(rep(0,Narms)))
  colnames(Qval)     =sapply(1:Narms, function(n) {paste('Qbandit',n,sep="")})
  df                 =data.frame()
  
for (block in 1:Nblocks){
  
  Qval      = as.matrix(t(rep(0,Narms)))
  
  for (trial in 1:Ntrials_perblock){

    #computer offer
    options    = sample(1:Narms,Noptions,prob=rep(1/Narms,Narms)) 
    options    = sort(options)
    
    #players choice
    p         = exp(beta*Qval[options]) / sum(exp(beta*Qval[options]))
    choice    = sample(options,1,prob=p)
    unchosen  = options[choice!=options]
    
    #outcome 
    reward = sample(0:1,1,prob=c(1-expvalues[choice,trial],expvalues[choice,trial]))
    
    #save trial's data
    
      #create data for current trials
      dfnew=data.frame(
            subject              = subject,
            block                = block,
            trial                = trial,
            first_trial_in_block = (trial==1)*1,
            choice               = choice,
            selected_offer       = (choice==options[2])*1,
            unchosen             = unchosen,
            offer1               = options[1],
            offer2               = options[2],
            expval_ch            = expvalues[choice,trial],
            expval_unch          = expvalues[options[choice!=options],trial],
            reward               = reward
            )
      
      dfnew=cbind(dfnew,Qval)
      dfnew=cbind(dfnew,t(t(expvalues)[trial,]))
      
      #bind to the overall df
      df=rbind(df,dfnew)
       
    
    
    #updating Qvalues
    Qval[choice] = Qval[choice] + alpha*(reward - Qval[choice])
  }
}     
  return (df)
}