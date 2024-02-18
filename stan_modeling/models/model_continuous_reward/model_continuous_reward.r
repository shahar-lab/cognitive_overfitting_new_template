#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
  #pre-allocation
  
  #set parameters
  alpha_relevant = inv_logit_scaled(parameters['alpha_relevant'])
  alpha_irrelevant = inv_logit_scaled(parameters['alpha_irrelevant'])
  beta  = parameters['beta']
  lambda0 = parameters['lambda0']
  #set initial var
  Narms              = cfg$Narms
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  Ndims              = cfg$Ndims
  #expvalues          = cfg$rndwlk
  #rownames(expvalues)=c('ev1','ev2','ev3','ev4')
  df                 =data.frame()
  prior_relevant=c(1,0)
  weight_uniform=rep(1/Ndims,Ndims)
  lambda=lambda0 
  transformed_lambda=inv_logit_scaled(lambda)
  weights= transformed_lambda* prior_relevant + (1 - transformed_lambda) * weight_uniform
  for (block in 1:Nblocks){
    expvalues=t(data.frame(a=rep(-1,Ntrials_perblock),b=rep(1,Ntrials_perblock),c=rep(-1,Ntrials_perblock),d=rep(1,Ntrials_perblock)))

    Q_cards= rep(0, Narms+4)
    Q_keys = rep(0, Narms)
    
    for (trial in 1:Ntrials_perblock){
      if (trial%%3==0){
        catch_trial=1
        options=sample(5:8,Nraffle)
        keys = sample(1:4,Nraffle)
        Qnet = Q_keys[keys]
        p= exp(beta * Qnet) / sum(exp(beta * Qnet)) #get prob for each action
        #players choice
        ch_card = sample(options, 1, prob = p) #chose a card according to probs
        ch_key = if_else(ch_card==options[1],keys[1],keys[2]) #get key of chosen card 1 =left
        #outcome 
        reward =9999
      }
      else{
        catch_trial=0
      #computer offer
      options=sample(1:4,Nraffle)
      keys = sample(1:4,Nraffle)
      #value of offered cards
      Q_cards_offered = Q_cards[options] #use their Q values
      Q_keys_offered = Q_keys[keys]
      Qnet = weights[1]*Q_cards_offered + weights[2]*Q_keys_offered
      
      p= exp(beta * Qnet) / sum(exp(beta * Qnet)) #get prob for each action
      #players choice
      ch_card = sample(options, 1, prob = p) #chose a card according to probs
      ch_key = if_else(ch_card==options[1],keys[1],keys[2]) #get key of chosen card 1 =left
      unch_key = if_else(ch_card==options[1],keys[2],keys[1])
      #outcome 
      #reward = sample(0:1, 1, prob = c(1 - expvalues[ch_card, trial], expvalues[ch_card, trial])) #reward according to card
      reward = rnorm(1,mean=expvalues[ch_card, trial],sd=2)
      }
      #calc PE
      PE_keys= reward-Q_keys[ch_key]
      PE_cards=reward-Q_cards[ch_card]
      
      #save trial's data
      
      #create data for current trials
      dfnew=data.frame(
        subject,
        block,
        trial,
        catch_trial=catch_trial,
        card_right = options[2],
        card_left = options[1],
        ch_card,
        ch_key,
        selected_offer=ch_key-1,
        reward,
        Q_ch_card = Q_cards[ch_card], #to get previous trial
        Q_right_card = Q_cards[options[2]],
        Q_left_card = Q_cards[options[1]],
        Q_key1 = Q_keys_offered[1],
        Q_key2 = Q_keys_offered[2],
        Q_ch_key = Q_keys[ch_key],
        PE_cards,
        PE_keys,
        p_high=if_else(p[1]>p[2],p[1],p[2]),
        alpha_relevant,
        alpha_irrelevant,
        beta,
        lambda0,
        transformed_lambda=transformed_lambda,
        weight_card=weights[1],
        weight_key=weights[2]
      )
      df=rbind(df,dfnew)
      #updating Qvalues
      if (catch_trial==0){
     
      Q_cards[ch_card] = Q_cards[ch_card]  + alpha_relevant * PE_cards
      #Q_cards[unch_card]=Q_cards[unch_card]+alpha *(1-reward-Q_cards[unch_card])

      Q_keys[ch_key] = Q_keys[ch_key] +alpha_irrelevant * PE_keys
      
     # Q_keys[unch_key]=Q_keys[unch_key]+alpha*(1-reward-Q_keys[unch_key])
      }
    }
  }     
  
  return (df)
}