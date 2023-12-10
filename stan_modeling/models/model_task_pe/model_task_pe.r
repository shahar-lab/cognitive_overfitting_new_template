#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
  #pre-allocation
  
  #set parameters
  alpha = parameters['alpha']
  alpha_task = parameters['alpha_task']
  beta  = parameters['beta']
  eta = parameters['eta']
  #set initial var
  Narms              = cfg$Narms
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  Ndims              =cfg$Ndims
  #expvalues          = cfg$rndwlk
  #rownames(expvalues)=c('ev1','ev2','ev3','ev4')
  df                 =data.frame()
  prior_relevant=1
  weight_uniform=1/Ndims
  for (block in 1:Nblocks){
    
    expvalues=t(data.frame(a=rep(0.2,Ntrials_perblock),b=rep(0.8,Ntrials_perblock),c=rep(0.2,Ntrials_perblock),d=rep(0.8,Ntrials_perblock)))

    Q_cards= rep(0.5, Narms)
    Q_keys = rep(0.5, Nraffle)
    V_task=0.5
    weights=c(prior_relevant,1-prior_relevant)
    lambda=1
    for (trial in 1:Ntrials_perblock){
      #computer offer
      pair  = sample(1:2,1)
      if(pair==1){
        options=sample(1:2,2)
      }
      else{
        options=sample(3:4,2)
      }

      #value of offered cards
      Q_cards_offered = Q_cards[options] #use their Q values
      Qnet = Q_cards_offered*weights[1] + Q_keys*weights[2]

      p= exp(beta * Qnet) / sum(exp(beta * Qnet)) #get prob for each action
      #players choice
      ch_card = sample(options, 1, prob = p) #chose a card according to probs
      ch_key = which(options == ch_card) #get key of chosen card 1 =left
      
      
      #outcome 
      reward = sample(0:1, 1, prob = c(1 - expvalues[ch_card, trial], expvalues[ch_card, trial])) #reward according to card
      
      #calc PE
      PE_keys= reward-Q_keys[ch_key]
      PE_cards=reward-Q_cards[ch_card]
      PE_task=reward-V_task
      PE_total=reward-Qnet
      #save trial's data
      
      #create data for current trials
      dfnew=data.frame(
        subject,
        block,
        trial,
        card_right = options[2],
        card_left = options[1],
        ch_card,
        ch_key,
        selected_offer=ch_key-1,
        reward,
        Q_ch_card = Q_cards[ch_card], #to get previous trial
        Q_unch_card = Q_cards[options[which(options != ch_card)]],
        Q_right_card = Q_cards[options[2]],
        Q_left_card = Q_cards[options[1]],
        Q_left_key = Q_keys[1],
        Q_right_key = Q_keys[2],
        Q_card_1   = Q_cards[1],
        Q_card_2   = Q_cards[2],
        Q_card_3   = Q_cards[3],
        Q_card_4   = Q_cards[4],
        offer1     =options[1],
        offer2     =options[2],
        exp_val_right=expvalues[options[2], trial],
        exp_val_left=expvalues[options[1], trial],
        exp_val_ch = expvalues[ch_card, trial],
        exp_val_unch = expvalues[options[which(options != ch_card)], trial],
        Q_ch_key = Q_keys[ch_key],
        Q_unch_key = Q_keys[which(options != ch_card)],
        PE_cards,
        PE_keys,
        PE_task,
        V_task,
        alpha,
        alpha_task,
        eta,
        beta,
        lambda,
        weight_card=weights[1],
        weight_key=weights[2]
      )
      df=rbind(df,dfnew)
      #updating Qvalues
      
      Q_cards[ch_card] = Q_cards[ch_card] + alpha * PE_cards
      Q_keys [ch_key] = Q_keys[ch_key] +alpha * PE_keys
      V_task = V_task +alpha_task * PE_task
      
      #updating weights
      lambda = lambda + eta * ((1 - abs(PE_task)) - lambda)
      
      weights[1]= lambda* prior_relevant + (1 - lambda) * weight_uniform
      weights[2]= 1-weights[1]
    }
  }     
  
  return (df)
}
    