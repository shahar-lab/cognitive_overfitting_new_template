#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
  #pre-allocation
  
  #set parameters
  alpha = parameters['alpha']
  beta  = parameters['beta']
  rho = parameters['rho']
  # eta = parameters['eta']
  #set initial var
  Narms              = cfg$Narms
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  #expvalues          = cfg$rndwlk
  #rownames(expvalues)=c('ev1','ev2','ev3','ev4')
  df                 =data.frame()
  for (block in 1:Nblocks){
    if(block%%2==0){
    expvalues=t(data.frame(a=rep(0.1,Ntrials_perblock),b=rep(0.9,Ntrials_perblock),c=rep(0.1,Ntrials_perblock),d=rep(0.9,Ntrials_perblock)))
    }
    else{
    expvalues=t(data.frame(a=rep(0.4,Ntrials_perblock),b=rep(0.5,Ntrials_perblock),c=rep(0.4,Ntrials_perblock),d=rep(0.5,Ntrials_perblock)))
    }
    Q_cards= rep(0.5, Narms)
    Q_keys = rep(0.5, Nraffle)
    weights=c(0,1) #key,card

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


      Qnet = weights[2]*Q_cards_offered + weights[1]*Q_keys
      
      p= exp(beta * Qnet) / sum(exp(beta * Qnet)) #get prob for each action
      #players choice
      ch_card = sample(options, 1, prob = p) #chose a card according to probs
      ch_key = which(options == ch_card) #get key of chosen card 1 =left
      
      
      #outcome 
      reward = sample(0:1, 1, prob = c(1 - expvalues[ch_card, trial], expvalues[ch_card, trial])) #reward according to card
      
      #calc PE
      PE_keys= reward-Q_keys[ch_key]
      PE_cards=reward-Q_cards[ch_card]
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
        exp_val_right=expvalues[options[2], trial],
        exp_val_left=expvalues[options[1], trial],
        exp_val_ch = expvalues[ch_card, trial],
        exp_val_unch = expvalues[options[which(options != ch_card)], trial],
        Q_ch_key = Q_keys[ch_key],
        Q_unch_key = Q_keys[which(options != ch_card)],
        PE_cards,
        PE_keys,
        alpha,
        beta,
        rho,
        #eta,
        weight_key=weights[1],
        weight_card=weights[2]
      )
      df=rbind(df,dfnew)
      #updating Qvalues

      Q_cards[ch_card] = Q_cards[ch_card] + alpha * PE_cards
      Q_keys[ch_key] = Q_keys[ch_key] +alpha * PE_keys
      
      #updating weights
      weights[1] = weights[1] + rho * (abs(PE_cards))
      weights[2] = weights[2] + rho * (abs(PE_keys))
      
      weights=weights^1/sum(weights^1) 
   
    }
  }     
  
  return (df)
}