#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
  #pre-allocation
  
  #set parameters
  alpha = parameters['alpha']
  beta  = parameters['beta']
  omega_key = 0
  omega_card = 1
  rho_omega =0.1
  # omega_difficulty = parameters ["omega_difficulty"]
  # omega_scarcity = parameters ["omega_scarcity"]
  # omega_interaction = parameters["omega_interaction"]
  # omega_difficulty = 0
  # omega_scarcity = 0
  # omega_interaction = 0
  
  #set initial var
  Narms              = cfg$Narms
  Noptions           = cfg$Noptions
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  df                 =data.frame()

  for (block in 1:Nblocks){
    if (block==1|block==5){
      expvalues=t(data.frame(a=rep(0.6,Ntrials_perblock),b=rep(0.9,Ntrials_perblock),c=rep(0.5,Ntrials_perblock),d=rep(0.9,Ntrials_perblock)))
      condition_difficulty=0
      condition_scarcity=0
    }
    else if(block==2|block==6){
      expvalues=t(data.frame(a=rep(0.1,Ntrials_perblock),b=rep(0.4,Ntrials_perblock),c=rep(0.1,Ntrials_perblock),d=rep(0.5,Ntrials_perblock)))
      condition_difficulty=0
      condition_scarcity=1
    }
    else if(block==3|block==7){
      expvalues=t(data.frame(a=rep(0.7,Ntrials_perblock),b=rep(0.8,Ntrials_perblock),c=rep(0.65,Ntrials_perblock),d=rep(0.75,Ntrials_perblock)))
      condition_difficulty=1
      condition_scarcity=0
    }
    else{
      expvalues=t(data.frame(a=rep(0.2,Ntrials_perblock),b=rep(0.3,Ntrials_perblock),c=rep(0.25,Ntrials_perblock),d=rep(0.35,Ntrials_perblock)))
      condition_difficulty=1
      condition_scarcity=1
    }
    #omega_total = omega+condition_scarcity*omega_scarcity+condition_difficulty*omega_difficulty+condition_difficulty*condition_scarcity*omega_interaction
    expvalues = rbind(expvalues,matrix(0.5,2,Ntrials_perblock))

    Q_cards= rep(0.5, Narms+Noptions) #2catch options
    Q_keys = rep(0.5, Noptions)
    for (trial in 1:Ntrials_perblock){
      #catch trials
      if (trial%%4==0){
        catch_trial=1
        options=sample(5:6,Noptions)
        #value of offered cards
        Q_cards_offered = Q_cards[options] #use their Q values
        Qnet = omega_card*Q_cards_offered + omega_key*Q_keys
        
        p= exp(beta * Qnet) / sum(exp(beta * Qnet)) #get prob for each action
        #players choice
        ch_card = sample(options, 1, prob = p) #chose a card according to probs
        ch_key = which(options == ch_card) #get key of chosen card 1 =left
        #outcome 
        reward =9999
      }
      else{
        catch_trial=0
        #computer offer, only pairs are offered
        pair  = sample(1:2,1)
        if(pair==1){
          options=sample(1:2,2)
        }
        else{
          options=sample(3:4,2)
        }
        
        #value of offered cards
        Q_cards_offered = Q_cards[options] #use their Q values
        Qnet = omega_card*Q_cards_offered + omega_key*Q_keys
        
        p= exp(beta * Qnet) / sum(exp(beta * Qnet)) #get prob for each action
        #players choice
        ch_card = sample(options, 1, prob = p) #chose a card according to probs
        ch_key = which(options == ch_card) #get key of chosen card 1 =left
        #outcome 
        reward = sample(0:1, 1, prob = c(1 - expvalues[ch_card, trial], expvalues[ch_card, trial])) #reward according to card
      }
      #save trial's data
      block_num=block #block is conserved in stan
      #create data for current trials
      dfnew=data.frame(
        subject,
        block_num,
        trial,
        catch_trial = catch_trial,
        card_right = options[2],
        card_left = options[1],
        ch_card,
        ch_key,
        selected_offer=ch_key-1,
        reward,
        Q_ch_card = Q_cards[ch_card], #to get previous trial
        Q_unch_card = Q_cards[options[which(options != ch_card)]],
        PE_card=if (trial %%4!=0) {PE_card =reward - Q_cards[ch_card]} else {PE_card =0},
        Q_right_card = Q_cards[options[2]],
        Q_left_card = Q_cards[options[1]],
        exp_val_right=expvalues[options[2], trial],
        exp_val_left=expvalues[options[1], trial],
        exp_val_ch = expvalues[ch_card, trial],
        exp_val_unch = expvalues[options[which(options != ch_card)], trial],
        Q_ch_key = Q_keys[ch_key],
        Q_unch_key = Q_keys[which(options != ch_card)],
        Q_right_key = Q_keys[1],
        Q_left_key = Q_keys[2],
        PE_key=if (trial %%4!=0) { PE_key=reward - Q_keys[ch_key]} else {PE_key =0},
        condition_difficulty,
        condition_scarcity,
        alpha,
        beta,
        omega_card,
        omega_key,
        rho_omega
      )
      df=rbind(df,dfnew)
      #updating Qvalues
      if (trial%%4!=0){
        PE_card=reward - Q_cards[ch_card]
        PE_key=reward - Q_keys[ch_key]
        Q_cards[ch_card] = Q_cards[ch_card] + alpha * (PE_card)
        Q_keys[ch_key] = Q_keys[ch_key] + alpha * (PE_key)
      }
      omega_card = omega_card +rho_omega*PE_card
      omega_card = omega_card -rho_omega*PE_key
      omega_key = omega_key +rho_omega*PE_key
      omega_key = omega_key -rho_omega*PE_card
      #omega_card = pmin(pmax(omega_card, 0), 1)
     # omega_key = pmin(pmax(omega_key, 0), 1)
    }
  }     
  
  return (df)
}