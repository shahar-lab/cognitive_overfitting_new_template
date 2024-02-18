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
    expvalues <- data.frame(exp1 = c(0.1, 0.2, 0.3, 0.4,0,0),  # Fill the first column with values 1 to 4
                     exp0 = c(0.5, 0.5, 0.5, 0.5,0,0))  # Fill the second column with letters A to D
    
    Q_cards= rep(0, Narms+2)
    Q_keys = rep(0, Nraffle)
    
    for (trial in 1:Ntrials_perblock){
      if (trial%%3==0){
        catch_trial=1
        options=sample(5:6,Nraffle)
        Qnet = Q_keys
        p= exp(beta * Qnet) / sum(exp(beta * Qnet)) #get prob for each action
        #players choice
        ch_card = sample(options, 1, prob = p) #chose a card according to probs
        ch_key = which(options == ch_card) #get key of chosen card 1 =left
        unch_key = which(options!=ch_card)
        #outcome 
        reward =9999
      }
      else{
        catch_trial=0
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

      Qnet = weights[1]*Q_cards_offered + weights[2]*Q_keys
      
      p= exp(beta * Qnet) / sum(exp(beta * Qnet)) #get prob for each action
      #players choice
      ch_card = sample(options, 1, prob = p) #chose a card according to probs
      unch_card=options[which(options != ch_card)]
      ch_key = which(options == ch_card) #get key of chosen card 1 =left
      unch_key = which(options!=ch_card)
      #outcome 
      reward = sample(c(1,0,-1), 1, prob = c(expvalues[ch_card, 1], expvalues[ch_card, 2],1-(expvalues[ch_card, 1]+ expvalues[ch_card, 2]))) #reward according to card
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
        Q_unch_card = Q_cards[options[which(options != ch_card)]],
        Q_right_card = Q_cards[options[2]],
        Q_left_card = Q_cards[options[1]],
        Q_left_key = Q_keys[1],
        Q_right_key = Q_keys[2],
        exp_val1_right=expvalues[options[2], 1],
        exp_val1_left=expvalues[options[1], 1],
        exp_val0_right=expvalues[options[2], 2],
        exp_val0_left=expvalues[options[1], 2],
        exp_val1_ch = expvalues[ch_card, 1],
        exp_val1_unch = expvalues[options[which(options != ch_card)], 1],
        exp_val0_ch = expvalues[ch_card, 2],
        exp_val0_unch = expvalues[options[which(options != ch_card)], 2],
        Q_ch_key = Q_keys[ch_key],
        Q_unch_key = Q_keys[which(options != ch_card)],
        PE_cards,
        PE_keys,
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