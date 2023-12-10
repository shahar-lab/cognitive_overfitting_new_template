#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
#pre-allocation
  
  #set parameters
  alpha_card = parameters['alpha_card']
  alpha_key = parameters['alpha_key']
  beta_card  = parameters['beta_card']
  beta_key  = parameters['beta_key']
  
  #set initial var
  Narms              = cfg$Narms
  Noptions            = cfg$Noptions
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues          = cfg$rndwlk
  rownames(expvalues)=c('ev1','ev2','ev3','ev4')
  df                 =data.frame()
  
for (block in 1:Nblocks){
  
  Q_cards= rep(0.5, Narms)
  Q_keys = rep(0.5, Narms)
  for (trial in 1:Ntrials_perblock){

    #computer offer
    card_options    = sample(1:Narms,Noptions) 
    #sample location for cards
    key_options     = sample(1:Narms,Noptions)
    #value of offered cards
    Q_cards_offered = Q_cards[card_options] #use their Q values
    #value of offered keys
    Q_keys_offered = Q_keys[key_options] #use their Q values
    Qnet = beta_card*Q_cards_offered + beta_key*Q_keys_offered
    
    p= exp(Qnet) / sum(exp(Qnet)) #get prob for each action
    #players choice
    ch_card = sample(card_options, 1, prob = p) #chose a card according to probs
    ch_key = key_options[card_options == ch_card] #get key of chosen card 

    
    #outcome 
    reward = sample(0:1, 1, prob = c(1 - expvalues[ch_card, trial], expvalues[ch_card, trial])) #reward according to card
    
    #save trial's data
    
      #create data for current trials
      dfnew=data.frame(
          subject,
          block,
          trial,
          card_left = card_options[1],
          card_right = card_options[2],
          first_location =key_options[1] ,
          second_location =key_options[2] ,
          ch_card,
          ch_key,
          selected_offer=(ch_card==card_options[2])*1,
          reward,
          Q_ch_card = Q_cards[ch_card], #to get previous trial
          Q_unch_card = Q_cards[card_options[which(card_options != ch_card)]],
          Q_right_card = Q_cards[card_options[2]],
          Q_left_card = Q_cards[card_options[1]],
          exp_val_right=expvalues[card_options[2], trial],
          exp_val_left=expvalues[card_options[1], trial],
          exp_val_ch = expvalues[ch_card, trial],
          exp_val_unch = expvalues[card_options[which(card_options != ch_card)], trial],
          Q_ch_key = Q_keys_offered[ch_key],
          Q_unch_key = Q_keys_offered[which(card_options != ch_card)],
          alpha_card,
          alpha_key,
          beta_card,
          beta_key
        )
      df=rbind(df,dfnew)
    #updating Qvalues
    Q_cards[ch_card] = Q_cards[ch_card] + alpha_card * (reward - Q_cards[ch_card])
    Q_keys[ch_key] = Q_keys[ch_key] + alpha_key * (reward - Q_keys[ch_key])
  }
}     
  
  return (df)
}