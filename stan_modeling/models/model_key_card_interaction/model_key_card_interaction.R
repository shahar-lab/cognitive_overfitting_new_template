#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
#pre-allocation
  
  #set parameters
  alpha_card = parameters['alpha_card']
  alpha_key = parameters['alpha_key']
  alpha_key_card = parameters['alpha_key_card']
  beta_card  = parameters['beta_card']
  beta_key  = parameters['beta_key']
  beta_key_card  = parameters['beta_key_card']
  
  #set initial var
  Narms              = cfg$Narms
  Noptions            = cfg$Noptions
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues          = cfg$rndwlk
  rownames(expvalues)=c('ev1','ev2','ev3','ev4')
  df                 =data.frame()
  Q_key_cards_offered=c()
for (block in 1:Nblocks){
  
  Q_cards= rep(0.5, Narms)
  Q_keys = rep(0.5, Noptions)
  Q_key_cards = matrix(rep(0.5, 8),ncol=4)
  for (trial in 1:Ntrials_perblock){

    #computer offer
    options    = sample(1:Narms,Noptions) 

    #value of offered cards
    Q_cards_offered = Q_cards[options] #use their Q values
    Q_key_cards_offered[1] = Q_key_cards[1,options[1]]
    Q_key_cards_offered[2] = Q_key_cards[2,options[2]]
    Qnet = beta_card*Q_cards_offered + beta_key*Q_keys+beta_key_card*Q_key_cards_offered
    
    p= exp(Qnet) / sum(exp(Qnet)) #get prob for each action
    #players choice
    ch_card = sample(options, 1, prob = p) #chose a card according to probs
    ch_key = which(options == ch_card) #get key of chosen card 1 =left

    
    #outcome 
    reward = sample(0:1, 1, prob = c(1 - expvalues[ch_card, trial], expvalues[ch_card, trial])) #reward according to card
    
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
          exp_val_right=expvalues[options[2], trial],
          exp_val_left=expvalues[options[1], trial],
          exp_val_ch = expvalues[ch_card, trial],
          exp_val_unch = expvalues[options[which(options != ch_card)], trial],
          Q_ch_key = Q_keys[ch_key],
          Q_unch_key = Q_keys[which(options != ch_card)],
          alpha_card,
          alpha_key,
          alpha_key_card,
          beta_card,
          beta_key,
          beta_key_card
        )
      df=rbind(df,dfnew)
      
    #updating Qvalues
    Q_cards[ch_card] = Q_cards[ch_card] + alpha_card * (reward - Q_cards[ch_card])
    Q_keys[ch_key] = Q_keys[ch_key] + alpha_key * (reward - Q_keys[ch_key])
    Q_key_cards[ch_key,ch_card] = Q_key_cards[ch_key,ch_card] + alpha_key_card * (reward - Q_key_cards[ch_key,ch_card])
  }
}     
  
  return (df)
}