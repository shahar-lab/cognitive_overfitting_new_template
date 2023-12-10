#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
  #pre-allocation
  
  #set parameters
  eta = parameters['eta']
  beta  = parameters['beta']
  alpha = parameters['alpha']
  omega = parameters['omega']
  #set initial var
  Narms              = cfg$Narms
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  Ndims              = cfg$Ndims
  Nfeatures          = cfg$Nfeatures
  df                 =data.frame()
  p_r_c_f=matrix(rep(0,Ndims*Nfeatures),Ndims,Nfeatures)

  expvalues=t(data.frame(a=rep(0.1,Ntrials_perblock),b=rep(0.12,Ntrials_perblock)))
  for (block in 1:Nblocks){
    cumulative_reward  =0
    probability_reward =0.5
    p_f= matrix(rep(0,Ndims*Nfeatures),Ndims,Nfeatures)
    p_f[1,]=2
    p_f[2,]=0.1
    p_d=rep(0,Ndims)
    phi=rep(0,Ndims)
    Q_cards= rep(0.5, Nraffle)
    Q_keys = rep(0.5, Nraffle)

    for (trial in 1:Ntrials_perblock){

      #players choice
      options=sample(1:2,2) #shuffle the two cards between the keys
      
      #Update dimension weights based on a summation of feature weights
      p_d[1]=sum(unlist(p_f[1,]))
      p_d[2]=sum(unlist(p_f[2,]))
      
      #Normalizing p_d
      phi=p_d^alpha/sum(p_d^alpha) 
      Q_cards_offered = Q_cards[options]
      Qnet=phi[1]*Q_cards_offered+phi[2]*Q_keys #integrate Qcard of right option with Qcard of left option
      p= exp(beta * Qnet) / sum(exp(beta * Qnet)) #get prob for each action
      
      ch_card = sample(options, 1, prob = p) #chose a card according to probs
      unch_card=ifelse(ch_card==1,2,1)
      ch_key = which(options == ch_card) #get key of chosen card 1 =left
      unch_key=ifelse(ch_key==1,2,1)
      
      #outcome 
      reward = sample(0:1, 1, prob = c(1 - expvalues[ch_card, trial], expvalues[ch_card, trial])) #reward according to card
      cumulative_reward=cumulative_reward+reward
      probability_reward=0.7*probability_reward+0.3*(cumulative_reward/trial)
      #calc PE
      PE=reward-Qnet[ch_key]
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
        PE,
        alpha,
        beta,
        eta,
        phi_card=phi[1],
        phi_key=phi[2],
        p_card_1=p_f[1,1],
        p_card_2=p_f[1,2],
        p_key_1=p_f[2,1],
        p_key_2=p_f[2,2]
      )
      df=rbind(df,dfnew)
      #updating Qvalues

      Q_cards[ch_card] = Q_cards[ch_card] + eta*phi[1] * PE
      Q_keys[ch_key] = Q_keys[ch_key] +eta *phi[2]* PE
      Q_cards[unch_card] = (1-omega)*Q_cards[unch_card]
      Q_keys[unch_key] = (1-omega)*Q_keys[unch_key]
      #calculate feature values and weights
      
      #update feautre values 
      #(eq.4)
      
      #Featrues included in stimulus -> Sct(f)=1
      p_r_c_f[1,ch_card]=probability_reward*reward+(1-probability_reward)*(1-reward)
      p_r_c_f[2,ch_key]=probability_reward*reward+(1-probability_reward)*(1-reward)
      
      #Features not included in stimulus-> Sct(f)=0
      p_r_c_f[1,unch_card]=(1-probability_reward*reward)+probability_reward*(1-reward)
      p_r_c_f[2,unch_key]=(1-probability_reward*reward)+probability_reward*(1-reward)
    
      #eq.6
      #update the probability that the feature is good

      p_f = p_f * p_r_c_f
      }
  }     
  
  return (df)
}