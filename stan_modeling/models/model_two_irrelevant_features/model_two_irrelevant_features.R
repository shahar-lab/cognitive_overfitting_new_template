#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
#pre-allocation
  
  #set parameters
  alpha_color = parameters['alpha_color']
  alpha_key = parameters['alpha_key']
  alpha_shape = parameters['alpha_shape']
  beta_color  = parameters['beta_color']
  beta_key  = parameters['beta_key']
  beta_shape = parameters['beta_shape']
  #set initial var
  Narms              = cfg$Narms
  Noptions            = cfg$Noptions
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues          = cfg$rndwlk
  rownames(expvalues)=c('ev1','ev2','ev3','ev4')
  df                 =data.frame()
  Qnet               =c()
for (block in 1:Nblocks){
  
  Q_colors= rep(0.5, Narms)
  Q_keys = rep(0.5, Noptions)
  Q_shapes = rep(0.5, Noptions)
  for (trial in 1:Ntrials_perblock){

    #computer offer
    options    = sample(1:Narms,Noptions) #sample two offered cards and their locations (index 1 is left and 2 is right)
    shapes     = sample(1:Noptions,Noptions) #sample shape for each color, 1 means square, 2 means circle
    shape_left = shapes[1]
    shape_right = shapes[2]
    #value of offered colors
    Q_colors_offered = Q_colors[options] #use their Q values
    Qnet[1] = beta_color*Q_colors_offered[1] + beta_key*Q_keys[1]+beta_shape*Q_shapes[shape_left]
    Qnet[2] = beta_color*Q_colors_offered[2] + beta_key*Q_keys[2]+beta_shape*Q_shapes[shape_right]
    p= exp(Qnet) / sum(exp(Qnet)) #get prob for each action
    #players choice
    ch_color = sample(options, 1, prob = p) #chose a color according to probs
    ch_key = which(options == ch_color) #get key of chosen color 1 =left
    ch_shape=shapes[ch_key] #according to chosen key, find out which shape is there.
    unch_shape=ifelse(ch_key==1,shapes[2],shapes[1])
    #outcome 
    reward = sample(0:1, 1, prob = c(1 - expvalues[ch_color, trial], expvalues[ch_color, trial])) #reward according to color
    
    #save trial's data
    
      #create data for current trials
      dfnew=data.frame(
          subject,
          block,
          trial,
          color_left = options[1],
          color_right = options[2],
          shape_left = shapes[1],
          shape_right = shapes[2],
          ch_color,
          ch_key,
          ch_shape,
          selected_offer=ch_key-1,
          reward,
          Q_ch_color = Q_colors[ch_color], #to get previous trial
          Q_unch_color = Q_colors[options[which(options != ch_color)]],
          Q_right_color = Q_colors[options[2]],
          Q_left_color = Q_colors[options[1]],
          exp_val_right=expvalues[options[2], trial],
          exp_val_left=expvalues[options[1], trial],
          exp_val_ch = expvalues[ch_color, trial],
          exp_val_unch = expvalues[options[which(options != ch_color)], trial],
          Q_ch_key = Q_keys[ch_key],
          Q_unch_key = Q_keys[which(options != ch_color)],
          Q_ch_shape = Q_shapes[ch_shape],
          unch_shape,
          Q_unch_shape = Q_shapes[unch_shape],
          alpha_color,
          alpha_key,
          alpha_shape,
          beta_color,
          beta_key,
          beta_shape
        )
      df=rbind(df,dfnew)
    #updating Qvalues
    Q_colors[ch_color] = Q_colors[ch_color] + alpha_color * (reward - Q_colors[ch_color])
    Q_keys[ch_key] = Q_keys[ch_key] + alpha_key * (reward - Q_keys[ch_key])
    Q_shapes[ch_shape] = Q_shapes[ch_shape] + alpha_shape * (reward -Q_shapes[ch_shape])
  }
}     
  
  return (df)
}