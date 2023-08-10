#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
  #pre-allocation
  
  #set parameters
  alpha_color = parameters['alpha_color']
  alpha_key = parameters['alpha_key']
  alpha_shape = parameters['alpha_shape']
  alpha_texture = parameters['alpha_texture']
  beta_key = parameters['beta_key']
  beta_visual  = parameters['beta_visual']
  #set initial var
  Narms              = cfg$Narms
  Noptions           = cfg$Noptions
  Nblocks            = cfg$Nblocks
  Ntrials_perblock   = cfg$Ntrials_perblock
  expvalues          = cfg$rndwlk
  rownames(expvalues)=c('ev1','ev2','ev3','ev4')
  df                 =data.frame()
  Qnet               =c()
  for (block in 1:Nblocks){
    
    Q_keys= rep(0.5, Narms)
    Q_colors = rep(0.5, Noptions)
    Q_shapes = rep(0.5, Noptions)
    Q_textures = rep(0.5, Noptions)
    for (trial in 1:Ntrials_perblock){
      
      #computer offer
      key_options= sample(1:Narms,Noptions) #sample two offered keys - first index is key1 and goes with color1 etc.
      colors     = sample(1:Noptions,Noptions) #sample color for each key - first index for key1 and second for key2
      shapes     = sample(1:Noptions,Noptions) #sample shape for each key
      textures    = sample(1:Noptions,Noptions) #sample texture for each key
      #value of offered colors
      Q_keys_offered = Q_keys[key_options] #use their Q values
      Qnet[1] = beta_key*Q_keys_offered[1]+beta_visual*(Q_colors[colors[1]] +Q_shapes[shapes[1]]+Q_textures[textures[1]])
      Qnet[2] = beta_key*Q_keys_offered[2]+ beta_visual*(Q_colors[colors[2]]+Q_shapes[shapes[2]]+Q_textures[textures[2]])
      p= exp(Qnet) / sum(exp(Qnet)) #get prob for each action
      #players choice
      ch_key = sample(key_options, 1, prob = p) #choose a key according to probs [1-4]
      unch_key= key_options[key_options!=ch_key]
      ch_option = which(ch_key==key_options) #option 1 or 2
      unch_option = which(ch_key!=key_options)
      ch_color=colors[ch_option] #according to chosen option, find out which color is there.
      ch_shape=shapes[ch_option] #according to chosen option, find out which shape is there.
      ch_texture=textures[ch_option] #according to chosen option, find out which texture is there.
      unch_color=colors[unch_option] #according to chosen option, find out which color is there.
      unch_shape=shapes[unch_option] #according to chosen option, find out which shape is there.
      unch_texture=textures[unch_option] #according to chosen option, find out which texture is there.
      
      #outcome 
      reward = sample(0:1, 1, prob = c(1 - expvalues[ch_key, trial], expvalues[ch_key, trial])) #reward according to color
      
      #save trial's data
      
      #create data for current trials
      dfnew=data.frame(
        subject,
        block,
        trial,
        key1 = key_options[1],
        key2 = key_options[2],
        color1 = colors[1],
        color2 = colors[2],
        shape1 = shapes[1],
        shape2 = shapes[2],
        texture1 = textures[1],
        texture2 = textures[2],
        ch_key,
        ch_color,
        ch_shape,
        ch_texture,
        selected_offer=ch_option-1,
        reward,
        Q_ch_key = Q_keys[ch_key], 
        Q_unch_key = Q_keys[unch_key],
        Q_ch_color = Q_colors[ch_color], 
        Q_unch_color = Q_colors[unch_color],
        Q_ch_shape = Q_shapes[ch_shape], 
        Q_unch_shape = Q_shapes[unch_shape],
        Q_ch_texture = Q_textures[ch_texture], 
        Q_unch_texture = Q_textures[unch_texture],
        exp_val1=expvalues[key_options[1], trial],
        exp_val2=expvalues[key_options[2], trial],
        exp_val_ch = expvalues[ch_key, trial],
        exp_val_unch = expvalues[unch_key, trial],
        alpha_key,
        alpha_color,
        alpha_shape,
        alpha_texture,
        beta_key,
        beta_visual
      )
      df=rbind(df,dfnew)
      #updating Qvalues
      Q_keys[ch_key] = Q_keys[ch_key] + alpha_key * (reward - Q_keys[ch_key])
      Q_colors[ch_color] = Q_colors[ch_color] + alpha_color * (reward - Q_colors[ch_color])
      Q_shapes[ch_shape] = Q_shapes[ch_shape] + alpha_shape * (reward - Q_shapes[ch_shape])
      Q_textures[ch_texture] = Q_textures[ch_texture] + alpha_texture * (reward - Q_textures[ch_texture])
    }
  }     
  
  return (df)
}