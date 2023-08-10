data {
  
  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         //number of subjects
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    //number of trials left for each subject after data omission
  int<lower = 2> Narms;                                             //number of overall alternatives
  int<lower = 2> Noptions;                                           //number of options per trial
  
  //Behavioral data:
    //each variable being a subject x trial matrix
  //the data is padded in make_standata function so that all subjects will have the same number of trials
  int<lower = 0> ch_key[Nsubjects,Ntrials];        //index of which key was chosen coded 1 to 4
  int<lower = 0> ch_color[Nsubjects,Ntrials];        //index of which color was chosen coded 1 to 2
  int<lower = 0> ch_shape[Nsubjects,Ntrials];         //index of which shape was chosen coded 1 to 2
  int<lower = 0> ch_texture[Nsubjects,Ntrials];         //index of which texture was chosen coded 1 to 2
  int<lower = 0> reward[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> key1[Nsubjects,Ntrials];              //first offered key
  int<lower = 0> key2[Nsubjects,Ntrials];             //second offered key
  int<lower = 0> color1[Nsubjects,Ntrials];            //offered color in first bandit
  int<lower = 0> color2[Nsubjects,Ntrials];            //offered color in second bandit
  int<lower = 0> shape1[Nsubjects,Ntrials];
  int<lower = 0> shape2[Nsubjects,Ntrials];
  int<lower = 0> texture1[Nsubjects,Ntrials];
  int<lower = 0> texture2[Nsubjects,Ntrials];
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials]; 
  int<lower = 0> selected_offer [Nsubjects,Ntrials];
}

transformed data{
  int<lower = 1> Nparameters=6; //number of parameters
  vector[Narms] Q_keys_initial;     // initial values for Qkeys
  vector[Noptions] Q_colors_initial;     // initial values for Qcolors 
  vector[Noptions] Q_shapes_initial;   // initial values for Qshapes
  vector[Noptions] Q_textures_initial;   // initial values for Qshapes
  Q_keys_initial = rep_vector(0.5, Narms);
  Q_colors_initial = rep_vector(0.5, Noptions);
  Q_shapes_initial = rep_vector(0.5, Noptions);
  Q_textures_initial = rep_vector(0.5, Noptions);
}

parameters {
  // Declare parameters vectors. the notation "aux" indicate that the values are before transformation
  
  //population level parameters 
  vector[Nparameters] population_locations;                    //vector with the population level mean for each model parameter
  vector<lower=0>[Nparameters] population_scales;          //vector of random effects variance for each model parameter
  
  //individuals level
  vector[Nsubjects] alpha_key_random_effect;
  vector[Nsubjects] alpha_color_random_effect;
  vector[Nsubjects] alpha_shape_random_effect;
  vector[Nsubjects] alpha_texture_random_effect;
  vector[Nsubjects] beta_key_random_effect;
  vector[Nsubjects] beta_visual_random_effect;
}


transformed parameters {
  //declare variables and parameters
  vector<lower=0, upper=1>[Nsubjects] alpha_key;
  vector<lower=0, upper=1>[Nsubjects] alpha_color;
  vector<lower=0, upper=1>[Nsubjects] alpha_shape;
  vector<lower=0, upper=1>[Nsubjects] alpha_texture;
  vector                  [Nsubjects] beta_key;
  vector                  [Nsubjects] beta_visual;
  matrix                  [Ntrials,Nsubjects] p_ch_action;
  matrix                  [Ntrials,Nsubjects] Qnet_diff;
  matrix                  [Ntrials,Nsubjects] Qkeys_diff;
  matrix                  [Ntrials,Nsubjects] Qcolors_diff;
  matrix                  [Ntrials,Nsubjects] Qshapes_diff;
  matrix                  [Ntrials,Nsubjects] Qtextures_diff;
  vector [Narms] Q_keys;
  vector [Noptions] Q_colors;
  vector [Noptions] Q_shapes;
  vector [Noptions] Q_textures;
  vector[Noptions] Qnet;
 
for (subject in 1:Nsubjects) {
  alpha_key[subject] = inv_logit(population_locations[1]  + population_scales[1] * alpha_key_random_effect[subject]);
  alpha_color [subject] = inv_logit(population_locations[2]  + population_scales[2] * alpha_color_random_effect[subject]);
  alpha_shape [subject] = inv_logit(population_locations[3]  + population_scales[3] * alpha_shape_random_effect[subject]);
  alpha_texture [subject] = inv_logit(population_locations[4]  + population_scales[4] * alpha_texture_random_effect[subject]);
  beta_key [subject] =          (population_locations[5]  + population_scales[5] * beta_key_random_effect[subject]);
  beta_visual  [subject] =       (population_locations[6]  + population_scales[6] * beta_visual_random_effect[subject]);
  for (trial in 1:Ntrials_per_subject[subject]){
  if (first_trial_in_block[subject,trial] == 1) {
      Q_keys=Q_keys_initial;
      Q_colors=Q_colors_initial;
      Q_shapes=Q_shapes_initial;
      Q_textures=Q_textures_initial;
    }
          Qnet[1]=beta_key[subject]*Q_keys[key1[subject,trial]]+beta_visual[subject]*(Q_colors[color1[subject,trial]]+Q_shapes[shape1[subject,trial]]+Q_textures[texture1[subject,trial]]); //We compound all the values of bandit1

          Qnet[2]=beta_key[subject]*Q_keys[key2[subject,trial]]+beta_visual[subject]*(Q_colors[color2[subject,trial]]+Q_shapes[shape2[subject,trial]]+Q_textures[texture2[subject,trial]]); //We compound all the values of bandit2
        
        //likelihood function
        Qkeys_diff[trial,subject]= Q_keys[key2[subject,trial]]-Q_keys[key1[subject,trial]];
        Qcolors_diff[trial,subject]= Q_colors[color2[subject,trial]]-Q_colors[color1[subject,trial]];
        Qshapes_diff[trial,subject]= Q_shapes[shape2[subject,trial]]-Q_shapes[shape1[subject,trial]];
        Qtextures_diff[trial,subject]= Q_textures[texture2[subject,trial]]-Q_textures[texture1[subject,trial]];
        Qnet_diff[trial,subject]  = Qnet[2]-Qnet[1]; //higher values of Qdiff mean higher chance to choose right option.
        
        p_ch_action[trial,subject] = inv_logit(Qnet_diff[trial,subject]);
         //ch_key=1 means choosing right
 Q_keys[ch_key[subject,trial]] += alpha_key[subject] * (reward[subject,trial] - Q_keys[ch_key[subject,trial]]); //update key value according to reward
 Q_colors[ch_color[subject,trial]] += alpha_color[subject] * (reward[subject,trial] - Q_colors[ch_color[subject,trial]]); //update color_value according to reward
 Q_shapes[ch_shape[subject,trial]] += alpha_shape[subject] * (reward[subject,trial] - Q_shapes[ch_shape[subject,trial]]); //update shape value according to reward
 Q_textures[ch_texture[subject,trial]] += alpha_texture[subject] * (reward[subject,trial] - Q_textures[ch_texture[subject,trial]]); //update shape value according to reward
  }
 //Qvalues update

}
}

model {
  
  // population level priors (hyper-parameters)
  population_locations   ~ normal(0,2);
  population_scales      ~ cauchy(0,2);    
  
  // individual level priors (subjects' parameters)
  alpha_key_random_effect ~ std_normal();
  alpha_color_random_effect ~ std_normal();
  alpha_shape_random_effect ~ std_normal();
  alpha_texture_random_effect ~ std_normal();
  beta_key_random_effect ~ std_normal();
  beta_visual_random_effect ~ std_normal();

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    target+= bernoulli_logit_lpmf(selected_offer[subject,]|Qnet_diff[,subject]);
  
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
