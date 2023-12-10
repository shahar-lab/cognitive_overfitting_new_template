data {
  
  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         //number of subjects
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    //number of trials left for each subject after data omission
  int<lower = 2> Narms;                                             //number of overall alternatives
  int<lower = 2> Nraffle;                                           //number of colors per trial
  
  //Behavioral data:
    //each variable being a subject x trial matrix
  //the data is padded in make_standata function so that all subjects will have the same number of trials
  int<lower = 0> ch_color[Nsubjects,Ntrials];        //index of which color was chosen coded 1 to 4
  int<lower = 0> ch_key[Nsubjects,Ntrials];        //index of which color was chosen coded 1 to 4
  int<lower = 0> ch_shape[Nsubjects,Ntrials]; 
  int<lower = 0> reward[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> color_left[Nsubjects,Ntrials];            //offered color in left bandit
  int<lower = 0> color_right[Nsubjects,Ntrials];            //offered color in right bandit
  int<lower = 0> shape_left[Nsubjects,Ntrials];
  int<lower = 0> shape_right[Nsubjects,Ntrials];
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials]; 
  int<lower = 0> selected_offer [Nsubjects,Ntrials];
}

transformed data{
  int<lower = 1> Nparameters=6; //number of parameters
  vector[Narms] Q_colors_initial;     // initial values for Qcolors (defined here to avoid doing this many times across iterations)
  vector[Nraffle] Q_keys_initial;     // initial values for Qkeys
  vector[Nraffle] Q_shapes_initial;   // initial values for Qshapes
  Q_colors_initial = rep_vector(0.5, Narms);
  Q_keys_initial = rep_vector(0.5, Nraffle);
  Q_shapes_initial = rep_vector(0.5, Nraffle);
}

parameters {
  // Declare parameters vectors. the notation "aux" indicate that the values are before transformation
  
  //population level parameters 
  vector[Nparameters] population_locations;                    //vector with the population level mean for each model parameter
  vector<lower=0>[6] population_scales;          //vector of random effects variance for each model parameter
  
  //individuals level
  vector[Nsubjects] alpha_color_random_effect;
  vector[Nsubjects] alpha_key_random_effect;
  vector[Nsubjects] alpha_shape_random_effect;
  vector[Nsubjects] beta_color_random_effect;
  vector[Nsubjects] beta_key_random_effect;
  vector[Nsubjects] beta_shape_random_effect;
}


transformed parameters {
  //declare variables and parameters
  vector<lower=0, upper=1>[Nsubjects] alpha_color;
  vector<lower=0, upper=1>[Nsubjects] alpha_key;
  vector<lower=0, upper=1>[Nsubjects] alpha_shape;
  vector                  [Nsubjects] beta_color;
  vector                  [Nsubjects] beta_key;
  vector                  [Nsubjects] beta_shape;
  matrix                  [Ntrials,Nsubjects] p_ch_action;
  matrix                  [Ntrials,Nsubjects] Qnet_diff;
  matrix                  [Ntrials,Nsubjects] Qcolors_diff;
  matrix                  [Ntrials,Nsubjects] Qkeys_diff;
  matrix                  [Ntrials,Nsubjects] Qshapes_diff;
  vector [Narms] Q_colors;
  vector [Nraffle] Q_keys;
  vector [Nraffle] Q_shapes;
  vector[Nraffle] Qnet;
 
for (subject in 1:Nsubjects) {
  alpha_color[subject] = inv_logit(population_locations[1]  + population_scales[1] * alpha_color_random_effect[subject]);
  alpha_key [subject] = inv_logit(population_locations[2]  + population_scales[2] * alpha_key_random_effect[subject]);
  alpha_shape [subject] = inv_logit(population_locations[3]  + population_scales[3] * alpha_shape_random_effect[subject]);
  beta_color [subject] =          (population_locations[4]  + population_scales[4] * beta_color_random_effect[subject]);
  beta_key  [subject] =          (population_locations[5]  + population_scales[5] * beta_key_random_effect[subject]);
  beta_shape  [subject] =          (population_locations[6]  + population_scales[6] * beta_shape_random_effect[subject]);
  for (trial in 1:Ntrials_per_subject[subject]){
  if (first_trial_in_block[subject,trial] == 1) {
      Q_colors=Q_colors_initial;
      Q_keys=Q_keys_initial;
      Q_shapes=Q_shapes_initial;
    }
          Qnet[1]=beta_color[subject]*Q_colors[color_left[subject,trial]]+beta_key[subject]*Q_keys[1]+beta_shape[subject]*Q_shapes[shape_left[subject,trial]]; //We compound the value of the color appearing on the left and the value of the left key.

          Qnet[2]=beta_color[subject]*Q_colors[color_right[subject,trial]]+beta_key[subject]*Q_keys[2]+beta_shape[subject]*Q_shapes[shape_right[subject,trial]];
        
        //likelihood function
        Qcolors_diff[trial,subject]= Q_colors[color_right[subject,trial]]-Q_colors[color_left[subject,trial]];
        Qkeys_diff[trial,subject] = Q_keys[2]-Q_keys[1];
        Qshapes_diff[trial,subject] = Q_shapes[2]-Q_shapes[1]; //this is not equivalent to the former Qdiffs
        Qnet_diff[trial,subject]  = Qnet[2]-Qnet[1]; //higher values of Qdiff mean higher chance to choose right option.
        
        p_ch_action[trial,subject] = inv_logit(Qnet_diff[trial,subject]);
         //ch_key=1 means choosing right
 Q_colors[ch_color[subject,trial]] += alpha_color[subject] * (reward[subject,trial] - Q_colors[ch_color[subject,trial]]); //update color_value according to reward
 Q_keys[ch_key[subject,trial]] += alpha_key[subject] * (reward[subject,trial] - Q_keys[ch_key[subject,trial]]); //update key value according to reward
 Q_shapes[ch_shape[subject,trial]] += alpha_shape[subject] * (reward[subject,trial] - Q_shapes[ch_shape[subject,trial]]); //update shape value according to reward
  }
 //Qvalues update

}
}

model {
  
  // population level priors (hyper-parameters)
  population_locations   ~ normal(0,2);
  population_scales      ~ cauchy(0,2);    
  
  // individual level priors (subjects' parameters)
  alpha_color_random_effect ~ std_normal();
  alpha_key_random_effect ~ std_normal();
  alpha_shape_random_effect ~ std_normal();
  beta_color_random_effect ~ std_normal();
  beta_key_random_effect ~ std_normal();
  beta_shape_random_effect ~ std_normal();

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    target+= bernoulli_logit_lpmf(selected_offer[subject,]|Qnet_diff[,subject]);
  
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
