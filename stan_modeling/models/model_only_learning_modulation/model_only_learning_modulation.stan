data {
  int<lower=1> Nsubjects; //number of subjects
  
  int<lower=1> Nblocks;
  
  int<lower=1> Ntrials; //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  
  array[Nsubjects] int<lower=1> Ntrials_per_subject; //number of trials left for each subject after data omission
  
  int<lower=2> Narms; //number of overall alternatives
  
  int<lower=2> Nraffle; //number of cards per trial
  
  int<lower=2> Ndims; //number of dimensions
  
  //Behavioral data:
  
  //each variable being a subject x trial matrix
  
  //the data is padded in make_standata function so that all subjects will have the same number of trials
  
  array[Nsubjects, Ntrials] int<lower=0> ch_card; //index of which card was chosen coded 1 to 4
  
  array[Nsubjects, Ntrials] int<lower=0> ch_key; //index of which card was chosen coded 1 to 4
  
  array[Nsubjects, Ntrials] int<lower=0> reward; //outcome of bandit arm pull
  
  array[Nsubjects, Ntrials] int<lower=0> card_left; //offered card in left bandit
  
  array[Nsubjects, Ntrials] int<lower=0> card_right; //offered card in right bandit
  
  array[Nsubjects, Ntrials] int<lower=0> first_trial_in_block;
  
  array[Nsubjects, Ntrials] int<lower=0> selected_offer;
}
transformed data {
  int<lower=1> Nparameters = 3; //number of parameters
}
parameters {
  // Declare parameters vectors. the notation "aux" indicate that the values are before transformation
  
  //population level parameters 
  
  vector[Nparameters] population_locations; //vector with the population level mean for each model parameter
  
  vector<lower=0>[Nparameters] population_scales; //vector of random effects variance for each model parameter
  
  //individuals level
  
  vector[Nsubjects] alpha_relevant_random_effect;
  
  vector[Nsubjects] alpha_irrelevant_random_effect;
  
  vector[Nsubjects] beta_random_effect;
}
transformed parameters {
  //declare variables and parameters
  
  vector<lower=0, upper=1>[Nsubjects] alpha_relevant;
  
  vector<lower=0, upper=1>[Nsubjects] alpha_irrelevant;
  
  vector[Nsubjects] beta;
  
  matrix[Nsubjects, Ntrials] PE_card;
  
  matrix[Nsubjects, Ntrials] PE_key;
  
  matrix[Ntrials, Nsubjects] PE_card_ext;
  
  matrix[Ntrials, Nsubjects] PE_key_ext;
  
  matrix[Ntrials, Nsubjects] Qnet_diff;
  
  matrix[Ntrials, Nsubjects] Qcard_left_ext;
  
  matrix[Ntrials, Nsubjects] Qcard_right_ext;
  
  matrix[Ntrials, Nsubjects] Qkey_left_ext;
  
  matrix[Ntrials, Nsubjects] Qkey_right_ext;
  
  vector[Narms] Q_cards;
  
  vector[Nraffle] Q_keys;
  
  vector[Nraffle] Qnet;
  
  for (subject in 1 : Nsubjects) {
    alpha_relevant[subject] = inv_logit(population_locations[1]
                                        + population_scales[1]
                                          * alpha_relevant_random_effect[subject]);
    
    alpha_irrelevant[subject] = inv_logit(population_locations[2]
                                          + population_scales[2]
                                            * alpha_irrelevant_random_effect[subject]);
    
    beta[subject] = (population_locations[3]
                     + population_scales[3] * beta_random_effect[subject]);
    
    for (trial in 1 : Ntrials_per_subject[subject]) {
      if (first_trial_in_block[subject, trial] == 1) {
        Q_cards = rep_vector(0.5, Narms);
        
        Q_keys = rep_vector(0.5, Nraffle);
      }
      
      Qnet[1] = Q_cards[card_left[subject, trial]] + Q_keys[1]; //We compound the value of the card appearing on the left and the value of the left key.
      
      Qnet[2] = Q_cards[card_right[subject, trial]] + Q_keys[2];
      
      Qcard_left_ext[trial, subject] = Q_cards[card_left[subject, trial]];
      
      Qcard_right_ext[trial, subject] = Q_cards[card_right[subject, trial]];
      
      Qkey_left_ext[trial, subject] = Q_keys[1];
      
      Qkey_right_ext[trial, subject] = Q_keys[2];
      
      //likelihood function
      
      Qnet_diff[trial, subject] = Qnet[2] - Qnet[1]; //higher values of Qdiff mean higher chance to choose right option.
      
      //calculating PEs
      
      PE_card[subject, trial] = reward[subject, trial]
                                - Q_cards[ch_card[subject, trial]];
      
      PE_key[subject, trial] = reward[subject, trial]
                               - Q_keys[ch_key[subject, trial]];
      
      PE_card_ext[trial, subject] = PE_card[subject, trial];
      
      PE_key_ext[trial, subject] = PE_key[subject, trial];
      
      //Update values ch_key=1 means choosing right
      
      Q_cards[ch_card[subject, trial]] += alpha_relevant[subject]
                                          * (PE_card[subject, trial]); //update card_value according to reward
      
      Q_keys[ch_key[subject, trial]] += alpha_irrelevant[subject]
                                        * (PE_key[subject, trial]); //update key value according to reward
    }
  }
}
model {
  // population level priors (hyper-parameters)
  
  population_locations ~ normal(0, 3);
  
  population_scales ~ normal(0, 3);
  
  // individual level priors (subjects' parameters)
  
  alpha_relevant_random_effect ~ std_normal();
  
  alpha_irrelevant_random_effect ~ std_normal();
  
  beta_random_effect ~ std_normal();
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  //Likelihood function per subject per trial
  
  for (subject in 1 : Nsubjects) {
    for (trial in 1 : Ntrials_per_subject[subject]) {
      target += bernoulli_logit_lpmf(selected_offer[subject, trial] | beta[subject]
                                                                    * Qnet_diff[trial, subject]);
    }
  }
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}


