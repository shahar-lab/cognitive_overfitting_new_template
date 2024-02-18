data {
  int<lower=1> Nsubjects; //number of subjects
  
  int<lower=1> Nblocks;
  
  int<lower=1> Ntrials; //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  
  array[Nsubjects] int<lower=1> Ntrials_per_subject; //number of trials left for each subject after data omission
  
  int<lower=2> Narms; //number of overall alternatives
  
  int<lower=2> Nraffle; //number of cards per trial
  
  int<lower=2> Ndims; //number of dimensions
  
  real testfold;
  
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
  
  array[Nsubjects, Ntrials] int<lower=0> fold;
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
  
  vector[Nsubjects] alpha_random_effect;
  
  vector[Nsubjects] beta_random_effect;
  
  vector[Nsubjects] lambda0_random_effect;
}
transformed parameters {
  //declare variables and parameters
  
  vector<lower=0, upper=1>[Nsubjects] alpha;
  
  vector[Nsubjects] beta;
  
  vector[Nsubjects] lambda0;
  
  vector[Ndims] weights;
  
  matrix[Ntrials, Nsubjects] weight_key;
  
  matrix[Ntrials, Nsubjects] weight_card;
  
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
  
  vector[Ndims] prior_relevant;
  
  vector[Ndims] weight_uniform;
  
  real total_weights;
  
  real lambda;
  
  real transformed_lambda;
  
  prior_relevant[1] = 1;
  
  prior_relevant[2] = 0;
  
  weight_uniform[1] = 1.0 / Ndims;
  
  weight_uniform[2] = 1.0 / Ndims;
  
  for (subject in 1 : Nsubjects) {
    alpha[subject] = inv_logit(population_locations[1]
                               + population_scales[1]
                                 * alpha_random_effect[subject]);
    
    beta[subject] = (population_locations[2]
                     + population_scales[2] * beta_random_effect[subject]);
    
    lambda0[subject] = (population_locations[3]
                        + population_scales[3]
                          * lambda0_random_effect[subject]);
    
    //store weights
    
    //updating weights
    
    lambda = lambda0[subject];
    
    transformed_lambda = inv_logit(lambda);
    
    weights[1] = transformed_lambda * prior_relevant[1]
                 + (1 - transformed_lambda) * weight_uniform[1];
    
    weights[2] = transformed_lambda * prior_relevant[2]
                 + (1 - transformed_lambda) * weight_uniform[2];
    
    for (trial in 1 : Ntrials_per_subject[subject]) {
      if (fold[subject, trial] != testfold) {
        if (first_trial_in_block[subject, trial] == 1) {
          Q_cards = rep_vector(0.5, Narms);
          
          Q_keys = rep_vector(0.5, Nraffle);
        }
        
        Qnet[1] = weights[1] * Q_cards[card_left[subject, trial]]
                  + weights[2] * Q_keys[1]; //We compound the value of the card appearing on the left and the value of the left key.
        
        Qnet[2] = weights[1] * Q_cards[card_right[subject, trial]]
                  + weights[2] * Q_keys[2];
        
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
        
        Q_cards[ch_card[subject, trial]] += alpha[subject]
                                            * (PE_card[subject, trial]); //update card_value according to reward
        
        Q_keys[ch_key[subject, trial]] += alpha[subject]
                                          * (PE_key[subject, trial]); //update key value according to reward
        
        weight_card[trial, subject] = weights[1];
        
        weight_key[trial, subject] = weights[2];
      }
    }
  }
}
model {
  // population level priors (hyper-parameters)
  
  population_locations ~ normal(0, 3);
  
  population_scales ~ normal(0, 3);
  
  // individual level priors (subjects' parameters)
  
  alpha_random_effect ~ std_normal();
  
  beta_random_effect ~ std_normal();
  
  lambda0_random_effect ~ std_normal();
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  //Likelihood function per subject per trial
  
  for (subject in 1 : Nsubjects) {
    for (trial in 1 : Ntrials_per_subject[subject]) {
      if (fold[subject, trial] != testfold) {
        target += bernoulli_logit_lpmf(selected_offer[subject, trial] | beta[subject]
                                                                    * Qnet_diff[trial, subject]);
      }
    }
  }
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

generated quantities {
  matrix[Ntrials, Nsubjects] log_lik;
  
  matrix[Nsubjects, Ntrials] PE_card_g;
  
  matrix[Nsubjects, Ntrials] PE_key_g;
  
  matrix[Ndims, Nsubjects] weights_g;
  
  vector[Ndims] prior_relevant_g;
  
  vector[Ndims] weight_uniform_g;
  
  vector[Nsubjects] transformed_lambda_g;
  
  prior_relevant_g[1] = 1;
  
  prior_relevant_g[2] = 0;
  
  weight_uniform_g[1] = 1.0 / Ndims;
  
  weight_uniform_g[2] = 1.0 / Ndims;
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  //Likelihood function per subject per trial
  
  log_lik = rep_matrix(0, Ntrials, Nsubjects);
  
  for (subject in 1 : Nsubjects) {
    transformed_lambda_g[subject] = inv_logit(lambda0[subject]);
    
    weights_g[1, subject] = transformed_lambda_g[subject]
                            * prior_relevant_g[1]
                            + (1 - transformed_lambda_g[subject])
                              * weight_uniform_g[1];
    
    weights_g[2, subject] = transformed_lambda_g[subject]
                            * prior_relevant_g[2]
                            + (1 - transformed_lambda_g[subject])
                              * weight_uniform_g[2];
    
    vector[Narms] Qcards_g;
    
    vector[Nraffle] Qkeys_g;
    
    vector[Nraffle] Qnet_g;
    
    real Qnet_diff_g;
    
    for (trial in 1 : Ntrials_per_subject[subject]) {
      if (fold[subject, trial] == testfold) {
        //reset Qvalues (first trial only)
        
        if (first_trial_in_block[subject, trial] == 1) {
          Qcards_g = rep_vector(0.5, Narms);
          
          Qkeys_g = rep_vector(0.5, Nraffle);
        }
        
        Qnet_g[1] = weights_g[1, subject]
                    * Qcards_g[card_left[subject, trial]]
                    + weights_g[2, subject] * Qkeys_g[1]; //We compound the value of the card appearing on the left and the value of the left key.
        
        Qnet_g[2] = weights_g[1, subject]
                    * Qcards_g[card_right[subject, trial]]
                    + weights_g[2, subject] * Qkeys_g[2];
        
        Qnet_diff_g = Qnet_g[2] - Qnet_g[1];
        
        log_lik[trial, subject] = bernoulli_logit_lpmf(selected_offer[subject, trial] | beta[subject]
                                                                    * Qnet_diff_g);
        
        //calculating PEs
        
        PE_card_g[subject, trial] = reward[subject, trial]
                                    - Qcards_g[ch_card[subject, trial]];
        
        PE_key_g[subject, trial] = reward[subject, trial]
                                   - Qkeys_g[ch_key[subject, trial]];
        
        //Update values ch_key=1 means choosing right
        
        Qcards_g[ch_card[subject, trial]] += alpha[subject]
                                             * (PE_card_g[subject, trial]); //update card_value according to reward
        
        Qkeys_g[ch_key[subject, trial]] += alpha[subject]
                                           * (PE_key_g[subject, trial]); //update key value according to reward
      }
    }
  }
}


