data {
  
  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         //number of subjects
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    //number of trials left for each subject after data omission
  int<lower = 2> Narms;                                             //number of overall alternatives
  int<lower = 2> Noptions;                                           //number of cards per trial
  
  //Behavioral data:
    //each variable being a subject x trial matrix
  //the data is padded in make_standata function so that all subjects will have the same number of trials
  int<lower = 0> ch_card[Nsubjects,Ntrials];        //index of which card was chosen coded 1 to 4
  int<lower = 0> ch_key[Nsubjects,Ntrials];        //index of which card was chosen coded 1 to 4
  int<lower = 0> reward[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> card_left[Nsubjects,Ntrials];            //offered card in left bandit
  int<lower = 0> card_right[Nsubjects,Ntrials];            //offered card in right bandit
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials]; 
  int<lower = 0> selected_offer [Nsubjects,Ntrials];
  int<lower = 0> block [Nsubjects,Ntrials];
  int catch_trial [Nsubjects,Ntrials];
  int condition_difficulty [Nsubjects,Ntrials];
  int condition_scarcity [Nsubjects,Ntrials];
}

transformed data{
  int<lower = 1> Nparameters=6; //number of parameters
  vector[Narms+2] Q_cards_initial;     // initial values for Qcards (defined here to avoid doing this many times across iterations)
  vector[Noptions] Q_keys_initial;     // initial values for Qkeys
  Q_cards_initial = rep_vector(0.5, Narms+2);
  Q_keys_initial = rep_vector(0.5, Noptions);
}

parameters {
  // Declare parameters vectors. the notation "aux" indicate that the values are before transformation
  
  //population level parameters 
  vector[Nparameters] population_locations;                    //vector with the population level mean for each model parameter
  vector<lower=0>[Nparameters] population_scales;          //vector of random effects variance for each model parameter
  
  //individuals level
  vector[Nsubjects] alpha_random_effect;
  vector[Nsubjects] beta_random_effect;
  vector[Nsubjects] omega_random_effect;
  vector[Nsubjects] omega_difficulty_random_effect;
  vector[Nsubjects] omega_scarcity_random_effect;
  vector[Nsubjects] omega_interaction_random_effect;
}


transformed parameters {
  //declare variables and parameters
  vector<lower=0, upper=1>[Nsubjects] alpha;
  vector                  [Nsubjects] beta;
  vector                  [Nsubjects] omega; //key choice dependence
  vector                  [Nsubjects] omega_difficulty; //key choice dependence
  vector                  [Nsubjects] omega_scarcity; //key choice dependence
  vector                  [Nsubjects] omega_interaction; //key choice dependence
  matrix                  [Ntrials,Nsubjects] Qnet_diff;
  
for (subject in 1:Nsubjects) {
  
  matrix [Ntrials,Nsubjects] PE_cards;
  matrix [Ntrials,Nsubjects] PE_keys;
  vector [Narms+2] Q_cards;
  vector [Noptions] Q_keys;
  vector [Noptions] Qnet;
  real omega_total;
  
  alpha[subject]     =    inv_logit(population_locations[1]  + population_scales[1] * alpha_random_effect[subject]);
  beta [subject]     =             (population_locations[2]  + population_scales[2] * beta_random_effect[subject]);
  omega[subject]     =    inv_logit(population_locations[3]  + population_scales[3] * omega_random_effect[subject]);
  omega_difficulty [subject]     = (population_locations[4]  + population_scales[4] * omega_difficulty_random_effect[subject]);
  omega_scarcity   [subject]     = (population_locations[5]  + population_scales[5] * omega_scarcity_random_effect[subject]);
  omega_interaction[subject]     = (population_locations[6]  + population_scales[6] * omega_interaction_random_effect[subject]);
  
  for (trial in 1:Ntrials_per_subject[subject]){
  if (first_trial_in_block[subject,trial] == 1) {
      Q_cards=Q_cards_initial;
      Q_keys=Q_keys_initial;
    }
          omega_total=omega[subject]+condition_difficulty[subject,trial]*omega_difficulty[subject]+condition_scarcity[subject,trial]*omega_scarcity[subject]+condition_difficulty[subject,trial]*condition_scarcity[subject,trial]*omega_interaction[subject];
          Qnet[1]=Q_cards[card_left[subject,trial]]+omega_total*Q_keys[1]; //We compound the value of the card appearing on the left and the value of the left key.

          Qnet[2]=Q_cards[card_right[subject,trial]]+omega_total*Q_keys[2];
        
  //likelihood function
  Qnet_diff[trial,subject]  = Qnet[2]-Qnet[1]; //higher values of Qdiff mean higher chance to choose right option.
  
  //Qvalues update
  //ch_key=1 means choosing right
 if(catch_trial[subject,trial]==0){
 PE_cards[trial,subject]          =reward[subject,trial] - Q_cards[ch_card[subject,trial]];
 PE_keys [trial,subject]          =reward[subject,trial] - Q_keys[ch_key[subject,trial]];
 Q_cards [ch_card[subject,trial]] += alpha[subject] * PE_cards[trial,subject]; //update card_value according to reward
 Q_keys  [ch_key[subject,trial]]  += alpha[subject] * PE_keys [trial,subject]; //update key value according to reward
 }
  }
 
}
}

model {
  
  // population level priors (hyper-parameters)
  population_locations   ~ normal(0,2);
  population_scales      ~ cauchy(0,2);    
  
  // individual level priors (subjects' parameters)
  alpha_random_effect ~ std_normal();
  beta_random_effect ~ std_normal();
  omega_random_effect ~ std_normal();
  omega_difficulty_random_effect ~ std_normal();
  omega_scarcity_random_effect ~ std_normal();
  omega_interaction_random_effect ~ std_normal();

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    for (trial in 1:Ntrials_per_subject[subject]){
    target+= bernoulli_logit_lpmf(selected_offer[subject,trial]|beta[subject]*Qnet_diff[trial,subject]);
    }
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
