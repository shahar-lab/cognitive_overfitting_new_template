data {
  
  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         //number of subjects
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    //number of trials left for each subject after data omission
  int<lower = 2> Narms;                                             //number of overall alternatives
  int<lower = 2> Nraffle;                                           //number of cards per trial
  real testfold;
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
  int<lower = 0> fold[Nsubjects,Ntrials];
}

transformed data{
  int<lower = 1> Nparameters=3; //number of parameters
}

parameters {
  // Declare parameters vectors. the notation "aux" indicate that the values are before transformation
  
  //population level parameters 
  vector[Nparameters] population_locations;                    //vector with the population level mean for each model parameter
  vector<lower=0>[Nparameters] population_scales;          //vector of random effects variance for each model parameter
  
  //individuals level
  vector[Nsubjects] alpha_random_effect;
  vector[Nsubjects] beta_random_effect;
  vector[Nsubjects] rho_random_effect;
}


transformed parameters {
  //declare variables and parameters
  vector<lower=0, upper=1>[Nsubjects] alpha;
  vector                  [Nsubjects] beta;
  vector<lower=0, upper=1>[Nsubjects]rho; 
  matrix[Ntrials,Nsubjects] weight_key;
  matrix[Nsubjects,Ntrials] PE_card;
  matrix[Nsubjects,Ntrials] PE_key;
  matrix                  [Ntrials,Nsubjects] p_ch_action;
  matrix                  [Ntrials,Nsubjects] Qnet_diff;
  matrix                  [Ntrials,Nsubjects] Qcards_diff;
  matrix                  [Ntrials,Nsubjects] Qkeys_diff;
  vector [Narms] Q_cards;
  vector [Nraffle] Q_keys;
  vector [Nraffle] Qnet;
for (subject in 1:Nsubjects) {

  alpha[subject]     = inv_logit(population_locations[1]  + population_scales[1] * alpha_random_effect[subject]);
  beta [subject] =              (population_locations[2]  + population_scales[2] * beta_random_effect[subject]);
  rho[subject]     = inv_logit(population_locations[3]  + population_scales[3] * rho_random_effect[subject]);
  
  for (trial in 1:Ntrials_per_subject[subject]){
    
    if(fold[subject,trial]!=testfold){
      
  if (first_trial_in_block[subject,trial] == 1) {
      Q_cards=rep_vector(0.5, Narms);
      Q_keys=rep_vector(0.5, Nraffle);
      weight_key[trial,subject]=0;
    }
          Qnet[1]=Q_cards[card_left[subject,trial]]+weight_key[trial,subject]*Q_keys[1]; //We compound the value of the card appearing on the left and the value of the left key.

          Qnet[2]=Q_cards[card_right[subject,trial]]+weight_key[trial,subject]*Q_keys[2];
        
        //likelihood function
        Qnet_diff[trial,subject]  = Qnet[2]-Qnet[1]; //higher values of Qdiff mean higher chance to choose right option.

 
 //calculating PEs
 PE_card[subject,trial]=reward[subject,trial] - Q_cards[ch_card[subject,trial]];
 PE_key[subject,trial]=reward[subject,trial] - Q_keys[ch_key[subject,trial]];
  //Update values ch_key=1 means choosing right
 Q_cards[ch_card[subject,trial]] += alpha[subject] * (PE_card[subject,trial]); //update card_value according to reward
 Q_keys[ch_key[subject,trial]] += alpha[subject] * (PE_key[subject,trial]); //update key value according to reward
 
//Update weights
if(trial!=Ntrials_per_subject[subject]){
 weight_key[trial+1,subject]=weight_key[trial,subject]+rho[subject]*(PE_key[subject,trial]-PE_card[subject,trial]);
}

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
  rho_random_effect ~ std_normal();
  beta_random_effect ~ std_normal();

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial
  for (subject in 1:Nsubjects){
    for (trial in 1:Ntrials_per_subject[subject]){
      if(fold[subject,trial]!=testfold){
      target+= bernoulli_logit_lpmf(selected_offer[subject,trial] | beta[subject] * Qnet_diff[trial,subject]);
    }
    }
  }
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
generated quantities {

  matrix[Ntrials,Nsubjects]  log_lik;
  matrix[Ntrials,Nsubjects] weight_key_g;
  matrix[Nsubjects,Ntrials] PE_card_g;
  matrix[Nsubjects,Ntrials] PE_key_g;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial
log_lik=rep_matrix(0,Ntrials,Nsubjects);
    for (subject in 1:Nsubjects) {
   vector[Narms] Qcards_g;
  vector[Nraffle] Qkeys_g;
  vector[Nraffle] Qnet_g;
        for (trial in 1:Ntrials_per_subject[subject]){
         
         if(fold[subject,trial] == testfold) {
 
      //reset Qvalues (first trial only)
  if (first_trial_in_block[subject,trial] == 1) {
      Qcards_g=rep_vector(0.5, Narms);
      Qkeys_g=rep_vector(0.5, Nraffle);
      weight_key_g[trial,subject]=0;
    }
          Qnet_g[1]=Qcards_g[card_left[subject,trial]]+weight_key_g[trial,subject]*Qkeys_g[1]; //We compound the value of the card appearing on the left and the value of the left key.

          Qnet_g[2]=Qcards_g[card_right[subject,trial]]+weight_key_g[trial,subject]*Qkeys_g[2];
        
        log_lik[trial,subject] = bernoulli_logit_lpmf(selected_offer[subject, trial] | beta[subject] * Qnet_g);

 
 //calculating PEs
 PE_card_g[subject,trial]=reward[subject,trial] - Qcards_g[ch_card[subject,trial]];
 PE_key_g[subject,trial]=reward[subject,trial] - Qkeys_g[ch_key[subject,trial]];
  //Update values ch_key=1 means choosing right
 Qcards_g[ch_card[subject,trial]] += alpha[subject] * (PE_card_g[subject,trial]); //update card_value according to reward
 Qkeys_g[ch_key[subject,trial]] += alpha[subject] * (PE_key_g[subject,trial]); //update key value according to reward
 
//Update weights
if(trial!=Ntrials_per_subject[subject]){
 weight_key_g[trial+1,subject]=weight_key_g[trial,subject]+rho[subject]*(PE_key_g[subject,trial]-PE_card_g[subject,trial]);
}
       
        }
        }
    }
}
