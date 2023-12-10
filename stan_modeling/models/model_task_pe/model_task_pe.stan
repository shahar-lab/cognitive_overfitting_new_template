data {

  //General fixed parameters for the experiment/models

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

  int<lower=1> Nparameters = 4; //number of parameters

}

parameters {

  // Declare parameters vectors. the notation "aux" indicate that the values are before transformation

  

  //population level parameters 

  vector[Nparameters] population_locations; //vector with the population level mean for each model parameter

  vector<lower=0>[Nparameters] population_scales; //vector of random effects variance for each model parameter

  

  //individuals level

  vector[Nsubjects] alpha_random_effect;
  
  vector[Nsubjects] alpha_task_random_effect;
  
  vector[Nsubjects] beta_random_effect;
  
  vector[Nsubjects] eta_random_effect;

}

transformed parameters {

  //declare variables and parameters

  vector<lower=0, upper=1>[Nsubjects] alpha;
  
  vector<lower=0, upper=1>[Nsubjects] alpha_task;

  vector[Nsubjects] beta;
  
  vector <lower=0, upper=1> [Nsubjects] eta;


  vector[Ndims] weights;

  matrix[Ntrials, Nsubjects] weight_key;

  matrix[Ntrials, Nsubjects] weight_card;

  matrix[Nsubjects, Ntrials] PE_card;

  matrix[Nsubjects, Ntrials] PE_key;

  matrix[Nsubjects, Ntrials] PE_task;

  matrix[Ntrials, Nsubjects] Qnet_diff;

  vector[Narms] Q_cards;

  vector[Nraffle] Q_keys;

  vector[Nraffle] Qnet;

  real V_task;
  
  real lambda;
  
  real prior_relevant = 1;
  
  real weight_uniform =1.0/Ndims;
  
  for (subject in 1 : Nsubjects) {

    alpha[subject] = inv_logit(population_locations[1]

                               + population_scales[1]

                               * alpha_random_effect[subject]);
                               
    alpha_task[subject] = inv_logit(population_locations[2]

                                    + population_scales[2]

                                    * alpha_task_random_effect[subject]);


    beta[subject] = (population_locations[3]

                     + population_scales[3] * beta_random_effect[subject]);

    eta[subject] = inv_logit(population_locations[4]

                    + population_scales[4] * eta_random_effect[subject]);



    for (trial in 1 : Ntrials_per_subject[subject]) {

      if (first_trial_in_block[subject, trial] == 1) {

        Q_cards = rep_vector(0.5, Narms);

        Q_keys = rep_vector(0.5, Nraffle);

        V_task = 0.5;

        weights[1] = prior_relevant;

        weights[2] = 1-prior_relevant;
        
        lambda = 1;

      }

      Qnet[1] = weights[1] * Q_cards[card_left[subject, trial]]

                + weights[2] * Q_keys[1]; //We compound the value of the card appearing on the left and the value of the left key.

      

      Qnet[2] = weights[1] * Q_cards[card_right[subject, trial]]

                + weights[2] * Q_keys[2];

      

      //likelihood function

      Qnet_diff[trial, subject] = Qnet[2] - Qnet[1]; //higher values of Qdiff mean higher chance to choose right option.

      

      //calculating PEs

      PE_card[subject, trial] = reward[subject, trial]

                                - Q_cards[ch_card[subject, trial]];

      PE_key[subject, trial] = reward[subject, trial]

                               - Q_keys[ch_key[subject, trial]];

      PE_task[subject, trial] = reward[subject, trial] - V_task;

      //Update values ch_key=1 means choosing right

      Q_cards[ch_card[subject, trial]] += alpha[subject]

                                          * (PE_card[subject, trial]); //update card_value according to reward

      Q_keys[ch_key[subject, trial]] += alpha[subject]

                                        * (PE_key[subject, trial]); //update key value according to reward

      V_task += alpha_task[subject] * (PE_task[subject, trial]); //update general value

      

      //store weights

      weight_card[trial, subject] = weights[1];
      weight_key[trial, subject] = weights[2];


      //updating weights
      lambda = lambda + eta[subject] * ((1 - abs(PE_task[subject,trial]))-lambda);
      weights[1]= lambda* prior_relevant + (1 - lambda) * weight_uniform;
      weights[2]= 1-weights[1];

    }

  }

}

model {

  // population level priors (hyper-parameters)

  population_locations ~ normal(0, 2);

  population_scales ~ normal(0, 2);

  // individual level priors (subjects' parameters)

  alpha_random_effect ~ std_normal();
  
  alpha_task_random_effect ~ std_normal();

  beta_random_effect ~ std_normal();

  eta_random_effect ~ std_normal();

  

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



