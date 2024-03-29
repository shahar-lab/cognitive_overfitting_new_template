data {

  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    
  int<lower = 2> Narms;                                             
  int<lower = 2> Nraffle;                                           


  //Behavioral data:
  int<lower = 0> choice[Nsubjects,Ntrials];        
  int<lower = 0> unchosen[Nsubjects,Ntrials];     
  int<lower = 0> reward[Nsubjects,Ntrials];       
  int<lower = 0> offer1[Nsubjects,Ntrials];       
  int<lower = 0> offer2[Nsubjects,Ntrials];       
  int<lower = 0> selected_offer[Nsubjects,Ntrials];
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials];           
  int<lower = 0> fold[Nsubjects,Ntrials];           
  real           testfold;                          

}

transformed data{
  int<lower = 1> Nparameters=3; 
  vector[Narms] Qvalue_initial; 
  Qvalue_initial = rep_vector(0.5, Narms);
}

parameters {

  //population level parameters 
  vector[Nparameters] population_locations;         
  vector<lower=0>[Nparameters] population_scales;   
  
//individuals level
  vector[Nsubjects] alpha_ch_random_effect;
  vector[Nsubjects] beta_random_effect;
  vector[Nsubjects] alpha_unch_random_effect;

}


transformed parameters {
//declare variables and parameters
  vector<lower=0, upper=1>[Nsubjects]  alpha_ch;
  vector<lower=0, upper=1>[Nsubjects]  alpha_unch;
  vector                  [Nsubjects]  beta;

    
  for (subject in 1:Nsubjects) {
    alpha_ch[subject]   = inv_logit(population_locations[1]  + population_scales[1]  * alpha_ch_random_effect[subject]);
    alpha_unch[subject] = inv_logit(population_locations[2]  + population_scales[2]  * alpha_unch_random_effect[subject]);
    beta[subject]       =          (population_locations[3]  + population_scales[3]  * beta_random_effect[subject]);
  }

}



model {
  
  // population level  
  population_locations  ~ normal(0, 2);            
  population_scales     ~ cauchy(0,2);        

  // indvidual level  
  alpha_ch_random_effect    ~ std_normal();
  beta_random_effect        ~ std_normal();
  alpha_unch_random_effect  ~ std_normal();
 

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    vector[Narms] Qcard; 
    vector[Nraffle] Qoffer; 
    real PE;
    real choiceprob;
    real   Qdiff;
      for (trial in 1:Ntrials_per_subject[subject]){
          if(fold[subject,trial] != testfold) {
            
          if (first_trial_in_block[subject,trial] == 1) {
                        Qcard=Qvalue_initial;
        }
        
          Qoffer[1]=Qcard[offer1[subject,trial]];
          Qoffer[2]=Qcard[offer2[subject,trial]];

        Qdiff  = Qoffer[2]- Qoffer[1];
        //liklihood function 
         target+= bernoulli_logit_lpmf(selected_offer[subject,trial] | beta[subject] * Qdiff);
            
        //Qvalues update
	choiceprob = inv_logit(beta[subject]*Qdiff);
        PE = (reward[subject,trial] - Qcard[choice[subject,trial]]);
        Qcard[choice[subject,trial]] += alpha_ch[subject]  * PE * beta[subject] * (1-choiceprob);
        Qcard[unchosen[subject,trial]] += alpha_unch[subject]  * -PE * beta[subject] * choiceprob;

        //put the Q values in the range of 0 to 1
          if (Qcard[choice[subject,trial]]>1){
            Qcard[choice[subject,trial]]=1;
          }
          else if(Qcard[choice[subject,trial]]<0){
            Qcard[choice[subject,trial]]=0;
          }
          if (Qcard[unchosen[subject,trial]]>1){
            Qcard[unchosen[subject,trial]]=1;
          }
          else if(Qcard[unchosen[subject,trial]]<0){
            Qcard[unchosen[subject,trial]]=0;
          }
      }
      }
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

generated quantities {

  matrix[Nsubjects,Ntrials]     log_lik;
  real PE;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial (placed in generetaed quantities block to save time and memory)

  { //
      log_lik=rep_matrix(0,Nsubjects,Ntrials);

    for (subject in 1:Nsubjects) {
        vector[Narms] Qcard;
        vector[Nraffle] Qoffer;
        real   Qdiff;
	real   choiceprob;

        Qcard=Qvalue_initial;


        for (trial in 1:Ntrials_per_subject[subject]){
         
         if(fold[subject,trial] == testfold) {

        //offer values
          Qoffer[1]=Qcard[offer1[subject,trial]];
          Qoffer[2]=Qcard[offer2[subject,trial]];
   
         Qdiff  = Qoffer[2]- Qoffer[1];
       // compute log likelihood of current trial
        log_lik[subject,trial] =  bernoulli_logit_lpmf(selected_offer[subject,trial] | beta[subject] * Qdiff);

        // compute log likelihood of current trial
	choiceprob = inv_logit(beta[subject]*Qdiff);
        PE = (reward[subject,trial] - Qcard[choice[subject,trial]]);
        Qcard[choice[subject,trial]] += alpha_ch[subject]  * PE * beta[subject] * (1-choiceprob);
        Qcard[unchosen[subject,trial]] += alpha_unch[subject]  * -PE * beta[subject] * choiceprob;
        
        //put the Q values in the range of 0 to 1
          if (Qcard[choice[subject,trial]]>1){
            Qcard[choice[subject,trial]]=1;
          }
          else if(Qcard[choice[subject,trial]]<0){
            Qcard[choice[subject,trial]]=0;
          }
          if (Qcard[unchosen[subject,trial]]>1){
            Qcard[unchosen[subject,trial]]=1;
          }
          else if(Qcard[unchosen[subject,trial]]<0){
            Qcard[unchosen[subject,trial]]=0;
          }
        }
        }
    }
  }
}
