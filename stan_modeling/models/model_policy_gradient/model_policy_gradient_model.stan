
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

}


transformed data{
  int<lower = 1> Nparameters=3; 
  vector[Narms] Qvalue_initial; 
  Qvalue_initial = rep_vector(0.5, Narms);
}




parameters {
  //population level parameters 
  vector         [Nparameters] population_locations;      
  vector<lower=0>[Nparameters] population_scales;         
  
  //individuals level
  vector[Nsubjects] alpha_ch_random_effect;
  vector[Nsubjects] alpha_unch_random_effect;
  vector[Nsubjects] beta_random_effect;
}


transformed parameters {
  
  vector<lower=0, upper=1>[Nsubjects]  alpha_ch;
  vector<lower=0, upper=1>[Nsubjects]  alpha_unch;
  vector                  [Nsubjects]  beta;
  matrix                  [Ntrials,Nsubjects] Qdiff_external;
  //RL
  for (subject in 1:Nsubjects) {
    //internal variabels
    real   Qdiff;
    real   choiceprob;
    real   PE;
    real   Qval[Narms];
	  
    //set indvidual parameters
    alpha_ch[subject]   = inv_logit(population_locations[1]  + population_scales[1] * alpha_ch_random_effect[subject]);
    alpha_unch[subject]   = inv_logit(population_locations[2]  + population_scales[2] * alpha_unch_random_effect[subject]);
    beta[subject]    =          (population_locations[3]  + population_scales[3] * beta_random_effect [subject]);
    
        //likelihood estimation
        for (trial in 1:Ntrials_per_subject[subject]){
        
        //reset Qvalues (first trial only)
    		if (first_trial_in_block[subject,trial] == 1) {
        Qval = rep_array(0.5, Narms);
    		}
        
        //calculate probability for each action
        Qdiff        = Qval[offer2[subject,trial]]- Qval[offer1[subject,trial]];

        
        //update Qvalues
	choiceprob = inv_logit(beta[subject]*Qdiff);
        PE  = reward[subject,trial]  - Qval[choice[subject,trial]];
        Qval[choice[subject,trial]] += alpha_ch[subject]*PE * beta[subject] * (1-choiceprob);
        Qval[unchosen[subject,trial]] += alpha_unch[subject]  * -PE * beta[subject] * choiceprob;
        Qdiff_external[trial,subject] = Qdiff;
        
      }
 
  }

}


model {
  
  // population level  
  population_locations  ~ normal(0,2);            
  population_scales     ~ cauchy(0,2);        

  // indvidual level  
  alpha_ch_random_effect ~ std_normal();
  alpha_unch_random_effect ~ std_normal();
  beta_random_effect  ~ std_normal();
 

  for (subject in 1:Nsubjects){
    for (trial in 1:Ntrials_per_subject[subject]){
      target+= bernoulli_logit_lpmf(selected_offer[subject,trial] | beta[subject] * Qdiff_external[trial,subject]);
    }
  }
}
