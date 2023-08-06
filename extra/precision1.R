
library(dplyr)

# Function to generate dataset
generate_data = function(n_subjects, n_observations) {
  data = data.frame(
    subject = rep(1:n_subjects, each = n_observations),
    ASD = rep(sample(c(0, 1), n_subjects, replace = TRUE), each = n_observations), #fix to 50-50
    reward_oneback = sample(c(0, 1), n_subjects * n_observations, replace = TRUE)
  )
  data= data%>%mutate(reward_oneback_ASD=reward_oneback*ASD)
  
  # Define coefficients
  coef_intercept = rnorm(n_subjects, -0.23, 0.38) #I took the population and fixed effects from win-loss data
  coef_reward_oneback = rnorm(n_subjects, 0.24, 0.1) 
  coef_ASD = rnorm(1, 0, 0.1)
  coef_interaction=rnorm(1, 0.2, 0.1)
  data=data%>%mutate(coef_intercept=rep(coef_intercept,each=n_observations),coef_reward_oneback=rep(coef_reward_oneback,each=n_observations),
                     coef_ASD=rep(coef_ASD,n_subjects*n_observations),coef_interaction=rep(coef_interaction,n_subjects*n_observations))
  # Generate stay_key variable
  data = data %>% mutate(
    logit_prob = coef_intercept + coef_reward_oneback * reward_oneback + coef_ASD * ASD+coef_interaction*reward_oneback_ASD,
    prob = 1 / (1 + exp(-logit_prob)),
    stay_key = rbinom(n(), 1, prob)
  )
  
  return(data)
}

library(brms)

# Function to fit model and return precision
fit_and_extract_precision = function(data) {
  # Fit model
  fit = brm(stay_key ~ ASD * reward_oneback + (1+reward_oneback| subject), data = data, family = bernoulli(), 
             iter = 1000, chains = 2, backend = "cmdstanr", refresh = 0)
  
  # Extract summaries
  summary = summary(fit)$fixed
  
  #CI limits
  upper_bound=summary$`u-95% CI`
  lower_bound=summary$`l-95% CI`
  # Calculate CI width
  ci_width = abs(upper_bound-lower_bound)
  #Posterior median
  posterior_median  = summary$Estimate
  
  return(list(ci_width = ci_width, posterior_median = posterior_median))
}

# Specify numbers of subjects and observations
n_subjects_list = c(50, 100, 150)
n_observations_list = c(150, 300, 450)

# Create an empty data frame to store results
results = data.frame()

# Run experiment
for (n_subjects in n_subjects_list) {
  for (n_observations in n_observations_list) {
    data = generate_data(n_subjects, n_observations)
    precision = fit_and_extract_precision(data)
    
    # Create a data frame with results
    results_temp = data.frame(
      n_subjects = n_subjects,
      n_observations = n_observations,

      reward_ci_width = precision$ci_width[2],
      reward_posterior_median = precision$posterior_median[2],

      interaction_ci_width = precision$ci_width[4],
      interaction_posterior_median = precision$posterior_median[4]
    )
    
    # Append results to the main results data frame
    results = bind_rows(results, results_temp)
  }
}

print(results)

#add range and median of multiple experiments