# #### step 1: compile data generating model ------

library(tidyverse)
library(brms)
library(bayestestR)
library(knitr)
library(kableExtra)
library(cmdstanr)
# # regression equation
 f_model <-  stay_color ~ Previous_outcome*Nfeatures
# 
# # some empty data (for brms)
 df      <- data.frame(stay_color = c(0,1), Previous_outcome = as.factor(c("Rewarded","Not_Rewarded")),Nfeatures =as.factor(c("1","2")))

#set constant priors
get_prior(f_model,
          family = bernoulli(link="logit"),
          data = df)

priors_for_gen <-
  set_prior("normal(-0.21,0.05)", class = "Intercept") +
  set_prior("normal(2.22,0.05)", coef = "Previous_outcomeRewarded")+
  set_prior("normal(0,0.05)",coef = "Previous_outcomeRewarded:Nfeatures2")+
  set_prior("normal(0,1)",coef = "Nfeatures2")


validate_prior(priors_for_gen,
               f_model,
               family = bernoulli(link="logit"),
               data = df)

#compile model
model_generative   <- brm(f_model,
                          family = bernoulli(link="logit"),
                          data = df,
                          prior = priors_for_gen,
                          sample_prior = "only",
                          cores = 4,
                          backend="cmdstanr")

# #### step 2: compile the sampling model -----

#set priors (here we use default - but this can be changed)
priors_for_sampling =
  set_prior("normal(0,1)", class = "Intercept") +
  set_prior("normal(0,1)", coef = "Previous_outcomeRewarded")+
  set_prior("normal(0,1)",coef = "Previous_outcomeRewarded:Nfeatures2")+
  set_prior("normal(0,1)",coef = "Nfeatures2")

#compile model (using the same df and f_model from step 1)
model_sample   <- brm(f_model,
                      family  = bernoulli(link="logit"),
                      data    = df,
                      prior   = priors_for_sampling,
                      backend = "cmdstanr",
                      cores   = 4
)

# #### step 3: run the simulation -----------------

#sample size
nsubjects     <- c(25, 50,100)

#number of studies
kstudies <- 20

for (n in nsubjects) {

  for (k in seq_len(kstudies)) {

    print(paste0('simulation ', k ,' for sample size of ', n))

    #generate data with predictors
    df      <- data.frame(Previous_outcome = as.factor(c("Rewarded","Not_Rewarded","Not_Rewarded","Rewarded")),Nfeatures =as.factor(c("1","2","1","2")))
    df      <- bind_rows(replicate(n/2, df, simplify = FALSE))


    #simulate y values using known parameters
    df$stay_color    <- posterior_predict(model_generative,
                                 newdata           = df,
                                 allow_new_levels  = TRUE,
                                 ndraw = 1
    )[1,]
    #estimate parameters from y values
    temp_model = update(model_sample, newdata = df, iter=1000,warmup=500,cores=4)

    #save
    if(k==1){results_models=list()}
    results_models[[k]]<-temp_model
    save(results_models,file = paste0('data/precision_analysis/raw_Ni=',n,'.rdata'))
  }
}

# ####step 4: housekeeping ----

#here we  translate the "results_models" object to a list with the parameters, median and hdi for each model
rm(list=ls())

#sample size
nsubjects     <- c(25, 50,100)

#number of studies
kstudies <- 20
precision_models=list()

for (n in nsubjects) {
  load(paste0('data/precision_analysis/raw_Ni=',n,'.rdata'))

  for (k in seq_len(kstudies)){

    model = results_models[[k]]
    x95 = describe_posterior(model,ci=c(0.95))
    x   = data.frame(
      Sample    = k,
      Parameters= x95$Parameter,
      Median    = x95$Median,
      CI95low   = x95$CI_low,
      CI95high  = x95$CI_high,
      CI95      = x95$CI_high - x95$CI_low,
      Include0  = ((x95$CI_high * x95$CI_low)<0)*1)
    precision_models[[k]]=x
  }
  save(precision_models,file=paste0('data/precision_analysis/results_Ni=',n,'.rdata'))
}

# ####step 5 - generate the results for each simulation------
df_intercept_total=data.frame()
df_reward_total=data.frame()
df_interaction_total=data.frame()
for (n in nsubjects){
load(paste0('data/precision_analysis/results_Ni=',n,'.rdata'))
df=do.call(rbind,precision_models)
df=df%>%mutate(Nsubjects=n)

df_intercept=df%>%filter(Parameters=="b_Intercept")
df_intercept=df_intercept%>%summarize(sample_size=mean(Nsubjects),mean_median=mean(Median),across(starts_with("CI"), mean, .names = "mean_{.col}"))
df_reward = df%>%filter(Parameters=="b_Previous_outcomeRewarded")
df_reward=df_reward%>%summarize(sample_size=mean(Nsubjects),mean_median=mean(Median),across(starts_with("CI"), mean, .names = "mean_{.col}"))
df_interaction=df%>%filter(Parameters=="b_Previous_outcomeRewarded:Nfeatures2")
df_interaction=df_interaction%>%summarize(sample_size=mean(Nsubjects),mean_median=mean(Median),across(starts_with("CI"), mean, .names = "mean_{.col}"))

df_intercept_total = rbind(df_intercept_total,df_intercept)
df_reward_total = rbind(df_reward_total,df_reward)
df_interaction_total= rbind(df_interaction_total,df_interaction)
}
save(df_intercept_total,file=paste0('data/precision_analysis/summary_intercept.rdata'))
save(df_reward_total,file=paste0('data/precision_analysis/summary_reward.rdata'))
save(df_interaction_total,file=paste0('data/precision_analysis/summary_interaction.rdata'))
# Create a nicely formatted table
table_intercept <- kable(df_intercept_total, align = "c", format = "html") %>%
kable_styling(bootstrap_options = c("striped", "hover"))

table_reward <- kable(df_reward_total, align = "c", format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

table_interaction <- kable(df_interaction_total, align = "c", format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
# Display the table
print(table_intercept)
print(table_reward)
print(table_interaction)
