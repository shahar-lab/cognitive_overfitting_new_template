library(tidyverse)
library(lme4)
load(paste0(path$data,'/artificial_data.Rdata'))

df=df%>%mutate(block=as.factor(block),reward_oneback=as.factor(lag(reward)),
               reoffer_ch=lag(ch_card)==card_right|lag(ch_card)==card_left,
               unch_card=if_else(ch_card==card_right,card_left,card_right),
               reoffer_unch=lag(unch_card)==card_right|lag(unch_card)==card_left,
               stay_key=ch_key==lag(ch_key),condition=factor(if_else(as.numeric(block)%%2==1,1,0)))%>%na.omit(df)  
m=lm(data=df,formula=weight_key~eta)
m_key=lm('weight_key ~ eta', data=df)
m_key_weight=glmer('stay_key ~ reward_oneback+ (reward_oneback|subject)', data=df%>%filter(reoffer_ch==F,reoffer_unch==F), family = binomial)
m_key_b <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback*block+(1+block+reward_oneback|subject),
    data = df,
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 20,
    cores = 20,
    seed = 123,
    backend = "cmdstanr",
    #prior=mypriors
  )
save(m_key_b,file=paste0(path$data,'/model_regression.Rdata'))


model=brm(formula=stay_key~1+reward_oneback*condition_difficulty*condition_scarcity+(1+reward_oneback|subject),  
               data=df,
               family=bernoulli(link = "logit"),
               warmup = 1000, 
               iter = 2000, 
               chains = 4, 
               cores=4,
               seed = 123,
               backend="cmdstanr")

save(model,file=paste0(path$data,"/model_regression.rdata"))

m_key <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 2,
    cores = 2,
    seed = 123,
    backend = "cmdstanr",
    #prior=mypriors
  )

save(m_key,file=("data/empirical_data/regression_key.rdata"))
