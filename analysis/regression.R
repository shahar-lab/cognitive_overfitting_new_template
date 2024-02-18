library(tidyverse)
library(lme4)
library(brms)
load(paste0(path$data,'/artificial_data.Rdata'))

df=df%>%mutate(block=as.factor(block),reward_oneback=as.factor(lag(reward)),
               reoffer_ch=lag(ch_card)==card_right|lag(ch_card)==card_left,
               unch_card=if_else(ch_card==card_right,card_left,card_right),
               reoffer_unch=lag(unch_card)==card_right|lag(unch_card)==card_left,
               stay_key=ch_key==lag(ch_key),condition=factor(as.numeric(block)%%2),entropy=factor(if_else(condition==0,"High","Low")))%>%na.omit(df)  
m=lm(data=df,formula=weight_key~lambda1)
m_key=lm('weight_key ~ lambda0', data=df)
m_key_weight=glmer('stay_key ~ reward_oneback*lambda+ (reward_oneback|subject)', data=df, family = binomial)
m_key_weight=glmer('stay_key ~ reward_oneback*alpha_irrelevant+ (reward_oneback|subject)', data=df%>%filter(catch_trial==1), family = binomial)

df$accuracy=df$exp_val_ch>df$exp_val_unch
a=df%>%filter(block==2)%>%group_by(subject)%>%summarise(acc=mean(accuracy),alpha_irr=mean(alpha_irrelevant),lambda1=mean(lambda1),lambda2=mean(lambda2))
cor.test(a$acc,a$alpha_irr)
a%>%ggplot(aes(x=alpha_irr,y=acc))+geom_point()+geom_smooth(method="lm")
mypriors = c(
  set_prior(
    prior = "normal(0,1)",
    class = "b",
    coef = "Intercept"
  ),
  set_prior(
    prior = "normal(0,1)",
    class = "b",
    coef = "reward_oneback1"
)
)
df$reward_oneback=factor(df$reward_oneback)
df$reward=factor(df$reward)
df$accuracy=factor(df$accuracy)
m_key_lambda_empirical <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback*lambda+(1+reward_oneback|subject),
    data = df,
    family = bernoulli(link="logit"),
    warmup = 1000,
    iter = 2000,
    chains = 2,
    cores = 2,
    seed = 123,
    backend = "cmdstanr"
  )
save(m_key_lambda_empirical,file=paste0(path$data,'/model_regression.Rdata'))


model_reward_sim_lambda=brm(formula=reward~1+transformed_lambda,  
               data=df,
               family=bernoulli(link = "logit"),
               warmup = 1000, 
               iter = 2000, 
               chains = 4, 
               cores=4,
               seed = 123,
               backend="cmdstanr")

save(model_reward_sim_lambda,file=paste0(path$data,"/model_reward_sim_lambda.rdata"))

model_acc_sim_lambda=brm(formula=accuracy~1+transformed_lambda,  
                            data=df,
                            family=bernoulli(link = "logit"),
                            warmup = 1000, 
                            iter = 2000, 
                            chains = 4, 
                            cores=4,
                            seed = 123,
                            backend="cmdstanr")

save(model_acc_sim_lambda,file=paste0(path$data,"/model_acc_sim_lambda.rdata"))


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

library(raincloudplots)


df<-na.omit(df)

df=df%>%mutate(block=as.factor(block),reward_oneback=as.factor(lag(reward)),
               reoffer_ch=lag(ch_key)==key1|lag(ch_key)==key2,
               unch_key=if_else(ch_key==key1,key2,key1),
               reoffer_unch=lag(unch_key)==key1|lag(unch_key)==key2,
               stay_color=ch_color==lag(ch_color),
               stay_shape=ch_shape==lag(ch_shape),
               stay_texture=ch_texture==lag(ch_texture),
               acc=exp_val_ch>exp_val_unch)

df= df%>%filter(reoffer_ch==F,reoffer_unch==F)


mean_accuracy=mean(df%>%group_by(subject)%>%summarise(acc=mean(acc))%>%pull(acc))
group_color=df%>%group_by(reward_oneback)%>%summarise(mean(stay_color))
group_shape=df%>%group_by(reward_oneback)%>%summarise(mean(stay_shape))
group_texture=df%>%group_by(reward_oneback)%>%summarise(mean(stay_texture))
#-----------------------------------------------------------------------------------

model= glmer(stay_color ~ reward_oneback+(reward_oneback| subject), 
             data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

library(effects)
  plot(effect('reward_oneback',model))




#-----------------------------------------------------------------------------------
library(brms)

mypriors=c(set_prior(prior = "normal(0,0.2)", class = "b",coef = "Intercept"),
           set_prior(prior = "normal(0,0.2)", class = "b",coef = "reward_oneback1")
)

model1= brm(stay_color ~ 0 + Intercept+reward_oneback+(1+reward_oneback|subject), 
           data = df%>%filter(reoffer_ch==F,reoffer_unch==F), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =20,
           chains=20,
           prior=mypriors,
           backend = "cmdstanr",
          )
             
save(model1, file=paste0(path$data,'/regression_model_interaction.rdata'))
plot(conditional_effects(model1),plot=TRUE)[[1]]+theme_bw()
summary(model1)
my_posterior_plot(model1,"reward_oneback1","reward_oneback", "gray",0,2)

