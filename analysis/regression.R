
library(data.table)
library(reshape2)
library(rstatix)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
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
