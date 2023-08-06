
library(data.table)
library(reshape2)
library(rstatix)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(raincloudplots)

rm(list=ls())
load('./data/empirical_data/inbal_empirical_data.rdata')
df<-na.omit(df)

df=df%>%mutate(block=as.factor(block),reward_oneback=as.factor(lag(reward)),
               reoffer_ch=lag(ch_card)==card_right|lag(ch_card)==card_left,
               unch_card=if_else(ch_card==card_right,card_left,card_right),
               reoffer_unch=lag(unch_card)==card_right|lag(unch_card)==card_left,
               stay_key=ch_key==lag(ch_key))
df=df%>%mutate(block=as.numeric(block),difficulty=as.factor(if_else((block==1|block==2),1,0)),scarcity=as.factor(if_else((block==1|block==3),1,0)))         
              
accuracy=df%>%group_by(subject)%>%summarise(accuracy=mean(accuracy))%>%pull(accuracy)
omega=unique(df$omega)
cor.test(rew,omega)

df%>%group_by(subject)%>%summarise(mean_acc=mean(acc))

model= glm(stay_key ~ reward_oneback*block, 
             data = df%>%filter(catch_trial==T), 
             family = binomial)

model= glm(stay_key ~ reward_oneback*scarcity*difficulty, 
           data = df%>%filter(catch_trial==T), 
           family = binomial)

summary(model)

#-----------------------------------------------------------------------------------
library(brms)
model1= brm(stay_key ~ 0 + Intercept+reward_oneback+(1+reward_oneback|subject), 
           data = df%>%filter(reoffer_ch==F,reoffer_unch==F), 
           family = bernoulli(link = "logit"),
           warmup = 1000,
           iter = 2000,    
           cores =20,
           chains=20,
           )
save(model, file=paste0(path$data,'/regression_model_interaction.rdata'))
plot(conditional_effects(model1),plot=FALSE)[[1]]+theme_classic()

interaction <- emmeans(model, ~ reward_oneback * scarcity * difficulty,type="response")
emmip(interaction,  scarcity~ reward_oneback  | difficulty , CIs = TRUE)