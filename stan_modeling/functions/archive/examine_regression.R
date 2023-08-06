# Aim: Run some basic sanity checks using linear / logistic regression
rm(list=ls())
source('./functions/my_packages.R')
source('./functions/my_starter.R')

#--------------------------------------------------------------------------------------------------------


load(paste0(path$data,'/artificial_data.Rdata'))



df=df%>%mutate(stay                  =(ch_card==lag(ch_card)),
               reward_oneback         =lag(reward),
               stay_key              =ch_key==lag(ch_key),
               reoffer_ch            =lag(ch_card)==card_right|lag(ch_card)==card_left,
               unchosen              =if_else(ch_card==card_right,card_left,card_right),
               reoffer_unch            =lag(unchosen)==card_right|lag(unchosen)==card_left)
               

model= glmer(stay ~ reward_oneback+(reward_oneback| subject), 
             data = df,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

effects::effect()