df_long=df%>%group_by(trial)%>%summarise(card=mean(phi_card),key=mean(phi_key))%>%gather(key = "variable", value = "value", -trial)
df_long%>%ggplot(aes(x = trial, y = value, color = variable, group = variable)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Card and Key Values by Trial",
       x = "Trial",
       y = "Value",
       color = "Variable")

df=df%>%mutate(block=as.factor(block),reward_oneback=as.factor(lag(reward)),
                 reoffer_ch=lag(ch_card)==card_right|lag(ch_card)==card_left,
                  unch_card=if_else(ch_card==card_right,card_left,card_right),
                   reoffer_unch=lag(unch_card)==card_right|lag(unch_card)==card_left,
  stay_key=ch_key==lag(ch_key))%>%na.omit(df)  

df%>%group_by(reward_oneback)%>%summarise(mean(stay_key))

summary(glm(data=df,stay_key~reward_oneback,family="binomial"))
