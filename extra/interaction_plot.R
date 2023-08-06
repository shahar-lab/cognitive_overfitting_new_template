library(ggdist)

df_pred <- expand.grid(reward_oneback = unique(df$reward_oneback),
                       scarcity = unique(df$scarcity))
df_pred$reward_oneback <- factor(df_pred$reward_oneback , labels = c("0", "1"))
df_pred$scarcity <- factor(df_pred$scarcity , labels = c("0", "1"))
df_pred$difficulty <- factor(df_pred$difficulty , labels = c("0", "1"))

df_pred$pred <- posterior_epred(model, newdata = df_pred, re_formula = NA) %>% 
  posterior::rvar()

ggplot(df_pred, aes(y = reward_oneback, fill = scarcity)) + 
  facet_grid(~difficulty) +
  stat_slabinterval(aes(xdist = pred))+theme_classic()
