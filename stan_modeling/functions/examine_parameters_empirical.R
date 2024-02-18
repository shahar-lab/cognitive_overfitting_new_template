#This code plot recovered parameters against the true parameters

rm(list=ls())
source('./functions/my_packages.R')
source('./functions/my_starter.R')
mydatatype=set_datatype()


#--------------------------------------------------------------------------------------------------------
#load recovered parameters
fit=readRDS(paste0(path$data,'/modelfit_empirical.rds'))

#load artificial parameters
load(paste0(path$data,'/model_parameters.Rdata'))


#--------------------------------------------------------------------------------------------------------

#population level parameters
my_posteriorplot(x       = plogis(fit$draws(variables ='population_locations[1]',
                                               format='draws_matrix')),
                     myxlim  = c(0,1),
                     my_vline= 0, 
                     myxlab  = expression(alpha['location']),
                     mycolor = "pink")

my_posteriorplot(x       = (fit$draws(variables ='population_locations[2]',
                                            format='draws_matrix')),
                 myxlim  = c(0,10),
                 my_vline= 0, 
                 myxlab  = expression(beta['location']),
                 mycolor = "pink")

my_posteriorplot(x       = plogis(fit$draws(variables ='population_locations[3]',
                                            format='draws_matrix')),
                 myxlim  = c(0,1),
                 my_vline= 0, 
                 myxlab  = expression(rho['location']),
                 mycolor = "pink")

my_posteriorplot(x       = plogis(fit$draws(variables ='population_locations[4]',
                                            format='draws_matrix')),
                 myxlim  = c(0,1),
                 my_vline= 0, 
                 myxlab  = expression(alpha['location']),
                 mycolor = "pink")

my_posteriorplot(x       = fit$draws(variables ='population_locations[5]',
                                            format='draws_matrix'),
                 myxlim  = c(0,10),
                 my_vline= 0, 
                 myxlab  = expression(alpha['location']),
                 mycolor = "pink")
#--------------------------------------------------------------------------------------------------------
median(plogis(fit$draws(variables ='population_locations[1]',
                     format='draws_matrix')))
hdi(plogis(fit$draws(variables ='population_locations[1]',
                     format='draws_matrix')))
median(plogis(fit$draws(variables ='population_locations[2]',
                        format='draws_matrix')))
hdi(plogis(fit$draws(variables ='population_locations[2]',
                     format='draws_matrix')))
median((fit$draws(variables ='population_locations[3]',
                        format='draws_matrix')))
hdi((fit$draws(variables ='population_locations[3]',
                     format='draws_matrix')))

# individual level parameters

hist(apply(fit$draws(variables ='alpha',format='draws_matrix'), 2, mean))
mean(apply(fit$draws(variables ='alpha',format='draws_matrix'), 2, mean))
hist(apply(fit$draws(variables ='beta',format='draws_matrix'), 2, mean))
mean(apply(fit$draws(variables ='beta',format='draws_matrix'), 2, mean))
hist(apply(fit$draws(variables ='rho',format='draws_matrix'), 2, mean))
mean(apply(fit$draws(variables ='rho',format='draws_matrix'), 2, mean))

rho=apply(fit$draws(variables ='rho' ,format='draws_matrix'), 2, mean)
weight_key=na.omit(apply(fit$draws(variables ='weight_key' ,format='draws_matrix'), 2, mean))
Qcard_left_ext=na.omit(apply(fit$draws(variables ='Qcard_left_ext' ,format='draws_matrix'), 2, mean))
Qcard_right_ext=na.omit(apply(fit$draws(variables ='Qcard_right_ext' ,format='draws_matrix'), 2, mean))
Qkey_left_ext=na.omit(apply(fit$draws(variables ='Qkey_left_ext' ,format='draws_matrix'), 2, mean))
Qkey_right_ext=na.omit(apply(fit$draws(variables ='Qkey_right_ext' ,format='draws_matrix'), 2, mean))

df=df%>%mutate(weight_key=weight_key,Qcard_left_ext=Qcard_left_ext,Qcard_right_ext=Qcard_right_ext,Qkey_left_ext=Qkey_left_ext,Qkey_right_ext=Qkey_right_ext)
