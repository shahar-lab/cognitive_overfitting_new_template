

examine_individual_parameters_recovery <-function(path) {

library(ggplot2)
library(bayestestR)
library(stringr)
library(ggpubr)
  
  mytheme=
    theme_pubclean()+
    theme(panel.border   = element_blank(), 
          axis.line      = element_line(color='gray'),
          text           = element_text(size=14,  family="serif"),
          axis.title     = element_text(size=14),
          legend.position= "right",
          plot.title     = element_text(hjust = 0.5))

#load recovered parameters
fit=readRDS(paste0(path$data,'/modelfit_recovery.rds'))

#load artificial parameters
source(paste0(path$model,'_parameters.r'))
load(paste0(path$data,'/model_parameters.Rdata'))


Nparameters = length(model_parameters$artificial_population_location)
p=list()
for ( i in 1:Nparameters){
  if (model_parameters$transformation[i]=='logit'){
    true_values=inv_logit_scaled(model_parameters$artificial_individual_parameters[,model_parameters$names[i]]) 
  }
  else{
    true_values = model_parameters$artificial_individual_parameters[,model_parameters$names[i]]
  }
  recovered = apply(fit$draws(variables =model_parameters$names[i],format='draws_matrix'), 2, mean)
  p[[i]]=
  my_xyplot(true_values,
            recovered,
            'true',
            'recovered',
            'navy')+
    xlab(model_parameters$names[i])+
    mytheme
}
do.call("grid.arrange", c(p, ncol=1))
}