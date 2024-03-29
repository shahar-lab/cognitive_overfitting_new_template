

examine_population_parameters_recovery <-function(path) {

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
  samples    = fit$draws(variables = paste0('population_locations[',i,']'),
                      format    = 'matrix')
  sample_value = mean(model_parameters$artificial_individual_parameters[,i])
  true_value=model_parameters$artificial_population_location[i]
  limit=c(0,10)
  if (model_parameters$transformation[i]=='logit'){
    samples = plogis(samples)
    sample_value = plogis(mean(model_parameters$artificial_individual_parameters[,i]))
    true_value=plogis(model_parameters$artificial_population_location[i])
    limit=c(0,1)
  }
  if (model_parameters$transformation[i]=='exp'){
    samples = log(samples)
  }
  
  samples    = data.frame(samples=unlist(samples))

  p[[i]]=
  ggplot(data.frame(samples=as.numeric(unlist(samples))),aes(x=samples))+
    ggdist::stat_halfeye(point_interval = 'median_hdi',
                         .width = c(0.85,0.95),
                         fill = 'grey')+
    geom_vline(xintercept = true_value, 
               linetype="dotted",
               color = "blue", 
               linewidth=1.5)+
    geom_vline(xintercept = sample_value, 
               linetype="dotted",
               color = "lightblue", 
               linewidth=1.5)+
    xlab(model_parameters$names[i])+mytheme
    if (model_parameters$transformation[i] == "logit") {
      p[[i]] = p[[i]] + scale_x_continuous(limits = c(0, 1))
    } else {
      p[[i]] = p[[i]] + scale_x_continuous(limits = c(-1, 10))
    }
    xlab(model_parameters$names[i])+scale_x_continuous(limits=limit)+
    mytheme
    theme(axis.ticks.y=element_blank(),
          axis.text.y=element_blank())
  
}
do.call("grid.arrange", c(p, ncol=2))
}
