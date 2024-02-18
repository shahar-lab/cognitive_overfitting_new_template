

examine_population_parameters_empirical <-function(path) {

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
fit=readRDS(paste0(path$data,'/modelfit_empirical.rds'))

#load artificial parameters
load(paste0(path$data,'/model_parameters.Rdata'))

Nparameters = length(model_parameters$artificial_population_location)
p=list()
for ( i in 1:Nparameters){
  samples    = fit$draws(variables = paste0('population_locations[',i,']'),
                         format    = 'matrix')
  p[[i]]=
    ggplot(data.frame(samples=as.numeric(unlist(samples))),aes(x=samples))+
    ggdist::stat_halfeye(point_interval = 'median_hdi',
                         .width = c(0.85,0.95),
                         fill = 'grey')+
    xlab(model_parameters$names[i])+mytheme
  if (model_parameters$transformation[i] == "logit") {
    p[[i]] = p[[i]] + scale_x_continuous(limits = c(0, 1))
  } else {
    p[[i]] = p[[i]] + scale_x_continuous(limits = c(-1, 10))
  }
  xlab(model_parameters$names[i])+
    mytheme
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank())
  
}

do.call("grid.arrange", c(p, ncol=2))
}
