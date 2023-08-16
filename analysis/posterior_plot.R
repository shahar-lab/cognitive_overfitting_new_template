my_posterior_plot=function (model, variable_name, xlabel, mycolor = "gray",xlim_start,xlim_end) 
{
  library(ggplot2)
  library(insight)
  library(bayestestR)
  params = insight::get_parameters(model)
  variable = params[, paste0("b_", variable_name)]
  ggplot(data.frame(x = variable), aes(x = variable)) + geom_density(alpha = 0.5, 
                                                                     fill = mycolor) + geom_vline(xintercept = 0, 
                                                                                                  linetype = "dotted", color = "blue", size = 1.5) + 
    geom_segment(aes(x = hdi(variable, ci = 0.95)$CI_low, 
                     y = 0, xend = hdi(variable, ci = 0.95)$CI_high, yend = 0), 
                 color = "darkgray", size = 2, show.legend = F) + 
    xlab(xlabel) + ylab("Density") + theme_classic()+
    xlim(xlim_start,xlim_end)
}

