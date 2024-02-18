
modelfit_compile_loo <-function(path,format){
  
  set_cmdstan_path(path = NULL)
  if(format){ #changing to stan in newer version
    my_compiledmodel <- cmdstan_model(paste0(path$model,'_loo.stan'),compile=F)
    formatted_model=my_compiledmodel$format(canonicalize = list("deprecations"),overwrite_file = TRUE)
    my_compiledmodel$compile()
  }
  else{
    my_compiledmodel <- cmdstan_model(paste0(path$model,'_loo.stan'))
  }
 
  save(my_compiledmodel, file=paste0(path$data,'/modelfit_loo_compile.rdata'))
  cat(paste0('[stan_modeling]:  "modelfit_loo_compile.Rdata" was saved at "',path$data,'"'))
  
}
