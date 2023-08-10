rm(list=ls())
source('./functions/my_starter.R')
path=set_workingmodel()



#####simulate data--------------------

cfg = list(Nsubjects        = 50,
           Nblocks          = 4,
           Ntrials_perblock = 50,
           Narms            = 4,  #number of arms in the task 
           Noptions         = 2,  #number of arms offered for selection each trial
           rndwlk           = read.csv("randomwalk_ 4 arms_ 50 trials1.csv",header=F)
           )

simulate_parameters(path,cfg,plotme=T)

simulate_artifical_data(path,cfg)
load(paste0(path$data,'/artificial_data.Rdata'))

source(paste0(path$model,'_standata_parameters.r'))
simulate_convert_to_standata(path,cfg,model_stan_parameters$parameters)

#####sample posterior--------------------

modelfit_compile(path)

modelfit_mcmc(path,
               
              mymcmc = list(
                datatype = set_datatype() ,
                samples  = 100,
                warmup  = 1000,
                chains  = 10,
                cores   = 10,
                parallel_chains = 10)
)

#examine_mcmc(path) #needs debugging

examine_population_parameters_recovery(path)

examine_individual_parameters_recovery(path)


####examine model
#load parameters
fit   = readRDS(paste0(path$data,'/modelfit_recovery.rds'))
Qdiff = fit$draws(variables ='Qdiff_external',format='draws_matrix')
Qval1 = fit$draws(variables ='Qval1_external',format='draws_matrix')
Qval2 = fit$draws(variables ='Qval2_external',format='draws_matrix')
Qval3 = fit$draws(variables ='Qval3_external',format='draws_matrix')
Qval4 = fit$draws(variables ='Qval4_external',format='draws_matrix')

PE    = fit$draws(variables ='PE_external',format='draws_matrix')


#####compare models--------------------

#which model?
modelfit_compile_loo(path)
#which data?
modelfit_mcmc_loo(path,
              
              mymcmc = list(
                datatype = set_datatype() ,
                samples  = 500,
                warmup  = 500,
                chains  = 8,
                cores   = 8)
)
compare=compare_models(path)

####Documentation:------------------
#
#simulate_parameters:
#This function generate artificial model parameters in an hierarchical structure
#based on the definition in "_artificial_parameters.r" file that you need
#to place in the specific model folder.
#It then generate a 'model_parameters.Rdata' in the model data folder
#
#simulate_artifical_data:
#This function generates artificial data based on 'model_parameters.Rdata'
#and creates a df.rdata with artifical behavior in the model data folder
#
#simulate_convert_to_standata:
#converts the dataframe to matrix format and handles missing data with padding
#
#modelfit_compile:
#compile and save stan model (so you wont have to redo this everytime you re-run the model with different parameters)
#
#
#examine_parameters_recovery:
#This function plot recovered parameters against the true parameters
#use debug() and undebug() to find problems in these functions

