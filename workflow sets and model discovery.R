#model discovery walk through

#devtools::install_local('C:\\Users\\loren\\Documents\\R\\mostlytidyMMM',force=T)
setwd('C:\\Users\\loren\\Documents\\R\\tidymmm')
librarian::shelf(tidymodels,tune,recipes,multilevelmod,tidyverse,arrow,workflowsets,rethinking,rstan)


#source('tidymodels methods.R')
library(mostlytidyMMM)
source('mmm functions reducing.R')

control_file<-'example_model_control_stages.xlsx'

#get control spreadsheet
#var_controls -- must have 1 and only 1 role=time_id record
#TODO: write function to perform checkcs on control file: 1) 1 outcome 2) role and role 2 assignment checks 3)

var_controls<-readxl::read_xlsx(control_file,'variables')
transform_controls<-readxl::read_xlsx(control_file,'role controls')

workflow_controls<-readxl::read_xlsx(control_file,"workflow") %>% select(-desc)

#ok, the recipe contains adstock and saturation transformation parameters or 
#hyperapameter tuning selections

#but the model formula controls which terms have random slopes and which don't, and
#how many fourier terms are used

# we will tune the adstock and saturation while determining number of fft_terms to use

#read data and get names right;
data1<-data.table::fread("example2.csv") %>% 
  rename_columns_per_controls()%>% mutate(week=as.Date(week,"%m/%d/%Y"))

data1<-add_fourier_vars(data_to_use=data1,vc=var_controls) %>% 
  add_groups_and_sort(vc=var_controls) 

recipe3<-create_recipe(data_to_use = data1)

#need to create formulas with 0 to fft_terms pairs of sins and cos
#and with/without interaction
make_list_of_fft_formulae<-function(vc=workflow_controls,recipe_to_use=recipe3){
  fft_interact_options<-get_control("interaction_fft") %>% strsplit(split=',',fixed=T) %>% unlist()
  if(!("" %in% fft_interact_options)){fft_interact_options=c("",fft_interact_options)}
  
  fft_count_options<-get_control("fft_terms") %>% unlist() %>% as.numeric()
  #vc<-workflow_controls
  list_of_configs<-vector('list')
  idx=0
  for (this_fterms in 0:fft_count_options){
    for (iterm in 1:length(fft_interact_options)){
      idx=idx+1
      this_iterm=fft_interact_options[iterm]
      
      this_vc=vc %>% mutate(Value=ifelse(R_name=='interaction_fft',this_iterm,Value),
                            Value=ifelse(R_name=='fft_terms',this_fterms,Value))
      list_of_configs[[idx]]=this_vc
    }
  }
  formulae<-lapply(list_of_configs,function(x) create_formula(recipe_to_use,x,ignore_rands = T))
return(list(formulae=formulae,configs=list_of_configs))
}

fft_formulae0<-make_list_of_fft_formulae(workflow_controls,recipe3)
formulae<-fft_formulae0[[1]]
configs_fft_options<-fft_formulae0[[2]]

#make a model for each formula
assemble_workflow<-function(this_formula=built_formula,recipe_to_use=recipe3){
  if(grepl("\\|",this_formula)){tune_spec<-linear_reg(engine='lmer')} else{
    tune_spec<-linear_reg(engine='lm')}
  #  hardhat::extract_parameter_set_dials(reg_wf)%>% finalize(data1) 
  #need to build a formula with random effects specs for stan_glmer
  
  
  mmm_wf<-workflow() %>%  add_recipe(recipe_to_use) %>% 
    add_model(tune_spec,formula=as.formula(this_formula))   
  return(mmm_wf)
}
names(formulae)<-as.character(1:length(formulae))

list_of_flows<-lapply(formulae,assemble_workflow)

tune_all_these<-as_workflow_set(!!!list_of_flows)

data1<-data1 |>mutate(combo_id= paste0(store,'_',product))

fft_selecting_tune<-workflow_map(tune_all_these,grid=25,resamples=vfold_cv(data1,v=2,strata=combo_id))

id_of_best<-rank_results(fft_selecting_tune,rank_metric="rmse",select_best=F) %>%
  select(wflow_id) %>% slice_head(n=1) %>% unlist()

hyper_parms<-select_best(
  extract_workflow_set_result(fft_selecting_tune,id=id_of_best),metric='rmse')

best_vc<-configs_fft_options[[as.numeric(id_of_best)]]
best_formula<-formulae[[as.numeric(id_of_best)]]
# autoplot(fft_selecting_tune,metric='rmse')

#let's keep these hyperparms, but try the random effects
hyper_parms_finalized=recipe3 %>% finalize_recipe(hyper_parms)

list_of_formulae_rands<-vector('list')

#get all possible terms
terms_to_add<-paste(get_control("list_rand_ints",best_vc),
      get_control('list_rand_slopes',best_vc),sep=',') %>% strsplit(split=',') %>% unlist()
pastey<-function(x){paste(unlist(x),collapse=" + ")}
all_combinations<-unlist(lapply(1:length(terms_to_add),function(x) combn(terms_to_add,x,FUN=pastey,simplify = F)))

for(i in 1:length(all_combinations)){
  list_of_formulae_rands[[i]]<-paste0(best_formula,'+',all_combinations[[i]])
}
list_of_formulae_rands<-append(list_of_formulae_rands,best_formula)

list_of_flows2<-lapply(list_of_formulae_rands,assemble_workflow,hyper_parms_finalized)
names(list_of_flows2)<-as.character(1:length(list_of_formulae_rands))
tune_all_these2<-as_workflow_set(!!!list_of_flows2)

rands_selecting_tune<-workflow_map(tune_all_these2,grid=1,resamples=vfold_cv(data1,v=2,strata=combo_id))
rank_results(rands_selecting_tune)
id_of_best_rand<-rank_results(rands_selecting_tune,rank_metric="rmse",select_best=T) %>%
  select(wflow_id) %>% slice_head(n=1) %>% unlist()

best_formula<-list_of_formulae_rands[[as.numeric(id_of_best_rand)]][1]
