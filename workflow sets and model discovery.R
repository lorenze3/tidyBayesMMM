
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

#
recipe3<-create_recipe(data_to_use = data1,vc=var_controls,mc=transform_controls,wc=workflow_controls)


#get list of seasonality specifications to try
fft_formulae0<-make_list_of_fft_formulae(workflow_controls,recipe3)
#split the output into two lists, one for formula and one for workflow config tables:
#list must have names for workflowsets
formulae<-fft_formulae0[[1]]
names(formulae)<-as.character(1:length(formulae))

configs_fft_options<-fft_formulae0[[2]]

#create workflows from each formula, with recipe3 + formula as pre-processor
list_of_flows<-lapply(formulae,assemble_workflow,recipe3)

tune_all_these<-as_workflow_set(!!!list_of_flows)

data1<-data1 |>mutate(combo_id= paste0(store,'_',product))

fft_selecting_tune<-workflow_map(tune_all_these,grid=25,resamples=vfold_cv(data1,v=2,strata=combo_id))

id_of_best<-rank_results(fft_selecting_tune,rank_metric="rmse",select_best=F) %>%
  select(wflow_id) %>% slice_head(n=1) %>% unlist()

if(check_if_needs_tune(recipe3)){
  hyper_parms<-select_best(
    extract_workflow_set_result(fft_selecting_tune,id=id_of_best),metric='rmse')
  hyper_parms_finalized<-recipe3 %>% finalize_recipe(hyper_parms)
}else{
  hyper_parms_finalized<-recipe3
}  

best_seas_vc<-configs_fft_options[[as.numeric(id_of_best)]]
best_seas_formula<-formulae[[as.numeric(id_of_best)]]
# autoplot(fft_selecting_tune,metric='rmse')

#let's keep these hyperparms, but try the random effects


list_of_formulae_rands<-make_list_of_rands_formula()

list_of_flows2<-lapply(list_of_formulae_rands,assemble_workflow,hyper_parms_finalized)
names(list_of_flows2)<-as.character(1:length(list_of_formulae_rands))
tune_all_these2<-as_workflow_set(!!!list_of_flows2)

rands_selecting_tune<-workflow_map(tune_all_these2,grid=1,resamples=vfold_cv(data1,v=2,strata=combo_id))
rank_results(rands_selecting_tune)
id_of_best_rand<-rank_results(rands_selecting_tune,rank_metric="rmse",select_best=T) %>%
  select(wflow_id) %>% slice_head(n=1) %>% unlist()

best_formula<-list_of_formulae_rands[[as.numeric(id_of_best_rand)]][1]

