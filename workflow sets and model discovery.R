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

#TODO::need to edit cfreate_recipe to obey the interaction_fft term and to have a flag to ignore
#random int and slope terms!

fft_interact_options<-get_control("interaction_fft") %>% strsplit(split=',',fixed=T) %>% unlist()
if(!("" %in% fft_interact_options)){fft_interact_options=c("",fft_interact_options)}

fft_count_options<-get_control("fft_terms") %>% unlist() %>% as.numeric()
vc<-workflow_controls
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
formulae<-lapply(list_of_configs,function(x) create_formula(recipe3,x,ignore_rands = T))

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
names(formulae)<-paste0("formula",1:length(formulae))

list_of_flows<-lapply(formulae,assemble_workflow)

tune_all_these<-as_workflow_set(!!!list_of_flows)

time_id<-summary(recipe_to_use) %>% filter(role=='time_id') %>% select(variable) %>% unlist()
samples<-sliding_index(data1 %>% arrange(across(all_of(!!time_id))),index=!!time_id,step=52,assess_stop=26)

fft_selecting_tune<-workflow_map(tune_all_these,grid=25,resamples=samples)
