#additive ridge regression with adstock and hill function transformations on media variables
#suitable for use with bayeisan tuning methods
setwd('C:\\Users\\t.lorenzen\\Documents\\analytics\\6. MMM v0')
librarian::shelf(tidymodels,tune,recipes,multilevelmod,tidyverse,arrow,workflowsets,bayesian,rethinking,rstan)

source('tidymodels methods.R')
source('mmm functions.R')

control_file<-'tidy model control.xlsx'


#get data
setwd('C:\\Users\\t.lorenzen\\Documents\\analytics\\6. MMM v0')
#groups used to control adstocking, so make sure they are correct here
data1<-read_feather('leads_location_3_product_1_event_3.feather') %>% 
  as_tibble()  %>% 
  mutate(across(c(product),as.factor)) %>% group_by(product,location) %>% arrange(product,location,week) %>% 
  mutate(day_int=as.numeric(week),
         cos1=cos(2*pi*day_int/356),
         cos2=cos(4*pi*day_int/356),
         cos3 =cos(6*pi*day_int/356),
         cos4 = cos(8*pi*day_int/356),
         cos5 = cos(10*pi*day_int/356),
         sin1=sin(2*pi*day_int/356),
         sin2=sin(4*pi*day_int/356),
         sin3=sin(6*pi*day_int/356),
         sin4=sin(8*pi*day_int/356),
         sin5=sin(10*pi*day_int/356)) %>% select(-day_int) %>% mutate(location=as.factor(location))

#get control spreadsheet
var_controls<-readxl::read_xlsx(control_file,'variables')
transform_controls<-readxl::read_xlsx(control_file,'role controls')
workflow_control<-readxl::read_xlsx(control_file,'workflow')

#TODO: write function to perform checkcs on control file: 1) 1 outcome 2) role and role 2 assignment checks 3)


#get names right
data1<-rename_columns_per_controls(data1)


#start recipe by assigning roles
#  small data is good, going to need to loop over recipe repeatedly, lots of internal copying
recipe0<-recipe(head(data1,n=1) ) 

recipe1<-recipe0 %>% bulk_update_role() %>% bulk_add_role() 

recipe2<-recipe1 %>% add_steps_media() %>%  step_select(-product,-has_role('postprocess'))
##TODO: test all variations of tune vs fixed -- test alpha, e.g.


recipe3 <-recipe2 %>% step_mutate(week=as.numeric(week)-19247.65) %>%# step_center(week) %>%
  update_role(c(sin1,sin2,sin3,sin4,sin5,cos1,cos2,cos3,cos4,cos5),new_role='time') %>% 
  add_role(c(sin1,sin2,sin3,sin4,sin5,cos1,cos2,cos3,cos4,cos5),new_role='predictor')


#create formula for multilevel model
groups_and_time<-unlist(summary(recipe3) %>% filter(role %in% c('group','time')) %>% select(variable))
predictors<-unlist(summary(recipe3) %>% 
                     filter(role=='predictor') %>% select(variable))

final_predictors<-predictors[!(predictors %in% groups_and_time)]

built_formula<-paste0('leads~', paste(final_predictors,collapse='+'),'+sin1+cos1+sin2+cos2+week+(1|store) ')



#create bounds based on variable control
boundaries<-make_bound_statements()

#translate user defined priors
user_defined_priors<-make_prior_statements()

#create list of remaining priors -- 'uninformative' priors to be used here which is just normal(0,10)
all_terms<-attr(terms(as.formula(built_formula)),'term.labels') 
all_coefs<-paste0("b_",attr(terms(as.formula(built_formula)),'term.labels') )

#random ints, i.e. 1|<var> we need to make the prior on <var>_int[<var>_id] and the deterministic formula will be <var>_int[<var>_id]
random_ints0<- all_coefs[grep("1 | ",all_coefs,fixed=T)]
random_ints<-gsub("^.*1 \\| ","",random_ints0)

priors_for_random_ints<-lapply(paste0(random_ints,'_int[',random_ints,'_id] ~ normal(65,int_sigma)'),as.formula)
names(priors_for_random_ints)<-random_ints

#TODO: random slopes?

#get diffuse priors on fixed effects not specified by user
fixed_coefs<-all_coefs[!(all_coefs %in% random_ints0)]
fixed_terms<-all_terms[!(all_coefs %in% random_ints0)]


fixed_coefs_needing_priors<-fixed_coefs[!(fixed_coefs %in% names(user_defined_priors))]

priors_for_fixed<-lapply(paste0(fixed_coefs_needing_priors,"~normal(0,10)"),as.formula)
names(priors_for_fixed)<-fixed_coefs_needing_priors

main_model_formula<-as.formula(paste0(as.character(as.formula(built_formula))[2],'~ normal(big_model,big_sigma)'))
#prior on big_sigma
prior_on_big_sigma<-as.formula('big_sigma ~ half_cauchy(0,100)')
#prior on grand intercept
prior_on_a0<-as.formula('a0 ~ normal(50,50)')

#prior_on_store_ints_spread
prior_on_store_int_sigma<-as.formula('int_sigma~half_cauchy(0,10)')

big_model<-paste('big_model <-',paste(fixed_coefs,fixed_terms,sep='*',collapse='+'),'+',paste(paste0(random_ints,"_int[",random_ints,"_id]"),collapse='+'), '+a0')


big_model0<-paste('big_model_1 <-',paste(fixed_coefs[1:15],fixed_terms[1:15],sep='*',collapse='+'))
big_model<-paste(' big_model<- big_model_1','+',paste(fixed_coefs[16:23],fixed_terms[16:23],sep='*',collapse='+'),'+',paste(paste0(random_ints,"_int[",random_ints,"_id]"),collapse='+'), '+a0')
#big_model<-paste('big_model<- b_twitter*twitter + a0')

formula_list<-c(main_model_formula,parse(text=big_model),priors_for_random_ints,priors_for_fixed,user_defined_priors,prior_on_a0,prior_on_big_sigma,prior_on_store_int_sigma)
class(formula_list)<-'list'

form_list2<-c(main_model_formula,parse(text=big_model),parse(text=big_model0),priors_for_random_ints,priors_for_fixed,user_defined_priors,prior_on_a0,prior_on_big_sigma,prior_on_store_int_sigma)
class(form_list2)<-'list'