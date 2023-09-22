#additive ridge regression with adstock and hill function transformations on media variables
#suitable for use with bayeisan tuning methods
setwd('C:\\Users\\loren\\Documents\\R\\tidymmm')
librarian::shelf(tidymodels,tune,recipes,multilevelmod,tidyverse,arrow,workflowsets,rethinking,rstan)

source('tidymodels methods.R')
source('mmm functions.R')

control_file<-'example model control.xlsx'
#get control spreadsheet
var_controls<-readxl::read_xlsx(control_file,'variables')
transform_controls<-readxl::read_xlsx(control_file,'role controls')

workflow_controls<-readxl::read_xlsx(control_file,"workflow") %>% select(-desc)

tune_this_time<-get_control('tune_this_time')

#read data and get names right;
data1<-data.table::fread("example.csv") %>% rename_columns_per_controls()
  



# read_feather('leads_location_3_product_1_event_3.feather') %>% 
data1<-data1 %>%   as_tibble()  %>% 
  mutate(across(c(product),as.factor)) %>% group_by(product,store) %>% arrange(product,store,week) %>% 
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
         sin5=sin(10*pi*day_int/356)) %>% select(-day_int) %>% mutate(store=as.factor(store))


#TODO: write function to perform checkcs on control file: 1) 1 outcome 2) role and role 2 assignment checks 3)



#start recipe by assigning roles
#  small data is good, going to need to loop over recipe repeatedly, lots of internal copying
recipe0<-recipe(head(data1,n=1) ) 

recipe1<-recipe0 %>% bulk_update_role() %>% bulk_add_role() 

recipe2<-recipe1 %>% add_steps_media() %>%  step_select(-has_role('postprocess'))
##TODO: test all variations of tune vs fixed -- test alpha, e.g.


recipe3 <-recipe2 %>% step_mutate(week=as.numeric(week)-19247.65) %>%# step_center(week) %>%
  update_role(c(sin1,sin2,sin3,cos1,cos2,cos3),new_role='time') %>% 
  add_role(c(sin1,sin2,sin3,cos1,cos2,cos3),new_role='predictor')


#build formula to match config file and dataset
built_formula<-create_formula()

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

if(length(random_ints0)>0){
priors_for_random_ints<-lapply(paste0(random_ints,'_int[',random_ints,'_id] ~ normal(65,int_sigma)'),as.formula)
names(priors_for_random_ints)<-random_ints
}else{
  priors_for_random_ints=list()
}
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

#more than around 15 terms, need to split it
number_of_terms<-length(fixed_coefs)
rand_ints_formula_for_ulam<-  ifelse(length(random_ints0)>0,
         paste(paste0(random_ints,"_int[",random_ints,"_id]"),collapse='+'),
         '')
if(number_of_terms<=15){
big_model<-paste('big_model <-',paste(fixed_coefs,fixed_terms,sep='*',collapse='+'),'+',rand_ints_formula_for_ulam, '+a0')
}else{
big_model0<-paste('big_model <-',paste(fixed_coefs[1:15],fixed_terms[1:15],sep='*',collapse='+'))
big_model<-paste(' big_model<- big_model','+',paste(fixed_coefs[16:number_of_terms],fixed_terms[16:number_of_terms],sep='*',collapse='+'),'+',rand_ints_formula_for_ulam, '+a0')
}
#big_model<-paste('big_model<- b_twitter*twitter + a0')
if(number_of_terms<=15){
formula_list<-c(main_model_formula,parse(text=big_model),priors_for_random_ints,priors_for_fixed,user_defined_priors,prior_on_a0,prior_on_big_sigma,prior_on_store_int_sigma)
}else{
formula_list<-c(main_model_formula,parse(text=big_model),parse(text=big_model0),priors_for_random_ints,priors_for_fixed,user_defined_priors,prior_on_a0,prior_on_big_sigma,prior_on_store_int_sigma)
}
class(formula_list)<-'list'
# ulam_list<-c(priors_for_random_ints,priors_for_fixed,user_defined_priors,prior_on_a0,prior_on_big_sigma,prior_on_store_int_sigma,main_model_formula)
# tune_spec<-bayesian(family='gaussian',engine='brms',mode='regression',chains=4,iter=4000,
#                     stanvars = if(exists('inject_this_for_signs')) inject_this_for_signs else NULL,
#                     #save_model = 'mmm stan code.stan',
#                     init=0,
#                     algorithm = 'sampling',
#                     prior = if(exists('priors_to_add'))priors_to_add  else NULL)

if(rand_int_formula==""){tune_spec<-linear_reg(engine='lm')} else{
  tune_spec<-linear_reg(engine='lmer')}
#  hardhat::extract_parameter_set_dials(reg_wf)%>% finalize(data1) 
#need to build a formula with random effects specs for stan_glmer


mmm_wf<-workflow() %>%  add_recipe(recipe3) %>% 
  add_model(tune_spec,formula=as.formula(built_formula)) 

# #testing if stan spec is rigth:
# best_mmm<-finalize_workflow(mmm_wf,final_hypers)
# 
# fit(best_mmm,data1)


#going to stratfiy on store to make sure time series terms aren't stupid . . . ?
folds<-vfold_cv(data1,v=5,strata=store,pool=0.001)

#try to tune .  . . fingers crossed!

#get parameters (if tunable methods correctly working this will be all that's needed) 
tune_these_parms<-extract_parameter_set_dials(mmm_wf) #will have default ranges

#get ranges from transform_controls
gamma_ranges<-transform_controls %>%rowwise() %>%  mutate(dial_id=paste0(role,"_saturation_speed")) %>% 
  mutate(new_range=map2(list(saturation_speed_low),list(saturation_speed_high),~c(.x,.y)),
         dial_func='saturation_speed') %>% select(dial_id,new_range,dial_func)

alpha_ranges<-transform_controls %>%rowwise() %>%  mutate(dial_id=paste0(role,"_asymptote")) %>% 
  mutate(new_range=map2(list(asymptote_low),list(asymptote_high),~c(.x,.y)),dial_func='asymptote') %>% select(dial_id,new_range,dial_func)

retention_ranges<-transform_controls %>%rowwise() %>%  mutate(dial_id=paste0(role,"_retention")) %>% 
  mutate(new_range=map2(list(retention_low),list(retention_high),~c(.x,.y)),dial_func='retention') %>% select(dial_id,new_range,dial_func)

all_transform_ranges<-rbind(gamma_ranges,alpha_ranges,retention_ranges) 

update_range_from_control<-function(parameter_set,controls){
  for(i in 1:length(parameter_set$id)){
    parameter_set$object[i][[1]]<-do.call(
      unlist(all_transform_ranges[all_transform_ranges$dial_id==parameter_set$id[i],'dial_func']),
      list(range=unlist(all_transform_ranges[all_transform_ranges$dial_id==parameter_set$id[i],'new_range']))
    )
  }
  return(parameter_set)
}

tune_these_parms<-update_range_from_control(tune_these_parms,all_transform_ranges)



if(tune_this_time){

  #create cluster handle
  # this_cl<-parallel::makeCluster(8)
  #make list of variables to copy to each worker:
  big_var_list<-c(lsf.str(),'new_quant_param','data1','priors_to_add',
                  'inject_this_for_signs')
  vars_that_we_need<-c(big_var_list[big_var_list %in% ls()],'new_quant_param')
  
   # parallel::clusterExport(this_cl,varlist=vars_that_we_need)
   # doParallel::registerDoParallel(this_cl)
  
  tuning_trials<- tune_bayes(
    mmm_wf,
    resamples = folds,
    initial=length(tune_these_parms$id)*2,
    #grid=20,
    param_info = tune_these_parms
  )
  # parallel::stopCluster(this_cl)
  # final_hypers<-readRDS('best_hypers_b2.RDS')
  final_hypers3<-select_best(tuning_trials)
  
  #see what this looks like
  best_mmm<-finalize_workflow(mmm_wf ,final_hypers3)  %>% fit(data1 %>% group_by(product,store) %>% arrange(product,store,week))
  
  data1<-data1 %>% arrange(store,week) %>% group_by(store)
  best_mmm<-fit(best_mmm,data1)
  
  # 
  # fit(best_mmm,data1)
  
  
  #saveRDS(best_mmm,'best_mmm_b3.RDS')
   saveRDS(final_hypers3,'best_hypers_lmer.RDS')
}
use_these_hypers<-readRDS('best_hypers_lmer.RDS')

fin_rec_3<-recipe3 %>% finalize_recipe(use_these_hypers) %>% prep()
new_data_to_fake<-bake(fin_rec_3,data1)
data1$pred_lmer<-predict(best_mmm %>% extract_fit_engine(),new_data_to_fake)

rsq(data1 %>% ungroup() ,truth = !!outcome,estimate = pred_lmer)


#now fit mmm using bayesian (sign constraint and priors and . . .)
# bayes_spec<-bayesian(family='gaussian',engine='brms',mode='regression',chains=4,iter=4000,
#                                        #stanvars = if(exists('inject_this_for_signs')) inject_this_for_signs else NULL,
#                                        save_model = 'mmm stan code.stan',
#                                        init=0,
#                                        algorithm = 'sampling',
#                                        thin=10,
#                                        # file='some_stan_model_file.RDS',
#                                        prior = if(exists('priors_to_add') )priors_to_add  else NULL)


# use_these_hypers<-readRDS('best_hypers_lmer.RDS')

# mmm_bayes_wf<-workflow() %>%  add_recipe(recipe3) %>% 
#   add_model(bayes_spec,formula=as.formula(built_formula)) %>% finalize_workflow(use_these_hypers) %>% fit(data1)
# saveRDS(mmm_bayes_wf,'best_bayes_mmm.RDS')

recipe3 %>% finalize_recipe(use_these_hypers) %>% prep()-> recipe_finalized

data3<-bake(recipe_finalized ,data1)

#TODO: setup <var>_id columns for every random int!

data3 <-data3 %>% ungroup() 
data3$store_id<-rethinking::coerce_index(data3$store)
data3<-data3 %>% select(-store,-product)






rethinking_results<-ulam(formula_list,
               chains=4,iter=4000,
               thin=1,
               data=data3,
               constraints = boundaries,
               sample = T,
               #pars=c('b_week','a0','store_int',paste0('b_',final_predictors),'big_sigma','int_sigma'),
               cmdstan = T,
               file='ulam_fit_test',
               cores=4,
               declare_all_data=F,
               messages=F
               )

model_text<-if (is.list(rethinking_results)){rethinking_results$model}else{rethinking_results@model}
fileConn<-file("ulam stan.stan")
writeLines(model_text, fileConn)  
close(fileConn)

data4<-data3

predict.ulam<-function(ulamobj,new_data,n=1000,reduce=T,conf=.95){
 link_output<-link(ulamobj,new_data=new_data,n=n)
 if(reduce){
   preds<-colMeans(link_output)
   low_conf<-apply(link_output,2,quantile,1-conf)
   high_conf<-apply(link_output,2,quantile,conf)
   pred_output<-cbind(preds,low_conf,high_conf)
   colnames(pred_output)<-c('pred',paste0('pred_lower_',conf),paste0('pred_upper_',conf))
 }
 return(pred_output)
}
pp<-predict(rethinking_results,data4)

data4$hat<-pp[,1]  #colMeans(link(rethinking_results,data4)$big_model)

rsq(data4,truth = sales,estimate=hat)

ggplot(data4 ,aes(x=sales,y=hat,color=store_id))+
  geom_point()+ geom_abline(slope=1,intercept=0)+ggthemes::theme_tufte()+
  ggtitle("Predicted vs Actual",subtitle="store 170 looks a bit off . . .")


data1$preds<-data4$hat

natl_plot<-data1  %>% group_by(week) %>% summarise(across(all_of(c('preds','sales','pred_lmer',
                                                                  !!final_predictors)),
                                                         sum))

natl_plot_long<-natl_plot %>% select(week,preds,pred_lmer,sales) %>% 
  pivot_longer(c(preds,sales,pred_lmer),values_to='sales') %>% 
  mutate(name=ifelse(name=='sales','actual',name))
# ggplot(data4 %>% filter(leads<2000),aes(x=week,y=leads,group=store_id))+geom_smooth()
natl_rsq<-round(rsq(natl_plot,truth = sales,estimate=preds)[3],2)

ggplot(natl_plot_long,aes(x=week,y=sales,color=name))+geom_point() +ggthemes::theme_tufte()+
  ggtitle("Aggregated Leads, LMER Predicted Leads, and Bayesian Multilevel Leads",
          subtitle = paste0("rsq = ",natl_rsq))+
  guides(color=  guide_legend(title=NULL))+geom_line()

#get decomps (assume linear model)
get_decomps_linear<-function(modeled_data=data3,model_coefs=rethinking_results@coef){
  #for now, ignore [bracket] codefs
  model_coefs<-model_coefs[names(model_coefs) %in% paste0("b_",final_predictors)]
  #order them by final_predictors order
  model_coefs<-model_coefs[order(factor(names(model_coefs),levels=paste0("b_",final_predictors)))]
  matrix_data<-modeled_data %>% select(all_of(final_predictors) )%>% as.matrix()
  for (i in 1:nrow(matrix_data)){
    matrix_data[i,]<-matrix_data[i,]*model_coefs
  }
  decomps<-as_tibble(matrix_data)
  names(decomps)<-final_predictors
  return(decomps)
}
decomps<-get_decomps_linear() 
#TODO:need to get accurate cost data
#media0 %>% filter(str_starts(event,"Camping World\\|\\|Facebook"),week>="2022-01-01",week<='2023-06-01') %>% summarise(sum(spend))
costs<-fread('costs_event_level_3.csv')
costs$platform<-paste0('spend_',costs$platform) 
costs<-costs %>% rename(start_name=platform) %>% inner_join(var_controls %>% select(
  varname,start_name) 
) %>% mutate(decomp=gsub("spend_","",varname,fixed=T))

dim_event<-arrow::read_feather('dim_event.feather')

#get cost per leads for all terms with roles not time or group? already in final_predictors
media_for_summary<-read_feather('media_for_summary.feather')
compute_cost_per<-function(varname,spend_data=costs,
                           decomp_data=decomps){
  spend_vec<-costs$spend
  names(spend_vec)<-costs$decomp
  
  decomp_total<-colSums(decomp_data)
  names(decomp_total)<-names(decomp_data)
  return(round(spend_vec/decomp_total,2))
}
compute_cost_per()
  


#plot decomp
fin_pred<-final_predictors
names(fin_pred)<-NULL

decomps<-get_decomps_linear() %>%rowwise() %>%  mutate(media=sum(c_across(all_of(fin_pred))))
decomps$week<-data1$week 
decomps$leads<-data1$leads
decomps$base<-decomps$leads-decomps$media


decomps_natl<-decomps %>% group_by(week) %>% summarise(across(where(is.numeric),sum))

decomps_natl<-decomps_natl %>% pivot_longer(cols=c(-week,-leads,-media))

ggplot(data=decomps_natl,aes(x=week,y=value,fill=name)) + geom_area()+ggthemes::theme_tufte()+
  ggtitle("Leads Decomposition By Week",subtitle="no one believes taboola is this good")+
  theme(legend.position = 'bottom')

#changed to half cauchy sigma prior & turn off cmdstan
# saveRDS(rethinking_results,'ulam_fit.RDS')


# rethinking_results<-readRDS('ulam_out.RDS')



#manual call -- used to insert some print statments and learn that
#the ulam_list order needs to be correct for this to work
# stan_obj<-rstan::stan('ulam stan a.stan',model_name='find_the_nans',data=data3,iter=10,
#             chains=1,cores=1,algorithm='NUTS')


#example with multiple intercepts from ulam function doc

# library(rethinking)
# data(chimpanzees)
# 
# d <- list( 
#   pulled_left = chimpanzees$pulled_left ,
#   prosoc_left = chimpanzees$prosoc_left ,
#   condition = as.integer( 2 - chimpanzees$condition ) ,
#   actor = as.integer( chimpanzees$actor ) ,
#   blockid = as.integer( chimpanzees$block )
# )
# 
# m2 <- ulam(
#   alist(
#     pulled_left ~ bernoulli(theta),
#     logit(theta) <- a + aj[actor] + bp[condition]*prosoc_left,
#     aj[actor] ~ normal( 0 , sigma_actor ),
#     a ~ normal(0,4),
#     bp[condition] ~ normal(0,1),
#     sigma_actor ~ exponential(1)
#   ) ,
#   data=d, chains=2 , cores=1 , sample=T,iter=10)


# fileConn<-file("example.stan")
# writeLines(m2@model, fileConn)
# close(fileConn)

