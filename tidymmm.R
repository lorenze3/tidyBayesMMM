#TODO: figure out what to do about having the tuning or not in a script -- it blocks
# some of the script flow around lmer preds . .. probably don't need that?
#TODO: reshape this as an example script
#TODO: put more of the tuning steps inside the if tune this time frame -- make a function?
#TODO: create response curves from final model results


#devtools::install_local('C:\\Users\\loren\\Documents\\R\\mostlytidyMMM',force=T)
setwd('C:\\Users\\loren\\Documents\\R\\tidymmm')
librarian::shelf(tidymodels,tune,recipes,multilevelmod,tidyverse,arrow,workflowsets,rethinking,rstan)


#source('tidymodels methods.R')
library(mostlytidyMMM)
source('mmm functions reducing.R')

control_file<-'example model control.xlsx'

#get control spreadsheet
#var_controls -- must have 1 and only 1 role=time_id record
#TODO: write function to perform checkcs on control file: 1) 1 outcome 2) role and role 2 assignment checks 3)

var_controls<-readxl::read_xlsx(control_file,'variables')
transform_controls<-readxl::read_xlsx(control_file,'role controls')

workflow_controls<-readxl::read_xlsx(control_file,"workflow") %>% select(-desc)

tune_this_time<-get_control('tune_this_time')

#read data and get names right;
data1<-data.table::fread("example2.csv") %>% 
  rename_columns_per_controls()%>% mutate(week=as.Date(week,"%m/%d/%Y"))

data1<-add_fourier_vars(data_to_use=data1,vc=var_controls) %>% 
  add_groups_and_sort(vc=var_controls) 

recipe3<-create_recipe(data_to_use = data1)#,adding_trend = get_control("add_trend"))
#build formula to match config file and dataset


#create bounds based on variable control
boundaries<-make_bound_statements()

formula_list2<-create_ulam_list(prior_controls=var_controls, model_formula=built_formula)

# tune_spec<-bayesian(family='gaussian',engine='brms',mode='regression',chains=4,iter=4000,
#                     stanvars = if(exists('inject_this_for_signs')) inject_this_for_signs else NULL,
#                     #save_model = 'mmm stan code.stan',
#                     init=0,
#                     algorithm = 'sampling',
#                     prior = if(exists('priors_to_add'))priors_to_add  else NULL)

tune_this_time=F
use_these_hypers<-tune_or_fetch_hyperparameters(tune_this_time,
                                                saved_parameter_RDS='best_hypers_lmer.RDS',
                                                recipe_to_use=recipe3,
                                        model_formula=built_formula,
                                        data_set=data1,control_ranges=transform_controls)


# fin_rec_3<-recipe3 %>% finalize_recipe(use_these_hypers) %>% prep()
# new_data_to_fake<-bake(fin_rec_3,data1)


# data1$pred_lmer<-predict(best_mmm %>% extract_fit_engine(),new_data_to_fake)
# 
# rsq(data1 %>% ungroup() ,truth = !!outcome,estimate = pred_lmer)


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

# raw_data4<-head(data1)
# raw_data4$store=as.factor("Albany")
# 
# data5<-bake(recipe_finalized,raw_data4)
#  data5$store_id=NULL
#TODO: setup <var>_id columns for every random int!

data3 <-data3 

rethinking_results<-ulam(formula_list2,
               chains=4,iter=4000,
               thin=1,
               data=data3,
               constraints = boundaries,
               sample = T,
               #pars=c('b_week','a0','store_int',paste0('b_',final_predictors),'big_sigma','int_sigma'),
               cmdstan = T,
               file='ulam_fit_test_rs',
               cores=4,
               declare_all_data=F,
               messages=F
               )

# reth2<-readRDS('ulam_fit_test.RDS')

# model_text<-if (is.list(rethinking_results)){rethinking_results$model}else{rethinking_results@model}
# fileConn<-file("ulam stan rs.stan")
# writeLines(model_text, fileConn)
# close(fileConn)

data4<-data3



pp<-predict.ulam(rethinking_results,data4)

data4$hat<-pp[,1]  #colMeans(link(rethinking_results,data4)$big_model)

rsq(data4,truth = sales,estimate=hat)

ggplot(data4 ,aes(x=sales,y=hat,color=store_id))+
  geom_point()+ geom_abline(slope=1,intercept=0)+ggthemes::theme_tufte()+
  ggtitle("Predicted vs Actual")



#plot decomp
fin_pred<-get_predictors_vector(recipe3)
names(fin_pred)<-NULL

recipe6<-recipe_finalized %>% step_select(-week)

decomps<-get_decomps_irregardless(data_to_use = data3, recipe_to_use=recipe3) %>% mutate(residual=sales-pred)

#decomps$sales<-data4$sales


decomps_natl<-decomps %>% select(week,all_of(!!fin_pred),residual) %>% group_by(week) %>% summarise(across(where(is.numeric),sum))

decomps_natl<-decomps_natl %>% pivot_longer(cols=c(-week))

ggplot(data=decomps_natl,aes(x=week,y=value,fill=name)) + geom_area()+ggthemes::theme_tufte()+
  ggtitle("Decomposition By Week")+
  theme(legend.position = 'bottom')

################



natl_plot<-data4  %>% group_by(week) %>% summarise(across(all_of(c('hat','sales',
                                                                  !!fin_pred)),
                                                         sum))

natl_plot_long<-natl_plot %>% select(week,hat,sales) %>% 
  pivot_longer(c(hat,sales),values_to='sales') %>% 
  mutate(name=ifelse(name=='hat','pred',name))
# ggplot(data4 %>% filter(leads<2000),aes(x=week,y=leads,group=store_id))+geom_smooth()
natl_rsq<-round(rsq(natl_plot,truth = sales,estimate=hat)[3],2)

ggplot(natl_plot_long,aes(x=week,y=sales,color=name))+geom_point() +ggthemes::theme_tufte()+
  ggtitle("Aggregated Leads, LMER Predicted Leads, and Bayesian Multilevel Leads",
          subtitle = paste0("rsq = ",natl_rsq))+
  guides(color=  guide_legend(title=NULL))+geom_line()


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

