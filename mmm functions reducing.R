

#get decomps (assume linear model)
# get_decomps_linear<-function(modeled_data=data3,model_coefs=rethinking_results@coef,
#                              predictors=get_predictors_vector(recipe3)){
#   #for now, ignore [bracket] codefs
#   model_coefs<-model_coefs[names(model_coefs) %in% paste0("b_",predictors)]
#   #order them by final_predictors order
#   model_coefs<-model_coefs[order(factor(names(model_coefs),levels=paste0("b_",predictors)))]
#   matrix_data<-modeled_data %>% ungroup() %>% select(all_of(predictors) )%>% as.matrix()
#   for (i in 1:nrow(matrix_data)){
#     matrix_data[i,]<-matrix_data[i,]*model_coefs
#   }
#   decomps<-as_tibble(matrix_data)
#   names(decomps)<-predictors
#   return(decomps)
# }





tune_or_fetch_hyperparameters<-function(tune_this_time,saved_parameter_RDS=NULL,recipe_to_use=recipe3,
                                        model_formula=built_formula,data_set=data1,control_ranges=transform_controls){
  if(tune_this_time){
    #unforutnately, lmer won't run without a random effect in the model formula
    if(grepl("\\|",model_formula)){tune_spec<-linear_reg(engine='lmer')} else{
      tune_spec<-linear_reg(engine='lm')}
    #  hardhat::extract_parameter_set_dials(reg_wf)%>% finalize(data1) 
    #need to build a formula with random effects specs for stan_glmer
    
    
    mmm_wf<-workflow() %>%  add_recipe(recipe_to_use) %>% 
      add_model(tune_spec,formula=as.formula(model_formula)) 
    
    # #testing if stan spec is rigth:
    # best_mmm<-finalize_workflow(mmm_wf,final_hypers)
    # 
    # fit(best_mmm,data1)
    
    #going to stratfiy on store to make sure time series terms aren't stupid . . . ?
    folds<-vfold_cv(data_set,v=5,strata=store,pool=0.001)
    
    #try to tune .  . . fingers crossed!
    
    #get parameters (if tunable methods correctly working this will be all that's needed) 
    tune_these_parms<-create_dials_from_wf_and_controls(workflow=mmm_wf,control_ranges=control_ranges)
    
    #TODO:: add parallel option?
    #create cluster handle
    # this_cl<-parallel::makeCluster(8)
    #make list of variables to copy to each worker:
    # big_var_list<-c(lsf.str(),'new_quant_param','data1','priors_to_add',
    #                 'inject_this_for_signs')
    # vars_that_we_need<-c(big_var_list[big_var_list %in% ls()],'new_quant_param')
    # 
    # parallel::clusterExport(this_cl,varlist=vars_that_we_need)
    # doParallel::registerDoParallel(this_cl)
    if(nrow(tune_these_parms)>0){
      tuning_trials<- tune_bayes(
        mmm_wf,
        resamples = folds,
        initial=length(tune_these_parms$id)*2,
        #grid=20,
        param_info = tune_these_parms
      )
      # parallel::stopCluster(this_cl)
      # final_hypers<-readRDS('best_hypers_b2.RDS')
      final_hypers<-select_best(tuning_trials)
    }else{
      final_hypers=tibble()
    }
    #see what this looks like
    #get groups -- 
    groupings=as.character(groups(data_set))
    time_var<-summary(recipe_to_use) %>% filter(role=='time_id') %>% select(variable) %>% unlist()
    sort_vars<-c(groupings,time_var)
    best_mmm<-finalize_workflow(mmm_wf ,final_hypers)  %>% fit(data_set %>% group_by(across(all_of(groupings))) %>% 
                                                                 arrange(across(all_of(sort_vars))))
    
    # data_set<-data_set %>%  arrange(across(all_of(sort_vars))) %>% group_by(across(all_of(groupings)))
    # best_mmm<-fit(best_mmm,data_set)
    # 
    # fit(best_mmm,data1)
    #saveRDS(best_mmm,'best_mmm_b3.RDS')
    saveRDS(final_hypers,saved_parameter_RDS)
    return(final_hypers)
    
    
  }else{
    return(readRDS(saved_parameter_RDS))
  }
}



#' Creates a string that represents a model formula from a recipe and the workflow controls data
#' 
#' @param base_recipe defaults to recipe3; is the recipe containing variables to build a formula for
#' @param control defaults to workflow_controls, should be a tibble with columns R_name and Value, which must have rows with R_name =='Y','list_rand_ints' and 'fft_terms'
#' @param ignore_rands defaults to FALSE, but if set to TRUE the created formula 
#' will not contain random intercepts or slopes other than fourier transform 
#' related slopes (if those are specified in the control frame)
#' @return
#' a string that reads like an lmer formula
#' @export
#' 
#' @example
create_formula<-function(base_recipe=recipe3,control=workflow_controls,ignore_rands=FALSE){
  #we will remove grouping vars (used for random effects) and time series stuff (for now)
  
  final_predictors<-get_predictors_vector(base_recipe=base_recipe)
  outcome<-get_control(this_control='Y',control=control)  
  fft_interaction_term<-get_control("interaction_fft",control=control)
  
  fft_terms<-get_control("fft_terms",control=control) |> as.numeric()
  fft_terms<-ifelse(is.na(fft_terms),0,fft_terms)
  
  fft_formula<-""
  
  if(fft_terms>0){
    for(i in 1:fft_terms){
      if (i==1){
        if(fft_interaction_term!="" & !is.na(fft_interaction_term)){ fft_formula=
            paste0("(sin1|",fft_interaction_term,") + (cos1|",fft_interaction_term,")")}
        else{  fft_formula= 'sin1 + cos1'}
      }
      else{
        if(fft_interaction_term!="" & !is.na(fft_interaction_term)){
          new_term<-paste(paste0('(',c('cos','sin'),i,'|',fft_interaction_term,')'),sep='',collapse='+')
          fft_formula=paste(fft_formula,new_term,sep='+')
        }
        else{
        fft_formula=paste(fft_formula,paste(c('cos','sin'),i,sep='',collapse='+'),sep='+')
        }
      }}}
  
  list_rand_ints<-gsub(" ","",get_control("list_rand_ints",control=control),fixed=T) |> strsplit(',',fixed=T) |> unlist()
  list_rand_ints<-list_rand_ints[!is.na(list_rand_ints)]
  
  rand_int_formula<-""
  if(length(list_rand_ints)>0){
    rand_int_formula<-paste(list_rand_ints,collapse =' + ')
  }
  
  list_rand_slopes<-gsub(" ","",get_control("list_rand_slopes",control=control),fixed=T) |> strsplit(',',fixed=T) |> unlist()
  list_rand_slopes<-list_rand_slopes[!is.na(list_rand_slopes)]
  
  rand_slope_formula<-""
  if(length(list_rand_slopes)>0){
    rand_slope_formula<-paste(list_rand_slopes,collapse =' + ')
  }
  
  if(!ignore_rands){
   built_formula<-paste(paste0(outcome,' ~ ',
                              paste(final_predictors,collapse=' + ')),
                       fft_formula,rand_int_formula,rand_slope_formula,sep=' + ')
  }
  if(ignore_rands){
    built_formula<-paste(paste0(outcome,' ~ ',
                                paste(final_predictors,collapse=' + ')),
                         fft_formula,sep=' + ')
  }
  #remove trailing +, left by the paste in built_formula if one of fft_formula, 
  #rand_int_formula, or rand_slope_formula are empty strings
  total_times<-(length(list_rand_slopes)==0)+(length(list_rand_ints)==0)+(fft_terms==0)
  
  if (total_times>0){
    for(i in 1:total_times){
      built_formula<-gsub("(\\+\\s* $)","",built_formula)
    }
    built_formula<-trimws(built_formula)
  
  }
  built_formula <- gsub("\\+  \\+", "\\+", built_formula)  
  return(built_formula)
}


