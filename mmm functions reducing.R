

#get decomps (assume linear model)
get_decomps_linear<-function(modeled_data=data3,model_coefs=rethinking_results@coef,
                             predictors=get_predictors_vector(recipe3)){
  #for now, ignore [bracket] codefs
  model_coefs<-model_coefs[names(model_coefs) %in% paste0("b_",predictors)]
  #order them by final_predictors order
  model_coefs<-model_coefs[order(factor(names(model_coefs),levels=paste0("b_",predictors)))]
  matrix_data<-modeled_data %>% ungroup() %>% select(all_of(predictors) )%>% as.matrix()
  for (i in 1:nrow(matrix_data)){
    matrix_data[i,]<-matrix_data[i,]*model_coefs
  }
  decomps<-as_tibble(matrix_data)
  names(decomps)<-predictors
  return(decomps)
}

get_predictors_vector<-function(base_recipe=recipe3){
  groups_and_time<-unlist(summary(base_recipe) |> filter(role %in% c('group','time')) |> select(variable))
  
  predictors<-unlist(summary(base_recipe) |> 
                       filter(role=='predictor') |> select(variable))
  names(predictors)<-NULL
  return(predictors[!(predictors %in% groups_and_time)])
}

#get decomps (assume linear model)
get_decomps_irregardless<-function(data_to_use=data3,model_obj=rethinking_results,
                             predictors=get_predictors_vector(recipe3),sample_size=1000){
  
  #todo:: have to build from int only to total, and then rescale by taking all the extra
  # and distributing it per contribution to the sum 
  #make a list of data_to_use versions -- one with each predictor set to 0
  variations<-vector('list',length=length(predictors))
  for(i in 1:length(variations)){
    variations[[i]]<-data_to_use
    variations[[i]][predictors[i]]<-0
  }
  names(variations)<-predictors
  
  preds_variations<-lapply(variations,function(x) {colMeans(link(model_obj,x,n=sample_size)$big_model)})
  
  preds_main<-colMeans(link(model_obj,data_to_use,n=sample_size)$big_model)
  
  #intercept_only
 
  # int_off_preds<-colMeans(link(model_obj,
  #                               data_to_use |>
  #                                mutate(across(all_of(names(data_to_use)[grepl("_id",names(data_to_use))]),function(x) 0 )),
  #                               n=sample_size
  #                               )$big_model)
  
  #this initial delta will be not additive if model is mutiplicative
  list_decomps_0<-lapply(preds_variations,function(x) preds_main-x)
  decomps_0<-as_tibble(list_decomps_0) 
  #sum decomps_0 . . .
  # decomps_0$int_only<-int_off_preds-preds_main
  decomps_0$decomp0_tot<-rowSums(decomps_0,na.rm=T)
  decomps_0$preds_base=preds_main
  
  decomps_0$ratio<-  decomps_0$preds_base/decomps_0$decomp0_tot
  decomps_1<- decomps_0 |>mutate(across(all_of(!!predictors),function(x) x*ratio))
  
  
  #for now, ignore [bracket] codefs
  
  
  model_coefs<-model_coefs[names(model_coefs) %in% paste0("b_",predictors)]
  #order them by final_predictors order
  model_coefs<-model_coefs[order(factor(names(model_coefs),levels=paste0("b_",predictors)))]
  matrix_data<-modeled_data %>% ungroup() %>% select(all_of(predictors) )%>% as.matrix()
  for (i in 1:nrow(matrix_data)){
    matrix_data[i,]<-matrix_data[i,]*model_coefs
  }
  decomps<-as_tibble(matrix_data)
  names(decomps)<-predictors
  return(decomps)
}


update_range_from_control<-function(parameter_set,controls){
  for(i in 1:length(parameter_set$id)){
    parameter_set$object[i][[1]]<-do.call(
      unlist(controls[controls$dial_id==parameter_set$id[i],'dial_func']),
      list(range=unlist(controls[controls$dial_id==parameter_set$id[i],'new_range']))
    )
  }
  return(parameter_set)
}

create_dials_from_wf_and_controls<-function(workflow=mmm_wf,control_ranges=transform_controls){
  tune_these_parms<-extract_parameter_set_dials(workflow) #will have default ranges
  if(nrow(tune_these_parms)==0){return(tune_these_parms)}
  #get ranges from transform_controls
  gamma_ranges<-control_ranges %>%rowwise() %>%  mutate(dial_id=paste0(role,"_saturation_speed")) %>% 
    mutate(new_range=map2(list(saturation_speed_low),list(saturation_speed_high),~c(.x,.y)),
           dial_func='saturation_speed') %>% select(dial_id,new_range,dial_func)
  
  alpha_ranges<-control_ranges %>%rowwise() %>%  mutate(dial_id=paste0(role,"_asymptote")) %>% 
    mutate(new_range=map2(list(asymptote_low),list(asymptote_high),~c(.x,.y)),dial_func='asymptote') %>% select(dial_id,new_range,dial_func)
  
  retention_ranges<-control_ranges %>%rowwise() %>%  mutate(dial_id=paste0(role,"_retention")) %>% 
    mutate(new_range=map2(list(retention_low),list(retention_high),~c(.x,.y)),dial_func='retention') %>% select(dial_id,new_range,dial_func)
  
  
  tune_these_parms<-update_range_from_control(tune_these_parms,rbind(gamma_ranges,alpha_ranges,retention_ranges))
  return(tune_these_parms)
}

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

