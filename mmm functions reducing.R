
#extract Value from workflow_controls


#inputs are var_controls worksheet -- let's call it prior_controls, and the formula (built_formula)



#light testing

# create_ulam_list()

# lotsa_vars<-paste0('x',1:8)
# new_x<-replicate(60,runif(nrow(data1))) %>% as_tibble()
# names(new_x)<-lotsa_vars
# 
# 
# data_for_lotsa_vars<-cbind(data1 ,new_x)
# recipea<-recipe(head(data_for_lotsa_vars,n=1) )
# 
# recipeb<-recipea %>% bulk_update_role() %>% bulk_add_role()
# 
# recipeb<-recipeb %>% add_steps_media() %>%  step_select(-has_role('postprocess'))

# 
# recipec <-recipeb %>% step_mutate(week=as.numeric(week)-19247.65) %>%# step_center(week) %>%
#   update_role(c(sin1,sin2,sin3,cos1,cos2,cos3),new_role='time') %>%
#   add_role(c(sin1,sin2,sin3,cos1,cos2,cos3),new_role='predictor')
# recipec<-recipec %>% update_role(starts_with('x'),new_role='predictor')
# 
# formula_with_lotsa_vars<-create_formula(recipec)
# create_ulam_list(model_formula=formula_with_lotsa_vars)



#get decomps (assume linear model)
get_decomps_linear<-function(modeled_data=data3,model_coefs=rethinking_results@coef,
                             predictors=get_predictors_vector(recipe3)){
  #for now, ignore [bracket] codefs
  model_coefs<-model_coefs[names(model_coefs) %in% paste0("b_",predictors)]
  #order them by final_predictors order
  model_coefs<-model_coefs[order(factor(names(model_coefs),levels=paste0("b_",predictors)))]
  matrix_data<-modeled_data %>% select(all_of(predictors) )%>% as.matrix()
  for (i in 1:nrow(matrix_data)){
    matrix_data[i,]<-matrix_data[i,]*model_coefs
  }
  decomps<-as_tibble(matrix_data)
  names(decomps)<-predictors
  return(decomps)
}

add_fourier_vars<-function(data_to_use=data1,vc=var_controls){
  time_id_var <-vc%>% filter(role=='time_id') %>% select(varname) %>% unlist()
  
  time_id_vec<-data_to_use %>% select(all_of(time_id_var)) %>% unlist()
  if(!is.numeric(time_id_vec)){stop("non numeric vector used as time_id var.  
                                  For variable set as time_id var (ie where role=time_id), please reset it to be a date or number representing a date ")}
  
  data_to_use$day_int=time_id_vec
  return(data_to_use %>% mutate(cos1=cos(2*pi*day_int/356),
                                cos2=cos(4*pi*day_int/356),
                                cos3 =cos(6*pi*day_int/356),
                                cos4 = cos(8*pi*day_int/356),
                                cos5 = cos(10*pi*day_int/356),
                                sin1=sin(2*pi*day_int/356),
                                sin2=sin(4*pi*day_int/356),
                                sin3=sin(6*pi*day_int/356),
                                sin4=sin(8*pi*day_int/356),
                                sin5=sin(10*pi*day_int/356)) %>% select(-day_int))
}

add_groups_and_sort<-function(data_to_use=data1,vc=var_controls){
  #extract groups from vc
  groupings<-vc %>% filter(role2=='group') %>% select(varname) %>% unlist()
  names(groupings)<-NULL
  time_id_var <-vc%>% filter(role=='time_id') %>% select(varname) %>% unlist()
  if(length(groupings)>0){
    
    data_to_use<-data_to_use%>% 
      mutate(across(all_of(!!groupings),as.factor))
    
    return(data_to_use %>%  group_by(across(all_of(groupings))) %>% 
             arrange(across(all_of( c(!!groupings,!!time_id_var)))) )
  }
  else{
    return(data_to_use %>% arrange(across(all_of(c(!!time_id_var))) )
           ) }
}

create_recipe<-function(data_to_use=data1,vc=var_controls){
  #start recipe by assigning roles
  #  small data is good, going to need to loop over recipe repeatedly, lots of internal copying
  
  # if(adding_trend){
  #   #identifyt he time variable and flag if more than 1
  #   vc %>% filter(role2=='trend',role=='time_id') %>% select(varname) %>% unlist()->trend_vars
  #   if(length(trend_vars)>1){stop('control file asks to add a centered trend but two variables are defined as time predictors.\nCheck the variables tab for role2="trend", role="time_id" and make sure only a single variable is listed' )}
  #   if(length(trend_vars)==0){stop('control file asks to add a centered trend but no variable has role="time_id" and role2 = "trend".')}
  #   # column_to_change<-data_to_use %>%ungroup() %>%  select(!!trend_vars) %>% unlist() %>% as.numeric()
  #   # data_to_use[trend_vars]<-column_to_change
  # }
  groupings<-as.character(groups(data_to_use))
  
  print(groupings)
  recipe0<-recipe(head(data_to_use,n=1) ) 
  
  recipe1<-recipe0 %>% bulk_update_role() %>% bulk_add_role() 
  # if(adding_trend){
  #   recipe1<-recipe1 %>% add_role(has_role('trend'),new_role='predictor')# %>% step_center(has_role='trend') 
  # }else{
  # #   #this will remove the trend term from forumals down stream
  #   recipe1<-recipe1 %>% update_role(has_role('trend'),new_role='time',old_role='trend') 
  # }
  recipe2<-recipe1 %>% add_steps_media() %>%  step_select(-has_role('postprocess'))
  ##TODO: test all variations of tune vs fixed -- test alpha, e.g.
  
  recipe3 <-recipe2  %>%# step_center(week) %>%
    update_role(c(sin1,sin2,sin3,sin4,sin5,cos1,cos2,cos3,cos4,cos5),new_role='time') %>% 
    add_role(c(sin1,sin2,sin3,sin4,sin5,cos1,cos2,cos3,cos4,cos5),new_role='predictor') %>% 
    step_novel(all_of(!!groupings)) %>% 
     step_mutate_at(all_of(!!groupings),fn=list(id=as.integer))
  return(recipe3)
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

