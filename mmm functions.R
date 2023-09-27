

#functions to loop over controls tibbles and create recipe steps
bulk_update_role<-function(this_recipe,vars_to_append_roles=var_controls$varname,roles_to_be_appended=var_controls$role){
  unique_roles<-unique(roles_to_be_appended)
  for (i in 1:length(unique_roles)){
    this_role<-unique_roles[i]
    this_recipe<-this_recipe %>% update_role(vars_to_append_roles[roles_to_be_appended==this_role],
                                             new_role=this_role)
  }
  return(this_recipe)
}

bulk_add_role<-function(this_recipe,vars_to_append_roles=var_controls$varname[!is.na(var_controls$role2)],roles_to_be_appended=var_controls$role2[!is.na(var_controls$role2)]){
  unique_roles<-unique(roles_to_be_appended)
  for (i in 1:length(unique_roles)){
    this_role<-unique_roles[i]
    this_recipe<-this_recipe %>% add_role(vars_to_append_roles[roles_to_be_appended==this_role],
                                          new_role=this_role)
  }
  return(this_recipe)
}



add_steps_media<-function(this_recipe,var_specific_controls=var_controls,media_controls=transform_controls){
  #two cases -- specific variable settings in var_specific_controls or not.
  
  #find var specific controls here.  
  
  var_specific_controls <-var_specific_controls %>% select(varname,retention,asymptote,saturation_speed) %>% 
    mutate(across(c(retention,asymptote,saturation_speed),as.numeric) )%>% 
    filter(!is.na(retention) | !is.na(asymptote) |!is.na(saturation_speed)) 
  
  n_specific_controls<-nrow(var_specific_controls)
  
  vars_to_skip<-vector('character') #this will hold vars with specific settings so they are skipped when doing group -based transforms
  
  these_groups<-as.character(groups(this_recipe$template))
  
  if(n_specific_controls>0){
    for(specific in 1:n_specific_controls){
      this_row<-var_specific_controls[specific,c('varname','asymptote','saturation_speed','retention')]
      this_var<-var_specific_controls$varname[specific]
      asymptote_id=paste0(this_var,'_asymptote')
      saturation_speed_id=paste0(this_var,'_saturation_speed')
      retention_id=paste0(this_var,'_retention')
      if(is.na(var_specific_controls$asymptote[specific])){this_asymptote<-tune(asymptote_id)}else{this_asymptote<-var_specific_controls$asymptote[specific]}
      if(is.na(var_specific_controls$saturation_speed[specific])){this_saturation_speed<-tune(saturation_speed_id)}else{this_saturation_speed<-var_specific_controls$saturation_speed[specific]}
      if(is.na(var_specific_controls$retention[specific])){this_retention<-tune(retention_id)}else{this_retention<-var_specific_controls$retention[specific]}
      
      this_recipe<-this_recipe %>% step_adstock(!!this_var,retention=this_retention,groups = these_groups) %>%
        step_saturation(!!this_var,asymptote=this_asymptote,saturation_speed=this_saturation_speed)
    }
    #don't need to add steps for any of those variables, so make list to keep out in the settings by role
    vars_to_skip<-var_specific_controls$varname
  }
  
  if(nrow(media_controls)>0){
    for(group_no in 1:nrow(media_controls)){
      this_role<-media_controls$role[group_no]
      asymptote_id<-paste0(this_role,'_asymptote')
      saturation_speed_id<-paste0(this_role,'_saturation_speed')
      retention_id<-paste0(this_role,'_retention')
      
      if(media_controls$retention_high[group_no]==media_controls$retention_low[group_no]){
        this_retention<-media_controls$retention_high[group_no]}else{this_retention<-tune(retention_id)}
      if(media_controls$asymptote_high[group_no]==media_controls$asymptote_low[group_no]){
        this_asymptote<-media_controls$asymptote_high[group_no]}else{this_asymptote<-tune(asymptote_id)}
      if(media_controls$saturation_speed_high[group_no]==media_controls$saturation_speed_low[group_no]){
        this_saturation_speed<-media_controls$saturation_speed_high[group_no]}else{this_saturation_speed<-tune(saturation_speed_id)}
      
      if(length(vars_to_skip)>0){  
       vars_with_role<-summary(this_recipe) %>% group_by(variable) %>% 
          summarise(roles=paste(role,collapse=',')) %>% 
         filter(grepl("predictor",roles,fixed=T),grepl(this_role,roles,fixed=T),
                !(variable %in% !!vars_to_skip) )%>% select(variable) %>% distinct() %>% unlist()
        if(length(vars_with_role)>0){
          this_recipe<-this_recipe %>% step_adstock(all_of(vars_with_role),
                                                    retention = this_retention,groups = these_groups) %>% 
            step_saturation(all_of(vars_with_role),
                      asymptote=this_asymptote,
                      saturation_speed=this_saturation_speed)
          }
       }
      else{
        this_recipe<-this_recipe %>% step_adstock(has_role(!!this_role),
                                                  retention = this_retention,groups=these_groups) %>% 
          step_saturation(has_role(!!this_role),
                          asymptote=this_asymptote,
                          saturation_speed=this_saturation_speed)
      }
    }
  }
  return(this_recipe)
}
# recipe0<-recipe(head(data_set,n=1) ) 
# 
# recipe1<-recipe0 %>% bulk_update_role() %>% bulk_add_role() 
# add_steps_media(recipe1)

#use the lookup to rename some variables
rename_columns_per_controls<-function(working_df,variable_controls=var_controls){
  new_names<-variable_controls$varname[match(names(working_df),variable_controls$start_name)]
  #catch for names not in the controls table
  na_idx<-which(is.na(new_names))
  new_names[na_idx]<-names(working_df)[na_idx]
  #rename
  names(working_df)<-new_names
  return(working_df)
}



make_bound_statements<-function(variable_controls=var_controls){
  #for sign constraints
  #if sign is not empty and indicates a positive or negaive in a variety of ways, we bound at upper
  #if sign is not empty and is not one o those ways, we assume sign is a stan-ready string of boundaries
  
  bounded_coefs<-variable_controls %>% filter(!is.na(sign)) %>% rowwise() %>% mutate(
    bound_statement =list(ifelse(sign %in% c('>=0','>0','>','+'),'lower=0',
                                 ifelse(sign %in% c('<=0','<0','<','-'),'upper=0',sign))),
    name_for_list=ifelse(tolower(role)=='outcome',varname,paste0('b_',varname))
  )
  list_of_bounds<-bounded_coefs$bound_statement
  names(list_of_bounds)<-bounded_coefs$name_for_list
  return(list_of_bounds)
}


#construct priors from control sheet -- here we can use names and not indices!

make_prior_statements<-function(variable_controls=var_controls){
  prior_frame<-variable_controls %>% filter(!is.na(prior)) %>% select(varname,sign,prior,prior_sd) %>% mutate(prior_sd=ifelse(is.na(prior_sd),prior*5,prior_sd))
  prior_frame<-prior_frame %>% rowwise() %>% mutate(coef_name=paste0("b_",varname),
                                                    prior_def=list(as.formula(paste0("b_",varname,"~ normal(",prior,",",prior_sd,")"))))
  
  priors_for_ulam<-prior_frame$prior_def
  names(priors_for_ulam)<-prior_frame$coef_name
  return(priors_for_ulam)
  }

#extract Value from workflow_controls
get_control<-function(this_control,control=workflow_controls){
  if(length(this_control)==0){stop("get_control requires this_control to be non-null")}
  control %>% filter(R_name==!!this_control) %>% select(Value) %>% unlist()
}

get_predictors_vector<-function(base_recipe=recipe3){
  groups_and_time<-unlist(summary(base_recipe) %>% filter(role %in% c('group','time')) %>% select(variable))
  
  predictors<-unlist(summary(base_recipe) %>% 
                       filter(role=='predictor') %>% select(variable))
  
  return(predictors[!(predictors %in% groups_and_time)])
}
#create formula for multilevel model
create_formula<-function(base_recipe=recipe3,control=workflow_controls){
  #we will remove grouping vars (used for random effects) and time series stuff (for now)
  
  final_predictors<-get_predictors_vector(base_recipe=base_recipe)
  outcome<-get_control(this_control='Y',control=control)  
  fft_terms<-get_control("fft_terms",control=control) %>% as.numeric()
  
  fft_formula<-""
  
  if(fft_terms>0){
    for(i in 1:fft_terms){
      if (i==1){fft_formula= '+ sin1 + cos1'}
      else{
        fft_formula=paste(fft_formula,paste(c('cos','sin'),i,sep='',collapse='+'),sep='+')
      }}}
  
  list_rand_ints<-gsub(" ","",get_control("list_rand_ints",control=control),fixed=T) %>% strsplit(',',fixed=T) %>% unlist()
  list_rand_ints<-list_rand_ints[!is.na(list_rand_ints)]
  
  rand_int_formula<-""
  if(length(list_rand_ints)>0){
    for (int in list_rand_ints){
      if(length(rand_int_formula)==0){rand_int_formula<-paste0('+ (1|',int,')')}
      else{rand_int_formula=paste(rand_int_formula,paste0('(1|',int,')'),sep='+')}
    }
  }
  
  built_formula<-paste0(outcome,' ~ ',paste(final_predictors,collapse='+'),
                        fft_formula,rand_int_formula)
  return(built_formula)
}



#inputs are var_controls worksheet -- let's call it prior_controls, and the formula (built_formula)

create_ulam_list<-function(prior_controls=var_controls, model_formula=built_formula,
                           rand_int_prior_mean=65,
                           main_error_term_prior='half_cauchy(0,100)',
                           grand_intercept_prior= 'normal(50,50)',
                           random_int_stdev_prior ='half_cauchy(0,10)',
                           unspecified_priors='normal(0,10)'
){
  
  #translate user defined priors
  user_defined_priors<-make_prior_statements(variable_controls = prior_controls)
  
  #create list of remaining priors -- 'uninformative' priors to be used here which is just normal(0,10)
  all_terms<-attr(terms(as.formula(model_formula)),'term.labels') 
  all_coefs<-paste0("b_",attr(terms(as.formula(model_formula)),'term.labels') )
  
  #random ints, i.e. 1|<var> we need to make the prior on <var>_int[<var>_id] and the deterministic formula will be <var>_int[<var>_id]
  random_ints0<- all_coefs[grep("1 | ",all_coefs,fixed=T)]
  random_ints<-gsub("^.*1 \\| ","",random_ints0)
  
  if(length(random_ints0)>0){
    priors_for_random_ints<-lapply(paste0(random_ints,'_int[',random_ints,'_id] ~ normal(',rand_int_prior_mean,',int_sigma)'),as.formula)
    names(priors_for_random_ints)<-random_ints
  }else{
    priors_for_random_ints=list()
  }
  #TODO: random slopes?
  
  #get diffuse priors on fixed effects not specified by user
  fixed_coefs<-all_coefs[!(all_coefs %in% random_ints0)]
  fixed_terms<-all_terms[!(all_coefs %in% random_ints0)]
  
  
  fixed_coefs_needing_priors<-fixed_coefs[!(fixed_coefs %in% names(user_defined_priors))]
  
  priors_for_fixed<-lapply(paste0(fixed_coefs_needing_priors,"~",unspecified_priors),as.formula)
  names(priors_for_fixed)<-fixed_coefs_needing_priors
  
  main_model_formula<-as.formula(paste0(as.character(as.formula(model_formula))[2],'~ normal(big_model,big_sigma)'))
  #prior on big_sigma
  prior_on_big_sigma<-as.formula(paste('big_sigma ~ ',main_error_term_prior))
  #prior on grand intercept
  prior_on_a0<-as.formula(paste('a0 ~ ',grand_intercept_prior))
  
  #prior_on_store_ints_spread
  prior_on_store_int_sigma<-as.formula(paste('int_sigma ~',random_int_stdev_prior))
  
  #more than around 15 terms, need to split it
  number_of_terms<-length(all_terms)
  
  
  rand_ints_formula_for_ulam<-  ifelse(length(random_ints0)>0,
                                       paste(paste0(random_ints,"_int[",random_ints,"_id]"),collapse='+'),
                                       '')
  
  number_of_fixed<-number_of_terms-length(rand_ints_formula_for_ulam[rand_ints_formula_for_ulam!=''])
  
  big_model_list<-vector('list')
  included_terms=0
  start_term=1
  last_end_term=15
  iter_of_big_model=1
  while(start_term<=number_of_fixed){
    
    this_end_term=min(last_end_term,number_of_fixed)
    if(iter_of_big_model==1){
      big_model_list[[iter_of_big_model]]<- paste(paste0('big_model_',iter_of_big_model,' <-'),paste(fixed_coefs[start_term:this_end_term],
                                                                                  fixed_terms[start_term:this_end_term],sep='*',collapse='+'))
    }
    else{
      big_model_list[[iter_of_big_model]]<-paste(paste0('big_model_',iter_of_big_model,' <- big_model_',iter_of_big_model-1,' + '),paste(fixed_coefs[start_term:this_end_term],
                                                                                               fixed_terms[start_term:this_end_term],sep='*',collapse='+'))
    }
    start_term=this_end_term+1
    last_end_term=this_end_term+15
    iter_of_big_model=iter_of_big_model+1
  }
  if(rand_ints_formula_for_ulam!='')
  {big_model_list[[iter_of_big_model]]<-paste(paste0('big_model <- big_model_',iter_of_big_model-1),'a0',rand_ints_formula_for_ulam,sep='+')}else
  {big_model_list[[iter_of_big_model]]<-paste(paste0('big_model <- big_model_',iter_of_big_model-1),'a0',sep='+')}
  
  big_model_list_parsed<-sapply(big_model_list,function(x) parse(text=x))
  formula_list<-c(main_model_formula,rev(big_model_list_parsed),priors_for_random_ints,priors_for_fixed,user_defined_priors,prior_on_a0,prior_on_big_sigma,prior_on_store_int_sigma)
  
  class(formula_list)<-'list'
  return(formula_list)
}

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

predict.ulam<-function(ulamobj,new_data,n=1000,reduce=T,conf=.95){
  link_output<-link(ulamobj,new_data=new_data,n=n)$big_model
  if(reduce){
    preds<-colMeans(link_output)
    low_conf<-apply(link_output,2,quantile,1-conf)
    high_conf<-apply(link_output,2,quantile,conf)
    pred_output<-cbind(preds,low_conf,high_conf)
    colnames(pred_output)<-c('pred',paste0('pred_lower_',conf),paste0('pred_upper_',conf))
  }
  return(pred_output)
}



create_recipe<-function(data_to_use=data1,vc=var_controls,adding_trend=TRUE){
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
    add_role(c(sin1,sin2,sin3,sin4,sin5,cos1,cos2,cos3,cos4,cos5),new_role='predictor')  
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
