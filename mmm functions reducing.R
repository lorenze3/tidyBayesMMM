

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

#' produce a list of  formula for testing combinations of random effects
#' 
#' @param vc defaults to best_seas vc; a tibble with a single seasonality specification typically determined
#' by the seasonality_search process
#' @param seasonality_formula defaults to best_seas_formula; a string containing the formula for
#' all fixed effects in the model plus the seasonaity terms (which may include random slopes).
#' Typically will be produced by tuning a workflowset to identify the best seasonality specification.
#' @export
#' @return a list of strings suitable to be coerced to formula for lmer
#' and the specifications for random effects.  In the case where the control
#' search_randoms = 'FALSE' , the string in seasonality_formula is returned in a list of 1
#' 
make_list_of_rands_formula<-function(seasonality_formula=best_seas_formula,
                                     vc=best_seas_vc){
  
  if(get_control("search_randoms",vc)=="FALSE"){
    #in this case we expect that the seasonality_formula will already contain
    # any random effects specified (unless some process has problematically tampered
    #with the search_randoms control)
    
    random_terms=gsub(',','+',paste(get_control("list_rand_ints",vc),
                                    get_control('list_rand_slopes',vc),sep=','),fixed=T)
    
    list_of_formulae_rands<-list(seasonality_formula)
    cat("Nota Bene: make_list_of_rands_formula has been called 
        when search_randoms is FALSE\n Output is a single formula string matchting seasonality_formula.")
  }
  
  else{
    list_of_formulae_rands<-vector('list')
    #get all possible terms
    terms_to_add<-paste(get_control("list_rand_ints",best_vc),
                        get_control('list_rand_slopes',best_vc),sep=',') %>% strsplit(split=',') %>% unlist()
    #create all_combinations using collapsed combn output
    pastey<-function(x){paste(unlist(x),collapse=" + ")}
    all_combinations<-unlist(lapply(1:length(terms_to_add),function(x) combn(terms_to_add,x,FUN=pastey,simplify = F)))
    
    for(i in 1:length(all_combinations)){
      list_of_formulae_rands[[i]]<-paste0(seasonality_formula,'+',all_combinations[[i]])
    }
    list_of_formulae_rands<-append(list_of_formulae_rands,seasonality_formula)
  }
  return(list_of_formulae_rands)
}

#' creates a tidymodels workflow from a a recipe and a model
#' @param this_formula defaults to built_formula; a string suitable to be coerced to a formula for lmer
#' @param recipe_to_use defaults to recipe3; a recipe with roles for predictor, outcome, time_id, and groups
#' @export
#' @example 
#' @importFrom workflows workflow
assemble_workflow<-function(this_formula=built_formula,recipe_to_use=recipe3){
  if(grepl("\\|",this_formula)){tune_spec<-linear_reg(engine='lmer')} else{
    tune_spec<-linear_reg(engine='lm')}
  #  hardhat::extract_parameter_set_dials(reg_wf)|> finalize(data1) 
  #need to build a formula with random effects specs for stan_glmer
  
  return(workflow() |>  add_recipe(recipe_to_use) |> 
           add_model(tune_spec,formula=as.formula(this_formula)))
}

#' create a list of model formula (per the rules for lmer()) with different
#' seasonality specifications as listed in the configuration tibble.
#' 
#' @param vc defaults to workflow_controls; a tibble with R_name and Value columns
#' where R_name contains the control name and Value is it's value
#' @param recipe_to_use defaults to recipe3; a recipes::recipe with pre-modeling transformations and
#' variable roles; the outcome, predictor, group, and time_id roles are used in determining
#' the formula.
#' @return a list with names formulae and configs, formulae is a list of strings suitable
#'  to be coerced to a formula and passed to lmer.  configs is a list of tibbles with information like vc,
#'   but reset to match a single seasonality specification.  If search_seasonality is set to FALSE in
#'   the config table, the two lists have a single formula containing the specified seasonality
#'   and no random terms.  A note is printed to the console in this case.
#' @example 
#' @export
#' @importFrom 
make_list_of_fft_formulae<-function(vc=workflow_controls,recipe_to_use=recipe3){
  #use search_randoms control value to determine if formula created here will include
  #random effects not in seaonslity. If search_randoms if TRUE, we do want to ignore
  #random effects specification (for later searching)
  are_we_ignoring_rands = get_control('search_randoms')=="TRUE"
  if(get_control("search_seasonality")=="FALSE"){
    list_of_configs=list(vc)
    formulae=list(create_formula(recipe_to_use,vc,ignore_rands=are_we_ignoring_rands))
    cat("Nota Bene: make_list_off_formulae is called but search_seaonality is FALSE")
  }else{
    fft_interact_options<-get_control("interaction_fft") |> strsplit(split=',',fixed=T) |> unlist()
    if(!("" %in% fft_interact_options)){fft_interact_options=c("",fft_interact_options)}
    
    fft_count_options<-get_control("fft_terms") |> unlist() |> as.numeric()
    #vc<-workflow_controls
    list_of_configs<-vector('list')
    idx=0
    for (this_fterms in 0:fft_count_options){
      for (iterm in 1:length(fft_interact_options)){
        idx=idx+1
        this_iterm=fft_interact_options[iterm]
        
        this_vc=vc |> mutate(Value=ifelse(R_name=='interaction_fft',this_iterm,Value),
                             Value=ifelse(R_name=='fft_terms',this_fterms,Value),
                             Value=ifelse(R_name=='search_seasonality',"FALSE",Value))
        list_of_configs[[idx]]=this_vc
      }
    }
  }
  formulae<-lapply(list_of_configs,function(x) create_formula(recipe_to_use,x,ignore_rands = are_we_ignoring_rands))
  return(list(formulae=formulae,configs=list_of_configs))
}
