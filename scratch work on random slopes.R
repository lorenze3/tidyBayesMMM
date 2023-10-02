#' Creates a string that represents a model formula from a recipe and the workflow controls data
#' 
#' @param base_recipe defaults to recipe3; is the recipe containing variables to build a formula for
#' @param control defaults to workflow_controls, should be a tibble with columns R_name and Value, which must have rows with R_name =='Y','list_rand_ints' and 'fft_terms'
#' @return
#' a string that reads like an lmer formula
#' @export
#' 
#' @example
create_formula<-function(base_recipe=recipe3,control=workflow_controls){
  #we will remove grouping vars (used for random effects) and time series stuff (for now)
  
  final_predictors<-get_predictors_vector(base_recipe=base_recipe)
  outcome<-get_control(this_control='Y',control=control)  
  fft_terms<-get_control("fft_terms",control=control) %>% as.numeric()
  fft_terms<-ifelse(is.na(fft_terms),0,fft_terms)
  
  fft_formula<-""
  
  if(fft_terms>0){
    for(i in 1:fft_terms){
      if (i==1){fft_formula= 'sin1 + cos1'}
      else{
        fft_formula=paste(fft_formula,paste(c('cos','sin'),i,sep='',collapse='+'),sep='+')
      }}}
  
  list_rand_ints<-gsub(" ","",get_control("list_rand_ints",control=control),fixed=T) %>% strsplit(',',fixed=T) %>% unlist()
  list_rand_ints<-list_rand_ints[!is.na(list_rand_ints)]
  
  rand_int_formula<-""
  if(length(list_rand_ints)>0){
    rand_int_formula<-paste(list_rand_ints,collapse =' + ')
  }
  
  list_rand_slopes<-gsub(" ","",get_control("list_rand_slopes",control=control),fixed=T) %>% strsplit(',',fixed=T) %>% unlist()
  list_rand_slopes<-list_rand_slopes[!is.na(list_rand_slopes)]
  
  rand_slope_formula<-""
  if(length(list_rand_slopes)>0){
    rand_slope_formula<-paste(list_rand_slopes,collapse =' + ')
  }
  
  built_formula<-paste(paste0(outcome,' ~ ',
                               paste(final_predictors,collapse=' + ')),
                        fft_formula,rand_int_formula,rand_slope_formula,sep=' + ')
  
  #remove trailing +, left by the paste in built_formula if one of fft_formula, 
  #rand_int_formula, or rand_slope_formula are empty strings
  total_times<-(length(list_rand_slopes)==0)+(length(list_rand_ints)==0)+(fft_terms==0)
  
  if (total_times>0){
  for(i in 1:total_times){
      built_formula<-gsub("(\\+\\s* $)","",built_formula)
    }
    built_formula<-trimws(built_formula)
  }
  
  return(built_formula)
}
workflow_controls<-readxl::read_xlsx(control_file,"workflow") %>% select(-desc)
(built_formula<-create_formula())


create_ulam_list<-function(prior_controls=var_controls, model_formula=built_formula,
                           rand_int_prior_mean=65,
                           main_error_term_prior='half_cauchy(0,100)',
                           grand_intercept_prior= 'normal(50,50)',
                           random_int_stdev_prior ='half_cauchy(0,10)',
                           random_slope_stdev_prior='half_cauchy(0,10)',
                           unspecified_priors='normal(0,10)'
){
  
  #translate user defined priors
  user_defined_priors<-make_prior_statements(variable_controls = prior_controls)
  
  #create list of remaining priors -- 'uninformative' priors to be used here which is just normal(0,10)
  all_terms<-attr(terms(as.formula(model_formula)),'term.labels') 
  all_coefs<-paste0("b_",attr(terms(as.formula(model_formula)),'term.labels') )
  
  #random ints, i.e. 1|<var> we need to make the prior on <var>_int[<var>_id] and the deterministic formula will be <var>_int[<var>_id]
  random_ints0<- all_coefs[grep("b_1 | ",all_coefs,fixed=T)]
  random_ints<-gsub("^.*b_1 \\| ","",random_ints0)
  
  if(length(random_ints0)>0){
    priors_for_random_ints<-lapply(paste0(random_ints,'_int[',random_ints,'_id] ~ normal(',rand_int_prior_mean,',int_sigma)'),as.formula)
    names(priors_for_random_ints)<-random_ints
  }else{
    priors_for_random_ints=list()
  }
  #random slopes, i.e. b_var|group_var
  random_slopes0<-all_coefs[grepl("\\|",all_coefs) & !grepl("b_1 \\|",all_coefs)]
   #split into grouping vars and new 'coeff' with coeff name +_interact_<group name>
  random_slope_group_vars<-trimws(sub('.+\\|(.+)', '\\1', random_slopes0))
  random_slope_idvars<- paste0(random_slope_groupvars,"_id")
  
  random_slope_real_vars<-paste0(trimws(sub('(.+)\\|.+', '\\1', random_slopes0)),"_interact_",random_slope_group_vars)
  
  if(length(random_slopes0)>0){
    priors_for_random_slopes<-lapply(
      paste0(random_slope_real_vars,'[',random_slope_idvars,'] ~ normal(',
             0,',slope_sigma)'),as.formula)
    names(priors_for_random_slopes)<-random_slope_real_vars
  }else{
    priors_for_random_slopes=list()
  }
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
  #prior on random slope spread
  prior_on_slope_sigma<-as.formula(paste('slope_sigma ~',random_slope_stdev_prior))
  #more than around 15 terms to sum for the expected value in the model, need to split it rethinking::ulam builds an incorrect stan file.
  
  
  number_of_terms<-length(all_terms)
  
  rand_ints_formula_for_ulam<-  ifelse(length(random_ints0)>0,
                                       paste(paste0(random_ints,"_int[",random_ints,"_id]"),collapse='+'),
                                       '')
  rand_slopes_formula_for_ulam<-ifelse(length(random_slopes0)>0,
                                       paste(paste0(random_slope_real_vars,'[',random_slope_idvars,']'),collapse=' + '),
                                       '')
  
  #TODO::update to use counts of + in the rand formulas above
  #TODO:: update the big model summation to include random slopes
  number_of_fixed<-number_of_terms-
    stringr::str_count(rand_slopes_formula_for_ulam,"\\[") - 
    stringr::str_count(rand_ints_formula_for_ulam,"\\[") 
  
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
  if(rand_ints_formula_for_ulam!='' & rand_slopes_formula_for_ulam != '')
  {big_model_list[[iter_of_big_model]]<-paste(paste0('big_model <- big_model_',
                                                     iter_of_big_model-1),'a0',
                                              rand_ints_formula_for_ulam,
                                              rand_slopes_formula_for_ulam,sep='+')}
  else if(rand_ints_formula_for_ulam!=''){
    big_model_list[[iter_of_big_model]]<-paste(paste0('big_model <- big_model_',
                                                      iter_of_big_model-1),'a0',
                                               rand_ints_formula_for_ulam,sep='+')
  }else if(rand_slopes_formula_for_ulam!=''){
    big_model_list[[iter_of_big_model]]<-paste(paste0('big_model <- big_model_',
                                                      iter_of_big_model-1),'a0',
                                               rand_slopes_formula_for_ulam,sep='+')
  }
    else
    {big_model_list[[iter_of_big_model]]<-paste(paste0('big_model <- big_model_',
                                                     iter_of_big_model-1),'a0',sep='+')}
  
  #to have expressions in the final list, need to prase the strings in big_modl
  big_model_list_parsed<-sapply(big_model_list,function(x) parse(text=x))
  
  formula_list<-c(main_model_formula,rev(big_model_list_parsed),priors_for_random_ints,priors_for_fixed,user_defined_priors,prior_on_a0,prior_on_big_sigma,prior_on_store_int_sigma)
  
  class(formula_list)<-'list'
  return(formula_list)
}

workflow_controls<-readxl::read_xlsx(control_file,"workflow") %>% select(-desc)
(built_formula<-create_formula())
(create_ulam_list())
