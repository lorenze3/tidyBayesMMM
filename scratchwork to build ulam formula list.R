

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

  number_of_fixed<-number_of_terms-length(rand_ints_formula_for_ulam)
  
  big_model_list<-vector('list')
  included_terms=0
  start_term=1
  last_end_term=15
  iter_of_big_model=1
  while(start_term<number_of_terms){
    
    this_end_term=min(last_end_term,number_of_fixed)
    if(iter_of_big_model==1){
    big_model_list[[iter_of_big_model]]<- parse(text=paste('big_model <-',paste(fixed_coefs[start_term:this_end_term],
                                                                   fixed_terms[start_term:this_end_term],sep='*',collapse='+')))
    }
    else{
      big_model_list[[iter_of_big_model]]<- parse(text=paste('big_model <- big_model + ',paste(fixed_coefs[start_term:this_end_term],
                                                                     fixed_terms[start_term:this_end_term],sep='*',collapse='+')))
    }
    start_term=this_end_term+1
    last_end_term=this_end_term+15
    iter_of_big_model=iter_of_big_model+1
  }
  big_model_list[[iter_of_big_model]]<-parse(text=paste('big_model <- big_model +',rand_ints_formula_for_ulam,'+a0'))
  
  
  formula_list<-c(main_model_formula,rev(big_model_list),priors_for_random_ints,priors_for_fixed,user_defined_priors,prior_on_a0,prior_on_big_sigma,prior_on_store_int_sigma)
  
  class(formula_list)<-'list'
  return(formula_list)
}

#light testing

# create_ulam_list()

# lotsa_vars<-paste0('x',1:60)
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
# ##TODO: test all variations of tune vs fixed -- test alpha, e.g.
# 
# 
# recipec <-recipeb %>% step_mutate(week=as.numeric(week)-19247.65) %>%# step_center(week) %>%
#   update_role(c(sin1,sin2,sin3,cos1,cos2,cos3),new_role='time') %>% 
#   add_role(c(sin1,sin2,sin3,cos1,cos2,cos3),new_role='predictor')
# recipec<-recipec %>% update_role(starts_with('x'),new_role='predictor')
# 
# formula_with_lotsa_vars<-create_formula(recipec)
# create_ulam_list(model_formula=formula_with_lotsa_vars)
