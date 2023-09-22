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