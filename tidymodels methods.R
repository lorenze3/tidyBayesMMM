#code for tidymodels framework

step_saturation <- function(
    recipe, 
    ..., 
    role = NA, 
    trained = FALSE, 
    options = list( names = TRUE),
    skip = FALSE,
    asymptote=100,
    saturation_speed=.05,
    hills=NULL,
    id = rand_id("saturation")
) {
  
  add_step(
    recipe, 
    step_saturation_new(
      terms = enquos(...), 
      trained = trained,
      role = role, 
      options = options,
      skip = skip,
      id = id,
      asymptote=asymptote,
      saturation_speed=saturation_speed,
      hills=hills
    )
  )
}

step_saturation_new <- 
  function(terms, role, trained, asymptote,saturation_speed,hills, options, skip, id) {
    step(
      subclass = "saturation", 
      terms = terms,
      role = role,
      trained = trained,
      asymptote=asymptote,
      saturation_speed=saturation_speed,
      hills=hills,
      options = options,
      skip = skip,
      id = id
    )
  }


#worker bee function
get_train_saturation<-function(x,asymptote,saturation_speed){
  stopifnot(length(asymptote) == 1)
  stopifnot(length(saturation_speed) == 1)
  x_scurve<-asymptote*(1-exp(-saturation_speed*x))
  
  return(x_scurve)
}

prep.step_saturation <- function(x, training, info = NULL, ...) {
  
  #this will select the appropriate columns from the training set
  col_names <- recipes_eval_select(x$terms, training, info) 
  
  ## We'll use the names later so make sure they are available
  if (x$options$names == FALSE) {
    rlang::abort("`names` should be set to TRUE")
  }
  
  #transforms computed here
  step_saturation_new(terms=x$terms,
                      trained=TRUE,
                      role=x$role,
                      options=x$options,
                      skip=x$skip,
                      id=x$id,
                      asymptote=x$asymptote,
                      saturation_speed=x$saturation_speed,
                      hills=col_names
  )
  
}


bake.step_saturation<-function(object,new_data,...){
  vars<-names(object$hills)
  groupings<-as.character(groups(new_data))
  
  new_data[,vars]<-new_data[,object$hills] %>% reframe(across(everything(),function(x){get_train_saturation(x,
                                                                                                            object$asymptote,object$saturation_speed)})) 
  if(length(groupings)>0) {new_data<-as_tibble(new_data) %>% group_by(across(all_of(groupings)))}
  else{new_data<-as_tibble(new_data)}
  return(new_data)
  
}



print.step_saturation <-
  function(x, width = max(20, options()$width - 35), ...) {
    
    print_step(
      # Names before prep (could be selectors)
      untr_obj = x$terms,
      # Names after prep:
      tr_obj = names(x$hills),
      # Has it been prepped? 
      trained = x$trained,
      # An estimate of how many characters to print on a line: 
      width = width,
      title = paste("Saturation (asymptote=",x$asymptote,"saturation_speed=",x$saturation_speed,"Transformation on")
    )
    invisible(x)
  }
tunable.step_saturation <- function (x, ...) {
  tibble::tibble(
    name = c("asymptote","saturation_speed"),
    call_info = list(list( fun = "asymptote"),list(fun='saturation_speed')),
    source = c("recipe","recipe"),
    component = c("step_saturation","step_saturation"),
    component_id = x$id
  )
}
asymptote<-function(range=c(0,300)){new_quant_param(type='double',range=range,inclusive=c(FALSE,TRUE),
                                                    label=c(asymptote='saturation asymptote'),finalize = NULL)}
saturation_speed<-function(range=c(.001,.009)){new_quant_param(type='double',range=range,inclusive=c(FALSE,FALSE),
                                                               label=c(saturation_speed='saturation speed'),finalize = NULL)}
#test it:
# mktdata<-rbind(tibble(prod='brand',store='store1',sales=c(100,100,100,100,100),tv=c(10,100,0,0,100),search=c(0,10,20,50,50)) ,
#                tibble(prod='brand',store='store2',sales=c(10,10,10,10,10),tv=c(0,0,0,0,0),search=c(0,2,2,0,0) ) ) %>% group_by(prod,store) 
# 
# mktdata2<-tibble(prod='brand',store='all',sales=100,tv=1000,search=1000) %>% group_by(prod,store)
# 
# rec_obj <-
#  recipe(sales ~ ., data = mktdata) %>%
#  step_saturation(c(tv,search),asymptote=200,saturation_speed=.003 ) %>%
#  prep(training = mktdata)
# 
# rec_obj2<-  recipe(sales ~ ., data = mktdata) %>%
#   step_saturation(c(tv,search),asymptote=tune(),saturation_speed=tune() )
# 
# extract_parameter_set_dials(rec_obj2)
# 
# 
#  print(rec_obj)
# 
#  bake(rec_obj,mktdata)
#  bake(rec_obj,mktdata2)




#########################################


step_adstock <- function(
    recipe, 
    ..., 
    role = NA, 
    trained = FALSE, 
    options = list( names = TRUE), #change to be range of retention
    skip = FALSE,
    retention=.5,
    groups=c('prod','store'),
    adstocks=NULL,
    id = rand_id("adstock")
) {
  
  add_step(
    recipe, 
    step_adstock_new(
      terms = enquos(...), 
      trained = trained,
      role = role, 
      options = options,
      skip = skip,
      id = id,
      retention=retention,
      adstocks=adstocks,
      groups=groups
    )
  )
}
step_adstock_new <- 
  function(terms, role, trained, retention, adstocks, groups,options, skip, id) {
    step(
      subclass = "adstock", 
      terms = terms,
      role = role,
      trained = trained,
      adstocks=adstocks,
      retention=retention,
      groups=groups,
      options = options,
      skip = skip,
      id = id
      
    )
  }

prep.step_adstock <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info) 
  
  ## We'll use the names later so make sure they are available
  if (x$options$names == FALSE) {
    rlang::abort("`names` should be set to TRUE")
  }
  
  step_adstock_new(terms=x$terms,
                   role=x$role,
                   trained=TRUE,
                   retention=x$retention,
                   groups=x$groups,
                   adstocks=col_names,
                   options=x$options,
                   skip=x$skip,
                   id=x$id
  )
}

# as.numeric(stats::filter(activity,filter=c(retain),method='recursive'))
get_adstock<-function(x,retain){
  x<-as.numeric(stats::filter(x,retain,'recursive'))
  return(x)
}

bake.step_adstock<-function(object,new_data,...){
  vars<-names(object$adstocks)
  groupings<-object$groups#as.character(groups(new_data))

  if(length(groupings)==0){rlang::warn("No grouping vars in data for step_adstock -- assumes data is one continous time series!!!  \nIf this isn't true, group and sort the data appropriately!")}
  
  stocks<-new_data %>% group_by(across(all_of(groupings))) %>% reframe(across(all_of(vars), function(x){get_adstock(x,object$retention)} ))
  
  new_data[,vars]<-stocks[,vars]
  
  
  if(length(groupings)>0){
    stocks<-new_data %>% group_by(across(all_of(groupings))) %>% reframe(across(all_of(vars), function(x){get_adstock(x,object$retention)} ))
    new_data<-as_tibble(new_data) %>% group_by(across(all_of(groupings)))
  }
  else{
    stocks<-new_data %>% reframe(across(all_of(vars), function(x){get_adstock(x,object$retention)} ))
    
  }
  return(new_data)
  
}

print.step_adstock <-
  function(x, width = max(20, options()$width - 35), ...) {
    
    print_step(
      # Names before prep (could be selectors)
      untr_obj = x$terms,
      # Names after prep:
      tr_obj = names(x$adstocks),
      # Has it been prepped? 
      trained = x$trained,
      # An estimate of how many characters to print on a line: 
      width = width,
      title=paste("Adstock Transformation with retention",x$retention,"on"),
      case_weights=x$case_weights
    )
    invisible(x)
  }
tunable.step_adstock <- function (x, ...) {
  tibble::tibble(
    name = c("retention"),
    call_info = list(list( fun = "retention")),
    source = "recipe",
    component = "step_adstock",
    component_id = x$id
  )
}

retention<-function(range=c(0,.8)){new_quant_param(type='double',range=range,inclusive=c(TRUE,TRUE),
                                                   label=c(retention='retention'),finalize = NULL)}

##test it:
# 
# 
# mktdata<-rbind(tibble(prod='brand',store='store1',sales=c(100.,100.,100.,100.,100.),tv=c(10.,100.,0.,0.,100),search=c(0,10,20,50.,50.)) ,
#                tibble(prod='brand',store='store2',sales=c(10.,10,10,10,10),tv=c(0.,0,0,0,0),search=c(0.,2,2,0,0) ) ) %>% group_by(prod,store)
# 
# mktdata2<-tibble(prod='brand',store='all',sales=100,tv=1000,search=1000) %>% group_by(prod,store)
# rec_obj <-  recipe(sales ~ ., data = mktdata) %>% step_adstock(tv,retention=.5,groups=c('prod','store')) %>%
#   step_adstock(search,retention=.5,groups=c('prod','store')) %>%
#   prep(training = mktdata)
# 
# 
# print(rec_obj)
# 
# bake(rec_obj,mktdata)
# bake(rec_obj,mktdata2)

#check to make sure that per-variable application of the step_adstock and step-saturation do not break the grouping structure even though
#the output tibble from bake is ungrouped (irritatingly)

# rec_both_steps<-recipe(sales~.,data=mktdata) %>% step_adstock(tv,retention=.1) %>% step_saturation(tv,asymptote=1000,saturation_speed=.001) %>%
#   step_adstock(search,retention=.1)  %>% prep()
# bake(rec_both_steps,mktdata)

# #try workflow
# model_spec<-linear_reg()
# wf<-workflow() %>% add_recipe(rec_both_steps) %>% add_model(model_spec,formula = sales~ tv + search +1) 
# 
# wf<-wf %>% fit(mktdata)
# 
# extract_recipe(wf)



