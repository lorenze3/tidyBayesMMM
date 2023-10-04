setwd('C:\\Users\\loren\\Documents\\R\\tidymmm')
librarian::shelf(tidymodels,tune,recipes,multilevelmod,tidyverse,arrow,mostlytidyMMM,workflowsets,rethinking,rstan)

control_file<-system.file('no_tuning_example.xlsx',package='mostlytidyMMM')


#get each relevant table of the control file:
var_controls<-readxl::read_xlsx('no_tuning_example.xlsx','variables')
transform_controls<-readxl::read_xlsx('no_tuning_example.xlsx','role controls')
workflow_controls<-readxl::read_xlsx('no_tuning_example.xlsx',"workflow") |> select(-desc)

#pull data, rename columns per control sheet, add fourier terms for seasonality, 
#group the data by variables with role=group and arrange by groups and time_id
data1<-read.csv('example2.csv')|>rename_columns_per_controls(variable_controls=var_controls)|>
  rename_columns_per_controls()|> mutate(week=as.Date(week,"%m/%d/%Y"))|>
  add_fourier_vars(vc=var_controls) |>  add_groups_and_sort(vc=var_controls) 

#create a recipe for pre-processing based on the data and the 3 config tables
(no_tuning_recipe<-create_recipe(data1,vc=var_controls,mc=transform_controls,wc=workflow_controls))

#and the lmer() style formula, based on the recipe
(formula_in_a_string<-create_formula(base_recipe=no_tuning_recipe,control=workflow_controls))

#create a rethinking::ulam appropriate flist from the formula and the config tables (priors from config, e.g.)
(expressions_for_ulam<-create_ulam_list(prior_controls=var_controls,model_formula=formula_in_a_string,
                 grand_intercept_prior='normal(45,25)') )

#constraints for ulam
(bounds_for_ulam<-make_bound_statements(variable_controls=var_controls))

#bake data and call ulam
model_data<-no_tuning_recipe %>% prep() %>% bake(data1)

fitted_model_obj<-ulam(expressions_for_ulam, 
                     model_data,
                     constraints=bounds_for_ulam,
                     chains=2,
                     iter=100,
                     cores=2,
                     declare_all_data=F,
                     messages=F
                   )

#a predict method for ulam objects is included in the mostlytidyMMM package
model_data$pred<-predict(fitted_model_obj,model_data)[,1]

#Some basic charts of fit, as examples:
this_rsq<-rsq(model_data|>ungroup(),truth=sales,estimate=pred)['.estimate'] %>% unlist()
this_mape<-mape(model_data|>ungroup(),truth=sales,estimate=pred)['.estimate'] %>% unlist()
ggplot(model_data ,aes(x=sales,y=pred,color=store_id))+
  geom_point()+ geom_abline(slope=1,intercept=0)+ggthemes::theme_tufte()+
  ggtitle("Predicted vs Actual",subtitle=paste0('Rsq is ',round(this_rsq,2)))

model_preds_long<-model_data %>% pivot_longer(c(pred,sales))

ggplot(model_preds_long,aes(x=week,y=value,color=name))+geom_line()+
  ggtitle("Sales and Predicted Sales by Week",subtitle=paste('MAPE is',round(this_mape)))

#a function for decomposition is included as well:
decomps<-get_decomps_irregardless(model_data %>% ungroup(),recipe_to_use=no_tuning_recipe,
                         model_obj=fitted_model_obj,
                         )

#roll those up to total by week and plot them:
decomps_natl<-decomps %>% select(week,all_of(!!get_predictors_vector(no_tuning_recipe))) %>% group_by(week) %>% summarise(across(where(is.numeric),sum))

decomps_natl<-decomps_natl %>% pivot_longer(cols=c(-week))

ggplot(data=decomps_natl,aes(x=week,y=value,fill=name)) + geom_area()+ggthemes::theme_tufte()+
  ggtitle("Decomposition By Week")+
  theme(legend.position = 'bottom')

          