data{
    vector[208] sales;
    int store_id[208];
    vector[208] trend;
    vector[208] ProgVideo1;
    vector[208] TV2;
    vector[208] TV1;
    int price[208];
}
parameters{
    vector[2] b_TV1_interact_store;
    vector[2] b_TV2_interact_store;
    vector[2] store_int;
    real b_trend;
    real<upper=0> b_price;
    real<lower=0> b_TV1;
    real<lower=0> b_TV2;
    real<lower=0> b_ProgVideo1;
    real a0;
    real<lower=0> big_sigma;
    real<lower=0> int_sigma;
    real<lower=0> slope_sigma;
}
model{
    vector[208] big_model;
    vector[208] big_model_1;
    slope_sigma ~ cauchy( 0 , 10 );
    int_sigma ~ cauchy( 0 , 10 );
    big_sigma ~ cauchy( 0 , 100 );
    a0 ~ normal( 50 , 50 );
    b_ProgVideo1 ~ normal( 1 , 10 );
    b_TV2 ~ normal( 1 , 10 );
    b_TV1 ~ normal( 1 , 10 );
    b_price ~ normal( -1 , 10 );
    b_trend ~ normal( 0 , 10 );
    store_int ~ normal( 65 , int_sigma );
    b_TV2_interact_store ~ normal( 0 , slope_sigma );
    b_TV1_interact_store ~ normal( 0 , slope_sigma );
    for ( i in 1:208 ) {
        big_model_1[i] = b_price * price[i] + b_TV1 * TV1[i] + b_TV2 * TV2[i] + b_ProgVideo1 * ProgVideo1[i] + b_trend * trend[i];
    }
    for ( i in 1:208 ) {
        big_model[i] = big_model_1[i] + a0 + store_int[store_id[i]] + b_TV1_interact_store[store_id[i]] + b_TV2_interact_store[store_id[i]];
    }
    sales ~ normal( big_model , big_sigma );
}


