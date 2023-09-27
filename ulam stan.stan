data{
    vector[208] sales;
    vector[208] sin2;
    vector[208] cos2;
    vector[208] cos1;
    vector[208] sin1;
    vector[208] ProgVideo1;
    vector[208] TV2;
    vector[208] TV1;
    int price[208];
}
parameters{
    real b_sin1;
    real b_cos1;
    real b_cos2;
    real b_sin2;
    real<upper=0> b_price;
    real<lower=0> b_TV1;
    real<lower=0> b_TV2;
    real<lower=0> b_ProgVideo1;
    real a0;
    real<lower=0> big_sigma;
    real<lower=0> int_sigma;
}
model{
    vector [208]  big_model;
    vector[208] big_model_1;
    int_sigma ~ cauchy( 0 , 10 );
    big_sigma ~ cauchy( 0 , 100 );
    a0 ~ normal( 50 , 50 );
    b_ProgVideo1 ~ normal( 1 , 10 );
    b_TV2 ~ normal( 1 , 10 );
    b_TV1 ~ normal( 1 , 10 );
    b_price ~ normal( -1 , 10 );
    b_sin2 ~ normal( 0 , 10 );
    b_cos2 ~ normal( 0 , 10 );
    b_cos1 ~ normal( 0 , 10 );
    b_sin1 ~ normal( 0 , 10 );
    for ( i in 1:208 ) {
        big_model_1[i] = b_price * price[i] + b_TV1 * TV1[i] + b_TV2 * TV2[i] + b_ProgVideo1 * ProgVideo1[i] + b_sin1 * sin1[i] + b_cos1 * cos1[i] + b_cos2 * cos2[i] + b_sin2 * sin2[i];
    }
    for ( i in 1:208 ) {
        big_model[i] = big_model_1[i] + a0;
    }
    sales ~ normal( big_model , big_sigma );
}


