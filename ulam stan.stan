data{
     vector[12956] leads;
    array[12956] int store_id;
     vector[12956] week;
     vector[12956] cos2;
     vector[12956] sin2;
     vector[12956] cos1;
     vector[12956] sin1;
     vector[12956] twitter;
     vector[12956] tiktok;
     vector[12956] taboola;
     vector[12956] tv;
     vector[12956] video;
     vector[12956] snapchat;
     vector[12956] simplifi;
     vector[12956] radio;
     vector[12956] pinterest;
     vector[12956] ooh;
    array[12956] int newspaper;
     vector[12956] mntn;
     vector[12956] marketingcloud;
     vector[12956] google;
     vector[12956] facebook;
     vector[12956] digital;
     vector[12956] cable;
     vector[12956] bing;
}
parameters{
     vector[178] store_int;
     real b_sin1;
     real b_cos1;
     real b_sin2;
     real b_cos2;
     real b_week;
     real<lower=0> b_bing;
     real<lower=0> b_cable;
     real<lower=0> b_digital;
     real<lower=0> b_facebook;
     real<lower=0> b_google;
     real<lower=0> b_marketingcloud;
     real<lower=0> b_mntn;
     real<lower=0> b_newspaper;
     real<lower=0> b_ooh;
     real<lower=0> b_pinterest;
     real<lower=0> b_radio;
     real<lower=0> b_simplifi;
     real<lower=0> b_snapchat;
     real<lower=0> b_video;
     real<lower=0> b_tv;
     real<lower=0> b_taboola;
     real<lower=0> b_tiktok;
     real<lower=0> b_twitter;
     real a0;
     real<lower=0> big_sigma;
     real<lower=0> int_sigma;
}
model{
     vector[12956] big_model;
    int_sigma ~ cauchy( 0 , 10 );
    big_sigma ~ cauchy( 0 , 100 );
    a0 ~ normal( 50 , 50 );
    b_twitter ~ normal( 1 , 10 );
    b_tiktok ~ normal( 1 , 10 );
    b_taboola ~ normal( 1 , 10 );
    b_tv ~ normal( 1 , 10 );
    b_video ~ normal( 1 , 10 );
    b_snapchat ~ normal( 1 , 10 );
    b_simplifi ~ normal( 1 , 10 );
    b_radio ~ normal( 1 , 10 );
    b_pinterest ~ normal( 1 , 10 );
    b_ooh ~ normal( 1 , 10 );
    b_newspaper ~ normal( 1 , 10 );
    b_mntn ~ normal( 1 , 10 );
    b_marketingcloud ~ normal( 1 , 10 );
    b_google ~ normal( 1 , 10 );
    b_facebook ~ normal( 1 , 10 );
    b_digital ~ normal( 1 , 10 );
    b_cable ~ normal( 1 , 10 );
    b_bing ~ normal( 1 , 10 );
    b_week ~ normal( 0 , 10 );
    b_cos2 ~ normal( 0 , 10 );
    b_sin2 ~ normal( 0 , 10 );
    b_cos1 ~ normal( 0 , 10 );
    b_sin1 ~ normal( 0 , 10 );
    store_int ~ normal( 65 , int_sigma );
    for ( i in 1:12956 ) {
        big_model[i] = b_bing * bing[i] + b_cable * cable[i] + b_digital * digital[i] + b_facebook * facebook[i] + b_google * google[i] + b_marketingcloud * marketingcloud[i] + b_mntn * mntn[i] + b_newspaper * newspaper[i] + b_ooh * ooh[i] + b_pinterest * pinterest[i] + b_radio * radio[i] + b_simplifi * simplifi[i] + b_snapchat * snapchat[i] + b_video * video[i] + b_tv * tv[i];
    }
    for ( i in 1:12956 ) {
        big_model[i] = big_model[i] + b_taboola * taboola[i] + b_tiktok * tiktok[i] + b_twitter * twitter[i] + b_sin1 * sin1[i] + b_cos1 * cos1[i] + b_sin2 * sin2[i] + b_cos2 * cos2[i] + b_week * week[i] + store_int[store_id[i]] + a0;
    }
    leads ~ normal( big_model , big_sigma );
}


