// Model 1: Simple
data {
  // model values
  int n_patients;
  array[n_patients] int<lower=0, upper=1> y; // Diagnosis: 0=none, 1=disease
  
  // quantitative
  array[n_patients] int<lower=0, upper=100> age;
  array[n_patients] int<lower=0> maxHR;
  array[n_patients] real<lower=-10, upper=10> oldPeak;
  
  // categorical
  array[n_patients] int<lower=0, upper=1> sex;
  array[n_patients] int<lower=0, upper=1> exAngina;
  array[n_patients] int<lower=-1, upper=1> stSlope1;
  array[n_patients] int<lower=-1, upper=1> stSlope2;
}

parameters {
  real intercept;
  real betaSlope_age;
  real betaSlope_maxHR;
  real betaSlope_oldPeak;
  real betaSlope_exAngina;
  real gamSlope1;
  real gamSlope2;
  //vector<lower=0, upper=1>[n_patients] likelihood_disease;
}

transformed parameters {
  vector<lower=0, upper=1>[n_patients] loglikelihood_disease;
  for (i in 1:n_patients) {
    loglikelihood_disease[i]     
      = inv_logit(intercept + 
        betaSlope_age*age[i] + 
        betaSlope_maxHR*maxHR[i] +
        betaSlope_oldPeak*oldPeak[i] +
        betaSlope_exAngina*exAngina[i] +
        gamSlope1*stSlope1[i] + 
        gamSlope2*stSlope2[i]
      ); 
  }
}

model {
  intercept ~ normal(0, 25);
  betaSlope_age ~ normal(0, 25);
  betaSlope_maxHR ~ normal(0, 25);
  betaSlope_oldPeak ~ normal(0, 25);
  betaSlope_exAngina ~ normal(0, 25);
  gamSlope1 ~ normal(0, 25);
  gamSlope2 ~ normal(0, 25);

  for (i in 1:n_patients) {
    y[i] ~ bernoulli(loglikelihood_disease);
  }
}

generated quantities {
  vector[n_patients] predDisease_prob = inv_logit(loglikelihood_disease);
}
