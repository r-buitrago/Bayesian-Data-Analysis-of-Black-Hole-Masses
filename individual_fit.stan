
data {
  int<lower=0> N;
  vector[N] yobs;
  vector[N] yerr1;
  vector[N] yerr2;
  vector[N] xobs;
  vector[N] xerr;
}

transformed data{
  vector[N] ylim;
  for(i in 1:N){
    ylim[i] = 0; 
    if(yerr1[i] != yerr2[i]){
      ylim[i] = yobs[i];
    }
  }
}

parameters {
  real beta0;
  real beta1;
  vector[N] yreal;
  vector[N] xreal;
  real<lower=0> sigma;
}

model {
  vector[N] yf;
  for(i in 1:N){
    yf[i] = fmax(ylim[i],yreal[i]);
  }

  yreal ~ normal(beta0 + beta1*xreal,sigma);
  yobs ~ normal(yf,yerr1);
  xobs ~ normal(xreal,xerr);

  beta0 ~ normal(0.,10.);
  beta1 ~ normal(0.,10.);
  xreal ~ normal(0.,10.);
  sigma ~ cauchy(0.,10.);
}
