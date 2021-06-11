
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] yobs;
  vector[N] yerr;
  vector[N] yerrB;
}
transformed data {
  vector[N] ylim;
  for(i in 1:N){
    ylim[i] = 0;
    if (yerr[i] != yerrB[i]){
      ylim[i] = yobs[i];
    }
  }
}
parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
  vector[N] yreal;
}
model {
  vector[N] yf;
  for(i in 1:N){
    yf[i] = fmax(ylim[i],yreal[i]);
  }
  yreal ~ normal(beta0 + beta1*x,sigma);
  yobs ~ normal(yf,yerr);
  beta0 ~ normal(0.,10.);
  beta1 ~ normal(0.,10.);
  sigma ~ cauchy(0.,10.);
}
