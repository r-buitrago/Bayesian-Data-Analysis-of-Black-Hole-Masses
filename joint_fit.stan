data {
  int<lower=0> N;
  int<lower=0> M;
  vector[N] yobs;
  vector[N] yerr1;
  vector[N] yerr2;
  matrix[M,N] xobs;
  matrix[M,N] xerr;
}
transformed data{
  vector[N] ylim;
  vector[M] xcen;
  vector<lower=0.>[M] xran;
  for (i in 1:N){
    ylim[i] = 0;
    if(yerr1[i] != yerr2[i]){
      ylim[i] = yobs[i];
    }
  }
  for (j in 1:M) {
    xcen[j] = mean(xobs[j,]);
    xran[j] = 5.*sd(xobs[j,]);
  }
}
parameters {
  vector[M] beta0;
  vector[M] beta1;
  vector[N] yreal;
  vector<lower=0>[M] sigma;
  matrix[M,N] xreal;
}
model {
  vector[N] yf;
  for(i in 1:N){
    yf[i] = fmax(ylim[i], yreal[i]);
  }
  for(i in 1:M){
    xreal[i] ~ normal(xcen[i], xran[i]);
    target += normal_lpdf(xobs[i] | xreal[i], xerr[i]) / M;
    target +=
    normal_lpdf(yreal | beta0[i] + xreal[i]*beta1[i], sigma[i]) / M;
  }
  yobs ~ normal(yf, yerr1);
  beta0 ~ normal(0.,10.);
  beta1 ~ normal(0.,10.);
  sigma ~ cauchy(0.,10.);
}
