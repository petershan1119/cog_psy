data {
  int<lower=0> N;
  int<lower=1> T;
  int<lower=1, upper=T> Tsubj[N];
  int<lower=0, upper=1> Decision[N, T];
  real<lower=1, upper=5> OC[N, T];
  int<lower=-1, upper=1> reFeedback[N, T];
}

transformed data {
}

parameters {
  vector[4] mu_pr;
  vector<lower=0>[4] sigma;
  vector[N] alpha_pr;
  vector[N] beta_vsp_pr;
  vector[N] beta_oc_pr;
  vector[N] beta_0_pr;
}

transformed parameters {
  vector<lower=0, upper=1>[N] alpha;
  vector[N] beta_vsp;
  vector[N] beta_oc;
  vector[N] beta_0;
  
  for (i in 1:N) {
    alpha[i] = Phi_approx(mu_pr[1] + sigma[1] * alpha_pr[i]) * 1;
  }
  
  beta_vsp = mu_pr[2] + sigma[2] * beta_vsp_pr;
  beta_oc = mu_pr[3] + sigma[3] * beta_oc_pr;
  beta_0 = mu_pr[4] + sigma[4] * beta_0_pr;
  
}

model {
  mu_pr ~ normal(0, 1);
  sigma ~ cauchy(0, 1);
  
  alpha_pr ~ normal(0, 1);
  beta_vsp_pr ~ normal(0, 1);
  beta_oc_pr ~ normal(0, 1);
  beta_0_pr ~ normal(0, 1);
  
  for (i in 1:N) {
    vector[Tsubj[i]] VSP;
    vector[Tsubj[i]] pDecision;
    vector[Tsubj[i]] VSP_r;
    vector[Tsubj[i]] OC_r;
    vector[Tsubj[i]] VSP_mean;
    vector[Tsubj[i]] OC_mean;
    vector[Tsubj[i]] OC_v;
    
    
    VSP = rep_vector(0, Tsubj[i]);
    OC_v = rep_vector(0, Tsubj[i]);
    
    for (t in 1:Tsubj[i]) {
      real PE;
      
      if (t==1) {
        PE = OC[i, t];
        VSP[t] = alpha[i] * PE;
      } else {
        PE = OC[i, t] - VSP[t];
        VSP[t] = VSP[t-1] + alpha[i] * PE;
      }
      OC_v[t] = OC[i, t];
    }
    
    VSP_mean = rep_vector(mean(VSP), Tsubj[i]);
    OC_mean = rep_vector(mean(OC_v), Tsubj[i]);
    VSP_r = VSP - VSP_mean;
    OC_r = OC_v - OC_mean;
    
    for (t in 1:Tsubj[i]) {
      pDecision[t] = inv_logit(beta_vsp[i]*VSP_r[t] + beta_oc[i]*OC_r[t] + beta_0[i]);
      Decision[i, t] ~ bernoulli(pDecision[t]);  
    }
  }
}
