#include <TMB.hpp>                                // Links in the TMB libraries

template<class Type>
  Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(clo);                                 // Data vector transmitted from R
  DATA_FACTOR(subj);                                 // Data vector transmitted from R
  DATA_FACTOR(subDay);
  DATA_FACTOR(subj_from_subDay);
  DATA_FACTOR(sex);                                 // Data vector transmitted from R
  
  PARAMETER_VECTOR(u);                             // Random effects
  PARAMETER_VECTOR(v);                             // Random effects
  PARAMETER_VECTOR(gamma);                             // Random effects
  
  // Parameters
  PARAMETER(mu);
  PARAMETER(beta);                    // Parameter value transmitted from R
  PARAMETER(alpha);                    // Parameter value transmitted from R
  PARAMETER(sigma_u);                              // Parameter value transmitted from R
  PARAMETER(sigma_v);                              // Parameter value transmitted from R
  PARAMETER(sigma_G);                              // Parameter value transmitted from R
  PARAMETER(sigma);                                // Parameter value transmitted from R
  
  ADREPORT(gamma);
  Type mean_ran = Type(0);
  
  int i, ij;
  Type f = 0;                                      // Declare the "objective function" (neg. log. likelihood)
  
  // gamma contribution  
  for (int i=0; i < gamma.size(); i++){
    f -= dnorm(gamma[i], mean_ran, pow(sigma_G, 2), true);
  }
  
  //u contribution
  for (int i=0; i < u.size(); i++){
    f -= dnorm(u[i], mean_ran, pow(sigma_u, 2)*(1 - alpha*sex[i])*exp(-gamma[i]), true);
  }
  
  // v contribution
  for (int ij=0; ij < v.size(); ij++){
    i = subj_from_subDay[ij];
    f -= dnorm(v[ij], mean_ran, pow(sigma_v,2)*(1 - alpha*sex[i])*exp(-gamma[i]), true);
  }
  
  for (int k=0; k < clo.size(); k++){
    i = subj[k];
    ij = subDay[k];
    // main contribution
    f -= dnorm(clo[k], mu + beta*sex[i] + u[i] + v[ij], pow(sigma,2)*(1 - alpha*sex[i])*exp(-gamma[i]), true);
  }
  return f;
}

