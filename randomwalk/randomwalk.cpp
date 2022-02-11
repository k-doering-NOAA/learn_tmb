#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_MATRIX(y); // Has n rows and t columns

  //TODO: add mu_exp as a parameter

  // Parameters
  PARAMETER(mu);
  PARAMETER(log_sigma_proc);
  PARAMETER(log_sigma_obs);
  PARAMETER_VECTOR(mu_exp);

  // objective function
  int n = y.rows();
  int t = y.cols();


  Type nll = 0.0; // Declare the "objective function" (neg. log. likelihood)
  // Contribution for first equations
  nll += -dnorm(mu_exp(0), Type(0.0), exp(log_sigma_proc), true);
  for(int i = 0; i < (t-1); i++) {
    nll += -dnorm(mu_exp(i+1), mu + mu_exp(i), exp(log_sigma_proc), true);
  }
  // contribution for the y values
  for(int i = 0; i < n; i++) {
    for(int j = 0; j < t; j++) {
      nll += -dnorm(y(i,j), mu_exp(j), exp(log_sigma_obs), true);
    }
  }

  REPORT(n); // ceck indices being calculated
  REPORT(t); // check indices being calculated correctly
  REPORT(mu_exp);


  return nll;
}
