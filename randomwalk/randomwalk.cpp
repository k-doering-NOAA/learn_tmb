#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_INTEGER(t); // Length of the y time series (way to calc within tmb?)
  DATA_INTEGER(n); // Number of time series observed (way to count within tmb?)
  DATA_MATRIX(y); // Has n rows and t columns

  //TODO: add mu_exp as a parameter

  // Parameters
  PARAMETER(mu);
  PARAMETER(log_sigma_proc);
  PARAMETER(log_sigma_obs);
  PARAMETER_VECTOR(mu_exp);

  // objective function
  Type t_test = y.cols();
  Type n_test = y.rows();

  Type nll = 0.0; // Declare the "objective function" (neg. log. likelihood)
  // Contribution for first equations
  nll += -dnorm(mu_exp(0), Type(0.0), exp(log_sigma_proc), true);
  for(int i = 0; i < (t-1); i++) {
    nll += -dnorm(mu_exp(i+1), mu + mu_exp(i), exp(log_sigma_proc), true);
  }
  // contribution for the y values
  for(int i = 0; i < n; i++) {
    nll += -sum(dnorm(y(i), mu_exp, exp(log_sigma_obs), true));
  }

  REPORT(t_test);
  REPORT(n_test);

  return nll;
}
