#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_INTEGER(t); // Length of the y time series (way to calc within tmb?)
  DATA_INTEGER(n); // Number of time series observed (way to count within tmb?)
  DATA_MATRIX(y); // Has n rows and t columns

  // Parameters
  PARAMETER(mu);
  PARAMETER(log_sigma_proc); //to do: may want to put these on log scale for better likelihood surface?
  PARAMETER(log_sigma_obs);

  // calcs, expected values for time series
  vector<Type> mu_exp(t);
  for (int i = 0; i < (t - 1); i++) {
  mu_exp[0] = Type(0.0);
    mu_exp(i+1) = mu + mu_exp(i);
  }

  // objective function
  Type nll = 0.0; // Declare the "objective function" (neg. log. likelihood)
  // Contribution for first 2 equations???
  nll += -dnorm(mu_exp[0], Type(0.0), exp(log_sigma_proc), true); //need something like this?
  for(int i = 0; i < (t-1); i++) {
    nll += -dnorm(mu_exp(i+1), mu + mu_exp(i), exp(log_sigma_proc), true);
  }
  // contribution for the y values
  for(int i = 0; i < n; i++) {
    nll += -sum(dnorm(y(i), mu_exp, exp(log_sigma_obs), true));
  }

  return nll;
}
