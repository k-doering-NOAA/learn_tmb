#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_VECTOR(y);

  // Parameters
  PARAMETER_VECTOR(mu_exp); // this may not be a parameter? not sure.
  PARAMETER(mu);
  //PARAMETER(mu_init);
  PARAMETER(sigma_proc);
  PARAMETER(sigma_obs);
  // calcs
  int timeSteps=y.size();
  mu_exp(0) = 0;
  for (int i = 0; i < (timeSteps - 1); i++) {
    mu_exp(i+1) = mu + mu_exp(i);
  }

  // objective function
  Type nll = 0.0; // Declare the "objective function" (neg. log. likelihood)
  // Contribution for the mu_exp values
  //nll += -dnorm(mu_exp(0), 0, sigma_proc, true); //need something like this?
  nll += -sum(dnorm(mu_exp, mu, sigma_proc, true)); // Use R-style call to normal density
  // contribution for the y values
  nll += -sum(dnorm(y, mu_exp, sigma_obs, true)); // Use R-style call to normal density

  return nll;
}
