#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_VECTOR(y);

  // Parameters
  PARAMETER_VECTOR(mu);
  PARAMETER(sigma_proc);
  PARAMETER(sigma_obs);
  // calcs --- not sure how to do the calcs and likelihood section
  // mu1 ~ N(0, sigma_proc)
  int timeSteps=y.size();

+
  // objective function
  Type nll = 0; // Declare the "objective function" (neg. log. likelihood)
  nll = -sum(dnorm(1, 0, sigma_proc, true)); // Use R-style call to normal density
  nll += -sum(dnorm(y,mu,sigma,true)); // Use R-style call to normal density
  nll = -sum(dnorm(y,mu,sigma,true)); // Use R-style call to normal density


  return nll;
}
