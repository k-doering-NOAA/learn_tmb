# Run the random walk exercise ----
# call libraries -----
library(TMB)
source("randomwalk/randomwalk_funs.R")

# sample the data ----
y_dat <- get_y_data(nyrs = 20, nsampy = 50)

# compile and load ----
compile_and_load_tmb_cpp("randomwalk/randomwalk.cpp")

# make r obj ----

r_fun <- MakeADFun(
  data = list(y = y_dat$data),
  parameters = list(mu = 0.70, log_sigma_proc = log(1), log_sigma_obs = log(1.1),
                    mu_exp = rep(0.0, length.out = ncol(y_dat$data))),
  random = "mu_exp"
)

# fit -----
fit <- nlminb(r_fun$par,
              r_fun$fn,
              r_fun$gr,
              lower = c(-10.0, -10.0, -10.0), upper = c(10.0, 10.0, 10.0)
)

print(fit)
print(fit$par)

# report values ----
r_fun$report()

reporting <- sdreport(fit)
summary(reporting, "fixed", p.value = TRUE)

# maybe log_sigma_proc needs to be fixed? goes to -10, the lower bound

# integrated likelihood?? Maybe this is what I should be using??

# now, try integrated likelihood ----
f_integrated <- MakeADFun(
  data = list(t = ncol(y_dat), n = nrow(y_dat$data), y = y_dat$data),
  parameters = list(mu = 0.70, log_sigma_proc = log(1.1), log_sigma_obs = log(1.1)),
  random = "mu_exp"
)

fit2 <- nlminb(start = f_integrated$par,
               objective = f_integrated$fn,
               gradient = f_integrated$gr,
               lower = c(-10, -10),
               upper = c(10, 10))
print(fit2)
# still doesn't work....

# TMB questions ----
# 1. How to fix a parameter from R?
# 2. How to get matrix dims within Cpp instead of needing to pass from r? .size function??
# look at this: http://kaskr.github.io/adcomp/matrix_arrays_8cpp-example.html
# 3. Remedial stats help; converting a hierarchical model to TMB. What needs likelihood
# components?

# Debugging tips and tricks
# gdb
# can use cout within cpp, which helps if you get it to compile
# REPORT() can also be helpful
# callr package? can use it to avoid crashes in TMB
# when to use parentheses or brackets?
# referencing a column, use parentheses; but needs to be square brackets
# C++ uses square brackets like x[i][j] ; Eigen library has some different
# stuff

# other stuff
# Keep vector???OSA = one step ahead (in the random walk tmb example)
# DATA_VECTOR_INDICATOR

