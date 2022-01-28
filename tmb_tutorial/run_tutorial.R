# run fxn in tmb_tutorial.cpp
# make data ----
n <- 100
x <- rnorm(n = n, mean = 0, sd = 1) # Generate data

# use tmb to create r obj -----
library(TMB)
compile("tutorial.cpp") # Compile the C++ file. 0 means compiled.
dyn.load(dynlib("tutorial")) # Dynamically link the C++ code

# make the r object called r_fun that represents the c++ fxn
r_fun <- MakeADFun(
  data = list(x = x),
  parameters = list(mu = 0, sigma = 1)
)
# note variable names must correspond to those in c++ program
# can look at this list object to see info, like the First and second order
# derivatives, the C++ functon

# minimize the neg log likelihood ----
fit <- nlminb(r_fun$par,
  r_fun$fn,
  r_fun$gr,
  lower = c(-10, 0.0), upper = c(10.0, 10.0)
)
print(fit)
print(fit$par)

# now, try integrated likelihood ----
f_integrated <- MakeADFun(
  data = list(x = x),
  parameters = list(mu = 0, sigma = 1),
  random = "mu"
)

fit2 <- nlminb(start = f_integrated$par,
               objective = f_integrated$fn,
               gradient = f_integrated$gr,
               lower = c(0.00001),
               upper = c(10.0))

#note that the sigma from the model is is the the ordinary empirical sd.
print(fit2$par)
sd(x)

