
# generate random walk data
# dimensions and constants
nyrs <- 20
nseries <- 1 # we actually just may need 1 series for this example.
mu_con <- 0.75
sigma_proc <- 1
sigma_obs <- 1
nsampy <- 20
# calculate
mu_1 <- rnorm(nseries, mean = 0, sd = sigma_proc)
# calculate the mu time series
mu_ts <- lapply(mu_1, function(first_mu, len, mu_con) {
  mu_ts <- vector(mode = "numeric", length = len)
  mu_ts[1] <- first_mu
  for(i in seq_len(len - 1)) {
    mu_ts[i+1] <- rnorm(1, mean = mu_con + mu_ts[i], sd = sigma_proc)
  }
  mu_ts
}, len = nyrs, mu_con = mu_con)
names(mu_ts) <- paste0("ts_", seq_len(nseries))

# calculate ys based on mu

ys_ts <- lapply(mu_ts, function(one_mu_ts, sd_obs, nsampy){
  tmp_y <- lapply(one_mu_ts, function(each_mu, sd_obs, nsampy) {
       y <- rnorm(nsampy, mean = each_mu, sd = sd_obs)
       y
  }, sd_obs = sd_obs, nsampy = nsampy)
  tmp_y # this is a vector containing
}, sd_obs = sigma_obs, nsampy = nsampy)


