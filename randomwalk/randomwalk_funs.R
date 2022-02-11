# functions ----
#' Generate randomwalk data based on parameters
#'
#'  @param nyrs The total number of years in each time series
#'  @param mu_con The mu constant value in the random walk
#'  @param sigma_proc The process error
#'  @param sigma_obs The observation error
#'  @param nsampy The number of reps for each time series. Note total number of
#'   obs is nyrs*nsampy
#'  @return a matrix with nsampy rows and nyrs columns containing the y observations.
get_y_data <- function(nyrs, mu_con = 0.75, sigma_proc = 1,
                       sigma_obs = 1, nsampy) {
  nseries <- 1 # hard coded because that is all we needed for this example
  # and would need to rework the last step if using > nseries
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

  # tranform data to be the right dimensions
  if(nseries != 1) {
    stop("converting to matrix code will not work")
  }
  ys_ts_mat <- matrix(data = unlist(ys_ts), nrow = nsampy, ncol = nyrs, byrow = FALSE)

  return(list(data = ys_ts_mat, hidden_states = unlist(mu_ts)))
}

#' compile admb fxn ---
#'
#'
compile_and_load_tmb_cpp <- function(cpp_path) {
  orig_wd <- getwd()
  setwd(dirname(cpp_path))
  on.exit(setwd(orig_wd))
  status <- TMB::compile(basename(cpp_path))
  modname <- gsub(".cpp", "", basename(cpp_path), fixed = TRUE)
  dyn.load(TMB::dynlib(modname))
  return(status)
}
