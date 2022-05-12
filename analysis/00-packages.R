# install packages

# will need this version to replicate report
remotes::install_github("pbs-assess/sdmTMB", ref = "delta")
devtools::install_github("seananderson/ggsidekick")
install.packages(c("StanHeaders", "rstan", "tmbstan"),type="source")

if(!require(raster))install.packages("raster")
if(!require(sf))install.packages("sf")
if(!require(tidyverse))install.packages("tidyverse")
if(!require(marmap))install.packages("marmap")

if(!require(patchwork))install.packages("patchwork")
if(!require(here))install.packages("here")

# # tests to confirm working
# library(rstan)
# scode <- "
# parameters {
#   real y[2];
# }
# model {
#   y[1] ~ normal(0, 1);
#   y[2] ~ double_exponential(0, 2);
# }
# "
# fit1 <- stan(model_code = scode, iter = 10, verbose = FALSE)
# print(fit1)
#
# options(mc.cores = parallel::detectCores())
#
# fit2 <- stan(fit = fit1, iter = 100000, chains = 6, verbose = FALSE)
#
# #  the above works in parallel, below doesn't work in parallel
#
# library(tmbstan)
# runExample("simple")
#
# ## Run a single chain in serial with defaults
# fit <- tmbstan(obj, chains=1)
#
# ## Run in parallel with a init function
# cores <- parallel::detectCores()-1
# options(mc.cores = cores)
# init.fn <- function()
#   list(u=rnorm(114), beta=rnorm(2), logsdu=runif(1,0,10), logsd0=runif(1,0,1))
# fit <- tmbstan(obj, chains=cores, open_progress=FALSE, init=init.fn)
#


