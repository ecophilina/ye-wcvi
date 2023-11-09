# install packages

# data wrangling, mapping and plotting packages
if(!require(tidyverse))install.packages("tidyverse")
if(!require(here))install.packages("here")
if(!require(lubridate))install.packages("lubridate")
if(!require(raster))install.packages("raster")
if(!require(sf))install.packages("sf")
if(!require(marmap))install.packages("marmap")
if(!require(patchwork))install.packages("patchwork")
if(!require(PBSmapping))install.packages("PBSmapping")
# devtools::install_github("pbs-software/pbs-mapping/PBSmapping")
if(!require(ggsidekick))devtools::install_github("seananderson/ggsidekick")
if(!require(gfplot))devtools::install_github("pbs-assess/gfplot")

# modeling packages
if(!require(sdmTMB))install.packages("sdmTMB")
# remotes::install_github("pbs-assess/sdmTMB")
remotes::install_github("pbs-assess/sdmTMBextra")
install.packages(c("StanHeaders", "rstan", "tmbstan"),type="source")


# # tests to confirm rstan is working
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
