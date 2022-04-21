# install packages

# will need this version to replicate report
remotes::install_github("pbs-assess/sdmTMB", ref = "delta")
devtools::install_github("seananderson/ggsidekick")

if(!require(sf))install.packages("sf")
if(!require(tidyverse))install.packages("tidyverse")


if(!require(patchwork))install.packages("patchwork")
if(!require(here))install.packages("here")
