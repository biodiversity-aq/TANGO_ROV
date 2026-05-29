library(tidyverse)
library(httr2)
library(fs)

get_zenodo_config <- function(sandbox = TRUE) {
  
  if (sandbox) {
    list(
      base_url = "https://sandbox.zenodo.org/api",
      token = Sys.getenv("ZENODO_SANDBOX_TOKEN")
    )
  } else {
    list(
      base_url = "https://zenodo.org/api",
      token = Sys.getenv("ZENODO_PROD_TOKEN")
    )
  }
}

cfg <- get_zenodo_config(sandbox = TRUE)

base_url <- cfg$base_url
token <- cfg$token


