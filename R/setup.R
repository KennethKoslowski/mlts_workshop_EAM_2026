library(tidyverse)
library(mlts)
library(knitr)
library(ggplot2)
library(knitr)
library(kableExtra)

theme_set(
  theme_minimal(base_size = 18)
)

knitr::opts_chunk$set(
  fig.width = 8,
  fig.height = 5,
  fig.align = "center",
  out.width = "90%",
  collapse = TRUE,
  comment = " ",
  warning = FALSE,
  message = FALSE
)

options(knitr.kable.NA = '')

bayesplot::bayesplot_theme_set(
  bayesplot::theme_default(base_size = 18)
)

# load data at this point to make it globally available
# source("live_session/bolzen_prepare.R") # this takes a long time to render
# when internet is slow, I deactivate this
load("./downloaded_data/bolzen_baseline.rda")
load("./downloaded_data/bolzen.rda")
