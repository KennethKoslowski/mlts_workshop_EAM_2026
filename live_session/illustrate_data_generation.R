
# Illustration of Data Generation -------------------------------------------- #

library(mlts)

# model specification
ar1 = mlts_model(q = 1, max_lag = 1)

# generate data according to model
simData = mlts_sim(
  model = ar1,
  N = 100,
  TP = 70,
  default = TRUE,
  seed.true = 12345,
  seed = 100
  )

class(simData)

# inspect true values for population parameters
simData$model[,c("Type", "true.val")]

# inspect true values for cluster-specific effects
head(simData$RE.pars)

# inspect the generated data
head(simData$data, 10)

# potential to adjust data:
# e.g., add missing values
prop_missing = 0
missing_rows = sample(x    = 1:nrow(simData$data),
                      size = prop_missing*nrow(simData$data))

simData$data$Y1[missing_rows] <- NA


# quick estimation of the model
fit <- mlts_fit(
  model = ar1,

  # choose one of:
  data = simData,         # will retain data generating values (uses na.rm = TRUE)

  # data = simData$data,  # will not store data generating values but can be combined with tinterval
  # tinterval = 1,
  # time = "time"

  id = "ID",
  ts = "Y1",
  iter = 1000,
  monitor_person_pars = T
  )

# quick summary check
summary(fit)

# lets plot the results
## population parameters
mlts_plot(fit = fit, type = "fe", add_true = T)

## cluster-specific estimates
mlts_plot(fit = fit, type = "re", add_true = T,
          sort_est = "mu_1", hide_xaxis_text = F)


## Inspect individual parameter reliability -----------------------------------
## i.e., squared correlation between true values and estimates
param = "phi(1)_11"
phis_true = simData$RE.pars[,param]
phis_est  = fit$person.pars.summary[fit$person.pars.summary$Param == param ,"mean"]

# reliability
cor(phis_est, phis_true)^2

# outlook: mlts_rel(), currently not available to you
# mlts_rel(fit, method = "RMU")
# provides an estimate of individual parameter reliability (and CI, if requested)
# based on the estimates alone.


# User-specified population parameter values ----------------------------------

## 1. either overwrite default values
simData$model$true.val[simData$model$Param == "phi(1)_11"] <- 0.3
simData$model[,c("Type", "true.val")]

## 2. use estimates of the model
simData$model[,c("Type", "true.val")]
fit$pop.pars.summary[,c("Param", "mean")]

# again overwrite the values
simData$model$true.val = fit$pop.pars.summary$mean


# generate data according to model
simData = mlts_sim(
  model = simData$model,
  N = 100,
  TP = 70,
  default = FALSE, # (!)
  seed = 1001
  )


