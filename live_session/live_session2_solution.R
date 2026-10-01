###############################################################################
############################### Live Session 2 ################################
###############################################################################

# load packages
library(mlts)
library(dplyr)
library(tidyr)
library(ggplot2)

# Run 'bolzen_prepare.R' to download and preprocess the data 
source("live_session/bolzen_prepare.R")
## should load two objects into the environment:
### 'bolzen'       
### 'bolzen_grid' 


# preparate a folder to store results 
dir.create(path = "live_session/live_session2_fits/")


# 1. Bivariate VAR(1) model ====================================================

# model specification
model = mlts_model(
  q = 2,
  fix_dynamics = TRUE,
  fix_inno_vars = TRUE
)

# model estimation 
fit_1.1 = mlts_fit(
  model = model,
  data = bolzen %>% filter(group == "dm" & phase == "i"), 
  id = "id",
  ts = c("rnt", "posaff"),
  iter = 2000,
  seed = 1234
)

# check results 
summary(fit_1.1, flag_signif = TRUE, digits = 2)


# save fitted object 
save(fit_1.1, file = "live_session/live_session2_fits/Model_1.1_VAR1.Rdata")



# 2. Between-Level Covariates ==================================================

# Selected variables: 
##  Time-varying: negaff (negative affect - composite score)

##  Person-level covariate: binary-coded intervention condition 
##                          (0 = mindfullness, 1 = detached mindfullness)
bolzen$group_bi = ifelse(bolzen$group == "dm", 1, 0)

# Selected data:
## Run analyses only for intervention phase only 


# model specification
model = mlts_model(
  q = 1,                   
  ranef_pred = "group_bi" # name of covariate in the data
)

# model estimation 
fit_2.1 = mlts_fit(
  model = model,
  data = bolzen %>% filter(phase == "i"), # subset of the data 
  id = "id",
  ts = c("negaff"),
  iter = 2000,
  seed = 1234,
  # IMPORTANT:
  # For binary covariates, to avoid cluster-mean centering, set
  center_covs = FALSE
)

# check results 
summary(fit_2.1, flag_signif = TRUE, digits = 2)

# get standardized regression estimates:
mlts_standardized(fit_2.1)

# save fitted object 
save(fit_2.1, file = "live_session/live_session2_fits/Model_2.1_AR1_with_covariate.Rdata")


# 3. Prediction of Time-Stable Outcomes ========================================

# Selected variables: 
##  Time-varying: negaff (negative affect - composite score)
##  External Outcome: post_rtq_sum 

# Selected data:
## Run analyses only for intervention phase only 

# model specification
model = mlts_model(
  q = 1,                   
  out_pred = "post_rtq_sum",
  out_pred_add_btw = "pre_rtq_sum"
)

# model estimation 
fit_3.1 = mlts_fit(
  model = model,
  data = bolzen %>% filter(phase == "i"), # subset of the data 
  id = "id",
  ts = c("posaff"),
  iter = 2000,
  seed = 1234
)

# check results 
summary(fit_3.1, flag_signif = TRUE, digits = 2)

# get standardized regression estimates:
mlts_standardized(fit_3.1)


# save fitted object 
save(fit_3.1, file = "live_session/live_session2_fits/Model_3.1_AR1_with_outcome.Rdata")

# 4. Multiple-Group Models =====================================================

# Selected variables: 
##  Time-varying: negaff (negative affect - composite score)
##  Here use condition-variable (i.e. `group`) to run multiple-group model 
 
# Selected data:
## As for Model 2.1, run analyses for intervention phase only 

# model specification
model = mlts_model(
  q = 1,
  group = 2
)

# model estimation 
fit_4.1 = mlts_fit(
  model = model,
  data = bolzen %>% filter(phase == "i"), # subset of the data 
  id = "id",
  ts = c("negaff"),
  group = "group",  # new argument: pass name of grouping variable
  iter = 2000,
  seed = 1234
)

# save fitted object 
save(fit_4.1, file = "live_session/live_session2_fits/Model_4.1_multiple_group.Rdata")

# check results 
summary(fit_4.1, flag_signif = TRUE, digits = 2)
# Summary now returns group-specific estimates for all parameters of the dynamic model

# mlts_plot now returns the group-specific estimates 
mlts_plot(fit_4.1)


### Compare with results of the Model 2.1:
# For example, difference in average negaff inertia

## Model 2.1 - based on the regression weight of the binary group predictor:
par = "b_phi(1)_11.ON.group_bi"
fit_2.1$pop.pars.summary[fit_2.1$pop.pars.summary$Param == par, c("mean", "sd", "2.5%", "97.5%")]

## Model 4.1 - Based on differences variable 
# Compute difference scores based on post-warmup MCMC samples:
diff = as.vector(fit_4.1$posteriors[,,"dm: phi(1)_11"] - 
                 fit_4.1$posteriors[,,"gi: phi(1)_11"])

# Get summary statistics across MCMC draws
round(c("M" = mean(diff), 
        "SD" = sd(diff), 
        quantile(diff, probs = c(.025, .975))),3)


## Specific to the multiple-group models: 

##  Differences in random effect variances (SDs) and random effect correlations
##  use: correlation between means and log innovation variances:
diff = as.vector(fit_4.1$posteriors[,,"dm: r_mu_1.ln.sigma2_1"] - 
                 fit_4.1$posteriors[,,"gi: r_mu_1.ln.sigma2_1"])

# Get summary statistics across MCMC draws
round(c("M" = mean(diff), 
        "SD" = sd(diff), 
        quantile(diff, probs = c(.025, .975))),3)


# 5. Interaction Effects on the Dynamic Within-Level ===========================


# 5.1 Fit model with random intercepts
bolzen$phase_bi = ifelse(bolzen$phase == "b", 0,1)

# model specification
model <- mlts_model(
  q = 2,                          
  is_exogenous = 1,               
  incl_t0_effects = "phi(0)_21",  
  fixef_zero = "phi(1)_21",       
  inno_covs_zero = TRUE,            
  
  # include interaction effect:
  incl_interaction_effects = "phi(i)_2.2(1)1(0)",
  
  # assume constant regression effects and innovation variances
  fix_dynamics = T,
  fix_inno_vars = T
)

# model estimation 
fit_5.1 = mlts_fit(
  model = model,
  data = bolzen, 
  id = "id",
  ts = c("phase_bi", "rnt"),
  iter = 2000,
  seed = 1234,
)

summary(fit_5.1, digits = 2)

# save fitted object 
save(fit_5.1, file = "live_session/live_session2_fits/Model_5.1_interaction.Rdata")

# 5.2 Multi-group version of the model

# model specification
model <- mlts_model(
  q = 2,                          
  is_exogenous = 1,               
  incl_t0_effects = "phi(0)_21",  
  fixef_zero = "phi(1)_21",       
  inno_covs_zero = TRUE,            
  incl_interaction_effects = "phi(i)_2.2(1)1(0)",
  fix_dynamics = T,
  fix_inno_vars = T,
  group = 2
)

# model estimation 
fit_5.2 = mlts_fit(
  model = model,
  data = bolzen, 
  id = "id",
  ts = c("phase_bi", "rnt"),
  group = "group",
  iter = 2000,
  seed = 1234,
)

summary(fit_5.2, digits = 2)

# save fitted object 
save(fit_5.2, file = "live_session/live_session2_fits/Model_5.2_interaction_by_group.Rdata")



# 6. Multiple-Indicator Factor Models ==========================================

# Selected variables: 
## aengstlich --> anxious
## nervoes    --> nervous
## entspannt  --> relaxed

# first, rescale relaxed item
bolzen$rlx_r = 8 - bolzen$entspannt

# model specification
model = mlts_model(
  q = 1,   # one common latent construct
  p = 3,   # measured by three manifest indicators
  fix_dynamics = TRUE,
  fix_inno_vars = TRUE,
  btw_factor = FALSE
)

# run again without this subject 62: 
fit_6.1 = mlts_fit(
  model = model,
  data = bolzen %>% filter(group == "dm"),
  id = "id",
  ts = c("aengstlich", "nervoes", "rlx_r"),
  iter = 3000,
  seed = 1234
)


summary(fit_6.1)

# save fitted object 
save(fit_6.1, file = "live_session/live_session2_fits/Model_6.1_ThreeIndicator_AR1.Rdata")



# 7. Censoring Models ==========================================================

# Selected variable: rnt (repetitive negative thinking)

describe_ts(data = bolzen, ts = "negaff")
# > 14.5% of non-missing observations at the lower scale bound!

# inspect the distribution in histogram
hist(bolzen$rnt)

# get the lower scale bound 
LB = min(bolzen$rnt, na.rm = T)  # = 1

# 7.1. Apply Censored AR(1) Model ----------------------------------------------

# model specification
model = mlts_model(
  q = 1,
  censor_left = LB,  # equals 1 (see above)
)

# model estimation 
fit_7.1 = mlts_fit(
  model = model,
  data = bolzen,
  id = "id",
  ts = "negaff",
  iter = 2000
)

# save fitted object 
save(fit_7.1, file = "live_session/live_session2_fits/Model_7.1_CensoredAR1.Rdata")

# check results 
summary(fit_7.1, flag_signif = TRUE, digits = 2)


# 7.2. Apply Standard AR(1) Model ----------------------------------------------

# model specification
model = mlts_model(
  q = 1
)

# model estimation 
fit_7.2 = mlts_fit(
  model = model,
  data = bolzen,
  id = "id",
  ts = "negaff",
  iter = 2000
)

# save fitted object 
save(fit_7.2, file = "live_session/live_session2_fits/Model_7.2_AR1.Rdata")

bayesplot::mcmc_trace(fit_7.2$posteriors[,,])

# check results 
summary(fit_7.2, flag_signif = TRUE, digits = 2)



## Compare estimates: 
cbind(
  fit_7.1$pop.pars.summary[,c("Param", "mean", "2.5%", "97.5%")],
  fit_7.2$pop.pars.summary[,c("mean", "2.5%", "97.5%")]
)


