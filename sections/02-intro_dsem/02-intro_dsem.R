###############################################################################
################################ Intro to DSEM ################################
###############################################################################

# load packages
library(mlts)
library(lme4)
library(osfr)
library(dplyr)
library(tidyr)


# Get Data ####################################################################

# load data
load("./downloaded_data/bolzen.rda")


# Two-Level AR(1)-Model using lme4 ############################################

# create lagged variable for lme4
# and take only baseline ESM survey
bolzen_baseline <- bolzen %>%
  filter(phase == "b") %>% # select baseline phase
  group_by(id) %>%
  mutate(
    posaff_pmc = posaff - mean(posaff, na.rm = T),
    posaff_pmc_lag1 = lag(posaff_pmc)
  )

# model in lme4
lme4_fit1 <- lmer(
  posaff ~ 1 + posaff_pmc_lag1 +
    (1 + posaff_pmc_lag1 | id),
  data = bolzen_baseline
)

summary(lme4_fit1)


## Plot Model Results =========================================================

### 1) model predictions ------------------------------------------------------

# fixed effects
fe <- fixef(lme4_fit1)

# person-specific parameters
re <- ranef(lme4_fit1)$id
coefs <- coef(lme4_fit1)$id  # this already gives fixed + random per id
coefs$id <- rownames(coefs)
names(coefs)[1:2] <- c("intercept", "slope")

# data range for drawing lines (use actual range of the predictor)
x_range <- range(bolzen_baseline$posaff_pmc_lag1, na.rm = TRUE)

ggplot() +
  # individual lines
  geom_abline(
    data = coefs,
    aes(intercept = intercept, slope = slope, group = id),
    color = "grey70", linewidth = 0.3, alpha = 0.6
  ) +
  # fixed-effect (average) line
  geom_abline(
    intercept = fe[1], slope = fe[2],
    color = "green4", linewidth = 1.5
  ) +
  xlim(x_range) +
  ylim(range(bolzen_baseline$posaff, na.rm = TRUE)) +
  labs(
    x = "Positive affect (lag 1, person-mean centered)",
    y = "Positive affect",
    title = "Individual (grey) vs. fixed-effect (green) slopes"
  ) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(size = 16))


### 2) time series ------------------------------------------------------------

# requires that you refit the model with na.action = na.exclude!
lme4_fit1 <- lmer(
  posaff ~ 1 + posaff_pmc_lag1 +
    (1 + posaff_pmc_lag1 | id),
  data = bolzen_baseline,
  na.action = na.exclude
)

# extract fitted values
bolzen_baseline$fitted <- predict(lme4_fit1)

# pick a handful of individuals to keep it readable
set.seed(14)
sample_ids <- sample(unique(bolzen_baseline$id), 4)

# plot
bolzen_baseline %>%
  filter(id %in% sample_ids) %>%
  ggplot(aes(x = obs_counter, y = posaff)) +   # replace `time` with your actual time/obs-index variable
  geom_line(color = "grey40") +
  geom_point(color = "grey40", size = 1) +
  geom_line(aes(y = fitted), color = "steelblue", linewidth = 0.8) +
  facet_wrap(~ id, ncol = 2, nrow = 3) +
  labs(y = "Positive affect", x = "Time",
       title = paste0("Observed (grey) vs. predicted (blue) positive affect for ", length(sample_ids),  " IDs")) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(size = 16))


# Two-Level AR(1)-Model using mlts ###########################################

## Without restrictions ======================================================

# model in mlts
mlts_m1 <- mlts_model(q = 1, max_lag = 1)
mlts_m1 # inspect

# helper functions: path model
mlts_paths(mlts_m1)

# model formula
mlts_model_formula(mlts_m1)
# - this creates a pdf and a .rmd file with LaTeX code in your
#   working directory

# path model in LaTeX
mlts_model_formula(mlts_m1)
# - this creates a pdf and a .rmd file with LaTeX code in your
#   working directory

# fit model
mlts_fit1 <- mlts_fit(
  model = mlts_m1, # model object
  data = bolzen,   # data set
  id = "id",       # id variable
  ts = "posaff",   # time series construct
  iter = 2000      # number of MCMC iterations
)

summary(mlts_fit1)

# inspect traceplot for chain mixing
rstan::traceplot(mlts_fit1$stanfit)

# save in directory
saveRDS(mlts_fit1, file = "./sections/02-intro_dsem/mlts_fit1.rds")


## Helper functions after model fitting =======================================

# forest plot of model coefficients
mlts_plot(mlts_fit1, bpe = "mean")

# posterior predictive checks plot requires that you fit the model
# with argument `monitor_person_pars = TRUE`
# fit model
mlts_fit1_pp <- mlts_fit(
  model = mlts_m1,
  data = bolzen,
  id = "id",
  ts = "posaff",
  iter = 2000,
  monitor_person_pars = TRUE # save person-specific parameters
)

# plot posterior predictions against observed data
mlts_pp_check(mlts_fit1_pp)

# set plot limits by hand if too large (i.e., choose mean +- 4 SD)
bolzen_mean <- mean(bolzen_baseline$posaff)
bolzen_sd <- sd(bolzen_baseline$posaff)

mlts_pp_check(mlts_fit1_pp) +
  scale_x_continuous(
    limits = c(bolzen_mean - 4 * bolzen_sd,
               bolzen_mean + 4 * bolzen_sd)
  )

# save
saveRDS(mlts_fit1_pp, file = "./sections/02-intro_dsem/mlts_fit1_pp.rds")


## Restrict innovation variance ===============================================

# fix inno vars!
mlts_m2 <- mlts_model(q = 1, fix_inno_vars = TRUE)
mlts_m2

# alternatively, restrict random effects to zero by parameter name
mlts_m2 <- mlts_model(q = 1, ranef_zero = "ln.sigma2_1")
mlts_m2 # same as above

# paths and model formula
mlts_paths(mlts_m2)
mlts_model_formula(mlts_m2)


# fit model
mlts_fit2 <- mlts_fit(
  model = mlts_m2,
  data = bolzen,
  id = "id",
  ts = "posaff",
  iter = 2000
)

summary(mlts_fit2)

# fit model, save person pars as well if you want pp checks
mlts_fit2_pp <- mlts_fit(
  model = mlts_m2,
  data = bolzen,
  id = "id",
  ts = "posaff",
  iter = 2000,
  monitor_person_pars = TRUE
)

mlts_pp_check(mlts_fit2_pp)

# save
saveRDS(mlts_fit2, file = "./sections/02-intro_dsem/mlts_fit2.rds")
saveRDS(mlts_fit2_pp, file = "./sections/02-intro_dsem/mlts_fit2_pp.rds")


## Add AR(2) effect ===========================================================

# fix inno vars!
mlts_m3 <- mlts_model(q = 1, max_lag = 2, fix_inno_vars = TRUE)
mlts_m3

# paths and model formula
mlts_paths(mlts_m3)
# mlts_model_formula(mlts_m3)

# fit model
mlts_fit3 <- mlts_fit(
  model = mlts_m3,
  data = bolzen,
  id = "id",
  ts = "posaff",
  iter = 2000
)

summary(mlts_fit3)

# fit model, save person pars as well if you want pp checks
mlts_fit3_pp <- mlts_fit(
  model = mlts_m3,
  data = bolzen,
  id = "id",
  ts = "posaff",
  iter = 2000,
  monitor_person_pars = TRUE
)

mlts_pp_check(mlts_fit3_pp)


# save
saveRDS(mlts_fit3, file = "./sections/02-intro_dsem/mlts_fit3.rds")
saveRDS(mlts_fit3_pp, file = "./sections/02-intro_dsem/mlts_fit3_pp.rds")

# Check assumptions of AR model ###############################################

# plot trend
ggplot(aes(x = obs_counter, y = posaff),
       data = bolzen_baseline) +
  geom_smooth(aes(group = id),
              method = "lm", se = FALSE,
              color = "grey", linewidth = .5) +
  geom_smooth(method = "lm") +
  theme_minimal(base_size = 14) +
  labs(y = "Positive affect", x = "(Integer) Time Point")
# look good


# check measurement intervals
head(bolzen_baseline[
  , c("id", "timepoint", "time_window", "time_diff")])

# plot time points and positive affects
library(ggh4x) # for facet_grid2

# for id 1
bolzen_baseline %>%
  mutate(
    time_window = factor(time_window,levels = c("mor", "mid", "eve"))) %>%
  filter(id == 1) %>%
  ggplot(aes(y = posaff, x = timepoint, group = time_window)) +
  geom_point() +
  geom_line() +
  facet_grid(. ~ dayphase_counter + time_window) +
  labs(x = "Time Point", y = "Positive affect", title = "Real Data Structure") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(size = 10))

# for id 1
bolzen_baseline %>%
  filter(id == 1) %>%
  ggplot(aes(x = obs_counter, y = posaff)) +
  geom_point() +
  geom_line() +
  theme_minimal(base_size = 14) +
  labs(x = "Integer Time", y = "Positive affect", title = "Data Structure assumed by AR model") +
  theme(axis.text.x = element_text(size = 10))



