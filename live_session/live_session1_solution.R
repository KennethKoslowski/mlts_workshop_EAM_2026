###############################################################################
############################### Live Session 1 ################################
###############################################################################

# load packages
library(mlts)
library(dplyr)
library(tidyr)
library(rstan)


# 1. AR(1) model for repetitive negative thinking #############################

# model in mlts
mlts_m1 <- mlts_model(q = 1)
mlts_m1
# mlts_model_paths(mlts_m1)

# fit
mlts_fit1 <- mlts_fit(
  model = mlts_m1,
  data = bolzen_baseline,
  id = "id",
  ts = "rnt",
  iter = 2000
) # throws an error


# Error Message:
# "Within-cluster variance is zero for indicator rnt in cluster 19"
# - means that there is no variation in the variable for that id
#   (i.e., always answered in the same category)

# Solution: filter out id 19 manually:

bolzen_clean <- bolzen_baseline %>%
  filter(id != 19)

# refit
mlts_fit1 <- mlts_fit(
  model = mlts_m1,
  data = bolzen_clean,
  id = "id",
  ts = "rnt",
  iter = 2000
) # throws another error

# better solution: filter out all ids with Within-cluster variance == zero
# on the respective indicator
# can be achieved, e.g., with n_distinct from dplyr
bolzen_clean <- bolzen_baseline %>%
  group_by(id) %>%
  filter(n_distinct(rnt, na.rm = TRUE) > 1)

# refit
mlts_fit1 <- mlts_fit(
  model = mlts_m1,
  data = bolzen_clean,
  id = "id",
  ts = "rnt",
  iter = 500
) # throws another error

summary(mlts_fit1) # mixing is bad
traceplot(mlts_fit1$stanfit)


# 2. AR(1) model for repetitive negative thinking w/ restrictions #############

# model in mlts
mlts_m2 <- mlts_model(q = 1, fix_inno_vars = TRUE)
mlts_m2

# refit
mlts_fit2 <- mlts_fit(
  model = mlts_m2,
  data = bolzen_clean,
  id = "id",
  ts = "rnt",
  iter = 2000
)

summary(mlts_fit2)
traceplot(mlts_fit2$stanfit)



# 3. Check assumptions ########################################################

# plot trend
ggplot(aes(x = obs_counter, y = rnt),
       data = bolzen_baseline) +
  geom_smooth(aes(group = id),
              method = "lm", se = FALSE,
              color = "grey", linewidth = .5) +
  geom_smooth(method = "lm") +
  theme_minimal(base_size = 14) +
  labs(y = "RNT", x = "(Integer) Time Point")

# other method: fit growth curve model in lme4
library(lme4)

# fit growth curve model
gcm <- lmer(rnt ~ 1 + obs_counter + (1 + obs_counter | id),
            data = bolzen_baseline)
summary(gcm) # literally no growth
