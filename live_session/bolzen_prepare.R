
library(mlts)
library(dplyr)
library(tidyr)

# Dataset:
# Link to OSF: https://osf.io/z2e83/
# Link to Article: https://doi.org/10.1007/s12671-024-02350-5 
# Link to Codebook: https://osf.io/z2e83/files/uph2q

# Link to ESM data on OSF
esm_link = "https://osf.io/q4czt"
survey_link = "https://osf.io/ecsj9"

# obtain datasets directly from OSF
library(osfr)

# ESM DATA 
downloaded <- osf_download(osf_retrieve_file(esm_link), path = tempdir(), conflicts = "overwrite")
esm   <- read.csv(downloaded$local_path)

# SURVEY DATA 
downloaded <- osf_download(osf_retrieve_file(survey_link), path = tempdir(), conflicts = "overwrite")
survey   <- read.csv(downloaded$local_path)

# clean-up
unlink(downloaded)
rm(downloaded)

# select relevant survey items to add to esm data 
use = c("id", "group", "pre_rtq_sum", "post_rtq_sum")
survey = survey[,use]
survey$pre_rtq_sum = survey$pre_rtq_sum / 10
survey$post_rtq_sum = survey$post_rtq_sum / 10
survey$pre_rtq_sum_cen = survey$pre_rtq_sum - mean(survey$pre_rtq_sum)
survey$post_rtq_sum_cen = survey$post_rtq_sum - mean(survey$post_rtq_sum)


# Merge data 
bolzen <- merge(x = survey, y = esm, by = c("id", "group"), all = TRUE)

# Sort data 
bolzen <- bolzen %>%
  arrange(id, obs_counter)


# inspect dataset characteristics 
# length(table(bolzen$id))
# hist(table(bolzen$id))    
# table(survey$group)
# * 50 GI = actice control group / 50 DM = detached mindfulness
# table(esm$id, esm$phase)

# esm items
esm_items = c("gluecklich","froehlich", "entspannt",  "energiegeladen",
              "traurig",  "niedergeschlagen", "aengstlich", "nervoes",
              "veraergert", "reizbar", "posaff", "negaff", "rnt")

# summary(bolzen[,esm_items])


# Filter baseline data ########################################################

# take only baseline ESM survey
bolzen_baseline <- bolzen %>%
  filter(phase == "b")


# Add time grid for missing imputation ########################################

# Add missings to reconstruct measurement intervals

# WHAT WE SHOULD SEE: 
# There are rows missing in the data. We should correct for this by adding 
# missing data rows during estimation. 

# set factor levels 
bolzen$time_window = factor(bolzen$time_window, 
                            levels = c("mor", "mid", "eve"))
bolzen$timepoint = factor(bolzen$timepoint, 
                          levels = c("t0", "t1", "t2"))
bolzen$phase = factor(bolzen$phase, levels = c("b","i"))


# start with a dummy grid that we merge to each data: 
table(bolzen$id,bolzen$day_counter)
N_days = 10
N_windows = 3
N_tp = 3

# construct grid
dummy_grid = data.frame(
  "day_counter"       = rep(1:N_days, each = N_windows*N_tp),
  "time_window"       = rep(c("mor", "mid", "eve"), each = N_tp, times = N_days),
  "timepoint"         = rep(c("t0","t1","t2"), times = N_days*N_windows),
  "count"             = 1:(N_days*N_windows*N_tp)
)

# add additional space for overnight-lags and between time windows
dummy_grid$shift = ifelse(dummy_grid$time_window != lag(dummy_grid$time_window),4,1)
dummy_grid$shift = ifelse(dummy_grid$day_counter > lag(dummy_grid$day_counter), 10, dummy_grid$shift)
dummy_grid$shift[1] = 0
dummy_grid$int_time_spaced = cumsum(dummy_grid$shift)

# add new grid variables to the data 
bolzen_grid = merge(x = bolzen, y = dummy_grid, by = c("day_counter", "time_window", "timepoint"))

bolzen_grid = bolzen_grid %>% arrange(id, int_time_spaced)

# plot
# bolzen_grid %>%
#   filter(id == 1) %>%
#   ggplot(aes(x = int_time_spaced, y = rnt)) +
#   geom_point() +
#   geom_line()
# 
# bolzen_grid %>%
#   filter(id == 1) %>%
#   ggplot(aes(x = obs_counter, y = rnt)) +
#   geom_point() +
#   geom_line()


# create missings according to grid
bolzen_grid = create_missings(
  data = bolzen_grid, 
  tinterval = 1,
  id = "id",
  # choose one of: 
  # time = "int_time_spaced"   # this one for higher temporal resolution
  time = "count"
) %>%
  select(!int_time)

summary(bolzen_grid$phase_bi)
summary(bolzen_grid$group_bi)

# fill NAs for exogenous variables
bolzen_grid <- bolzen_grid %>%
  group_by(id) %>%
  mutate(
    group = unique(group[!is.na(group)]),
    pre_rtq_sum_cen = unique(pre_rtq_sum_cen[!is.na(pre_rtq_sum_cen)]),
    phase_bi = ifelse(day_counter<6, 0,1),
    group_bi = ifelse(any(group=="gi"),0,1)) %>%
  group_by(id) %>%
  fill(phase_bi, .direction = "downup") %>%
  ungroup()


# summary(bolzen_grid$phase_bi)
# summary(bolzen_grid$group_bi)


# Save files ##################################################################

# save
dir.create("downloaded_data")
save(bolzen, file = "./downloaded_data/bolzen.rda")
save(bolzen_grid, file = "./downloaded_data/bolzen_grid.rda")
save(bolzen_baseline, file = "downloaded_data/bolzen_baseline.rda")



