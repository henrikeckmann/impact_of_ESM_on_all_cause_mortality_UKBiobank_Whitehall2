rm(list = ls())
source(".../R/2_create_analysis_dataset.R")

library(survival)
#library(ggplot2)
library(survminer)
library(lmtest)

## ENMO, MODEL 0

## Check sex, model 0, ENMO without interaction
fit0nosex = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ avgaccel_enmo + ig_enmo +
    season_spring + season_winter + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) + as.factor(alcohol_frequency) +
    as.factor(sleep_duration),
  analysis_df
)

fit0sex = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ avgaccel_enmo + ig_enmo + as.factor(sex) +
    season_spring + season_winter + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) + as.factor(alcohol_frequency) +
    as.factor(sleep_duration),
  analysis_df
)

fit0sexint = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ avgaccel_enmo + ig_enmo +
    as.factor(sex) + avgaccel_enmo:as.factor(sex) + ig_enmo:as.factor(sex) +
    season_spring + season_winter + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) + as.factor(alcohol_frequency) +
    as.factor(sleep_duration),
  analysis_df
)

lrtest(fit0nosex, fit0sex, fit0sexint)
rm(fit0nosex); rm(fit0sex); rm(fit0sexint)
# sex interaction not significant


## mutally adjusted non-linear model, no interaction
fit0 = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + season_spring + season_winter + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration),
  analysis_df
)

# test proportional hazards
test_ph = cox.zph(fit0)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# all non-significant, seasons both looking a little off visually

# test linearity
plot(analysis_df$avgaccel_enmo, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df$avgaccel_enmo, residuals(fit0, type = "martingale")), col = "red")

plot(analysis_df$ig_enmo, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df$ig_enmo, residuals(fit0, type = "martingale")), col = "red")

plot(analysis_df$season_spring, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df$season_spring, residuals(fit0, type = "martingale")), col = "red")

plot(analysis_df$season_winter, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df$season_winter, residuals(fit0, type = "martingale")), col = "red")
#all looking ok, still try non-lin seasons

# trying non-lin seasons
fit0 = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration),
  analysis_df
)

# test proportional hazards
test_ph = cox.zph(fit0)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# some improvement

# test linearity
plot(analysis_df$avgaccel_enmo, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df$avgaccel_enmo, residuals(fit0, type = "martingale")), col = "red")

plot(analysis_df$ig_enmo, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df$ig_enmo, residuals(fit0, type = "martingale")), col = "red")

plot(analysis_df$season_spring, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df$season_spring, residuals(fit0, type = "martingale")), col = "red")

plot(analysis_df$season_winter, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df$season_winter, residuals(fit0, type = "martingale")), col = "red")
#all ok

summary(fit0)
# avg_accel only significant in some intervals, not others, 
# IG significant

## Median split ENMO, model 0
## create median split variable
analysis_df = mutate(
  analysis_df,
  mediansplit_enmo = case_when(
    avgaccel_enmo <= median(avgaccel_enmo) & ig_enmo <= median(ig_enmo) ~ "aa. lo-lo",
    avgaccel_enmo <= median(avgaccel_enmo) & ig_enmo > median(ig_enmo) ~ "acc lo - ig hi",
    avgaccel_enmo > median(avgaccel_enmo) & ig_enmo <= median(ig_enmo) ~ "acc hi - ig lo",
    avgaccel_enmo > median(avgaccel_enmo) & ig_enmo > median(ig_enmo) ~ "hi-hi",
  )
)

## create model, like model 0
fit0_split = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    as.factor(mediansplit_enmo) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
     as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration),
  analysis_df
)

# test proportional hazards
test_ph = cox.zph(fit0_split)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# all good

# test linearity
plot(analysis_df$season_spring, residuals(fit0_split, type = "martingale"))
lines(smooth.spline(analysis_df$season_spring, residuals(fit0_split, type = "martingale")), col = "red")

plot(analysis_df$season_winter, residuals(fit0_split, type = "martingale"))
lines(smooth.spline(analysis_df$season_winter, residuals(fit0_split, type = "martingale")), col = "red")
#all ok

summary(fit0_split)
# hi-hi best but only slightly better than lo-hi, hi-lo not significant

# ENMO, MODEL 1

fit1 = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration) + drug_count +
    as.factor(bmi) + as.factor(serious_cvd) + other_diseases + as.factor(cancer),
  analysis_df
)

# test proportional hazards
test_ph = cox.zph(fit1)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# cancer fails, so use to stratisfy baseline hazards, season spring is close but ok

fit1 = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration) + drug_count +
    as.factor(bmi) + as.factor(serious_cvd) + other_diseases + strata(as.factor(cancer)),
  analysis_df
)

test_ph = cox.zph(fit1)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# all good, also improves season spring

# test linearity

plot(analysis_df$avgaccel_enmo, residuals(fit1, type = "martingale"))
lines(smooth.spline(analysis_df$avgaccel_enmo, residuals(fit1, type = "martingale")), col = "red")

plot(analysis_df$ig_enmo, residuals(fit1, type = "martingale"))
lines(smooth.spline(analysis_df$ig_enmo, residuals(fit1, type = "martingale")), col = "red")

plot(analysis_df$season_spring, residuals(fit1, type = "martingale"))
lines(smooth.spline(analysis_df$season_spring, residuals(fit1, type = "martingale")), col = "red")

plot(analysis_df$season_winter, residuals(fit1, type = "martingale"))
lines(smooth.spline(analysis_df$season_winter, residuals(fit1, type = "martingale")), col = "red")

plot(analysis_df$drug_count, residuals(fit1, type = "martingale"))
lines(smooth.spline(analysis_df$drug_count, residuals(fit1, type = "martingale")), col = "red")

plot(analysis_df$other_diseases, residuals(fit1, type = "martingale"))
lines(smooth.spline(analysis_df$other_diseases, residuals(fit1, type = "martingale")), col = "red")
#all good

summary(fit1)
# avg_accel not significant, ig significant


## Median split ENMO model 1
# create model
fit1_split = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    as.factor(mediansplit_enmo) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration) + drug_count +
    as.factor(bmi) + as.factor(serious_cvd) + other_diseases + strata(as.factor(cancer)),
  analysis_df
)

# test proportional hazards
test_ph = cox.zph(fit1_split)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# all good

# test linearity
plot(analysis_df$season_spring, residuals(fit1_split, type = "martingale"))
lines(smooth.spline(analysis_df$season_spring, residuals(fit1_split, type = "martingale")), col = "red")

plot(analysis_df$season_winter, residuals(fit1_split, type = "martingale"))
lines(smooth.spline(analysis_df$season_winter, residuals(fit1_split, type = "martingale")), col = "red")

plot(analysis_df$drug_count, residuals(fit1_split, type = "martingale"))
lines(smooth.spline(analysis_df$drug_count, residuals(fit1_split, type = "martingale")), col = "red")

plot(analysis_df$other_diseases, residuals(fit1_split, type = "martingale"))
lines(smooth.spline(analysis_df$other_diseases, residuals(fit1_split, type = "martingale")), col = "red")
#all good

summary(fit1_split)
# only hi-hi significant