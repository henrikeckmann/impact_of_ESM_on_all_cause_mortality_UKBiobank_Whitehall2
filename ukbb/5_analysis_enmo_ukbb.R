rm(list = ls())
source(".../R/UKBB/2_create_analysis_dataset_ukbb.R")

library(survival)
#library(ggplot2)
library(survminer)
library(lmtest)

## ENMO, MODEL 0

## Check sex, model 0, ENMO without interaction
fit0nosex = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ avgaccel_enmo + ig_enmo +
    nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(smoking) + as.factor(sleep_duration) + as.factor(work_status) +
    as.factor(alcohol_frequency),
  analysis_df
)

fit0sex = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ avgaccel_enmo + ig_enmo + as.factor(sex) +
    nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(smoking) + as.factor(sleep_duration) + as.factor(work_status) +
    as.factor(alcohol_frequency),
  analysis_df
)

fit0sexint = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ avgaccel_enmo + ig_enmo +
    as.factor(sex) + avgaccel_enmo:as.factor(sex) + ig_enmo:as.factor(sex) +
    nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(smoking) + as.factor(sleep_duration) + as.factor(work_status) +
    as.factor(alcohol_frequency),
  analysis_df
)

lrtest(fit0nosex, fit0sex, fit0sexint)
rm(fit0nosex); rm(fit0sex); rm(fit0sexint)
# no interaction


## mutally adjusted non-linear model, no interaction
fit0 = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(smoking) + as.factor(sleep_duration) + as.factor(work_status) +
    as.factor(alcohol_frequency),
  analysis_df
)

# test proportional hazards
test_ph = cox.zph(fit0)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# AVG_ACCEL FAILS
# smoking and work status non-proportional. stratify baseline hazards? visual fine

# stratify by smoking, work status
fit0 = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency),
  analysis_df,
)

# test proportional hazards
test_ph = cox.zph(fit0)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# AVG_ACCEL STILL FAILS, otherwhise fine

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
# vol and IG both significant

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
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    as.factor(mediansplit_enmo) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency),
  analysis_df
)

# test proportional hazards
test_ph = cox.zph(fit0_split)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# works.

# test linearity
plot(analysis_df$season_spring, residuals(fit0_split, type = "martingale"))
lines(smooth.spline(analysis_df$season_spring, residuals(fit0_split, type = "martingale")), col = "red")

plot(analysis_df$season_winter, residuals(fit0_split, type = "martingale"))
lines(smooth.spline(analysis_df$season_winter, residuals(fit0_split, type = "martingale")), col = "red")
#all ok

summary(fit0_split)
# hi-hi unsurprisingly best, but hi-lo surprisingly second, then lo-hi


# ENMO, MODEL 1

fit1 = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency) + drug_count + as.factor(bmi) + 
    as.factor(serious_cvd) + other_diseases + as.factor(cancer) +
    as.factor(lsdis),
  analysis_df,
)

# test proportional hazards
test_ph = cox.zph(fit1)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# AVG_ACCEL FAILS
#other_diseases, cancer, lsdis also fails

# stratify by other diseases, cancer and lsdis

fit1 = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency) + drug_count + as.factor(bmi) + 
    as.factor(serious_cvd) + strata(other_diseases) + strata(as.factor(cancer)) +
    strata(as.factor(lsdis)),
  analysis_df,
)

# test proportional hazards
test_ph = cox.zph(fit1)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# AVG_ACCEL STILL FAILS, otherwise fine

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
#drug count slightly off at extreme high end, but ok

summary(fit1)
# avg_accel, ig both significant

## Median split ENMO model 1
# create model
fit1_split = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    as.factor(mediansplit_enmo) +
    strata(as.factor(sex)) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency) + drug_count + as.factor(bmi) + 
    as.factor(serious_cvd) + strata(other_diseases) + strata(as.factor(cancer)) +
    strata(as.factor(lsdis)),
  analysis_df,
)

# test proportional hazards
test_ph = cox.zph(fit1_split)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# graphing not working correctly, but tests all good

# test linearity
plot(analysis_df$season_spring, residuals(fit1_split, type = "martingale"))
lines(smooth.spline(analysis_df$season_spring, residuals(fit1_split, type = "martingale")), col = "red")

plot(analysis_df$season_winter, residuals(fit1_split, type = "martingale"))
lines(smooth.spline(analysis_df$season_winter, residuals(fit1_split, type = "martingale")), col = "red")

plot(analysis_df$drug_count, residuals(fit1_split, type = "martingale"))
lines(smooth.spline(analysis_df$drug_count, residuals(fit1_split, type = "martingale")), col = "red")
# same as model 1 cont. all ok

summary(fit1_split)
# hi-hi best, then hi-lo, then lo-hi

### OVER 60

# mutually adjusted, no interaction, stratify by smoking and work_status
fit0 = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency),
  analysis_df_o60,
)

# test proportional hazards
test_ph = cox.zph(fit0)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# AVG_ACCEL STILL FAILS

# test linearity
plot(analysis_df_o60$avgaccel_enmo, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df_o60$avgaccel_enmo, residuals(fit0, type = "martingale")), col = "red")

plot(analysis_df_o60$ig_enmo, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df_o60$ig_enmo, residuals(fit0, type = "martingale")), col = "red")

plot(analysis_df_o60$season_spring, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df_o60$season_spring, residuals(fit0, type = "martingale")), col = "red")

plot(analysis_df_o60$season_winter, residuals(fit0, type = "martingale"))
lines(smooth.spline(analysis_df_o60$season_winter, residuals(fit0, type = "martingale")), col = "red")
#all ok

summary(fit0)
# vol and IG both significant

## Median split ENMO, model 0
## create median split variable
analysis_df_o60 = mutate(
  analysis_df_o60,
  mediansplit_enmo = case_when(
    avgaccel_enmo <= median(avgaccel_enmo) & ig_enmo <= median(ig_enmo) ~ "aa. lo-lo",
    avgaccel_enmo <= median(avgaccel_enmo) & ig_enmo > median(ig_enmo) ~ "acc lo - ig hi",
    avgaccel_enmo > median(avgaccel_enmo) & ig_enmo <= median(ig_enmo) ~ "acc hi - ig lo",
    avgaccel_enmo > median(avgaccel_enmo) & ig_enmo > median(ig_enmo) ~ "hi-hi",
  )
)

## create model, like model 0
fit0_split = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    as.factor(mediansplit_enmo) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency),
  analysis_df_o60
)

# test proportional hazards
test_ph = cox.zph(fit0_split)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# works

# test linearity
plot(analysis_df_o60$season_spring, residuals(fit0_split, type = "martingale"))
lines(smooth.spline(analysis_df_o60$season_spring, residuals(fit0_split, type = "martingale")), col = "red")

plot(analysis_df_o60$season_winter, residuals(fit0_split, type = "martingale"))
lines(smooth.spline(analysis_df_o60$season_winter, residuals(fit0_split, type = "martingale")), col = "red")
#all ok

summary(fit0_split)
# similar to full data set:
# hi-hi unsurprisingly best, but hi-lo surprisingly second, then lo-hi.
# lo-hi closer to hi-lo here tho

# ENMO, MODEL 1

fit1 = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency) + drug_count + as.factor(bmi) + 
    as.factor(serious_cvd) + strata(other_diseases) + strata(as.factor(cancer)) +
    strata(as.factor(lsdis)),
  analysis_df_o60,
)

# test proportional hazards
test_ph = cox.zph(fit1)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# AVG_ACCEL STILL FAILS

# test linearity
plot(analysis_df_o60$avgaccel_enmo, residuals(fit1, type = "martingale"))
lines(smooth.spline(analysis_df_o60$avgaccel_enmo, residuals(fit1, type = "martingale")), col = "red")

plot(analysis_df_o60$ig_enmo, residuals(fit1, type = "martingale"))
lines(smooth.spline(analysis_df_o60$ig_enmo, residuals(fit1, type = "martingale")), col = "red")

plot(analysis_df_o60$season_spring, residuals(fit1, type = "martingale"))
lines(smooth.spline(analysis_df_o60$season_spring, residuals(fit1, type = "martingale")), col = "red")

plot(analysis_df_o60$season_winter, residuals(fit1, type = "martingale"))
lines(smooth.spline(analysis_df_o60$season_winter, residuals(fit1, type = "martingale")), col = "red")

plot(analysis_df_o60$drug_count, residuals(fit1, type = "martingale"))
lines(smooth.spline(analysis_df_o60$drug_count, residuals(fit1, type = "martingale")), col = "red")
# all ok


summary(fit1)
# avg_accel, ig both significant

## Median split ENMO model 1
# create model
fit1_split = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    as.factor(mediansplit_enmo) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency) + drug_count + as.factor(bmi) + 
    as.factor(serious_cvd) + strata(other_diseases) + strata(as.factor(cancer)) +
    strata(as.factor(lsdis)),
  analysis_df_o60
)

# test proportional hazards
test_ph = cox.zph(fit1_split)
test_ph
ggcoxzph(test_ph, ggtheme = theme_bw())
# all good

# test linearity
plot(analysis_df_o60$season_spring, residuals(fit1_split, type = "martingale"))
lines(smooth.spline(analysis_df_o60$season_spring, residuals(fit1_split, type = "martingale")), col = "red")

plot(analysis_df_o60$season_winter, residuals(fit1_split, type = "martingale"))
lines(smooth.spline(analysis_df_o60$season_winter, residuals(fit1_split, type = "martingale")), col = "red")

plot(analysis_df_o60$drug_count, residuals(fit1_split, type = "martingale"))
lines(smooth.spline(analysis_df_o60$drug_count, residuals(fit1_split, type = "martingale")), col = "red")
# same as model 1 cont. all ok

summary(fit1_split)
# similar to full dataset