source(".../R/2_create_analysis_dataset.R")
library(survival)

### ENMO

fit0_enmo = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration),
  as.data.frame(analysis_df), x=TRUE
)

fit1_enmo = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration) + drug_count +
    as.factor(bmi) + as.factor(serious_cvd) + other_diseases + strata(as.factor(cancer)),
  as.data.frame(analysis_df), x=TRUE
)

### MAD MOD 

fit0_mad_mod = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_mad_mod, 4) + nsk(ig_mad_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration),
  as.data.frame(analysis_df), x=TRUE
)

fit1_mad_mod = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_mad_mod, 4) + nsk(ig_mad_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration) + drug_count +
    as.factor(bmi) + as.factor(serious_cvd) + other_diseases + strata(as.factor(cancer)),
  as.data.frame(analysis_df), x=TRUE
)

### COUNTS MOD 

fit0_counts_mod = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_counts_mod, 4) + nsk(ig_counts_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration),
  as.data.frame(analysis_df), x=TRUE
)

fit1_counts_mod = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_counts_mod, 4) + nsk(ig_counts_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration) + drug_count +
    as.factor(bmi) + as.factor(serious_cvd) + other_diseases + strata(as.factor(cancer)),
  as.data.frame(analysis_df), x=TRUE
)

### MIMS MOD 

fit0_mims_mod = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_mims_mod, 4) + nsk(ig_mims_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration),
  as.data.frame(analysis_df), x=TRUE
)

fit1_mims_mod = coxph(
  Surv(age_s11, age_s11 + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_mims_mod, 4) + nsk(ig_mims_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    as.factor(work_status) + as.factor(smoking) +
    as.factor(alcohol_frequency) + as.factor(sleep_duration) + drug_count +
    as.factor(bmi) + as.factor(serious_cvd) + other_diseases + strata(as.factor(cancer)),
  as.data.frame(analysis_df), x=TRUE
)




