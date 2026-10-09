source(".../R/UKBB/2_create_analysis_dataset_ukbb.R")
library(survival)

### ENMO

fit0_enmo = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency),
  as.data.frame(analysis_df_o60), x = TRUE,
)

fit1_enmo = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_enmo, 4) + nsk(ig_enmo, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency) + drug_count + as.factor(bmi) + 
    as.factor(serious_cvd) + strata(other_diseases) + strata(as.factor(cancer)) +
    strata(as.factor(lsdis)),
  as.data.frame(analysis_df_o60), x = TRUE,
)

### MAD MOD 

fit0_mad_mod = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_mad_mod, 4) + nsk(ig_mad_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency),
  as.data.frame(analysis_df_o60), x = TRUE,
)

fit1_mad_mod = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_mad_mod, 4) + nsk(ig_mad_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency) + drug_count + as.factor(bmi) + 
    as.factor(serious_cvd) + strata(other_diseases) + strata(as.factor(cancer)) +
    strata(as.factor(lsdis)),
  as.data.frame(analysis_df_o60), x = TRUE,
)

### COUNTS MOD 

fit0_counts_mod = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_counts_mod, 4) + nsk(ig_counts_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency),
  as.data.frame(analysis_df_o60), x = TRUE,
)

fit1_counts_mod = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_counts_mod, 4) + nsk(ig_counts_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency) + drug_count + as.factor(bmi) + 
    as.factor(serious_cvd) + strata(other_diseases) + strata(as.factor(cancer)) +
    strata(as.factor(lsdis)),
  as.data.frame(analysis_df_o60), x = TRUE,
)

### MIMS MOD 

fit0_mims_mod = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_mims_mod, 4) + nsk(ig_mims_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency),
  as.data.frame(analysis_df_o60), x = TRUE,
)

fit1_mims_mod = coxph(
  Surv(age, age + follow_up_time, mort_allcause) ~ 
    nsk(avgaccel_mims_mod, 4) + nsk(ig_mims_mod, 4) +
    as.factor(sex) + nsk(season_spring, 2) + nsk(season_winter, 2) + as.factor(ethnicity) +
    strata(as.factor(smoking)) + as.factor(sleep_duration) + strata(as.factor(work_status)) +
    as.factor(alcohol_frequency) + drug_count + as.factor(bmi) + 
    as.factor(serious_cvd) + strata(other_diseases) + strata(as.factor(cancer)) +
    strata(as.factor(lsdis)),
  as.data.frame(analysis_df_o60), x = TRUE,
)
