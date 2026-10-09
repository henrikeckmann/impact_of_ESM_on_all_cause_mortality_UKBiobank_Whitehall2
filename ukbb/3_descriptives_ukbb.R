library(gtsummary)
library(flextable)
source(".../R/UKBB/2_create_analysis_dataset_ukbb.R")

### Descriptives

descriptives_df$ft_totaldummy = descriptives_df$follow_up_time
descriptives_df$age_death = descriptives_df$age + descriptives_df$follow_up_time

descriptives_df = mutate(
  descriptives_df,
  age_death = case_when(
    mort_allcause == 1 ~ age_death,
    mort_allcause != 1 ~ NA
  )
)

descr_table = add_overall(
  tbl_summary(
    descriptives_df,
    by = sex,
    include = c(
      age,
      ethnicity,
      #marital_status,
      work_status,
      smoking,
      alcohol_frequency,
      sleep_duration,
      drug_count,
      bmi,
      lsdis,
      serious_cvd,
      cancer,
      other_diseases,
      follow_up_time,
      ft_totaldummy,
      mort_allcause,
      mort_cvd,
      mort_cancer,
      mort_resp,
      age_death,
      calib_err,
      valid_days,
      avgaccel_enmo,
      avgaccel_mad_mod,
      avgaccel_mims_mod,
      avgaccel_counts_mod,
      ig_enmo,
      ig_mad_mod,
      ig_mims_mod,
      ig_counts_mod,
    ),
    statistic = ft_totaldummy ~ "{sum}",
    type = list(
      valid_days ~ "continuous",
      other_diseases ~ "continuous",
      drug_count ~ "continuous"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1),
      starts_with("ig_") ~ 3,
      avgaccel_mims_mod = 2,
      valid_days = 0,
      calib_err = 3,
      drug_count = 0,
      other_diseases = 0,
      age = 1,
      age_death = 1,
      ft_totaldummy = 0
    ),
    label = list(
      age ~ "Age",
      ethnicity ~ "Ethnicity",
      #marital_status ~ "Cohabiting/single",
      work_status ~ "Employment status",
      smoking ~ "Smoking",
      alcohol_frequency ~ "Alcohol frequency",
      sleep_duration ~ "Avg. sleep duration",
      drug_count ~ "No. of prescribed medicines",
      bmi ~ "BMI classification",
      lsdis ~ "Long-standing illness or disease",
      serious_cvd ~ "Prevalent CVD",
      cancer ~ "Prevalent cancer",
      other_diseases ~ "No. of other diseases",
      follow_up_time ~ "Follow-up time, years",
      ft_totaldummy ~ "Follow-up time, total person-years",
      mort_allcause ~ "Deaths",
      mort_cvd ~ "CVD deaths",
      mort_cancer ~ "Cancer deaths",
      mort_resp ~ "Respiratory deaths",
      age_death ~ "Age at death",
      calib_err ~ "Calibration error, mg",
      valid_days ~ "Valid accelerometer wear days"
    )
  )
)

write_csv(
  as.data.frame(descr_table),
  ""
)


descriptives_df_o60$ft_totaldummy = descriptives_df_o60$follow_up_time
descriptives_df_o60$age_death = descriptives_df_o60$age + descriptives_df_o60$follow_up_time

descriptives_df_o60 = mutate(
  descriptives_df_o60,
  age_death = case_when(
    mort_allcause == 1 ~ age_death,
    mort_allcause != 1 ~ NA
  )
)

descr_table_o60 = add_overall(
  tbl_summary(
    descriptives_df_o60,
    by = sex,
    include = c(
      age,
      ethnicity,
      #marital_status,
      work_status,
      smoking,
      alcohol_frequency,
      sleep_duration,
      drug_count,
      bmi,
      lsdis,
      serious_cvd,
      cancer,
      other_diseases,
      follow_up_time,
      ft_totaldummy,
      mort_allcause,
      mort_cvd,
      mort_cancer,
      mort_resp,
      age_death,
      calib_err,
      valid_days,
      avgaccel_enmo,
      avgaccel_mad_mod,
      avgaccel_mims_mod,
      avgaccel_counts_mod,
      ig_enmo,
      ig_mad_mod,
      ig_mims_mod,
      ig_counts_mod,
    ),
    statistic = ft_totaldummy ~ "{sum}",
    type = list(
      valid_days ~ "continuous",
      other_diseases ~ "continuous",
      drug_count ~ "continuous"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1),
      starts_with("ig_") ~ 3,
      avgaccel_mims_mod = 2,
      valid_days = 0,
      calib_err = 3,
      drug_count = 0,
      other_diseases = 0,
      age = 1,
      age_death = 1,
      ft_totaldummy = 0
    ),
    label = list(
      age ~ "Age",
      ethnicity ~ "Ethnicity",
      #marital_status ~ "Cohabiting/single",
      work_status ~ "Employment status",
      smoking ~ "Smoking",
      alcohol_frequency ~ "Alcohol frequency",
      sleep_duration ~ "Avg. sleep duration",
      drug_count ~ "No. of prescribed medicines",
      bmi ~ "BMI classification",
      lsdis ~ "Long-standing illness or disease",
      serious_cvd ~ "Prevalent CVD",
      cancer ~ "Prevalent cancer",
      other_diseases ~ "No. of other diseases",
      follow_up_time ~ "Follow-up time, years",
      ft_totaldummy ~ "Follow-up time, total person-years",
      mort_allcause ~ "Deaths",
      mort_cvd ~ "CVD deaths",
      mort_cancer ~ "Cancer deaths",
      mort_resp ~ "Respiratory deaths",
      age_death ~ "Age at death",
      calib_err ~ "Calibration error, mg",
      valid_days ~ "Valid accelerometer wear days"
    )
  )
)


write_csv(
  as.data.frame(descr_table_o60),
  ""
)

