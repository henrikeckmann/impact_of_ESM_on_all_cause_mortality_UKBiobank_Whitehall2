# rm(list=ls())

library(tidyverse)
library(haven)
library(labelled)
library(lubridate)

### Import accelerometer QC and outcome data
acc_path = ""

# import enmo outcomes and qc variables
acc_participant_df = rename(
  select(
    mutate(
      read_csv(paste0("")),
      valid_days = rowSums(across(c("N valid WEdays", "N valid WKdays"))),
    ),
    filename,
    complete_24hcycle,
    calib_err,
    valid_days,
  ),
)

enmo_df = summarise(
  group_by(
    rename(
      drop_na(
        select(
          read_csv(paste0(acc_path, "part2_daysummary_25mg.csv")),
          filename,
          "mean_ENMO_mg_0-24hr",
          "ig_gradient_ENMO_0-24hr",
          "ig_rsquared_ENMO_0-24hr"
        )
      ),
      avgaccel_enmo = "mean_ENMO_mg_0-24hr",
      ig_enmo = "ig_gradient_ENMO_0-24hr",
      rsq_enmo = "ig_rsquared_ENMO_0-24hr",
    ),
    filename
  ),
  avgaccel_enmo = mean(avgaccel_enmo),
  ig_enmo = mean(ig_enmo),
  rsq_enmo = mean(rsq_enmo),
)
acc_df = merge(acc_participant_df, enmo_df, by = "filename", all.x = TRUE)
rm(acc_participant_df)
rm(enmo_df)

# import mad modelled outcomes
mad_mod_df = summarise(
  group_by(
    rename(
      drop_na(
        select(
          read_csv(paste0(acc_path, "part2_daysummary_mad_mod.csv")),
          filename,
          "mean_MAD_mg_0-24hr",
          "ig_gradient_MAD_0-24hr",
          "ig_rsquared_MAD_0-24hr"
        )
      ),
      avgaccel_mad_mod = "mean_MAD_mg_0-24hr",
      ig_mad_mod = "ig_gradient_MAD_0-24hr",
      rsq_mad_mod = "ig_rsquared_MAD_0-24hr",
    ),
    filename
  ),
  avgaccel_mad_mod = mean(avgaccel_mad_mod),
  ig_mad_mod = mean(ig_mad_mod),
  rsq_mad_mod = mean(rsq_mad_mod),
)
acc_df = merge(acc_df, mad_mod_df, by = "filename", all.x = TRUE)
rm(mad_mod_df)

# import mims modelled outcomes
mims_mod_df = summarise(
  group_by(
    rename(
      drop_na(
        select(
          read_csv(paste0(acc_path, "part2_daysummary_mims_mod.csv")),
          filename,
          "mean_LFENMO_mg_0-24hr",
          "ig_gradient_LFENMO_0-24hr",
          "ig_rsquared_LFENMO_0-24hr"
        )
      ),
      avgaccel_mims_mod = "mean_LFENMO_mg_0-24hr",
      ig_mims_mod = "ig_gradient_LFENMO_0-24hr",
      rsq_mims_mod = "ig_rsquared_LFENMO_0-24hr",
    ),
    filename
  ),
  avgaccel_mims_mod = mean(avgaccel_mims_mod) / 1000,
  ig_mims_mod = mean(ig_mims_mod),
  rsq_mims_mod = mean(rsq_mims_mod),
)
acc_df = merge(acc_df, mims_mod_df, by = "filename", all.x = TRUE)
rm(mims_mod_df)

# import counts modelled outcomes
counts_mod_df = summarise(
  group_by(
    rename(
      drop_na(
        select(
          read_csv(paste0(acc_path, "part2_daysummary_counts_mod.csv")),
          filename,
          "mean_NeishabouriCount_vm_mg_0-24hr",
          "ig_gradient_NeishabouriCount_vm_0-24hr",
          "ig_rsquared_NeishabouriCount_vm_0-24hr"
        )
      ),
      avgaccel_counts_mod = "mean_NeishabouriCount_vm_mg_0-24hr",
      ig_counts_mod = "ig_gradient_NeishabouriCount_vm_0-24hr",
      rsq_counts_mod = "ig_rsquared_NeishabouriCount_vm_0-24hr",
    ),
    filename
  ),
  avgaccel_counts_mod = mean(avgaccel_counts_mod),
  ig_counts_mod = mean(ig_counts_mod),
  rsq_counts_mod = mean(rsq_counts_mod),
)
acc_df = merge(acc_df, counts_mod_df, by = "filename", all.x = TRUE)
rm(counts_mod_df)

# change filename column to match covariate files and add season of wear
acc_df = rename(
  mutate(
    acc_df,
    filename = str_extract(substr(filename, 1, 6), "[0-9]+"),
  ),
  stno = filename,
)



### Covariates
covariate_path = ""

# import age
age_df = read_dta(paste0(covariate_path, "age.dta"))
covariate_df = zap_formats(
  select(
    rename(
      age_df,
      age_s11 = fage_si,
    ),
    stno,
    age_s11
  )
)
rm(age_df)

# import ethnicity
ethnicity_df = zap_formats(
  mutate(
    rename(
      read_dta(paste0(covariate_path, "ethnicity.dta")),
      ethnicity = ethn_ds,
    ),
    ethnicity = case_when(
      ethnicity == 1 ~ "aa. white",
      ethnicity == 2 ~ "non-white"
    )
  )
)
covariate_df = merge(covariate_df, ethnicity_df, by = "stno")
rm(ethnicity_df)

# import sex
sex_df = zap_formats(
    mutate(
      read_dta(paste0(covariate_path, "sex.dta")),
      sex = case_when(
        sex == 1 ~ "aa. male",
        sex == 2 ~ "female"
      )
    )
)
attributes(sex_df$sex) = NULL
covariate_df = merge(covariate_df, sex_df, by = "stno")
rm(sex_df)

# import marital status
maritalstatus_df = zap_formats(
    mutate(
      rename(
        read_dta(paste0(covariate_path, "maritalstatus_imputed.dta")),
        marital_status = fstatusx_i,
      ),
      marital_status = case_when(
        marital_status == 1 ~ "aa. cohabit",
        marital_status != 1 ~ "single"
      )
    )
)
attributes(maritalstatus_df$marital_status) = NULL
covariate_df = merge(covariate_df, maritalstatus_df, by = "stno")
rm(maritalstatus_df)

# import work status
workstatus_df = zap_formats(
    mutate(
      rename(
      read_dta(paste0(covariate_path, "work_status.dta")),
      work_status = flabstat,
      ),
      work_status = case_when(
        work_status == 1 | work_status == 2 ~ "working",
        work_status != 1 & work_status != 2 ~ "aa. not working"
      )
    )
)
attributes(workstatus_df$work_status) = NULL
covariate_df = merge(covariate_df, workstatus_df, by = "stno")
rm(workstatus_df)

# import healthbehaviours
# not including alcohol units, only frequency
healthbehaviours_df = zap_formats(
    mutate(
      select(
        read_dta(paste0(covariate_path, "healthbehaviors_variables.dta")),
        stno,
        smoking,
      ),
      smoking = case_when(
        smoking == 1 ~ "aa. never",
        smoking == 2 ~ "ex",
        smoking == 3 ~ "current"
      )
    )
)
attributes(healthbehaviours_df$smoking) = NULL
covariate_df = merge(covariate_df, healthbehaviours_df, by = "stno")
rm(healthbehaviours_df)

# import alcohol frequency
alcohol_frequency_df = zap_formats(
    mutate(
      rename(
        select(
          read_dta(paste0(covariate_path, "alcohol_freq.dta")),
          stno,
          fdrnkf,
        ),
        alcohol_frequency = fdrnkf,
      ),
      alcohol_frequency = case_when(
        alcohol_frequency == 0 ~ "never",
        alcohol_frequency == 1 ~ "<1/mo",
        alcohol_frequency == 2 ~ "2-4/mo",
        alcohol_frequency == 3 ~ "2-3/w",
        alcohol_frequency == 4 ~ "<<aa. 4+/w",
      )
    )
)
attributes(alcohol_frequency_df$alcohol_frequency) = NULL
covariate_df = merge(covariate_df, alcohol_frequency_df, by = "stno")
rm(alcohol_frequency_df)

# import sleepduration and drugs
sleepdur_drugs_df = zap_formats(
    rename(
      select(
        mutate(
          read_dta(paste0(covariate_path, "sleepdur_drgs.dta")),
          across(2:16, ~.x != ""),
          drug_count = rowSums(across(2:16)),
          fsleep = cut(fsleep, c(0, 2.5, 4.5, 6), labels=FALSE),
          fsleep = case_when(
            fsleep == 1 ~ "under",
            fsleep == 2 ~ "aa. normal",
            fsleep == 3 ~ "over"
          )
        ),
        stno,
        fsleep,
        drug_count
      ),
      sleep_duration = fsleep,
    )
)
attributes(sleepdur_drugs_df$sleep_duration) = NULL
covariate_df = merge(covariate_df, sleepdur_drugs_df, by = "stno")
rm(sleepdur_drugs_df)

# import bmi
bmi_df = zap_formats(
    mutate(
      rename(
        read_dta(paste0(covariate_path, "BMI.dta")),
        bmi = fbmi_i,
      ),
      bmi = as.numeric(cut(bmi, c(0, 18.5, 25.0, 30.0, 100.0), right=FALSE, labels=FALSE)),
      bmi = case_when(
        bmi == 1 ~ "under",
        bmi == 2 ~ "aa. normal",
        bmi == 3 ~ "over",
        bmi == 4 ~ "obese"
      )
    )
)
covariate_df = merge(covariate_df, bmi_df, by = "stno")
rm(bmi_df)

# import chronic diseases
disease_df = zap_formats(
    select(
      mutate( # derive DEMENTIA_p, serious_cvd, cancer, other_diseases
        merge( # combine demencefinal file and chronic disease for dementia
          select(
              read_dta(paste0(covariate_path, "demence0323_final.dta")),
              stno,
              date_demence,
            ),
            read_dta(paste0(covariate_path, "chronic_disease.dta")),
            by = "stno"
        ),
        dementia_pre_screen = date_demence < fdatscrn,
        dementia_pre_screen = as.numeric(replace_na(dementia_pre_screen, FALSE)),
        DEMENTIA_p = dementia_pre_screen + demence_prev,
        DEMENTIA_p = as.numeric(DEMENTIA_p > 0),
        serious_cvd = as.numeric(rowSums(across(c(CHD_p, STROKE_p, HF_p))) > 0),
        cancer = CANCER_p,
        other_diseases = rowSums(across(c(
          PARK_p, COPD_p, DEPRESSION_p, MD_p, LIVER_p,
          ARTH_p, HYPTEN_p, DIABETES_p, DEMENTIA_p
        ))),
      ),
      stno,
      serious_cvd,
      cancer,
      other_diseases,
    )
)
covariate_df = merge(covariate_df, disease_df, by = "stno")
rm(disease_df)


### Mortality
# import mortality
mortality_df = zap_formats(
  select(
    mutate(
      read_dta(paste0(covariate_path, "mort0323.dta")),
      mort_allcause = mort23ac,
      mort_unknowncause = unkcaus,
      mort_cvd = mort23cvd,
      mort_cancer = as.numeric(
        rowSums(across(c(mort23neo, mort23pros, mort23neod, mort23neoi))) > 0
      ),
      mort_other = as.numeric(
        rowSums(across(c(mort23resp, mort23othd))) > 0
      ),
      date_screening = as.POSIXct(floor_date(FPHDATE, "months")),
      date_event = as.POSIXct(floor_date(exp_fup0923)),
      follow_up_time = as.numeric(date_event - date_screening) / (60^2*24*365.25),
      season_spring = sin(2 * pi * yday(date_screening) / 365.25),
      season_winter = cos(2 * pi * yday(date_screening) / 365.25),
    ),
    stno,
    mort_allcause,
    mort_unknowncause,
    mort_cvd,
    mort_cancer,
    mort_other,
    date_screening,
    date_event,
    follow_up_time,
    season_spring,
    season_winter
  )
)
rm(covariate_path)

### Analysis dataset
analysis_df = relocate(
  merge(
    acc_df, merge(covariate_df, mortality_df, by = "stno"), by = "stno"
  ),
  c(season_spring, season_winter, follow_up_time), .before = age_s11,
)
rm(covariate_df)
rm(mortality_df)
rm(acc_df)

write_csv(analysis_df, "")


rows_before_exclusions = nrow(analysis_df)

### Exclusions
exc_calib = nrow(analysis_df) - nrow(filter(analysis_df, calib_err <= 0.01))
analysis_df = filter(
  analysis_df,
  calib_err <= 0.01,
)

exc_24hcycle = nrow(analysis_df) - nrow(filter(analysis_df, complete_24hcycle == 1))
analysis_df = filter(
  analysis_df,
  complete_24hcycle == 1,
)

exc_valid_days = nrow(analysis_df) - nrow(filter(analysis_df, valid_days >= 3))
analysis_df = filter(
  analysis_df,
  valid_days >= 3,
)

exc_unrealistic = nrow(analysis_df) - nrow(filter(analysis_df, avgaccel_enmo < 100))
analysis_df = filter(
  analysis_df,
  avgaccel_enmo < 100,
)

exc_covariates = nrow(analysis_df) - nrow(drop_na(analysis_df))
analysis_df = drop_na(analysis_df)

# outliers, trim 0.5, 99.5 centile
exc_outliers = nrow(analysis_df) - nrow(
  filter(
    analysis_df,
    avgaccel_enmo < quantile(avgaccel_enmo, .995) & 
      avgaccel_enmo > quantile(avgaccel_enmo, .005) &
      ig_enmo < quantile(ig_enmo, .995) &
      ig_enmo > quantile(ig_enmo, .005)
  )
)
analysis_df = filter(
  analysis_df,
  avgaccel_enmo < quantile(avgaccel_enmo, .995) & 
    avgaccel_enmo > quantile(avgaccel_enmo, .005) &
    ig_enmo < quantile(ig_enmo, .995) &
    ig_enmo > quantile(ig_enmo, .005)
)



# standardise exposures
descriptives_df = analysis_df

analysis_df =  mutate(
  analysis_df,
  across(
    c(
      avgaccel_enmo, 
      ig_enmo,
      avgaccel_mad_mod,
      ig_mad_mod,
      avgaccel_mims_mod,
      ig_mims_mod,
      avgaccel_counts_mod,
      ig_counts_mod,
    ),
    ~ scale(.)[,1]
  )
)
