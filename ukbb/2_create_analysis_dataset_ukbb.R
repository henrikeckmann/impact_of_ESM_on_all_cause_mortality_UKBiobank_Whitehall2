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
      read_csv(paste0(acc_path, "enmo_participant_combined.csv")),
      valid_days = rowSums(across(c("N valid WEdays", "N valid WKdays"))),
      season_spring = sin(2 * pi * yday(start_time) / 365.25),
      season_winter = cos(2 * pi * yday(start_time) / 365.25),
    ),
    filename,
    start_time,
    complete_24hcycle,
    calib_err,
    valid_days,
    season_spring,
    season_winter,
  ),
)

enmo_df = summarise(
  group_by(
    rename(
      drop_na(
        select(
          read_csv(paste0(acc_path, "enmo_day_combined.csv")),
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
          read_csv(paste0(acc_path, "mad_mod_day_combined.csv")),
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
          read_csv(paste0(acc_path, "mims_mod_day_combined.csv")),
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
          read_csv(paste0(acc_path, "counts_mod_day_combined.csv")),
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

# filter out duplicate measurements (i.e. measurements 2-4 for participants with multiple)
rows_before_exclusions = nrow(acc_df)
exc_duplicate = nrow(acc_df) - nrow(
  filter(
    acc_df,
    is.na(str_extract(filename, '_0_0.cwa')) == FALSE
  )
)
acc_df = filter(
  acc_df,
  is.na(str_extract(filename, '_0_0.cwa')) == FALSE
)
  
# change filename column to match covariate files and add season of wear
acc_df = rename(
  mutate(
    acc_df,
    filename = substr(filename, 1, 7),
  ),
  stno = filename,
)


### Covariates
covariate_path = ""

covariate_df = rename(
  select(
    mutate(
      read_dta(paste0(covariate_path, "")),
      ethnicity = case_when(
        is.na(eth) ~ NA,
        eth %in% c(-3, -1) ~ NA,
        eth %in% c(1, 1001, 1002, 1003) ~ "aa. white",
        !(eth %in% c(-3, -1, 1, 1001, 1002, 1003)) ~ "non-white",
      ),
      sex = case_when(
        is.na(sex) ~ NA,
        sex == 0 ~ "female",
        sex == 1 ~ "aa. male",
      ),
      smoking = case_when(
        is.na(smok) ~ NA,
        smok == -3 ~ NA,
        smok == 0 ~ "aa. never",
        smok == 1 ~ "ex",
        smok == 2 ~ "current",
      ),
      sleep_duration = case_when(
        is.na(sleep) ~ NA,
        sleep < 7 ~ "under",
        sleep %in% c(7,8) ~ "aa. normal",
        sleep > 8 ~ "over",
      ),
      bmi = as.numeric(cut(bmi, c(0, 18.5, 25.0, 30.0, 100.0), right=FALSE, labels=FALSE)),
      bmi = case_when(
        is.na(bmi) ~ NA,
        bmi == 1 ~ "under",
        bmi == 2 ~ "aa. normal",
        bmi == 3 ~ "over",
        bmi == 4 ~ "obese",
      ),
      cancer = case_when(
        #bcancer == 0 ~ 0,
        #bcancer == 1 ~ 1,
        #is.na(ts_90010_0_0) ~ NA,
        #canin_d <= ts_90010_0_0 ~ 1, # ts_90010_0_0 start time of acc wear
        is.na(bcancer) | is.na(ts_90010_0_0) ~ NA,
        bcancer == 1 ~ 1,
        canin_d <= ts_90010_0_0 ~ 1, # ts_90010_0_0 start time of acc wear
        bcancer == 0 ~ 0,
      ),
      serious_cvd = case_when(
        #bcvd == 0 ~ 0,
        #bcvd == 1 ~ 1,
        #is.na(ts_90010_0_0) ~ NA,
        #cvdin_d <= ts_90010_0_0 ~ 1, # ts_90010_0_0 start time of acc wear
        is.na(bcvd) | is.na(ts_90010_0_0) ~ NA,
        bcvd == 1 ~ 1,
        cvdin_d <= ts_90010_0_0 ~ 1, # ts_90010_0_0 start time of acc wear
        bcvd == 0 ~ 0,
      ),
      lsdis = case_when(
        is.na(lsdis) ~ NA,
        lsdis == 1 ~ 1,
        lsdis != 1 ~ 0,
      ),
      bdiabetes = case_when(
        is.na(btd) & is.na (bt2d) ~ NA,
        btd == 1 | bt2d == 1 ~ 1,
        !(btd == 1 | bt2d == 1) ~ 0,
      ),
      other_diseases = rowSums(across(c(
        bckd, bdiabetes, bhtn
      ))),
      n_6153_0_0 = case_when(
        n_6153_0_0 %in% c(1, 2, 3, 4) ~ 1,
        !(n_6153_0_0 %in% c(1, 2, 3, 4)) ~ 0,
      ),
      n_6153_0_1 = case_when(
        n_6153_0_1 %in% c(1, 2, 3, 4) ~ 1,
        !(n_6153_0_1 %in% c(1, 2, 3, 4)) ~ 0,
      ),
      n_6153_0_2 = case_when(
        n_6153_0_2 %in% c(1, 2, 3, 4) ~ 1,
        !(n_6153_0_2 %in% c(1, 2, 3, 4)) ~ 0,
      ),
      n_6153_0_3 = case_when(
        n_6153_0_3 %in% c(1, 2, 3, 4) ~ 1,
        !(n_6153_0_3 %in% c(1, 2, 3, 4)) ~ 0,
      ),
      n_6177_0_0 = case_when(
        n_6177_0_0 %in% c(1, 2, 3) ~ 1,
        !(n_6177_0_0 %in% c(1, 2, 3)) ~ 0,
      ),
      n_6177_0_1 = case_when(
        n_6177_0_1 %in% c(1, 2, 3) ~ 1,
        !(n_6177_0_1 %in% c(1, 2, 3)) ~ 0,
      ),
      n_6177_0_2 = case_when(
        n_6177_0_2 %in% c(1, 2, 3) ~ 1,
        !(n_6177_0_2 %in% c(1, 2, 3)) ~ 0,
      ),
      drug_count = rowSums(across(c(
        n_6153_0_0, n_6153_0_1, n_6153_0_2, n_6153_0_3,
        n_6177_0_0, n_6177_0_1, n_6177_0_2, 
      ))),
      mort_cancer = case_when(
        substr(s_40001_0_0, 1, 1) == "C" ~ 1, # primary cod ICD code starts with C
        substr(s_40001_0_0, 1, 1) != "C" ~ 0,
      ),
      mort_cvd = case_when(
        substr(s_40001_0_0, 1, 1) == "I" ~ 1, # primary cod ICD code starts with I
        substr(s_40001_0_0, 1, 1) != "I" ~ 0,
      ),
      mort_resp = case_when(
        substr(s_40001_0_0, 1, 1) == "J" ~ 1, # primary cod ICD code starts with C
        substr(s_40001_0_0, 1, 3) == "U07" ~ 1,
        substr(s_40001_0_0, 1, 1) != "J" & substr(s_40001_0_0, 1, 3) != "U07"~ 0,
      ),
      mort_covid_only = case_when(
        substr(s_40001_0_0, 1, 3) == "U07" ~ 1, # primary cod ICD code starts with I
        substr(s_40001_0_0, 1, 1) != "U07" ~ 0,
      ),
      work_status = case_when(
        is.na(cemploy) ~ NA,
        cemploy == 1 ~ 1,
        cemploy == -3 ~ NA,
        (!cemploy %in% c(1, -3)) ~ 0, 
      ),
      alcohol_frequency = case_when(
        is.na(alcfreq) ~ NA,
        alcfreq == -3 ~ NA,
        alcfreq == 1 ~ "4+/w",
        alcfreq == 2 ~ "<<aa. 3-4/w",
        alcfreq == 3 ~ "1-2/w",
        alcfreq == 4 ~ "1-3/mo",
        alcfreq == 5 ~ "<1/mo",
        alcfreq == 6 ~ "Never",
      ),
      tmort = as.numeric(
        as.POSIXct(censor_d) - as.POSIXct(Acc_start_date)
      ) / 365.25
    ),
    id_steps,
    age_accel,
    tmort,
    died,
    mort_cancer,
    mort_cvd,
    mort_resp,
    mort_covid_only,
    ethnicity,
    sex,
    smoking,
    sleep_duration,
    bmi,
    cancer,
    serious_cvd,
    drug_count,
    other_diseases,
    alcohol_frequency,
    lsdis,
    work_status,
  ),
  age = age_accel,
  follow_up_time = tmort,
  mort_allcause = died,
  stno = id_steps,
)

### Analysis df
analysis_df = zap_formats(
  merge(acc_df, covariate_df, by = "stno", all.x = TRUE)
)
rm(acc_df); rm(covariate_df); rm(acc_path); rm(covariate_path)

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

descriptives_df_o60 = descriptives_df[which(descriptives_df$age >= 60),]
descriptives_df_u60 = descriptives_df[which(descriptives_df$age < 60),]

analysis_df =  mutate(
  analysis_df,
  across(
    c(
      avgaccel_enmo, 
      ig_enmo,
      avgaccel_mad_naive,
      ig_mad_naive,
      avgaccel_mims_naive,
      ig_mims_naive,
      avgaccel_counts_naive,
      ig_counts_naive,
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

analysis_df_o60 = analysis_df[which(analysis_df$age >= 60),]
analysis_df_u60 = analysis_df[which(analysis_df$age < 60),]

