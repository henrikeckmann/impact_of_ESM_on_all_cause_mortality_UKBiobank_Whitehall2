rm(list = ls())
source(".../R/UKBB/2_create_analysis_dataset_ukbb.R")
source(".../R/cors_df_function.R")
library(flextable)

cors_aa_mod = cors_df_o60('avgaccel', 'mod')
cors_ig_mod = cors_df_o60('ig', 'mod')

cors_aa_mod = select(
  mutate(
    cors_aa_mod,
    cor = str_glue("{cor} ({cor_lci}, {cor_uci})")
  ),
  (-c(cor_lci, cor_uci))
)
cors_ig_mod = select(
  mutate(
    cors_ig_mod,
    icc = str_glue("{icc} ({icc_lci}, {icc_uci})"),
    cor = str_glue("{cor} ({cor_lci}, {cor_uci})")
  ),
  (-c(icc_lci, icc_uci, cor_lci, cor_uci))
)

write_csv(
  as.data.frame(cors_aa_mod),
  ""
)

write_csv(
  as.data.frame(cors_ig_mod),
  ""
)

### Intra metric corrs

metric_cors_df = metric_cors_df_o60()
