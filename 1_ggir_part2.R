rm(list = ls())

library(GGIR)

con_out_file <- file(
  "", open = "a"
  )

sink(con_out_file, append = TRUE, type = "output", split = TRUE)

print("")
print("")
print("STARTING NEW RUN")
print("processing started at")
print(Sys.time())

datadir = "" # change to path of raw data directory, just like GGIR

# Run GGIR part 2
# 25 mg bins
GGIR(
  mode = c(2),
  datadir = datadir,
  outputdir = "",
  overwrite = FALSE,
  iglevels = 1
)

# Modelled bins

bins <- read.csv("")

# mad modelled bins
GGIR(
  mode = c(2),
  datadir = datadir,
  outputdir = "",
  overwrite = FALSE,
  iglevels = bins$mad_bins
)

# mims modelled bins
GGIR(
  mode = c(2),
  datadir = datadir,
  outputdir = "",
  overwrite = FALSE,
  iglevels = bins$mims_bins
)

# counts modelled bins
GGIR(
  mode = c(2),
  datadir = datadir,
  outputdir = "",
  overwrite = FALSE,
  iglevels = bins$counts_bins/1000
)

print("processing ended at")
print(Sys.time())

sink()
close(con_out_file)
