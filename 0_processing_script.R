rm(list = ls())

source("PATH TO addMIMS_func.R") # change to path of addMIMS_func.R script
datadir = "PATH TO RAW DATA DIRECTORY" # change to path of raw data directory, just like GGIR
outputdir = "PATH FOR OUTPUT" # change to path where output will be generated, just like GGIR

# NOTE: one of the internal functions borrowed from GGIR
# apparently doesn't handle tilde-expansion in paths, so I've added path.expand()
# to expand to full paths ie. "~/username/data/" becomes "/Users/username/data/"
# This works, but I don't know which conditions might break it (ie. some non-standard home directory)
# so recommend using full paths in the first place, ie. don't use ~ in path.
# Script will now print outputdir and datadir paths after expansion to allow debugging

dynrange = c(-8, 8) # for MIMSunit function. Don't change.

# Run GGIR part 1
# don't change parameters
GGIR(
  mode = c(1),
  datadir = datadir,
  outputdir = outputdir,
  maxdur = 9,
  windowsizes = c(5, 900, 3600),
  overwrite = FALSE,
  do.enmo = TRUE, # generate ENMO
  do.mad = TRUE, # generate MAD
  do.neishabouricounts = TRUE #generate counts,
)

# Add MIMS to metafiles output from GGIR part 1
# MIMS vector in metafile output will be called "LFENMO" to trick GGIR part 2 later
addMIMS(
  datadir = datadir,
  outputdir = outputdir,
  dynrange = dynrange,
  desiredtz = "",
  configtz = "",
  overwrite = FALSE
)
