# knitr::stitch_rmd(script="./manipulation/0-ellis-map.R", output="./manipulation/stitched-output/0-ellis-map.md")
# This script reads two files: encounter counts with location mapping and encounter timelines for selected individuals
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
source("./manipulation/function-support.R")  # assisting functions for data wrangling and testing
source("./manipulation/object-glossary.R")   # object definitions
source("./scripts/common-functions.R")       # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions
source("./scripts/suppression-functions.R")  # mechanized suppression of small cells
# ---- load-packages -----------------------------------------------------------
library(ggplot2)  # graphing
# library(dplyr)    # data wrangling
library(magrittr) # pipes

requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables

# ---- declare-globals ---------------------------------------------------------
# link to the source of the location mapping
path_input          <- "./data-unshared/derived/dto-2-tested.rds"
# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script
path_save <- "./data-unshared/derived/dto-3-graphed"

# ---- load-data ---------------------------------------------------------------
dto <- readRDS(path_input) 
bc_health_map <- dto$meta

# Contents
# dto$raw    - dframe - flat data file as obtained from MoH
# dto$meta   - dframe - heirachical map and other meta information
# dto$target - dframe - a fictional case of surveillance, target shape for mechanized suppression
# dto$FRAMED - list - 
# dto$FRAMED$raw - deconstructed `dto$raw` with each frame = disease * year
# dto$FRAMED$tuned   - 
# dto$FRAMED$test1  - 
# dto$FRAMED$test1  - 
# dto$FRAMED$test1  -


# ---- inspect-data ---------------------------
lapply(dto, names)
lapply(dto$FRAMED$raw, names)

# initial target shape we need in order to apply mechanized suppression
dto$target
# this script will develop and apply the function that bring `greeted`` formed into `tuned` form
dto$FRAMED$raw$`Flower Deafness`$`1999` %>% print(n= nrow(.))
dto$FRAMED$tuned$`Flower Deafness`$`1999`
dto$FRAMED$test1$`Flower Deafness`$`1999`
dto$FRAMED$test2$`Flower Deafness`$`1999`
dto$FRAMED$test3$`Flower Deafness`$`1999`

# ---- utility-functions ----------------------------------------------------- 


# ---- tweak-data -------------


# ---- explore-data ------------------------------------------
# compare results
# compare results
dto$FRAMED$raw$`Flower Deafness`$`1999` %>% print(n= nrow(.))
dto$FRAMED$tuned$`Flower Deafness`$`1999`
dto$FRAMED$test1$`Flower Deafness`$`1999`
dto$FRAMED$test2$`Flower Deafness`$`1999`
dto$FRAMED$test3$`Flower Deafness`$`1999`


# ---- save-to-disk ----------------
saveRDS(dto, paste0(path_save,".rds"))
# readr::write_csv(ds_long, paste0(path_save,".csv"))
lapply(dto, names)
