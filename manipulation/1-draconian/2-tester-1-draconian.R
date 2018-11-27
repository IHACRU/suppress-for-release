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
source("./scripts/suppression-functions1-draconian.R") # mechanized suppression of small cells
# ---- load-packages -----------------------------------------------------------
library(magrittr) # pipes
requireNamespace("dplyr")
requireNamespace("readr")
requireNamespace("testit")
requireNamespace("tidyr")

# ---- declare-globals ---------------------------------------------------------
# link to the source of the location mapping
path_input          <- "./data-unshared/derived/dto-1-tuned.rds"
# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script
path_save <- "./data-unshared/derived/dto-2-tested"

# ---- load-data ---------------------------------------------------------------
dto <- readRDS(path_input) 
# bc_health_map <- dto$meta
# Contents
# dto$raw            - dframe - flat data file as obtained from MoH
# dto$meta           - dframe - heirachical map and other meta information
# dto$target         - dframe - a fictional case of surveillance, target shape for mechanized suppression
# dto$FRAMED         - list   - a list, each element of which is disease*year  
# dto$FRAMED$raw     - dframe [L] deconstructed `dto$raw` with each frame = disease * year
# dto$FRAMED$cleaned - dframe [L] tidies values in `region` and `region_desc`
# dto$FRAMED$tuned   - dframe [W] spread into wide from `cleaned` and shape into decision frame

# To be added in this script
# dto$FRAMED$test1   - dframe [W] results of the logical test 1 : smaller than 5
# dto$FRAMED$test1   - dframe [W] results of the logical test 2 : gender triplet
# dto$FRAMED$test1   - dframe [W] results of the logical test 3 : higher-order unit

# ---- inspect-data ---------------------------
lapply(dto, names)
lapply(dto$FRAMED$greeted, names)

# initial target shape we need in order to apply mechanized suppression
dto$target
# this script will develop and apply the function that bring `greeted`` formed into `tuned` form
dto$FRAMED$raw$`Flower Deafness`$`1999` %>% print(n= nrow(.))
dto$FRAMED$cleaned$`Flower Deafness`$`1999`
dto$FRAMED$tuned$`Flower Deafness`$`1999`
# dto$FRAMED$raw$`Multiple Sclerosis`$`1999` %>% print(n= nrow(.))
# dto$FRAMED$cleaned$`Multiple Sclerosis`$`1999`
# dto$FRAMED$tuned$`Multiple Sclerosis`$`1999`


# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script


# ---- tweak-data -------------
# let's remind ourselves what we are doing
# dto$target # this is where we want the data to get
# create a stem to which one can attach the counts 
dstem <- dto$meta %>% 
  lookup_meta("hsda") %>% 
  dplyr::select(label_prov, label_ha, label_hsda)
# we will use this stem in tidy_frame() function

# a suppression decision is made within a context of suppression frame = disease * year 
# pick a case to use in demonstrations
(df <- dto$FRAMED$tuned$`Flower Deafness`$`1999`)
# (df <- dto$FRAMED$tuned$`Multiple Sclerosis`$`1999`)
# dview <- df;# View(dview) # inspect before proceding
# IMPORTANT NOTE: the subsequent functions rely on this shape of data
# note that it is different from 

# ----- demonstrate-logical-tests ---------------------------
# funtion to return the test whether a cell value is less than 5
# TEST 1: What cells are `too small` ( < 5)
# Censor 1: What cells should be suppressed as "too small"?
d1_small_cell <- df %>% detect_small_cell()
# creates a replica of the data, with count values are replaced by TRUE/FALSE according to test

# TEST 2: What cells can help calculate the suppressed cells from the same triple?
# Censor 2: What triples should be suppressed? (eg. F-M-T)
# reverse calculate from:
d2_recalc_from_triplet <- df %>% detect_recalc_triplet()

# TEST 3: Is this is the only triple that is being suppressed in a higher order block?
# Censor 3: What cells should be suppressed as those that could be calculated from higher order count?
d3_single_suppression <- df %>% detect_single_suppression()

# a separate function combines and elongates suppression decision
# this form is suited for attaching results to raw, and to graph the decisions
print(combine_logical_tests)
d_combined_tests <- df %>% combine_logical_tests()
# this form is transient, it is not stored in dto, it's meant to be generated in session


# ----- apply-logical-tests --------------------------------
lapply(dto$FRAMED$tuned, names)
# start with the same structure, to be replaced with transformed frames
dto[["FRAMED"]][["test1"]] <- dto[["FRAMED"]][["tuned"]] 
dto[["FRAMED"]][["test2"]] <- dto[["FRAMED"]][["tuned"]] 
dto[["FRAMED"]][["test3"]] <- dto[["FRAMED"]][["tuned"]] 
lapply(dto$FRAMED$test1, names)


dto$FRAMED$tuned$`Multiple Sclerosis`$`1999` %>% detect_small_cell()

for(disease_ in names(dto$FRAMED$tuned)){
  # loop through available years
  for(year_ in names(dto$FRAMED$tuned[[disease_]]) ){
    year_ <- as.character(year_)
    # apply the logic of test 1
    dto$FRAMED$test1[[disease_]][[year_]] <- 
      dto$FRAMED$tuned[[disease_]][[year_]] %>% 
      detect_small_cell()
    # apply the logic of test 2
    dto$FRAMED$test2[[disease_]][[year_]] <- 
      dto$FRAMED$tuned[[disease_]][[year_]] %>% 
      detect_recalc_triplet()
    # apply the logic of test 3
    dto$FRAMED$test3[[disease_]][[year_]] <- 
      dto$FRAMED$tuned[[disease_]][[year_]] %>% 
      detect_single_suppression()
  }
}


# ---- explore-data ------------------------------------------
# compare results
# compare results
dto$FRAMED$raw$`Multiple Sclerosis`$`1999` %>% print(n= nrow(.))
dto$FRAMED$cleaned$`Multiple Sclerosis`$`1999`
dto$FRAMED$tuned$`Multiple Sclerosis`$`1999`
dto$FRAMED$test1$`Multiple Sclerosis`$`1999`
dto$FRAMED$test2$`Multiple Sclerosis`$`1999`
dto$FRAMED$test3$`Multiple Sclerosis`$`1999`

# Note, while we store the results of logical test in wide form for transparency
# and as convenience to humans, such operations as (1) bringing back the 
# redactions back to the `dto$raw`


# ---- save-to-disk ----------------
saveRDS(dto, paste0(path_save,".rds"))
# readr::write_csv(ds_long, paste0(path_save,".csv"))
lapply(dto, names)
