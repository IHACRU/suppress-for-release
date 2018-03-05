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
path_input          <- "./data-unshared/derived/dto-1-tuned.rds"
# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script
path_save <- "./data-unshared/derived/dto-2-tested"

# ---- load-data ---------------------------------------------------------------
dto <- readRDS(path_input) 
# bc_health_map <- dto$meta
# Contents
# dto$raw    - dframe - flat data file as obtained from MoH
# dto$meta   - dframe - heirachical map and other meta information
# dto$target - dframe - a fictional case of surveillance, target shape for mechanized suppression
# dto$FRAMED - list - 
# dto$FRAMED$raw - deconstructed `dto$raw` with each frame = disease * year
# dto$FRAMED$tuned   - list - suppression-ready frames, structural mirrow of dto$FRAMED$raw

# ---- inspect-data ---------------------------
lapply(dto, names)
lapply(dto$FRAMED$greeted, names)

# initial target shape we need in order to apply mechanized suppression
dto$target
# this script will develop and apply the function that bring `greeted`` formed into `tuned` form
dto$FRAMED$raw$`Flower Deafness`$`1999` %>% print(n= nrow(.))
dto$FRAMED$tuned$`Flower Deafness`$`1999`


# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script

# function to carry out a full join among all components of the list object
full_join_multi <- function(list_object){
  # list_object <- datas[["physical"]][["161"]]
  d <- list_object %>%
    Reduce(function(dtf1,dtf2) dplyr::full_join(dtf1,dtf2), .)
}

# ---- tweak-data -------------
# let's remind ourselves what we are doing
# dto$target # this is where we want the data to get
# create a stem to which one can attach the counts 
dstem <- dto$meta %>% 
  lookup_meta("hsda") %>% 
  dplyr::select(label_prov, label_ha, label_hsda)
# we will use this stem in tidy_frame() function



lapply(dto$FRAMED$tuned, names)
# start with the same structure, to be replaced with transformed frames
dto[["FRAMED"]][["test1"]] <- dto[["FRAMED"]][["tuned"]] 
dto[["FRAMED"]][["test2"]] <- dto[["FRAMED"]][["tuned"]] 
dto[["FRAMED"]][["test3"]] <- dto[["FRAMED"]][["tuned"]] 
lapply(dto$FRAMED$test1, names)


dto$FRAMED$tuned$`Flower Deafness`$`1999` %>% detect_small_cell()

for(disease_ in names(dto$FRAMED$tuned)){
  # loop through available years
  for(year_ in names(dto$FRAMED$tuned[[disease_]]) ){
    year_ <- as.character(year_)
    # apply the logic of test 1
    dto$FRAMED$test1[[disease_]][[year_]] <- 
      dto$FRAMED$tuned[[disease_]][[year_]] %>% 
      detect_small_cell()
    # apply the logic of test 1
    dto$FRAMED$test2[[disease_]][[year_]] <- 
      dto$FRAMED$tuned[[disease_]][[year_]] %>% 
      detect_recalc_triplet()
    # apply the logic of test 1
    dto$FRAMED$test3[[disease_]][[year_]] <- 
      dto$FRAMED$tuned[[disease_]][[year_]] %>% 
      detect_single_suppression()
  }
}


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
