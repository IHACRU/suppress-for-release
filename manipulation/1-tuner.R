# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./manipulation/1-tuner.R",
#   output = "./manipulation/stitched_output/1-tuner.md"
# )

# This script brings the source data into tidy format, widens it and prepares for testing
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
source("./manipulation/function-support.R")  # assisting functions for data wrangling and testing
source("./manipulation/object-glossary.R")   # object definitions
source("./scripts/common-functions.R")       # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions
source("./scripts/suppression-functions-2-targeted.R") # mechanized suppression of small cells

# ---- load-packages -----------------------------------------------------------
library(magrittr) # pipes
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("readr", quietly=TRUE)
requireNamespace("testit", quietly=TRUE)
requireNamespace("tidyr", quietly=TRUE)

# ---- declare-globals ---------------------------------------------------------
# link to the source of the location mapping
path_input          <- "./data-unshared/derived/dto-0-greeted.rds"
path_fictional_case <- "./data-public/raw/fictional-cases/fictional-case-0.csv"
# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script
path_save <- "./data-unshared/derived/dto-1-tuned"

# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script


# ---- load-data ---------------------------------------------------------------
dto <- readRDS(path_input) 

# Contents
# dto$raw            - dframe - flat data file as obtained from MoH
# dto$meta           - dframe - heirachical map and other meta information
# dto$target         - dframe - a fictional case of surveillance, target shape for mechanized suppression
# dto$FRAMED         - list   - a list, each element of which is disease*year  
# dto$FRAMED$raw     - dframe [L] deconstructed from `dto$raw` with each frame = disease * year

# To be added in this script:
# dto$FRAMED$cleaned - dframe [L] tidies values in `region` and `region_desc`
# dto$FRAMED$tuned   - dframe [W] spread into wide from `cleaned` and shape into decision frame

# this script will develop and apply the function that 
# brings `raw` form first into `cleaned` and then into `tuned` forms

# ---- inspect-data ---------------------------
lapply(dto, names)
lapply(dto$FRAMED, names)

# select a unit for suppression decision; all transformation will be keyed to this shape
df <- dto$FRAMED$raw$`Flower Deafness`$`2000`
# df <- dto$FRAMED$raw$`Multiple Sclerosis`$`2000`
df %>% print(n = nrow(.))
# compare it to the shape we need it to be to apply mechanized suppression
dto$target


# ---- tweak-data -------------
# now we will create a list object                  dto$tuned, 
# which will mirrow the tructure of                 dto$raw
# but will contain frames conformed to the shape of dto$target

# let's remind ourselves what we are doing
# dto$target # this is where we want the data to get
# create a stem to which one can attach the counts 
dstem <- dto$meta %>% 
  lookup_meta("hsda") %>% 
  dplyr::select(label_prov, label_ha, label_hsda)
# we will use this stem in tidy_frame() function

lapply(dto$FRAMED$raw, names)
# start with the same structure, to be replaced with transformed frames
dto[["FRAMED"]][["cleaned"]] <- dto[["FRAMED"]][["raw"]] 
dto[["FRAMED"]][["tuned"]]   <- dto[["FRAMED"]][["raw"]] 
lapply(dto$FRAMED$tuned, names)

for(disease_ in names(dto$FRAMED$raw)){
  # loop through available years
  for(year_ in names(dto$FRAMED$raw[[disease_]]) ){
    
    # create a long form to connect back to the raw
    dto$FRAMED$cleaned[[disease_]][[year_]] <- 
      dto$FRAMED$raw[[disease_]][[year_]] %>% 
      clean_raw(stem = dstem)
    
    # creat a wide form to connect to the logical test
    dto$FRAMED$tuned[[disease_]][[year_]] <- 
      dto$FRAMED$raw[[disease_]][[year_]] %>% 
      tidy_frame(stem = dstem)
  }
}

# ---- explore-data ------------------------------------------
# compare results
dto$FRAMED$raw$`Flower Deafness`$`1999` %>% print(n= nrow(.))
dto$FRAMED$cleaned$`Flower Deafness`$`1999`
dto$FRAMED$tuned$`Flower Deafness`$`1999`
# dto$FRAMED$raw$`Multiple Sclerosis`$`1999` %>% print(n= nrow(.))
# dto$FRAMED$cleaned$`Multiple Sclerosis`$`1999`
# dto$FRAMED$tuned$`Multiple Sclerosis`$`1999`


# ---- save-to-disk ----------------
saveRDS(dto, paste0(path_save,".rds"))
# readr::write_csv(ds_long, paste0(path_save,".csv"))
lapply(dto, names)
