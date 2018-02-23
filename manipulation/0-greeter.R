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
path_input          <- "./data-public/raw/fictional-input-from-MoH.csv"
path_region_map     <- "./data-public/raw/bc-health-system-map.csv"
path_fictional_case <- "./data-public/raw/fictional-cases/fictional-case-0.csv"

# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script
path_save <- "./data-unshared/derived/dto-0-greeted"


# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script

# ---- load-data ---------------------------------------------------------------
ds             <- readr::read_csv(path_input) %>% as.data.frame() %>% tibble::as_tibble()
bc_health_map  <- readr::read_csv(path_region_map)
fictional_case <- readr::read_csv(path_fictional_case)

# ---- inspect-data -----------------------------------------------------------
# surveillance file from MoH comes as a flat .csv with
# each row = disease * year * locale
ds %>% dplyr::glimpse()
# we have created a logical map of heirarchical map of HSDA -> HA with some meta added
bc_health_map %>% dplyr::glimpse()

# we can take a direct subset of metadata to limit our focus by granularity of the smallest unit
bc_health_map_hsda <- bc_health_map %>% 
  dplyr::select(-id_lha, -label_lha) %>%  # this level is not supported at this time
  dplyr::distinct() %>% 
  print(n = nrow(.))
  
# or better yet, let us create a lookup table for the selected level of granularity
# using a custom function to shape the file for future use
(lkp_hsda <- lookup_meta(meta = bc_health_map, agg_level = "hsda"))
# this is the structure that we will use to think through the logical tests
# and how they can be encoded into scripted commands
# compare it to a manually constructed fictional case
fictional_case

# ---- tweak-data -------------------------------------------------------------
(names(ds) <- tolower(names(ds)))
ds <- ds %>% 
  dplyr::arrange(sex, region)
# what does the data look at this point for a single frame of analysis?
ds %>% 
  dplyr::filter(
    disease ==  "Flower Deafness" # disease + year = FRAME
    ,year    ==  "2001"            # disease + year = FRAME
  ) %>% 
  print(n=nrow(.))

# before we do anything with it, it is important or reflect that 
# THIS is the state of the data that should result at the end of mechanized suppression
# the whole point, the TARGET deliverable is just one additional column/field
# logical vector, indicating decision to suppress (TRUE) or not to suppress (FALSE)

# ---- make-dto ----------------
# let us create a list object `dto` to store heirarchy of flat frames disease * year
dto <- list()
# the first thing we would like to store is the RAW state of the data
dto[["raw"]] <- ds
# at the end of the project we will augment this state with a single variable
# which will contian a logical vectori with suppression decision 

# we also store the imported meta-data with heirarchical map and other meta
dto[["meta"]] <- bc_health_map
# remember that you can shape this meta for better viewing/use by lookup_meta() 
# finally, let's store the target shape of the data that will be crucial to keep in mind
dto[["target"]] <- fictional_case


# now we will deconstruct the flat data frame into a list object in which 
#  level-1 components correspond to diseases and 
# level-2 componets correspond to the years for which we have surveillance data
# Example:
# disease A   # list
#   - year 1  # data frame
#   - year 2  # data frame
# disease B   # list
#   - year 1  # data frame
#   - year 3  # data frame
#   - year 3  # data frame

greeted_list <- list()
# determine the years for which surveillance data is available
diseases_available <- ds %>%  # or dto[["raw]]
  dplyr::distinct(disease) %>% 
  as.list() %>% unlist() %>% as.character()
# loop through available diseases
for(disease_ in diseases_available){
  # determine the years for which data is available for this disease
  years_available <- ds %>% 
    dplyr::filter(disease == disease_) %>% 
    dplyr::arrange(year) %>% 
    dplyr::distinct(year) %>% 
    as.list() %>% unlist() %>% as.character()
  # loop through available years
  for(year_ in years_available ){
    d1 <- ds %>% 
      dplyr::filter(disease == disease_) %>% 
      dplyr::filter(year    == year_)
    greeted_list[[disease_]][[year_]] <- d1
  }
}
sapply(greeted_list, names)
# this list object in which the smallest unit is the analytic frame: disease * year
dto[["greeted"]] <- greeted_list


# ---- save-to-disk --------------------
saveRDS(dto, paste0(path_save,".rds"))



