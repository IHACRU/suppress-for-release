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
library(ggplot2) #For graphing
library(dplyr)
library(magrittr) #Pipes
requireNamespace("readxl")

requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables

# ---- declare-globals ---------------------------------------------------------
# link to the source of the location mapping
# path_input <- "./data-unshared/raw/SearchVariables.csv"
path_input <- "./data-unshared/raw/flower-deafness.csv"
path_region_map <- "./data-public/raw/bc_health_system_map.csv"
path_fictional_case <- "./data-public/raw/fictional_cases/fictional-case-1.csv"

# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script
# path_save <- "./data-unshared/derived/memory"
path_save <- "./data-unshared/derived/0-dto"
# See definitions of commonly  used objects in:
source("./manipulation/object-glossary.R")   # object   definitions
source("./manipulation/function-support.R")  # function definitions

# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script

# ---- load-data ---------------------------------------------------------------
ds <- readr::read_csv(path_input) %>% as.data.frame() %>% tibble::as_tibble()
bc_health_map <- readr::read_csv(path_region_map)
fictional_case <- readr::read_csv(path_fictional_case)


# ---- inspect-data -----------------------------------------------------------
ds %>% dplyr::glimpse()
bc_health_map %>% dplyr::glimpse()

# a direct subset of metadata
bc_health_map_hsda <- bc_health_map %>% 
  dplyr::select(-id_lha, -label_lha) %>%  # this level is not supported at this time
  dplyr::distinct() %>% 
  print(n = nrow(.))
  
# groomed production of the lookup table for the selected level of smallest unit
(lkp_hsda <- lookup_meta(meta = bc_health_map, agg_level = "hsda"))


# ---- tweak-data -------------------------------------------------------------
(names(ds) <- tolower(names(ds)))
# what does the data look at this point for a single frame of analysis?
ds %>% 
  dplyr::filter(
     disease ==  "Flower Deafness" # diseas + year = FRAME
    ,year    ==  "2001"
  ) %>% 
  dplyr::arrange(sex, region) %>% 
  print(n=nrow(.))

# before we do anything with it, it is important or reflect that 
# THIS is the state of the data that should result at the end of mechanized suppression
# the whole point, the TARGET deliverable is just one additional column/field
# logical vector, indicating decision to suppress (TRUE) or not to suppress (FALSE)

# we make a conscious decision to avoid sex == `U` and therefore remove it from the suppression logic
ds <- ds %>% 
  dplyr::filter(
    sex %in% c("F", "M", "T") # state in the affirmative for data quality
  )
# now check the frame again
ds %>% 
  dplyr::filter(
    disease ==  "Flower Deafness" # diseas + year = FRAME
    ,year    ==  "2001"
  ) %>% 
  dplyr::arrange(sex, region) %>% 
  print(n=nrow(.))

# at this point, we need to bring the data into a tidy format to connected with 'bc_heath_map'
# see https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html for tidy data explanation
# variables `region` and `region_desc` needs to be deconstructed
ds %>% dplyr::distinct(region) %>% print(n = nrow(.))
ds %>% dplyr::distinct(region_desc) %>% print(n = nrow(.))
# use www.regex101.com to develop the regular expression for this case
regex_region = "^(\\w+)-?(\\d+)?"
regex_desc = "^(\\d+)* ?(.*)"
ds <- ds %>% 
  dplyr::mutate(
     region_id    = gsub(pattern = regex_region, replacement = "\\2", x = region) %>% as.integer()
    ,region_label = gsub(pattern = regex_region, replacement = "\\1", x = region)
    ,desc_label   = gsub(pattern = regex_desc,   replacement = "\\2", x = region_desc)
  )
# inspect the results of deconstruction
ds %>% 
  dplyr::distinct(region, region_desc, region_id, region_label, desc_label) %>% 
  dplyr::arrange(region) %>% 
  print(n = nrow(.))
# create distinct numeric codes for missingness of different units of the heirarchy
ds <- ds %>% 
  dplyr::mutate(
     region_id = ifelse( region_label == "BC" , 0, region_id ) 
    ,region_id = ifelse( region_id == "99", NA, region_id) 
    ,region_id = as.integer(region_id) 
  )
# inspect the results of deconstruction
ds %>% 
  dplyr::distinct(region, region_desc, region_id, region_label, desc_label) %>% 
  dplyr::arrange(region) %>% 
  print(n = nrow(.))


# now check the frame again
ds %>% 
  dplyr::filter(
    disease ==  "Flower Deafness" # diseas + year = FRAME
    ,year    ==  "2001"
  ) %>% 
  dplyr::arrange(sex, region) %>% 
  print(n=nrow(.))


# attach the meta data

# ---- tidy ------------------------


# now we need to tidy the data further
head(ds)
ds_long <- ds %>% 
  dplyr::select(-region, -region_desc) %>% 
  dplyr::filter(year == 2000) %>% 
  dplyr::arrange(sex, region_id) 
# this is the raw state that needs to be transformed to conform to the decision state
ds_long %>% filter(sex == "F") %>% print(n = nrow(.))

ds_wide <- ds_long %>% 
  dplyr::mutate(
    region_by_sex = paste0(region_label,"_",sex)
  ) %>% 
  dplyr::select(-region_label, -sex) %>% 
  tidyr::spread(key = region_by_sex, value = incase)
    
 


bc_health_map
# ---- explore-data ------------------------------------------


# ---- save-to-disk ----------------
saveRDS(ds_long, paste0(path_save,".rds"))
readr::write_csv(ds_long, paste0(path_save,".csv"))



