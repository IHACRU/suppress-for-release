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

# ---- inspect-data ---------------------------
lapply(dto, names)
lapply(dto$greeted, names)

# select a unit for suppression decision; all transformation will be keyed to this shape
df <- dto$greeted$`Flower Deafness`$`1999`
df %>% print(n = nrow(.))
# compare it to the shape we need it to be to apply mechanized suppression
dto$target
# this script will develop and apply the function that bring `greeted`` formed into `tuned` form


# -----

# subset values to be evaluated
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

