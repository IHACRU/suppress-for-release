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

# function to transform the analytic frame into target shape
tidy_frame <- function(
  d      # a single data frame from the greeted element
  ,stem  # a foundation for assembling individual frames used in analysis
){
  # d <-df
  # subset values to be evaluated
  # we make a conscious decision to avoid sex == `U` and therefore remove it from the suppression logic
  d1 <- d %>% 
    dplyr::filter(
      ! sex %in% c("U") # remove counts for unknown or unidentified sex
    ) %>% 
    # counts in the `Unknown` locales are not passed for public release
    dplyr::filter(
      ! region_desc %in% c("Unknown HSDA","Unknown HA","Unknown LHA")
    )
  # d1 %>% print(n = nrow(.))
  dview <- d1 
  
  # use www.regex101.com to develop the regular expression for this case
  regex_region = "^(\\w+)-?(\\d+)?"
  regex_desc   = "^(\\d+)* ?(.*)"
  d2 <- d1 %>% 
    dplyr::mutate(
      region_id    = gsub(pattern = regex_region, replacement = "\\2", x = region) %>% as.integer()
      ,region_label = gsub(pattern = regex_region, replacement = "\\1", x = region)
      ,desc_label   = gsub(pattern = regex_desc,   replacement = "\\2", x = region_desc)
    ) %>% 
    dplyr::mutate(
      region_id = ifelse( region_label == "BC" , 0, region_id )
      ,region_id = as.integer(region_id) 
    )
  # inspect the results of deconstruction
  # d2 %>% print(n = 25)
  dview <- d2
  
  d3 <- d2 %>% 
    # dplyr::filter(sex == "F") %>% 
    # dplyr::select(-region, -region_desc, -region_id) %>% 
    dplyr::mutate( 
      newvar = paste0("label_", tolower(region_label))
      ,newvar = ifelse(region_label == "BC","label_prov", newvar)
    ) %>% 
    tidyr::spread( key = newvar, value = desc_label) %>% 
    dplyr::mutate(
      region_by_sex = paste0(region_label,"_",sex)
    ) %>% 
    tidyr::spread( key = region_by_sex, value = incase) %>% 
    dplyr::arrange(sex)
  dview <- d3
  
  ls4 <- list()
  for(i in c("HSDA_F","HSDA_M","HSDA_T") ){
    ls4[["hsda"]][[i]] <- d3 %>% 
      dplyr::select_(.dots = c("label_hsda", i)) %>% 
      dplyr::filter(stats::complete.cases(.))
  } 
  for(i in c("HA_F","HA_M","HA_T") ){
    ls4[["ha"]][[i]] <- d3 %>% 
      dplyr::select_(.dots = c("label_ha", i)) %>% 
      dplyr::filter(stats::complete.cases(.))
  }
  for(i in c("BC_F","BC_M","BC_T") ){
    ls4[["prov"]][[i]] <- d3 %>% 
      dplyr::select_(.dots = c("label_prov", i)) %>% 
      dplyr::filter(stats::complete.cases(.))
  } 
  ls4
  
  
  ls5 <- list()
  for(i in names(ls4)){
    ls5[[i]] <- ls4[[i]] %>% full_join_multi()
  }
  
  # ls6 <- list()
  # ls6[["stem"]] <- dstem
  
  d4 <- stem %>% 
    dplyr::left_join(ls5$prov) %>% 
    dplyr::left_join(ls5$ha) %>%
    dplyr::left_join(ls5$hsda)
  
  return(d4)
  
}
# usage
# d <- dto$greeted$`Flower Deafness`$`1999` %>% tidy_frame(dstem)

# function to carry out a full join among all components of the list object
full_join_multi <- function(list_object){
  # list_object <- datas[["physical"]][["161"]]
  d <- list_object %>%
    Reduce(function(dtf1,dtf2) dplyr::full_join(dtf1,dtf2), .)
}

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



# ----- define-utility-functions ----------------------------

# ---- tweak-data -------------
# now we will create a list object                  dto$tuned, 
# which will mirrow the tructure of                 dto$greeted
# but will contain frames conformed to the shape of dto$target

# let's remind ourselves what we are doing
dto$target # this is where we want the data to get
# create a stem to which one can attach the counts 
dstem <- dto$meta %>% 
  lookup_meta("hsda") %>% 
  dplyr::select(label_prov, label_ha, label_hsda)
# we will use this stem in tidy_frame() function

lapply(dto$greeted, names)
# loop through available diseases
dto[["tuned"]] <- dto[["greeted"]] # start with the same structure, replace with transformed frames
lapply(dto$tuned, names)

for(disease_ in names(dto$greeted)){
  # loop through available years
  for(year_ in names(dto$greeted[[disease_]]) ){
    # year_ <- names(dto$greeted[[disease_]])[1]
    dto[["tuned"]][[disease_]][[year_]] <- 
      dto[["greeted"]][[disease_]][[year_]] %>% 
      tidy_frame(stem = dstem)
  }
}

# ---- explore-data ------------------------------------------
# compare results
dto$greeted$`Flower Deafness`$`1999` %>% print(n= nrow(.))
dto$tuned$`Flower Deafness`$`1999`


# ---- save-to-disk ----------------
saveRDS(dto, paste0(path_save,".rds"))
# readr::write_csv(ds_long, paste0(path_save,".csv"))
lapply(dto, names)
