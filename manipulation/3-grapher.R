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
source("./scripts/suppression-functions-2-targeted.R")  # mechanized suppression of small cells
# ---- load-packages -----------------------------------------------------------
library(ggplot2)  # graphing
library(magrittr) # pipes
requireNamespace("dplyr")
requireNamespace("readr")
requireNamespace("testit")
requireNamespace("tidyr")
requireNamespace("rmarkdown")

# ---- declare-globals ---------------------------------------------------------
# link to the source of the location mapping
path_input          <- "./data-unshared/derived/dto-2-tested.rds"
# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script
path_save <- "./data-unshared/derived/dto-3-graphed"

# ---- load-data ---------------------------------------------------------------
dto <- readRDS(path_input) 
# bc_health_map <- dto$meta
# Contents
# dto$raw            - dframe - flat data file as obtained from MoH
# dto$meta           - dframe - heirachical map and other meta information
# dto$target         - dframe - a fictional case of surveillance, target shape for mechanized suppression
# dto$FRAMED         - list   - a list, each element of which is disease*year FRAME 
# dto$FRAMED$raw     - dframe [L] deconstructed `dto$raw` with each frame = disease * year
# dto$FRAMED$cleaned - dframe [L] tidies values in `region` and `region_desc`
# dto$FRAMED$tuned   - dframe [W] spread into wide from `cleaned` and shape into decision frame
# dto$FRAMED$test1   - dframe [W] results of the logical test 1 : smaller than 5
# dto$FRAMED$test2   - dframe [W] results of the logical test 2 : gender triplet
# dto$FRAMED$test3   - dframe [W] results of the logical test 3 : higher-order unit
# dto$FRAMED$test3d   - dframe [W] results of the logical test 3d : higher-order unit (draconian)

# To be completed in this script
# dto$FRAMED$redacted
# dto$augmented


# ---- inspect-data ---------------------------
lapply(dto, names)
lapply(dto$FRAMED$raw, names)

# initial target shape we need in order to apply mechanized suppression
dto$target
# this script will develop and apply the function that bring `greeted`` formed into `tuned` form
dto$FRAMED$raw$`Flower Deafness`$`1999` %>% print(n= nrow(.))
dto$FRAMED$tuned$`Flower Deafness`$`1999`
# dto$FRAMED$raw$`Multiple Sclerosis`$`1999` %>% print(n= nrow(.))
# dto$FRAMED$tuned$`Multiple Sclerosis`$`1999`


# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script
redact_clean_frame <- function(
  dto
  ,disease_
  ,year_
){
  # values for testing
  # disease_ = "Multiple Sclerosis"
  # year_    = "1999"
  (df_cleaned <- dto$FRAMED$cleaned[[disease_]][[year_]])
  (df_tuned <- dto$FRAMED$tuned[[disease_]][[year_]])

  df_tested <- df_tuned %>% combine_logical_tests() %>% 
      dplyr::mutate(
        # the censors cannot reverse redaction decision, they are cumulative
        redact_row  = censor3_single_suppression # the last test includes ALL redactions
        
      ) %>% 
      dplyr::select(-censor0, -censor1_small_cell, # we don't need them here 
                    -censor2_recalc_triplet, -censor3_single_suppression,
                    -censor3_single_suppression_draconian, -censor_activated)  
  prov_value <- df_tested %>% 
      dplyr::filter(agg_level == "PROV") %>% 
      dplyr::distinct(label_prov, sex, value, redact_row) %>% 
      dplyr::rename(region_label = label_prov)
  ha_value <- df_tested %>% 
      dplyr::filter(agg_level == "HA") %>% 
      dplyr::distinct(label_ha, sex, value, redact_row) %>% 
      dplyr::arrange(label_ha) %>% 
      dplyr::rename(region_label = label_ha)
  hsda_value <- df_tested %>% 
      dplyr::filter(agg_level == "HSDA") %>% 
      dplyr::distinct(label_hsda, sex, value, redact_row) %>% 
      dplyr::arrange(label_hsda) %>% 
      dplyr::rename(region_label = label_hsda)
  # combine the rows to obtain needed form
  d_values <- dplyr::bind_rows(
    list( prov_value, ha_value, hsda_value)
  )
  # the following join operates under the assumption that
  # no label of province, health authority, and health service delivery area are identidcal
  # linking through `region_id` connected to the `dto$meta` is preferable
  # but avoided at present time for brevity and simplicity
  df_redacted <- df_cleaned %>% 
      dplyr::left_join(
        d_values, 
        by = c("desc_label" = "region_label","sex", "incase" = "value")
      )
  return(df_redacted)
}
# usage
# dto$FRAMED[["redacted"]] <- dto$FRAMED$cleaned # create shell to populate
# dto$FRAMED[["redacted"]] <- dto %>% redact_clean_frame("Multiple Sclerosis", "1999")
# dto$FRAMED[["redacted"]] <- dto %>% redact_clean_frame("Flower Deafness", "1999")



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
# (df <- dto$FRAMED$tuned$`Multiple Sclerosis`$`1999`)
(df <- dto$FRAMED$tuned$`Flower Deafness`$`1999`)


# ----- demonstrate-redaction-mechanism ------------------------

# dto$FRAMED
(df_raw <- dto$FRAMED$raw$`Multiple Sclerosis`$`1999`)  
(df_cleaned <- dto$FRAMED$cleaned$`Multiple Sclerosis`$`1999`)
(df_tuned <- dto$FRAMED$tuned$`Multiple Sclerosis`$`1999`)
# (df_test1 <- dto$FRAMED$test1$`Multiple Sclerosis`$`1999`)
# (df_test2 <- dto$FRAMED$test2$`Multiple Sclerosis`$`1999`)
# (df_test3 <- dto$FRAMED$test3$`Multiple Sclerosis`$`1999`)
(df_tested <- df_tuned %>% combine_logical_tests() %>% 
    dplyr::mutate(
      redact_row  = censor3_single_suppression # the last test includes ALL redactions
      
    ) %>% 
    dplyr::select(-censor0, -censor1_small_cell, # we don't need them here 
                  -censor2_recalc_triplet, -censor3_single_suppression)
  # consider keeping censor1, 2, and 3 as is in  order to color code our decisions
)
(
  prov_value <- df_tested %>% 
    dplyr::filter(agg_level == "PROV") %>% 
    dplyr::distinct(label_prov, sex, value, redact_row) %>% 
    dplyr::rename(
      region_label = label_prov
    )
)
(
  ha_value <- df_tested %>% 
    dplyr::filter(agg_level == "HA") %>% 
    dplyr::distinct(label_ha, sex, value, redact_row) %>% 
    dplyr::arrange(label_ha) %>% 
    dplyr::rename(
      region_label = label_ha
    )
)
(
  hsda_value <- df_tested %>% 
    dplyr::filter(agg_level == "HSDA") %>% 
    dplyr::distinct(label_hsda, sex, value, redact_row) %>% 
    dplyr::arrange(label_hsda) %>% 
    dplyr::rename(
      region_label = label_hsda
    )
)
d_values <- dplyr::bind_rows(
  list( prov_value, ha_value, hsda_value)
) 

# the following join operates under the assumption that
# no label of province, health authority, and health service delivery area are identidcal
# linking through `region_id` connected to the `dto$meta` is preferable
# but avoided at present time for brevity and simplicity

df_redacted <- df_cleaned %>% 
  dplyr::left_join(d_values, by = c("desc_label" = "region_label","sex", "incase" = "value"))

# the logic demonstrated above is encoded into `redact_clean_frame()`
# which is stored in the `utility-functions` chunk

# ----- implement-redaction ----------------------
lapply(dto$FRAMED$tuned, names)
# start with the same structure, to be replaced with transformed frames
dto[["FRAMED"]][["redacted"]] <- dto[["FRAMED"]][["tuned"]]  # create shell to populate

for(disease_i in names(dto$FRAMED$tuned)){
  # loop through available years
  for(year_i in names(dto$FRAMED$tuned[[disease_i]]) ){
    year_i <- as.character(year_i) # in case in passed as integer
    # populate shells with redacted data frames
    dto$FRAMED[["redacted"]][[disease_i]][[year_i]] <- dto %>% 
      redact_clean_frame(disease_i, year_i)
  }
}

# verify
# compare results
# dto$FRAMED$raw$`Multiple Sclerosis`$`1999` %>% print(n= nrow(.))
dto$FRAMED$cleaned$`Multiple Sclerosis`$`1999`
# dto$FRAMED$tuned$`Multiple Sclerosis`$`1999`
# dto$FRAMED$test1$`Multiple Sclerosis`$`1999`
# dto$FRAMED$test2$`Multiple Sclerosis`$`1999`
# dto$FRAMED$test3$`Multiple Sclerosis`$`1999`
dto$FRAMED$redacted$`Multiple Sclerosis`$`1999`

# ---- demonstrate-graphing-functions ------------------------
# now we need to create a graphical representation of redaction decisions

# first bring in the disease*year elements of FRAMES as variables
disease_ = "Parkinsonism"
year_ = "1999"
df <- dto$FRAMED$tuned[[disease_]][[year_]] %>% 
  dplyr::mutate(disease = disease_, year = year_) %>% 
  dplyr::select(disease, year, dplyr::everything())
# create a list object with data shape suited for plotting
l <- df %>% prepare_for_tiling(dto$meta)
# saveRDS(df,"./data-public/derived/example-for-graph-making.rds") # a case for graphing scenario
# saveRDS(l,"./data-public/derived/example-for-graph-making.rds") # a case for graphing scenario
# generate a graph of a single logical test
df %>% make_tile_graph(dto$meta, "censor0")
df %>% make_tile_graph(dto$meta, "censor1_small_cell")
df %>% make_tile_graph(dto$meta, "censor2_recalc_triplet")
df %>% make_tile_graph(dto$meta, "censor3_single_suppression")

# it is very useful to segregate how
# (1) a plot is assembled with graphing script from how
# (2) a plot is committed to a hard digital form (PNG, JPG, PDF) 
# can help us avoid going insane from trying to make it look right/useful on paper/screen
# there are many decision about the appearance of the plot that needs to be scripted
df %>% print_tile_graph(dto$meta, path_folder = "./manipulation/prints/", size = 3)

# so far, df referred to a single Data Frame = a context for a single suppression decision
# we can use a wrapper function to loop through  a large number of frames


# ----- graph-redacted-frames --------------------------------
# the graphing functions will require this object 
bc_health_map <- dto$meta

# loop through available diseases
for(disease_i in names(dto$FRAMED$tuned)){
  # loop through available years
  for(year_i in names(dto$FRAMED$tuned[[disease_i]]) ){
   
    # disease_i = "Multiple Sclerosis"
    # year_i = "1999"
    
    df <- dto$FRAMED$tuned[[disease_i]][[year_i]] %>% 
      dplyr::mutate(disease = disease_i, year = year_i) %>% 
      dplyr::select(disease, year, dplyr::everything())
    
    df %>% print_one_frame(
      disease_ = disease_i,
      year_ = year_i,
      folder = "./manipulation/prints/demo/")
    
    # store a graph with the final redaction recommendation in dto
    # this feature is currently (under consideration)
    # dto$FRAMED[["graph"]][[disease_i]][[year_i]] <- df %>%
      # make_tile_graph(bc_health_map, "censor3_single_suppression")

  }
}

# ----- augment-raw ------------------------------------------
# we can now create the final product of this implementation:
# a mirrow copy of `dto$raw` with a added column containing redaction decision
ls_temp <- dto$FRAMED$tuned # shell to fill with values
# loop through available diseases
for(disease_i in names(dto$FRAMED$tuned)){
  ls_temp[[disease_i]] <- 
    dto$FRAMED$redacted[[disease_i]] %>% dplyr::bind_rows()
}
(df_combined <- dplyr::bind_rows(ls_temp))
# now we can augment the original `raw` dataset with the redaction decision
dto$raw # remined the structure
dto[["augmented"]] <- dto$raw %>% 
  dplyr::left_join(
    df_combined,
    by = c("disease", "region", "region_desc", "year", "sex", "incase")
  )

# ---- save-to-disk ----------------
saveRDS(dto, paste0(path_save,".rds"))
# readr::write_csv(ds_long, paste0(path_save,".csv"))
lapply(dto, names)

# let us also save the flat file with with redaction recommendation augmented to it
readr::write_csv(dto$augmented, "./manipulation/prints/raw_augmented.csv")
