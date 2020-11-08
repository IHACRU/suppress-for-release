# knitr::stitch_rmd(script="./manipulation/0-ellis-map.R", output="./manipulation/stitched-output/0-ellis-map.md")
# This script reads two files: encounter counts with location mapping and encounter timelines for selected individuals
cat("\014")
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
library(magrittr) # pipes
requireNamespace("dplyr")
requireNamespace("readr")
requireNamespace("testit")
requireNamespace("tidyr")
requireNamespace("rmarkdown")

# ---- declare-globals ---------------------------------------------------------
# link to the source of the location mapping
# path_input <- "./data-public/raw/fictional-cases/fictional-case-0.csv"
path_input <- "./data-public/raw/fictional-cases/flower-deafness-all-years.csv"
path_region_map <- "./data-public/raw/bc-health-system-map.csv"

baseSize = 10
# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script

# ---- load-data ---------------------------------------------------------------
# ds0 <- readr::read_csv(path_input_folder)
ds0 <- readr::read_csv(path_input)
bc_health_map <- readr::read_csv(path_region_map)
# source here because `bc_health_map` must be availabe to define the functions
base::source("./scripts/suppression-functions-2-targeted.R")  # mechanized suppression of small cells


# # save this example for future reference in other repos
# list(
#   "bc_data" = ds0,
#   "bc_health_map" = bc_health_map
# ) %>% saveRDS("../../dss-ialh/graph-making-scenarios/data-public/raw/scenario-2/example-data.rds")

# ---- inspect-data-1 -----------------------------------------------------------
ds0 %>% dplyr::filter(year==1995)

bc_health_map %>% lookup_meta("prov")
bc_health_map %>% lookup_meta("ha")
bc_health_map %>% lookup_meta("hsda")

# ---- tweak-data -----------------------------------------------
# a suppression decision is made within a context of suppression frame = disease * year 
# To evaluate this decision we need to derive a data set, which provide such a context 
# We call this dataset a `suppression-ready` data frame 
# it is the precise context for mechanized suppression
df <- ds0 %>% dplyr::filter(year == 1995)
df 
d <- df # another alias to use for testing functions
dview <- df;# View(dview) # inspect before proceding
# IMPORTANT NOTE: the subsequent functions rely on this shape of data
# note that it is different from 
ds0
# which contains ALL years of observation, whereas df contains a single year


# ----- logical-tests ---------------------------

# TEST 1: What cells are `too small` ( < 5)
# Censor 1: What cells should be suppressed as "too small"?
d1_small_cell <- df %>% detect_small_cell()
# creates a replica of the data, with count values are replaced by TRUE/FALSE according to test

# TEST 2: What cells can help calculate the suppressed cells from the same triple?
# because we need to remove them, otherwise they make recalculation possible.
# Censor 2: What triples should be suppressed? (eg. F-M-T)
# reverse calculate from:
d2_recalc_from_triplet <- df %>% detect_recalc_triplet()

# TEST 3: Is this is the only triplet that is being suppressed in a higher order block?
# because if yes, recalculation is possible
# Censor 3: What cells should be suppressed as those that could be calculated from higher order count?
d3_single_suppression <- df %>% detect_single_suppression()


d3_single_suppression_draconian <- df %>% detect_single_suppression_draconian()

# ---- service-functions ------------------------
# function to elongate the VALUE (count) in the smallest decision frame
d_long_values <-  df %>% elongate_values()

# function to elongate the LABEL (name) in the smallest decision frame
d_long_labels <- df %>% elongate_labels(c("label_prov", "label_ha","label_hsda"))

# create color scale to highlight suppression decisions
d_colors <- bc_health_map %>% make_color_scale()

# apply sequential logical tests to suppress desired cells
d_combined_tests <- df %>% combine_logical_tests()

# # save this example for future reference in other repos
# list(
#   "raw_wide" = df,
#   "redacted_long" = d_combined_tests,
#   "bc_health_map" = bc_health_map
# ) %>% saveRDS("../../dss-ialh/graph-making-scenarios/data-public/raw/scenario-2/example-data.rds")

# ---- graphing-functions ------------------------
base::source("./scripts/suppression-functions-2-targeted.R")  # mechanized suppression of small cells
source("./sandbox/examiner-3/by-steps.R") # overwrites `make_tile_graph()` to allow step-by-step printing (customization for IPDLN-2020 presentation)

# prepare the context for suppression = smallest decision frame
# create a list object containing required data in required shape to generate graphs
l <- df %>% prepare_for_tiling(bc_health_map)

# generate a graph of a single logical test
df %>% make_tile_graph(bc_health_map)

# it is very useful to segregate how
# (1) a plot is assembled with graphing script from how
# (2) a plot is committed to a hard digital form (PNG, JPG, PDF) 
# can help us avoid going insane from trying to make it look right/useful on paper/screen
# there are many decision about the appearance of the plot that needs to be scripted
df %>% print_tile_graph(bc_health_map, path_folder = "./sandbox/examiner-3/prints/",steps_displayed = 4L )
df %>% print_tile_graph(bc_health_map, path_folder = "./sandbox/examiner-3/prints/",steps_displayed = 3L )
df %>% print_tile_graph(bc_health_map, path_folder = "./sandbox/examiner-3/prints/",steps_displayed = 2L )
df %>% print_tile_graph(bc_health_map, path_folder = "./sandbox/examiner-3/prints/",steps_displayed = 1L )
df %>% print_tile_graph(bc_health_map, path_folder = "./sandbox/examiner-3/prints/",steps_displayed = 0L )
# Note: Individual steps need to be created only for demonstration purposes, so they created files are renamed manually. 

# so far, df referred to a single Data Frame = a context for a single suppression decision
# we can use a wrapper function to loop through  a large number of frames

# 
df %>% print_tile_graph(bc_health_map,"./sandbox/examiner-3/prints/")
# ---- application ----------------------------
ds0 %>% # notice that it takes the file with ALL suppression framed
  print_one_frame(
    disease_ = "Flower Deafness"
    ,year_   =  1995
    ,folder  = "./sandbox/examiner-3/prints/"
  )

# to print multiple cases, we will use a for-loop to cycle through possible values

print_folder = "./sandbox/examiner-3/prints/"
# determine the list of diseases for which incidence counts are available
diseases_available <- ds0 %>%  # or dto[["raw"]]
  dplyr::distinct(disease) %>% 
  as.list() %>% unlist() %>% as.character()

# loop through available diseases
for(disease_i in diseases_available){
  # determine the years for which data is available for this disease
  years_available <- ds0 %>% 
    dplyr::filter(disease == disease_i) %>% 
    dplyr::arrange(year) %>% 
    dplyr::distinct(year) %>% 
    as.list() %>% unlist() %>% as.character()
  print_folder_disease = paste0(print_folder,"/",disease_i,"/")
  dir.create(print_folder_disease)
  
  # loop through available years
  for(year_i in years_available ){
    d1 <- ds0 %>% 
      dplyr::filter(disease == disease_i) %>% 
      dplyr::filter(year    == year_i)
    # for each observable disease*year frame, produce suppression decision graph
    d1 %>% print_one_frame(disease_ = disease_i, year_ = year_i, folder = print_folder_disease)
  }
}

