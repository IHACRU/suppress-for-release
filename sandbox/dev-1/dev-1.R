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
# source("./scripts/graphing/graph-presets.R") # font and color conventions
# ---- load-packages -----------------------------------------------------------
library(ggplot2)      #For graphing
library(dplyr)
library(magrittr)     #Pipes
library(rmarkdown)
# library(kableExtra)   # Currently absent
# library(DT)           # Current absent

# requireNamespace("readxl")
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables

# ---- declare-globals ---------------------------------------------------------
# link to the source of the location mapping

path_input   <- "./data-public/raw/fictional-case-1.csv"
# path_cdr_2014   <- "./data-unshared/derived/dto_10000.rds"
path_region_map <- "./data-public/raw/province_health_map.csv"
# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script

# ---- load-data ---------------------------------------------------------------
ds <- readr::read_csv(path_input)
bc_health_map <- readr::read_csv(path_region_map)
# pryr::object_size(ds)
# ds <- readr::read_csv(path_input) %>% as.data.frame() 

# ---- inspect-data-1 -----------------------------------------------------------
ds
bc_health_map %>% 
  arrange(id_ha, id_hsda, id_lha) %>%  
  knitr::kable() %>% 
  print(n = nrow(.))


# ---- tweak-data -----------------------------------------------

# ---- utility-functions -------------------------------------------------------

# ---- tweak-data -------------------------------------------------------------
# a suppression decision is made within a context: disease by year by geography
# To evaluate this decision we need to derive a data set, which provide such a context 
# We call this dataset a smallest analyzable subset of data
ds

# ----- design-filters ------------------------------------
subject <- list()

subject$observed <- ds 
d_observed <- subject$observed

# funtion to return the test whether a cell value is less than 5
detect_small_cell <- function(
  l # a list object containing observed counts for the decision context
){
  # l <- subject
  d <- l$observed
  # split varnames into two groups
  varnames <- names(d)
  count_variables <- grep("_[MFT]$",varnames, value = T)
  stem_variables <- setdiff(varnames, count_variables)
  d_count <- d %>% dplyr::select_(.dots = count_variables )
  # define the rule for detecting a small cell
  cellistoosmall <- function(x){
    toosmall <- ifelse(x >0 & x < 5, TRUE, FALSE)
  }
  d_small <- dplyr::bind_cols(
    d %>% dplyr::select_(.dots = stem_variables),
    d_count %>% dplyr::mutate_all(dplyr::funs(cellistoosmall))
  )
  l$small <- d_small
  return(l)
}
# usage
subject <- subject %>% detect_small_cell()
d_small <- subject$small


l <- subject

d <- d_observed




# function to prepare the smallest context for graphing
prepare_for_tiling <- function(d){
  # extract stable info
  disease = as.data.frame(d %>% dplyr::distinct(disease))[1,1]
  year    = as.data.frame(d %>% dplyr::distinct(year))[1,1]
  # remove stable info from the standard 
  d1 <- d %>% dplyr::select(-disease, -year)
  # split variables into counts and labels
  count_variables <- grep("_[MFT]$",names(d), value = T)
  label_variables <- setdiff(names(d1), count_variables)
  
  # record the row coordinate
  d_rows <- d1 %>% 
    dplyr::select_("label_hsda") %>% 
    dplyr::rename_("row_name" = "label_hsda") %>%
    tibble::rownames_to_column("row")%>% 
    dplyr::mutate(
      row = as.integer(row)
    ) #%>% as.data.frame()
  
  # record the column coordinate
  d_cols <- names(d1) %>% 
    tibble::as_tibble() %>% 
    dplyr::rename_("column_name" = "value") %>% 
    tibble::rownames_to_column("col") %>% 
    dplyr::mutate(
      col = as.integer(col)
    ) #%>% as.data.frame()
  
  # transform for tile graph
  d2 <- d1 %>% 
    tibble::rownames_to_column("row") %>% # record the row coordinate
    # dplyr::select_(.dots = elongation_variables) %>%
    tidyr::gather_("column_name","value",c(label_variables, count_variables)) %>% 
    dplyr::left_join(d_cols, by = "column_name") %>% 
    # dplyr::left_join(d_rows, by = "row_name") %>% 
    dplyr::mutate( 
      col = factor(
        col, 
        levels = as.data.frame(d_cols)[,"col"],
        labels = as.data.frame(d_cols)[,"column_name"] 
      ),
      row = factor(
        row, 
        levels = as.data.frame(d_rows)[,"row"],
        labels = as.data.frame(d_rows)[,"row_name"] 
      )
    ) %>%  
    dplyr::select(row, col, column_name, value)
  return(d2)
} # usage:
# d <- d_observed %>% prepare_for_tiling()

l <- subject
# function that graphs the toosmall decision
show_small_cells <- function(
  l # a list object containing observed counts for the decision context
){
  
  # d_small    <- l %>% detect_small_cell() # error
  # the values in these cells are labels, the row/col coordinates are new values
  d_observed <- l$observed
  d <- d_observed %>% prepare_for_tiling() 
  
  
  g <- d %>%  ggplot2::ggplot(aes(x = col,y = row))+
    geom_raster()+
    # geom_tile(fill = NA)+
    # geom_point(shape = NA)+
    geom_text(aes(label=value), color = "white")
  g
  
  d_observed %>% 
    ggplot(aes(x))
  
  
}
