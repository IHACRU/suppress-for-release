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

path_input_folder  <- "./data-public/raw/"

# path_cdr_2014   <- "./data-unshared/derived/dto_10000.rds"
path_region_map <- "./data-public/raw/province_health_map.csv"

baseSize = 10
# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script

# ---- load-data ---------------------------------------------------------------
# ds0 <- readr::read_csv(path_input_folder)
bc_health_map <- readr::read_csv(path_region_map)
# pryr::object_size(ds)
# ds <- readr::read_csv(path_input) %>% as.data.frame() 

# load the fictional cases created with the help of ./data-public/raw/proto_data_generator.xlsx
path_input_files <- grep("fictional-case-\\d+.csv$", list.files(path_input_folder, full.names = T), value = T)
l <- list()
for(i in seq_along(path_input_files)){
  l[[i]] <- readr::read_csv(path_input_files[i])
}
l
ds0 <- dplyr::bind_rows(l, .id = "case")

# ---- inspect-data-1 -----------------------------------------------------------
ds0 %>% filter(case==1)
bc_health_map %>% 
  arrange(id_ha, id_hsda, id_lha) %>%  
  knitr::kable() %>% 
  print(n = nrow(.))


# ---- tweak-data -----------------------------------------------
# ideally, these tweaks would be implemented higher upstream, in the ellis island
# view these corrections as temporary, needed to connect the workflows
ds0 <- ds0 %>% 
  dplyr::mutate(
    label_pr = "BC"
  ) %>% 
  dplyr::select(
    case, disease, year, label_pr, label_ha, label_hsda,
    HSDA_F,HSDA_M,HSDA_T,HA_F, HA_F, HA_M, HA_T,  BC_F, BC_M, BC_T 
  )
names(ds0) <- gsub("^BC_","PR_", names(ds0))

bc_health_map <- bc_health_map %>% 
  dplyr::rename(
    id_pr = id_prov
  ) %>% 
  dplyr::mutate(
    label_pr = "BC"
  ) %>% 
  dplyr::select(
    id_pr, id_ha, id_hsda, id_lha,
                label_pr, label_ha, label_hsda, label_lha,
                dplyr::everything()) %>% 
  dplyr::select(-label_prov)

# select data to work with for development
ds <- ds0 %>% filter(case ==2) %>% select(-case)
  

# ---- utility-functions -------------------------------------------------------
# function returning a look up table for a given level of aggregation
lookup_meta <- function(
  meta       # meta-data file contains BC health boundries heirarchy and other definitions
  ,agg_level = "hsda"  #
){
  if(agg_level == "hsda"){
    lookup_table <- meta %>% 
      dplyr::distinct(id_ha, id_hsda, label_ha, label_hsda, color_hsda, color_hsda_label) %>% 
      dplyr::select(id_ha, id_hsda, label_ha, dplyr::everything()) %>% 
      dplyr::arrange(id_ha, id_hsda)
  }
  if(agg_level == "ha"){
    lookup_table <- meta %>% 
      dplyr::distinct(id_ha, label_ha, color_ha, color_ha_label) %>% 
      dplyr::arrange(id_ha)
  }
  return(lookup_table)
}
# usage
# lkp_hsda <- bc_health_map %>% lookup_meta("hsda")
# lkp_ha <- bc_health_map %>% lookup_meta("ha")

# function to elongate a smallest dataframe unit
elongate_values <- function( 
   d                 # dataframe with a single Decision Unit
  ,regex = "_[MTF]$" # regular expression used to select variables with counts
){
  d_wide <- d # reminder that a wide format is expected
  # split variables into counts and labels
  (count_variables <- grep(regex, names(d_wide), value = T))
  (label_variables <- setdiff(names(d_wide), count_variables))
  # convert from wide to long 
  d_long <- d_wide %>% 
    tidyr::gather_("column_name","value",c( count_variables)) %>%
    dplyr::mutate( # split column_name into individual variables
       agg_level = gsub("^(\\w+)_(\\w+)$", "\\1", column_name) # aggregation level
      ,sex       = gsub("^(\\w+)_(\\w+)$", "\\2", column_name)
    ) %>%
    dplyr::select_(.dots =
      c(label_variables, "column_name","agg_level","sex", "value")
    )
  return(d_long)
}
# usage
# ds_long_values <-  ds %>% elongate_values()


# function to elongate
elongate_labels <- function(
   d # a standard SAU: disease-year-labels-values
  ,varnames # names of the variables (aggregation levels), which need to be elongated
){
  d_wide <- d # reminder that a wide format is expected
  # split variables into counts and labels
  (count_variables <- grep("_[MFT]$",names(d_wide), value = T))
  (label_variables <- varnames)
  # create a duplicate dataframe that will contain values of the labels
  d_label_values <- d_wide %>% 
    dplyr::select_(.dots = label_variables)
  names(d_label_values) <- gsub("^label_", "value_", names(d_label_values))
  # the intent is to print the following grid of values as a graph:
  d_label_values
  # now we construct a long dataset from which the graph will be printed
  d_long_labels <- d_wide %>%
    dplyr::select_(.dots = label_variables) %>% 
    dplyr::bind_cols(d_label_values) %>% 
    tidyr::gather_("agg_level", "value", gsub("^label_", "value_",label_variables)) %>% 
    dplyr::mutate(  
       agg_level  = gsub("^value_","",agg_level)
      ,agg_level  = toupper(agg_level)
      ,agg_level  = factor(agg_level,  levels = c("PR","HA","HSDA")) 
    )
}
# usage
# d_long_labels <- ds %>% elongate_labels(c("label_pr", "label_ha","label_hsda"))


# ---- inspect-data-2 -------------------------------------------------------------
# a suppression decision is made within a context: disease by year by geography
# To evaluate this decision we need to derive a data set, which provide such a context 
# We call this dataset a smallest analyzable subset of data
ds 
# IMPORTANT NOTE: the subsequent functions rely on this shape

# note that it is different from 
ds0
# which contains ALL cases, whereas ds contains a single case

# ----- graphing-settings ----------------------- 
# the gnesting structure, groupings,  and color definitions
# are defined in the meta-data 
bc_health_map
# IMPORTANT NOTE: the subsequent functions rely on existance of this object


# ----- logical-filters ------------------------------------

# funtion to return the test whether a cell value is less than 5
# TEST 1: What cells are `too small` ( < 5)
# Censor 1: What cells should be suppressed as "too small"?
detect_small_cell <- function(
  d # smallest decision unit: disease-year-labels-values
){
  # split varnames into two groups
  (varnames <- names(d))
  (count_variables <- grep("_[MFT]$",varnames, value = T)) # which ends with `_F` or `_M` or `_T`
  (stem_variables <- setdiff(varnames, count_variables)) 

  # define the rule for detecting a small cell
  cellistoosmall <- function(x){
    toosmall <- ifelse(x >0 & x < 5, TRUE, FALSE)
  }
  d_small <- dplyr::bind_cols(
    d %>% dplyr::select_(.dots = stem_variables),        # 1st argument
    d %>% dplyr::select_(.dots = count_variables ) %>%   # 2nd arguemnt
      dplyr::mutate_all(dplyr::funs(cellistoosmall))     
  )
  return(d_small)
}
# usage
# d_small_cell <- ds %>% detect_small_cell()
# creates a replica of the data, with count values are replaced by TRUE/FALSE according to test

# TEST 2: What cells can help calculated suppressed cells from the same triple?
# Censor 2: What triples should be suppressed? (eg. F-M-T)
# reverse calculate from:
detect_recalc_triplet <- function(
  d # smallest decision unit: disease-year-labels-values
){
  # d <- ds
  # split varnames into two groups
  (varnames <- names(d))
  (count_variables <- grep("_[MFT]$",varnames, value = T)) # which ends with `_F` or `_M` or `_T`
  (stem_variables <- setdiff(varnames, count_variables)) 
  
  d1 <- d  %>% detect_small_cell()
  
  # alt
  d2 <- d1 %>% 
    dplyr::mutate(
      HSDA_F = ifelse(HSDA_M,TRUE, HSDA_F),
      HSDA_M = ifelse(HSDA_F,TRUE, HSDA_M),
      HA_M   = ifelse(HA_F,TRUE, HA_M),
      HA_F   = ifelse(HA_M,TRUE, HA_F),
      PR_M   = ifelse(PR_F,TRUE, PR_M),
      PR_F   = ifelse(PR_M,TRUE, PR_F)
    )
 return(d2)   
} 
# usage
d2 <- ds %>% detect_recalc_triplet()

# TEST 3: Is this is the only triple that is being suppressed in a higher order block?
# Censor 3: What cells should be suppressed as those that could be calculated from higher order count?
detect_single_suppression <- function(
  d
){
  # d <- ds0 %>% filter(case ==2) %>% select(-case) 
  (varnames <- names(d))
  (count_variables <- grep("_[MFT]$",varnames, value = T)) # which ends with `_F` or `_M` or `_T`
  (stem_variables <- setdiff(varnames, count_variables)) 
  ########### Single suppression at HSDA level
  d1 <- d %>% detect_recalc_triplet()
  d2 <- d1 %>% 
    dplyr::group_by(label_ha) %>% 
    dplyr::mutate(
       n_sup = sum(HSDA_F & HSDA_M) # both must be TRUE to count as 1
      ,n_tot = sum(HSDA_T) # must be TRUE to count
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # if totale is TRUE then M and F are too
      HSDA_T = ifelse(n_tot == 1,TRUE,HSDA_T ),
      # if one or total is TRUE then other should be too  
      HSDA_F = ifelse(n_sup == 1 | HSDA_T, TRUE, HSDA_F ), 
      HSDA_M = ifelse(n_sup == 1 | HSDA_T, TRUE, HSDA_M )
    ) %>% 
    dplyr::select(-n_sup, -n_tot)
  ########### Single suppression at HA level
  d3 <- d2 %>% 
    dplyr::group_by(label_pr) %>%
    dplyr::mutate(
      n_sup = sum(HA_F & HA_M) # both must be TRUE to count as 1
     ,n_tot = sum(HA_T) # must be TRUE to count
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # if totale is TRUE then M and F are too
      HA_T = ifelse(n_tot == 1,TRUE,HA_T ),
      # if one or total is TRUE then other should be too  
      HA_F = ifelse(n_sup == 1 | HA_T, TRUE, HA_F ), 
      HA_M = ifelse(n_sup == 1 | HA_T, TRUE, HA_M )
      ) %>% 
    dplyr::select(-n_sup)
 
   # Enforce original sorting order (optional)
  # d4 <- d3 %>% 
  #   elongate_values() %>% 
  #   dplyr::mutate(
  #     column_name = factor(column_name, levels = count_variables)
  #   ) %>% 
  #   dplyr::select(-agg_level, -sex) %>% 
  #   tidyr::spread(column_name, value)
  
  return(d3)  
}
# usage
# d_single_suppression <- d %>% detect_single_suppression()

# ----- graphing-functions --------------------
combine_censors <- function(
  d
){
  # d <- ds
  # create a list object with progressive stages through chain of decisions/censors
  l <- list(
    "observed"                   = d                                 
   ,"censor1_small_cell"         = d %>% detect_small_cell()         
   ,"censor2_recalc_triplet"     = d %>% detect_recalc_triplet()    
   ,"censor3_single_suppression" = d %>% detect_single_suppression() 
  )

  combined_list <- list()
  vals <- list()
  for(i in names(l) ){
    combined_list[[i]]   <- l[[i]] %>% elongate_values()
    vals[[i]] <- combined_list[[i]]$value
  }
  dvals <- vals %>% dplyr::bind_cols()
  dd <- combined_list[["observed"]] %>% 
    dplyr::mutate(censor0=FALSE) %>% 
    dplyr::bind_cols(dvals) %>% 
    dplyr::select(-observed)
  return(dd)
}
# usage
# dd <- ds %>% combine_censors()

# function to prepare the smallest context for graphing by geom_tile
# input = disease-by-year ds, output = list object with meaningful components
prepare_for_tiling <- function(
  d     # a standard SAU: disease-year-labels-values
  ,meta=bc_health_map # meta-data file contains BC health boundries heirarchy and other definitions
){
  # d <- ds
  # meta <- bc_health_map
  #(1)######## Establish the reference definitions from the common meta data object
  # obtain lookup tables from the meta-data object  
  lkp_hsda <- meta %>% lookup_meta("hsda")
  lkp_ha <- meta %>% lookup_meta("ha")
  # extract stable info
  disease = as.data.frame(d %>% dplyr::distinct(disease))[1,1]
  year    = as.data.frame(d %>% dplyr::distinct(year))[1,1]
  # remove stable info from the standard input format
  d_wide <- d %>% dplyr::select(-disease, -year) #%>% 
    # dplyr::mutate(label_pr = "BC") %>% # add manually, for balance
    # dplyr::select(label_pr, dplyr::everything()) # sort
  # the object `d_wide` is now the stem for sebsequent operations
    # split variables into counts and labels
  (count_variables <- grep("_[MFT]$",names(d_wide), value = T))
  (label_variables <- setdiff(names(d_wide), count_variables))
  # d_wide %>% print(n = nrow(.))
 
  #(2)######## Create data for the LEFT PANEL
  # create data set for the left panel of the graph (labels of health boundries)
  # d_long_labels <- d %>% elongate_labels(c("label_ha","label_hsda")) %>% 
  d_long_labels <- d %>% elongate_labels(label_variables) %>% 
    dplyr::mutate(   
       label_hsda = factor(label_hsda, levels = lkp_hsda$label_hsda) 
      ,label_ha   = factor(label_ha,   levels = lkp_ha$label_ha)
      ,label_hsda = factor(label_hsda, levels = rev(levels(label_hsda)) )
      ,label_ha   = factor(label_ha,   levels = rev(levels(label_ha)))
    ) %>% 
    # dplyr::arrange(desc(label_ha), desc(label_hsda))
    dplyr::arrange(label_ha, label_hsda)
    # inspect if needed
  # d_long_labels %>% print(n=nrow(.))

  #(3)######## Create data for the RIGHT PANEL
  # the right panel will contain only numbers (FMT counts of variable selected for suppression)
  # d_long_values <- d %>% elongate_values(regex = "_[MTF]$") %>% 
  d_long_values <- d %>% combine_censors() %>% 
    dplyr::mutate(   
      label_hsda = factor(label_hsda, levels = lkp_hsda$label_hsda) 
      ,label_ha   = factor(label_ha,   levels = lkp_ha$label_ha)
      ,label_hsda = factor(label_hsda, levels = rev(levels(label_hsda)) )
      ,label_ha   = factor(label_ha,   levels = rev(levels(label_ha)))
    ) %>% 
    # dplyr::arrange(desc(label_ha), desc(label_hsda))
    dplyr::arrange(label_ha, label_hsda)
    # inspect if needed
  # d_long_values %>% print(n=nrow(.))
  
  #(4)######## Aseemble the output = list object
   l <- list(
     "disease"   = disease
     ,"year"     = year
     ,"labels_long" = d_long_labels 
     ,"values_long" = d_long_values 
    )
   return(l)
} # used in make_tile_graph()
# usage:
# l <- ds %>% prepare_for_tiling(bc_health_map)

# function that graphs the toosmall decision
make_tile_graph <- function(
  d   # a dataset containing observed counts for the decision context
  ,meta=bc_health_map# a meta data object containing grouping and coloring settings
  ,censor 
  ,...
){
  # d <- ds # turn on for testing, if needed
  # meta <- bc_health_map
  
  censor_labels <- c(
        "censor0"                     = "- Observed counts"
      , "censor1_small_cell"          = "- Censor (1) Small cell?"
      , "censor2_recalc_triplet"      = "- Censor (2) Recalculate from triplet?"
      , "censor3_single_suppression"  = "- Censor (3) Single Suppression?"
  )
  
  # maybe later
  # font_size_left  <- baseSize + (font_size - baseSize) 
  # font_size_right <- font_size_left + right_size_adjustment
    
  l <- d %>% prepare_for_tiling(meta)
  
  ##--##--##--##--##--##--##--##--##--##--##
  # graph the labels - LEFT SIDE OF THE TABLET
  g <- l$labels_long %>%  
    dplyr::mutate(dummy = "") %>% 
    ggplot2::ggplot(
      aes_string(
         x     = "dummy"
        ,y     = "label_hsda"
        ,label = "value" 
      )
    )
  g <- g + geom_tile(fill = "grey99")
  g <- g + facet_grid(.~agg_level)
  # g <- g + geom_text(size = baseSize-7, hjust =.5)
  g <- g + geom_text(hjust =.5, ...)
  g <- g + theme_minimal()
  g <- g + theme(
    # axis.text.x =  element_blank(),
    axis.text.x =  element_text(color = "white"),
    axis.text.y         = element_blank(),
    axis.ticks          = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor.y  = element_blank(),
    legend.position="left"
  )
  g <- g + guides(color=FALSE)
  g <- g + labs(x=NULL, y=NULL)
  g_labels <- g
  g_labels 
  
  ##--##--##--##--##--##--##--##--##--##--##
  # graph the values - RIGHT SIDE OF THE TABLET   
  g <- l$values_long %>%  
    dplyr::mutate(
      agg_level = factor(agg_level, levels = c("HSDA","HA","PR"))
    ) %>% 
    ggplot2::ggplot(
      aes_string(
         x     = "sex"
        ,y     = "label_hsda"
        ,label = "value" 
      )
    )
  g <- g + geom_tile(aes_string(fill = censor))
  # g <- g + geom_text(size = baseSize-7, hjust=.4)
  # g <- g + geom_text(size = fontsize_right, hjust=.5)
  # g <- g + geom_text(aes(color = censor1_small_cell),hjust=.5, ...)
  g <- g + geom_text(aes_string(color = censor),hjust=.5)
  g <- g + facet_grid(. ~ agg_level )
  g <- g + scale_fill_manual(values = c("TRUE"="black", "FALSE"="white"))
  g <- g + scale_color_manual(values = c("TRUE"="white", "FALSE"="black"))
  
  # g <- g + scale_y_discrete(limits=rev(cog_measures_sorted_domain))
  # g <- g + scale_color_manual(values=domain_colors_text)
  # g <- g + scale_fill_manual(values=domain_colors_fill)
  # g <- g + annotate(geom="text", size=baseSize, hjust = 1.5, label="XXXX \n SSSSS",x=Inf, y=Inf)
  # g <- g + theme1
  g <- g + theme_minimal()
  g <- g + theme(
    # axis.text.x         =  element_blank(),
    axis.text.x         = element_text(color = "grey50"),
    axis.text.y         = element_blank(),
    axis.ticks          = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor.y  = element_blank(),
    legend.position="left"
  )
  g <- g + guides(color=FALSE, fill=FALSE)
  g <- g + labs(x=NULL, y=NULL)
  
  g_values <- g
  g_values 
  
  # combine sides into a single display
  
  grid::grid.newpage()    
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(nrow=2, ncol=2,
                              widths=grid::unit(c(.5, .5) ,c("null","null")),
                              heights=grid::unit(c(.05, .95), c("null","null"))
  )
  grid::pushViewport(grid::viewport(layout=layout))
  main_title <-paste0(toupper(l$disease)," - ", l$year," ", censor_labels[censor] )
  
  grid::grid.text(main_title, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1), just = "left")
  print(g_labels,  vp=grid::viewport(layout.pos.row=2, layout.pos.col=1 ))
  print(g_values, vp=grid::viewport(layout.pos.row=2, layout.pos.col=2 ))
  grid::popViewport(0)
  return(grid::popViewport(0))
  
} # usage
# ds %>% make_tile_graph(bc_health_map)


print_tile_graph <- function(
  d
  ,meta=bc_health_map
  ,path_folder = "./sandbox/dev-1/prints/"
  ,...
){
  # vector containing the list of graphs to create (e.i. censors to apply)
  censor_vector <- c(
     "censor0" 
    , "censor1_small_cell"
    , "censor2_recalc_triplet"
    , "censor3_single_suppression"
  )
  
  disease <- as.data.frame(d)[1,1]
  year    <- as.data.frame(d)[1,2]
  
  for(i in seq_along(censor_vector)){
    path_save = paste0(path_folder,disease,"-",year,"-censor-",i-1,".png")
    png(filename = path_save, width = 900, height = 500,res = 100)
    d %>% make_tile_graph(meta,censor = censor_vector[i],...)
    dev.off()
  }

}
#usage
# ds %>% print_tile_graph(bc_health_map, size = 3)

# wrapper function to print one case  
print_one_case <- function(d,folder="./sandbox/dev-1/prints/", selected_case,...){
  d %>% 
    filter(case==selected_case) %>% 
    select(-case) %>%
    print_tile_graph(path_folder=folder,...)
  
}
#usage
# ds0 %>% print_one_case(selected_case = 4)

# ----- workflow --------------------------
# the script loads the fictional examples from ./data-public/raw/ folder
# place all fictional case you want to graph there, as separate csvs
# this scripts reads all csv files that start with "fictional-case-" and
# assembles them into a data frame
ds0
ds0 %>% distinct(case) 
# you can isolate a case 
ds <- ds0 %>% 
  filter(case==1) %>% 
  select(-case)
# to pass to the graphing funtion:
# ds %>%
#   print_tile_graph(path_folder = "./sandbox/dev-1/prints/")

# or use a wrapper function to print directly from ds0
# ds0 %>% print_one_case(selected_case=2)

# to print multiple cases, use a for loop
for(i in 1:6){
# for(i in c(2,5){
  ds0 %>% print_one_case(selected_case=i)
}

