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

path_input   <- "./data-public/raw/fictional-case-1.csv"
# path_cdr_2014   <- "./data-unshared/derived/dto_10000.rds"
path_region_map <- "./data-public/raw/province_health_map.csv"

baseSize = 10
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
ds <- ds %>% 
  dplyr::mutate(
    label_pr = "BC"
  ) %>% 
  dplyr::select(disease, year, label_pr, dplyr::everything())
names(ds) <- gsub("^BC_","PR_", names(ds))

bc_health_map <- bc_health_map %>% 
  dplyr::rename(
    id_pr = id_prov
  ) %>% 
  dplyr::mutate(
    label_pr = "BC"
  ) %>% 
  dplyr::select(id_pr, id_ha, id_hsda, id_lha,
                label_pr, label_ha, label_hsda, label_lha,
                dplyr::everything()) %>% 
  dplyr::select(-label_prov)
  

# ---- utility-functions -------------------------------------------------------
# function returning a look up table for HA level of aggregaton
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

# function to elongate a SAU
elongate_values <- function( 
  d # a standard SAU: disease-year-labels-values
  # ,meta
  ,regex = "_[MTF]$"
){
  d_wide <- d
  
  # obtain lookup tables from the meta-data object  
  # lkp_hsda <- meta %>% lookup_meta("hsda")
  # lkp_ha <- meta %>% lookup_meta("ha")
  # 
  # split variables into counts and labels
  (count_variables <- grep(regex, names(d_wide), value = T))
  (label_variables <- setdiff(names(d_wide), count_variables))
  
  d_long <- d_wide %>% 
    tidyr::gather_("column_name","value",c( count_variables)) %>%
    dplyr::mutate(
      agg_level = gsub("^(\\w+)_(\\w+)$", "\\1", column_name)
      ,sex       = gsub("^(\\w+)_(\\w+)$", "\\2", column_name)
      # ,agg_level = gsub("^BC$","PR",agg_level)
      # ,label_hsda = factor(label_hsda, levels = lkp_hsda$label_hsda)
      # ,label_ha   = factor(label_ha,   levels = lkp_ha$label_ha)
      # ,label_hsda = factor(label_hsda, levels = rev(levels(label_hsda)) )
      # ,label_ha   = factor(label_ha,   levels = rev(levels(label_ha)))
    ) #%>%
    # dplyr::arrange(desc(label_ha), desc(label_hsda))
    # dplyr::arrange(label_ha, label_hsda)
  
  return(d_long)
}
# usage
ds_long_values <-  ds %>% elongate_values()


# function to elongate
elongate_labels <- function(
  d # a standard SAU: disease-year-labels-values
  # ,meta
  ,varnames # names of the variables, which need to be elongated
){
  
  d_wide <- d
  # meta <- bc_health_map
  # obtain lookup tables from the meta-data object  
  # lkp_hsda <- meta %>% lookup_meta("hsda")
  # lkp_ha   <- meta %>% lookup_meta("ha")
  
  # split variables into counts and labels
  (count_variables <- grep("_[MFT]$",names(d_wide), value = T))
  (label_variables <- varnames)
  
  ######### LEFT PANEL
  # create data set for the left panel of the graph (labels of health boundries)
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
      # ,label_hsda = factor(label_hsda, levels = lkp_hsda$label_hsda)
      # ,label_ha   = factor(label_ha,   levels = lkp_ha$label_ha)
      # ,label_hsda = factor(label_hsda, levels = rev(levels(label_hsda)) )
      # ,label_ha   = factor(label_ha,   levels = rev(levels(label_ha)))
    ) #%>% 
    # dplyr::arrange(desc(label_ha), desc(label_hsda))
    # dplyr::arrange(label_ha, label_hsda)
}
# usage
# d_long_labels <- ds %>% elongate_labels(c("label_pr", "label_ha","label_hsda"))


# ---- tweak-data -------------------------------------------------------------
# a suppression decision is made within a context: disease by year by geography
# To evaluate this decision we need to derive a data set, which provide such a context 
# We call this dataset a smallest analyzable subset of data
ds

# ----- graphing-settings ----------------------- 
# the gnesting structure, groupings,  and color definitions
# are defined in the meta-data 
bc_health_map



# ----- logical-filters ------------------------------------

# funtion to return the test whether a cell value is less than 5
# TEST 1: What cells are `too small` ( < 5)
detect_small_cell <- function(
  d # a standard SAU: disease-year-labels-values
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


# TEST 2: What cells can help calculated suppressed cells from the same HSDA?
# reverse calculate from:
detect_recalc_hsda <- function(
  d # a standard SAU: disease-year-labels-values
){
  # d <- ds
  # split varnames into two groups
  (varnames <- names(d))
  (count_variables <- grep("_[MFT]$",varnames, value = T)) # which ends with `_F` or `_M` or `_T`
  (stem_variables <- setdiff(varnames, count_variables)) 
  
  d_small_cell <- d %>% detect_small_cell()
  
  d2 <- d_small_cell %>% 
    elongate_values()  
    # dplyr::mutate(
    #   agg_level = gsub("(\\w+)_(\\w+)","\\1", column_name )
      # sex       = gsub("(\\w+)_(\\w+)","\\2", column_name )
    # ) %>%
  
  d3 <- d2 %>% 
    dplyr::group_by(label_ha, label_hsda, agg_level) %>% 
    dplyr::mutate( 
      group_sum = sum(value)
    ) %>%  
    dplyr::ungroup() %>% 
    dplyr::group_by(label_ha, label_hsda, agg_level) %>% 
    dplyr::mutate(
      value_new = ifelse( group_sum >= 1, TRUE, value),
      sum_new = sum(value_new)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(label_ha, label_hsda, agg_level)
  
  d4 <- d3 %>% 
    dplyr::mutate(
      value = value_new
    ) %>% 
  dplyr::select_(.dots = setdiff( names(d2), c("agg_level","sex") ) ) %>% 
    tidyr::spread(column_name, value) 
  return(d4)   
  
} 
# usage
# d4 <- ds %>% recalc_from_hsda()

# TEST: is this HSDA the only one in its HA that has been suppressed?
detect_single_suppression <- function(
  d
){
  
  d_small <- d %>% detect_small_cell()
  
  
}
# usage
  d_single_suppression <- detect_single_suppression()

# ----- graphing-functions --------------------
# function to prepare the smallest context for graphing by geom_tile
# input = disease-by-year ds, output = list object with meaningful components
prepare_for_tiling <- function(
  d     # a standard SAU: disease-year-labels-values
  ,meta # meta-data file contains BC health boundries heirarchy and other definitions
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

  #(3)######## Create data for theRIGHT PANEL
  # the right panel will contain only numbers (FMT counts of variable selected for suppression)
  d_long_values <- d %>% elongate_values(regex = "_[MTF]$") %>% 
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
     # ,"observed" = list(
       ,"wide"     = d_wide
       ,"labels_long" = d_long_labels 
       ,"values_long" = d_long_values 
      # )
   )
   return(l)
} # used in make_tile_graph()
# usage:
# l <- ds %>% prepare_for_tiling(bc_health_map)

# function that graphs the toosmall decision
make_tile_graph <- function(
  d   # a dataset containing observed counts for the decision context
  ,meta # a meta data object containing grouping and coloring settings
  ,...
){
  # d <- ds # turn on for testing, if needed
  # meta <- bc_health_map
  
  # maybe later
  # font_size_left  <- baseSize + (font_size - baseSize) 
  # font_size_right <- font_size_left + right_size_adjustment
    
  l <- d %>% prepare_for_tiling(meta)
  
  # graph labels - LEFT SIDE OF THE TABLET
  # graph the data
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
    axis.text.x =  element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x  =  element_blank(),
    # panel.grid.major.y  =  element_blank(),
    legend.position="left"
  )
  g <- g + guides(color=FALSE)
  g <- g + labs(x=NULL, y=NULL)
  g_labels <- g
  g_labels 
  
  # graph values - RIGHT SIDE OF THE TABLET   
  # main_title = "Right side title"
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
  g <- g + geom_tile(fill = "grey99")
  # g <- g + geom_text(size = baseSize-7, hjust=.4)
  # g <- g + geom_text(size = fontsize_right, hjust=.5)
  g <- g + geom_text(hjust=.5, ...)
  g <- g + facet_grid(. ~ agg_level )
  # g <- g + scale_y_discrete(limits=rev(cog_measures_sorted_domain))
  # g <- g + scale_color_manual(values=domain_colors_text)
  # g <- g + scale_fill_manual(values=domain_colors_fill)
  # g <- g + annotate(geom="text", size=baseSize, hjust = 1.5, label="XXXX \n SSSSS",x=Inf, y=Inf)
  # g <- g + theme1
  g <- g + theme_minimal()
  g <- g + theme(
    axis.text.x =  element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x  =  element_blank(),
    panel.grid.minor.x  =  element_blank(),
    panel.grid.major.y  =  element_blank(),
    panel.grid.minor.y  =  element_blank(),
    legend.position="left"
  )
  g <- g + guides(color=FALSE)
  g <- g + labs(x=NULL, y=NULL)
  
  g_values <- g
  g_values 
  
  # combine sides into a single display
  
  grid::grid.newpage()    
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(nrow=2, ncol=2,
                              widths=grid::unit(c(.4, .6) ,c("null","null")),
                              heights=grid::unit(c(.05, .95), c("null","null"))
  )
  grid::pushViewport(grid::viewport(layout=layout))
  main_title <-paste0(toupper(l$disease)," - ", l$year)
  
  grid::grid.text(main_title, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(g_labels,  vp=grid::viewport(layout.pos.row=2, layout.pos.col=1 ))
  print(g_values, vp=grid::viewport(layout.pos.row=2, layout.pos.col=2 ))
  grid::popViewport(0)
  return(grid::popViewport(0))
  
} # usage
# ds %>% make_tile_graph(bc_health_map)


print_tile_graph <- function(d,meta,...){
  
  path_save = "./sandbox/dev-1/temp-example3.png"
  png(filename = path_save, width = 900, height = 500,res = 100)
  
  d %>% make_tile_graph(meta,...)
  
  dev.off()
  
}
#usage
ds %>% print_tile_graph(bc_health_map, size = 3)
# detect_small_cell() %>% 
  

# function that adds results of the test onto the tile graph

# ----- workflow --------------------------

ds
l <- list(
  "observed" = ds %>% prepare_for_tiling(bc_health_map),
  "small_cell" = ds %>% detect_small_cell() %>% prepare_for_tiling(bc_health_map),
  "recalc_hsda" = ds %>% detect_recalc_hsda() %>% prepare_for_tiling(bc_health_map)
) 


####################

# function to prepare the smallest context for graphing
# prepare_for_tiling <- function(d){
# extract stable info
disease = as.data.frame(d %>% dplyr::distinct(disease))[1,1]
year    = as.data.frame(d %>% dplyr::distinct(year))[1,1]
# remove stable info from the standard 
d1 <- d %>% dplyr::select(-disease, -year) %>% 
  dplyr::mutate(label_pr = "BC") %>% 
  dplyr::select(label_pr, dplyr::everything())
# split variables into counts and labels
(count_variables <- grep("_[MFT]$",names(d), value = T))
(label_variables <- setdiff(names(d1), count_variables))

# record the row coordinate / define authoritive sorting for  HSDA
d_rows <- d1 %>% 
  dplyr::select_("label_hsda") %>% 
  dplyr::rename_("row_name" = "label_hsda") %>%
  tibble::rownames_to_column("row")%>% 
  dplyr::mutate(
    row = as.integer(row)
  ) #%>% as.data.frame()

# record the column coordinate / define authorit
d_cols <- names(d1) %>% 
  tibble::as_tibble() %>% 
  dplyr::rename_("column_name" = "value") %>% 
  tibble::rownames_to_column("col") %>% 
  dplyr::mutate(
    col = as.integer(col)
  ) #%>% as.data.frame()




