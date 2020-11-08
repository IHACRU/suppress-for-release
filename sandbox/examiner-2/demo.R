
ds0           <- readr::read_csv("./data-public/raw/fictional-cases/flower-deafness-all-years.csv")
bc_health_map <-  readr::read_csv("./data-public/raw/bc-health-system-map.csv")

# a suppression decision is made within a context of suppression frame = disease * year 
# To evaluate this decision we need to derive a data set, which provide such a context 
# We call this dataset a `suppression-ready` data frame 
# it is the precise context for mechanized suppression
df <- ds0 %>% dplyr::filter(year == 1995)

# load custom fuctions for mechanized suppression
base::source("./scripts/suppression-functions-2-targeted.R")  

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

# ---- service-functions ------------------------

# function to elongate the VALUE (count) in the smallest decision frame
d_long_values <-  df %>% elongate_values()

# function to elongate the LABEL (name) in the smallest decision frame
d_long_labels <- df %>% elongate_labels(c("label_prov", "label_ha","label_hsda"))

# create color scale to highlight suppression decisions
d_colors <- bc_health_map %>% make_color_scale()

# apply sequential logical tests to suppress desired cells
d_combined_tests <- df %>% combine_logical_tests()

# ---- graphing-functions ------------------------

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
df %>% print_tile_graph(bc_health_map, path_folder = "./sandbox/examiner-2/prints/", size = 3)

# so far, df referred to a single Data Frame = a context for a single suppression decision
# we can use a wrapper function to loop through  a large number of frames

# ---- application ----------------------------
ds0 %>% # notice that it takes the file with ALL suppression framed
  print_one_frame(
    disease_ = "Flower Deafness"
    ,year_   =  1995
    ,folder  = "./sandbox/examiner-2/prints/"
  )

# to print multiple cases, we will use a for-loop to cycle through possible values

print_folder = "./sandbox/examiner-2/prints/"
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
