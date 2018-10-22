



This report was automatically generated with the R package **knitr**
(version 1.20).


```r
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./manipulation/0-greeter.R",
#   output = "./manipulation/stitched_output/0-greeter.md"
# )

# This script inputs the raw data and prepares it for tuning.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
```

```r
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
source("./manipulation/function-support.R")  # assisting functions for data wrangling and testing
source("./manipulation/object-glossary.R")   # object definitions
source("./scripts/common-functions.R")       # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions
```

```r
library(magrittr) # pipes
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("readr", quietly=TRUE)
requireNamespace("testit", quietly=TRUE)
```

```r
# declare the location of the data sources to be used in this script
path_input          <- "./data-public/raw/fictional-input-from-MoH.csv"
path_region_map     <- "./data-public/raw/bc-health-system-map.csv"
path_fictional_case <- "./data-public/raw/fictional-cases/fictional-case-0.csv"
# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script
path_save           <- "./data-unshared/derived/dto-0-greeted"
```

```r
# functions, the use of which is localized to this script
```

```r
ds             <- readr::read_csv(path_input) %>% as.data.frame() %>% tibble::as_tibble()
```

```
## Parsed with column specification:
## cols(
##   DISEASE = col_character(),
##   REGION = col_character(),
##   REGION_DESC = col_character(),
##   YEAR = col_integer(),
##   SEX = col_character(),
##   INCASE = col_integer()
## )
```

```r
bc_health_map  <- readr::read_csv(path_region_map)
```

```
## Parsed with column specification:
## cols(
##   id_prov = col_integer(),
##   id_ha = col_integer(),
##   id_hsda = col_integer(),
##   id_lha = col_integer(),
##   label_lha = col_character(),
##   label_hsda = col_character(),
##   label_ha = col_character(),
##   label_prov = col_character(),
##   color_ha = col_character(),
##   color_ha_label = col_character(),
##   color_hsda = col_character(),
##   color_hsda_label = col_character(),
##   color_prov = col_character(),
##   color_prov_label = col_character()
## )
```

```r
fictional_case <- readr::read_csv(path_fictional_case)
```

```
## Parsed with column specification:
## cols(
##   disease = col_character(),
##   year = col_integer(),
##   label_prov = col_character(),
##   label_ha = col_character(),
##   label_hsda = col_character(),
##   BC_F = col_integer(),
##   BC_M = col_integer(),
##   BC_T = col_integer(),
##   HA_F = col_integer(),
##   HA_M = col_integer(),
##   HA_T = col_integer(),
##   HSDA_F = col_integer(),
##   HSDA_M = col_integer(),
##   HSDA_T = col_integer()
## )
```

```r
# Contents
# This is our starting point at which we begin to assemble our 
# (d)ata (t)ransfer (u)nit or `dto`, as we will refer to it in the future

# Components available initially:
# ds                 - dframe - flat data file as obtained from MoH
# bc_health_map      - dframe - heirachical map and other meta information
# fictional_case     - dframe - a fictional case of surveillance, target shape for mechanized suppression

# To be created in this script:
# dto$raw            - dframe - flat data file as obtained from MoH
# dto$meta           - dframe - heirachical map and other meta information
# dto$target         - dframe - a fictional case of surveillance, target shape for mechanized suppression
# dto$FRAMED         - list   - a list, each element of which is (DISEASE*YEAR) = FRAME 
# dto$FRAMED$raw     - dframe [L] deconstructed `dto$raw` with each frame = disease * year

# this script kick-offs a data transfer object and
# introduces the FRAME as the focal structre in the analysis
# the FRAME contains all data relevant to surveillance of (DISEASE*YEAR) unit
```

```r
# surveillance file from MoH comes as a flat .csv with
# each row = disease * year * locale
ds %>% dplyr::glimpse()
```

```
## Observations: 480
## Variables: 6
## $ DISEASE     <chr> "Flower Deafness", "Flower Deafness", "Flower Deaf...
## $ REGION      <chr> "BC", "BC", "BC", "BC", "BC", "BC", "BC", "BC", "B...
## $ REGION_DESC <chr> "BC", "BC", "BC", "BC", "BC", "BC", "BC", "BC", "B...
## $ YEAR        <int> 1999, 1999, 1999, 1999, 2000, 2000, 2000, 2000, 20...
## $ SEX         <chr> "T", "U", "M", "F", "T", "U", "M", "F", "T", "U", ...
## $ INCASE      <int> 31032, 8, 14341, 16683, 31103, 9, 14357, 16737, 29...
```

```r
# we have created a logical map of heirarchical map of HSDA -> HA with some meta added
bc_health_map %>% dplyr::glimpse()
```

```
## Observations: 89
## Variables: 14
## $ id_prov          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ id_ha            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
## $ id_hsda          <int> 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 1...
## $ id_lha           <int> 1, 2, 3, 4, 5, 18, 6, 7, 9, 10, 11, 12, 13, 1...
## $ label_lha        <chr> "Fernie", "Cranbrook", "Kimberley", "Winderme...
## $ label_hsda       <chr> "East Kootenay", "East Kootenay", "East Koote...
## $ label_ha         <chr> "Interior", "Interior", "Interior", "Interior...
## $ label_prov       <chr> "BC", "BC", "BC", "BC", "BC", "BC", "BC", "BC...
## $ color_ha         <chr> "#4daf4a", "#4daf4a", "#4daf4a", "#4daf4a", "...
## $ color_ha_label   <chr> "green", "green", "green", "green", "green", ...
## $ color_hsda       <chr> "#edf8e9", "#edf8e9", "#edf8e9", "#edf8e9", "...
## $ color_hsda_label <chr> "pale green", "pale green", "pale green", "pa...
## $ color_prov       <chr> "#ffffff", "#ffffff", "#ffffff", "#ffffff", "...
## $ color_prov_label <chr> "white", "white", "white", "white", "white", ...
```

```r
# we can take a direct subset of metadata to limit our focus by granularity of the smallest unit
bc_health_map_hsda <- bc_health_map %>% 
  dplyr::select(-id_lha, -label_lha) %>%  # this level is not supported at this time
  dplyr::distinct() %>% 
  print(n = nrow(.))
```

```
## # A tibble: 16 x 12
##    id_prov id_ha id_hsda label_hsda        label_ha    label_prov color_ha
##      <int> <int>   <int> <chr>             <chr>       <chr>      <chr>   
##  1       0     1      11 East Kootenay     Interior    BC         #4daf4a 
##  2       0     1      12 Kootenay Boundary Interior    BC         #4daf4a 
##  3       0     1      13 Okanagan          Interior    BC         #4daf4a 
##  4       0     1      14 Thompson Cariboo~ Interior    BC         #4daf4a 
##  5       0     2      21 Fraser East       Fraser      BC         #e41a1c 
##  6       0     2      22 Fraser North      Fraser      BC         #e41a1c 
##  7       0     2      23 Fraser South      Fraser      BC         #e41a1c 
##  8       0     3      31 Richmond          Vancouver ~ BC         #377eb8 
##  9       0     3      32 Vancouver         Vancouver ~ BC         #377eb8 
## 10       0     3      33 North Shore/Coas~ Vancouver ~ BC         #377eb8 
## 11       0     4      41 South Vancouver ~ Vancouver ~ BC         #984ea3 
## 12       0     4      42 Central Vancouve~ Vancouver ~ BC         #984ea3 
## 13       0     4      43 North Vancouver ~ Vancouver ~ BC         #984ea3 
## 14       0     5      51 Northwest         Northern    BC         #ff7f00 
## 15       0     5      52 Northern Interior Northern    BC         #ff7f00 
## 16       0     5      53 Northeast         Northern    BC         #ff7f00 
## # ... with 5 more variables: color_ha_label <chr>, color_hsda <chr>,
## #   color_hsda_label <chr>, color_prov <chr>, color_prov_label <chr>
```

```r
# or better yet, let us create a lookup table for the selected level of granularity
# using a custom function to shape the file for future use
(lkp_hsda <- lookup_meta(meta = bc_health_map, agg_level = "hsda"))
```

```
## # A tibble: 16 x 8
##    id_prov id_ha id_hsda label_prov label_ha    label_hsda      color_hsda
##      <int> <int>   <int> <chr>      <chr>       <chr>           <chr>     
##  1       0     1      11 BC         Interior    East Kootenay   #edf8e9   
##  2       0     1      12 BC         Interior    Kootenay Bound~ #bae4b3   
##  3       0     1      13 BC         Interior    Okanagan        #74c476   
##  4       0     1      14 BC         Interior    Thompson Carib~ #238b45   
##  5       0     2      21 BC         Fraser      Fraser East     #fcae91   
##  6       0     2      22 BC         Fraser      Fraser North    #fb6a4a   
##  7       0     2      23 BC         Fraser      Fraser South    #cb181d   
##  8       0     3      31 BC         Vancouver ~ Richmond        #bdd7e7   
##  9       0     3      32 BC         Vancouver ~ Vancouver       #6baed6   
## 10       0     3      33 BC         Vancouver ~ North Shore/Co~ #2171b5   
## 11       0     4      41 BC         Vancouver ~ South Vancouve~ #cbc9e2   
## 12       0     4      42 BC         Vancouver ~ Central Vancou~ #9e9ac8   
## 13       0     4      43 BC         Vancouver ~ North Vancouve~ #6a51a3   
## 14       0     5      51 BC         Northern    Northwest       #fdbe85   
## 15       0     5      52 BC         Northern    Northern Inter~ #fd8d3c   
## 16       0     5      53 BC         Northern    Northeast       #d94701   
## # ... with 1 more variable: color_hsda_label <chr>
```

```r
# this is the structure that we will use to think through the logical tests
# and how they can be encoded into scripted commands
# compare it to a manually constructed fictional case
fictional_case
```

```
## # A tibble: 16 x 14
##    disease   year label_prov label_ha  label_hsda   BC_F  BC_M  BC_T  HA_F
##    <chr>    <int> <chr>      <chr>     <chr>       <int> <int> <int> <int>
##  1 Flower ~  1995 BC         Interior  East Koote~   104    97   201    25
##  2 Flower ~  1995 BC         Interior  Kootenay B~   104    97   201    25
##  3 Flower ~  1995 BC         Interior  Okanagan      104    97   201    25
##  4 Flower ~  1995 BC         Interior  Thompson C~   104    97   201    25
##  5 Flower ~  1995 BC         Fraser    Fraser East   104    97   201    15
##  6 Flower ~  1995 BC         Fraser    Fraser Nor~   104    97   201    15
##  7 Flower ~  1995 BC         Fraser    Fraser Sou~   104    97   201    15
##  8 Flower ~  1995 BC         Vancouve~ Richmond      104    97   201    20
##  9 Flower ~  1995 BC         Vancouve~ Vancouver     104    97   201    20
## 10 Flower ~  1995 BC         Vancouve~ North Shor~   104    97   201    20
## 11 Flower ~  1995 BC         Vancouve~ South Vanc~   104    97   201    19
## 12 Flower ~  1995 BC         Vancouve~ Central Va~   104    97   201    19
## 13 Flower ~  1995 BC         Vancouve~ North Vanc~   104    97   201    19
## 14 Flower ~  1995 BC         Northern  Northwest     104    97   201    25
## 15 Flower ~  1995 BC         Northern  Northern I~   104    97   201    25
## 16 Flower ~  1995 BC         Northern  Northeast     104    97   201    25
## # ... with 5 more variables: HA_M <int>, HA_T <int>, HSDA_F <int>,
## #   HSDA_M <int>, HSDA_T <int>
```

```r
(names(ds) <- tolower(names(ds)))
```

```
## [1] "disease"     "region"      "region_desc" "year"        "sex"        
## [6] "incase"
```

```r
ds <- ds %>% 
  dplyr::arrange(sex, region)
# what does the data look at this point for a single frame of analysis?
ds %>% 
  dplyr::filter(
    disease ==  "Flower Deafness" # disease + year = FRAME
    ,year    ==  "2001"            # disease + year = FRAME
  ) %>% 
  print(n=nrow(.))
```

```
## # A tibble: 96 x 6
##    disease         region  region_desc                   year sex   incase
##    <chr>           <chr>   <chr>                        <int> <chr>  <int>
##  1 Flower Deafness BC      BC                            2001 F      15691
##  2 Flower Deafness HA-01   01 Interior                   2001 F       2447
##  3 Flower Deafness HA-02   02 Fraser                     2001 F       5727
##  4 Flower Deafness HA-03   03 Vancouver Coastal          2001 F       3535
##  5 Flower Deafness HA-04   04 Vancouver Island           2001 F       2813
##  6 Flower Deafness HA-05   05 Northern                   2001 F       1111
##  7 Flower Deafness HA-99   Unknown HA                    2001 F         58
##  8 Flower Deafness HSDA-11 11 East Kootenay              2001 F        205
##  9 Flower Deafness HSDA-12 12 Kootenay Boundary          2001 F        249
## 10 Flower Deafness HSDA-13 13 Okanagan                   2001 F       1213
## 11 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   2001 F        780
## 12 Flower Deafness HSDA-21 21 Fraser East                2001 F       1087
## 13 Flower Deafness HSDA-22 22 Fraser North               2001 F       1972
## 14 Flower Deafness HSDA-23 23 Fraser South               2001 F       2668
## 15 Flower Deafness HSDA-31 31 Richmond                   2001 F        476
## 16 Flower Deafness HSDA-32 32 Vancouver                  2001 F       2183
## 17 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  2001 F        876
## 18 Flower Deafness HSDA-41 41 South Vancouver Island     2001 F       1352
## 19 Flower Deafness HSDA-42 42 Central Vancouver Island   2001 F        995
## 20 Flower Deafness HSDA-43 43 North Vancouver Island     2001 F        466
## 21 Flower Deafness HSDA-51 51 Northwest                  2001 F        280
## 22 Flower Deafness HSDA-52 52 Northern Interior          2001 F        633
## 23 Flower Deafness HSDA-53 53 Northeast                  2001 F        198
## 24 Flower Deafness HSDA-99 Unknown HSDA                  2001 F         58
## 25 Flower Deafness BC      BC                            2001 M      13722
## 26 Flower Deafness HA-01   01 Interior                   2001 M       1992
## 27 Flower Deafness HA-02   02 Fraser                     2001 M       5232
## 28 Flower Deafness HA-03   03 Vancouver Coastal          2001 M       3216
## 29 Flower Deafness HA-04   04 Vancouver Island           2001 M       2199
## 30 Flower Deafness HA-05   05 Northern                   2001 M        988
## 31 Flower Deafness HA-99   Unknown HA                    2001 M         95
## 32 Flower Deafness HSDA-11 11 East Kootenay              2001 M        172
## 33 Flower Deafness HSDA-12 12 Kootenay Boundary          2001 M        187
## 34 Flower Deafness HSDA-13 13 Okanagan                   2001 M       1004
## 35 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   2001 M        629
## 36 Flower Deafness HSDA-21 21 Fraser East                2001 M       1026
## 37 Flower Deafness HSDA-22 22 Fraser North               2001 M       1791
## 38 Flower Deafness HSDA-23 23 Fraser South               2001 M       2415
## 39 Flower Deafness HSDA-31 31 Richmond                   2001 M        448
## 40 Flower Deafness HSDA-32 32 Vancouver                  2001 M       2010
## 41 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  2001 M        758
## 42 Flower Deafness HSDA-41 41 South Vancouver Island     2001 M        967
## 43 Flower Deafness HSDA-42 42 Central Vancouver Island   2001 M        845
## 44 Flower Deafness HSDA-43 43 North Vancouver Island     2001 M        387
## 45 Flower Deafness HSDA-51 51 Northwest                  2001 M        271
## 46 Flower Deafness HSDA-52 52 Northern Interior          2001 M        512
## 47 Flower Deafness HSDA-53 53 Northeast                  2001 M        205
## 48 Flower Deafness HSDA-99 Unknown HSDA                  2001 M         95
## 49 Flower Deafness BC      BC                            2001 T      29415
## 50 Flower Deafness HA-01   01 Interior                   2001 T       4439
## 51 Flower Deafness HA-02   02 Fraser                     2001 T      10961
## 52 Flower Deafness HA-03   03 Vancouver Coastal          2001 T       6751
## 53 Flower Deafness HA-04   04 Vancouver Island           2001 T       5012
## 54 Flower Deafness HA-05   05 Northern                   2001 T       2099
## 55 Flower Deafness HA-99   Unknown HA                    2001 T        153
## 56 Flower Deafness HSDA-11 11 East Kootenay              2001 T        377
## 57 Flower Deafness HSDA-12 12 Kootenay Boundary          2001 T        436
## 58 Flower Deafness HSDA-13 13 Okanagan                   2001 T       2217
## 59 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   2001 T       1409
## 60 Flower Deafness HSDA-21 21 Fraser East                2001 T       2114
## 61 Flower Deafness HSDA-22 22 Fraser North               2001 T       3764
## 62 Flower Deafness HSDA-23 23 Fraser South               2001 T       5083
## 63 Flower Deafness HSDA-31 31 Richmond                   2001 T        924
## 64 Flower Deafness HSDA-32 32 Vancouver                  2001 T       4193
## 65 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  2001 T       1634
## 66 Flower Deafness HSDA-41 41 South Vancouver Island     2001 T       2319
## 67 Flower Deafness HSDA-42 42 Central Vancouver Island   2001 T       1840
## 68 Flower Deafness HSDA-43 43 North Vancouver Island     2001 T        853
## 69 Flower Deafness HSDA-51 51 Northwest                  2001 T        551
## 70 Flower Deafness HSDA-52 52 Northern Interior          2001 T       1145
## 71 Flower Deafness HSDA-53 53 Northeast                  2001 T        403
## 72 Flower Deafness HSDA-99 Unknown HSDA                  2001 T        153
## 73 Flower Deafness BC      BC                            2001 U          1
## 74 Flower Deafness HA-01   01 Interior                   2001 U          0
## 75 Flower Deafness HA-02   02 Fraser                     2001 U          3
## 76 Flower Deafness HA-03   03 Vancouver Coastal          2001 U          0
## 77 Flower Deafness HA-04   04 Vancouver Island           2001 U          0
## 78 Flower Deafness HA-05   05 Northern                   2001 U          0
## 79 Flower Deafness HA-99   Unknown HA                    2001 U          0
## 80 Flower Deafness HSDA-11 11 East Kootenay              2001 U          0
## 81 Flower Deafness HSDA-12 12 Kootenay Boundary          2001 U          0
## 82 Flower Deafness HSDA-13 13 Okanagan                   2001 U          0
## 83 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   2001 U          0
## 84 Flower Deafness HSDA-21 21 Fraser East                2001 U          3
## 85 Flower Deafness HSDA-22 22 Fraser North               2001 U          4
## 86 Flower Deafness HSDA-23 23 Fraser South               2001 U          0
## 87 Flower Deafness HSDA-31 31 Richmond                   2001 U          0
## 88 Flower Deafness HSDA-32 32 Vancouver                  2001 U          0
## 89 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  2001 U          0
## 90 Flower Deafness HSDA-41 41 South Vancouver Island     2001 U          0
## 91 Flower Deafness HSDA-42 42 Central Vancouver Island   2001 U          0
## 92 Flower Deafness HSDA-43 43 North Vancouver Island     2001 U          0
## 93 Flower Deafness HSDA-51 51 Northwest                  2001 U          0
## 94 Flower Deafness HSDA-52 52 Northern Interior          2001 U          0
## 95 Flower Deafness HSDA-53 53 Northeast                  2001 U          0
## 96 Flower Deafness HSDA-99 Unknown HSDA                  2001 U          0
```

```r
# before we do anything with it, it is important or reflect that 
# THIS is the state of the data that should result at the end of mechanized suppression
# the whole point, the TARGET deliverable is just one additional column/field
# logical vector, indicating decision to suppress (TRUE) or not to suppress (FALSE)
```

```r
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

dto[["FRAMED"]] <- list() # a shell to hold the frames of different content
dto[["FRAMED"]][["raw"]] <- list() # the first element will break up the raw data into digestable frame

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
sapply(dto, names) 
```

```
## $raw
## [1] "disease"     "region"      "region_desc" "year"        "sex"        
## [6] "incase"     
## 
## $meta
##  [1] "id_prov"          "id_ha"            "id_hsda"         
##  [4] "id_lha"           "label_lha"        "label_hsda"      
##  [7] "label_ha"         "label_prov"       "color_ha"        
## [10] "color_ha_label"   "color_hsda"       "color_hsda_label"
## [13] "color_prov"       "color_prov_label"
## 
## $target
##  [1] "disease"    "year"       "label_prov" "label_ha"   "label_hsda"
##  [6] "BC_F"       "BC_M"       "BC_T"       "HA_F"       "HA_M"      
## [11] "HA_T"       "HSDA_F"     "HSDA_M"     "HSDA_T"    
## 
## $FRAMED
## [1] "raw"
```

```r
sapply(greeted_list, names) 
```

```
## $`Flower Deafness`
## [1] "1999" "2000" "2001"
## 
## $`Orange Nose`
## [1] "2000" "2001"
```

```r
# this list object in which the smallest unit is the analytic frame: disease * year
dto[["FRAMED"]][["raw"]] <- greeted_list

sapply(dto$FRAMED, names) 
```

```
##      raw              
## [1,] "Flower Deafness"
## [2,] "Orange Nose"
```

```r
sapply(dto$FRAMED$raw, names) 
```

```
## $`Flower Deafness`
## [1] "1999" "2000" "2001"
## 
## $`Orange Nose`
## [1] "2000" "2001"
```

```r
saveRDS(dto, paste0(path_save,".rds"))
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 3.4.4 (2018-03-15)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows >= 8 x64 (build 9200)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
## [1] bindrcpp_0.2.2     magrittr_1.5       RColorBrewer_1.1-2
## [4] dichromat_2.0-0    ggplot2_3.0.0     
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.18     knitr_1.20       bindr_0.1.1      hms_0.4.1       
##  [5] tidyselect_0.2.3 munsell_0.5.0    testit_0.8       colorspace_1.3-2
##  [9] R6_2.2.2         rlang_0.2.2      highr_0.6        stringr_1.3.1   
## [13] plyr_1.8.4       dplyr_0.7.6      tools_3.4.4      gtable_0.2.0    
## [17] utf8_1.1.3       cli_1.0.0        withr_2.1.1      yaml_2.1.19     
## [21] lazyeval_0.2.1   assertthat_0.2.0 tibble_1.4.2     crayon_1.3.4    
## [25] purrr_0.2.5      readr_1.1.1      evaluate_0.10.1  glue_1.3.0      
## [29] stringi_1.1.7    compiler_3.4.4   pillar_1.2.1     scales_1.0.0    
## [33] markdown_0.8     pkgconfig_2.0.1
```

```r
Sys.time()
```

```
## [1] "2018-10-21 10:26:00 PDT"
```

