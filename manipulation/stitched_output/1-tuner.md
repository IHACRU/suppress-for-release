



This report was automatically generated with the R package **knitr**
(version 1.20).


```r
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./manipulation/1-tuner.R",
#   output = "./manipulation/stitched_output/1-tuner.md"
# )

# This script brings the source data into tidy format, widens it and prepares for testing
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
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
requireNamespace("tidyr", quietly=TRUE)
```

```r
# link to the source of the location mapping
path_input          <- "./data-unshared/derived/dto-0-greeted.rds"
path_fictional_case <- "./data-public/raw/fictional-cases/fictional-case-0.csv"
# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script
path_save <- "./data-unshared/derived/dto-1-tuned"
```

```r
# functions, the use of which is localized to this script
```

```r
dto <- readRDS(path_input) 

# Contents
# dto$raw            - dframe - flat data file as obtained from MoH
# dto$meta           - dframe - heirachical map and other meta information
# dto$target         - dframe - a fictional case of surveillance, target shape for mechanized suppression
# dto$FRAMED         - list   - a list, each element of which is disease*year  
# dto$FRAMED$raw     - dframe [L] deconstructed from `dto$raw` with each frame = disease * year

# To be added in this script:
# dto$FRAMED$cleaned - dframe [L] tidies values in `region` and `region_desc`
# dto$FRAMED$tuned   - dframe [W] spread into wide from `cleaned` and shape into decision frame

# this script will develop and apply the function that 
# brings `raw` form first into `cleaned` and then into `tuned` forms
```

```r
lapply(dto, names)
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
lapply(dto$FRAMED, names)
```

```
## $raw
## [1] "Flower Deafness" "Orange Nose"
```

```r
# select a unit for suppression decision; all transformation will be keyed to this shape
df <- dto$FRAMED$raw$`Flower Deafness`$`2000`
df %>% print(n = nrow(.))
```

```
## # A tibble: 96 x 6
##    disease         region  region_desc                   year sex   incase
##    <chr>           <chr>   <chr>                        <int> <chr>  <int>
##  1 Flower Deafness BC      BC                            2000 F      16737
##  2 Flower Deafness HA-01   01 Interior                   2000 F       2757
##  3 Flower Deafness HA-02   02 Fraser                     2000 F       5894
##  4 Flower Deafness HA-03   03 Vancouver Coastal          2000 F       3804
##  5 Flower Deafness HA-04   04 Vancouver Island           2000 F       3036
##  6 Flower Deafness HA-05   05 Northern                   2000 F       1180
##  7 Flower Deafness HA-99   Unknown HA                    2000 F         66
##  8 Flower Deafness HSDA-11 11 East Kootenay              2000 F        283
##  9 Flower Deafness HSDA-12 12 Kootenay Boundary          2000 F        271
## 10 Flower Deafness HSDA-13 13 Okanagan                   2000 F       1341
## 11 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   2000 F        862
## 12 Flower Deafness HSDA-21 21 Fraser East                2000 F       1147
## 13 Flower Deafness HSDA-22 22 Fraser North               2000 F       2101
## 14 Flower Deafness HSDA-23 23 Fraser South               2000 F       2646
## 15 Flower Deafness HSDA-31 31 Richmond                   2000 F        580
## 16 Flower Deafness HSDA-32 32 Vancouver                  2000 F       2314
## 17 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  2000 F        910
## 18 Flower Deafness HSDA-41 41 South Vancouver Island     2000 F       1410
## 19 Flower Deafness HSDA-42 42 Central Vancouver Island   2000 F       1105
## 20 Flower Deafness HSDA-43 43 North Vancouver Island     2000 F        521
## 21 Flower Deafness HSDA-51 51 Northwest                  2000 F        330
## 22 Flower Deafness HSDA-52 52 Northern Interior          2000 F        595
## 23 Flower Deafness HSDA-53 53 Northeast                  2000 F        255
## 24 Flower Deafness HSDA-99 Unknown HSDA                  2000 F         66
## 25 Flower Deafness BC      BC                            2000 M      14357
## 26 Flower Deafness HA-01   01 Interior                   2000 M       2226
## 27 Flower Deafness HA-02   02 Fraser                     2000 M       5271
## 28 Flower Deafness HA-03   03 Vancouver Coastal          2000 M       3363
## 29 Flower Deafness HA-04   04 Vancouver Island           2000 M       2380
## 30 Flower Deafness HA-05   05 Northern                   2000 M       1015
## 31 Flower Deafness HA-99   Unknown HA                    2000 M        102
## 32 Flower Deafness HSDA-11 11 East Kootenay              2000 M        203
## 33 Flower Deafness HSDA-12 12 Kootenay Boundary          2000 M        229
## 34 Flower Deafness HSDA-13 13 Okanagan                   2000 M       1046
## 35 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   2000 M        748
## 36 Flower Deafness HSDA-21 21 Fraser East                2000 M        996
## 37 Flower Deafness HSDA-22 22 Fraser North               2000 M       1808
## 38 Flower Deafness HSDA-23 23 Fraser South               2000 M       2467
## 39 Flower Deafness HSDA-31 31 Richmond                   2000 M        480
## 40 Flower Deafness HSDA-32 32 Vancouver                  2000 M       2139
## 41 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  2000 M        744
## 42 Flower Deafness HSDA-41 41 South Vancouver Island     2000 M       1087
## 43 Flower Deafness HSDA-42 42 Central Vancouver Island   2000 M        887
## 44 Flower Deafness HSDA-43 43 North Vancouver Island     2000 M        406
## 45 Flower Deafness HSDA-51 51 Northwest                  2000 M        254
## 46 Flower Deafness HSDA-52 52 Northern Interior          2000 M        524
## 47 Flower Deafness HSDA-53 53 Northeast                  2000 M        237
## 48 Flower Deafness HSDA-99 Unknown HSDA                  2000 M        102
## 49 Flower Deafness BC      BC                            2000 T      31103
## 50 Flower Deafness HA-01   01 Interior                   2000 T       4985
## 51 Flower Deafness HA-02   02 Fraser                     2000 T      11169
## 52 Flower Deafness HA-03   03 Vancouver Coastal          2000 T       7167
## 53 Flower Deafness HA-04   04 Vancouver Island           2000 T       5418
## 54 Flower Deafness HA-05   05 Northern                   2000 T       2196
## 55 Flower Deafness HA-99   Unknown HA                    2000 T        168
## 56 Flower Deafness HSDA-11 11 East Kootenay              2000 T        486
## 57 Flower Deafness HSDA-12 12 Kootenay Boundary          2000 T        500
## 58 Flower Deafness HSDA-13 13 Okanagan                   2000 T       2388
## 59 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   2000 T       1611
## 60 Flower Deafness HSDA-21 21 Fraser East                2000 T       2143
## 61 Flower Deafness HSDA-22 22 Fraser North               2000 T       3909
## 62 Flower Deafness HSDA-23 23 Fraser South               2000 T       5117
## 63 Flower Deafness HSDA-31 31 Richmond                   2000 T       1060
## 64 Flower Deafness HSDA-32 32 Vancouver                  2000 T       4453
## 65 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  2000 T       1654
## 66 Flower Deafness HSDA-41 41 South Vancouver Island     2000 T       2498
## 67 Flower Deafness HSDA-42 42 Central Vancouver Island   2000 T       1992
## 68 Flower Deafness HSDA-43 43 North Vancouver Island     2000 T        928
## 69 Flower Deafness HSDA-51 51 Northwest                  2000 T        584
## 70 Flower Deafness HSDA-52 52 Northern Interior          2000 T       1120
## 71 Flower Deafness HSDA-53 53 Northeast                  2000 T        492
## 72 Flower Deafness HSDA-99 Unknown HSDA                  2000 T        168
## 73 Flower Deafness BC      BC                            2000 U          9
## 74 Flower Deafness HA-01   01 Interior                   2000 U          4
## 75 Flower Deafness HA-02   02 Fraser                     2000 U          1
## 76 Flower Deafness HA-03   03 Vancouver Coastal          2000 U          0
## 77 Flower Deafness HA-04   04 Vancouver Island           2000 U          3
## 78 Flower Deafness HA-05   05 Northern                   2000 U          3
## 79 Flower Deafness HA-99   Unknown HA                    2000 U          0
## 80 Flower Deafness HSDA-11 11 East Kootenay              2000 U          0
## 81 Flower Deafness HSDA-12 12 Kootenay Boundary          2000 U          0
## 82 Flower Deafness HSDA-13 13 Okanagan                   2000 U          2
## 83 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   2000 U          4
## 84 Flower Deafness HSDA-21 21 Fraser East                2000 U          0
## 85 Flower Deafness HSDA-22 22 Fraser North               2000 U          0
## 86 Flower Deafness HSDA-23 23 Fraser South               2000 U          2
## 87 Flower Deafness HSDA-31 31 Richmond                   2000 U          0
## 88 Flower Deafness HSDA-32 32 Vancouver                  2000 U          0
## 89 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  2000 U          0
## 90 Flower Deafness HSDA-41 41 South Vancouver Island     2000 U          3
## 91 Flower Deafness HSDA-42 42 Central Vancouver Island   2000 U          0
## 92 Flower Deafness HSDA-43 43 North Vancouver Island     2000 U          4
## 93 Flower Deafness HSDA-51 51 Northwest                  2000 U          0
## 94 Flower Deafness HSDA-52 52 Northern Interior          2000 U          3
## 95 Flower Deafness HSDA-53 53 Northeast                  2000 U          0
## 96 Flower Deafness HSDA-99 Unknown HSDA                  2000 U          0
```

```r
# compare it to the shape we need it to be to apply mechanized suppression
dto$target
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
# now we will create a list object                  dto$tuned, 
# which will mirrow the tructure of                 dto$raw
# but will contain frames conformed to the shape of dto$target

# let's remind ourselves what we are doing
# dto$target # this is where we want the data to get
# create a stem to which one can attach the counts 
dstem <- dto$meta %>% 
  lookup_meta("hsda") %>% 
  dplyr::select(label_prov, label_ha, label_hsda)
# we will use this stem in tidy_frame() function

lapply(dto$FRAMED$raw, names)
```

```
## $`Flower Deafness`
## [1] "1999" "2000" "2001"
## 
## $`Orange Nose`
## [1] "2000" "2001"
```

```r
# start with the same structure, to be replaced with transformed frames
dto[["FRAMED"]][["cleaned"]] <- dto[["FRAMED"]][["raw"]] 
dto[["FRAMED"]][["tuned"]]   <- dto[["FRAMED"]][["raw"]] 
lapply(dto$FRAMED$tuned, names)
```

```
## $`Flower Deafness`
## [1] "1999" "2000" "2001"
## 
## $`Orange Nose`
## [1] "2000" "2001"
```

```r
for(disease_ in names(dto$FRAMED$raw)){
  # loop through available years
  for(year_ in names(dto$FRAMED$raw[[disease_]]) ){
    
    # create a long form to connect back to the raw
    dto$FRAMED$cleaned[[disease_]][[year_]] <- 
      dto$FRAMED$raw[[disease_]][[year_]] %>% 
      clean_raw(stem = dstem)
    
    # creat a wide form to connect to the logical test
    dto$FRAMED$tuned[[disease_]][[year_]] <- 
      dto$FRAMED$raw[[disease_]][[year_]] %>% 
      tidy_frame(stem = dstem)
  }
}
```

```
## Error in clean_raw(., stem = dstem): could not find function "clean_raw"
```

```r
# compare results
dto$FRAMED$raw$`Flower Deafness`$`1999` %>% print(n= nrow(.))
```

```
## # A tibble: 96 x 6
##    disease         region  region_desc                   year sex   incase
##    <chr>           <chr>   <chr>                        <int> <chr>  <int>
##  1 Flower Deafness BC      BC                            1999 F      16683
##  2 Flower Deafness HA-01   01 Interior                   1999 F       2693
##  3 Flower Deafness HA-02   02 Fraser                     1999 F       5945
##  4 Flower Deafness HA-03   03 Vancouver Coastal          1999 F       3847
##  5 Flower Deafness HA-04   04 Vancouver Island           1999 F       2973
##  6 Flower Deafness HA-05   05 Northern                   1999 F       1159
##  7 Flower Deafness HA-99   Unknown HA                    1999 F         66
##  8 Flower Deafness HSDA-11 11 East Kootenay              1999 F        268
##  9 Flower Deafness HSDA-12 12 Kootenay Boundary          1999 F        295
## 10 Flower Deafness HSDA-13 13 Okanagan                   1999 F       1277
## 11 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   1999 F        853
## 12 Flower Deafness HSDA-21 21 Fraser East                1999 F       1142
## 13 Flower Deafness HSDA-22 22 Fraser North               1999 F       2059
## 14 Flower Deafness HSDA-23 23 Fraser South               1999 F       2744
## 15 Flower Deafness HSDA-31 31 Richmond                   1999 F        608
## 16 Flower Deafness HSDA-32 32 Vancouver                  1999 F       2314
## 17 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  1999 F        925
## 18 Flower Deafness HSDA-41 41 South Vancouver Island     1999 F       1390
## 19 Flower Deafness HSDA-42 42 Central Vancouver Island   1999 F       1087
## 20 Flower Deafness HSDA-43 43 North Vancouver Island     1999 F        496
## 21 Flower Deafness HSDA-51 51 Northwest                  1999 F        316
## 22 Flower Deafness HSDA-52 52 Northern Interior          1999 F        612
## 23 Flower Deafness HSDA-53 53 Northeast                  1999 F        231
## 24 Flower Deafness HSDA-99 Unknown HSDA                  1999 F         66
## 25 Flower Deafness BC      BC                            1999 M      14341
## 26 Flower Deafness HA-01   01 Interior                   1999 M       2352
## 27 Flower Deafness HA-02   02 Fraser                     1999 M       5118
## 28 Flower Deafness HA-03   03 Vancouver Coastal          1999 M       3345
## 29 Flower Deafness HA-04   04 Vancouver Island           1999 M       2391
## 30 Flower Deafness HA-05   05 Northern                   1999 M       1041
## 31 Flower Deafness HA-99   Unknown HA                    1999 M         94
## 32 Flower Deafness HSDA-11 11 East Kootenay              1999 M        230
## 33 Flower Deafness HSDA-12 12 Kootenay Boundary          1999 M        284
## 34 Flower Deafness HSDA-13 13 Okanagan                   1999 M       1070
## 35 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   1999 M        768
## 36 Flower Deafness HSDA-21 21 Fraser East                1999 M        955
## 37 Flower Deafness HSDA-22 22 Fraser North               1999 M       1758
## 38 Flower Deafness HSDA-23 23 Fraser South               1999 M       2405
## 39 Flower Deafness HSDA-31 31 Richmond                   1999 M        535
## 40 Flower Deafness HSDA-32 32 Vancouver                  1999 M       2057
## 41 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  1999 M        753
## 42 Flower Deafness HSDA-41 41 South Vancouver Island     1999 M       1051
## 43 Flower Deafness HSDA-42 42 Central Vancouver Island   1999 M        925
## 44 Flower Deafness HSDA-43 43 North Vancouver Island     1999 M        415
## 45 Flower Deafness HSDA-51 51 Northwest                  1999 M        310
## 46 Flower Deafness HSDA-52 52 Northern Interior          1999 M        512
## 47 Flower Deafness HSDA-53 53 Northeast                  1999 M        219
## 48 Flower Deafness HSDA-99 Unknown HSDA                  1999 M         94
## 49 Flower Deafness BC      BC                            1999 T      31032
## 50 Flower Deafness HA-01   01 Interior                   1999 T       5046
## 51 Flower Deafness HA-02   02 Fraser                     1999 T      11065
## 52 Flower Deafness HA-03   03 Vancouver Coastal          1999 T       7194
## 53 Flower Deafness HA-04   04 Vancouver Island           1999 T       5367
## 54 Flower Deafness HA-05   05 Northern                   1999 T       2200
## 55 Flower Deafness HA-99   Unknown HA                    1999 T        160
## 56 Flower Deafness HSDA-11 11 East Kootenay              1999 T        498
## 57 Flower Deafness HSDA-12 12 Kootenay Boundary          1999 T        580
## 58 Flower Deafness HSDA-13 13 Okanagan                   1999 T       2347
## 59 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   1999 T       1621
## 60 Flower Deafness HSDA-21 21 Fraser East                1999 T       2097
## 61 Flower Deafness HSDA-22 22 Fraser North               1999 T       3818
## 62 Flower Deafness HSDA-23 23 Fraser South               1999 T       5150
## 63 Flower Deafness HSDA-31 31 Richmond                   1999 T       1143
## 64 Flower Deafness HSDA-32 32 Vancouver                  1999 T       4373
## 65 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  1999 T       1678
## 66 Flower Deafness HSDA-41 41 South Vancouver Island     1999 T       2442
## 67 Flower Deafness HSDA-42 42 Central Vancouver Island   1999 T       2014
## 68 Flower Deafness HSDA-43 43 North Vancouver Island     1999 T        911
## 69 Flower Deafness HSDA-51 51 Northwest                  1999 T        626
## 70 Flower Deafness HSDA-52 52 Northern Interior          1999 T       1124
## 71 Flower Deafness HSDA-53 53 Northeast                  1999 T        450
## 72 Flower Deafness HSDA-99 Unknown HSDA                  1999 T        160
## 73 Flower Deafness BC      BC                            1999 U          8
## 74 Flower Deafness HA-01   01 Interior                   1999 U          2
## 75 Flower Deafness HA-02   02 Fraser                     1999 U          2
## 76 Flower Deafness HA-03   03 Vancouver Coastal          1999 U          1
## 77 Flower Deafness HA-04   04 Vancouver Island           1999 U          1
## 78 Flower Deafness HA-05   05 Northern                   1999 U          0
## 79 Flower Deafness HA-99   Unknown HA                    1999 U          0
## 80 Flower Deafness HSDA-11 11 East Kootenay              1999 U          0
## 81 Flower Deafness HSDA-12 12 Kootenay Boundary          1999 U          2
## 82 Flower Deafness HSDA-13 13 Okanagan                   1999 U          0
## 83 Flower Deafness HSDA-14 14 Thompson Cariboo Shuswap   1999 U          0
## 84 Flower Deafness HSDA-21 21 Fraser East                1999 U          0
## 85 Flower Deafness HSDA-22 22 Fraser North               1999 U          2
## 86 Flower Deafness HSDA-23 23 Fraser South               1999 U          2
## 87 Flower Deafness HSDA-31 31 Richmond                   1999 U          0
## 88 Flower Deafness HSDA-32 32 Vancouver                  1999 U          4
## 89 Flower Deafness HSDA-33 33 North Shore/Coast Gariba~  1999 U          0
## 90 Flower Deafness HSDA-41 41 South Vancouver Island     1999 U          4
## 91 Flower Deafness HSDA-42 42 Central Vancouver Island   1999 U          4
## 92 Flower Deafness HSDA-43 43 North Vancouver Island     1999 U          0
## 93 Flower Deafness HSDA-51 51 Northwest                  1999 U          0
## 94 Flower Deafness HSDA-52 52 Northern Interior          1999 U          0
## 95 Flower Deafness HSDA-53 53 Northeast                  1999 U          0
## 96 Flower Deafness HSDA-99 Unknown HSDA                  1999 U          0
```

```r
dto$FRAMED$cleaned$`Flower Deafness`$`1999`
```

```
## # A tibble: 96 x 6
##    disease         region  region_desc           year sex   incase
##    <chr>           <chr>   <chr>                <int> <chr>  <int>
##  1 Flower Deafness BC      BC                    1999 F      16683
##  2 Flower Deafness HA-01   01 Interior           1999 F       2693
##  3 Flower Deafness HA-02   02 Fraser             1999 F       5945
##  4 Flower Deafness HA-03   03 Vancouver Coastal  1999 F       3847
##  5 Flower Deafness HA-04   04 Vancouver Island   1999 F       2973
##  6 Flower Deafness HA-05   05 Northern           1999 F       1159
##  7 Flower Deafness HA-99   Unknown HA            1999 F         66
##  8 Flower Deafness HSDA-11 11 East Kootenay      1999 F        268
##  9 Flower Deafness HSDA-12 12 Kootenay Boundary  1999 F        295
## 10 Flower Deafness HSDA-13 13 Okanagan           1999 F       1277
## # ... with 86 more rows
```

```r
dto$FRAMED$tuned$`Flower Deafness`$`1999`
```

```
## # A tibble: 96 x 6
##    disease         region  region_desc           year sex   incase
##    <chr>           <chr>   <chr>                <int> <chr>  <int>
##  1 Flower Deafness BC      BC                    1999 F      16683
##  2 Flower Deafness HA-01   01 Interior           1999 F       2693
##  3 Flower Deafness HA-02   02 Fraser             1999 F       5945
##  4 Flower Deafness HA-03   03 Vancouver Coastal  1999 F       3847
##  5 Flower Deafness HA-04   04 Vancouver Island   1999 F       2973
##  6 Flower Deafness HA-05   05 Northern           1999 F       1159
##  7 Flower Deafness HA-99   Unknown HA            1999 F         66
##  8 Flower Deafness HSDA-11 11 East Kootenay      1999 F        268
##  9 Flower Deafness HSDA-12 12 Kootenay Boundary  1999 F        295
## 10 Flower Deafness HSDA-13 13 Okanagan           1999 F       1277
## # ... with 86 more rows
```

```r
saveRDS(dto, paste0(path_save,".rds"))
# readr::write_csv(ds_long, paste0(path_save,".csv"))
lapply(dto, names)
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
## [1] "raw"     "cleaned" "tuned"
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
## [25] tidyr_0.8.1      purrr_0.2.5      readr_1.1.1      evaluate_0.10.1 
## [29] glue_1.3.0       stringi_1.1.7    compiler_3.4.4   pillar_1.2.1    
## [33] scales_1.0.0     markdown_0.8     pkgconfig_2.0.1
```

```r
Sys.time()
```

```
## [1] "2018-10-24 09:39:40 PDT"
```

