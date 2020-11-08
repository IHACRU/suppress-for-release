# ---- cleaning-and-tuning -------------------------------

# function to carry out a full join among all components of the list object
full_join_multi <- function(list_object){
  # list_object <- datas[["physical"]][["161"]]
  d <- list_object %>%
    Reduce(function(dtf1,dtf2) dplyr::full_join(dtf1,dtf2), .)
}

# function to split up `region` and `region_desc` into three variables
clean_raw <- function(
  d      # a single data frame from the raw element
  ,stem  # a foundation for assembling individual frames used in analysis
){
  # NOTE: this should be identical to the first two steps of `tidy_frame()`
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
  return(d2)
}

# function to transform the analytic frame into target shape
tidy_frame <- function(
  d      # a single data frame from the raw element
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
    toosmall <- ifelse(x > 0 & x < 5, TRUE, FALSE)
  }
  d_small <- dplyr::bind_cols(
    d %>% dplyr::select_(.dots = stem_variables),        # 1st argument
    d %>% dplyr::select_(.dots = count_variables ) %>%   # 2nd arguemnt
      dplyr::mutate_all(dplyr::funs(cellistoosmall))     
  )
  return(d_small)
}
# usage
# d_small_cell <- df %>% detect_small_cell()
# creates a replica of the data, with count values are replaced by TRUE/FALSE according to test

# TEST 2: What cells can help calculate the suppressed cells from the same triplet?
# Censor 2: What triplets should be suppressed? (eg. F-M-T)
# reverse calculate from:
detect_recalc_triplet <- function(
  d # smallest decision unit: disease-year
){
  # d <- ds
  # split varnames into two groups
  (varnames        <- names(d))
  (count_variables <- grep("_[MFT]$",varnames, value = T)) # which ends with `_F` or `_M` or `_T`
  (stem_variables  <- setdiff(varnames, count_variables)) 
  
  d1 <- d  %>% detect_small_cell()
  
  # alt
  d2 <- d1 %>% 
    dplyr::mutate(
      HSDA_F = ifelse(HSDA_M,TRUE, HSDA_F),
      HSDA_M = ifelse(HSDA_F,TRUE, HSDA_M),
      HA_M   = ifelse(HA_F,TRUE, HA_M),
      HA_F   = ifelse(HA_M,TRUE, HA_F),
      BC_M   = ifelse(BC_F,TRUE, BC_M),
      BC_F   = ifelse(BC_M,TRUE, BC_F)
    )
  return(d2)   
} 
# usage
# d2_recalc_from_triplet <- df %>% detect_recalc_triplet()

# TEST 3: Is this is the only triple that is being suppressed in a higher order block?
# Censor 3: What cells should be suppressed as those that could be calculated from higher order count?

detect_single_suppression <- function(
  d
){
  # d <- ds0 %>% filter(case ==2) %>% select(-case) # to pick needed case
  # d <- df
  (varnames <- names(d))
  (count_variables <- grep("_[MFT]$",varnames, value = T)) # which ends with `_F` or `_M` or `_T`
  (stem_variables <- setdiff(varnames, count_variables)) 
  
  # load the current suppression decisions 
  d1 <- d %>% detect_recalc_triplet()
  
  ########### Single suppression at HSDA level
  d2 <- d %>% 
    dplyr::select_(.dots = c(stem_variables,"HSDA_F","HSDA_M", "HSDA_T")) %>% 
    dplyr::rename(f = HSDA_F, m = HSDA_M, total_count = HSDA_T) %>%  # will help us to choose what to suppress
    # dplyr::arrange(label_ha, HSDA_T) %>% #
    dplyr::left_join(
      d1 %>% # original, `suppression-ready` dataset with unsuppressed counts
        dplyr::select(label_hsda, HSDA_F, HSDA_M, HSDA_T) 
    ) %>%  
    dplyr::group_by(label_ha) %>%
    # The above place numerical and logical values into the same view
    # for INDIVIDUAL HA, let us identify the situations where
    # 1) a single row of both M and F are suppressed # M-F   pair
    # 2) all three values are suppressed             # M-F-T triplet
    # we will accomplish this by
    dplyr::mutate(
      # counting the number of rows in the group that has M-F pair suppessed
      n_pair_suppressed = sum(HSDA_F & HSDA_M) # both must be TRUE to count as 1
      # counting the number of rows in the group that has M-F-T triplet suppressed
      ,n_triplet_suppressed = sum(HSDA_T) # must be TRUE to count (M-F will be TRUE if T is)
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # identify HSDAs with a single pair suppression (in a Health Authority)
      single_pair = ifelse(n_pair_suppressed == 1 & (HSDA_F & HSDA_M), TRUE, FALSE)
      # identify HSDAs with a single triples suppression (in a Health Authority)
      ,single_triplet = ifelse(n_triplet_suppressed == 1 & HSDA_T, TRUE, FALSE)
    ) %>% 
    dplyr::select(-n_pair_suppressed, -n_triplet_suppressed) %>% 
    # in what HAs should we further suppress?
    dplyr::group_by(label_ha) %>% 
    dplyr::mutate(
      ha_with_single_pair     = any(single_pair)
      ,ha_with_single_triplet = any(single_triplet)
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::arrange(label_ha, total_count) 
    
    # pairs in what HDSA(s) must be suppressed?
    d_suppress_these_hsda_pairs <- d2 %>% 
    # keep only the HAs that require further suppression
      dplyr::filter(ha_with_single_pair) %>% 
      # remove the pair that has already been suppressed
      dplyr::filter(!single_pair) %>% 
      dplyr::group_by(label_ha) %>% 
      dplyr::slice(which.min(total_count)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(suppress_this_hsda_pair = TRUE) %>% 
      dplyr::select(label_ha, label_hsda, suppress_this_hsda_pair)
   
    d_suppress_these_hsda_triplets <- d2 %>% 
      # keep only the HAs that require further suppression
      dplyr::filter(ha_with_single_triplet) %>% 
      # remove the triplet that has already been suppressed
      dplyr::filter(!single_triplet) %>% 
      dplyr::group_by(label_ha) %>% 
      dplyr::slice(which.min(total_count)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(suppress_this_hsda_triplet = TRUE) %>% 
      dplyr::select(label_ha, label_hsda, suppress_this_hsda_triplet)
    
    ########### Single suppression at HA level
    # No need to get M/F columns because it is caught by the previous logical test (recal_triplet)
    d3 <- d %>% 
      dplyr::select_(.dots = c(stem_variables,"HA_T")) %>%  
      dplyr::rename(total_count = HA_T) %>%  # will help us to choose what to suppress
      # dplyr::arrange(label_ha, HSDA_T) %>% #
      dplyr::left_join(
        d1 %>% # original, `suppression-ready` dataset with unsuppressed counts
          dplyr::select(label_ha, label_hsda, HA_F, HA_M, HA_T) 
      ) %>% 
      dplyr::select(-label_hsda) %>% 
      dplyr::distinct() %>% 
      dplyr::group_by(label_prov) %>% 
      # for INDIVIDUAL  PROV, let us identify the situations where
      # 1) a single row of both M and F are suppressed # M-F   pair
      # 2) all three values are suppressed             # M-F-T triplet
      # we will accomplish this by
      dplyr::mutate(
        # counting the number of rows in the group that has M-F pair suppessed
        n_pair_suppressed = sum(HA_F & HA_M ) # both must be TRUE to count as 1
        # counting the number of rows in the group that has M-F-T triplet suppressed
        ,n_triplet_suppressed = sum(HA_T) # must be TRUE to count (M-F will be TRUE if T is)
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(
        # identify HSDAs with a single pair suppression (in a Health Authority)
        single_pair = ifelse(n_pair_suppressed == 1 & (HA_F & HA_M), TRUE, FALSE)
        # identify HSDAs with a single triples suppression (in a Health Authority)
        ,single_triplet = ifelse(n_triplet_suppressed == 1 & HA_T, TRUE, FALSE)
      ) %>% 
      dplyr::select(-n_pair_suppressed, -n_triplet_suppressed) %>% 
      dplyr::arrange(label_ha, total_count) %>% 
      # in what PROVs should we further suppress?
      dplyr::group_by(label_prov) %>% 
      dplyr::mutate(
        prov_with_single_pair     = any(single_pair)
        ,prov_with_single_triplet = any(single_triplet)
      ) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(label_ha, total_count) 
  
    d_suppress_these_ha_pairs <- d3 %>% 
      # keep only the HAs that require further suppression
      dplyr::filter(prov_with_single_pair) %>% 
      # remove the pair that has already been suppressed
      dplyr::filter(!single_pair) %>% 
      dplyr::group_by(label_prov) %>% 
      dplyr::slice(which.min(total_count)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(suppress_this_ha_pair = TRUE)%>% 
      dplyr::select(label_ha, suppress_this_ha_pair)
    
    
    d_suppress_these_ha_triplets <- d3 %>% 
      # keep only the HAs that require further suppression
      dplyr::filter(prov_with_single_triplet) %>% 
      # remove the triplet that has already been suppressed
      dplyr::filter(!single_triplet) %>% 
      dplyr::group_by(label_prov) %>% 
      dplyr::slice(which.min(total_count)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(suppress_this_ha_triplet = TRUE)%>% 
      dplyr::select(label_ha, suppress_this_ha_triplet)
    
    
  ###### attach suppression decision to the input data
    d4 <- d1 %>% 
      dplyr::left_join(d_suppress_these_hsda_pairs) %>%  
      dplyr::left_join(d_suppress_these_hsda_triplets) %>% 
      dplyr::left_join(d_suppress_these_ha_pairs) %>%  
      dplyr::left_join(d_suppress_these_ha_triplets) %>% 
      dplyr::mutate(
        # because doesn't catch later during evaluation
        suppress_this_hsda_pair     = ifelse(is.na(suppress_this_hsda_pair)   , FALSE, suppress_this_hsda_pair)
        ,suppress_this_hsda_triplet = ifelse(is.na(suppress_this_hsda_triplet), FALSE, suppress_this_hsda_triplet)
        ,suppress_this_ha_pair      = ifelse(is.na(suppress_this_ha_pair)     , FALSE, suppress_this_ha_pair)
        ,suppress_this_ha_triplet   = ifelse(is.na(suppress_this_ha_triplet)  , FALSE, suppress_this_ha_triplet)
        
      )
  
  ###### implement redaction and remove auxillary variables
    d5 <- d4 %>% 
      dplyr::mutate(
        # replace with TRUE or leave as is. adds further suppression
        HSDA_F  = ifelse(suppress_this_hsda_pair | suppress_this_hsda_triplet, TRUE, HSDA_F)
        ,HSDA_M = ifelse(suppress_this_hsda_pair | suppress_this_hsda_triplet, TRUE, HSDA_M)
        ,HSDA_T = ifelse(suppress_this_hsda_triplet, TRUE, HSDA_T)
        #
        ,HA_F = ifelse(suppress_this_ha_pair | suppress_this_ha_triplet, TRUE, HA_F)
        ,HA_M = ifelse(suppress_this_ha_pair | suppress_this_ha_triplet, TRUE, HA_M)
        ,HA_T = ifelse(suppress_this_ha_triplet, TRUE, HA_T)
        
      ) %>% 
      dplyr::select_(.dots = names(d))

  # Enforce original sorting order (optional)
  # d6 <- d5 %>%
  #   elongate_values() %>%
  #   dplyr::mutate(
  #     column_name = factor(column_name, levels = count_variables)
  #   ) %>%
  #   dplyr::select(-agg_level, -sex) %>%
  #   tidyr::spread(column_name, value)
  
  return(d5)  
}
# usage
# d3_single_suppression <- df %>% detect_single_suppression()


# THis test is from version 1 "Draconian", a very conservative approach
# which removes all other units if a single unit is suppressed in a higher-order unit
# TEST 3: Is this is the only triple that is being suppressed in a higher order block?
# Censor 3: What cells should be suppressed as those that could be calculated from higher order count?
detect_single_suppression_draconian <- function(
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
    dplyr::group_by(label_prov) %>%
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
    dplyr::select(-n_sup, -n_tot)
  
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
# ---- tidy-functions ------------------------

# function to elongate (value column in the)  smallest decision frame
elongate_values <- function( 
  d                 # data frame with a single Decision Unit
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
      ,agg_level = ifelse(agg_level == "BC", "PROV", agg_level) # idiosyncratic name, exercise caution, consider changing
    ) %>%
    dplyr::select_(.dots =
                     c(label_variables, "column_name","agg_level","sex", "value")
    )
  return(d_long)
}
# usage
# d_long_values <-  df %>% elongate_values()


# function to elongate (label column in the) smallest decision frame
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
      ,agg_level  = factor(agg_level,  levels = c("PROV","HA","HSDA")) 
    )
}
# usage
# d_long_labels <- df %>% elongate_labels(c("label_prov", "label_ha","label_hsda"))

# ----- graphing-functions --------------------
# apply sequential logical tests to suppress desired cells
combine_logical_tests <- function(
  d
){
  # d <- df
  # create a list object with progressive stages through chain of decisions/censors
  l <- list(
    "observed"                   = d                                 
    ,"censor1_small_cell"         = d %>% detect_small_cell()         
    ,"censor2_recalc_triplet"     = d %>% detect_recalc_triplet()    
    ,"censor3_single_suppression" = d %>% detect_single_suppression() 
    ,"censor3_single_suppression_draconian" = d %>% detect_single_suppression_draconian() 
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
    dplyr::select(-observed) %>% 
    dplyr::mutate(
       censor_activated = ifelse(censor1_small_cell, "1-small-cell",
                                 ifelse(censor2_recalc_triplet & !censor1_small_cell, "2-recalc-triplet",
                                        ifelse(censor3_single_suppression & !censor2_recalc_triplet, "3-single-sup", 
                                               ifelse(censor3_single_suppression_draconian & !censor2_recalc_triplet, "3-single-sup-draco"
                                                      ,"0-none")))) 
    )

  return(dd)
}
# usage
# d_combined_censors <- df %>% combine_logical_tests()


# create color scale to highlight suppression decisions
make_color_scale <- function(
  meta = bc_health_map
){
  
  lkp_hsda <- meta %>% lookup_meta("hsda")
  lkp_ha   <- meta %>% lookup_meta("ha")
  lkp_prov <- meta %>% lookup_meta("prov")
  
  # create a color scale for fill and text
  # extrac color definitions from the meta data
  d_color_fill <- 
    dplyr::bind_rows(
      list(
        "hsda" =  lkp_hsda %>% 
          dplyr::select(label_hsda, color_hsda) %>% 
          dplyr::rename(label_text = label_hsda, color_value =color_hsda )
        ,"ha"  = lkp_ha %>% 
          dplyr::select(label_ha, color_ha) %>% 
          dplyr::rename(label_text = label_ha, color_value =color_ha )
        ,"prov"  = lkp_prov %>% 
          dplyr::select(label_prov, color_prov) %>% 
          dplyr::rename(label_text = label_prov, color_value =color_prov )
      )
    ) %>% 
    dplyr::distinct()
  # convert to a named vector
  v_color_fill <- as.data.frame(d_color_fill)[,"color_value"]
  names(v_color_fill) <- as.data.frame(d_color_fill)[,"label_text"]
  v_color_fill # inspect
  # derived the color of the text by reversing the brightness and hue
  # see http://www.nbdtech.com/Blog/archive/2008/04/27/Calculating-the-Perceived-Brightness-of-a-Color.aspx
  v_color_text <- 
    as.data.frame(t(col2rgb(v_color_fill))) %>% 
    dplyr::mutate(
      brightness = sqrt(red*red*.241 + green*green*.691 + blue*blue*.068),
      color      = ifelse(brightness>130, "gray2", "gray98")
    ) %>%
    dplyr::select(color) %>% 
    unlist()
  names(v_color_text) <- names(v_color_fill)
  
  # combine into the final data set
  d_colors <- dplyr::full_join(
    as.data.frame(v_color_fill) %>% tibble::rownames_to_column("value")
    ,as.data.frame(v_color_text) %>% tibble::rownames_to_column("value")
    , by = "value"
  )
  return(d_colors)
  
}
# usage
# d_colors <- bc_health_map %>% make_color_scale()

# function to prepare the smallest context for graphing by geom_tile
# input = disease-by-year ds, output = list object with meaningful components
prepare_for_tiling <- function(
  d     # a standard SAU: disease-year-labels-values
  ,meta = bc_health_map # meta-data file contains BC health boundries heirarchy and other definitions
){
  # d <- df
  # meta <- bc_health_map
  #(1)######## Establish the reference definitions from the common meta data object
  # obtain lookup tables from the meta-data object  
  lkp_hsda <- meta %>% lookup_meta("hsda")
  lkp_ha   <- meta %>% lookup_meta("ha")
  lkp_prov <- meta %>% lookup_meta("prov")
  
  d_colors <- meta %>% make_color_scale()
  
  # extract stable info
  disease = as.data.frame(d %>% dplyr::distinct(disease))[1,1]
  year    = as.data.frame(d %>% dplyr::distinct(year))[1,1]
  # remove stable info from the standard input format
  d_wide <- d %>% dplyr::select(-disease, -year) #%>% 
  # dplyr::mutate(label_prov = "BC") %>% # add manually, for balance
  # dplyr::select(label_prov, dplyr::everything()) # sort
  # the object `d_wide` is now the stem for sebsequent operations
  # split variables into counts and labels
  (count_variables <- grep("_[MFT]$",names(d_wide), value = T))
  (label_variables <- setdiff(names(d_wide), count_variables))
  # d_wide %>% print(n = nrow(.))
  
  #(2)######## Create data for the LEFT PANEL
  # create data set for the left panel of the graph (labels of health boundries)
  # d_long_labels <- d %>% elongate_labels(c("label_ha","label_hsda")) %>% 
  d_long_labels <- d %>% 
    elongate_labels(label_variables) %>% 
    # # add color info from the meta data
    dplyr::left_join(
      d_colors, by = c("value") # experimental feature, optional at this point
    ) %>%
    # make columns into factors to enforce order and aesthetic mapping
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
  # browser()
  #(3)######## Create data for the RIGHT PANEL
  # the right panel will contain only numbers (FMT counts of variable selected for suppression)
  # d_long_values <- d %>% elongate_values(regex = "_[MTF]$") %>% 
  d_long_values <- d %>% combine_logical_tests() %>% 
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
# l <- df %>% prepare_for_tiling(bc_health_map)

# function that graphs the toosmall decision
make_tile_graph <- function(
  d   # a dataset containing observed counts for the decision context
  ,meta   = bc_health_map# a meta data object containing grouping and coloring settings
  # ,censor ="censor_activated"
  ,...
){
  # d <- df # turn on for testing, if needed
  # meta <- bc_health_map
  # censor = "censor0"
  # censor_labels <- c(
  #   "censor0"                     = "- Observed counts"
  #   , "censor1_small_cell"          = "- Censor (1) Small cell?"
  #   , "censor2_recalc_triplet"      = "- Censor (2) Recalculate from triplet?"
  #   , "censor3_single_suppression"  = "- Censor (3) Single Suppression?"
  # )
  
  # maybe later
  # font_size_left  <- baseSize + (font_size - baseSize) 
  # font_size_right <- font_size_left + right_size_adjustment
  
  l <- d %>% prepare_for_tiling(meta)
  
  ##--##--##--##--##--##--##--##--##--##--##
  # graph the labels - LEFT SIDE OF THE TABLET
  g <- l$labels_long %>%  
    dplyr::filter(!agg_level=="PROV") %>% 
    dplyr::mutate(dummy = "") %>% 
    ggplot2::ggplot(
      aes_string(
        x     = "dummy"
        ,y     = "label_hsda"
        ,label = "value" 
      )
    )
  g <- g + geom_tile(fill = "white")
  # g <- g + geom_tile(aes_string(fill = "v_color_fill"))
  g <- g + facet_grid(.~agg_level)
  g <- g + geom_text(size = baseSize-7, hjust =.5)
  # g <- g + geom_text(aes_string(color = "v_color_fill"),hjust =.5, ...)
  # g <- g + geom_text(aes_string(color = "v_color_text"),hjust =.5)
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
    legend.position="none"
  )
  g <- g + guides(color=FALSE)
  g <- g + labs(x=NULL, y=NULL)
  g_labels <- g
  g_labels 
  
  # browser()
  ##--##--##--##--##--##--##--##--##--##--##
  # graph the values - RIGHT SIDE OF THE TABLET  
 # "#fc8d62" # red
 # "#66c2a5" # green
 # "#8da0cb" # blue
  # source: http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=4
  censor_colors = c(
     "0-none"              = NA
    ,"1-small-cell"        = "#fc8d62" # red
    ,"2-recalc-triplet"   = "#66c2a5" # green
    ,"3-single-sup"       = "#8da0cb" # blue
    ,"3-single-sup-draco" = "#cbd5e8" # pale blue
  )
  
  g <- l$values_long %>%  
    dplyr::mutate(
      agg_level = factor(agg_level, levels = c("HSDA","HA","PROV"))
    ) %>% 
    dplyr::mutate(
      censor_activated = factor(censor_activated, levels = names(censor_colors))
    ) %>% 
    ggplot2::ggplot(
      aes_string(
        x     = "sex"
        ,y     = "label_hsda"
        ,label = "value" 
      )
    )
  # g <- g + geom_tile(aes_string(fill = "agg_level"))
  # g <- g + geom_tile(aes_string(fill = censor))
  g <- g + geom_tile(aes_string(fill = "censor_activated"))
  # g <- g + geom_text(size = baseSize-7, hjust=.4)
  # g <- g + geom_text(size = fontsize_right, hjust=.5)
  # g <- g + geom_text(aes(color = censor1_small_cell),hjust=.5, ...)
  # g <- g + geom_text(aes_string(color = censor),hjust=.5)
  g <- g + geom_text(color = "black" ,hjust=.5)
  g <- g + facet_grid(. ~ agg_level )
  # g <- g + scale_fill_manual(values = c("0"=NA, "1"="#66c2a5", "2"="#fc8d62", "3" = "#8da0cb"))
  g <- g + scale_fill_manual(values = censor_colors)
  # g <- g + scale_fill_manual(values = c("TRUE"="black", "FALSE"="white"))
  # g <- g + scale_color_manual(values = c("TRUE"="white", "FALSE"="black"))
  
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
  main_title <-paste0(toupper(l$disease)," - ", l$year )
  
  grid::grid.text(main_title, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1), just = "left")
  print(g_labels,  vp=grid::viewport(layout.pos.row=2, layout.pos.col=1 ))
  print(g_values, vp=grid::viewport(layout.pos.row=2, layout.pos.col=2 ))
  grid::popViewport(0)
  return(grid::popViewport(0))
  
} # usage
# df %>% make_tile_graph(bc_health_map)
# df %>% make_tile_graph(bc_health_map, "censor0")


print_tile_graph <- function(
  d
  ,meta        = bc_health_map
  ,path_folder # = "./sandbox/dev-1/prints/"
  ,...
){
  
  disease <- as.data.frame(d)[1,1]
  year    <- as.data.frame(d)[1,2]
  
  path_save = paste0(path_folder,disease,"-",year,".png")
  png(filename = path_save, width = 900, height = 500,res = 100)
  # d %>% make_tile_graph(meta, censor = "censor_activated")
  d %>% make_tile_graph(meta,...)
  dev.off()
  
}

# print_tile_graph <- function(
#   d
#   ,meta        = bc_health_map
#   ,path_folder # = "./sandbox/dev-1/prints/"
#   ,...
# ){
#   # vector containing the list of graphs to create (e.i. censors to apply)
#   censor_vector <- c(
#     "censor0" 
#     , "censor1_small_cell"
#     , "censor2_recalc_triplet"
#     , "censor3_single_suppression"
#   )
#   
#   disease <- as.data.frame(d)[1,1]
#   year    <- as.data.frame(d)[1,2]
#   
#   for(i in seq_along(censor_vector)){
#     path_save = paste0(path_folder,disease,"-",year,"-censor-",i-1,".png")
#     png(filename = path_save, width = 900, height = 500,res = 100)
#     d %>% make_tile_graph(meta,censor = censor_vector[i],...)
#     dev.off()
#   }
#   
# }
#usage
# df %>% print_tile_graph(bc_health_map, size = 3)

# wrapper function to print one decision frame  
print_one_frame <- function(
  d
  ,disease_ #= "Flower Deafness"
  ,year_    #= 1995
  ,folder   #= "./sandbox/examiner-1/prints/"
  , ...
){
  d %>% 
    dplyr::filter(disease == disease_) %>% 
    dplyr::filter(year    == year_) %>% 
    print_tile_graph(path_folder=folder,...)
}
#usage
# ds0 %>% # notice that it takes the file with ALL suppression framed
#   print_one_frame(
#     disease_ = "Flower Deafness"
#     ,year_   =  1995
#     ,folder  = "./sandbox/examiner-1/prints/"
#   )

# ----- redaction-functions ---------------------------
