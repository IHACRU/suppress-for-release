# load common object definitions
source("./manipulation/object-glossary.R")

# ----- utility-functions --------------------------

# function returning a lookup table for a given level of aggregation
lookup_meta <- function(
  meta=bc_health_map       # meta-data file contains BC health boundries heirarchy and other definitions
  ,agg_level = "hsda"  #
){
  if(agg_level == "hsda"){
    lookup_table <- meta %>% 
      dplyr::distinct(id_prov, id_ha, id_hsda, label_prov, label_ha, label_hsda, color_hsda, color_hsda_label) %>% 
      dplyr::select( id_prov, id_ha, id_hsda, label_prov, label_ha, label_hsda, dplyr::everything()) %>% 
      dplyr::arrange(id_ha, id_hsda)
  }
  if(agg_level == "ha"){
    lookup_table <- meta %>% 
      dplyr::distinct(id_ha, label_ha, color_ha, color_ha_label) %>% 
      dplyr::arrange(id_ha)
  }
  if(agg_level == "prov"){
    lookup_table <- meta %>% 
      dplyr::distinct(id_prov, label_prov, color_prov, color_prov_label) %>% 
      dplyr::arrange(id_prov)
  }
  return(lookup_table)
}
# usage
# lkp_hsda <- bc_health_map %>% lookup_meta("hsda")
# lkp_ha <- bc_health_map %>% lookup_meta("ha")


# function to carry out a full join among all components of the list object
full_join_multi <- function(list_object){
  # list_object <- datas[["physical"]][["161"]]
  d <- list_object %>%
    Reduce(function(dtf1,dtf2) dplyr::full_join(dtf1,dtf2), .)
}

