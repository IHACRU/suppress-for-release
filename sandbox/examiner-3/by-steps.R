make_tile_graph <- function(
  d   # a dataset containing observed counts for the decision context
  ,meta   = bc_health_map# a meta data object containing grouping and coloring settings
  # ,censor ="censor_activated"
  ,steps_displayed = 4L
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
  # censor_colors = c(
  #   "0-none"              = NA
  #   ,"1-small-cell"        = "#fc8d62" # red
  #   ,"2-recalc-triplet"   = "#66c2a5" # green
  #   ,"3-single-sup"       = "#8da0cb" # blue
  #   ,"3-single-sup-draco" = "#cbd5e8" # pale blue
  # )
  
  if(steps_displayed==4L){
    censor_colors = c(
      "0-none"              = NA
      ,"1-small-cell"        = "#fc8d62" # red
      ,"2-recalc-triplet"   = "#66c2a5" # green
      ,"3-single-sup"       = "#8da0cb" # blue
      ,"3-single-sup-draco" = "#cbd5e8" # pale blue
    )
  }
  if(steps_displayed==3L){
    censor_colors = c(
      "0-none"              = NA
      ,"1-small-cell"       = "#fc8d62" # red
      ,"2-recalc-triplet"   = "#66c2a5" # green
      ,"3-single-sup"       = "#8da0cb" # blue
      ,"3-single-sup-draco" = "#8da0cb" # pale blue
    )
  }
  if(steps_displayed==2L){
    censor_colors = c(
      "0-none"              = NA
      ,"1-small-cell"       = "#fc8d62" # red
      ,"2-recalc-triplet"   = "#66c2a5" # green
      ,"3-single-sup"       = NA # blue
      ,"3-single-sup-draco" = NA # pale blue
    )
 }
  if(steps_displayed==1L){
    censor_colors = c(
      "0-none"              = NA
      ,"1-small-cell"       = "#fc8d62" # red
      ,"2-recalc-triplet"   = NA # green
      ,"3-single-sup"       = NA # blue
      ,"3-single-sup-draco" = NA # pale blue
    )
  }
  if(steps_displayed==0L){
    censor_colors = c(
      "0-none"              = NA
      ,"1-small-cell"       = NA # red
      ,"2-recalc-triplet"   = NA # green
      ,"3-single-sup"       = NA # blue
      ,"3-single-sup-draco" = NA # pale blue
    )
  }
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


# df %>% make_tile_graph(bc_health_map, cc = censor_colors_step_0)
# df %>% make_tile_graph(bc_health_map, cc = censor_colors_step_1)
# df %>% make_tile_graph(bc_health_map, cc = censor_colors_step_2)
# df %>% make_tile_graph(bc_health_map, cc = censor_colors_step_3)
# df %>% make_tile_graph(bc_health_map, cc = censor_colors_step_4)
