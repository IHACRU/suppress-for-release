
make_tile_practice <- function(
  d     # standard disease-year frame
  ,meta # bc_health_map
){
  # values for testing and development
  # d    <- df
  # meta <- bc_health_map
  # 
  # create a list object prepared for graphing
  l <- d %>% prepare_for_tiling(meta)
  
  ##-##-##-##-##-##
  # graph the labels - LEFT panel of the display
  g_labels_left <- l$labels_long %>% 
    dplyr::filter( ! agg_level == "PROV" ) %>% 
    ggplot2::ggplot(
      aes(
        x = ""
        ,y = label_hsda
      )
    )+
    geom_tile(fill = NA)+
    geom_text(aes(label = value))+
    facet_grid(.~agg_level)+
    theme_minimal()+
    labs(x = NULL, y = NULL)+
    theme(
      axis.text.y = element_blank()
      ,panel.grid = element_blank()
    )
  # g_labels_left
  
  ##-##-##-##-##-##
  # graph the values - RIGHT panel of the display
  # colors from: http://colorbrewer2.org/#type=qualitative&scheme=Set2&n=3
  censor_colors <- c(
    "0-none"            = NA                                 
    ,"1-small-cell"      = "#fc8d62" # red                     
    ,"2-recalc-triplet"  = "#66c2a5" # green                      
    ,"3-single-sup"      = "#8da0cb" # blue 
    ,"3-single-sup-draco"= "grey90" # grey                     
  )
  
  g_values_right <- l$values_long %>% 
    dplyr::mutate(
      agg_level         = factor(agg_level,        levels = c("HSDA","HA", "PROV")) 
      ,censor_activated = factor(censor_activated, levels = names(censor_colors) )
    ) %>% 
    ggplot2::ggplot(
      aes( x = sex, y = label_hsda )
    ) + 
    geom_tile(aes(fill = censor_activated))+
    geom_text(aes(label = value))+
    facet_grid(. ~ agg_level)+
    scale_fill_manual(values = censor_colors)+
    theme_minimal()+
    theme(
      axis.title       = element_blank()
      ,axis.text.y     = element_blank()
      ,axis.text.x     = element_text(color = "grey50")
      ,legend.position = "none"
      ,panel.grid      = element_blank()
    )
  # g_values_right
  
  ##-##-##-##-##-##
  # Combine panels into a single display
  main_title <- paste0( toupper(l$disease), " - ", l$year)
  
  # COMBINE panels
  grid::grid.newpage()
  layout <- grid::grid.layout(
    nrow = 2
    ,ncol = 2
    ,widths  = grid::unit(x = c(.5,   .5),units =  c("null","null") ) 
    ,heights = grid::unit(x = c(.05, .95),units =  c("null","null") ) 
  )
  grid::pushViewport(grid::viewport(layout = layout))
  grid::grid.text(main_title, vp = grid::viewport(layout.pos.row = 1, layout.pos.col=1), just = "left")
  print(g_labels_left, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(g_values_right, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
  # grid::popViewport(0)

  return(grid::popViewport(0))
  
}
# how to use:
g <- df %>% make_tile_practice(meta = bc_health_map)

print_tile_practice <- function(
  d #
  ,meta
  ,save_folder = "./sandbox/examiner-2/prints/"
){
  
  (disease <- as.data.frame(d)[1,1])
  (year <- as.data.frame(d)[1,2])
  (path_save <- paste0(save_folder, disease,"-",year,".png"))
  
  png(filename = path_save, width = 900 , height = 500, res = 100)
  
  d %>% make_tile_practice(meta = meta)
  
  dev.off()
  
}
# how to use
df %>% print_tile_practice(meta = bc_health_map)


ds0 %>% dplyr::filter(year == 2000) %>% print_tile_practice(bc_health_map)

all_available_years <- unique(ds0$year)

for(year_i in all_available_years ){
  
  ds0 %>% dplyr::filter(year == year_i) %>% 
    print_tile_practice(bc_health_map)
  
  
}
















