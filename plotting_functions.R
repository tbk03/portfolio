geom_minimal_hist <- function(ylim, ...){
  list(
    
    # adjust default colors of histogram to use less "ink"
    geom_histogram(colour = "grey50",
                   fill = "grey70", ...),
    
    # remove gap between x axis and base of bars
    coord_cartesian(ylim = ylim, expand = FALSE),
    
    # remove cluttering grid lines
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
  )
}

remove_all_grid_lines <- function(){
  # remove cluttering grid lines
  theme(panel.grid = element_blank())
}