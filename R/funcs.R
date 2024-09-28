# add year box to ggplot, make plotly
mataddyr_fun <- function(p, yrsel, lwd = 1.5){

  maxx <- length(unique(p$data$grp))
  
  p <- p +
    ggplot2::geom_hline(yintercept = yrsel - 0.5, lwd = lwd) + 
    ggplot2::geom_hline(yintercept = yrsel + 0.5, lwd = lwd) + 
    ggplot2::geom_segment(ggplot2::aes(x = 0.5, xend = 0.5, y = yrsel - 0.5, yend = yrsel + 0.5, lwd = lwd)) +
    ggplot2::geom_segment(ggplot2::aes(x = maxx + 0.5, xend = maxx + 0.5, y = yrsel - 0.5, yend = yrsel + 0.5, lwd = lwd))
  
  out <- tbeptools::show_matrixplotly(p)
  
  return(out)
  
}
