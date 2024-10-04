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

# download data proc
dldatproc_fun <- function(typseldl, yrseldl){
  
  out <- NULL
  
  if(typseldl == 'Baywide segment score categories')
    out <- tbeptools::anlz_fibmatrix(enterodata, bay_segment = unlist(areas1), yrrng = yrseldl, 
                                     indic = 'entero', warn = F) |> 
    dplyr::rename(bay_segment = grp) |> 
    dplyr::select(-Latitude, -Longitude)
  
  if(typseldl ==  'Baywide station score categories')
    out <- tbeptools::anlz_fibmatrix(enterodata, stas = unique(enterodata$station), yrrng = yrseldl, 
                                     indic = 'entero', warn = F) |> 
    dplyr::rename(Station = grp)
  
  if(typseldl == 'Baywide raw data')
    out <- enterodata |> 
    dplyr::rename(Station = station) |> 
    dplyr::select(-time, -time_zone, -long_name, -qualifier, -LabComments)
  
  if(typseldl == 'EPCHC station score categories')
    out <- tbeptools::anlz_fibmatrix(fibdata, stas = unique(fibdata$epchc_station), yrrng = yrseldl, 
                                     indic = 'fcolif', warn = F) |> 
    dplyr::rename(`EPCHC station` = grp)
  
  if(typseldl == 'EPCHC raw data')
    out <- fibdata |> 
    dplyr::rename(`EPCHC Station` = epchc_station) |> 
    dplyr::select(-Total_Depth_m, -Sample_Depth_m, -totcol, -totcol_q)
  
  out <- out |> 
    dplyr::filter(yr >= yrseldl[1] & yr <= yrseldl[2]) |> 
    dplyr::rename(
      Year = yr
    ) |> 
    dplyr::mutate(dplyr::across(dplyr::contains('Station'), ~ as.character(.)))
  
  if(!grepl('raw', typseldl))
    out <- out |> 
    dplyr::filter(!is.na(gmean)) |> 
    dplyr::rename(
      `Geometric mean` = gmean,
      `Score category` = cat
    )
  else
    out <- out |> 
    dplyr::rename(Month = mo)
  
  req(!is.null(out))
  
  return(out)
  
}

# download data table function
dldattab_fun <- function(dldat){
  
  out <- reactable::reactable(dldat,
                              columns = list(
                                Year = reactable::colDef(format = reactable::colFormat(digits = 0)),
                                Month = reactable::colDef(format = reactable::colFormat(digits = 0))
                              ),
                              defaultColDef = reactable::colDef(
                                footerStyle = list(fontWeight = "bold"),
                                format = reactable::colFormat(digits = 3, separators = F),
                                resizable = TRUE, 
                                align = 'left'
                              ),
                              filterable = T,
                              defaultPageSize = 15
  )
  
  return(out)
  
}

# enteromap popup plot
entmappopup_plo <- function(dat, station, yr, mo){

  levs <- tbeptools::util_fiblevs()
  
  cols <- c("#2DC938", "#E9C318", "#EE7600", "#CC3231")
  names(cols) <- levs$enterolbs
  
  toplo <- dat |> 
    dplyr::filter(station == !!station) |> 
    dplyr::mutate(
      Conditions = ifelse(wet_sample, 'Wet', 'Dry'),
      Conditions = factor(Conditions),
      Threshold = cut(entero, breaks = levs$enterolev, right = F, levs$enterolbs)
    ) |> 
    dplyr::filter(!is.na(Threshold))
  
  # ln <- lubridate::make_date(yr, mo, 1)

  out <- ggplot2::ggplot(toplo, ggplot2::aes(x = date, y = entero)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(shape = Conditions, color = Threshold), size = 3) +
    # ggplot2::geom_vline(xintercept = ln, linetype = 'dashed') +
    ggplot2::scale_shape_manual(values = c('Wet' = 16, 'Dry' = 17)) +
    ggplot2::scale_color_manual(values = cols, drop = F) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 15))) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(), 
      legend.position = 'bottom', 
      plot.title = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_text(size = 12)
    ) +
    ggplot2::labs(
      title = paste('Station:', station),
      shape = 'Sampling conditions',
      color = 'Threshold',
      x = NULL,
      y = 'Enterococcus (#/100mL)'
    )
  
  out <- plotly::ggplotly(out)
  
  return(out)
  
}

# fibmap popup plot
fibmappopup_plo <- function(dat, station, yr, mo){
  
  levs <- tbeptools::util_fiblevs()

  toplo <- dat |> 
    dplyr::filter(epchc_station == !!station) 
  
  clss <- unique(toplo$class)
  
  # sort conditional if freshwater or marine
  if(clss %in% c('1', '3F')){
    indic <- 'ecoli'
    ylb <- 'E. coli (#/100mL)'
    ttl <- paste('Station:', station, '(freshwater)')
    shp <- 17
    brks <- levs$ecolilev
    labs <- levs$ecolilbs
  }
  if(clss %in% c('2', '3M')){
    indic <- 'entero'
    ylb <- 'Enterococcus (#/100mL)'
    ttl <- paste('Station:', station, '(marine)')
    shp <- 16
    brks <- levs$enterolev
    labs <- levs$enterolbs
  }
  
  toplo <- toplo |> 
    dplyr::rename(Value = !!indic) |>
    dplyr::mutate(
      Threshold = cut(Value, breaks = brks, right = F, labs), 
      date = as.Date(SampleTime)
    ) |> 
    dplyr::filter(!is.na(Threshold))
  
  cols <- c("#2DC938", "#E9C318", "#EE7600", "#CC3231")
  names(cols) <- labs
  
  # ln <- lubridate::make_date(yr, mo, 1)

  out <- ggplot2::ggplot(toplo, ggplot2::aes(x = date, y = Value)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(color = Threshold), size = 3, shape = shp) +
    # ggplot2::geom_vline(xintercept = ln, linetype = 'dashed') +
    ggplot2::scale_color_manual(values = cols, drop = F) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(), 
      legend.position = 'bottom', 
      plot.title = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_text(size = 12)
    ) +
    ggplot2::labs(
      title = ttl,
      color = 'Threshold',
      x = NULL,
      y = ylb
    )
  
  out <- plotly::ggplotly(out)
  
  return(out)
  
}