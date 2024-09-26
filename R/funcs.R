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

# hacky extract from show_fibmatmap
show_fibmatmapex <- function(fibdata, yrsel, areasel, indic, threshold = NULL,
                           lagyr = 3, subset_wetdry = c("all", "wet", "dry"), precipdata = NULL,
                           temporal_window = NULL, wet_threshold = NULL){
  
  data(tbsegdetail)
  
  # get categories
  cols <- c('#2DC938', '#E9C318', '#EE7600', '#CC3231', '#800080')
  names(cols) <- c('A', 'B', 'C', 'D', 'E')
  
  # check if epchc data
  isepchc <- exists("epchc_station", fibdata)
  
  if(!isepchc){
    
    # includes bay segment check
    tomapseg <- anlz_fibmatrix(fibdata, yrrng = c(yrsel - lagyr, yrsel), stas = NULL, bay_segment = areasel,
                               indic = indic, threshold = threshold, lagyr = lagyr,
                               subset_wetdry = subset_wetdry, precipdata = precipdata,
                               temporal_window = temporal_window, wet_threshold = wet_threshold) %>%
      dplyr::filter(!is.na(cat)) %>%
      dplyr::filter(yr == !!yrsel) %>%
      dplyr::inner_join(tbsegdetail, ., by = c('bay_segment' = 'grp')) %>%
      dplyr::mutate(
        lab = paste0('<html>', long_name, '<br>Category: ', cat),
        col = as.character(cols[cat])
      )
    
    stas <- fibdata %>%
      dplyr::filter(bay_segment %in% !!areasel) %>%
      dplyr::filter(yr <= !!yrsel & yr >= (!!yrsel - !!lagyr)) %>%
      dplyr::pull(station) %>%
      unique()
    
    tomapsta <- anlz_fibmatrix(fibdata, yrrng = c(yrsel - lagyr, yrsel), stas = stas, bay_segment = NULL,
                               indic = indic, threshold = threshold, lagyr = lagyr,
                               subset_wetdry = subset_wetdry, precipdata = precipdata,
                               temporal_window = temporal_window, wet_threshold = wet_threshold)
    
  }
  
  if(isepchc){
    
    # check bay segment
    areas <- c('Alafia River', 'Hillsborough River', 'Big Bend', 'Cockroach Bay',
               'East Lake Outfall', 'Hillsborough Bay', 'Little Manatee River', 'Lower Tampa Bay',
               'McKay Bay', 'Middle Tampa Bay', 'Old Tampa Bay', 'Palm River', 'Tampa Bypass Canal',
               'Valrico Lake')
    
    chk <- !areasel %in% areas
    if(any(chk)){
      stop('Invalid value(s) for areasel: ', paste(areasel[chk], collapse = ', '))
    }
    
    stas <- fibdata %>%
      dplyr::filter(area %in% !!areasel) %>%
      dplyr::filter(yr <= !!yrsel & yr >= (!!yrsel - !!lagyr)) %>%
      dplyr::pull(epchc_station) %>%
      unique()
    
    tomapsta <- anlz_fibmatrix(fibdata, yrrng = c(yrsel - lagyr, yrsel), stas = stas, bay_segment = NULL,
                               indic = indic, threshold = threshold, lagyr = lagyr,
                               subset_wetdry = subset_wetdry, precipdata = precipdata,
                               temporal_window = temporal_window, wet_threshold = wet_threshold)
    
  }
  
  # create custom icon list for fib matrix categories
  icons <- util_fibicons('fibmat')
  
  # levs
  levs <- util_fiblevs()
  
  tomapsta <- tomapsta %>%
    dplyr::filter(!is.na(cat)) %>%
    dplyr::filter(yr == !!yrsel) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
    dplyr::mutate(
      cat = factor(cat, levels = levs$fibmatlev),
      lab = paste0('<html>Station Number: ', grp, '<br>Category: ', cat)
    )
  
  out <- list(
    icons = icons,
    tomapsta = tomapsta
  )
  
  # add bay segments if not epchc
  if(!isepchc){
    
    out <- c(out, list(
      tomapseg = tomapseg
    ))
    
  }
  
  return(out)
  
}