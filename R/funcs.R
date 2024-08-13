enterotomap_fun <- function(fibdata, yrsel, mosel, areasel, wetdry = FALSE, precipdata = NULL, temporal_window = NULL, wet_threshold = NULL){
  
  # get categories
  fibmap <- tbeptools::anlz_enteromap(fibdata, yrsel = yrsel, mosel = mosel, areasel = areasel, wetdry = wetdry,
                           precipdata = precipdata, temporal_window = temporal_window,
                           wet_threshold = wet_threshold)
  
  # make a column even if wetdry wasn't selected
  # and if it was, give it something other than true/false
  if (wetdry == FALSE) {
    fibmap$wet_sample = factor("all",
                               levels = "all",
                               labels = "all")
  } else {
    fibmap <- fibmap %>%
      dplyr::mutate(wet_sample = factor(dplyr::case_when(wet_sample == TRUE ~ "wet",
                                                         .default = "dry"),
                                        levels = c("dry", "wet"),
                                        labels = c("dry", "wet")))
  }
  
  # create the object to map
  tomap <- fibmap %>%
    dplyr::filter(!is.na(Longitude)) %>%
    dplyr::filter(!is.na(Latitude)) %>%
    dplyr::filter(!is.na(cat)) %>%
    sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>%
    dplyr::mutate(
      colnm = factor(col,
                     levels = c('#2DC938', '#E9C318', '#EE7600', '#CC3231'),
                     labels = c('green', 'yellow', 'orange', 'red')
      ),
      conc = round(conc, 1),
      cls = 'Marine'
    ) %>%
    tidyr::unite('grp', indnm, colnm, wet_sample, remove = F)
  
  # create levels for group, must match order of icons list
  levs <- expand.grid(levels(tomap$colnm), unique(tomap$indnm), levels(tomap$wet_sample)) %>%
    tidyr::unite('levs', Var2, Var1, Var3) %>%
    dplyr::pull(levs)
  
  # get correct levels
  tomap <- tomap %>%
    dplyr::mutate(
      grp = factor(grp, levels = levs),
      lab = paste0('<html>Station Number: ', station, '<br>Class: ', cls, ' (<i>', ind, '</i>)<br> Category: ', cat, ' (', conc, '/100mL)</html>')
    ) %>%
    dplyr::select(-colnm, -indnm)
  
  return(tomap)
  
}

fibtomap_fun <- function(fibdata, yrsel, mosel, areasel){
  
  # get categories
  tofibmap <- tbeptools::anlz_fibmap(fibdata, yrsel = yrsel, mosel = mosel, areasel = areasel)
  
  # create the object to map
  tomap <- tofibmap %>%
    dplyr::filter(!is.na(Longitude)) %>%
    dplyr::filter(!is.na(Latitude)) %>%
    dplyr::filter(!is.na(cat)) %>%
    sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>%
    dplyr::mutate(
      colnm = factor(col,
                     levels = c('#2DC938', '#E9C318', '#EE7600', '#CC3231'),
                     labels = c('green', 'yellow', 'orange', 'red')
      ),
      indnm = factor(ind,
                     levels = c('E. coli', 'Enterococcus'),
                     labels = c('ecoli', 'ecocci')
      ),
      conc = dplyr::case_when(
        indnm == 'ecoli' ~ ecoli,
        indnm == 'ecocci' ~ ecocci
      ),
      conc = round(conc, 1),
      cls = dplyr::case_when(
        indnm == 'ecoli' ~ 'Freshwater',
        indnm == 'ecocci' ~ 'Marine'
      )
    ) %>%
    tidyr::unite('grp', indnm, colnm, remove = F)
  
  # create levels for group, must match order of icons list
  levs <- expand.grid(levels(tomap$colnm), levels(tomap$indnm)) %>%
    tidyr::unite('levs', Var2, Var1) %>%
    dplyr::pull(levs)
  
  # get correct levels
  tomap <- tomap %>%
    dplyr::mutate(
      grp = factor(grp, levels = levs),
      lab = paste0('<html>Station Number: ', epchc_station, '<br>Class: ', cls, ' (<i>', ind, '</i>)<br> Category: ', cat, ' (', conc, '/100mL)</html>')
    ) %>%
    dplyr::select(-colnm, -indnm)
  
  return(tomap)
  
}