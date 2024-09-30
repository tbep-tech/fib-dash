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