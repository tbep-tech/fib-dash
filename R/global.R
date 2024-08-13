box::use(
  dplyr[`%>%`]
)

data('fibdata', package = 'tbeptools')
data('enterodata', package = 'tbeptools')
data('catchprecip', package = 'tbeptools')

cols <- c('#CC3231', '#E9C318', '#2DC938')

maxyr <- 2023

mos <- as.list(1:12)
names(mos) <- month.abb

areas1 <- c("Boca Ciega Bay", "Hillsborough Bay", "Lower Tampa Bay", "Manatee River", 
            "Middle Tampa Bay", "Old Tampa Bay")
areas2 <- c('Alafia River', 'Hillsborough River', 'Big Bend', 'Cockroach Bay', 
           'East Lake Outfall', 'Hillsborough Bay', 'Little Manatee River', 'Lower Tampa Bay',
           'McKay Bay', 'Middle Tampa Bay', 'Old Tampa Bay', 'Palm River', 'Tampa Bypass Canal',
           'Valrico Lake')


# create custom icon list for fib categories
ecocciicons <- leaflet::iconList(
  ecocci_green_wet <- leaflet::makeIcon(iconUrl = system.file('ecoli_green.png', package = 'tbeptools'),
                                        iconWidth = 18, iconHeight = 18),
  ecocci_yellow_wet <- leaflet::makeIcon(iconUrl = system.file('ecoli_yellow.png', package = 'tbeptools'),
                                         iconWidth = 18, iconHeight = 18),
  ecocci_orange_wet <- leaflet::makeIcon(iconUrl = system.file('ecoli_orange.png', package = 'tbeptools'),
                                         iconWidth = 18, iconHeight = 18),
  ecocci_red_wet <- leaflet::makeIcon(iconUrl = system.file('ecoli_red.png', package = 'tbeptools'),
                                      iconWidth = 18, iconHeight = 18),
  ecocci_green_dry <- leaflet::makeIcon(iconUrl = system.file('ecocci_green.png', package = 'tbeptools'),
                                        iconWidth = 18, iconHeight = 18),
  ecocci_yellow_dry <- leaflet::makeIcon(iconUrl = system.file('ecocci_yellow.png', package = 'tbeptools'),
                                         iconWidth = 18, iconHeight = 18),
  ecocci_orange_dry <- leaflet::makeIcon(iconUrl = system.file('ecocci_orange.png', package = 'tbeptools'),
                                         iconWidth = 18, iconHeight = 18),
  ecocci_red_dry <- leaflet::makeIcon(iconUrl = system.file('ecocci_red.png', package = 'tbeptools'),
                                      iconWidth = 18, iconHeight = 18)
  
)

# legend as HTML string
# using previously created code and icons that have ecoli in the name -
# so monkeying around a bit to make shapes match correctly
ecoccilevs <- tbeptools::util_fiblevs()
ecoccileg <- tibble::tibble(
  src = paste0('https://github.com/tbep-tech/tbeptools/blob/master/inst/', basename(sapply(ecocciicons, `[[`, 1)), '?raw=true'),
  brk = rep(ecoccilevs$ecoccilbs, times = 2)
) %>%
  tidyr::unite('val', src, brk, sep = "' style='width:10px;height:10px;'> ") %>%
  dplyr::mutate(
    val = paste0("<img src='", val)
  ) %>%
  dplyr::pull(val)
ecoccidryleg <- ecoccileg %>%
  grep('ecoli', ., value = T) %>%
  paste(collapse = '<br/>') %>%
  paste0('<b>Dry samples</b><br/>#/100mL<br/>', .)
ecocciwetleg <- ecoccileg %>%
  grep('ecocci', ., value = T) %>%
  paste(collapse = '<br/>') %>%
  paste0('<b>Wet samples</b><br/>#/100mL<br/>', .)
ecocciallleg <- ecoccileg %>%
  grep('ecoli', ., value = T) %>%
  paste(collapse = '<br/>') %>%
  paste0('<b>All samples</b><br/>#/100mL<br/>', .)

# create custom icon list for fib categories
fibicons <- leaflet::iconList(
  ecoli_green <- leaflet::makeIcon(iconUrl = system.file('ecoli_green.png', package = 'tbeptools'),
                                   iconWidth = 18, iconHeight = 18),
  ecoli_yellow <- leaflet::makeIcon(iconUrl = system.file('ecoli_yellow.png', package = 'tbeptools'),
                                    iconWidth = 18, iconHeight = 18),
  ecoli_orange <- leaflet::makeIcon(iconUrl = system.file('ecoli_orange.png', package = 'tbeptools'),
                                    iconWidth = 18, iconHeight = 18),
  ecoli_red <- leaflet::makeIcon(iconUrl = system.file('ecoli_red.png', package = 'tbeptools'),
                                 iconWidth = 18, iconHeight = 18),
  ecocci_green <- leaflet::makeIcon(iconUrl = system.file('ecocci_green.png', package = 'tbeptools'),
                                    iconWidth = 18, iconHeight = 18),
  ecocci_yellow <- leaflet::makeIcon(iconUrl = system.file('ecocci_yellow.png', package = 'tbeptools'),
                                     iconWidth = 18, iconHeight = 18),
  ecocci_orange <- leaflet::makeIcon(iconUrl = system.file('ecocci_orange.png', package = 'tbeptools'),
                                     iconWidth = 18, iconHeight = 18),
  ecocci_red <- leaflet::makeIcon(iconUrl = system.file('ecocci_red.png', package = 'tbeptools'),
                                  iconWidth = 18, iconHeight = 18)
)

# legend as HTML string
fiblevs <- tbeptools::util_fiblevs()
fibleg <- tibble::tibble(
  src = paste0('https://github.com/tbep-tech/tbeptools/blob/master/inst/', basename(sapply(fibicons, `[[`, 1)), '?raw=true'),
  brk = c(fiblevs$ecolilbs, fiblevs$ecoccilbs)
) %>%
  tidyr::unite('val', src, brk, sep = "' style='width:10px;height:10px;'> ") %>%
  dplyr::mutate(
    val = paste0("<img src='", val)
  ) %>%
  dplyr::pull(val)
ecolileg <- fibleg %>%
  grep('ecoli', ., value = T) %>%
  paste(collapse = '<br/>') %>%
  paste0('<b>Freshwater (<em>E. Coli</em>)</b><br/>#/100mL<br/>', .)
ecoccileg <- fibleg %>%
  grep('ecocci', ., value = T) %>%
  paste(collapse = '<br/>') %>%
  paste0('<b>Marine (<em>Enterococcus</em>)</b><br/>#/100mL<br/>', .)