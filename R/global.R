box::use(
  dplyr[`%>%`]
)

data('fibdata', package = 'tbeptools')
data('enterodata', package = 'tbeptools')

cols <- c('#CC3231', '#E9C318', '#2DC938')

maxyr <- 2023

mos <- as.list(1:12)
names(mos) <- month.abb

areas <- c('Alafia River', 'Hillsborough River', 'Big Bend', 'Cockroach Bay', 
           'East Lake Outfall', 'Hillsborough Bay', 'Little Manatee River', 'Lower Tampa Bay',
           'McKay Bay', 'Middle Tampa Bay', 'Old Tampa Bay', 'Palm River', 'Tampa Bypass Canal',
           'Valrico Lake')

# create custom icon list for fib categories
icons <- leaflet::iconList(
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
levs <- tbeptools::util_fiblevs()
leg <- tibble::tibble(
  src = paste0('https://github.com/tbep-tech/tbeptools/blob/master/inst/', basename(sapply(icons, `[[`, 1)), '?raw=true'),
  brk = c(levs$ecolilbs, levs$ecoccilbs)
) %>%
  tidyr::unite('val', src, brk, sep = "' style='width:10px;height:10px;'> ") %>%
  dplyr::mutate(
    val = paste0("<img src='", val)
  ) %>%
  dplyr::pull(val)
ecolileg <- leg %>%
  grep('ecoli', ., value = T) %>%
  paste(collapse = '<br/>') %>%
  paste0('<b>Freshwater (<em>E. Coli</em>)</b><br/>#/100mL<br/>', .)
ecoccileg <- leg %>%
  grep('ecocci', ., value = T) %>%
  paste(collapse = '<br/>') %>%
  paste0('<b>Marine (<em>Enterococcus</em>)</b><br/>#/100mL<br/>', .)