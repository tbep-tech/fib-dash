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