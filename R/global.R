box::use(
  dplyr[`%>%`]
)

data('enterodata', package = 'tbeptools')
data('fibdata', package = 'tbeptools')
data('mancofibdata', package = 'tbeptools')
data('catchprecip', package = 'tbeptools')
data('tbsegdetail', package = 'tbeptools')

enterowetdry <- tbeptools::anlz_fibwetdry(enterodata, catchprecip, temporal_window = 2, wet_threshold = 0.5)

cols <- c('#CC3231', '#E9C318', '#2DC938')

yrmin1 <- 2002
yrmin2 <- 1976
yrmin3 <- 1997
maxyr <- 2023

lwid <- 1.5

mos <- as.list(1:12)
names(mos) <- month.abb

areas1 <- list("BCB", "HB", "LTB", "MR", "MTB", "OTB")
names(areas1) <- c("Boca Ciega Bay", "Hillsborough Bay", "Lower Tampa Bay", "Manatee River", 
                             "Middle Tampa Bay", "Old Tampa Bay")

areas2 <- c('Alafia River', 'Hillsborough River', 'Big Bend', 'Cockroach Bay', 
           'East Lake Outfall', 'Hillsborough Bay', 'Little Manatee River', 'Lower Tampa Bay',
           'McKay Bay', 'Middle Tampa Bay', 'Old Tampa Bay', 'Palm River', 'Tampa Bypass Canal',
           'Valrico Lake')

areas3 <- c("Big Slough", "Bowlees Creek", "Braden River", "Bud Slough", 
            "Clay Gully", "Frog Creek", "Gap Creek", "Little Manatee River", 
            "Lower Tampa Bay", "Manatee River", "Mcmullen Creek", "Mud Lake Slough", 
            "Myakka River", "Palma Sola Bay", "Piney Point Creek")

# create custom icon list for fib categories
ecocciicons <- tbeptools::util_fibicons('entero')

# create custom icon list for fib categories
fibicons <- tbeptools::util_fibicons('fcolif')
