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
yrmin2 <- 2001
yrmin3 <- 2013
maxyr <- 2023

lwid <- 1.5

mos <- as.list(1:12)
names(mos) <- month.abb

areas1 <- list("BCB", "HB", "LTB", "MR", "MTB", "OTB")
names(areas1) <- c("Boca Ciega Bay", "Hillsborough Bay", "Lower Tampa Bay", "Manatee River", 
                             "Middle Tampa Bay", "Old Tampa Bay")

areas2 <- c('Alafia River', 'Hillsborough River', 'Cockroach Bay', 
           'East Lake Outfall', 'Hillsborough Bay', 'Little Manatee River', 'Lower Tampa Bay',
           'McKay Bay', 'Middle Tampa Bay', 'Old Tampa Bay', 'Palm River', 'Tampa Bypass Canal',
           'Valrico Lake')

areas3 <- c("Bowlees Creek", "Braden River", 
            "Clay Gully", "Frog Creek", "Gap Creek", "Little Manatee River", 
            "Manatee River", "Mcmullen Creek", 
            "Myakka River", "Palma Sola Bay")

# create custom icon list for fib categories
ecocciicons <- tbeptools::util_fibicons('entero')

# create custom icon list for fib categories
fibicons <- tbeptools::util_fibicons('entero&ecoli')

# plotly modebar items to remove
modrm <- c('autoScale', 'select2d', 'lasso2d')
