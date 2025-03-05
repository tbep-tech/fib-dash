box::use(
  dplyr[`%>%`]
)

data('enterodata', package = 'tbeptools')
data('fibdata', package = 'tbeptools')
data('mancofibdata', package = 'tbeptools')
data('pascofibdata', package = 'tbeptools')
data('polcofibdata', package = 'tbeptools')
data('catchprecip', package = 'tbeptools')
data('tbsegdetail', package = 'tbeptools')

enterowetdry <- tbeptools::anlz_fibwetdry(enterodata, catchprecip, temporal_window = 2, wet_threshold = 0.5)

cols <- c('#CC3231', '#E9C318', '#2DC938')

yrmin1 <- 2002
yrmin2 <- 2001
yrmin3 <- 2013
yrmin4 <- 2017
yrmin5 <- 2017
maxyr <- 2023

lwid <- 1.5

mos <- as.list(1:12)
names(mos) <- month.abb

# baywide
areas1 <- list("BCB", "HB", "LTB", "MR", "MTB", "OTB")
names(areas1) <- c("Boca Ciega Bay", "Hillsborough Bay", "Lower Tampa Bay", "Manatee River", 
                             "Middle Tampa Bay", "Old Tampa Bay")

# hillsborough
areas2 <- c('Alafia River', 'Hillsborough River', 'Cockroach Bay', 
           'East Lake Outfall', 'Hillsborough Bay', 'Little Manatee River', 'Lower Tampa Bay',
           'McKay Bay', 'Middle Tampa Bay', 'Old Tampa Bay', 'Palm River', 'Tampa Bypass Canal',
           'Valrico Lake')

# manatee
areas3 <- c("Bowlees Creek", "Braden River", 
            "Clay Gully", "Frog Creek", "Gap Creek", "Little Manatee River", 
            "Manatee River", "Mcmullen Creek", 
            "Myakka River", "Palma Sola Bay")

# pasco
areas4 <- c("Anclote River", "Bear Creek", "Cypress Creek", "Hillsborough River", 
            "New River", "Pithlachascotee River", "Trout Creek")

# polk
areas5 <- c("Alafia River", "Bear Creek", "Blackwater Creek", "Boggy Branch", 
            "Charlie Creek", "English Creek", "Fox Branch", "Gator Creek", 
            "Hamwet", "Horse Creek", "Itchepack Creek", "Lena Run", "Livingston Creek", 
            "Marion Creek", "Old Town Creek", "PC Canal", "Peace River", 
            "Poley Creek", "Pony Creek", "Reedy Inflow", "Saddle Creek", 
            "Scenic", "Simmers-Young Canal", "Thirtymile Creek", "Tiger Creek", 
            "Tiger Inflow", "W Bowlegs Creek", "Wahneta Canal", "Weohyakapka Creek", 
            "Withlacoochee River")

# create custom icon list for fib categories
ecocciicons <- tbeptools::util_fibicons('entero')

# create custom icon list for fib categories
fibicons <- tbeptools::util_fibicons('entero&ecoli')

# plotly modebar items to remove
modrm <- c('autoScale', 'select2d', 'lasso2d')
