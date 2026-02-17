box::use(
  dplyr[`%>%`]
)

##
# all files from tbeptools, but saved locally so the image doesnt have to be recreated with data updates

# updated annually
load(url('https://tbep-tech.github.io/fib-dash/data/enterodata.RData'))
load(url('https://tbep-tech.github.io/fib-dash/data/catchprecip.RData'))

# updated by github actions weekly
load(url('https://tbep-tech.github.io/fib-dash/data/fibdata.RData'))
load(url('https://tbep-tech.github.io/fib-dash/data/hcesdfibdata.RData'))
load(url('https://tbep-tech.github.io/fib-dash/data/mancofibdata.RData'))
load(url('https://tbep-tech.github.io/fib-dash/data/pascofibdata.RData'))
load(url('https://tbep-tech.github.io/fib-dash/data/polcofibdata.RData'))

# never updated
data('tbsegdetail', package = 'tbeptools')
data('dbasin', package = 'tbeptools')

enterowetdry <- tbeptools::anlz_fibwetdry(enterodata, catchprecip, temporal_window = 2, wet_threshold = 0.5)

cols <- c('#CC3231', '#E9C318', '#2DC938')

entminyr <- 2002
epcminyr <- 2001
esdminyr <- 2019
manminyr <- 2018
pasminyr <- 2017
polminyr <- 2017
entmaxyr <- max(enterodata$yr)
epcmaxyr <- max(fibdata$yr)
esdmaxyr <- max(hcesdfibdata$yr)
manmaxyr <- max(mancofibdata$yr)
pasmaxyr <- max(pascofibdata$yr)
polmaxyr <- max(polcofibdata$yr)

lwid <- 1.5

mos <- as.list(1:12)
names(mos) <- month.abb

# baywide
areas1 <- list("BCB", "HB", "LTB", "MR", "MTB", "OTB")
names(areas1) <- c("Boca Ciega Bay", "Hillsborough Bay", "Lower Tampa Bay", "Manatee River", 
                             "Middle Tampa Bay", "Old Tampa Bay")

# hillsborough epchc
areas2 <- c('Alafia River', 'Hillsborough River', 'Cockroach Bay', 
           'East Lake Outfall', 'Hillsborough Bay', 'Little Manatee River', 'Lower Tampa Bay',
           'McKay Bay', 'Middle Tampa Bay', 'Old Tampa Bay', 'Palm River', 'Tampa Bypass Canal',
           'Valrico Lake')

# hillsborough esd
areas3 <- c("Beaudette Pond", "BLO", "Channel G", "Cypress Creek", "Delaney Creek", 
            "East Lake", "Egypt Lake", "Henry Street Canal", "Lake Carroll", 
            "Lake Darby", "Lake Grady", "Lake Magdalene", "Lake Mango", "Lake Valrico", 
            "Lake Weeks", "Mango Drain", "Taliaferro Pond", "Valrico Canal")

# manatee
areas4 <- c("Bowlees Creek", "Braden River", 
            "Clay Gully", "Frog Creek", "Gap Creek", "Little Manatee River", 
            "Manatee River", "Mcmullen Creek", 
            "Myakka River", "Palma Sola Bay")

# pasco
areas5 <- c("Anclote River", "Bear Creek", "Cypress Creek", "Hillsborough River", 
            "New River", "Pithlachascotee River", "Trout Creek")

# polk
areas6 <- c("Alafia River", "Bear Creek", "Blackwater Creek", "Boggy Branch", 
            "Bowlegs Creek", "Charlie Creek", "English Creek", "Fox Branch", "Gator Creek", 
            "Hamwet", "Horse Creek", "Itchepack Creek", "Lena Run", "Livingston Creek", 
            "Marion Creek", "Old Town Creek", "PC Canal", "Peace River", 
            "Poley Creek", "Pony Creek", "Reedy Inflow", "Saddle Creek", 
            "Scenic", "Simmers-Young Canal", "Thirtymile Creek", "Tiger Creek", 
            "Tiger Inflow", "Wahneta Canal", "Weohyakapka Creek", 
            "Withlacoochee River")

# create custom icon list for fib categories
ecocciicons <- tbeptools::util_fibicons('entero')

# create custom icon list for fib categories
fibicons <- tbeptools::util_fibicons('entero&ecoli')

# plotly modebar items to remove
modrm <- c('autoScale', 'select2d', 'lasso2d')
