library(tbeptools)

# EPC
cat('EPC\n')
xlsx <- tempfile(fileext = '.xlsx')
fibdata <- read_importfib(xlsx, download_latest = TRUE)
save(fibdata, file = here::here('data/fibdata.RData'))

# ESC
cat('ESC\n')
hcesdfibdata <- read_importwqp('21FLHESD_WQX', type = 'fib')
save(hcesdfibdata, file = here::here('data/hcesdfibdata.RData'))

# Manatee County
cat('manco\n')
mancofibdata <- read_importwqp('21FLMANA_WQX', type = 'fib')
save(mancofibdata, file = here::here('data/mancofibdata.RData'))

# Pasco County
cat('pasco\n')
pascofibdata <- read_importwqp('21FLPASC_WQX', type = 'fib')
save(pascofibdata, file = here::here('data/pascofibdata.RData'))

# Polk County
cat('polco\n')
polcofibdata <- read_importwqp('21FLPOLK_WQX', type = 'fib')
save(polcofibdata, file = here::here('data/polcofibdata.RData'))
