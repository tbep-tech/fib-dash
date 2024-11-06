library(shiny)
library(bslib)
library(markdown)

# Source required files (commented out as we don't have access to them)
source(here::here('R/global.R'))
source(here::here('R/funcs.R'))

# Add Google Analytics
ga_tag <- tags$head(
  includeHTML("google-analytics.html")
)

ui <- page_navbar(
  title = "TAMPA BAY FIB DASHBOARD",
  
  # Add Google Analytics and custom CSS
  header = tagList(
    ga_tag,
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    # ), 
    # Add Google Analytics and custom CSS
    tags$style("
    .fill-height {
      display: flex;
      flex-direction: column;
      height: calc(100vh - 60px);
      overflow: hidden;
      padding: 1rem;  /* Add padding to fill-height */
    }
    .resizable-row {
      display: flex;
      gap: 1rem;  /* Increased gap to match card spacing */
      flex: 1;
      min-height: 0;
      padding: 0;  /* Remove padding from resizable-row */
    }
    .resizable-column {
      resize: horizontal;
      overflow: hidden;
      min-width: 200px;
      max-width: 90%;
    }
    .right-column {
      flex: 1;
      min-width: 200px;
    }
    .card {
      height: 100%;
      border: 1px solid rgba(0,0,0,.125);  
      display: flex;
      flex-direction: column;
    }
    .card-scroll {
      overflow-y: auto;
      scrollbar-width: none;
      -ms-overflow-style: none;
    },
    .modal-backdrop.show {
      z-index: 2056 !important;
    }
    .modal.show {
      z-index: 2057 !important;
    }
    .modal-dialog {
      z-index: 2057 !important;
    }
    .resizable-column {
      z-index: 1000;  /* Base z-index for normal state */
    }
    .resizable-column .card-fullscreen,
    .resizable-column .bslib-card.card-fullscreen,
    .resizable-column .bslib-navs-underline,
    .resizable-column .nav-tabs-container {
      z-index: 1055 !important;
    }
    /* Ensure the modal is always on top */
    body.modal-open .modal {
      z-index: 2057 !important;
    }
    .leaflet-top,
    .leaflet-bottom,
    .leaflet-left,
    .leaflet-right,
    .leaflet-control-container,
    .leaflet-control-zoom,
    .leaflet-control-attribution,
    .leaflet-control-layers,
    .leaflet-pane {
      z-index: 800 !important;
    }
  ")
    ),
  
  # Add logo
  nav_item(
    tags$img(src = "tarponlogo.png", height = "30px", style = "margin-right: 10px;")
  ),
  
  # First nav item - Overview
  nav_panel(
    class = 'fill-height',
    title = "OVERVIEW",
    navset_card_underline(
      full_screen = TRUE,
      height = '100%',
      nav_panel(
        title = "USING THE DASHBOARD",
        class = 'card-scroll',
        shiny::includeMarkdown('www/overview.md')   
      ),
      nav_panel(
        title = 'METHODS', 
        shiny::includeMarkdown('www/methods.md')
      )
    )
  ),
  
  # Second nav item - Baywide
  nav_panel(
    title = "1 BAYWIDE",
    class = 'fill-height',
    layout_columns(
      fill = F,
      card(
        height = 'auto',
        width = 12,
        div(
          style = 'display: flex; gap: 0rem; align-items: flex-end;',
          div(
            style = 'width: 33.33%;',
            div(
              style = "display: flex; flex-direction: column;",
              div(style = "height: 25px;", "Select year:"), 
              sliderInput('yrsel1', NULL, min = yrmin1, max = maxyr, value = maxyr, step = 1, sep = '', width = '90%')
            )
          ),
          div(
            style = 'width: 66.66%;',
            div(
              style = 'display: flex; flex-direction: column;',
              div(style = "height: 50px;", "Select area:"), 
              selectizeInput('areasel1', NULL, choices = areas1, selected = c('HB', 'OTB', 'MTB', 'LTB', 'BCB', 'MR'), multiple = T, width = '100%', options = list(dropdownParent = 'body')),
            )
          )
        )
      )
    ),
    div(
      class = 'resizable-row',
      div(
        class = 'resizable-column',
        style = 'width: 33.33%',
        navset_card_underline(
          full_screen = TRUE,
          nav_panel(
            "REPORT CARD",
              shinyWidgets::materialSwitch('segsel1', 'By bay segment?', value = T, width = '100%'),
              plotly::plotlyOutput('entmatrix', height = '100%')
            ),
          nav_panel(
            "Using this tab",
            shiny::includeMarkdown('www/baywideusing.md')
          )
        )
      ),
  
    div(
      class='right-column', 
      style = 'width: 66.66%',
      navset_card_underline(
        full_screen = T,
        nav_panel(
          "MAP BY YEAR",
          leaflet::leafletOutput('entmapyr')
        ),
        nav_panel(
          "MAP BY YEAR AND MONTH",
          div(
            style = "display: flex; align-items: center; gap: 1rem;",
            shinyWidgets::sliderTextInput('mosel1', 'Select month:', choices = names(mos), selected = 'Jul', force_edges = T, grid = T, width = '50%'),
            span('Click on a station to view a complete time series')
          ),
          leaflet::leafletOutput('entmap')
          )
        )
      )
    )
  ),

  # Third nav item - Hillsborough County
  nav_panel(
    title = "2 HILLSBOROUGH COUNTY",
    class = 'fill-height',
    layout_columns(
      fill = F,
      card(
        height = 'auto',
        width = 12,
        div(
          style = 'display: flex; gap: 0rem; align-items: flex-end;',
          div(
            style = 'width: 33.33%;',
            div(
              style = "display: flex; flex-direction: column;",
              div(style = "height: 25px;", "Select year:"), 
              sliderInput('yrsel2', NULL, min = yrmin2, max = maxyr, value = maxyr, step = 1, sep = '', width = '90%')
            )
          ),
          div(
            style = 'width: 66.66%;',
            div(
              style = 'display: flex; flex-direction: column;',
              div(style = "height: 50px;", "Select area:"), 
              selectizeInput('areasel2', NULL, choices = areas2, selected = c('Hillsborough River', 'Alafia River'), multiple = T, width = '100%', options = list(dropdownParent = 'body')),
            )
          )
        )
      )
    ),
    div(
      class = 'resizable-row',
      div(
        class = 'resizable-column',
        style = 'width: 33.33%',
        navset_card_underline(
          full_screen = TRUE,
          nav_panel(
            "REPORT CARD",
            plotly::plotlyOutput('fibmatrix', height = '100%')
          ),
          nav_panel(
            "Using this tab",
            shiny::includeMarkdown('www/hillsboroughusing.md')
          )
        )
      )
    ),
      
    div(
      class='right-column', 
      style = 'width: 66.66%',
      navset_card_underline(
        full_screen = T,
        nav_panel(
          "MAP BY YEAR",
          leaflet::leafletOutput('fibmapyr')
        ),
        nav_panel(
          "MAP BY YEAR AND MONTH",
          div(
            style = "display: flex; align-items: center; gap: 1rem;",
            shinyWidgets::sliderTextInput('mosel2', 'Select month:', choices = names(mos), selected = 'Jul', force_edges = T, grid = T, width = '50%'),
            span('Click on a station to view a complete time series')
          ),
          leaflet::leafletOutput('fibmap')
        )
      )
    )
    
  ),
  
  # Fourth nav item - Manatee County
  nav_panel(
    title = "3 MANATEE COUNTY",
    class = 'fill-height',
    layout_columns(
      fill = F,
      card(
        height = 'auto',
        width = 12,
        div(
          style = 'display: flex; gap: 0rem; align-items: flex-end;',
          div(
            style = 'width: 33.33%;',
            div(
              style = "display: flex; flex-direction: column;",
              div(style = "height: 25px;", "Select year:"), 
              sliderInput('yrsel3', NULL, min = yrmin3, max = maxyr, value = maxyr - 2, step = 1, sep = '', width = '90%')
            )
          ),
          div(
            style = 'width: 66.66%;',
            div(
              style = 'display: flex; flex-direction: column;',
              div(style = "height: 50px;", "Select area:"), 
              selectizeInput('areasel3', NULL, choices = areas3, selected = c('Braden River', 'Manatee River'), multiple = T, width = '100%', options = list(dropdownParent = 'body')),
            )
          )
        )
      )
    ),
    div(
      class = 'resizable-row',
      div(
        class = 'resizable-column',
        style = 'width: 33.33%',
        navset_card_underline(
          full_screen = TRUE,
          nav_panel(
            "REPORT CARD",
            plotly::plotlyOutput('mancofibmatrix', height = '100%')
          ),
          nav_panel(
            "Using this tab",
            shiny::includeMarkdown('www/manateeusing.md')
          )
        )
      ),
      
      div(
        class='right-column', 
        style = 'width: 66.66%',
        navset_card_underline(
          full_screen = T,
          nav_panel(
            "MAP BY YEAR",
            leaflet::leafletOutput('mancofibmapyr')
          ),
          nav_panel(
            "MAP BY YEAR AND MONTH",
            div(
              style = "display: flex; align-items: center; gap: 1rem;",
              shinyWidgets::sliderTextInput('mosel3', 'Select month:', choices = names(mos), selected = 'Jul', force_edges = T, grid = T, width = '50%'),
              span('Click on a station to view a complete time series')
            ),
            leaflet::leafletOutput('mancofibmap')
          )
        )
      )
    )
  ),
  
  # Fifth nav item - Data Downloads
  nav_panel(
    title = "4 DATA DOWNLOADS",
    navset_card_underline(
      full_screen = TRUE,
      height = '100%',
      nav_panel(
        title = "DOWNLOAD",
        div(
          style = 'display: flex; gap: 0rem; align-items: flex-end;',
          div(
            style = 'width: 50.00%;',
            div(
              style = "display: flex; flex-direction: column;",
              div(style = "height: 50px;", "Select type:"), 
              selectInput('typseldl', NULL, choices = c('Baywide segment score categories', 'Baywide station score categories', 'Baywide raw data', 'Hillsborough County station score categories', 'Hillsborough County raw data', 'Manatee County station score categories', 'Manatee County raw data'), width = '95%')
            )
          ),
          div(
            style = 'width: 50.00%;',
            div(
              style = 'display: flex; flex-direction: column;',
              div(style = "height: 25px;", "Select year range:"), 
              uiOutput('yrseldl')
            )
          )
        ),
        reactable::reactableOutput('dltabout'),
        shinyWidgets::downloadBttn('dwnld', 'Download data', style = 'simple', block = T, color = 'success')
      ),
      nav_panel(
        "METADATA",
        shiny::includeMarkdown('www/metadata.md')
      )
    )
  ),
  
  # Navbar configuration
  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://github.com/tbep-tech/fib-dash",
      target = "_blank",
      "Source Code"
    )
  )
)

server <- function(input, output, session) {
  
  # baywide ent matrix
  entmatrix <- reactive({
    
    # inputs
    areasel1 <- input$areasel1
    segsel1 <- input$segsel1
    yrsel1 <- input$yrsel1

    if(!segsel1){
      stas <- enterodata %>%
        dplyr::filter(bay_segment %in% areasel1) %>%
        dplyr::select(station) %>%
        dplyr::distinct() %>%
        dplyr::pull()

      p <- try(tbeptools::show_fibmatrix(enterodata, stas = stas, yrrng = c(yrmin1, maxyr),
                                         indic = 'entero', warn = F))

    }

    if(segsel1){

      p <- try(tbeptools::show_fibmatrix(enterodata, stas = NULL, bay_segment = areasel1,
                                         yrrng = c(yrmin1, maxyr), indic = 'entero', warn = F))

    }

    validate(
      need(!inherits(p, 'try-error'), 'No enterococcus data available for selection')
    )

    out <- mataddyr_fun(p, yrsel = yrsel1) |>
      plotly::layout(
        autosize = TRUE
      )


    return(out)
    
  })
  
  # data for baywide ent map, yr
  yrtomap1 <- reactive({
    
    yrsel1 <- input$yrsel1
    areasel1 <- input$areasel1
    
    req(areasel1)
    
    out <- tbeptools::show_fibmatmap(enterodata, yrsel = yrsel1, areasel = areasel1, 
                                     indic = 'entero', listout = T, warn = F)
    
    return(out)
    
  }) 
  
  # baywide ent data to map, yr
  observe({
    
    # inputs
    yrtomap1 <- try(yrtomap1())
    
    # create map
    if(inherits(yrtomap1, 'try-error'))
      leaflet::leafletProxy("entmapyr") %>%
      leaflet::clearMarkers() |> 
      leaflet::clearShapes()
    
    if(!inherits(yrtomap1, 'try-error'))
      leaflet::leafletProxy("entmapyr") %>%
      leaflet::clearMarkers() |>
      leaflet::clearShapes() |> 
      leaflet::addMarkers(
        data = yrtomap1$tomapsta,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = ~yrtomap1$icons[as.numeric(cat)],
        label = ~lapply(as.list(lab), tbeptools::util_html)
      ) |> 
      leaflet::addPolygons(
        data = yrtomap1$tomapseg,
        fillColor = ~I(col),
        fillOpacity = 0.5,
        color = 'black',
        weight = 1,
        label = ~lapply(as.list(lab), tbeptools::util_html)
      )
    
  })
  
  # baywide ent data to map, yr mo
  enterotomap <- reactive({
    
    # inputs
    yrsel1 <- input$yrsel1
    mosel1 <- input$mosel1
    areasel1 <- input$areasel1
    
    req(mosel1)
    
    mosel1 <- mos[[mosel1]]
    areasel <- names(areas1[areas1 %in% areasel1])
    
    tomap <- tbeptools::anlz_enteromap(enterodata, yrsel1, mosel1, areasel, wetdry = TRUE, precipdata = catchprecip, 
                                       temporal_window = 2, wet_threshold = 0.5, assf = T)
    
    return(tomap)
    
  })
  
  observe({
    
    # inputs
    enterotomap <- try(enterotomap())
    
    # create map
    if(inherits(enterotomap, 'try-error'))
      leaflet::leafletProxy('entmap') |> 
      leaflet::clearMarkers() 

    if(!inherits(enterotomap, 'try-error'))
      leaflet::leafletProxy('entmap') |> 
      leaflet::clearMarkers() |>
      leaflet::addMarkers(
        data = enterotomap,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = ~ecocciicons[as.numeric(grp)],
        label = ~lapply(as.list(lab), tbeptools::util_html), 
        layerId = ~station
      )
    
  })
  
  # entmap popup modal
  observeEvent(input$entmap_marker_click, {
    
    showModal(modalDialog(
      plotly::plotlyOutput('entmappopup', height = "500px"),
      easyClose = T,
      fade = F,
      footer = NULL, 
      size = 'l'
    ))
    
  })
  
  # create plot on entmap click
  entmappopup <- eventReactive(input$entmap_marker_click, {
    
    yrsel1 <- input$yrsel1
    mosel1 <- input$mosel1
    station <- input$entmap_marker_click$id

    req(mosel1)
    mosel1 <- mos[[mosel1]]
    
    out <- entmappopup_plo(enterowetdry, station, yrsel1, mosel1)
    
    return(out)
    
  })
  
  # epc fib matrix
  fibmatrix <- reactive({
    
    # inputs
    areasel2 <- input$areasel2
    yrsel2 <- input$yrsel2
    
    stas <- fibdata %>%
      dplyr::filter(area %in% areasel2) %>%
      dplyr::select(epchc_station) %>%
      dplyr::distinct() %>%
      dplyr::pull()
    
    p <- try(tbeptools::show_fibmatrix(fibdata, stas = stas, yrrng = c(yrmin2, maxyr), 
                                       indic = 'fcolif', warn = F))
    
    validate(
      need(!inherits(p, 'try-error'), 'No fecal coliform data available for selection')
    )
    
    out <- mataddyr_fun(p, yrsel = yrsel2)
    
    return(out)
    
  })
  
  # data for epc fib map, yr
  yrtomap2 <- reactive({
    
    yrsel2 <- input$yrsel2
    areasel2 <- input$areasel2
    
    req(areasel2)
    
    out <- tbeptools::show_fibmatmap(fibdata, yrsel = yrsel2, areasel = areasel2, 
                                     indic = 'fcolif', listout = T, warn = F)
    
    return(out)
    
  }) 
  
  # epc fib data to map, yr
  observe({
    
    # inputs
    yrtomap2 <- try(yrtomap2())
    
    # create map
    if(inherits(yrtomap2, 'try-error'))
      leaflet::leafletProxy("fibmapyr") %>%
      leaflet::clearMarkers() |> 
      leaflet::clearShapes()
    
    if(!inherits(yrtomap2, 'try-error'))
      leaflet::leafletProxy("fibmapyr") %>%
      leaflet::clearMarkers() |>
      leaflet::clearShapes() |> 
      leaflet::addMarkers(
        data = yrtomap2$tomapsta,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = ~yrtomap2$icons[as.numeric(cat)],
        label = ~lapply(as.list(lab), tbeptools::util_html)
      )
    
  })
  
  # epc fib data to map, yr mo
  fibtomap <- reactive({
    
    # inputs
    yrsel2 <- input$yrsel2
    mosel2 <- input$mosel2
    areasel2 <- input$areasel2
    
    req(mosel2)
    
    mosel2 <- mos[[mosel2]]
    
    tomap <- tbeptools::anlz_fibmap(fibdata, yrsel2, mosel2, areasel2, assf = T)
    
    return(tomap)
    
  })
  
  # epc fib map
  observe({
    
    # inputs
    fibtomap <- try(fibtomap())
    
    # create map
    if(inherits(fibtomap, 'try-error'))
      leaflet::leafletProxy('fibmap') |> 
      leaflet::clearMarkers()
    
    if(!inherits(fibtomap, 'try-error'))
      leaflet::leafletProxy('fibmap') |> 
      leaflet::clearMarkers() |> 
      leaflet::addMarkers(
        data = fibtomap,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = ~fibicons[as.numeric(grp)],
        label = ~lapply(as.list(lab), tbeptools::util_html), 
        layerId = ~station
      )
    
  })
  
  # fibmap popup modal
  observeEvent(input$fibmap_marker_click, {

    showModal(modalDialog(
      plotly::plotlyOutput('fibmappopup', height = "500px"),
      easyClose = T,
      fade = F,
      footer = NULL, 
      size = 'l'
    ))
    
  })
  
  # create plot on fibmap click
  fibmappopup <- eventReactive(input$fibmap_marker_click, {
    
    yrsel2 <- input$yrsel2
    mosel2 <- input$mosel2
    station <- input$fibmap_marker_click$id

    req(mosel2)
    mosel2 <- mos[[mosel2]]

    out <- fibmappopup_plo(fibdata, station, yrsel2, mosel2)
    
    return(out)
    
  })
  
  # manco fib matrix
  mancofibmatrix <- reactive({
    
    # inputs
    areasel3 <- input$areasel3
    yrsel3 <- input$yrsel3
    
    stas <- mancofibdata %>%
      dplyr::filter(area %in% areasel3) %>%
      dplyr::select(manco_station) %>%
      dplyr::distinct() %>%
      dplyr::pull()
    
    p <- try(tbeptools::show_fibmatrix(mancofibdata, stas = stas, yrrng = c(yrmin3, maxyr), 
                                       indic = 'fcolif', warn = F))
    
    validate(
      need(!inherits(p, 'try-error'), 'No fecal coliform data available for selection')
    )
    
    out <- mataddyr_fun(p, yrsel = yrsel3)
    
    return(out)
    
  })
  
  # data for manco fib map, yr
  yrtomap3 <- reactive({
    
    yrsel3 <- input$yrsel3
    areasel3 <- input$areasel3
    
    req(areasel3)
    
    out <- tbeptools::show_fibmatmap(mancofibdata, yrsel = yrsel3, areasel = areasel3, 
                                     indic = 'fcolif', listout = T, warn = F)
    
    return(out)
    
  }) 
  
  # manco fib data to map, yr
  observe({
    
    # inputs
    yrtomap3 <- try(yrtomap3())
    
    # create map
    if(inherits(yrtomap3, 'try-error'))
      leaflet::leafletProxy("mancofibmapyr") %>%
      leaflet::clearMarkers() |> 
      leaflet::clearShapes()
    
    if(!inherits(yrtomap3, 'try-error'))
      if(nrow(yrtomap3$tomapsta) == 0)
        leaflet::leafletProxy("mancofibmapyr") %>%
      leaflet::clearMarkers() |> 
      leaflet::clearShapes()
    else
      leaflet::leafletProxy("mancofibmapyr") %>%
      leaflet::clearMarkers() |>
      leaflet::clearShapes() |> 
      leaflet::addMarkers(
        data = yrtomap3$tomapsta,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = ~yrtomap3$icons[as.numeric(cat)],
        label = ~lapply(as.list(lab), tbeptools::util_html)
      )
    
  })
  
  # manco fib data to map, yr mo
  mancofibtomap <- reactive({
    
    # inputs
    yrsel3 <- input$yrsel3
    mosel3 <- input$mosel3
    areasel3 <- input$areasel3
    
    req(mosel3)
    
    mosel3 <- mos[[mosel3]]
    
    tomap <- tbeptools::anlz_fibmap(mancofibdata, yrsel3, mosel3, areasel3, assf = T)
    
    return(tomap)
    
  })
  
  # manco fib map
  observe({
    
    # inputs
    mancofibtomap <- try(mancofibtomap())
    
    # create map
    if(inherits(mancofibtomap, 'try-error'))
      leaflet::leafletProxy('mancofibmap') |> 
      leaflet::clearMarkers()
    
    if(!inherits(mancofibtomap, 'try-error'))
      leaflet::leafletProxy('mancofibmap') |> 
      leaflet::clearMarkers() |> 
      leaflet::addMarkers(
        data = mancofibtomap,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = ~fibicons[as.numeric(grp)],
        label = ~lapply(as.list(lab), tbeptools::util_html), 
        layerId = ~station
      )
    
  })
  
  # manco fibmap popup modal
  observeEvent(input$mancofibmap_marker_click, {
    
    showModal(modalDialog(
      plotly::plotlyOutput('mancofibmappopup', height = "500px"),
      easyClose = T,
      fade = F,
      footer = NULL, 
      size = 'l'
    ))
    
  })
  
  # create plot on manco fibmap click
  mancofibmappopup <- eventReactive(input$mancofibmap_marker_click, {
    
    yrsel3 <- input$yrsel3
    mosel3 <- input$mosel3
    station <- input$mancofibmap_marker_click$id
    
    req(mosel3)
    mosel3 <- mos[[mosel3]]
    
    out <- fibmappopup_plo(mancofibdata, station, yrsel3, mosel3)
    
    return(out)
    
  })
  
  # download data
  dldat <- reactive({
    
    typseldl <- input$typseldl
    yrseldl <- input$yrseldl
    
    req(yrseldl)
    
    out <- dldatproc_fun(typseldl, yrseldl)
    
    return(out)
    
  })
  
  # reactable table
  dltab <- reactive({
    
    dldat <- dldat()
    
    out <- dldattab_fun(dldat)
    
    return(out)
    
  })

  # baywide ent matrix
  output$entmatrix <- plotly::renderPlotly(entmatrix())
  
  # baywide ent map, yr
  output$entmapyr <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmatmap(enterodata, yrsel = maxyr, 
                              areasel = c('OTB', 'HB', 'MTB', 'LTB', 'BCB', 'MR'),
                              precipdata = catchprecip, indic = 'entero', warn = F)
    
  })
  
  # baywide ent map, yr mo
  output$entmap <- leaflet::renderLeaflet({
    
    tbeptools::show_enteromap(enterodata, yrsel = maxyr, mosel = 7, areasel = c('Hillsborough Bay', 'Old Tampa Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Boca Ciega Bay', 'Manatee River'),
                              wetdry = T, precipdata = catchprecip, temporal_window = 2, wet_threshold = 0.5)
    
  })
  
  # baywide ent map popup
  output$entmappopup <- plotly::renderPlotly(entmappopup())
  
  # epc fib matrix
  output$fibmatrix <- plotly::renderPlotly(fibmatrix())
  
  # epc fib map, yr
  output$fibmapyr <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmatmap(fibdata, yrsel = maxyr, 
                              areasel = c('Alafia River', 'Hillsborough River'),
                              precipdata = catchprecip, indic = 'fcolif', warn = F)
    
  })
  
  # epc fib map, yr mo
  output$fibmap <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmap(fibdata, yrsel = maxyr, mosel = 7, 
                           areasel = c('Alafia River', 'Hillsborough River'))
    
  })
  
  # epc fib map popup
  output$fibmappopup <- plotly::renderPlotly(fibmappopup())
  
  # manco fib matrix
  output$mancofibmatrix <- plotly::renderPlotly(mancofibmatrix())
  
  # manco fib map, yr
  output$mancofibmapyr <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmatmap(mancofibdata, yrsel = maxyr - 2, 
                              areasel = c('Braden River', 'Manatee River'),
                              precipdata = catchprecip, indic = 'fcolif', warn = F)
    
  })
  
  # manco fib map, yr mo
  output$mancofibmap <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmap(mancofibdata, yrsel = maxyr - 2, mosel = 7, 
                           areasel = c('Braden River', 'Manatee River'))
    
  })
  
  # manco fib map popup
  output$mancofibmappopup <- plotly::renderPlotly(mancofibmappopup())
  
  # year slider range for download
  output$yrseldl <- renderUI({
    
    typseldl <- input$typseldl
    
    if(!grepl('Hillsborough|Manatee', typseldl))
      minyr <- yrmin1
    if(grepl('Hillsborough', typseldl))
      minyr <- yrmin2
    if(grepl('Manatee', typseldl))
      minyr <- yrmin3
    
    sliderInput('yrseldl', NULL, min = minyr, max = maxyr, value = c(minyr, maxyr), step = 1, sep = '', width = '95%')
    
  })

  # download fib table
  output$dltabout <- reactable::renderReactable(dltab())
  
  # download fib handler
  output$dwnld <- downloadHandler(
    filename = function(){'downloaddata.csv'},
    content = function(file){
      
      # inputs
      dldat <- dldat()
      
      write.csv(dldat, file, quote = T, row.names = F)
      
    }
  )
  
  
}

shinyApp(ui, server)
