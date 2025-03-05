library(shiny)
library(bslib)

# Source required files (commented out as we don't have access to them)
source(here::here('R/global.R'))
source(here::here('R/funcs.R'))

# Add Google Analytics
ga_tag <- tags$head(
  includeHTML("google-analytics.html")
)

ui <- page_navbar(
  title = "TAMPA BAY FIB DASHBOARD",
  id = "main-nav",
  # bg = '#00806E',
  
  # initialize shinyjs for JavaScript handling
  shinyjs::useShinyjs(),
  
  # Add Google Analytics and custom CSS
  header = tagList(
    ga_tag,
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    )
  ),
  
  # Add logo
  nav_item(
    tags$img(src = "tarponlogo.png", height = "30px", style = "margin-right: 10px;")
  ),
  
  # First nav item - Overview
  nav_panel(
    class = 'fill-height',
    title = "OVERVIEW",
    value = 'overview',
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
    value = 'baywide',
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
          shinyWidgets::materialSwitch('addsta1', 'Add station labels', value = T),
          leaflet::leafletOutput('entmapyr')
        ),
        nav_panel(
          "MAP BY YEAR AND MONTH",
          div(
            style = "display: flex; align-items: center; gap: 1rem;",
            shinyWidgets::sliderTextInput('mosel1', 'Select month:', choices = names(mos), selected = 'Jul', force_edges = T, grid = T, width = '50%'),
            shinyWidgets::materialSwitch('addsta2', 'Add station labels', value = T),
            span('Click on a station to view a complete time series')
          ),
          leaflet::leafletOutput('entmap')
          )
        )
      )
    )
  ),

  # Third nav item as menu for counties
  nav_menu(
    title = "2 BY COUNTY",
    value = 'by-county',
  
    # Hillsborough County
    nav_panel(
      title = "HILLSBOROUGH COUNTY",
      value = 'hillsborough-county', 
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
              shiny::includeMarkdown('www/countyusing.md')
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
              shinyWidgets::materialSwitch('addsta3', 'Add station labels', value = T),
              leaflet::leafletOutput('fibmapyr')
            ),
            nav_panel(
              "MAP BY YEAR AND MONTH",
              div(
                style = "display: flex; align-items: center; gap: 1rem;",
                shinyWidgets::sliderTextInput('mosel2', 'Select month:', choices = names(mos), selected = 'Jul', force_edges = T, grid = T, width = '50%'),
                shinyWidgets::materialSwitch('addsta4', 'Add station labels', value = T),
                span('Click on a station to view a complete time series')
              ),
              leaflet::leafletOutput('fibmap')
            )
          )
        )
      )
    ),
    
    # Manatee County
    nav_panel(
      title = "MANATEE COUNTY",
      value = 'manatee-county',
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
                sliderInput('yrsel3', NULL, min = yrmin3, max = maxyr, value = maxyr, step = 1, sep = '', width = '90%')
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
              shiny::includeMarkdown('www/countyusing.md')
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
              shinyWidgets::materialSwitch('addsta5', 'Add station labels', value = T),
              leaflet::leafletOutput('mancofibmapyr')
            ),
            nav_panel(
              "MAP BY YEAR AND MONTH",
              div(
                style = "display: flex; align-items: center; gap: 1rem;",
                shinyWidgets::sliderTextInput('mosel3', 'Select month:', choices = names(mos), selected = 'Jul', force_edges = T, grid = T, width = '50%'),
                shinyWidgets::materialSwitch('addsta6', 'Add station labels', value = T),
                span('Click on a station to view a complete time series')
              ),
              leaflet::leafletOutput('mancofibmap')
            )
          )
        )
      )
    ),
    
    # Pasco County
    nav_panel(
      title = "PASCO COUNTY",
      value = 'pasco-county',
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
                sliderInput('yrsel4', NULL, min = yrmin4, max = maxyr, value = maxyr, step = 1, sep = '', width = '90%')
              )
            ),
            div(
              style = 'width: 66.66%;',
              div(
                style = 'display: flex; flex-direction: column;',
                div(style = "height: 50px;", "Select area:"), 
                selectizeInput('areasel4', NULL, choices = areas4, selected = areas4, multiple = T, width = '100%', options = list(dropdownParent = 'body')),
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
              plotly::plotlyOutput('pascofibmatrix', height = '100%')
            ),
            nav_panel(
              "Using this tab",
              shiny::includeMarkdown('www/countyusing.md')
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
              shinyWidgets::materialSwitch('addsta6', 'Add station labels', value = T),
              leaflet::leafletOutput('pascofibmapyr')
            ),
            nav_panel(
              "MAP BY YEAR AND MONTH",
              div(
                style = "display: flex; align-items: center; gap: 1rem;",
                shinyWidgets::sliderTextInput('mosel4', 'Select month:', choices = names(mos), selected = 'Jul', force_edges = T, grid = T, width = '50%'),
                shinyWidgets::materialSwitch('addsta7', 'Add station labels', value = T),
                span('Click on a station to view a complete time series')
              ),
              leaflet::leafletOutput('pascofibmap')
            )
          )
        )
      )
    ),
    
    # Polk County
    nav_panel(
      title = "POLK COUNTY",
      value = 'polk-county',
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
                sliderInput('yrsel5', NULL, min = yrmin5, max = maxyr, value = maxyr, step = 1, sep = '', width = '90%')
              )
            ),
            div(
              style = 'width: 66.66%;',
              div(
                style = 'display: flex; flex-direction: column;',
                div(style = "height: 50px;", "Select area:"), 
                selectizeInput('areasel5', NULL, choices = areas5, selected = areas5, multiple = T, width = '100%', options = list(dropdownParent = 'body')),
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
              plotly::plotlyOutput('polcofibmatrix', height = '100%')
            ),
            nav_panel(
              "Using this tab",
              shiny::includeMarkdown('www/countyusing.md')
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
              shinyWidgets::materialSwitch('addsta7', 'Add station labels', value = T),
              leaflet::leafletOutput('polcofibmapyr')
            ),
            nav_panel(
              "MAP BY YEAR AND MONTH",
              div(
                style = "display: flex; align-items: center; gap: 1rem;",
                shinyWidgets::sliderTextInput('mosel5', 'Select month:', choices = names(mos), selected = 'Jul', force_edges = T, grid = T, width = '50%'),
                shinyWidgets::materialSwitch('addsta8', 'Add station labels', value = T),
                span('Click on a station to view a complete time series')
              ),
              leaflet::leafletOutput('polcofibmap')
            )
          )
        )
      )
    )
  
  ),
  
  # Third nav item - Data Downloads
  nav_panel(
    title = "3 DATA DOWNLOADS",
    value = 'data-downloads',
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
              selectInput('typseldl', NULL, choices = c('Baywide segment score categories', 'Baywide station score categories', 'Baywide raw data', 'Hillsborough County station score categories', 'Hillsborough County raw data', 'Manatee County station score categories', 'Manatee County raw data', 'Pasco County station score categories', 'Pasco County raw data', 'Polk County station score categories', 'Polk County raw data'), width = '95%')
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
                                         warn = F))

    }

    if(segsel1){

      p <- try(tbeptools::show_fibmatrix(enterodata, stas = NULL, bay_segment = areasel1,
                                         yrrng = c(yrmin1, maxyr), warn = F))

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

  # baywide map proxise  
  entmapyr_proxy <- leaflet::leafletProxy('entmapyr')
  entmap_proxy <- leaflet::leafletProxy('entmap')

  observeEvent(list(input$yrsel1, input$areasel1, input$addsta1, input$mosel1, input$addsta2), {
    
    ##
    # baywide ent data to map, yr
    
    yrtomap1 <- try(tbeptools::show_fibmatmap(enterodata, yrsel = input$yrsel1, areasel = input$areasel1, 
                                                         listout = T, warn = F))
    
    # create map
    if(inherits(yrtomap1, 'try-error'))
      entmapyr_proxy %>%
        leaflet::clearMarkers() |> 
        leaflet::clearShapes()
    
    if(!inherits(yrtomap1, 'try-error')){
      entmapyr_proxy  %>%
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
    
      if(input$addsta1)
        entmapyr_proxy |> 
          leaflet::addLabelOnlyMarkers(
            data = yrtomap1$tomapsta,
            lng = ~Longitude,
            lat = ~Latitude,
            label = ~grp,
            labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE)
          )
      
    }
    
    ##
    # baywide ent data to map, yr mo

    mosel1 <- mos[[input$mosel1]]
    areasel <- names(areas1[areas1 %in% input$areasel1])
    
    enterotomap <- try(tbeptools::anlz_enteromap(enterodata, input$yrsel1, mosel1, areasel, wetdry = TRUE, precipdata = catchprecip, 
                                                 temporal_window = 2, wet_threshold = 0.5, assf = T))
    
    # create map
    if(inherits(enterotomap, 'try-error'))
      entmap_proxy |> 
        leaflet::clearMarkers() 
    
    if(!inherits(enterotomap, 'try-error')){
      entmap_proxy |> 
        leaflet::clearMarkers() |>
        leaflet::addMarkers(
          data = enterotomap,
          lng = ~Longitude,
          lat = ~Latitude,
          icon = ~ecocciicons[as.numeric(grp)],
          label = ~lapply(as.list(lab), tbeptools::util_html), 
          layerId = ~station
        )
      
      if(input$addsta2)
        entmap_proxy |> 
          leaflet::addLabelOnlyMarkers(
            data = enterotomap,
            lng = ~Longitude,
            lat = ~Latitude,
            label = ~station,
            labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE)
          )
      
    }
      
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
                                       warn = F))
    
    validate(
      need(!inherits(p, 'try-error'), 'No FIB data available for selection')
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
                                     listout = T, warn = F)
    
    return(out)
    
  }) 
  
  # epc fib data to map, yr
  observe({
    
    # inputs
    yrtomap2 <- try(yrtomap2())
    
    # create map
    if(inherits(yrtomap2, 'try-error'))
      out <- leaflet::leafletProxy("fibmapyr") %>%
        leaflet::clearMarkers() |> 
        leaflet::clearShapes()
    
    if(!inherits(yrtomap2, 'try-error')){
      out <- leaflet::leafletProxy("fibmapyr") %>%
        leaflet::clearMarkers() |>
        leaflet::clearShapes() |> 
        leaflet::addMarkers(
          data = yrtomap2$tomapsta,
          lng = ~Longitude,
          lat = ~Latitude,
          icon = ~yrtomap2$icons[as.numeric(cat)],
          label = ~lapply(as.list(lab), tbeptools::util_html)
        )
      
      if(input$addsta3)
        out <- out |> 
          leaflet::addLabelOnlyMarkers(
            data = yrtomap2$tomapsta,
            lng = ~Longitude,
            lat = ~Latitude,
            label = ~grp,
            labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE)
          )
      
    }
    
    return(out)
    
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
      out <- leaflet::leafletProxy('fibmap') |> 
        leaflet::clearMarkers()
    
    if(!inherits(fibtomap, 'try-error')){
      out <- leaflet::leafletProxy('fibmap') |> 
        leaflet::clearMarkers() |> 
        leaflet::addMarkers(
          data = fibtomap,
          lng = ~Longitude,
          lat = ~Latitude,
          icon = ~fibicons[as.numeric(grp)],
          label = ~lapply(as.list(lab), tbeptools::util_html), 
          layerId = ~station
        )
     
      if(input$addsta4)
        out <- out |> 
          leaflet::addLabelOnlyMarkers(
            data = fibtomap,
            lng = ~Longitude,
            lat = ~Latitude,
            label = ~station,
            labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE)
          )
      
    }
    
    return(out)
      
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
                                       warn = F))
    
    validate(
      need(!inherits(p, 'try-error'), 'No FIB data available for selection')
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
                                     listout = T, warn = F)
    
    return(out)
    
  }) 
  
  # manco fib data to map, yr
  observe({
    
    # inputs
    yrtomap3 <- try(yrtomap3())
    
    # create map
    if(inherits(yrtomap3, 'try-error'))
      out <- leaflet::leafletProxy("mancofibmapyr") %>%
        leaflet::clearMarkers() |> 
        leaflet::clearShapes()
    
    if(!inherits(yrtomap3, 'try-error')){
      if(nrow(yrtomap3$tomapsta) == 0)
        out <- leaflet::leafletProxy("mancofibmapyr") %>%
          leaflet::clearMarkers() |> 
          leaflet::clearShapes()
      else
        out <- leaflet::leafletProxy("mancofibmapyr") %>%
          leaflet::clearMarkers() |>
          leaflet::clearShapes() |> 
          leaflet::addMarkers(
            data = yrtomap3$tomapsta,
            lng = ~Longitude,
            lat = ~Latitude,
            icon = ~yrtomap3$icons[as.numeric(cat)],
            label = ~lapply(as.list(lab), tbeptools::util_html)
          )
      
      if(input$addsta5)
        out <- out |> 
          leaflet::addLabelOnlyMarkers(
            data = yrtomap3$tomapsta,
            lng = ~Longitude,
            lat = ~Latitude,
            label = ~grp,
            labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE)
          )
      
    }
    
    return(out)
    
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
      out <- leaflet::leafletProxy('mancofibmap') |> 
        leaflet::clearMarkers()
    
    if(!inherits(mancofibtomap, 'try-error')){
      out <- leaflet::leafletProxy('mancofibmap') |> 
        leaflet::clearMarkers() |> 
        leaflet::addMarkers(
          data = mancofibtomap,
          lng = ~Longitude,
          lat = ~Latitude,
          icon = ~fibicons[as.numeric(grp)],
          label = ~lapply(as.list(lab), tbeptools::util_html), 
          layerId = ~station
        )
      
      if(input$addsta6)
        out <- out |> 
          leaflet::addLabelOnlyMarkers(
            data = mancofibtomap,
            lng = ~Longitude,
            lat = ~Latitude,
            label = ~station,
            labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE)
          )
      
    }
    
    return(out)
    
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
  
  # pasco fib matrix
  pascofibmatrix <- reactive({
    
    # inputs
    areasel4 <- input$areasel4
    yrsel4 <- input$yrsel4
    
    stas <- pascofibdata %>%
      dplyr::filter(area %in% areasel4) %>%
      dplyr::select(pasco_station) %>%
      dplyr::distinct() %>%
      dplyr::pull()
    
    p <- try(tbeptools::show_fibmatrix(pascofibdata, stas = stas, yrrng = c(yrmin4, maxyr), 
                                       warn = F))
    
    validate(
      need(!inherits(p, 'try-error'), 'No FIB data available for selection')
    )
    
    out <- mataddyr_fun(p, yrsel = yrsel4)
    
    return(out)
    
  })
  
  # data for pasco fib map, yr
  yrtomap4 <- reactive({
    
    yrsel4 <- input$yrsel4
    areasel4 <- input$areasel4
    
    req(areasel4)
    
    out <- tbeptools::show_fibmatmap(pascofibdata, yrsel = yrsel4, areasel = areasel4, 
                                     listout = T, warn = F)
    
    return(out)
    
  }) 
  
  # pasco fib data to map, yr
  observe({
    
    # inputs
    yrtomap4 <- try(yrtomap4())
    
    # create map
    if(inherits(yrtomap4, 'try-error'))
      out <- leaflet::leafletProxy("pascofibmapyr") %>%
        leaflet::clearMarkers() |> 
        leaflet::clearShapes()
    
    if(!inherits(yrtomap4, 'try-error')){
      if(nrow(yrtomap4$tomapsta) == 0)
        out <- leaflet::leafletProxy("pascofibmapyr") %>%
          leaflet::clearMarkers() |> 
          leaflet::clearShapes()
      else
        out <- leaflet::leafletProxy("pascofibmapyr") %>%
          leaflet::clearMarkers() |>
          leaflet::clearShapes() |> 
          leaflet::addMarkers(
            data = yrtomap4$tomapsta,
            lng = ~Longitude,
            lat = ~Latitude,
            icon = ~yrtomap4$icons[as.numeric(cat)],
            label = ~lapply(as.list(lab), tbeptools::util_html)
          )
      
      if(input$addsta6)
        out <- out |> 
          leaflet::addLabelOnlyMarkers(
            data = yrtomap4$tomapsta,
            lng = ~Longitude,
            lat = ~Latitude,
            label = ~grp,
            labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE)
          )
      
    }
    
    return(out)
    
  })
  
  # pasco fib data to map, yr mo
  pascofibtomap <- reactive({
    
    # inputs
    yrsel4 <- input$yrsel4
    mosel4 <- input$mosel4
    areasel4 <- input$areasel4
    
    req(mosel4)
    
    mosel4 <- mos[[mosel4]]
    
    tomap <- tbeptools::anlz_fibmap(pascofibdata, yrsel4, mosel4, areasel4, assf = T)

    return(tomap)
    
  })
  
  # pasco fib map
  observe({
    
    # inputs
    pascofibtomap <- try(pascofibtomap())

    # create map
    if(inherits(pascofibtomap, 'try-error'))
      out <- leaflet::leafletProxy('pascofibmap') |> 
        leaflet::clearMarkers()
    
    if(!inherits(pascofibtomap, 'try-error')){
      out <- leaflet::leafletProxy('pascofibmap') |> 
        leaflet::clearMarkers() |> 
        leaflet::addMarkers(
          data = pascofibtomap,
          lng = ~Longitude,
          lat = ~Latitude,
          icon = ~fibicons[as.numeric(grp)],
          label = ~lapply(as.list(lab), tbeptools::util_html), 
          layerId = ~station
        )
      
      if(input$addsta7)
        out <- out |> 
          leaflet::addLabelOnlyMarkers(
            data = pascofibtomap,
            lng = ~Longitude,
            lat = ~Latitude,
            label = ~station,
            labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE)
          )
      
    }
    
    return(out)
    
  })
  
  # pasco fibmap popup modal
  observeEvent(input$pascofibmap_marker_click, {
    
    showModal(modalDialog(
      plotly::plotlyOutput('pascofibmappopup', height = "500px"),
      easyClose = T,
      fade = F,
      footer = NULL, 
      size = 'l'
    ))
    
  })
  
  # create plot on pasco fibmap click
  pascofibmappopup <- eventReactive(input$pascofibmap_marker_click, {
    
    yrsel4 <- input$yrsel4
    mosel4 <- input$mosel4
    station <- input$pascofibmap_marker_click$id
    
    req(mosel4)
    mosel4 <- mos[[mosel4]]
    
    out <- fibmappopup_plo(pascofibdata, station, yrsel4, mosel4)
    
    return(out)
    
  })

  # polco fib matrix
  polcofibmatrix <- reactive({
    
    # inputs
    areasel5 <- input$areasel5
    yrsel5 <- input$yrsel5
    
    stas <- polcofibdata %>%
      dplyr::filter(area %in% areasel5) %>%
      dplyr::select(polco_station) %>%
      dplyr::distinct() %>%
      dplyr::pull()
    
    p <- try(tbeptools::show_fibmatrix(polcofibdata, stas = stas, yrrng = c(yrmin5, maxyr), 
                                       warn = F))
    
    validate(
      need(!inherits(p, 'try-error'), 'No FIB data available for selection')
    )
    
    out <- mataddyr_fun(p, yrsel = yrsel5)
    
    return(out)
    
  })
  
  # data for polco fib map, yr
  yrtomap5 <- reactive({
    
    yrsel5 <- input$yrsel5
    areasel5 <- input$areasel5
    
    req(areasel5)
    
    out <- tbeptools::show_fibmatmap(polcofibdata, yrsel = yrsel5, areasel = areasel5, 
                                     listout = T, warn = F)
    
    return(out)
    
  }) 
  
  # polco fib data to map, yr
  observe({
    
    # inputs
    yrtomap5 <- try(yrtomap5())
    
    # create map
    if(inherits(yrtomap5, 'try-error'))
      out <- leaflet::leafletProxy("polcofibmapyr") %>%
        leaflet::clearMarkers() |> 
        leaflet::clearShapes()
    
    if(!inherits(yrtomap5, 'try-error')){
      if(nrow(yrtomap5$tomapsta) == 0)
        out <- leaflet::leafletProxy("polcofibmapyr") %>%
          leaflet::clearMarkers() |> 
          leaflet::clearShapes()
      else
        out <- leaflet::leafletProxy("polcofibmapyr") %>%
          leaflet::clearMarkers() |>
          leaflet::clearShapes() |> 
          leaflet::addMarkers(
            data = yrtomap5$tomapsta,
            lng = ~Longitude,
            lat = ~Latitude,
            icon = ~yrtomap5$icons[as.numeric(cat)],
            label = ~lapply(as.list(lab), tbeptools::util_html)
          )
      
      if(input$addsta7)
        out <- out |> 
          leaflet::addLabelOnlyMarkers(
            data = yrtomap5$tomapsta,
            lng = ~Longitude,
            lat = ~Latitude,
            label = ~grp,
            labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE)
          )
      
    }
    
    return(out)
    
  })
  
  # polco fib data to map, yr mo
  polcofibtomap <- reactive({
    
    # inputs
    yrsel5 <- input$yrsel5
    mosel5 <- input$mosel5
    areasel5 <- input$areasel5
    
    req(mosel5)
    
    mosel5 <- mos[[mosel5]]
    
    tomap <- tbeptools::anlz_fibmap(polcofibdata, yrsel5, mosel5, areasel5, assf = T)
    
    return(tomap)
    
  })
  
  # polco fib map
  observe({
    
    # inputs
    polcofibtomap <- try(polcofibtomap())
    
    # create map
    if(inherits(polcofibtomap, 'try-error'))
      out <- leaflet::leafletProxy('polcofibmap') |> 
        leaflet::clearMarkers()
    
    if(!inherits(polcofibtomap, 'try-error')){
      out <- leaflet::leafletProxy('polcofibmap') |> 
        leaflet::clearMarkers() |> 
        leaflet::addMarkers(
          data = polcofibtomap,
          lng = ~Longitude,
          lat = ~Latitude,
          icon = ~fibicons[as.numeric(grp)],
          label = ~lapply(as.list(lab), tbeptools::util_html), 
          layerId = ~station
        )
      
      if(input$addsta8)
        out <- out |> 
          leaflet::addLabelOnlyMarkers(
            data = polcofibtomap,
            lng = ~Longitude,
            lat = ~Latitude,
            label = ~station,
            labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE)
          )
      
    }
    
    return(out)
    
  })
  
  # polco fibmap popup modal
  observeEvent(input$polcofibmap_marker_click, {
    
    showModal(modalDialog(
      plotly::plotlyOutput('polcofibmappopup', height = "500px"),
      easyClose = T,
      fade = F,
      footer = NULL, 
      size = 'l'
    ))
    
  })
  
  # create plot on polco fibmap click
  polcofibmappopup <- eventReactive(input$polcofibmap_marker_click, {
    
    yrsel5 <- input$yrsel5
    mosel5 <- input$mosel5
    station <- input$polcofibmap_marker_click$id
    
    req(mosel5)
    mosel5 <- mos[[mosel5]]
    
    out <- fibmappopup_plo(polcofibdata, station, yrsel5, mosel5)
    
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

  # JavaScript to observe hashchange events
  shinyjs::runjs("
      $(document).on('click', 'a', function() {
        var href = $(this).attr('href');
        if (href.startsWith('#')) {
          Shiny.setInputValue('navigate', href.substring(1));
        }
      });
  ")
  
  # observe hash change and navigate to the corresponding panel
  observeEvent(input$navigate, {
    updateNavbarPage(session, "main-nav", selected = input$navigate)
  })

  # baywide ent matrix
  output$entmatrix <- plotly::renderPlotly(entmatrix())
  
  # baywide ent map, yr
  output$entmapyr <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmatmap(enterodata, yrsel = maxyr, 
                              areasel = c('OTB', 'HB', 'MTB', 'LTB', 'BCB', 'MR'),
                              precipdata = catchprecip, warn = F, addsta = T)
    
  })
  
  # baywide ent map, yr mo
  output$entmap <- leaflet::renderLeaflet({
    
    tbeptools::show_enteromap(enterodata, yrsel = maxyr, mosel = 7, areasel = c('Hillsborough Bay', 'Old Tampa Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Boca Ciega Bay', 'Manatee River'),
                              wetdry = T, precipdata = catchprecip, temporal_window = 2, wet_threshold = 0.5, addsta = T)
    
  })
  
  # baywide ent map popup
  output$entmappopup <- plotly::renderPlotly(entmappopup())
  
  # epc fib matrix
  output$fibmatrix <- plotly::renderPlotly(fibmatrix())
  
  # epc fib map, yr
  output$fibmapyr <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmatmap(fibdata, yrsel = maxyr, 
                              areasel = c('Alafia River', 'Hillsborough River'),
                              precipdata = catchprecip, warn = F, addsta = T)
    
  })
  
  # epc fib map, yr mo
  output$fibmap <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmap(fibdata, yrsel = maxyr, mosel = 7, 
                           areasel = c('Alafia River', 'Hillsborough River'), addsta = T)
    
  })
  
  # epc fib map popup
  output$fibmappopup <- plotly::renderPlotly(fibmappopup())
  
  # manco fib matrix
  output$mancofibmatrix <- plotly::renderPlotly(mancofibmatrix())
  
  # manco fib map, yr
  output$mancofibmapyr <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmatmap(mancofibdata, yrsel = maxyr, 
                              areasel = c('Braden River', 'Manatee River'),
                              precipdata = catchprecip, warn = F, addsta = T)
    
  })
  
  # manco fib map, yr mo
  output$mancofibmap <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmap(mancofibdata, yrsel = maxyr, mosel = 7, 
                           areasel = c('Braden River', 'Manatee River'), addsta = T)
    
  })
  
  # manco fib map popup
  output$mancofibmappopup <- plotly::renderPlotly(mancofibmappopup())
  
  # pasco fib matrix
  output$pascofibmatrix <- plotly::renderPlotly(pascofibmatrix())
  
  # pasco fib map, yr
  output$pascofibmapyr <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmatmap(pascofibdata, yrsel = maxyr, 
                              areasel = areas4,
                              precipdata = catchprecip, warn = F, addsta = T)
    
  })
  
  # pasco fib map, yr mo
  output$pascofibmap <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmap(pascofibdata, yrsel = maxyr, mosel = 7, 
                           areasel = areas4, addsta = T)
    
  })
  
  # pasco fib map popup
  output$pascofibmappopup <- plotly::renderPlotly(pascofibmappopup())
  
  # polco fib matrix
  output$polcofibmatrix <- plotly::renderPlotly(polcofibmatrix())
  
  # polco fib map, yr
  output$polcofibmapyr <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmatmap(polcofibdata, yrsel = maxyr, 
                              areasel = areas5,
                              precipdata = catchprecip, warn = F, addsta = T)
    
  })
  
  # polco fib map, yr mo
  output$polcofibmap <- leaflet::renderLeaflet({
    
    tbeptools::show_fibmap(polcofibdata, yrsel = maxyr, mosel = 7, 
                           areasel = areas5, addsta = T)
    
  })
  
  # polco fib map popup
  output$polcofibmappopup <- plotly::renderPlotly(polcofibmappopup())
  
  # year slider range for download
  output$yrseldl <- renderUI({
    
    typseldl <- input$typseldl
    
    if(!grepl('Hillsborough|Manatee|Pasco|Polk', typseldl))
      minyr <- yrmin1
    if(grepl('Hillsborough', typseldl))
      minyr <- yrmin2
    if(grepl('Manatee', typseldl))
      minyr <- yrmin3
    if(grepl('Pasco', typseldl))
      minyr <- yrmin4
    if(grepl('Polk', typseldl))
      minyr <- yrmin5
    
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
