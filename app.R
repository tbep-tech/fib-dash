library(shiny)
library(bslib)
library(leaflet)
library(here)
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
        markdown(
"          
<div class = 'row'>
<div class = 'col-md-2'></div>
<div class = 'col-md-8'>

<h2 style = 'text-align: center;'>WELCOME TO THE TAMPA BAY FIB DASHBOARD!</h2>
            
<img src='bannerimage.png' style='width: 80%; display: block; margin: 0 auto;'>  
<br>
            
This dashboard summarizes fecal indicator bacteria (FIB) data for a baywide assessment and for data collected as part of the Environmental Protection Commission of Hillsborough County (EPCHC) monitoring program.  The latter is specific to evaluating fecal impairments in the Hillsborough and Alafia river basins.  The assessments are meant to inform progress remediating fecal impairments or to support prioritization of areas for further investigation. They are not meant to support beach monitoring efforts or closures for recreational uses - alternative reporting products are available for that purpose (see <a href='https://www.floridahealth.gov/environmental-health/beach-water-quality/county-detail.html?County=Pinellas&Zip=33701-3109' target = '_blank'>FLDOH Healthy Beaches</a> and <a href='https://pinellas.wateratlas.usf.edu/maps/coastal-water-quality-map/' target='_blank'>Pinellas County Recreational Water Quality Map</a>). The dashboard is organized in the following sections:
            
1) [__BAYWIDE__](#baywide): View baywide summaries of *Enterococcus* data at select monitoring locations for each bay segment
1) [__HILLSBOROUGH COUNTY__](#hillsborough-county): View summaries of Environmental Protection Commission of Hillsborough County (EPCHC) FIB data for the Hillsborough and Alafia River basins
1) [__MANATEE COUNTY__](#manatee-county): View summaries of Manatee County FIB data for select monitoring locations
1) [__DATA DOWNLOADS__](#data-downloads): Download baywide, Hillsborough County, or Manatee County data
                    
The plots in this dashboard are interactive and display options can be controlled using a mouse. Most plots include a <a href='https://help.plot.ly/zoom-pan-hover-controls/' target='_blank'>control menu</a> on the top with different options for viewing the data.  For example, click the camera icon to download a plot.
                    
<img src='plotcontrols.PNG' style='width: 30%; display: block; margin: 0 auto;'>   
<br>

<h2 style = 'text-align: center;'>DATA SOURCES</h2>
                      
Source data used on this website were obtained from multiple sources for the baywide assessment and from the Environmental Protection Commission of Hillsborough County. Data can be obtained using functions from the tbeptools R package described below. Graphics and tables provided on the dashboard are made available for exploratory purposes only.  
                    
<h2 style = 'text-align: center;'>WEBSITE INFO</h2>
                      
<a href='https://tbep-tech.github.io/tbeptools/articles/fib.html' target='_blank' rel='noopener noreferrer'><img class='wp-image-14010 alignnone ' src='tbeptoolshex.png' alt='' width='200' height='200' /></a>
                      
The page source content can be viewed on <a href='https://github.com/tbep-tech/fib-dash' target='_blank'>Github</a>. Nearly all of the data, tables, and plots were created using functions in the <a href='https://tbep-tech.github.io/tbeptools' target='_blank'>tbeptools</a> R software package.  Please see the <a href='https://tbep-tech.github.io/tbeptools/articles/fib.html' target='_blank'>vignette</a> for a detailed overview of how you can use these functions on your own to work with the data. 
                    
Questions and comments about the dashboard can be sent to [Marcus Beck](mailto:mbeck@tbep.org). Like this app? Share it on social media using the <a href='https://twitter.com/hashtag/TampaBayOpenSci?src=hashtag_click' target='_blank'>#TampaBayOpenSci</a> hashtag.  
                      
Citation info here: <a href='https://doi.org/10.5281/zenodo.13881473' target='_blank'><img src='https://zenodo.org/badge/841089887.svg' alt='DOI'></a>
                      
<a rel='license' href='http://creativecommons.org/licenses/by/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by/4.0/88x31.png' /></a>&nbsp;&nbsp;This website is licensed under a <a rel='license' href='http://creativecommons.org/licenses/by/4.0/' target='_blank'>Creative Commons Attribution 4.0 International License</a>.

</div>
<div class = 'col-md-2'></div>
</div>
"
        )   
      ),
    nav_panel(
      title = 'METHODS', 
      markdown(
"
<div class = 'row'>
<div class = 'col-md-2'></div>
<div class = 'col-md-8'>

#### How to understand and use the dashboard

Fecal Indicator Bacteria (FIB) provide information on the potential exposure risk from contact recreation or fish/shellfish consumption from surface waters with high fecal loads.  These indicators are imperfect and provide only a general overview of the potential risk.  Presence of FIBs does not necessarily indicate the presence of fecal pathogens, but their presence can be used to prioritize areas for further investigation or remediation. 

The FIBs used in this dashboard are *Enterococcus* cell concentrations for the baywide assessment and a mix of pathogens for the EPCHC data. For the latter, these include fecal coliform for the report card and fecal coliform, *Enterococcus*, and *E. Coli* for the map summaries depending on which map is shown.

#### Report card score categories

The report cards on each page use similar methods for defining overall risk categories for the FIBs.  The scores are applied to individual monitoring stations or as a baywide total and describe the likelihood that the samples in a given year exceed a relevant cell concentration threshold.  The thresholds are 400 CFU / 100 mL of *Enterococcus* for the baywide assessments and 130 CFU / 100 mL of fecal coliform for the EPCHC and Manatee County assessments. The score categories are as follows: 

* <span style='color:#2DC938'>__A__</span>: < 10% likelihood that the samples exceed the threshold
* <span style='color:#E9C318'>__B__</span>: 10-30% likelihood that the samples exceed the threshold
* <span style='color:#EE7600'>__C__</span>: 30-50% likelihood that the samples exceed the threshold
* <span style='color:#CC3231'>__D__</span>: 50-75% likelihood that the samples exceed the threshold
* <span style='color:#800080'>__E__</span>: > 75% likelihood that the samples exceed the threshold

For the baywide assessments, the score categories are assigned to both individual stations and for each bay segment, whereas scores are only assigned to stations for the EPCHC data.  The stations for the baywide assessments were chosen specifically as downstream endpoints that drain directly into each bay segment, whereas the EPCHC and Manatee County stations are more distributed throughout the watershed.  The bay segment score categories are based on the aggregate of all samples from each station that drain into that segment.

#### Map summaries

The maps on each tab show year or year/month summaries for FIBs.  The annual maps show the same score categories for the report cards described above for each station and bay segment for the baywide assessments and for stations only for the EPCHC and Manatee County data. 

The year/month maps show the sample results for each station for a more detailed view of the data in each year.  The year/month summaries show the concentrations at each station using different threshold categories than the scores described above and separate the data differently depending on the dataset. 

The baywide dataset year/month maps show *Enterococcus* concentrations separately as wet or dry samples based on whether a sample was collected after a significant amount of rain occurred prior to sampling or if dry conditions were observed.  The thresholds below are then applied to both wet/dry samples:
  
* <span style='color:#2DC938'>__Green__</span>: Cell concentrations < 35 CFU / 100 mL
* <span style='color:#E9C318'>__Yellow__</span>: Cell concentrations 35 - 129 CFU / 100 mL
* <span style='color:#EE7600'>__Orange__</span>: Cell concentrations 130 - 999 CFU / 100 mL
* <span style='color:#CC3231'>__Red__</span>: Cell concentrations > 999 CFU / 100 mL

Similar thresholds are applied to station samples for the year/month maps for the EPCHC and Manatee County data.  However, samples are not distinguished as wet/dry and the FIB varies depending on the location of the sample.  *Enterococcus* is used for tidally-influenced locations and *E. coli* is used for freshwater locations.

Tidally-influenced *Enterococcus* thresholds for EPCHC/Manatee County year/month maps: 
  
* <span style='color:#2DC938'>__Green__</span>: Cell concentrations < 35 CFU / 100 mL
* <span style='color:#E9C318'>__Yellow__</span>: Cell concentrations 35 - 129 CFU / 100 mL
* <span style='color:#EE7600'>__Orange__</span>: Cell concentrations 130 - 999 CFU / 100 mL
* <span style='color:#CC3231'>__Red__</span>: Cell concentrations > 999 CFU / 100 mL

Freshwater *E. coli* thresholds for EPCHC/Manatee County year/month maps: 
  
* <span style='color:#2DC938'>__Green__</span>: Cell concentrations < 126 CFU / 100 mL
* <span style='color:#E9C318'>__Yellow__</span>: Cell concentrations 126 - 409 CFU / 100 mL
* <span style='color:#EE7600'>__Orange__</span>: Cell concentrations 410 - 999 CFU / 100 mL
* <span style='color:#CC3231'>__Red__</span>: Cell concentrations > 999 CFU / 100 mL

</div>
<div class = 'col-md-2'></div>
</div>
"
        )
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
            markdown(
"
The report card and maps are based on *Enterococcus* cell concentrations with results for bay segments and individual sampling stations. Please view the __METHODS__ tab on the [__OVERVIEW__](#overview) page for a description of the score categories in the report card and the map summaries. 

The displayed data for all of the graphics on this page can be selected using the year slider and area selections at the top.  The year/month map tab includes an additional month slider.  By default, all areas are shown.  To remove an area, select it from the menu and hit the backspace key.

The __REPORT CARD__ tab shows the overall score category each year for sampled locations in the bay.  The row for the selected year from the slider at the top of the page is outlined in black. By default, the bay segment scores are shown. The bay segment abbreviations are as follows: 

* __OTB__: Old Tampa Bay
* __HB__: Hillsborough Bay
* __MTB__: Middle Tampa Bay
* __BCB__: Boca Ciega Bay
* __LTB__: Lower Tampa Bay
* __MR__: Manatee River

Stations that apply to each bay segment can be shown instead by toggling the switch above the report card.

The report card is also interactive and can be zoomed by clicking the mouse and dragging to an area of interest.  Reset the plot by double-clicking.  An image of the report card can be saved by clicking the camera icon that is shown above the report card when the cursor is over the plot. 

The __MAP BY YEAR__ tab shows a map of the report card grades assigned each bay segment and the individual station grades for the selected year. These are the same values shown in the report card.

Placing the cursor over a bay segment or station will reveal additional information. 

<img src='baywideyrmapex.png' style='width: 250px; display: block; margin: 0 auto;'>   

The __MAP BY YEAR AND MONTH__ tab shows a map of *Enterococcus* concentration categories for the selected year and month. Samples are identified as occurring during wet (circles) or dry (triangles) conditions. See the __METHODS__ tab on the [__OVERVIEW__](#overview) page for a description of the concentration thresholds.

Placing the cursor over a bay segment or station will reveal additional information. 

<img src='baywideyrmomapex.png' style='width: 250px; display: block; margin: 0 auto;'>   

Finally, clicking on a station on the year/month map will create an interactive popup plot for the complete time series of *Enterococcus* concentrations at that location.  The popup plot can be closed by clicking outside the plot area.

<img src='baywidemodalex.png' style='width: 550px; display: block; margin: 0 auto;'> 
"              
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
            markdown(
            "
The report card and maps are based on fecal coliform, *Enterococcus*, and *E. Coli* cell concentrations using EPCHC data with results for individual sampling stations. Please view the __METHODS__ tab on the [__OVERVIEW__](#overview) page for a description of the score categories in the report card and the map summaries. 

The displayed data for all of the graphics on this page can be selected using the year slider and area selections at the top.  The year/month map tab includes an additional month slider. By default, only sampling stations in the Hillsborough and Alafia River tributary watersheds are shown. These were chosen based on state-designated bacterial impairments and efforts under the Basin Management Action Plans (BMAPs). Add more sampling areas from the dropdown menu. To remove an area, select it from the menu and hit the backspace key.

The __REPORT CARD__ tab shows the overall score category each year for fecal coliform concentrations at stations in the EPCHC dataset.  The row for the selected year from the slider at the top of the page is outlined in black. The stations apply to the areas chosen from the drop down at the top of the page. Bay segment summaries cannot be shown for the EPCHC because the stations represent broader sampling areas in the watershed.

The report card is also interactive and can be zoomed by clicking the mouse and dragging to an area of interest.  Reset the plot by double-clicking.  An image of the report card can be saved by clicking the camera icon that is shown above the report card when the cursor is over the plot. 

The __MAP BY YEAR__ tab shows a map of the report card grades assigned to each individual station for the selected year. These are the same values shown in the report card using fecal coliform concentrations.

Placing the cursor over a station will reveal additional information. 

<img src='epchcyrmapex.png' style='width: 250px; display: block; margin: 0 auto;'>  

The __MAP BY YEAR AND MONTH__ tab shows a map of *Enterococcus* and *E. Coli* concentration categories for the selected year and month. Tidally-influenced sampling stations use *Enterococcus* (circles) and freshwater sampling stations use *E. coli* (triangles). See the __METHODS__ tab on the [__OVERVIEW__](#overview) page for a description of the concentration thresholds.

Placing the cursor over a station will reveal additional information. 

<img src='epchcyrmomapex.png' style='width: 250px; display: block; margin: 0 auto;'>   

Finally, clicking on a station on the year/month map will create an interactive popup plot for the complete time series of *Enterococcus* or *E. Coli* concentrations at that location. The popup plot can be closed by clicking outside the plot area.

<img src='epchcmodalex.png' style='width: 550px; display: block; margin: 0 auto;'> 
"              
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
            markdown(
            "
The report card and maps are based on fecal coliform, *Enterococcus*, and *E. Coli* cell concentrations using Manatee County data with results for individual sampling stations. Please view the __METHODS__ tab on the [__OVERVIEW__](#overview) page for a description of the score categories in the report card and the map summaries. 

The displayed data for all of the graphics on this page can be selected using the year slider and area selections at the top.  The year/month map tab includes an additional month slider. By default, only sampling stations in the Braden and Manatee Rivers are shown. Add more sampling areas from the dropdown menu. To remove an area, select it from the menu and hit the backspace key.

The __REPORT CARD__ tab shows the overall score category each year for fecal coliform concentrations at stations in the Manatee County dataset.  The row for the selected year from the slider at the top of the page is outlined in black. The stations apply to the areas chosen from the drop down at the top of the page. Bay segment summaries cannot be shown for the Manatee County data because the stations represent broader sampling areas not appriopriate for evaluating bay segments.

The report card is also interactive and can be zoomed by clicking the mouse and dragging to an area of interest.  Reset the plot by double-clicking.  An image of the report card can be saved by clicking the camera icon that is shown above the report card when the cursor is over the plot. 

The __MAP BY YEAR__ tab shows a map of the report card grades assigned to each individual station for the selected year. These are the same values shown in the report card using fecal coliform concentrations.

Placing the cursor over a station will reveal additional information. 

<img src='mancoyrmapex.png' style='width: 250px; display: block; margin: 0 auto;'>  

The __MAP BY YEAR AND MONTH__ tab shows a map of *Enterococcus* and *E. Coli* concentration categories for the selected year and month. Tidally-influenced sampling stations use *Enterococcus* (circles) and freshwater sampling stations use *E. coli* (triangles). See the __METHODS__ tab on the [__OVERVIEW__](#overview) page for a description of the concentration thresholds.

Placing the cursor over a station will reveal additional information. 

<img src='mancoyrmomapex.png' style='width: 250px; display: block; margin: 0 auto;'>   

Finally, clicking on a station on the year/month map will create an interactive popup plot for the complete time series of *Enterococcus* or *E. Coli* concentrations at that location. The popup plot can be closed by clicking outside the plot area.

<img src='mancomodalex.png' style='width: 550px; display: block; margin: 0 auto;'> 
"              
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
    card(
      card_header("DATA"),
      navset_tab(
        nav_panel(
          "DOWNLOAD",
          "Download interface here"
        ),
        nav_panel(
          "METADATA",
          "Metadata information here"
        )
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
    
    sliderInput('yrseldl', 'Select year range:', min = minyr, max = maxyr, value = c(minyr, maxyr), step = 1, sep = '', width = '200%')
    
  })

  # download fib table
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
