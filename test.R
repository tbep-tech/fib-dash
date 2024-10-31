library(shiny)
library(bslib)
library(markdown)
library(plotly)

ui <- page_navbar(
  title = "TAMPA BAY FIB DASHBOARD",
  
  header = tagList(
    tags$style("
      .fill-height {
        display: flex;
        flex-direction: column;
        height: calc(100vh - 60px);
        overflow: hidden;
        padding: 1rem;
      }
      .resizable-row {
        display: flex;
        gap: 1rem;
        flex: 1;
        min-height: 0;
        padding: 0;
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
      }
      /* New styles for input container */
      .input-container {
        margin-bottom: 1rem;
      }
      .input-card {
        height: auto !important;
      }
    ")
  ),
  
  nav_item(
    tags$img(src = "tarponlogo.png", height = "30px", style = "margin-right: 10px;")
  ),
  
  nav_panel(
    title = "1 BAYWIDE",
    class = 'fill-height',
    
    # Replace layout_columns with layout_column_wrap
    div(class = "input-container",
      card(
        class = "input-card",
        layout_column_wrap(
          width = 1/2,
          heights_equal = "row",
          style = "gap: 0.5rem;",
          sliderInput('yrsel1', NULL, min = 1975, max = 2023, value = 2023, step = 1, sep = '', width = '90%'),
          selectizeInput('areasel1', NULL, choices = c('HB', 'OTB', 'LTB', 'MTB', 'HR', 'MCB', 'LA', 'WB'), 
                     selected = c('HB', 'OTB'), multiple = T, width = '100%', options = list(dropdownParent = 'body'))
        )
      )
    ),
    
    div(
      class = 'resizable-row',
      div(
        class = 'resizable-column',
        style = 'width: 66.66%',
        navset_card_underline(
          full_screen = TRUE,
          nav_panel(
            "Tab 1",
              'text'
            ),
          nav_panel(
            "Tab 2",
            'text'
            )
          )
        ),
      div(
        class='right-column', 
        style = 'width: 33.33%',
        card(
          height = '100%',
          full_screen = TRUE,
          card_header("METHODS"),
          div(
            class = 'card-scroll',
            markdown(
              "text"
            )
          )
        )
      )
    )
  ),
  
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
  NULL
}

shinyApp(ui, server)