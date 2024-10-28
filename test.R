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

    ")
  ),
  
  # Rest of the UI code remains exactly the same...
  nav_item(
    tags$img(src = "tarponlogo.png", height = "30px", style = "margin-right: 10px;")
  ),
  
  nav_panel(
    class = 'fill-height',
    title = "OVERVIEW",
    div(
      class = 'resizable-row',
      div(
        class = 'resizable-column',
        style = 'width: 66.66%',
        card(
          card_header("USING THE DASHBOARD"),
          full_screen = TRUE,
          height = '100%',
          div(
            class = 'card-scroll',
            markdown(
              "text"
            )
          ))
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
              sliderInput('yrsel1', NULL, min = 1975, max = 2023, value = 2023, step = 1, sep = '', width = '90%')
            )
          ),
          div(
            style = 'width: 66.66%;',
            div(
              style = 'display: flex; flex-direction: column;',
              div(style = "height: 50px;", "Select area:"), 
              selectInput('areasel1', NULL, choices = c('HB', 'OTB'), selected = c('HB', 'OTB'), multiple = T, width = '100%'),
            )
          )
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
              plotlyOutput("examplePlot1", height = "100%")
            ),
          nav_panel(
            "Tab 2",
              plotlyOutput("examplePlot2", height = "100%")
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
  output$examplePlot1 <- renderPlotly({
    set.seed(123)
    x <- rnorm(100)
    y <- x + rnorm(100, sd = 0.5)
    
    plot_ly(x = x, y = y, type = "scatter", mode = "markers")
  })
  
  output$examplePlot2 <- renderPlotly({
    set.seed(456)
    x <- rnorm(100)
    y <- x * 2 + rnorm(100, sd = 0.5)
    
    plot_ly(x = x, y = y, type = "scatter", mode = "markers")

  })
}

shinyApp(ui, server)
