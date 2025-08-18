library(shiny)
library(shinyMobile)
library(azmetr)
library(dplyr)
library(ggplot2)
library(thematic)
thematic_shiny(font = "auto")
# library(highcharter)

#global options
options <- f7DefaultOptions()
options$color <- "#0C234B" #arizona blue
options$filled <- TRUE #fill navbar and toolbar with options$color
options$dark <- FALSE
options$theme <- "md" #"ios" to see iOS them, "auto" or "md" to see Android theme
# options$pullToRefresh <- TRUE # intersting, maybe could be used to refresh data?

# pre-load station data
stations <- station_info$meta_station_id
names(stations) <- station_info$meta_station_name
ui <- f7Page(
  f7SingleLayout(
    navbar = f7Navbar(
      title = div("AZMet"),
      leftPanel = TRUE,
      hairline = FALSE
    ),
    toolbar = f7Toolbar(
      f7Button(
        inputId = "btn_refresh",
        label = "Refresh Data",
        icon = f7Icon("arrow_2_squarepath")
      ),
      position = "bottom",
      icons = TRUE
    ),
    panels = f7Panel(
      title = "Menu",
      f7Block(f7Link(
        " About",
        icon = f7Icon("question_circle"),
        href = "https://azmet.arizona.edu"
      )),
      f7Block(f7Link(
        " Source code",
        icon = f7Icon("logo_github"),
        href = "https://github.com/Aariq/azmet-mobile"
      )),
      side = "left",
      effect = "cover"
    ),
    f7List(
      f7SmartSelect(
        inputId = "station",
        label = "Select a Station",
        choices = stations
      ),
      strong = TRUE
    ),
    f7Accordion(
      f7AccordionItem(
        title = "Temperature",
        f7Card(
          plotOutput("temp")
        ),
        open = TRUE
      ),
      f7AccordionItem(
        title = "Precipitation",
        f7Card(
          plotOutput("precip")
        )
      )
    )
  ),
  options = options,
  title = "AZMet",
  allowPWA = TRUE
)

server <- function(input, output, session) {
  data <- reactive({
    azmetr::az_15min(
      start_date_time = lubridate::now(tzone = "America/Phoenix") -
        lubridate::hours(24)
    )
  }) |>
    shiny::bindEvent(input$btn_refresh, ignoreNULL = FALSE)

  data_station <- reactive({
    req(input$station)
    data() |>
      dplyr::filter(meta_station_id %in% input$station)
  })

  output$temp <- renderPlot({
    data_station() |>
      ggplot(aes(x = datetime, y = temp_airC)) +
      geom_line() +
      labs(title = "Air Temperature (ºC)", x = NULL)
  })
  output$precip <- renderPlot({
    data_station() |>
      ggplot(aes(x = datetime, y = precip_total_mm)) +
      geom_col() +
      labs(title = "Precipitation (mm)", x = NULL)
  })
}

shinyApp(ui, server)
