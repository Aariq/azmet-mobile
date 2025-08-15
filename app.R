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
options$dark <- FALSE
options$theme <- "auto"

# pre-load station data
stations <- station_info$meta_station_id
names(stations) <- station_info$meta_station_name
ui <- f7Page(
  f7SingleLayout(
    navbar = f7Navbar(
      title = div("AZMet"),
      hairline = FALSE
    ),
    toolbar = f7Toolbar(
      f7Button(inputId = "btn_refresh", label = "Refresh Data"),
      position = "bottom",
      icons = TRUE
    ),
    f7List(
      f7SmartSelect(
        inputId = "station",
        label = "Select a Station",
        choices = stations
      ),
      # f7DatePicker(
      #   inputId = "daterange",
      #   label = "Date Range",
      #   rangePicker = TRUE
      # ),
      # outline = TRUE,
      # dividers = TRUE,
      strong = TRUE
    ),
    f7Swiper(
      id = "plots_swiper",
      f7Slide(
        f7Card(
          plotOutput("temp")
        )
      ),
      f7Slide(
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
