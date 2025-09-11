library(shiny)
library(shinyMobile)
library(azmetr)
library(dplyr)
library(reactable)
library(ggplot2)
library(thematic)
thematic_shiny(font = "auto")
# library(highcharter)

#global options
options <- f7DefaultOptions()
# options$color <- "#0C234B" #arizona blue
# options$filled <- TRUE #fill navbar and toolbar with options$color
options$dark <- FALSE
options$theme <- "auto" #"ios" to see iOS them, "auto" or "md" to see Android theme
# options$skeletonsOnLoad <- TRUE #built-in loading indicators
# options$pullToRefresh <- TRUE # intersting, maybe could be used to refresh data?

# pre-load station data
stations <- station_info$meta_station_id
names(stations) <- station_info$meta_station_name
ui <- f7Page(
  shiny::includeCSS("custom.css"),
  f7TabLayout(
    navbar = f7Navbar(
      title = div("AZMet"),
      leftPanel = TRUE,
      hairline = FALSE
    ),
    panels = f7Panel(
      title = "Menu",
      f7Block(f7Toggle(
        inputId = "dark",
        label = f7Icon("moon"),
        checked = options$dark
      )),
      f7Block(f7Link(
        "  About",
        icon = f7Icon("question_circle"),
        href = "https://azmet.arizona.edu"
      )),
      f7Block(f7Link(
        "  Source code",
        icon = f7Icon("logo_github"),
        href = "https://github.com/Aariq/azmet-mobile"
      )),
      f7Block(f7Button(
        inputId = "btn_refresh",
        label = "Refresh Data",
        icon = f7Icon("arrow_2_squarepath")
      )),
      side = "left",
      effect = "cover"
    ),
    f7Tabs(
      f7Tab(
        title = "Plots",
        tabName = "tabplots",
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
          ),
          f7AccordionItem(
            title = "Relative Humidity",
            f7Card(
              plotOutput("rh")
            )
          )
        )
      ),
      f7Tab(
        title = "Table",
        tabName = "tabtable",
        f7BlockTitle("Latest 15-minute data from across the network"),
        f7Block(
          f7BlockHeader(
            "uses `f7Table()`"
          ),
          uiOutput("table")
        )
      ),
      f7Tab(
        title = "Table2",
        tabName = "tabtable2",
        f7BlockTitle("Latest 15-minute data from across the network"),
        f7Block(
          f7BlockHeader(
            "uses `reactable`"
          ),
          reactableOutput("table2"),
        )
      )
    )
  ),
  options = options,
  title = "AZMet",
  allowPWA = TRUE
)

server <- function(input, output, session) {
  # dark mode switch
  observeEvent(input$dark, ignoreInit = TRUE, {
    updateF7App(
      options = list(
        dark = input$dark
      )
    )
  })

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

  output$table <- renderUI({
    data() |>
      dplyr::group_by(meta_station_id) |>
      dplyr::filter(datetime == max(datetime)) |>
      dplyr::ungroup() |>
      dplyr::select(
        Station = meta_station_name,
        `Latest Update` = datetime,
        p = precip_total_mm,
        RH = relative_humidity,
        SR = sol_rad_total,
        T = temp_airC,
        T_max = temp_air_maxC,
        T_min = temp_air_minC,
        T_dew_pt = dwpt,
        T_soil_10 = temp_soil_10cmC,
        T_soil_50 = temp_soil_50cmC,
        WD = wind_vector_dir,
        WD_2min = wind_2min_vector_dir,
        WD_2min_max = wind_2min_vector_dir_max_hourly,
        WS = wind_spd_mps,
        WS_max = wind_spd_max_mps,
        WS_2min = wind_2min_spd_mean_mps,
        WS_2min_max = wind_2min_spd_max_mps_hourly
      ) |>
      f7Table(
        # card = TRUE #put table in card or not (modifies appearance slightly)
      )
  })

  output$table2 <- renderReactable({
    data() |>
      dplyr::group_by(meta_station_id) |>
      dplyr::filter(datetime == max(datetime)) |>
      dplyr::ungroup() |>
      dplyr::select(
        meta_station_name,
        datetime,
        p = precip_total_mm,
        RH = relative_humidity,
        SR = sol_rad_total,
        T = temp_airC,
        T_max = temp_air_maxC,
        T_min = temp_air_minC,
        T_dew_pt = dwpt,
        T_soil_10 = temp_soil_10cmC,
        T_soil_50 = temp_soil_50cmC,
        WD = wind_vector_dir,
        WD_2min = wind_2min_vector_dir,
        WD_2min_max = wind_2min_vector_dir_max_hourly,
        WS = wind_spd_mps,
        WS_max = wind_spd_max_mps,
        WS_2min = wind_2min_spd_mean_mps,
        WS_2min_max = wind_2min_spd_max_mps_hourly
      ) |>
      reactable::reactable(
        columns = list(
          meta_station_name = reactable::colDef(
            name = "Station",
            sticky = "left",
            minWidth = 150,
            rowHeader = FALSE
          ),
          datetime = reactable::colDef(
            name = "Latest Update",
            minWidth = 180,
            rowHeader = TRUE
          )
        ),
        pagination = FALSE
      )
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
  output$rh <- renderPlot({
    data_station() |>
      ggplot(aes(x = datetime, y = relative_humidity)) +
      geom_line() +
      labs(title = "RH (%)", x = NULL)
  })
}

shinyApp(ui, server)
