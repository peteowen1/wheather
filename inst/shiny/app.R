library(shiny)
library(bslib)
library(ggplot2)
library(data.table)
library(wheather)

# --- UI ---
ui <- page_sidebar(
  title = "wheather",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50"
  ),

  sidebar = sidebar(
    width = 320,
    title = "Compare Weather",

    textInput("city", "City", value = "Sydney"),
    actionButton("geocode_btn", "Look up", class = "btn-sm btn-outline-primary"),
    verbatimTextOutput("coords_display"),

    hr(),
    h6("Period 1 (this year)"),
    dateInput("start1", "Start", value = "2024-12-01"),
    dateInput("end1", "End", value = "2025-02-28"),

    h6("Period 2 (last year)"),
    dateInput("start2", "Start", value = "2023-12-01"),
    dateInput("end2", "End", value = "2024-02-29"),

    hr(),
    h6("Scoring Weights"),
    sliderInput("w_temp", "Temperature", min = 0, max = 1, value = 0.25, step = 0.05),
    sliderInput("w_rain", "Rain", min = 0, max = 1, value = 0.25, step = 0.05),
    sliderInput("w_sky", "Sky (sun+cloud)", min = 0, max = 1, value = 0.20, step = 0.05),
    sliderInput("w_humidity", "Humidity", min = 0, max = 1, value = 0.10, step = 0.05),
    sliderInput("w_wind", "Wind", min = 0, max = 1, value = 0.20, step = 0.05),

    hr(),
    h6("Temperature Ideal Range (\u00b0C)"),
    sliderInput("temp_range", NULL, min = 10, max = 40, value = c(21, 25), step = 1),

    hr(),
    actionButton("go", "Compare!", class = "btn-primary btn-lg w-100"),
    textOutput("weight_warning")
  ),

  # Main panel with tabs
  navset_card_tab(
    id = "main_tabs",

    nav_panel(
      "Overview",
      layout_columns(
        col_widths = c(4, 4, 4),
        uiOutput("verdict_card"),
        uiOutput("period1_card"),
        uiOutput("period2_card")
      ),
      card(
        card_header("Daily Scores Over Time"),
        plotOutput("scores_timeline", height = "400px")
      )
    ),

    nav_panel(
      "Components",
      card(
        card_header("Score Breakdown by Component"),
        plotOutput("component_bars", height = "350px")
      ),
      card(
        card_header("Component Scores Over Time"),
        plotOutput("component_timeline", height = "450px")
      )
    ),

    nav_panel(
      "Data",
      card(
        card_header("Raw Daily Data"),
        DT::dataTableOutput("raw_table")
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {


  # Stable colour palette: Period 1 always blue, Period 2 always orange
  period_colours <- function() {
    req(rv$comparison)
    labels <- unique(rv$comparison$data$label)
    setNames(c("#2980b9", "#e67e22")[seq_along(labels)], labels)
  }

  # Reactive values
  rv <- reactiveValues(
    lat = -33.87,
    lon = 151.21,
    city_name = "Sydney, AU",
    comparison = NULL
  )

  # Geocode city
  observeEvent(input$geocode_btn, {
    req(input$city)
    tryCatch({
      result <- geocode(input$city)
      rv$lat <- result$lat[1]
      rv$lon <- result$lon[1]
      rv$city_name <- paste0(result$name[1], ", ", result$country[1])
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
    })
  })

  output$coords_display <- renderText({
    sprintf("%s (%.2f, %.2f)", rv$city_name, rv$lat, rv$lon)
  })

  # Weight validation
  output$weight_warning <- renderText({
    total <- input$w_temp + input$w_rain + input$w_sky +
             input$w_humidity + input$w_wind
    if (abs(total - 1) > 0.01) {
      sprintf("Weights sum to %.2f (should be 1.00)", total)
    } else {
      ""
    }
  })

  # Run comparison
  observeEvent(input$go, {
    weights <- list(
      temp = input$w_temp, rain = input$w_rain, sky = input$w_sky,
      humidity = input$w_humidity, wind = input$w_wind
    )
    params <- default_params()
    # Shift all three temp ideal ranges by the same offset as the user's slider
    mean_shift_lo <- input$temp_range[1] - params$temp$mean_ideal_min
    mean_shift_hi <- input$temp_range[2] - params$temp$mean_ideal_max
    params$temp$mean_ideal_min <- input$temp_range[1]
    params$temp$mean_ideal_max <- input$temp_range[2]
    params$temp$max_ideal_min  <- params$temp$max_ideal_min + mean_shift_lo
    params$temp$max_ideal_max  <- params$temp$max_ideal_max + mean_shift_hi
    params$temp$min_ideal_min  <- params$temp$min_ideal_min + mean_shift_lo
    params$temp$min_ideal_max  <- params$temp$min_ideal_max + mean_shift_hi

    withProgress(message = "Fetching weather data...", {
      tryCatch({
        rv$comparison <- compare_periods(
          lat = rv$lat, lon = rv$lon,
          start1 = as.character(input$start1), end1 = as.character(input$end1),
          start2 = as.character(input$start2), end2 = as.character(input$end2),
          weights = weights, params = params
        )
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error")
      })
    })
  })

  # --- Overview tab ---
  output$verdict_card <- renderUI({
    req(rv$comparison)
    s <- rv$comparison$summary
    value_box(
      title = "Verdict",
      value = s$verdict,
      showcase = bsicons::bs_icon("trophy"),
      theme = "primary",
      full_screen = FALSE
    )
  })

  output$period1_card <- renderUI({
    req(rv$comparison)
    s <- rv$comparison$summary$period1
    value_box(
      title = s$label,
      value = sprintf("%.1f", s$mean),
      p(sprintf("SD: %.1f | Best: %.0f | Worst: %.0f", s$sd, s$best, s$worst)),
      showcase = bsicons::bs_icon("sun"),
      theme = "info"
    )
  })

  output$period2_card <- renderUI({
    req(rv$comparison)
    s <- rv$comparison$summary$period2
    value_box(
      title = s$label,
      value = sprintf("%.1f", s$mean),
      p(sprintf("SD: %.1f | Best: %.0f | Worst: %.0f", s$sd, s$best, s$worst)),
      showcase = bsicons::bs_icon("cloud-sun"),
      theme = "secondary"
    )
  })

  output$scores_timeline <- renderPlot({
    req(rv$comparison)
    dt <- rv$comparison$data
    ggplot(dt, aes(x = day_index, y = score_total, colour = label)) +
      geom_line(linewidth = 0.8, alpha = 0.7) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
      scale_colour_manual(values = period_colours()) +
      labs(x = "Day of Period", y = "Weather Score (0-100)", colour = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })

  # --- Components tab ---
  output$component_bars <- renderPlot({
    req(rv$comparison)
    dt <- rv$comparison$data
    score_cols <- c("score_temp", "score_rain", "score_sky",
                    "score_humidity", "score_wind")
    means <- dt[, lapply(.SD, mean, na.rm = TRUE), by = label, .SDcols = score_cols]
    melted <- data.table::melt(means, id.vars = "label", variable.name = "component", value.name = "score")
    melted[, component := gsub("score_", "", component)]

    ggplot(melted, aes(x = component, y = score, fill = label)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = period_colours()) +
      labs(x = NULL, y = "Mean Score (0-100)", fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })

  output$component_timeline <- renderPlot({
    req(rv$comparison)
    dt <- rv$comparison$data
    score_cols <- c("score_temp", "score_rain", "score_sky",
                    "score_humidity", "score_wind")
    melted <- data.table::melt(dt, id.vars = c("day_index", "label"),
                                measure.vars = score_cols,
                                variable.name = "component", value.name = "score")
    melted[, component := gsub("score_", "", component)]

    ggplot(melted, aes(x = day_index, y = score, colour = label)) +
      geom_line(alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
      facet_wrap(~component, ncol = 2, scales = "free_y") +
      scale_colour_manual(values = period_colours()) +
      labs(x = "Day of Period", y = "Score", colour = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })

  # --- Data tab ---
  output$raw_table <- DT::renderDataTable({
    req(rv$comparison)
    dt <- rv$comparison$data[, .(label, date, score_total, score_temp, score_rain,
                                  score_sky, score_humidity, score_wind,
                                  temp_mean, temp_min, temp_max,
                                  precip_total, sunshine_secs, cloud_cover,
                                  humidity_mean, wind_max_kmh, wind_gust_kmh)]
    # Convert sunshine to hours for readability
    data.table::set(dt, j = "sunshine_hrs", value = round(dt[["sunshine_secs"]] / 3600, 1))
    data.table::set(dt, j = "sunshine_secs", value = NULL)

    DT::datatable(dt, options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE) |>
      DT::formatRound(columns = c("score_total", "score_temp", "score_rain", "score_sky",
                                    "score_humidity", "score_wind"), digits = 1) |>
      DT::formatRound(columns = c("temp_mean", "temp_min", "temp_max",
                                    "precip_total", "wind_max_kmh", "wind_gust_kmh"), digits = 1)
  })
}

shinyApp(ui, server)
