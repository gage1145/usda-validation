library(shiny)
library(bslib)
library(ggplot2)
library(plotly)

# ---------------------------------------------------------------------------
# Load precomputed data
# ---------------------------------------------------------------------------
results  <- read.csv("data/results.csv", stringsAsFactors = FALSE)

df_ <- read.csv("data/results_clean.csv", stringsAsFactors = FALSE)
df_$assay <- factor(df_$assay, levels = c("RT-QuIC", "Nano-QuIC"))
df_$mpi   <- as.integer(df_$mpi)
for (col in grep("date|dob", names(df_), value = TRUE)) {
  df_[[col]] <- as.Date(df_[[col]])
}

coord_df <- read.csv("data/roc_coords.csv", stringsAsFactors = FALSE)
coord_df$metric <- factor(coord_df$metric, levels = sort(unique(coord_df$metric)))

auc_df <- read.csv("data/roc_auc.csv", stringsAsFactors = FALSE)
auc_df$metric <- factor(auc_df$metric, levels = levels(coord_df$metric))

# Explore tab helpers -------------------------------------------------------
sample_types  <- sort(unique(df_$sample_type))
color_choices <- c("mortem", "group", "genotype", "sex", "animal_id")
color_choices <- color_choices[color_choices %in% names(df_)]

metric_choices <- c("mpr", "ms", "auc", "raf", "ttt")
metric_choices <- metric_choices[metric_choices %in% names(df_)]

assay_choices <- c("Both", levels(df_$assay))

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_navbar(
  title = "USDA CWD Validation",
  theme = bs_theme(bootswatch = "flatly"),

  # -- Tab 1: RAMALT ROC ----------------------------------------------------
  nav_panel(
    "RAMALT ROC",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "roc_dilution", "Dilution",
          choices  = c("All", sort(unique(coord_df$dilution))),
          selected = "All"
        ),
        selectInput(
          "roc_metric", "Metric",
          choices  = c("All", levels(coord_df$metric)),
          selected = "All"
        )
      ),
      plotlyOutput("roc_plot", height = "650px")
    )
  ),

  # -- Tab 2: Explore -------------------------------------------------------
  nav_panel(
    "Explore",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("exp_sample_type", "Sample Type",
                    choices = sample_types, selected = sample_types[1]),
        selectInput("exp_metric_x", "X-Metric",
                    choices = toupper(metric_choices), selected = toupper(metric_choices[1])),
        selectInput("exp_metric_y", "Y-Metric",
                    choices = toupper(metric_choices), selected = toupper(metric_choices[3])),
        selectInput("exp_assay", "Assay",
                    choices = assay_choices, selected = "Both"),
        selectInput("exp_color", "Color by",
                    choices = color_choices, selected = color_choices[1]),
        radioButtons("exp_plot_type", "Plot type",
                     choices = c("Boxplot", "Jitter"), selected = "Boxplot"),
        sliderInput("exp_time_slider", "Months",
                    min = min(df_$mpi, na.rm = TRUE), max = max(df_$mpi, na.rm = TRUE),
                    value = min(df_$mpi, na.rm = TRUE), step = 1, animate = animationOptions(50, FALSE))
      ),
      plotlyOutput("explore_plot", height = "600px")
    )
  ),

  # -- Tab 3: Longitudinal --------------------------------------------------
  nav_panel(
    "Longitudinal",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("long_sample_type", "Sample Type",
                    choices = sample_types, selected = sample_types[1]),
        selectInput("long_metric", "Metric",
                    choices = toupper(metric_choices), selected = toupper(metric_choices[1])),
        selectInput("long_assay", "Assay",
                    choices = assay_choices, selected = "Both"),
        selectInput("long_color", "Color by",
                    choices = color_choices, selected = color_choices[1]),
        radioButtons("long_mode", "Display",
                     choices = c("Raw", "Cumulative"),
                     selected = "Raw")
      ),
      plotlyOutput("long_plot", height = "600px")
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  main_theme <- theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  )

  # -- RAMALT ROC -----------------------------------------------------------
  output$roc_plot <- renderPlotly({
    cd <- coord_df
    ad <- auc_df

    if (input$roc_dilution != "All") {
      cd <- cd[cd$dilution == input$roc_dilution, ]
      ad <- ad[ad$dilution == input$roc_dilution, ]
    }

    if (input$roc_metric != "All") {
      cd <- cd[cd$metric == input$roc_metric, ]
      ad <- ad[ad$metric == input$roc_metric, ]
    }

    cd <- cd[order(cd$sensitivity), ]

    p <- ggplot(cd, aes(
        specificity, sensitivity, color = assay, group = 1,
        text = paste0(
          "Assay: ", assay,
          "<br>Dilution: ", dilution,
          "<br>Spec: ", round(specificity, 3),
          "<br>Sens: ", round(sensitivity, 3),
          "<br>Thresh: ", round(threshold, 3)
        )
      )) +
      geom_abline(slope = 1, intercept = 1, linetype = "dashed", alpha = 0.5) +
      geom_step() +
      geom_text(
        aes(x = specificity, y = sensitivity, label = label, color = assay),
        data = ad, hjust = 0, size = 5,
        show.legend = FALSE, inherit.aes = FALSE
      ) +
      scale_color_manual(values = c("darkorange", "darkslateblue")) +
      facet_grid(vars(dilution), vars(metric)) +
      scale_x_reverse() +
      labs(y = "Sensitivity", x = "Specificity", color = "Assay") +
      theme_bw(base_size = 13) +
      main_theme +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text") |>
      layout(legend = list(orientation = "h", x = 0.3, y = -0.1))
  })

  # -- Explore --------------------------------------------------------------
  explore_data <- reactive({
    input_mpi    <- input$exp_time_slider
    metric_col_x <- tolower(input$exp_metric_x)
    metric_col_y <- tolower(input$exp_metric_y)

    mask <- df_$sample_type == input$exp_sample_type &
            !is.na(df_$mpi) & df_$mpi == input_mpi &
            !is.na(df_[[metric_col_x]]) &
            !is.na(df_[[metric_col_y]])
    d <- df_[mask, ]

    if (input$exp_assay != "Both") {
      d <- d[d$assay == input$exp_assay, ]
    }
    d
  })

  output$explore_plot <- renderPlotly({
    metric_col_x <- tolower(input$exp_metric_x)
    metric_col_y <- tolower(input$exp_metric_y)
    color_col    <- input$exp_color
    d            <- explore_data()

    if (nrow(d) == 0) {
      return(plotly_empty(type = "scatter") |>
               layout(title = "No data for selected filters"))
    }

    min_x <- min(df_[[metric_col_x]], na.rm = TRUE)
    max_x <- max(df_[[metric_col_x]], na.rm = TRUE)
    min_y <- min(df_[[metric_col_y]], na.rm = TRUE)
    max_y <- max(df_[[metric_col_y]], na.rm = TRUE)

    p <- ggplot(d, aes(
      x     = .data[[metric_col_x]],
      y     = .data[[metric_col_y]],
      color = .data[[color_col]],
      fill  = .data[[color_col]],
      text  = paste0(
        color_col, ": ", .data[[color_col]],
        "<br>", toupper(metric_col_x), ": ",
        round(as.numeric(.data[[metric_col_x]]), 3),
        "<br>", toupper(metric_col_y), ": ",
        round(as.numeric(.data[[metric_col_y]]), 3)
      )
    )) +
      geom_point(alpha = 0.7, size = 2) +
      facet_grid(vars(dilution), vars(assay), scales = "fixed") +
      labs(
        x     = toupper(metric_col_x),
        y     = toupper(metric_col_y),
        title = paste(input$exp_sample_type, "-", toupper(metric_col_x), "vs", toupper(metric_col_y)),
        fill  = color_col,
        color = color_col
      ) +
      scale_x_log10(limits = c(min_x, max_x)) +
      scale_y_log10(limits = c(min_y, max_y)) +
      theme_bw(base_size = 12) +
      main_theme +
      theme(legend.position = "right")

    ggplotly(p, tooltip = "text")
  })

  # -- Longitudinal ---------------------------------------------------------
  output$long_plot <- renderPlotly({
    metric_col <- tolower(input$long_metric)
    color_col  <- input$long_color

    mask <- df_$sample_type == input$long_sample_type &
            !is.na(df_$mpi) &
            !is.na(df_[[metric_col]])
    d <- df_[mask, ]

    if (input$long_assay != "Both") {
      d <- d[d$assay == input$long_assay, ]
    }

    if (nrow(d) == 0) {
      return(plotly_empty(type = "scatter") |>
               layout(title = "No data for selected filters"))
    }

    # Aggregate mean by group
    agg <- aggregate(
      d[[metric_col]],
      by = list(mpi = d$mpi, color_ = d[[color_col]], assay = d$assay, dilution = d$dilution),
      FUN = mean
    )
    names(agg)[names(agg) == "x"]      <- metric_col
    names(agg)[names(agg) == "color_"] <- color_col
    d <- agg[order(agg$mpi), ]

    if (input$long_mode == "Raw") {
      p <- ggplot(d, aes(
          x     = mpi,
          y     = .data[[metric_col]],
          color = .data[[color_col]],
          fill  = .data[[color_col]],
          group = .data[[color_col]],
          text  = paste0(
            "MPI: ", mpi,
            "<br>", toupper(metric_col), ": ", round(as.numeric(.data[[metric_col]]), 3),
            "<br>", color_col, ": ", .data[[color_col]]
          )
        )) +
        geom_line() +
        geom_point(size = 2, alpha = 0.8)

    } else {
      d$cum <- ave(d[[metric_col]], d[[color_col]], d$assay, d$dilution, FUN = cumsum)

      p <- ggplot(d, aes(
          x     = mpi,
          y     = cum,
          color = .data[[color_col]],
          fill  = .data[[color_col]],
          group = .data[[color_col]],
          text  = paste0(
            "MPI: ", mpi,
            "<br>Cumulative ", metric_col, ": ", round(as.numeric(cum), 3),
            "<br>", color_col, ": ", .data[[color_col]]
          )
        )) +
        stat_smooth() +
        geom_point(size = 2)
    }

    p <- p +
      facet_grid(vars(assay), vars(dilution)) +
      labs(
        x     = "Months Post-Inoculation (MPI)",
        y     = toupper(metric_col),
        title = paste(input$long_sample_type, "-", toupper(metric_col), "over time"),
        color = color_col,
        fill  = NULL
      ) +
      theme_bw(base_size = 12) +
      main_theme +
      theme(legend.position = "right")

    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
