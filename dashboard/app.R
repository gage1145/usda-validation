library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(pROC)
library(DT)
library(plotly)
library(lubridate)

# ---------------------------------------------------------------------------
# Load data at startup (shinylive bundles the data/ directory)
# ---------------------------------------------------------------------------
# animals <- read.csv("data/animals.csv", stringsAsFactors = FALSE)
results <- read.csv("data/results.csv", stringsAsFactors = FALSE)

# Merge and type-cast -------------------------------------------------------
df_ <- results %>%
  select(-c(temperature, result_id, sample_type_id, concentration, inoculum)) %>%
  mutate(
    across(c(animal_id, room_number, group, sex, genotype, sample_id, sample_type, mortem, well, reader), as.factor),
    across(matches("date|dob"), as_date),
    assay  = factor(assay, levels = c("RT-QuIC", "Nano-QuIC")),
    mpi    = as.integer(mpi),
    across(any_of(c("mpr", "raf", "ttt", "ms", "auc", "dilution")), as.numeric)
  )

# ROC dataset (RAMALT only) -------------------------------------------------
df_roc <- df_ %>%
  filter(sample_type == "RAMALT") %>%
  filter(mpi == 0 | mortem == "post-mortem") %>%
  mutate(positive = as.integer(mortem == "post-mortem")) %>%
  pivot_longer(cols = any_of(c("mpr", "ms", "auc")), names_to = "name", values_to = "value")

relabel_metrics <- function(x) {
  lvls <- sort(unique(x))
  factor(x, levels = lvls, labels = toupper(lvls))
}

# Compute ROC curves --------------------------------------------------------
compute_roc <- function(m, a, d) {
  sub_df <- df_roc %>% filter(name == m, assay == a, dilution == d)
  if (nrow(sub_df) < 2 || length(unique(sub_df$positive)) < 2) return(NULL)
  sub_roc <- tryCatch(
    roc(sub_df, response = "positive", predictor = "value", quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(sub_roc)) return(NULL)
  coord_df <- coords(sub_roc) %>%
    mutate(metric = m, assay = a, dilution = d)
  list(roc = sub_roc, coords = coord_df)
}

combos <- expand.grid(
  m = unique(df_roc$name),
  a = levels(df_roc$assay),
  d = unique(df_roc$dilution),
  stringsAsFactors = FALSE
)

roc_list    <- Map(compute_roc, combos$m, combos$a, combos$d)
valid       <- !sapply(roc_list, is.null)
roc_list    <- roc_list[valid]
combos_valid <- combos[valid, ]

coord_df <- lapply(roc_list, `[[`, "coords") %>%
  bind_rows() %>%
  mutate(metric = relabel_metrics(metric))

auc_df <- combos_valid %>%
  mutate(
    auc         = sapply(roc_list, function(r) as.numeric(auc(r$roc))),
    specificity = 0.65,
    sensitivity = ifelse(a == "RT-QuIC", 0.15, 0.05),
    label       = paste0(a, " AUC = ", signif(auc, 3)),
    m           = relabel_metrics(m)
  ) %>%
  rename(assay = a, dilution = d, metric = m) %>%
  arrange(desc(auc))

# Explore tab helpers -------------------------------------------------------
sample_types  <- sort(unique(df_$sample_type))
color_choices <- c("mortem", "group", "genotype", "sex")
color_choices <- color_choices[color_choices %in% names(df_)]

metric_choices <- c("mpr", "ms", "auc", "ttt")
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
        selectInput("exp_metric", "Metric",
                    choices = toupper(metric_choices), selected = toupper(metric_choices[1])),
        selectInput("exp_assay", "Assay",
                    choices = assay_choices, selected = "Both"),
        selectInput("exp_color", "Color by",
                    choices = color_choices, selected = color_choices[1]),
        radioButtons("exp_plot_type", "Plot type",
                     choices = c("Boxplot", "Jitter"), selected = "Boxplot")
      ),
      plotlyOutput("explore_plot", height = "600px")
    )
  ),

  # -- Tab 3: Results Table -------------------------------------------------
  nav_panel(
    "Results Table",
    DTOutput("results_table")
  ),

  # -- Tab 4: AUC Summary ---------------------------------------------------
  nav_panel(
    "AUC Summary",
    DTOutput("auc_table")
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # -- RAMALT ROC -----------------------------------------------------------
  output$roc_plot <- renderPlotly({
    cd <- coord_df
    ad <- auc_df

    if (input$roc_dilution != "All") {
      cd <- cd %>% filter(dilution == input$roc_dilution)
      ad <- ad %>% filter(dilution == input$roc_dilution)
    }

    p <- cd %>%
      group_by(assay, dilution, metric) %>%
      arrange(sensitivity) %>%
      ggplot(aes(specificity, sensitivity, color = assay,
                 text = paste0("Assay: ", assay,
                               "<br>Dilution: ", dilution,
                               "<br>Spec: ", round(specificity, 3),
                               "<br>Sens: ", round(sensitivity, 3)))) +
      geom_step() +
      scale_color_manual(values = c("darkslateblue", "darkorange")) +
      facet_grid(vars(dilution), vars(metric)) +
      scale_x_reverse() +
      labs(y = "Sensitivity", x = "Specificity", color = "Assay") +
      theme_bw(base_size = 13) +
      theme(legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.3, y = -0.1))
  })

  # -- Explore --------------------------------------------------------------
  explore_data <- reactive({
    metric_col <- tolower(input$exp_metric)
    d <- df_ %>%
      filter(sample_type == input$exp_sample_type) %>%
      filter(!is.na(.data[[metric_col]]))

    if (input$exp_assay != "Both") {
      d <- d %>% filter(assay == input$exp_assay)
    }
    d
  })

  output$explore_plot <- renderPlotly({
    metric_col <- tolower(input$exp_metric)
    color_col  <- input$exp_color
    d <- explore_data()

    if (nrow(d) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(title = "No data for selected filters"))
    }

    p <- ggplot(d, aes(
      x    = assay,
      y    = .data[[metric_col]],
      fill = .data[[color_col]],
      text = paste0(color_col, ": ", .data[[color_col]],
                    "<br>", toupper(metric_col), ": ",
                    round(as.numeric(.data[[metric_col]]), 3))
    ))

    if (input$exp_plot_type == "Boxplot") {
      p <- p + geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        geom_jitter(width = 0.15, alpha = 0.4, size = 1)
    } else {
      p <- p + geom_jitter(width = 0.2, alpha = 0.7, size = 2,
                           aes(color = .data[[color_col]]))
    }

    p <- p +
      facet_wrap(vars(dilution)) +
      labs(
        x     = "Assay",
        y     = toupper(metric_col),
        title = paste(input$exp_sample_type, "–", toupper(metric_col)),
        fill  = color_col,
        color = color_col
      ) +
      theme_bw(base_size = 12) +
      theme(legend.position = "right")

    ggplotly(p, tooltip = "text")
  })

  # -- Results Table --------------------------------------------------------
  output$results_table <- renderDT({
    datatable(
      results,
      filter    = "top",
      rownames  = FALSE,
      options   = list(pageLength = 25, scrollX = TRUE)
    )
  })

  # -- AUC Summary ----------------------------------------------------------
  output$auc_table <- renderDT({
    datatable(
      auc_df %>% select(metric, assay, dilution, auc) %>%
        mutate(auc = round(auc, 4)),
      filter   = "top",
      rownames = FALSE,
      options  = list(pageLength = 25, order = list(list(3, "desc")))
    )
  })
}

shinyApp(ui, server)
