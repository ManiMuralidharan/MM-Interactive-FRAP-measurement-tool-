# MM-Interactive-FRAP-measurement-tool-
FRAP measurement tool 
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(minpack.lm)
library(bslib)
library(plotly)
library(DT)

# Define Modern UI using bslib
ui <- page_sidebar(
  useShinyjs(),
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly",
    primary = "#2c3e50",
    success = "#27ae60",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c"
  ),
  
  title = tags$div(
    tags$span("MMFRAP - FRAP Analysis Tool", style = "font-weight: bold;")
  ),
  
  sidebar = sidebar(
    width = 420,
    open = TRUE,
    
    # 1. Dataset Selection
    card(
      card_header(
        tags$div(
          tags$span("1. Dataset Selection", style = "font-weight: bold;"),
          class = "bg-primary text-white p-2 rounded"
        )
      ),
      textInput("exp_name", "Experiment name:", value = "cdt1", placeholder = "(optional)"),
      selectInput("file_format", "File format:", choices = c(".csv", ".txt", ".xlsx"), selected = ".csv"),
      textInput("file_location", "Location of the files:", 
                value = "C:\\Users\\Marianna\\Documents\\MATLAB\\experiment1"),
      div(
        class = "d-flex gap-2",
        actionButton("browse_btn", "Browse", class = "btn-secondary", icon = icon("folder-open")),
        actionButton("load_btn", "Upload", class = "btn-success", icon = icon("upload"))
      ),
      hr(),
      helpText("Supported formats: CSV with columns for Time, ROI, Background, Reference")
    ),
    
    # 2. Raw Data Visualization & Exclusion Combined
    card(
      card_header(
        tags$div(
          tags$span("2. Sample Management", style = "font-weight: bold;"),
          class = "bg-info text-white p-2 rounded"
        )
      ),
      h6("Samples to Display/Analyze:"),
      checkboxGroupInput("samples_display", "Select samples:", choices = NULL),
      hr(),
      h6("Exclude Samples:"),
      checkboxGroupInput("exclude_samples", "Select samples to exclude:", choices = NULL),
      div(
        class = "d-flex gap-2",
        actionButton("delete_btn", "Delete Selected", class = "btn-danger", icon = icon("trash")),
        actionButton("restore_btn", "Restore All", class = "btn-secondary", icon = icon("undo"))
      )
    ),
    
    # 3. Bleaching Parameters
    card(
      card_header(
        tags$div(
          tags$span("3. Bleaching Parameters", style = "font-weight: bold;"),
          class = "bg-warning text-dark p-2 rounded"
        )
      ),
      fluidRow(
        column(6, numericInput("pre_bleach", "Pre-Bleach Values:", value = 50, min = 1)),
        column(6, numericInput("bleach_values", "Bleach Values:", value = 2, min = 1))
      ),
      fluidRow(
        column(6, numericInput("post_bleach", "Post-Bleach Values:", value = 250, min = 1)),
        column(6, numericInput("discard_values", "Initial Values to discard:", value = 10, min = 0))
      ),
      helpText("(optional, proposed value: 10)"),
      hr(),
      div(
        class = "bg-light p-2 rounded",
        verbatimTextOutput("bleaching_depth"),
        verbatimTextOutput("gap_ratio")
      ),
      div(
        class = "d-flex gap-2 mt-2",
        actionButton("compute_params", "Compute", class = "btn-info", icon = icon("calculator")),
        actionButton("reset_params", "Reset", class = "btn-secondary", icon = icon("refresh"))
      )
    ),
    
    # 4. Normalization
    card(
      card_header(
        tags$div(
          tags$span("4. Normalization", style = "font-weight: bold;"),
          class = "bg-success text-white p-2 rounded"
        )
      ),
      selectInput("norm_method", "Method:", 
                  choices = c("double" = "double", "full scale" = "full", "single" = "single"), 
                  selected = "double"),
      actionButton("normalize_btn", "Normalize", class = "btn-primary w-100", icon = icon("chart-line"))
    ),
    
    # 5. Curve Fitting
    card(
      card_header(
        tags$div(
          tags$span("5. Curve Fitting", style = "font-weight: bold;"),
          class = "bg-secondary text-white p-2 rounded"
        )
      ),
      selectInput("select_sample", "Select sample:", choices = NULL),
      radioButtons("fitting_eq", "Fitting Equation",
                   choices = c("Single term" = "single", "Double term" = "double"),
                   selected = "single",
                   inline = TRUE),
      div(
        class = "d-flex gap-2",
        actionButton("fit_btn", "Fit", class = "btn-warning", icon = icon("chart-line")),
        actionButton("fit_mean_btn", "Fit mean data", class = "btn-info", icon = icon("chart-bar"))
      )
    ),
    
    # 6. Results
    card(
      card_header(
        tags$div(
          tags$span("6. Results", style = "font-weight: bold;"),
          class = "bg-dark text-white p-2 rounded"
        )
      ),
      div(
        class = "bg-light p-2 rounded",
        verbatimTextOutput("mobile_fraction"),
        verbatimTextOutput("thalf"),
        verbatimTextOutput("rsquare")
      ),
      hr(),
      downloadButton("save_btn", "Save Results", class = "btn-success w-100", icon = icon("save"))
    )
  ),
  
  # Main Panel with Enhanced Tabs
  navset_card_tab(
    title = "Analysis Results",
    nav_panel(
      "Normalized Curves",
      layout_sidebar(
        sidebar = sidebar(
          position = "right",
          width = 300,
          title = "Plot Controls",
          checkboxInput("show_points", "Show individual points", value = TRUE),
          sliderInput("line_size", "Line thickness", min = 0.5, max = 2, value = 1, step = 0.1),
          sliderInput("point_size", "Point size", min = 0.5, max = 3, value = 1.5, step = 0.1),
          selectInput("color_palette", "Color palette", 
                     choices = c("Default", "Viridis", "Set1", "Dark2"), 
                     selected = "Default")
        ),
        plotlyOutput("norm_curves_plot", height = "600px")
      )
    ),
    nav_panel(
      "Curve Fitting",
      layout_sidebar(
        sidebar = sidebar(
          position = "right",
          width = 300,
          title = "Fit Options",
          checkboxInput("show_confidence", "Show confidence interval", value = FALSE),
          sliderInput("ci_level", "Confidence level", min = 0.8, max = 0.99, value = 0.95, step = 0.01)
        ),
        plotlyOutput("fitting_plot", height = "600px")
      )
    ),
    nav_panel(
      "Raw Data",
      DTOutput("raw_data_table")
    ),
    nav_panel(
      "Summary Statistics",
      DTOutput("summary_stats")
    ),
    nav_panel(
      "Batch Results",
      br(),
      DTOutput("batch_results"),
      downloadButton("download_batch", "Download All Results", class = "btn-success")
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  
  # Reactive values to store application state
  values <- reactiveValues(
    raw_data = list(),
    normalized_data = list(),
    excluded_samples = c(),
    fitted_models = list(),
    current_fit_data = NULL,
    bleach_params = list(bleaching_depth = NULL, gap_ratio = NULL),
    fit_metrics = data.frame(),
    mean_data = NULL
  )
  
  # Helper function to read and parse CSV files
  read_frap_data <- function(file_path, file_name) {
    df <- read.csv(file_path, stringsAsFactors = FALSE)
    colnames(df) <- tolower(colnames(df))
    
    # Intelligent column detection
    time_col <- grep("time|frame|t$", colnames(df), value = TRUE)[1]
    roi_col <- grep("roi|intensity|bleach|signal", colnames(df), value = TRUE)[1]
    bg_col <- grep("background|bg|bkg", colnames(df), value = TRUE)[1]
    ref_col <- grep("reference|ref", colnames(df), value = TRUE)[1]
    
    # If reference column not found, use a constant
    if(is.na(ref_col)) {
      ref_col <- NULL
      ref_value <- 100
    } else {
      ref_value <- NULL
    }
    
    clean_df <- data.frame(
      Time = if(!is.na(time_col)) df[[time_col]] else 1:nrow(df),
      ROI = df[[roi_col]],
      Background = if(!is.na(bg_col)) df[[bg_col]] else 0
    )
    
    if(!is.null(ref_value)) {
      clean_df$Reference <- ref_value
    } else {
      clean_df$Reference <- df[[ref_col]]
    }
    
    sample_name <- tools::file_path_sans_ext(file_name)
    return(list(name = sample_name, data = clean_df))
  }
  
  # 1. Browse button (simulate file selection dialog)
  observeEvent(input$browse_btn, {
    showNotification("Browse functionality would open file explorer", type = "info")
  })
  
  # 2. Load Data with error handling
  observeEvent(input$load_btn, {
    req(input$file_upload)
    
    showNotification("Loading and parsing files...", type = "message", duration = 3)
    
    files <- input$file_upload
    sample_list <- list()
    errors <- c()
    
    withProgress(message = 'Loading files...', value = 0, {
      for (i in 1:nrow(files)) {
        tryCatch({
          result <- read_frap_data(files$datapath[i], files$name[i])
          sample_list[[result$name]] <- result$data
          incProgress(1/nrow(files))
        }, error = function(e) {
          errors <- c(errors, paste(files$name[i], ":", e$message))
        })
      }
    })
    
    if(length(errors) > 0) {
      showNotification(paste("Errors loading files:", paste(errors, collapse = "\n")), 
                      type = "error", duration = 10)
    }
    
    values$raw_data <- sample_list
    sample_names <- names(sample_list)
    
    # Update UI based on loaded data
    updateCheckboxGroupInput(session, "samples_display", 
                            choices = sample_names, 
                            selected = sample_names)
    updateCheckboxGroupInput(session, "exclude_samples", 
                            choices = sample_names, 
                            selected = character(0))
    updateSelectInput(session, "select_sample", 
                     choices = sample_names, 
                     selected = if(length(sample_names) > 0) sample_names[1] else NULL)
    
    showNotification(paste("Loaded", length(sample_list), "samples successfully!"), 
                    type = "success", duration = 3)
  })
  
  # 3. Delete selected samples
  observeEvent(input$delete_btn, {
    req(input$exclude_samples)
    values$excluded_samples <- unique(c(values$excluded_samples, input$exclude_samples))
    
    # Update displayed samples
    available_samples <- setdiff(names(values$raw_data), values$excluded_samples)
    updateCheckboxGroupInput(session, "samples_display", 
                            selected = available_samples)
    
    showNotification(paste("Excluded", length(input$exclude_samples), "samples"), 
                    type = "warning")
  })
  
  # 4. Restore all samples
  observeEvent(input$restore_btn, {
    values$excluded_samples <- c()
    updateCheckboxGroupInput(session, "samples_display", 
                            selected = names(values$raw_data))
    showNotification("All samples restored", type = "info")
  })
  
  # 5. Compute Bleaching Parameters
  observeEvent(input$compute_params, {
    req(values$raw_data, input$samples_display)
    
    # Take first selected sample for metrics
    first_sample <- input$samples_display[1]
    df <- values$raw_data[[first_sample]]
    
    # Calculate pre-bleach average
    pre_bleach_end <- input$pre_bleach
    if(pre_bleach_end > nrow(df)) pre_bleach_end <- nrow(df)
    
    pre_bleach_mean <- mean(df$ROI[1:pre_bleach_end] - df$Background[1:pre_bleach_end])
    bleach_min <- min(df$ROI[pre_bleach_end:min(pre_bleach_end + input$bleach_values, nrow(df))] - 
                      df$Background[pre_bleach_end:min(pre_bleach_end + input$bleach_values, nrow(df))])
    post_bleach_end <- min(pre_bleach_end + input$post_bleach, nrow(df))
    post_bleach_mean <- mean(df$ROI[pre_bleach_end:post_bleach_end] - 
                            df$Background[pre_bleach_end:post_bleach_end])
    
    values$bleach_params$bleaching_depth <- round((pre_bleach_mean - bleach_min) / pre_bleach_mean, 6)
    values$bleach_params$gap_ratio <- round(post_bleach_mean / pre_bleach_mean, 6)
    
    output$bleaching_depth <- renderPrint({ 
      cat("Bleaching Depth:", values$bleach_params$bleaching_depth)
    })
    
    output$gap_ratio <- renderPrint({ 
      cat("Gap Ratio:", values$bleach_params$gap_ratio)
    })
    
    showNotification("Bleaching parameters computed", type = "success")
  })
  
  # 6. Reset bleaching parameters
  observeEvent(input$reset_params, {
    values$bleach_params$bleaching_depth <- NULL
    values$bleach_params$gap_ratio <- NULL
    
    output$bleaching_depth <- renderPrint({ cat("Bleaching Depth:") })
    output$gap_ratio <- renderPrint({ cat("Gap Ratio:") })
    
    showNotification("Parameters reset", type = "info")
  })
  
  # 7. Normalize Data with proper FRAP formula
  observeEvent(input$normalize_btn, {
    req(values$raw_data)
    
    selected_samples <- intersect(input$samples_display, names(values$raw_data))
    selected_samples <- setdiff(selected_samples, values$excluded_samples)
    
    if(length(selected_samples) == 0) {
      showNotification("No samples selected for normalization", type = "warning")
      return(NULL)
    }
    
    normalized <- list()
    
    withProgress(message = 'Normalizing data...', value = 0, {
      for(i in seq_along(selected_samples)) {
        name <- selected_samples[i]
        df <- values$raw_data[[name]]
        
        # Discard initial values if specified
        if(input$discard_values > 0 && input$discard_values < nrow(df)) {
          df <- df[-(1:input$discard_values), ]
        }
        
        # Calculate pre-bleach indices
        pre_idx <- 1:min(input$pre_bleach, nrow(df))
        
        # Calculate corrected intensities
        roi_corrected <- df$ROI - df$Background
        ref_corrected <- df$Reference - df$Background
        
        # Pre-bleach means
        roi_pre_mean <- mean(roi_corrected[pre_idx])
        ref_pre_mean <- mean(ref_corrected[pre_idx])
        
        # Apply normalization based on selected method
        if(input$norm_method == "double") {
          # Double normalization (standard FRAP)
          df$Norm_Intensity <- (roi_corrected / ref_corrected) * (ref_pre_mean / roi_pre_mean)
        } else if(input$norm_method == "full") {
          # Full scale normalization (0 to 1)
          temp_norm <- (roi_corrected / ref_corrected) * (ref_pre_mean / roi_pre_mean)
          df$Norm_Intensity <- (temp_norm - min(temp_norm)) / (max(temp_norm) - min(temp_norm))
        } else {
          # Single normalization
          df$Norm_Intensity <- roi_corrected / roi_pre_mean
        }
        
        # FIX #1: REMOVED THE ARTIFICIAL CLAMPING
        # Let the raw biological data speak for itself - no capping at 0 or 2
        
        normalized[[name]] <- df
        incProgress(1/length(selected_samples))
      }
    })
    
    values$normalized_data <- normalized
    showNotification("Data normalized successfully!", type = "success")
    
    # Calculate and display mean data if multiple samples
    if(length(normalized) > 1) {
      all_data <- bind_rows(lapply(names(normalized), function(n) {
        df <- normalized[[n]]
        df$Sample <- n
        return(df)
      }))
      
      values$mean_data <- all_data %>%
        group_by(Time) %>%
        summarise(
          Mean_Intensity = mean(Norm_Intensity, na.rm = TRUE),
          SD_Intensity = sd(Norm_Intensity, na.rm = TRUE),
          SEM_Intensity = sd(Norm_Intensity, na.rm = TRUE) / sqrt(n()),
          .groups = 'drop'
        )
    }
  })
  
  # 8. Curve Fitting with improved algorithm
  fit_exponential <- function(data, fit_type, session_id) {
    # Find bleach point (minimum intensity)
    bleach_idx <- which.min(data$Norm_Intensity)
    fit_data <- data[bleach_idx:nrow(data), ]
    fit_data$Time <- fit_data$Time - fit_data$Time[1]  # Reset time to 0 at bleach
    
    # Remove any NA or infinite values
    fit_data <- fit_data[is.finite(fit_data$Norm_Intensity) & is.finite(fit_data$Time), ]
    
    if(nrow(fit_data) < 5) {
      showNotification("Insufficient data points for fitting", type = "error")
      return(NULL)
    }
    
    tryCatch({
      if(fit_type == "single") {
        # Single exponential: y = plateau + (1 - plateau) * (1 - exp(-t/tau))
        fit <- nlsLM(Norm_Intensity ~ plateau + (1 - plateau) * (1 - exp(-Time/tau)),
                    data = fit_data,
                    start = list(plateau = max(fit_data$Norm_Intensity), 
                                tau = median(fit_data$Time)),
                    lower = c(0, 0.1),
                    upper = c(1.2, 1000),
                    control = nls.lm.control(maxiter = 500))
        
        params <- coef(fit)
        mobile_fraction <- 1 - params["plateau"]
        t_half <- params["tau"] * log(2)
        
        return(list(
          fit = fit,
          params = params,
          mobile_fraction = mobile_fraction,
          t_half = t_half,
          t_half_fast = NA,
          t_half_slow = NA,
          model_type = "single",
          r_squared = 1 - sum(residuals(fit)^2) / sum((fit_data$Norm_Intensity - mean(fit_data$Norm_Intensity))^2)
        ))
        
      } else {
        # Double exponential: y = plateau + a1*(1-exp(-t/tau1)) + a2*(1-exp(-t/tau2))
        fit <- nlsLM(Norm_Intensity ~ plateau + a1*(1-exp(-Time/tau1)) + a2*(1-exp(-Time/tau2)),
                    data = fit_data,
                    start = list(a1 = 0.4, tau1 = 2, a2 = 0.4, tau2 = 20, plateau = 0.2),
                    lower = c(0, 0, 0, 0, 0),
                    upper = c(1, 1000, 1, 1000, 1.2),
                    control = nls.lm.control(maxiter = 500))
        
        params <- coef(fit)
        mobile_fraction <- params["a1"] + params["a2"]
        
        return(list(
          fit = fit,
          params = params,
          mobile_fraction = mobile_fraction,
          t_half = NA,
          t_half_fast = params["tau1"] * log(2),
          t_half_slow = params["tau2"] * log(2),
          model_type = "double",
          r_squared = 1 - sum(residuals(fit)^2) / sum((fit_data$Norm_Intensity - mean(fit_data$Norm_Intensity))^2)
        ))
      }
    }, error = function(e) {
      showNotification(paste("Fitting failed:", e$message), type = "error", duration = 5)
      return(NULL)
    })
  }
  
  # Fit individual sample
  observeEvent(input$fit_btn, {
    req(values$normalized_data, input$select_sample)
    
    if(!(input$select_sample %in% names(values$normalized_data))) {
      showNotification("Selected sample not found in normalized data", type = "error")
      return(NULL)
    }
    
    showNotification("Fitting curve...", type = "message")
    
    df <- values$normalized_data[[input$select_sample]]
    fit_result <- fit_exponential(df, input$fitting_eq, "single")
    
    if(!is.null(fit_result)) {
      values$fitted_models[[input$select_sample]] <- fit_result
      values$current_fit_data <- df
      
      # Update results display
      output$mobile_fraction <- renderPrint({ 
        cat("Mobile Fraction:", round(fit_result$mobile_fraction, 6))
      })
      
      if(fit_result$model_type == "single") {
        output$thalf <- renderPrint({ 
          cat("t-half:", round(fit_result$t_half, 2))
        })
      } else {
        output$thalf <- renderPrint({ 
          cat("t-half (fast):", round(fit_result$t_half_fast, 2), "\n")
          cat("t-half (slow):", round(fit_result$t_half_slow, 2))
        })
      }
      
      output$rsquare <- renderPrint({ 
        cat("R-square:", round(fit_result$r_squared, 5))
      })
      
      showNotification("Fit completed successfully!", type = "success")
    }
  })
  
  # Fit mean data
  observeEvent(input$fit_mean_btn, {
    req(values$mean_data)
    
    showNotification("Fitting mean data...", type = "message")
    
    fit_result <- fit_exponential(values$mean_data %>% 
                                   rename(Norm_Intensity = Mean_Intensity), 
                                   input$fitting_eq, "mean")
    
    if(!is.null(fit_result)) {
      values$fitted_models[["Mean_Data"]] <- fit_result
      values$current_fit_data <- values$mean_data %>%
        rename(Norm_Intensity = Mean_Intensity)
      
      output$mobile_fraction <- renderPrint({ 
        cat("Mobile Fraction (Mean):", round(fit_result$mobile_fraction, 6))
      })
      
      if(fit_result$model_type == "single") {
        output$thalf <- renderPrint({ 
          cat("t-half (Mean):", round(fit_result$t_half, 2))
        })
      } else {
        output$thalf <- renderPrint({ 
          cat("t-half fast (Mean):", round(fit_result$t_half_fast, 2), "\n")
          cat("t-half slow (Mean):", round(fit_result$t_half_slow, 2))
        })
      }
      
      output$rsquare <- renderPrint({ 
        cat("R-square (Mean):", round(fit_result$r_squared, 5))
      })
      
      showNotification("Mean data fit completed!", type = "success")
    }
  })
  
  # 9. Interactive Plot: Normalized Curves
  output$norm_curves_plot <- renderPlotly({
    req(length(values$normalized_data) > 0)
    
    selected_samples <- intersect(input$samples_display, names(values$normalized_data))
    selected_samples <- setdiff(selected_samples, values$excluded_samples)
    
    if(length(selected_samples) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers",
                         text = "No samples selected"))
    }
    
    plot_data <- bind_rows(lapply(selected_samples, function(name) {
      df <- values$normalized_data[[name]]
      df$Sample <- name
      return(df)
    }))
    
    # Apply color palette
    if(input$color_palette == "Viridis") {
      p <- ggplot(plot_data, aes(x = Time, y = Norm_Intensity, color = Sample)) +
        scale_color_viridis_d()
    } else if(input$color_palette == "Set1") {
      p <- ggplot(plot_data, aes(x = Time, y = Norm_Intensity, color = Sample)) +
        scale_color_brewer(palette = "Set1")
    } else if(input$color_palette == "Dark2") {
      p <- ggplot(plot_data, aes(x = Time, y = Norm_Intensity, color = Sample)) +
        scale_color_brewer(palette = "Dark2")
    } else {
      p <- ggplot(plot_data, aes(x = Time, y = Norm_Intensity, color = Sample))
    }
    
    p <- p +
      geom_line(size = input$line_size)
    
    if(input$show_points) {
      p <- p + geom_point(size = input$point_size, alpha = 0.6)
    }
    
    # Add mean curve if multiple samples
    if(length(selected_samples) > 1 && !is.null(values$mean_data)) {
      p <- p + 
        geom_line(data = values$mean_data, 
                 aes(x = Time, y = Mean_Intensity), 
                 color = "black", size = input$line_size + 0.5, 
                 linetype = "dashed", inherit.aes = FALSE) +
        geom_ribbon(data = values$mean_data,
                   aes(x = Time, ymin = Mean_Intensity - SD_Intensity, 
                       ymax = Mean_Intensity + SD_Intensity),
                   alpha = 0.2, fill = "gray", inherit.aes = FALSE)
    }
    
    p <- p +
      theme_minimal() +
      labs(title = paste("Normalized FRAP Curves -", input$exp_name),
           x = "Time (seconds)",
           y = "Normalized Intensity") +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "right"
      )
    
    ggplotly(p) %>% 
      layout(legend = list(orientation = "v", yanchor = "top", y = 1))
  })
  
  # 10. Interactive Plot: Fitting Results
  output$fitting_plot <- renderPlotly({
    req(values$fitted_models, values$current_fit_data)
    
    df <- values$current_fit_data
    model_name <- if(input$select_sample %in% names(values$fitted_models)) 
                    input$select_sample else "Mean_Data"
    fit_result <- values$fitted_models[[model_name]]
    
    if(is.null(fit_result)) {
      return(plotly_empty(type = "scatter", mode = "markers",
                         text = "No fit available"))
    }
    
    # Generate fitted curve
    fit_data <- data.frame(Time = seq(min(df$Time), max(df$Time), length.out = 100))
    
    if(fit_result$model_type == "single") {
      fit_data$Fitted <- fit_result$params["plateau"] + 
                        (1 - fit_result$params["plateau"]) * 
                        (1 - exp(-fit_data$Time/fit_result$params["tau"]))
    } else {
      fit_data$Fitted <- fit_result$params["plateau"] + 
                        fit_result$params["a1"] * (1 - exp(-fit_data$Time/fit_result$params["tau1"])) +
                        fit_result$params["a2"] * (1 - exp(-fit_data$Time/fit_result$params["tau2"]))
    }
    
    p <- ggplot() +
      geom_point(data = df, aes(x = Time, y = Norm_Intensity), 
                alpha = 0.6, size = 2, color = "darkgray") +
      geom_line(data = fit_data, aes(x = Time, y = Fitted), 
                color = "#E74C3C", size = 1.2) +
    theme_minimal() +
      labs(title = paste("Curve Fitting:", model_name),
           x = "Time Post-Bleach (seconds)",
           y = "Normalized Intensity") +
      annotate("text", x = max(df$Time) * 0.7, y = max(df$Norm_Intensity) * 0.9,
               label = paste("R² =", round(fit_result$r_squared, 4)),
               size = 5, fontface = "bold")
    
    if(input$show_confidence && fit_result$model_type == "single") {
      # Add confidence interval (simplified)
      se <- sqrt(diag(vcov(fit_result$fit)))
      ci <- qt(input$ci_level, df.residual(fit_result$fit)) * se[2]
      fit_data$Upper <- fit_data$Fitted + ci
      fit_data$Lower <- fit_data$Fitted - ci
      p <- p + geom_ribbon(data = fit_data, aes(x = Time, ymin = Lower, ymax = Upper),
                           alpha = 0.2, fill = "red", inherit.aes = FALSE)
    }
    
    ggplotly(p)
    })
  
  # 11. Data Tables with DT for better formatting
  output$raw_data_table <- renderDT({
    req(length(values$raw_data) > 0)
    selected_samples <- intersect(input$samples_display, names(values$raw_data))
    
    if(length(selected_samples) == 0) {
      return(datatable(data.frame(Message = "No samples selected"), 
                       options = list(dom = 't')))
    }
    
    # Show first selected sample
    df <- values$raw_data[[selected_samples[1]]]
    datatable(df, 
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip'
              ),
              caption = paste("Raw data for:", selected_samples[1]))
  })
  
  output$summary_stats <- renderDT({
    req(length(values$normalized_data) > 0)
    selected_samples <- intersect(input$samples_display, names(values$normalized_data))
    selected_samples <- setdiff(selected_samples, values$excluded_samples)
    
    if(length(selected_samples) == 0) {
      return(datatable(data.frame(Message = "No samples selected"), 
                       options = list(dom = 't')))
    }
    
    summary_list <- lapply(selected_samples, function(name) {
      df <- values$normalized_data[[name]]
      bleach_idx <- which.min(df$Norm_Intensity)
      
      data.frame(
        Sample = name,
        Max_Recovery = round(max(df$Norm_Intensity, na.rm = TRUE), 4),
        Min_Bleach = round(min(df$Norm_Intensity, na.rm = TRUE), 4),
        Recovery_Level = round(max(df$Norm_Intensity) / 
                                 mean(df$Norm_Intensity[1:min(input$pre_bleach, nrow(df))]), 4),
        Time_to_Half = if(!is.null(values$fitted_models[[name]])) {
          if(values$fitted_models[[name]]$model_type == "single") {
            round(values$fitted_models[[name]]$t_half, 2)
          } else {
            round(values$fitted_models[[name]]$t_half_fast, 2)
          }
        } else NA
      )
    })
    
    summary_df <- do.call(rbind, summary_list)
    datatable(summary_df, 
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip'
              ),
              rownames = FALSE)
  })
  
  # 12. Batch Results Table - FIX #2: Check model parameters, not radio button
  output$batch_results <- renderDT({
    req(length(values$fitted_models) > 0)
    
    batch_df <- do.call(rbind, lapply(names(values$fitted_models), function(name) {
      fit <- values$fitted_models[[name]]
      
      # Check model_type stored in the fit result, not the current radio button
      if(fit$model_type == "single") {
        data.frame(
          Sample = name,
          Model_Type = "Single Exponential",
          Mobile_Fraction = round(fit$mobile_fraction, 4),
          t_half_1 = round(fit$t_half, 2),
          t_half_2 = NA,
          R_squared = round(fit$r_squared, 4)
        )
      } else {
        data.frame(
          Sample = name,
          Model_Type = "Double Exponential",
          Mobile_Fraction = round(fit$mobile_fraction, 4),
          t_half_1 = round(fit$t_half_fast, 2),
          t_half_2 = round(fit$t_half_slow, 2),
          R_squared = round(fit$r_squared, 4)
        )
      }
    }))
    
    datatable(batch_df, 
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip'
              ),
              rownames = FALSE)
  })
  
  # 13. Download Results
  output$save_btn <- downloadHandler(
    filename = function() {
      paste0("FRAP_results_", input$exp_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(length(values$fitted_models) > 0) {
        results_df <- do.call(rbind, lapply(names(values$fitted_models), function(name) {
          fit <- values$fitted_models[[name]]
          if(fit$model_type == "single") {
            data.frame(
              Sample = name,
              Model_Type = "Single",
              Mobile_Fraction = fit$mobile_fraction,
              t_half = fit$t_half,
              R_squared = fit$r_squared,
              Bleaching_Depth = values$bleach_params$bleaching_depth,
              Gap_Ratio = values$bleach_params$gap_ratio
            )
          } else {
            data.frame(
              Sample = name,
              Model_Type = "Double",
              Mobile_Fraction = fit$mobile_fraction,
              t_half_fast = fit$t_half_fast,
              t_half_slow = fit$t_half_slow,
              R_squared = fit$r_squared,
              Bleaching_Depth = values$bleach_params$bleaching_depth,
              Gap_Ratio = values$bleach_params$gap_ratio
            )
          }
        }))
        write.csv(results_df, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Message = "No fitting results available"), file, row.names = FALSE)
      }
    }
  )
  
  # 14. Download batch results - FIX #2 also applied here
  output$download_batch <- downloadHandler(
    filename = function() {
      paste0("FRAP_batch_results_", input$exp_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(length(values$fitted_models) > 0) {
        batch_df <- do.call(rbind, lapply(names(values$fitted_models), function(name) {
          fit <- values$fitted_models[[name]]
          if(fit$model_type == "single") {
            data.frame(
              Sample = name,
              Model_Type = "Single",
              Mobile_Fraction = fit$mobile_fraction,
              t_half = fit$t_half,
              R_squared = fit$r_squared
            )
          } else {
            data.frame(
              Sample = name,
              Model_Type = "Double",
              Mobile_Fraction = fit$mobile_fraction,
              t_half_fast = fit$t_half_fast,
              t_half_slow = fit$t_half_slow,
              R_squared = fit$r_squared
            )
          }
        }))
        write.csv(batch_df, file, row.names = FALSE)
      }
    }
  )
  }

shinyApp(ui = ui, server = server)
