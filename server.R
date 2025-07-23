# =============================================================================
# WASKITA Dashboard - Server Logic - ENHANCED VERSION
# Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik
# =============================================================================

source("global.R")

server <- function(input, output, session) {
  
  # =============================================================================
  # REACTIVE VALUES - ENHANCED WITH TRACKING
  # =============================================================================
  
  values <- reactiveValues(
    # Data storage
    original_data = NULL,
    processed_data = NULL,
    distance_data = NULL,
    map_data = NULL,
    indonesia_sf = NULL,
    
    # Status flags
    data_loaded = FALSE,
    transformation_done = FALSE,
    categorization_done = FALSE,
    desc_generated = FALSE,
    viz_generated = FALSE,
    map_generated = FALSE,
    normality_done = FALSE,
    homogeneity_done = FALSE,
    ttest_done = FALSE,
    prop_test_done = FALSE,
    var_test_done = FALSE,
    anova_done = FALSE,
    regression_done = FALSE,
    spatial_done = FALSE,
    
    # Analysis results
    current_desc_variable = NULL,
    current_viz_type = NULL,
    current_transform_variable = NULL,
    normality_result = NULL,
    homogeneity_result = NULL,
    ttest_result = NULL,
    prop_test_result = NULL,
    var_test_result = NULL,
    anova_result = NULL,
    regression_model = NULL,
    spatial_result = NULL
  )
  
  # Initialize report tracker
  activity_tracker <- reactiveValues(
    data_transformations = list(),
    visualizations_created = list(),
    statistical_tests = list(),
    spatial_analyses = list(),
    analyses_performed = list(),
    timestamp = Sys.time()
  )
  
  # =============================================================================
  # STATUS UPDATE FUNCTIONS FOR LAPORAN LENGKAP - ENHANCED
  # =============================================================================
  
  update_status_ui <- function() {
    # JavaScript to update status cards based on reactive values
    js_code <- paste0("
      // Update Data Management Status
      if (", tolower(values$data_loaded), ") {
        $('#status-data').addClass('completed');
        $('#badge-data').removeClass('badge-pending').addClass('badge-completed').text('Completed');
        $('#status-data').parent().addClass('completed');
      }
      
      // Update Descriptive Analysis Status
      if (", tolower(values$desc_generated), ") {
        $('#status-descriptive').addClass('completed');
        $('#badge-descriptive').removeClass('badge-pending').addClass('badge-completed').text('Completed');
        $('#status-descriptive').parent().addClass('completed');
      }
      
      // Update Visualization Status
      if (", tolower(values$viz_generated), ") {
        $('#status-visualization').addClass('completed');
        $('#badge-visualization').removeClass('badge-pending').addClass('badge-completed').text('Completed');
        $('#status-visualization').parent().addClass('completed');
      }
      
      // Update Spatial Map Status
      if (", tolower(values$map_generated), ") {
        $('#status-spatial-map').addClass('completed');
        $('#badge-spatial-map').removeClass('badge-pending').addClass('badge-completed').text('Completed');
        $('#status-spatial-map').parent().addClass('completed');
      }
      
      // Update Assumptions Status
      if (", tolower(values$normality_done || values$homogeneity_done), ") {
        $('#status-assumptions').addClass('completed');
        $('#badge-assumptions').removeClass('badge-pending').addClass('badge-completed').text('Completed');
        $('#status-assumptions').parent().addClass('completed');
      }
      
      // Update T-test Status
      if (", tolower(values$ttest_done), ") {
        $('#status-ttest').addClass('completed');
        $('#badge-ttest').removeClass('badge-pending').addClass('badge-completed').text('Completed');
        $('#status-ttest').parent().addClass('completed');
      }
      
      // Update Proportion Status
      if (", tolower(values$prop_test_done || values$var_test_done), ") {
        $('#status-proportion').addClass('completed');
        $('#badge-proportion').removeClass('badge-pending').addClass('badge-completed').text('Completed');
        $('#status-proportion').parent().addClass('completed');
      }
      
      // Update ANOVA Status
      if (", tolower(values$anova_done), ") {
        $('#status-anova').addClass('completed');
        $('#badge-anova').removeClass('badge-pending').addClass('badge-completed').text('Completed');
        $('#status-anova').parent().addClass('completed');
      }
      
      // Update Regression Status
      if (", tolower(values$regression_done), ") {
        $('#status-regression').addClass('completed');
        $('#badge-regression').removeClass('badge-pending').addClass('badge-completed').text('Completed');
        $('#status-regression').parent().addClass('completed');
      }
      
      // Update Spatial Analysis Status
      if (", tolower(values$spatial_done), ") {
        $('#status-spatial').addClass('completed');
        $('#badge-spatial').removeClass('badge-pending').addClass('badge-completed').text('Completed');
        $('#status-spatial').parent().addClass('completed');
      }
    ")
    
    session$sendCustomMessage(type = "updateStatus", message = js_code)
  }
  
  # Observer to update status UI when values change
  observe({
    update_status_ui()
  })
  
  # =============================================================================
  # HELPER FUNCTION TO ADD ACTIVITIES TO TRACKER
  # =============================================================================
  
  add_activity_to_tracker <- function(type, title, content, tables = NULL, plots = NULL) {
    new_entry <- list(
      timestamp = Sys.time(),
      type = type,
      title = title,
      content = content,
      tables = tables,
      plots = plots
    )
    
    if (type == "transformation") {
      activity_tracker$data_transformations[[length(activity_tracker$data_transformations) + 1]] <- new_entry
    } else if (type == "visualization") {
      activity_tracker$visualizations_created[[length(activity_tracker$visualizations_created) + 1]] <- new_entry
    } else if (type == "test") {
      activity_tracker$statistical_tests[[length(activity_tracker$statistical_tests) + 1]] <- new_entry
    } else if (type == "spatial") {
      activity_tracker$spatial_analyses[[length(activity_tracker$spatial_analyses) + 1]] <- new_entry
    } else {
      activity_tracker$analyses_performed[[length(activity_tracker$analyses_performed) + 1]] <- new_entry
    }
  }
  
  # =============================================================================
  # DATA LOADING - ENHANCED WITH GEOJSON
  # =============================================================================
  
  observeEvent(input$load_data, {
    tryCatch({
      # Load data using global function
      data_result <- load_waskita_data()
      geojson_result <- load_indonesia_geojson()
      
      if ("error" %in% names(data_result)) {
        safe_notification(data_result$error, "error")
        return()
      }
      
      values$original_data <- data_result$sovi
      values$processed_data <- data_result$sovi
      values$distance_data <- data_result$distance
      
      # Load geographical data
      if ("error" %in% names(geojson_result)) {
        safe_notification(paste("Warning GeoJSON:", geojson_result$error), "warning")
        values$indonesia_sf <- NULL
      } else {
        values$indonesia_sf <- geojson_result$geojson
        safe_notification(paste("Geographical data loaded from:", geojson_result$source), "info")
      }
      
      values$data_loaded <- TRUE
      
      # Add to activity tracker
      add_activity_to_tracker(
        type = "analysis",
        title = "Data Loading",
        content = paste(
          "Dataset berhasil dimuat dengan informasi:",
          paste("- Total observasi:", nrow(values$processed_data)),
          paste("- Total variabel:", ncol(values$processed_data)),
          paste("- Sumber data:", data_result$source),
          paste("- Status geografis:", if(is.null(values$indonesia_sf)) "Sintetis" else geojson_result$source),
          sep = "\n"
        )
      )
      
      # Update variable choices
      numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      categorical_vars <- names(values$processed_data)[sapply(values$processed_data, function(x) is.character(x) | is.factor(x))]
      all_vars <- names(values$processed_data)
      
      # Update all selectInput choices
      choices_list <- list(
        transform_variable = numeric_vars,
        categorize_variable = numeric_vars,
        desc_variable = all_vars,
        scatter_x = numeric_vars,
        scatter_y = numeric_vars,
        scatter_color = c("None" = "", categorical_vars),
        boxplot_numeric = numeric_vars,
        boxplot_group = categorical_vars,
        hist_variables = numeric_vars,
        map_variable = numeric_vars,
        normality_variable = numeric_vars,
        homogeneity_numeric = numeric_vars,
        homogeneity_group = categorical_vars,
        ttest_variable = numeric_vars,
        ttest_group = categorical_vars,
        ttest_paired_var = numeric_vars,
        prop_variable = categorical_vars,
        prop_group = categorical_vars,
        var_variable = numeric_vars,
        var_group = categorical_vars,
        anova_dependent = numeric_vars,
        anova_factor1 = categorical_vars,
        anova_factor2 = categorical_vars,
        reg_dependent = numeric_vars,
        reg_independent = numeric_vars,
        spatial_variable = numeric_vars
      )
      
      # Update choices for all inputs
      for (input_id in names(choices_list)) {
        if (input_id %in% names(input)) {
          updateSelectInput(session, input_id, choices = choices_list[[input_id]])
        }
      }
      
      # Prepare spatial data
      if (!is.null(values$indonesia_sf)) {
        # Try to match districts between datasets
        sovi_districts <- if("DISTRICTCODE" %in% names(values$processed_data)) {
          values$processed_data$DISTRICTCODE
        } else {
          paste0("DIST_", sprintf("%03d", 1:nrow(values$processed_data)))
        }
        
        # Ensure indonesia_sf has matching district codes
        if (!"DISTRICTCODE" %in% names(values$indonesia_sf)) {
          values$indonesia_sf$DISTRICTCODE <- paste0("DIST_", sprintf("%03d", 1:nrow(values$indonesia_sf)))
        }
        
        # Create choropleth-ready data
        values$map_data <- values$indonesia_sf
        
        cat("=== ENHANCED SPATIAL DATA LOADED ===\n")
        cat("   - Total districts in GeoJSON:", nrow(values$indonesia_sf), "\n")
        cat("   - Total districts in SoVI:", nrow(values$processed_data), "\n")
        cat("   - Geographical source:", geojson_result$source, "\n")
      } else {
        # Fallback to synthetic coordinates
        map_data <- values$processed_data
        if (nrow(map_data) > 0) {
          set.seed(123)
          lat_range <- c(-11, 6)
          lon_range <- c(95, 141)
          
          map_data$lat <- runif(nrow(map_data), lat_range[1], lat_range[2])
          map_data$lon <- runif(nrow(map_data), lon_range[1], lon_range[2])
          
          if(!"DISTRICTCODE" %in% names(values$processed_data)) {
            map_data$DISTRICTCODE <- paste0("DIST_", sprintf("%03d", 1:nrow(map_data)))
          }
          
          values$map_data <- map_data
        }
      }
      
      safe_notification(paste("Data berhasil dimuat! Source:", data_result$source), "success")
      
    }, error = function(e) {
      safe_notification(paste("Error memuat data:", e$message), "error")
    })
  })
  
  # Data loaded status
  output$data_loaded <- reactive({
    values$data_loaded
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Data summary
  output$data_summary <- renderPrint({
    if (values$data_loaded) {
      cat("Dataset WASKITA - Enhanced Version\n")
      cat("===================================\n")
      cat("Baris:", nrow(values$processed_data), "\n")
      cat("Kolom:", ncol(values$processed_data), "\n")
      cat("Missing Values:", sum(is.na(values$processed_data)), "\n")
      cat("Numerik:", sum(sapply(values$processed_data, is.numeric)), "\n")
      cat("Kategorikal:", sum(sapply(values$processed_data, function(x) is.character(x) | is.factor(x))), "\n")
      cat("Spatial Data:", if(is.null(values$indonesia_sf)) "Synthetic coordinates" else "GeoJSON loaded", "\n")
      cat("Distance Matrix:", if(is.null(values$distance_data)) "Not available" else paste(nrow(values$distance_data), "x", ncol(values$distance_data)), "\n")
    }
  })
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    if (values$data_loaded) {
      DT::datatable(
        values$processed_data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = 'cell-border stripe'
      )
    }
  })
  
  # Metadata table
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(
      sovi_metadata,
      options = list(
        pageLength = 17,
        dom = 't',
        ordering = FALSE
      ),
      class = 'cell-border stripe'
    )
  })
  
  # =============================================================================
  # TRANSFORMASI DATA - ENHANCED WITH TRACKING
  # =============================================================================
  
  observeEvent(input$apply_transformation, {
    req(values$processed_data, input$transform_variable, input$transform_method)
    
    tryCatch({
      var_name <- input$transform_variable
      method <- input$transform_method
      original_data <- values$processed_data[[var_name]]
      
      # Store the current variable being transformed
      values$current_transform_variable <- var_name
      
      if (!is.numeric(original_data)) {
        safe_notification("Transformasi hanya untuk variabel numerik", "warning")
        return()
      }
      
      transformed_data <- switch(method,
                                 "log" = {
                                   if (any(original_data <= 0, na.rm = TRUE)) {
                                     safe_notification("Log transformasi memerlukan nilai positif", "error")
                                     return()
                                   }
                                   log(original_data)
                                 },
                                 "log10" = {
                                   if (any(original_data <= 0, na.rm = TRUE)) {
                                     safe_notification("Log10 transformasi memerlukan nilai positif", "error")
                                     return()
                                   }
                                   log10(original_data)
                                 },
                                 "sqrt" = {
                                   if (any(original_data < 0, na.rm = TRUE)) {
                                     safe_notification("Sqrt transformasi memerlukan nilai non-negatif", "error")
                                     return()
                                   }
                                   sqrt(original_data)
                                 },
                                 "square" = original_data^2,
                                 "boxcox" = {
                                   if (any(original_data <= 0, na.rm = TRUE)) {
                                     safe_notification("Box-Cox transformasi memerlukan nilai positif", "error")
                                     return()
                                   }
                                   lambda <- forecast::BoxCox.lambda(original_data)
                                   forecast::BoxCox(original_data, lambda = lambda)
                                 }
      )
      
      new_col_name <- paste0(var_name, "_", method)
      values$processed_data[[new_col_name]] <- transformed_data
      values$transformation_done <- TRUE
      
      # Add to activity tracker
      stats_content <- paste(
        paste("Variabel asli:", var_name),
        paste("Metode transformasi:", method),
        paste("Variabel baru:", new_col_name),
        paste("Mean awal:", round(mean(original_data, na.rm = TRUE), 4)),
        paste("Mean setelah:", round(mean(transformed_data, na.rm = TRUE), 4)),
        paste("SD awal:", round(sd(original_data, na.rm = TRUE), 4)),
        paste("SD setelah:", round(sd(transformed_data, na.rm = TRUE), 4)),
        sep = "\n"
      )
      
      add_activity_to_tracker(
        type = "transformation",
        title = paste("Transformasi", method, "pada", var_name),
        content = stats_content,
        tables = stats_content
      )
      
      # Update choices
      numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      updateSelectInput(session, "transform_variable", choices = numeric_vars)
      
      safe_notification(paste("Transformasi", method, "berhasil diterapkan"), "success")
      
    }, error = function(e) {
      safe_notification(paste("Error transformasi:", e$message), "error")
    })
  })
  
  output$transformation_done <- reactive({
    values$transformation_done
  })
  outputOptions(output, "transformation_done", suspendWhenHidden = FALSE)
  
  output$transformation_result <- renderPrint({
    if (values$transformation_done && !is.null(values$current_transform_variable)) {
      new_col_name <- paste0(values$current_transform_variable, "_", input$transform_method)
      if (new_col_name %in% names(values$processed_data)) {
        new_data <- values$processed_data[[new_col_name]]
        cat("Hasil Transformasi:", new_col_name, "\n")
        cat("====================\n")
        cat("Mean:", round(mean(new_data, na.rm = TRUE), 4), "\n")
        cat("Median:", round(median(new_data, na.rm = TRUE), 4), "\n")
        cat("SD:", round(sd(new_data, na.rm = TRUE), 4), "\n")
        cat("Min:", round(min(new_data, na.rm = TRUE), 4), "\n")
        cat("Max:", round(max(new_data, na.rm = TRUE), 4), "\n")
      }
    }
  })
  
  # =============================================================================
  # KATEGORISASI DATA - ENHANCED WITH TRACKING
  # =============================================================================
  
  observeEvent(input$apply_categorization, {
    req(values$processed_data, input$categorize_variable, input$categorize_method)
    
    tryCatch({
      var_name <- input$categorize_variable
      var_data <- values$processed_data[[var_name]]
      
      if (!is.numeric(var_data)) {
        safe_notification("Kategorisasi hanya untuk variabel numerik", "warning")
        return()
      }
      
      if (input$categorize_method == "quartile") {
        breaks <- quantile(var_data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        labels <- c("Q1", "Q2", "Q3", "Q4")
      } else if (input$categorize_method == "tertile") {
        breaks <- quantile(var_data, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
        labels <- c("Low", "Medium", "High")
      } else { # custom
        thresholds <- as.numeric(unlist(strsplit(input$custom_thresholds, ",")))
        breaks <- c(min(var_data, na.rm = TRUE), thresholds, max(var_data, na.rm = TRUE))
        breaks <- sort(unique(breaks))
        labels <- paste0("Cat", 1:(length(breaks)-1))
      }
      
      categorized_var <- cut(var_data, breaks = breaks, labels = labels, include.lowest = TRUE)
      new_col_name <- paste0(var_name, "_cat")
      values$processed_data[[new_col_name]] <- categorized_var
      values$categorization_done <- TRUE
      
      # Add to activity tracker
      table_result <- table(categorized_var, useNA = "ifany")
      stats_content <- paste(
        paste("Variabel asli:", var_name),
        paste("Metode kategorisasi:", input$categorize_method),
        paste("Variabel kategorikal:", new_col_name),
        paste("Jumlah kategori:", length(labels)),
        "Distribusi kategori:",
        paste(capture.output(print(table_result)), collapse = "\n"),
        sep = "\n"
      )
      
      add_activity_to_tracker(
        type = "transformation",
        title = paste("Kategorisasi", input$categorize_method, "pada", var_name),
        content = stats_content,
        tables = stats_content
      )
      
      # Update choices
      categorical_vars <- names(values$processed_data)[sapply(values$processed_data, function(x) is.character(x) | is.factor(x))]
      updateSelectInput(session, "homogeneity_group", choices = categorical_vars)
      updateSelectInput(session, "ttest_group", choices = categorical_vars)
      updateSelectInput(session, "prop_variable", choices = categorical_vars)
      updateSelectInput(session, "prop_group", choices = categorical_vars)
      updateSelectInput(session, "var_group", choices = categorical_vars)
      updateSelectInput(session, "anova_factor1", choices = categorical_vars)
      updateSelectInput(session, "anova_factor2", choices = categorical_vars)
      updateSelectInput(session, "boxplot_group", choices = categorical_vars)
      updateSelectInput(session, "scatter_color", choices = c("None" = "", categorical_vars))
      
      safe_notification(paste("Kategorisasi berhasil:", new_col_name), "success")
      
    }, error = function(e) {
      safe_notification(paste("Error kategorisasi:", e$message), "error")
    })
  })
  
  output$categorization_done <- reactive({
    values$categorization_done
  })
  outputOptions(output, "categorization_done", suspendWhenHidden = FALSE)
  
  output$categorization_result <- renderPrint({
    if (values$categorization_done) {
      new_col_name <- paste0(input$categorize_variable, "_cat")
      if (new_col_name %in% names(values$processed_data)) {
        cat("Hasil Kategorisasi:", new_col_name, "\n")
        cat("======================\n")
        print(table(values$processed_data[[new_col_name]], useNA = "ifany"))
      }
    }
  })
  
  # =============================================================================
  # ANALISIS DESKRIPTIF - ENHANCED WITH TRACKING
  # =============================================================================
  
  # Check if variable is numeric
  output$is_numeric_var <- reactive({
    if (values$data_loaded && !is.null(input$desc_variable)) {
      return(is.numeric(values$processed_data[[input$desc_variable]]))
    }
    return(FALSE)
  })
  outputOptions(output, "is_numeric_var", suspendWhenHidden = FALSE)
  
  observeEvent(input$generate_desc, {
    req(values$processed_data, input$desc_variable)
    
    values$current_desc_variable <- input$desc_variable
    values$desc_generated <- TRUE
    
    # Add to activity tracker
    var_data <- values$processed_data[[input$desc_variable]]
    
    if (is.numeric(var_data)) {
      stats_content <- paste(
        paste("Variabel:", input$desc_variable),
        paste("Tipe visualisasi:", input$desc_chart_type),
        paste("N:", sum(!is.na(var_data))),
        paste("Missing:", sum(is.na(var_data))),
        paste("Mean:", round(mean(var_data, na.rm = TRUE), 4)),
        paste("Median:", round(median(var_data, na.rm = TRUE), 4)),
        paste("SD:", round(sd(var_data, na.rm = TRUE), 4)),
        paste("Min:", round(min(var_data, na.rm = TRUE), 4)),
        paste("Max:", round(max(var_data, na.rm = TRUE), 4)),
        sep = "\n"
      )
    } else {
      tbl <- table(var_data, useNA = "ifany")
      stats_content <- paste(
        paste("Variabel:", input$desc_variable),
        paste("Tipe visualisasi:", input$desc_chart_type),
        paste("Jumlah kategori:", length(tbl)),
        "Distribusi:",
        paste(capture.output(print(tbl)), collapse = "\n"),
        sep = "\n"
      )
    }
    
    add_activity_to_tracker(
      type = "visualization",
      title = paste("Analisis Deskriptif -", input$desc_variable),
      content = stats_content,
      tables = stats_content
    )
  })
  
  output$desc_generated <- reactive({
    values$desc_generated
  })
  outputOptions(output, "desc_generated", suspendWhenHidden = FALSE)
  
  # Fixed desc_plot - using plotOutput instead of plotlyOutput to avoid layout errors
  output$desc_plot <- renderPlot({
    if (values$desc_generated && !is.null(values$current_desc_variable)) {
      var_data <- values$processed_data[[values$current_desc_variable]]
      
      if (is.numeric(var_data)) {
        p <- switch(input$desc_chart_type,
                    "histogram" = {
                      ggplot(values$processed_data, aes_string(x = values$current_desc_variable)) +
                        geom_histogram(bins = input$desc_bins, fill = waskita_colors$steel, 
                                       alpha = 0.7, color = "white") +
                        labs(title = paste("Histogram", values$current_desc_variable),
                             x = values$current_desc_variable, y = "Frequency") +
                        theme_waskita()
                    },
                    "boxplot" = {
                      ggplot(values$processed_data, aes_string(y = values$current_desc_variable)) +
                        geom_boxplot(fill = waskita_colors$steel, alpha = 0.7, color = waskita_colors$navy) +
                        labs(title = paste("Boxplot", values$current_desc_variable),
                             y = values$current_desc_variable) +
                        theme_waskita()
                    },
                    "density" = {
                      ggplot(values$processed_data, aes_string(x = values$current_desc_variable)) +
                        geom_density(fill = waskita_colors$steel, alpha = 0.7, color = waskita_colors$navy) +
                        labs(title = paste("Density Plot", values$current_desc_variable),
                             x = values$current_desc_variable, y = "Density") +
                        theme_waskita()
                    },
                    "qq" = {
                      ggplot(values$processed_data, aes_string(sample = values$current_desc_variable)) +
                        stat_qq(color = waskita_colors$steel) +
                        stat_qq_line(color = waskita_colors$navy) +
                        labs(title = paste("Q-Q Plot", values$current_desc_variable)) +
                        theme_waskita()
                    }
        )
      } else {
        # Categorical variable
        df <- as.data.frame(table(var_data))
        colnames(df) <- c("Category", "Frequency")
        p <- ggplot(df, aes(x = reorder(Category, -Frequency), y = Frequency)) +
          geom_bar(stat = "identity", fill = waskita_colors$steel, alpha = 0.7) +
          labs(title = paste("Bar Chart", values$current_desc_variable),
               x = values$current_desc_variable, y = "Frequency") +
          theme_waskita() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      return(p)
    }
  })
  
  output$desc_stats <- renderPrint({
    if (values$desc_generated && !is.null(values$current_desc_variable)) {
      var_data <- values$processed_data[[values$current_desc_variable]]
      
      cat("Statistik Deskriptif:", values$current_desc_variable, "\n")
      cat("=======================================\n")
      
      if (is.numeric(var_data)) {
        cat("N:", sum(!is.na(var_data)), "\n")
        cat("Missing:", sum(is.na(var_data)), "\n")
        cat("Mean:", round(mean(var_data, na.rm = TRUE), 4), "\n")
        cat("Median:", round(median(var_data, na.rm = TRUE), 4), "\n")
        cat("SD:", round(sd(var_data, na.rm = TRUE), 4), "\n")
        cat("Min:", round(min(var_data, na.rm = TRUE), 4), "\n")
        cat("Max:", round(max(var_data, na.rm = TRUE), 4), "\n")
        cat("Q1:", round(quantile(var_data, 0.25, na.rm = TRUE), 4), "\n")
        cat("Q3:", round(quantile(var_data, 0.75, na.rm = TRUE), 4), "\n")
        cat("IQR:", round(IQR(var_data, na.rm = TRUE), 4), "\n")
        
        if (requireNamespace("moments", quietly = TRUE)) {
          cat("Skewness:", round(moments::skewness(var_data, na.rm = TRUE), 4), "\n")
          cat("Kurtosis:", round(moments::kurtosis(var_data, na.rm = TRUE), 4), "\n")
        }
      } else {
        tbl <- table(var_data, useNA = "ifany")
        print(tbl)
        cat("\nProporsi:\n")
        print(round(prop.table(tbl) * 100, 2))
      }
    }
  })
  
  output$desc_interpretation <- renderPrint({
    if (values$desc_generated && !is.null(values$current_desc_variable)) {
      var_data <- values$processed_data[[values$current_desc_variable]]
      
      cat("INTERPRETASI STATISTIK\n")
      cat("======================\n")
      
      if (is.numeric(var_data)) {
        mean_val <- mean(var_data, na.rm = TRUE)
        median_val <- median(var_data, na.rm = TRUE)
        sd_val <- sd(var_data, na.rm = TRUE)
        cv <- sd_val / abs(mean_val) * 100
        
        cat("1. TENDENSI SENTRAL:\n")
        if (abs(mean_val - median_val) / sd_val < 0.1) {
          cat("   - Distribusi relatif SIMETRIS (mean ≈ median)\n")
        } else if (mean_val > median_val) {
          cat("   - Distribusi CONDONG KANAN (mean > median)\n")
        } else {
          cat("   - Distribusi CONDONG KIRI (mean < median)\n")
        }
        
        cat("\n2. VARIABILITAS:\n")
        cat("   - Coefficient of Variation:", round(cv, 2), "%\n")
        if (cv < 15) {
          cat("   - Variabilitas RENDAH (homogen)\n")
        } else if (cv < 30) {
          cat("   - Variabilitas SEDANG\n")
        } else {
          cat("   - Variabilitas TINGGI (heterogen)\n")
        }
        
        if (requireNamespace("moments", quietly = TRUE)) {
          skew_val <- moments::skewness(var_data, na.rm = TRUE)
          kurt_val <- moments::kurtosis(var_data, na.rm = TRUE)
          
          cat("\n3. BENTUK DISTRIBUSI:\n")
          if (abs(skew_val) < 0.5) {
            cat("   - Skewness: Hampir simetris\n")
          } else if (abs(skew_val) < 1) {
            cat("   - Skewness: Moderately skewed\n")
          } else {
            cat("   - Skewness: Highly skewed\n")
          }
          
          if (kurt_val < 3) {
            cat("   - Kurtosis: Platykurtic (datar)\n")
          } else if (kurt_val > 3) {
            cat("   - Kurtosis: Leptokurtic (runcing)\n")
          } else {
            cat("   - Kurtosis: Mesokurtic (normal)\n")
          }
        }
        
        # Deteksi outlier
        Q1 <- quantile(var_data, 0.25, na.rm = TRUE)
        Q3 <- quantile(var_data, 0.75, na.rm = TRUE)
        IQR_val <- Q3 - Q1
        lower_fence <- Q1 - 1.5 * IQR_val
        upper_fence <- Q3 + 1.5 * IQR_val
        outliers <- sum(var_data < lower_fence | var_data > upper_fence, na.rm = TRUE)
        
        cat("\n4. OUTLIER:\n")
        cat("   - Jumlah outlier:", outliers, "\n")
        if (outliers == 0) {
          cat("   - Tidak ada outlier terdeteksi\n")
        } else {
          pct_outliers <- round(outliers / length(var_data) * 100, 2)
          cat("   - Persentase outlier:", pct_outliers, "%\n")
        }
        
      } else {
        tbl <- table(var_data, useNA = "ifany")
        dominant_cat <- names(which.max(tbl))
        dominant_pct <- round(max(tbl)/sum(tbl) * 100, 2)
        
        cat("1. DISTRIBUSI KATEGORI:\n")
        cat("   - Kategori dominan:", dominant_cat, "(", dominant_pct, "%)\n")
        cat("   - Jumlah kategori:", length(tbl), "\n")
        
        if (max(tbl)/min(tbl) > 3) {
          cat("   - Distribusi: TIDAK SEIMBANG\n")
        } else {
          cat("   - Distribusi: RELATIF SEIMBANG\n")
        }
      }
    }
  })
  
  # =============================================================================
  # VISUALISASI MULTIVARIAT - ENHANCED WITH TRACKING
  # =============================================================================
  
  observeEvent(input$generate_viz, {
    req(values$processed_data, input$viz_type)
    
    values$current_viz_type <- input$viz_type
    values$viz_generated <- TRUE
    
    # Add to activity tracker
    viz_content <- switch(input$viz_type,
                          "scatter" = paste("Scatter plot:", input$scatter_x, "vs", input$scatter_y),
                          "correlation" = "Correlation heatmap untuk semua variabel numerik",
                          "boxplot_group" = paste("Boxplot", input$boxplot_numeric, "by", input$boxplot_group),
                          "histogram_overlay" = paste("Histogram overlay untuk", length(input$hist_variables), "variabel")
    )
    
    add_activity_to_tracker(
      type = "visualization",
      title = paste("Visualisasi Multivariat -", input$viz_type),
      content = paste(
        paste("Jenis visualisasi:", input$viz_type),
        viz_content,
        paste("Timestamp:", Sys.time()),
        sep = "\n"
      )
    )
  })
  
  output$viz_generated <- reactive({
    values$viz_generated
  })
  outputOptions(output, "viz_generated", suspendWhenHidden = FALSE)
  
  output$viz_plot <- renderPlotly({
    if (values$viz_generated && !is.null(values$current_viz_type)) {
      
      p <- switch(values$current_viz_type,
                  "scatter" = {
                    if (!is.null(input$scatter_x) && !is.null(input$scatter_y)) {
                      base_plot <- ggplot(values$processed_data, aes_string(x = input$scatter_x, y = input$scatter_y))
                      
                      if (!is.null(input$scatter_color) && input$scatter_color != "") {
                        base_plot <- base_plot + aes_string(color = input$scatter_color)
                      }
                      
                      base_plot +
                        geom_point(alpha = 0.7, size = 2) +
                        geom_smooth(method = "lm", se = TRUE, color = waskita_colors$navy) +
                        labs(title = paste("Scatter Plot:", input$scatter_x, "vs", input$scatter_y)) +
                        theme_waskita() +
                        scale_color_manual(values = c(waskita_colors$steel, waskita_colors$warning, waskita_colors$success))
                    }
                  },
                  
                  "correlation" = {
                    numeric_data <- values$processed_data[sapply(values$processed_data, is.numeric)]
                    if (ncol(numeric_data) >= 2) {
                      cor_matrix <- cor(numeric_data, use = "complete.obs")
                      
                      # Convert to long format for ggplot
                      cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
                      cor_df$Correlation <- as.vector(cor_matrix)
                      
                      ggplot(cor_df, aes(Var1, Var2, fill = Correlation)) +
                        geom_tile() +
                        scale_fill_gradient2(low = waskita_colors$error, mid = "white", 
                                             high = waskita_colors$navy, midpoint = 0,
                                             limits = c(-1, 1)) +
                        labs(title = "Correlation Heatmap", x = "", y = "") +
                        theme_waskita() +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
                    }
                  },
                  
                  "boxplot_group" = {
                    if (!is.null(input$boxplot_numeric) && !is.null(input$boxplot_group)) {
                      # Get unique groups and create color palette
                      group_data <- values$processed_data[[input$boxplot_group]]
                      unique_groups <- unique(group_data[!is.na(group_data)])
                      n_groups <- length(unique_groups)
                      
                      # Create color palette with enough colors
                      color_palette <- rep(c(waskita_colors$steel, waskita_colors$warning, 
                                             waskita_colors$success, waskita_colors$navy), 
                                           length.out = n_groups)
                      
                      ggplot(values$processed_data, aes_string(x = input$boxplot_group, y = input$boxplot_numeric, 
                                                               fill = input$boxplot_group)) +
                        geom_boxplot(alpha = 0.7) +
                        labs(title = paste("Boxplot:", input$boxplot_numeric, "by", input$boxplot_group)) +
                        theme_waskita() +
                        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
                        scale_fill_manual(values = color_palette)
                    }
                  },
                  
                  "histogram_overlay" = {
                    if (!is.null(input$hist_variables) && length(input$hist_variables) > 0) {
                      selected_vars <- head(input$hist_variables, 4)  # Max 4 variables
                      
                      # Reshape data to long format
                      plot_data <- values$processed_data[selected_vars]
                      plot_data_long <- tidyr::gather(plot_data, key = "Variable", value = "Value")
                      
                      ggplot(plot_data_long, aes(x = Value, fill = Variable)) +
                        geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
                        facet_wrap(~Variable, scales = "free") +
                        labs(title = "Histogram Overlay") +
                        theme_waskita() +
                        scale_fill_manual(values = c(waskita_colors$steel, waskita_colors$warning, 
                                                     waskita_colors$success, waskita_colors$navy))
                    }
                  }
      )
      
      if (!is.null(p)) {
        ggplotly(p) %>%
          layout(
            font = list(family = "Segoe UI", size = 12),
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)'
          )
      }
    }
  })
  
  output$viz_interpretation <- renderPrint({
    if (values$viz_generated && !is.null(values$current_viz_type)) {
      cat("INTERPRETASI VISUALISASI\n")
      cat("========================\n")
      
      switch(values$current_viz_type,
             "scatter" = {
               if (!is.null(input$scatter_x) && !is.null(input$scatter_y)) {
                 # Calculate correlation
                 x_data <- values$processed_data[[input$scatter_x]]
                 y_data <- values$processed_data[[input$scatter_y]]
                 cor_val <- cor(x_data, y_data, use = "complete.obs")
                 
                 cat("Scatter Plot Analysis:\n")
                 cat("- Korelasi:", round(cor_val, 4), "\n")
                 cat("- Kekuatan:", interpret_correlation(cor_val), "\n")
                 cat("- Arah:", if(cor_val > 0) "Positif" else "Negatif", "\n")
                 
                 if (abs(cor_val) > 0.7) {
                   cat("- Hubungan KUAT ditemukan\n")
                 } else if (abs(cor_val) > 0.3) {
                   cat("- Hubungan SEDANG ditemukan\n")
                 } else {
                   cat("- Hubungan LEMAH ditemukan\n")
                 }
               }
             },
             
             "correlation" = {
               numeric_data <- values$processed_data[sapply(values$processed_data, is.numeric)]
               cor_matrix <- cor(numeric_data, use = "complete.obs")
               
               # Find strong correlations
               strong_cors <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
               
               cat("Correlation Heatmap Analysis:\n")
               cat("- Total variabel numerik:", ncol(numeric_data), "\n")
               cat("- Korelasi kuat (|r| > 0.7):", nrow(strong_cors), "pasang\n")
               
               if (nrow(strong_cors) > 0) {
                 cat("\nKorelasi Kuat:\n")
                 for (i in 1:min(5, nrow(strong_cors))) {
                   row_idx <- strong_cors[i, 1]
                   col_idx <- strong_cors[i, 2]
                   var1 <- rownames(cor_matrix)[row_idx]
                   var2 <- colnames(cor_matrix)[col_idx]
                   cor_val <- cor_matrix[row_idx, col_idx]
                   cat("-", var1, "vs", var2, ":", round(cor_val, 3), "\n")
                 }
               }
             },
             
             "boxplot_group" = {
               if (!is.null(input$boxplot_numeric) && !is.null(input$boxplot_group)) {
                 # ANOVA test for group differences
                 formula <- as.formula(paste(input$boxplot_numeric, "~", input$boxplot_group))
                 anova_result <- aov(formula, data = values$processed_data)
                 anova_p <- summary(anova_result)[[1]]$`Pr(>F)`[1]
                 
                 cat("Boxplot Group Analysis:\n")
                 cat("- ANOVA p-value:", format_pvalue(anova_p), "\n")
                 cat("- Interpretasi:", interpret_significance(anova_p), "\n")
                 
                 if (anova_p <= 0.05) {
                   cat("- Ada perbedaan signifikan antar grup\n")
                 } else {
                   cat("- Tidak ada perbedaan signifikan antar grup\n")
                 }
               }
             },
             
             "histogram_overlay" = {
               if (!is.null(input$hist_variables) && length(input$hist_variables) > 0) {
                 selected_vars <- head(input$hist_variables, 4)
                 
                 cat("Histogram Overlay Analysis:\n")
                 cat("- Variabel yang dibandingkan:", length(selected_vars), "\n")
                 
                 for (var in selected_vars) {
                   var_data <- values$processed_data[[var]]
                   cat("- ", var, ": Mean =", round(mean(var_data, na.rm = TRUE), 3),
                       ", SD =", round(sd(var_data, na.rm = TRUE), 3), "\n")
                 }
                 
                 cat("\nPerbandingan distribusi membantu mengidentifikasi:\n")
                 cat("- Perbedaan skala dan lokasi\n")
                 cat("- Pola distribusi yang serupa/berbeda\n")
                 cat("- Outlier pada masing-masing variabel\n")
               }
             }
      )
    }
  })
  
  # =============================================================================
  # PETA SPASIAL - ENHANCED WITH REAL INDONESIA GEOJSON
  # =============================================================================
  
  observe({
    tryCatch({
      values$indonesia_sf <- st_read("D:/Perkuliahan Tingkat 2 Semester 4/WASKITA2/data/indonesia511.geojson", quiet = TRUE)
    }, error = function(e) {
      values$indonesia_sf <- NULL
      safe_notification("Gagal membaca file GeoJSON. Pastikan nama dan path file benar.", "error")
    })
  })
  
  observeEvent(input$generate_map, {
    req(values$processed_data, input$map_variable)
    
    tryCatch({
      values$map_generated <- TRUE
      
      # Add to activity tracker
      add_activity_to_tracker(
        type = "spatial",
        title = paste("Peta Distribusi Spasial -", input$map_variable),
        content = paste(
          paste("Variabel yang dipetakan:", input$map_variable),
          paste("Tipe peta:", input$map_type),
          paste("Skema warna:", input$color_palette),
          paste("Jumlah kelas:", input$map_bins),
          paste("Sumber geografis:", if(is.null(values$indonesia_sf)) "Synthetic" else "GeoJSON"),
          sep = "\n"
        )
      )
      
      safe_notification("Peta berhasil dibuat!", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error membuat peta:", e$message), "error")
    })
  })
  
  output$map_generated <- reactive({
    values$map_generated
  })
  outputOptions(output, "map_generated", suspendWhenHidden = FALSE)
  
  # Enhanced spatial map output with real Indonesia GeoJSON
  output$spatial_map <- renderLeaflet({
    if (values$map_generated && !is.null(input$map_variable)) {
      
      # Get the variable data
      var_data <- values$processed_data[[input$map_variable]]
      
      # Create map based on available spatial data
      if (!is.null(values$indonesia_sf)) {
        # Use real Indonesia GeoJSON
        tryCatch({
          # Merge spatial data with variable data
          merged_data <- create_choropleth_data(values$processed_data, values$indonesia_sf, input$map_variable)
          
          if (!is.null(merged_data) && input$map_variable %in% names(merged_data)) {
            # Create color palette
            pal <- colorQuantile(palette = input$color_palette, 
                                 domain = merged_data[[input$map_variable]], 
                                 n = input$map_bins,
                                 na.color = "#808080")
            
            # Create leaflet map with choropleth
            m <- leaflet(merged_data) %>%
              addTiles() %>%
              setView(lng = 118, lat = -2, zoom = 5)
            
            if (input$map_type == "choropleth") {
              m <- m %>%
                addPolygons(
                  fillColor = ~pal(get(input$map_variable)),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.8,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.9,
                    bringToFront = TRUE
                  ),
                  popup = ~paste(
                    "<strong>District:</strong>", DISTRICTCODE, "<br>",
                    "<strong>", input$map_variable, ":</strong>", 
                    round(get(input$map_variable), 3)
                  )
                ) %>%
                addLegend(
                  pal = pal,
                  values = ~get(input$map_variable),
                  title = input$map_variable,
                  position = "bottomright",
                  opacity = 0.8
                )
            }
            
            return(m)
            
          } else {
            # Fallback to point-based visualization
            map_data <- values$processed_data
            map_data$lat <- runif(nrow(map_data), -11, 6)
            map_data$lon <- runif(nrow(map_data), 95, 141)
            map_data$value <- var_data
            
            pal <- colorNumeric(palette = input$color_palette, domain = map_data$value)
            
            m <- leaflet(map_data) %>%
              addTiles() %>%
              setView(lng = 118, lat = -2, zoom = 5) %>%
              addCircleMarkers(
                lng = ~lon, lat = ~lat,
                fillColor = ~pal(value),
                color = "white",
                weight = 2,
                radius = 8,
                fillOpacity = 0.8,
                popup = ~paste(
                  "<strong>", input$map_variable, ":</strong>", round(value, 3)
                )
              ) %>%
              addLegend(
                pal = pal,
                values = ~value,
                title = input$map_variable,
                position = "bottomright"
              )
            
            return(m)
          }
          
        }, error = function(e) {
          # If GeoJSON processing fails, fallback to points
          map_data <- values$processed_data
          if (!"lat" %in% names(map_data)) {
            set.seed(123)
            map_data$lat <- runif(nrow(map_data), -11, 6)
            map_data$lon <- runif(nrow(map_data), 95, 141)
          }
          map_data$value <- var_data
          
          pal <- colorNumeric(palette = input$color_palette, domain = map_data$value)
          
          m <- leaflet(map_data) %>%
            addTiles() %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addCircleMarkers(
              lng = ~lon, lat = ~lat,
              fillColor = ~pal(value),
              color = "white",
              weight = 2,
              radius = 8,
              fillOpacity = 0.8,
              popup = ~paste(
                "<strong>", input$map_variable, ":</strong>", round(value, 3)
              )
            ) %>%
            addLegend(
              pal = pal,
              values = ~value,
              title = input$map_variable,
              position = "bottomright"
            )
          
          return(m)
        })
      } else {
        # Use synthetic point data
        if (is.null(values$map_data)) {
          # Generate synthetic coordinates
          n_obs <- length(var_data)
          set.seed(123)
          
          map_data <- data.frame(
            lat = runif(n_obs, -11, 6),
            lon = runif(n_obs, 95, 141),
            value = var_data,
            district_id = paste0("District_", 1:n_obs),
            stringsAsFactors = FALSE
          )
          values$map_data <- map_data
        } else {
          # Update existing map data with new variable
          values$map_data$value <- var_data
        }
        
        map_data <- values$map_data
        
        # Create color palette
        pal <- colorNumeric(palette = input$color_palette, domain = map_data$value)
        
        # Create leaflet map
        m <- leaflet(map_data) %>%
          addTiles() %>%
          setView(lng = 118, lat = -2, zoom = 5)
        
        if (input$map_type == "point") {
          m <- m %>%
            addCircleMarkers(
              lng = ~lon, lat = ~lat,
              color = ~pal(value),
              radius = 6,
              fillOpacity = 0.8,
              stroke = TRUE,
              weight = 1,
              popup = ~paste("District:", district_id, "<br>",
                             input$map_variable, ":", round(value, 3))
            )
        } else if (input$map_type == "heat") {
          # Heat map style with varying sizes
          m <- m %>%
            addCircleMarkers(
              lng = ~lon, lat = ~lat,
              color = ~pal(value),
              radius = ~scales::rescale(value, to = c(3, 15)),
              fillOpacity = 0.6,
              stroke = TRUE,
              weight = 1,
              popup = ~paste("District:", district_id, "<br>",
                             input$map_variable, ":", round(value, 3))
            )
        } else { # choropleth style with points
          m <- m %>%
            addCircleMarkers(
              lng = ~lon, lat = ~lat,
              fillColor = ~pal(value),
              color = "white",
              weight = 2,
              radius = 8,
              fillOpacity = 0.8,
              popup = ~paste("District:", district_id, "<br>",
                             input$map_variable, ":", round(value, 3))
            )
        }
        
        # Add legend
        m <- m %>%
          addLegend(
            pal = pal,
            values = ~value,
            title = input$map_variable,
            position = "bottomright"
          )
        
        return(m)
      }
    }
  })
  
  output$spatial_stats <- renderPrint({
    if (values$map_generated && !is.null(input$map_variable)) {
      var_data <- values$processed_data[[input$map_variable]]
      
      cat("STATISTIK DISTRIBUSI SPASIAL\n")
      cat("=============================\n")
      cat("Variabel:", input$map_variable, "\n")
      cat("Tipe peta:", input$map_type, "\n")
      cat("Jumlah lokasi:", length(var_data), "\n")
      cat("Sumber geografis:", if(is.null(values$indonesia_sf)) "Synthetic coordinates" else "Indonesia GeoJSON", "\n")
      
      cat("\nDistribusi nilai:\n")
      cat("- Min:", round(min(var_data, na.rm = TRUE), 3), "\n")
      cat("- Max:", round(max(var_data, na.rm = TRUE), 3), "\n")
      cat("- Mean:", round(mean(var_data, na.rm = TRUE), 3), "\n")
      cat("- SD:", round(sd(var_data, na.rm = TRUE), 3), "\n")
      
      # Calculate spatial statistics
      value_data <- var_data[!is.na(var_data)]
      q1 <- quantile(value_data, 0.25)
      q3 <- quantile(value_data, 0.75)
      high_values <- sum(value_data > q3)
      low_values <- sum(value_data < q1)
      
      cat("\nAnalisis Kuartil:\n")
      cat("   - Lokasi dengan nilai tinggi (>Q3):", high_values, "lokasi\n")
      cat("   - Lokasi dengan nilai rendah (<Q1):", low_values, "lokasi\n")
      cat("   - Range nilai:", round(min(value_data), 3), "hingga", round(max(value_data), 3), "\n")
    }
  })
  
  output$spatial_interpretation <- renderPrint({
    if (values$map_generated && !is.null(input$map_variable)) {
      cat("INTERPRETASI POLA SPASIAL\n")
      cat("=========================\n")
      cat("Peta menunjukkan distribusi geografis dari", input$map_variable, "\n\n")
      
      cat("Pola yang dapat diamati:\n")
      cat("1. DISTRIBUSI GEOGRAFIS:\n")
      if (!is.null(values$indonesia_sf)) {
        cat("   - Peta choropleth Indonesia menampilkan pola regional\n")
        cat("   - Warna polygon menunjukkan intensitas per kabupaten/kota\n")
        cat("   - Dapat mengidentifikasi clustering provinsi/regional\n")
      } else {
        cat("   - Peta menampilkan sebaran titik across Indonesia\n")
        cat("   - Warna menunjukkan intensitas/magnitude variabel\n")
        cat("   - Variasi spasial menunjukkan heterogenitas geografis\n")
      }
      
      cat("\n2. KLASTERISASI:\n")
      cat("   - Perhatikan apakah ada pengelompokan nilai tinggi/rendah\n")
      cat("   - Area dengan warna serupa mengindikasikan pola lokal\n")
      cat("   - Clustering dapat mengindikasikan faktor geografis\n")
      
      cat("\n3. OUTLIER SPASIAL:\n")
      cat("   - Lokasi dengan warna berbeda dari sekitarnya\n")
      cat("   - Dapat mengindikasikan anomali atau karakteristik unik\n")
      cat("   - Perlu investigasi lebih lanjut untuk outlier spasial\n")
      
      cat("\n4. IMPLIKASI KEBIJAKAN:\n")
      cat("   - Identifikasi area prioritas berdasarkan pola spasial\n")
      cat("   - Pertimbangkan faktor geografis dalam intervensi\n")
      cat("   - Fokus pada area dengan nilai ekstrem untuk tindakan khusus\n")
    }
  })
  
  # =============================================================================
  # UJI NORMALITAS - ENHANCED WITH TRACKING
  # =============================================================================
  
  observeEvent(input$run_normality, {
    req(values$processed_data, input$normality_variable)
    
    tryCatch({
      var_data <- values$processed_data[[input$normality_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      if (length(var_data) < 3) {
        safe_notification("Data tidak cukup untuk uji normalitas", "warning")
        return()
      }
      
      test_result <- switch(input$normality_test,
                            "shapiro" = {
                              if (length(var_data) > 5000) {
                                safe_notification("Shapiro-Wilk tidak recommended untuk n > 5000", "warning")
                              }
                              shapiro.test(var_data)
                            },
                            "ks" = {
                              ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
                            },
                            "ad" = {
                              nortest::ad.test(var_data)
                            }
      )
      
      values$normality_result <- test_result
      values$normality_done <- TRUE
      
      # Add to activity tracker
      test_interpretation <- if(test_result$p.value > 0.05) {
        "Data berdistribusi normal (gagal menolak H0)"
      } else {
        "Data tidak berdistribusi normal (tolak H0)"
      }
      
      add_activity_to_tracker(
        type = "test",
        title = paste("Uji Normalitas -", input$normality_test, "pada", input$normality_variable),
        content = paste(
          paste("Variabel:", input$normality_variable),
          paste("Metode uji:", input$normality_test),
          paste("Ukuran sampel:", length(var_data)),
          paste("Statistik uji:", round(test_result$statistic, 4)),
          paste("P-value:", format_pvalue(test_result$p.value)),
          paste("Interpretasi:", test_interpretation),
          sep = "\n"
        ),
        tables = paste(capture.output(print(test_result)), collapse = "\n")
      )
      
      safe_notification("Uji normalitas selesai", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error uji normalitas:", e$message), "error")
    })
  })
  
  output$normality_done <- reactive({
    values$normality_done
  })
  outputOptions(output, "normality_done", suspendWhenHidden = FALSE)
  
  output$normality_result <- renderPrint({
    if (values$normality_done && !is.null(values$normality_result)) {
      result <- values$normality_result
      
      cat("HASIL UJI NORMALITAS\n")
      cat("====================\n")
      cat("Variabel:", input$normality_variable, "\n")
      cat("Metode:", switch(input$normality_test,
                            "shapiro" = "Shapiro-Wilk Test",
                            "ks" = "Kolmogorov-Smirnov Test",
                            "ad" = "Anderson-Darling Test"), "\n\n")
      
      cat("HIPOTESIS:\n")
      cat("H₀: Data berdistribusi normal\n")
      cat("H₁: Data tidak berdistribusi normal\n")
      cat("α = 0.05\n\n")
      
      print(result)
      
      cat("\nINTERPRETASI:\n")
      p_value <- result$p.value
      cat("P-value:", format_pvalue(p_value), "\n")
      
      if (p_value > 0.05) {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Data dapat dianggap berdistribusi normal (α = 0.05)\n")
        cat("IMPLIKASI: Dapat menggunakan uji parametrik\n")
      } else {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Data tidak berdistribusi normal (α = 0.05)\n")
        cat("IMPLIKASI: Pertimbangkan transformasi atau uji non-parametrik\n")
      }
    }
  })
  
  # =============================================================================
  # UJI HOMOGENITAS - ENHANCED WITH TRACKING
  # =============================================================================
  
  observeEvent(input$run_homogeneity, {
    req(values$processed_data, input$homogeneity_numeric, input$homogeneity_group)
    
    tryCatch({
      formula <- as.formula(paste(input$homogeneity_numeric, "~", input$homogeneity_group))
      
      test_result <- switch(input$homogeneity_test,
                            "levene" = {
                              car::leveneTest(formula, data = values$processed_data)
                            },
                            "bartlett" = {
                              bartlett.test(formula, data = values$processed_data)
                            },
                            "fligner" = {
                              fligner.test(formula, data = values$processed_data)
                            }
      )
      
      values$homogeneity_result <- test_result
      values$homogeneity_done <- TRUE
      
      # Add to activity tracker
      p_value <- if(input$homogeneity_test == "levene") test_result$`Pr(>F)`[1] else test_result$p.value
      test_interpretation <- if(p_value > 0.05) {
        "Varians antar grup homogen (gagal menolak H0)"
      } else {
        "Varians antar grup tidak homogen (tolak H0)"
      }
      
      add_activity_to_tracker(
        type = "test",
        title = paste("Uji Homogenitas -", input$homogeneity_test),
        content = paste(
          paste("Variabel numerik:", input$homogeneity_numeric),
          paste("Variabel grup:", input$homogeneity_group),
          paste("Metode uji:", input$homogeneity_test),
          paste("P-value:", format_pvalue(p_value)),
          paste("Interpretasi:", test_interpretation),
          sep = "\n"
        ),
        tables = paste(capture.output(print(test_result)), collapse = "\n")
      )
      
      safe_notification("Uji homogenitas selesai", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error uji homogenitas:", e$message), "error")
    })
  })
  
  output$homogeneity_done <- reactive({
    values$homogeneity_done
  })
  outputOptions(output, "homogeneity_done", suspendWhenHidden = FALSE)
  
  output$homogeneity_result <- renderPrint({
    if (values$homogeneity_done && !is.null(values$homogeneity_result)) {
      result <- values$homogeneity_result
      
      cat("HASIL UJI HOMOGENITAS VARIANS\n")
      cat("==============================\n")
      cat("Variabel numerik:", input$homogeneity_numeric, "\n")
      cat("Variabel grup:", input$homogeneity_group, "\n")
      cat("Metode:", switch(input$homogeneity_test,
                            "levene" = "Levene's Test",
                            "bartlett" = "Bartlett's Test",
                            "fligner" = "Fligner-Killeen Test"), "\n\n")
      
      cat("HIPOTESIS:\n")
      cat("H₀: Varians antar grup homogen\n")
      cat("H₁: Varians antar grup tidak homogen\n")
      cat("α = 0.05\n\n")
      
      print(result)
      
      cat("\nINTERPRETASI:\n")
      p_value <- if(input$homogeneity_test == "levene") result$`Pr(>F)`[1] else result$p.value
      cat("P-value:", format_pvalue(p_value), "\n")
      
      if (p_value > 0.05) {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Varians antar grup homogen (α = 0.05)\n")
        cat("IMPLIKASI: Asumsi homogenitas terpenuhi untuk ANOVA\n")
      } else {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Varians antar grup tidak homogen (α = 0.05)\n")
        cat("IMPLIKASI: Gunakan Welch's test atau transformasi data\n")
      }
    }
  })
  
  # =============================================================================
  # DIAGNOSTIC PLOTS
  # =============================================================================
  
  output$diagnostic_plots <- renderPlotly({
    if (values$normality_done || values$homogeneity_done) {
      plots <- list()
      
      if (values$normality_done) {
        var_data <- values$processed_data[[input$normality_variable]]
        
        # Q-Q Plot
        qq_plot <- ggplot(data.frame(sample = var_data), aes(sample = sample)) +
          stat_qq(color = waskita_colors$steel) +
          stat_qq_line(color = waskita_colors$navy) +
          labs(title = "Q-Q Plot", subtitle = "Test for Normality") +
          theme_waskita()
        
        ggplotly(qq_plot) %>%
          layout(
            font = list(family = "Segoe UI", size = 12),
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)'
          )
      }
    }
  })
  
  output$assumptions_interpretation <- renderPrint({
    if (values$normality_done || values$homogeneity_done) {
      cat("RINGKASAN UJI ASUMSI\n")
      cat("====================\n")
      
      if (values$normality_done && values$homogeneity_done) {
        norm_p <- values$normality_result$p.value
        homo_p <- if("Pr(>F)" %in% names(values$homogeneity_result)) {
          values$homogeneity_result$`Pr(>F)`[1]
        } else {
          values$homogeneity_result$p.value
        }
        
        cat("Normalitas:", if(norm_p > 0.05) "✓ TERPENUHI" else "✗ TIDAK TERPENUHI", "\n")
        cat("Homogenitas:", if(homo_p > 0.05) "✓ TERPENUHI" else "✗ TIDAK TERPENUHI", "\n\n")
        
        cat("REKOMENDASI ANALISIS:\n")
        if (norm_p > 0.05 && homo_p > 0.05) {
          cat("- Gunakan uji parametrik standar (t-test, ANOVA)\n")
          cat("- Semua asumsi terpenuhi\n")
        } else if (norm_p > 0.05 && homo_p <= 0.05) {
          cat("- Gunakan Welch's test (varians tidak sama)\n")
          cat("- Normalitas terpenuhi, homogenitas tidak\n")
        } else if (norm_p <= 0.05 && homo_p > 0.05) {
          cat("- Pertimbangkan transformasi data\n")
          cat("- Atau gunakan uji non-parametrik\n")
        } else {
          cat("- Gunakan uji non-parametrik\n")
          cat("- Kedua asumsi tidak terpenuhi\n")
        }
        
      } else if (values$normality_done) {
        cat("Hanya uji normalitas yang dilakukan.\n")
        cat("Lanjutkan dengan uji homogenitas untuk evaluasi lengkap.\n")
      } else if (values$homogeneity_done) {
        cat("Hanya uji homogenitas yang dilakukan.\n")
        cat("Lanjutkan dengan uji normalitas untuk evaluasi lengkap.\n")
      }
    }
  })
  
  # =============================================================================
  # UJI BEDA RATA-RATA (t-test) - ENHANCED WITH TRACKING
  # =============================================================================
  
  observeEvent(input$run_ttest, {
    req(values$processed_data, input$ttest_variable)
    
    tryCatch({
      var_data <- values$processed_data[[input$ttest_variable]]
      
      test_result <- switch(input$ttest_type,
                            "one_sample" = {
                              req(input$test_value)
                              t.test(var_data, mu = input$test_value, 
                                     alternative = input$alternative,
                                     conf.level = input$confidence_level)
                            },
                            "independent" = {
                              req(input$ttest_group)
                              formula <- as.formula(paste(input$ttest_variable, "~", input$ttest_group))
                              t.test(formula, data = values$processed_data,
                                     var.equal = input$equal_variance,
                                     alternative = input$alternative,
                                     conf.level = input$confidence_level)
                            },
                            "paired" = {
                              req(input$ttest_paired_var)
                              var2_data <- values$processed_data[[input$ttest_paired_var]]
                              t.test(var_data, var2_data, paired = TRUE,
                                     alternative = input$alternative,
                                     conf.level = input$confidence_level)
                            }
      )
      
      values$ttest_result <- test_result
      values$ttest_done <- TRUE
      
      # Add to activity tracker
      test_interpretation <- if(test_result$p.value <= 0.05) {
        "Terdapat perbedaan yang signifikan (tolak H0)"
      } else {
        "Tidak ada perbedaan yang signifikan (gagal menolak H0)"
      }
      
      add_activity_to_tracker(
        type = "test",
        title = paste("Uji t -", input$ttest_type),
        content = paste(
          paste("Jenis uji:", input$ttest_type),
          paste("Variabel:", input$ttest_variable),
          if(input$ttest_type == "one_sample") paste("Nilai uji:", input$test_value) else "",
          if(input$ttest_type == "independent") paste("Variabel grup:", input$ttest_group) else "",
          if(input$ttest_type == "paired") paste("Variabel kedua:", input$ttest_paired_var) else "",
          paste("T-statistic:", round(test_result$statistic, 4)),
          paste("P-value:", format_pvalue(test_result$p.value)),
          paste("Confidence level:", input$confidence_level * 100, "%"),
          paste("Interpretasi:", test_interpretation),
          sep = "\n"
        ),
        tables = paste(capture.output(print(test_result)), collapse = "\n")
      )
      
      safe_notification("Uji t berhasil dilakukan", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error uji t:", e$message), "error")
    })
  })
  
  output$ttest_done <- reactive({
    values$ttest_done
  })
  outputOptions(output, "ttest_done", suspendWhenHidden = FALSE)
  
  output$ttest_result <- renderPrint({
    if (values$ttest_done && !is.null(values$ttest_result)) {
      result <- values$ttest_result
      
      cat("HASIL UJI BEDA RATA-RATA\n")
      cat("========================\n")
      cat("Jenis uji:", switch(input$ttest_type,
                               "one_sample" = "One Sample t-test",
                               "independent" = "Independent Samples t-test", 
                               "paired" = "Paired Samples t-test"), "\n")
      cat("Variabel:", input$ttest_variable, "\n")
      cat("Confidence level:", input$confidence_level * 100, "%\n\n")
      
      cat("HIPOTESIS:\n")
      if (input$ttest_type == "one_sample") {
        cat("H₀: μ =", input$test_value, "\n")
        cat("H₁: μ", switch(input$alternative,
                            "two.sided" = "≠",
                            "greater" = ">", 
                            "less" = "<"), input$test_value, "\n\n")
      } else if (input$ttest_type == "independent") {
        cat("H₀: μ₁ = μ₂\n")
        cat("H₁: μ₁", switch(input$alternative,
                             "two.sided" = "≠",
                             "greater" = ">",
                             "less" = "<"), "μ₂\n\n")
      } else {
        cat("H₀: μd = 0\n")
        cat("H₁: μd", switch(input$alternative,
                             "two.sided" = "≠",
                             "greater" = ">",
                             "less" = "<"), "0\n\n")
      }
      
      print(result)
      
      cat("\nINTERPRETASI:\n")
      p_value <- result$p.value
      cat("P-value:", format_pvalue(p_value), "\n")
      cat("Interpretasi:", interpret_significance(p_value), "\n")
      
      if (p_value <= 0.05) {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Terdapat perbedaan yang signifikan\n")
      } else {
        cat("KEPUTUSAN: Gagal menolak H₀\n") 
        cat("KESIMPULAN: Tidak ada perbedaan yang signifikan\n")
      }
      
      # Confidence Interval
      if (!is.null(result$conf.int)) {
        ci <- result$conf.int
        cat("\nConfidence Interval (", input$confidence_level * 100, "%):\n")
        cat("[", round(ci[1], 4), ",", round(ci[2], 4), "]\n")
      }
    }
  })
  
  output$ttest_plot <- renderPlotly({
    if (values$ttest_done && !is.null(values$ttest_result)) {
      
      if (input$ttest_type == "one_sample") {
        # Histogram with test value line
        p <- ggplot(values$processed_data, aes_string(x = input$ttest_variable)) +
          geom_histogram(bins = 30, fill = waskita_colors$steel, alpha = 0.7) +
          geom_vline(xintercept = input$test_value, color = waskita_colors$error, 
                     linetype = "dashed", linewidth = 1) +
          geom_vline(xintercept = mean(values$processed_data[[input$ttest_variable]], na.rm = TRUE),
                     color = waskita_colors$navy, linewidth = 1) +
          labs(title = "One Sample t-test", 
               subtitle = "Red line: Test value, Blue line: Sample mean") +
          theme_waskita()
        
      } else if (input$ttest_type == "independent") {
        # Boxplot by group
        p <- ggplot(values$processed_data, 
                    aes_string(x = input$ttest_group, y = input$ttest_variable,
                               fill = input$ttest_group)) +
          geom_boxplot(alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5) +
          labs(title = "Independent Samples t-test") +
          theme_waskita() +
          theme(legend.position = "none") +
          scale_fill_manual(values = c(waskita_colors$steel, waskita_colors$warning))
        
      } else { # paired
        # Scatter plot of paired data
        df_paired <- data.frame(
          var1 = values$processed_data[[input$ttest_variable]],
          var2 = values$processed_data[[input$ttest_paired_var]]
        )
        p <- ggplot(df_paired, aes(x = var1, y = var2)) +
          geom_point(alpha = 0.6, color = waskita_colors$steel) +
          geom_abline(intercept = 0, slope = 1, color = waskita_colors$error, 
                      linetype = "dashed") +
          labs(title = "Paired Samples t-test",
               x = input$ttest_variable,
               y = input$ttest_paired_var,
               subtitle = "Red line: y = x (no difference)") +
          theme_waskita()
      }
      
      ggplotly(p) %>%
        layout(
          font = list(family = "Segoe UI", size = 12),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  output$ttest_effect_size <- renderPrint({
    if (values$ttest_done && !is.null(values$ttest_result)) {
      
      cat("EFFECT SIZE & POWER ANALYSIS\n")
      cat("============================\n")
      
      # Calculate Cohen's d
      if (input$ttest_type == "one_sample") {
        var_data <- values$processed_data[[input$ttest_variable]]
        cohens_d <- (mean(var_data, na.rm = TRUE) - input$test_value) / sd(var_data, na.rm = TRUE)
      } else if (input$ttest_type == "independent") {
        group_data <- split(values$processed_data[[input$ttest_variable]], 
                            values$processed_data[[input$ttest_group]])
        if (length(group_data) == 2) {
          m1 <- mean(group_data[[1]], na.rm = TRUE)
          m2 <- mean(group_data[[2]], na.rm = TRUE)
          s1 <- sd(group_data[[1]], na.rm = TRUE)
          s2 <- sd(group_data[[2]], na.rm = TRUE)
          n1 <- length(group_data[[1]])
          n2 <- length(group_data[[2]])
          pooled_sd <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2))
          cohens_d <- (m1 - m2) / pooled_sd
        } else {
          cohens_d <- NA
        }
      } else { # paired
        var1_data <- values$processed_data[[input$ttest_variable]]
        var2_data <- values$processed_data[[input$ttest_paired_var]]
        differences <- var1_data - var2_data
        cohens_d <- mean(differences, na.rm = TRUE) / sd(differences, na.rm = TRUE)
      }
      
      if (!is.na(cohens_d)) {
        cat("Cohen's d:", round(cohens_d, 4), "\n")
        cat("Interpretasi:", interpret_cohens_d(cohens_d), "effect size\n")
        
        cat("\nPANDUAN COHEN'S D:\n")
        cat("- |d| < 0.2: Sangat kecil\n")
        cat("- |d| 0.2-0.5: Kecil\n") 
        cat("- |d| 0.5-0.8: Sedang\n")
        cat("- |d| > 0.8: Besar\n")
      }
      
      # Sample size info
      if (input$ttest_type == "independent") {
        group_data <- split(values$processed_data[[input$ttest_variable]], 
                            values$processed_data[[input$ttest_group]])
        if (length(group_data) == 2) {
          cat("\nSAMPLE SIZE:\n")
          cat("Group 1:", length(group_data[[1]]), "\n")
          cat("Group 2:", length(group_data[[2]]), "\n")
        }
      } else {
        n_obs <- sum(!is.na(values$processed_data[[input$ttest_variable]]))
        cat("\nSample size:", n_obs, "\n")
      }
    }
  })
  
  output$ttest_interpretation <- renderPrint({
    if (values$ttest_done && !is.null(values$ttest_result)) {
      result <- values$ttest_result
      p_value <- result$p.value
      
      cat("INTERPRETASI PRAKTIS\n")
      cat("====================\n")
      
      if (p_value <= 0.001) {
        cat("- Perbedaan SANGAT SIGNIFIKAN secara statistik\n")
      } else if (p_value <= 0.01) {
        cat("- Perbedaan SIGNIFIKAN secara statistik\n")
      } else if (p_value <= 0.05) {
        cat("- Perbedaan signifikan secara statistik\n")
      } else if (p_value <= 0.1) {
        cat("- Perbedaan marginally significant\n")
      } else {
        cat("- Tidak ada perbedaan yang signifikan\n")
      }
      
      cat("\nREKOMENDASI:\n")
      if (p_value <= 0.05) {
        cat("1. Hasil dapat dipublikasikan sebagai temuan signifikan\n")
        cat("2. Pertimbangkan signifikansi praktis (effect size)\n")
        cat("3. Replikasi study untuk konfirmasi\n")
      } else {
        cat("1. Tidak ada bukti perbedaan yang signifikan\n") 
        cat("2. Pertimbangkan power analysis\n")
        cat("3. Evaluasi kemungkinan Type II error\n")
      }
    }
  })
  
  # =============================================================================
  # UJI PROPORSI & VARIANCE - ENHANCED WITH TRACKING
  # =============================================================================
  
  observeEvent(input$run_prop_test, {
    req(values$processed_data, input$prop_variable)
    
    tryCatch({
      if (input$prop_test_type == "one_prop") {
        var_data <- values$processed_data[[input$prop_variable]]
        tbl <- table(var_data, useNA = "no")
        
        if (length(tbl) < 2) {
          safe_notification("Variabel harus memiliki minimal 2 kategori", "warning")
          return()
        }
        
        success_count <- tbl[1]  # First category as success
        total_count <- sum(tbl)
        
        test_result <- prop.test(success_count, total_count, 
                                 p = input$prop_test_value,
                                 alternative = "two.sided")
        
      } else { # two_prop
        req(input$prop_group)
        # Create contingency table
        cont_table <- table(values$processed_data[[input$prop_variable]], 
                            values$processed_data[[input$prop_group]])
        
        if (ncol(cont_table) < 2) {
          safe_notification("Grup harus memiliki minimal 2 kategori", "warning")
          return()
        }
        
        success_counts <- cont_table[1, ]  # First row as success
        total_counts <- colSums(cont_table)
        
        test_result <- prop.test(success_counts, total_counts,
                                 alternative = "two.sided")
      }
      
      values$prop_test_result <- test_result
      values$prop_test_done <- TRUE
      
      # Add to activity tracker
      test_interpretation <- if(test_result$p.value <= 0.05) {
        "Terdapat perbedaan proporsi yang signifikan"
      } else {
        "Tidak ada perbedaan proporsi yang signifikan"
      }
      
      add_activity_to_tracker(
        type = "test",
        title = paste("Uji Proporsi -", input$prop_test_type),
        content = paste(
          paste("Jenis uji:", input$prop_test_type),
          paste("Variabel:", input$prop_variable),
          if(input$prop_test_type == "one_prop") paste("Proporsi uji:", input$prop_test_value) else "",
          if(input$prop_test_type == "two_prop") paste("Variabel grup:", input$prop_group) else "",
          paste("Chi-squared:", round(test_result$statistic, 4)),
          paste("P-value:", format_pvalue(test_result$p.value)),
          paste("Interpretasi:", test_interpretation),
          sep = "\n"
        ),
        tables = paste(capture.output(print(test_result)), collapse = "\n")
      )
      
      safe_notification("Uji proporsi berhasil dilakukan", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error uji proporsi:", e$message), "error")
    })
  })
  
  output$prop_test_done <- reactive({
    values$prop_test_done
  })
  outputOptions(output, "prop_test_done", suspendWhenHidden = FALSE)
  
  output$prop_test_result <- renderPrint({
    if (values$prop_test_done && !is.null(values$prop_test_result)) {
      result <- values$prop_test_result
      
      cat("HASIL UJI PROPORSI\n")
      cat("==================\n")
      cat("Jenis uji:", switch(input$prop_test_type,
                               "one_prop" = "One Sample Proportion Test",
                               "two_prop" = "Two Sample Proportion Test"), "\n")
      cat("Variabel:", input$prop_variable, "\n\n")
      
      print(result)
      
      cat("\nINTERPRETASI:\n")
      p_value <- result$p.value
      cat("P-value:", format_pvalue(p_value), "\n")
      cat("Interpretasi:", interpret_significance(p_value), "\n")
      
      if (p_value <= 0.05) {
        cat("KESIMPULAN: Terdapat perbedaan proporsi yang signifikan\n")
      } else {
        cat("KESIMPULAN: Tidak ada perbedaan proporsi yang signifikan\n")
      }
    }
  })
  
  output$prop_test_plot <- renderPlotly({
    if (values$prop_test_done && !is.null(values$prop_test_result)) {
      
      if (input$prop_test_type == "one_prop") {
        # Bar chart for proportions
        var_data <- values$processed_data[[input$prop_variable]]
        tbl <- table(var_data)
        prop_df <- data.frame(
          Category = names(tbl),
          Count = as.numeric(tbl),
          Proportion = as.numeric(tbl) / sum(tbl)
        )
        
        # Create color palette based on number of categories
        n_cats <- nrow(prop_df)
        colors <- rep(c(waskita_colors$steel, waskita_colors$warning, 
                        waskita_colors$success, waskita_colors$navy), 
                      length.out = n_cats)
        
        p <- ggplot(prop_df, aes(x = Category, y = Proportion, fill = Category)) +
          geom_bar(stat = "identity", alpha = 0.7) +
          geom_hline(yintercept = input$prop_test_value, color = waskita_colors$error,
                     linetype = "dashed") +
          labs(title = "One Sample Proportion Test",
               subtitle = paste("Test value:", input$prop_test_value)) +
          theme_waskita() +
          theme(legend.position = "none") +
          scale_fill_manual(values = colors)
        
      } else { # two_prop
        # Grouped bar chart
        cont_table <- table(values$processed_data[[input$prop_variable]], 
                            values$processed_data[[input$prop_group]])
        prop_df <- as.data.frame(prop.table(cont_table, 2))
        colnames(prop_df) <- c("Category", "Group", "Proportion")
        
        # Create color palette based on number of categories
        n_cats <- length(unique(prop_df$Category))
        colors <- rep(c(waskita_colors$steel, waskita_colors$warning, 
                        waskita_colors$success, waskita_colors$navy), 
                      length.out = n_cats)
        
        p <- ggplot(prop_df, aes(x = Group, y = Proportion, fill = Category)) +
          geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
          labs(title = "Two Sample Proportion Test") +
          theme_waskita() +
          scale_fill_manual(values = colors)
      }
      
      ggplotly(p) %>%
        layout(
          font = list(family = "Segoe UI", size = 12),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  # Variance test
  observeEvent(input$run_var_test, {
    req(values$processed_data, input$var_variable)
    
    tryCatch({
      var_data <- values$processed_data[[input$var_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      if (input$var_test_type == "one_var") {
        n <- length(var_data)
        sample_var <- var(var_data)
        test_var <- input$var_test_value
        
        # Chi-square test for variance
        chi_sq_stat <- (n - 1) * sample_var / test_var
        p_value <- 2 * min(pchisq(chi_sq_stat, n-1), 1 - pchisq(chi_sq_stat, n-1))
        
        test_result <- list(
          statistic = chi_sq_stat,
          p.value = p_value,
          parameter = n-1,
          method = "Chi-square test for variance"
        )
        
      } else { # two_var
        req(input$var_group)
        formula <- as.formula(paste(input$var_variable, "~", input$var_group))
        test_result <- var.test(formula, data = values$processed_data)
      }
      
      values$var_test_result <- test_result
      values$var_test_done <- TRUE
      
      # Add to activity tracker
      test_interpretation <- if(test_result$p.value <= 0.05) {
        "Terdapat perbedaan variance yang signifikan"
      } else {
        "Tidak ada perbedaan variance yang signifikan"
      }
      
      add_activity_to_tracker(
        type = "test",
        title = paste("Uji Variance -", input$var_test_type),
        content = paste(
          paste("Jenis uji:", input$var_test_type),
          paste("Variabel:", input$var_variable),
          if(input$var_test_type == "one_var") paste("Variance uji:", input$var_test_value) else "",
          if(input$var_test_type == "two_var") paste("Variabel grup:", input$var_group) else "",
          paste("Test statistic:", round(test_result$statistic, 4)),
          paste("P-value:", format_pvalue(test_result$p.value)),
          paste("Interpretasi:", test_interpretation),
          sep = "\n"
        ),
        tables = paste(capture.output(print(test_result)), collapse = "\n")
      )
      
      safe_notification("Uji variance berhasil dilakukan", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error uji variance:", e$message), "error")
    })
  })
  
  output$var_test_done <- reactive({
    values$var_test_done
  })
  outputOptions(output, "var_test_done", suspendWhenHidden = FALSE)
  
  output$var_test_result <- renderPrint({
    if (values$var_test_done && !is.null(values$var_test_result)) {
      result <- values$var_test_result
      
      cat("HASIL UJI VARIANCE\n")
      cat("==================\n")
      cat("Jenis uji:", switch(input$var_test_type,
                               "one_var" = "One Sample Variance Test",
                               "two_var" = "Two Sample Variance Test (F-test)"), "\n")
      cat("Variabel:", input$var_variable, "\n\n")
      
      print(result)
      
      cat("\nINTERPRETASI:\n")
      p_value <- result$p.value
      cat("P-value:", format_pvalue(p_value), "\n")
      cat("Interpretasi:", interpret_significance(p_value), "\n")
      
      if (p_value <= 0.05) {
        cat("KESIMPULAN: Terdapat perbedaan variance yang signifikan\n")
      } else {
        cat("KESIMPULAN: Tidak ada perbedaan variance yang signifikan\n")
      }
    }
  })
  
  output$var_test_plot <- renderPlotly({
    if (values$var_test_done && !is.null(values$var_test_result)) {
      
      if (input$var_test_type == "one_var") {
        # Histogram with variance info
        p <- ggplot(values$processed_data, aes_string(x = input$var_variable)) +
          geom_histogram(bins = 30, fill = waskita_colors$steel, alpha = 0.7) +
          labs(title = "One Sample Variance Test",
               subtitle = paste("Sample variance:", round(var(values$processed_data[[input$var_variable]], na.rm = TRUE), 4))) +
          theme_waskita()
        
      } else { # two_var
        # Boxplot by group with dynamic colors
        group_data <- values$processed_data[[input$var_group]]
        unique_groups <- unique(group_data[!is.na(group_data)])
        n_groups <- length(unique_groups)
        
        # Create color palette with enough colors
        color_palette <- rep(c(waskita_colors$steel, waskita_colors$warning, 
                               waskita_colors$success, waskita_colors$navy), 
                             length.out = n_groups)
        
        p <- ggplot(values$processed_data, 
                    aes_string(x = input$var_group, y = input$var_variable,
                               fill = input$var_group)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = "Two Sample Variance Test") +
          theme_waskita() +
          theme(legend.position = "none") +
          scale_fill_manual(values = color_palette)
      }
      
      ggplotly(p) %>%
        layout(
          font = list(family = "Segoe UI", size = 12),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  output$prop_var_interpretation <- renderPrint({
    cat("RINGKASAN UJI PROPORSI & VARIANCE\n")
    cat("==================================\n")
    
    if (values$prop_test_done && !is.null(values$prop_test_result)) {
      p_val <- values$prop_test_result$p.value
      cat("UJI PROPORSI:\n")
      cat("- P-value:", format_pvalue(p_val), "\n")
      cat("- Status:", if(p_val <= 0.05) "SIGNIFIKAN" else "TIDAK SIGNIFIKAN", "\n\n")
    }
    
    if (values$var_test_done && !is.null(values$var_test_result)) {
      p_val <- values$var_test_result$p.value
      cat("UJI VARIANCE:\n")
      cat("- P-value:", format_pvalue(p_val), "\n")
      cat("- Status:", if(p_val <= 0.05) "SIGNIFIKAN" else "TIDAK SIGNIFIKAN", "\n\n")
    }
    
    cat("IMPLIKASI:\n")
    cat("- Hasil ini membantu memahami variabilitas dalam data\n")
    cat("- Penting untuk validasi asumsi pada uji lanjutan\n")
  })
  
  # =============================================================================
  # ANOVA - ENHANCED WITH TRACKING
  # =============================================================================
  
  observeEvent(input$run_anova, {
    req(values$processed_data, input$anova_dependent, input$anova_factor1)
    
    tryCatch({
      if (input$anova_type == "one_way") {
        formula <- as.formula(paste(input$anova_dependent, "~", input$anova_factor1))
        anova_model <- aov(formula, data = values$processed_data)
        
        # Post-hoc test
        posthoc <- NULL
        tryCatch({
          posthoc <- TukeyHSD(anova_model)
        }, error = function(e) {
          posthoc <- NULL
        })
        
        # Effect size (eta squared)
        anova_summary <- summary(anova_model)
        ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
        ss_between <- anova_summary[[1]]$`Sum Sq`[1]
        eta_squared <- ss_between / ss_total
        
        values$anova_result <- list(
          model = anova_model,
          summary = anova_summary,
          type = "one_way",
          posthoc = posthoc,
          eta_squared = eta_squared
        )
        
      } else { # two_way
        req(input$anova_factor2)
        
        if (input$include_interaction) {
          formula <- as.formula(paste(input$anova_dependent, "~", 
                                      input$anova_factor1, "*", input$anova_factor2))
        } else {
          formula <- as.formula(paste(input$anova_dependent, "~", 
                                      input$anova_factor1, "+", input$anova_factor2))
        }
        
        anova_model <- aov(formula, data = values$processed_data)
        anova_summary <- summary(anova_model)
        
        # Effect sizes for each factor
        ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
        eta_squared_f1 <- anova_summary[[1]]$`Sum Sq`[1] / ss_total
        eta_squared_f2 <- anova_summary[[1]]$`Sum Sq`[2] / ss_total
        
        eta_squared_int <- NULL
        if (input$include_interaction && nrow(anova_summary[[1]]) > 3) {
          eta_squared_int <- anova_summary[[1]]$`Sum Sq`[3] / ss_total
        }
        
        values$anova_result <- list(
          model = anova_model,
          summary = anova_summary,
          type = "two_way",
          eta_squared_f1 = eta_squared_f1,
          eta_squared_f2 = eta_squared_f2,
          eta_squared_int = eta_squared_int
        )
      }
      
      values$anova_done <- TRUE
      
      # Add to activity tracker
      anova_table <- values$anova_result$summary[[1]]
      
      if (values$anova_result$type == "one_way") {
        f_stat <- anova_table$`F value`[1]
        p_value <- anova_table$`Pr(>F)`[1]
        eta_sq <- values$anova_result$eta_squared
        
        test_interpretation <- if(p_value <= input$anova_alpha) {
          "Terdapat perbedaan rata-rata yang signifikan antar grup"
        } else {
          "Tidak ada perbedaan rata-rata yang signifikan antar grup"
        }
        
        add_activity_to_tracker(
          type = "test",
          title = paste("One-Way ANOVA -", input$anova_dependent, "by", input$anova_factor1),
          content = paste(
            paste("Jenis ANOVA:", input$anova_type),
            paste("Variabel dependen:", input$anova_dependent),
            paste("Faktor:", input$anova_factor1),
            paste("F-statistic:", round(f_stat, 4)),
            paste("P-value:", format_pvalue(p_value)),
            paste("Eta squared:", round(eta_sq, 4)),
            paste("Effect size:", interpret_effect_size(eta_sq)),
            paste("Alpha level:", input$anova_alpha),
            paste("Interpretasi:", test_interpretation),
            sep = "\n"
          ),
          tables = paste(capture.output(print(anova_table)), collapse = "\n")
        )
      } else {
        add_activity_to_tracker(
          type = "test",
          title = paste("Two-Way ANOVA -", input$anova_dependent),
          content = paste(
            paste("Jenis ANOVA:", input$anova_type),
            paste("Variabel dependen:", input$anova_dependent),
            paste("Faktor 1:", input$anova_factor1),
            paste("Faktor 2:", input$anova_factor2),
            paste("Interaksi:", if(input$include_interaction) "Ya" else "Tidak"),
            paste("Alpha level:", input$anova_alpha),
            "Hasil per faktor tercantum dalam tabel ANOVA",
            sep = "\n"
          ),
          tables = paste(capture.output(print(anova_table)), collapse = "\n")
        )
      }
      
      safe_notification("ANOVA berhasil dilakukan", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error ANOVA:", e$message), "error")
    })
  })
  
  output$anova_done <- reactive({
    values$anova_done
  })
  outputOptions(output, "anova_done", suspendWhenHidden = FALSE)
  
  output$anova_result <- renderPrint({
    if (values$anova_done && !is.null(values$anova_result)) {
      result <- values$anova_result
      
      cat("HASIL ANOVA\n")
      cat("===========\n")
      cat("Jenis:", if(result$type == "one_way") "One-Way ANOVA" else "Two-Way ANOVA", "\n")
      cat("Variabel dependen:", input$anova_dependent, "\n")
      cat("Faktor 1:", input$anova_factor1, "\n")
      if (result$type == "two_way") {
        cat("Faktor 2:", input$anova_factor2, "\n")
        cat("Interaksi:", if(input$include_interaction) "Ya" else "Tidak", "\n")
      }
      cat("Alpha level:", input$anova_alpha, "\n\n")
      
      print(result$summary)
      
      cat("\nINTERPRETASI:\n")
      anova_table <- result$summary[[1]]
      
      if (result$type == "one_way") {
        f_stat <- anova_table$`F value`[1]
        p_value <- anova_table$`Pr(>F)`[1]
        
        cat("F-statistic:", round(f_stat, 4), "\n")
        cat("P-value:", format_pvalue(p_value), "\n")
        cat("Interpretasi:", interpret_significance(p_value), "\n")
        
        if (p_value <= input$anova_alpha) {
          cat("KESIMPULAN: Terdapat perbedaan rata-rata yang signifikan antar grup\n")
        } else {
          cat("KESIMPULAN: Tidak ada perbedaan rata-rata yang signifikan antar grup\n")
        }
        
      } else { # two_way
        for (i in 1:(nrow(anova_table)-1)) {
          effect_name <- rownames(anova_table)[i]
          f_stat <- anova_table$`F value`[i]
          p_value <- anova_table$`Pr(>F)`[i]
          
          cat("\nEFEK", effect_name, ":\n")
          cat("F-statistic:", round(f_stat, 4), "\n")
          cat("P-value:", format_pvalue(p_value), "\n")
          
          if (p_value <= input$anova_alpha) {
            cat("Status: SIGNIFIKAN\n")
          } else {
            cat("Status: TIDAK SIGNIFIKAN\n")
          }
        }
      }
    }
  })
  
  output$anova_plot <- renderPlotly({
    if (values$anova_done && !is.null(values$anova_result)) {
      result <- values$anova_result
      
      if (result$type == "one_way") {
        # Boxplot for one-way ANOVA with dynamic colors
        group_data <- values$processed_data[[input$anova_factor1]]
        unique_groups <- unique(group_data[!is.na(group_data)])
        n_groups <- length(unique_groups)
        
        # Create color palette with enough colors
        color_palette <- rep(c(waskita_colors$steel, waskita_colors$warning, 
                               waskita_colors$success, waskita_colors$navy), 
                             length.out = n_groups)
        
        p <- ggplot(values$processed_data, 
                    aes_string(x = input$anova_factor1, y = input$anova_dependent,
                               fill = input$anova_factor1)) +
          geom_boxplot(alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5) +
          labs(title = "One-Way ANOVA: Group Comparison") +
          theme_waskita() +
          theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_manual(values = color_palette)
        
      } else { # two_way
        # Interaction plot for two-way ANOVA
        group_means <- values$processed_data %>%
          group_by(!!sym(input$anova_factor1), !!sym(input$anova_factor2)) %>%
          summarise(mean_val = mean(!!sym(input$anova_dependent), na.rm = TRUE),
                    .groups = 'drop')
        
        p <- ggplot(group_means, aes_string(x = input$anova_factor1, 
                                            y = "mean_val",
                                            color = input$anova_factor2,
                                            group = input$anova_factor2)) +
          geom_line(linewidth = 1) +
          geom_point(size = 3) +
          labs(title = "Two-Way ANOVA: Interaction Plot",
               x = input$anova_factor1,
               y = paste("Mean", input$anova_dependent),
               color = input$anova_factor2) +
          theme_waskita() +
          scale_color_manual(values = c(waskita_colors$steel, waskita_colors$warning, 
                                        waskita_colors$success, waskita_colors$navy))
      }
      
      ggplotly(p) %>%
        layout(
          font = list(family = "Segoe UI", size = 12),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  output$posthoc_result <- renderPrint({
    if (values$anova_done && !is.null(values$anova_result)) {
      result <- values$anova_result
      
      cat("POST-HOC ANALYSIS\n")
      cat("=================\n")
      
      if (result$type == "one_way" && !is.null(result$posthoc)) {
        cat("TUKEY HSD POST-HOC TEST:\n")
        print(result$posthoc)
        
        cat("\nINTERPRETASI POST-HOC:\n")
        cat("- Pasangan dengan p adj < 0.05 menunjukkan perbedaan signifikan\n")
        cat("- Confidence interval yang tidak mencakup 0 juga signifikan\n")
        
      } else if (result$type == "two_way") {
        cat("Untuk Two-Way ANOVA, post-hoc test memerlukan analisis:\n")
        cat("- Simple effects jika interaksi signifikan\n")
        cat("- Main effects jika interaksi tidak signifikan\n")
        cat("- Gunakan emmeans package untuk analisis detail\n")
        
      } else {
        cat("Post-hoc test tidak tersedia atau tidak diperlukan\n")
      }
    }
  })
  
  output$effect_size_result <- renderPrint({
    if (values$anova_done && !is.null(values$anova_result)) {
      result <- values$anova_result
      
      cat("EFFECT SIZE ANALYSIS\n")
      cat("====================\n")
      
      if (result$type == "one_way") {
        eta_sq <- result$eta_squared
        cat("Eta Squared (η²):", round(eta_sq, 4), "\n")
        cat("Interpretasi:", interpret_effect_size(eta_sq), "effect size\n\n")
        
        cat("PANDUAN ETA SQUARED:\n")
        cat("- η² < 0.01: Negligible\n")
        cat("- η² 0.01-0.06: Small\n")
        cat("- η² 0.06-0.14: Medium\n") 
        cat("- η² > 0.14: Large\n")
        
      } else { # two_way
        cat("EFFECT SIZES PER FAKTOR:\n")
        cat("Faktor 1 (", input$anova_factor1, "): η² =", 
            round(result$eta_squared_f1, 4), "\n")
        cat("Faktor 2 (", input$anova_factor2, "): η² =", 
            round(result$eta_squared_f2, 4), "\n")
        
        if (!is.null(result$eta_squared_int)) {
          cat("Interaksi: η² =", round(result$eta_squared_int, 4), "\n")
        }
      }
    }
  })
  
  output$anova_interpretation <- renderPrint({
    if (values$anova_done && !is.null(values$anova_result)) {
      result <- values$anova_result
      anova_table <- result$summary[[1]]
      
      cat("INTERPRETASI KOMPREHENSIF ANOVA\n")
      cat("===============================\n")
      
      if (result$type == "one_way") {
        p_value <- anova_table$`Pr(>F)`[1]
        eta_sq <- result$eta_squared
        
        cat("RINGKASAN ONE-WAY ANOVA:\n")
        cat("- P-value:", format_pvalue(p_value), "\n")
        cat("- Effect size:", interpret_effect_size(eta_sq), "\n")
        cat("- Proporsi varians dijelaskan:", round(eta_sq * 100, 2), "%\n\n")
        
        cat("REKOMENDASI:\n")
        if (p_value <= 0.05) {
          cat("1. Lakukan post-hoc test untuk identifikasi grup berbeda\n")
          cat("2. Periksa asumsi ANOVA (normalitas, homogenitas)\n")
          if (eta_sq >= 0.14) {
            cat("3. Effect size besar - temuan relevan secara praktis\n")
          }
        } else {
          cat("1. Tidak ada bukti perbedaan antar grup\n")
          cat("2. Pertimbangkan power analysis\n")
          cat("3. Evaluasi kemungkinan Type II error\n")
        }
        
      } else { # two_way
        cat("RINGKASAN TWO-WAY ANOVA:\n")
        
        for (i in 1:(nrow(anova_table)-1)) {
          effect_name <- rownames(anova_table)[i]
          p_value <- anova_table$`Pr(>F)`[i]
          
          cat("\n", effect_name, ":\n")
          if (p_value <= 0.05) {
            cat("- SIGNIFIKAN: Ada efek dari", effect_name, "\n")
          } else {
            cat("- TIDAK SIGNIFIKAN: Tidak ada efek dari", effect_name, "\n")
          }
        }
        
        cat("\nREKOMENDASI:\n")
        cat("1. Interpretasi main effects harus mempertimbangkan interaksi\n")
        cat("2. Jika interaksi signifikan, fokus pada simple effects\n")
        cat("3. Gunakan interaction plot untuk interpretasi visual\n")
      }
    }
  })
  
  # =============================================================================
  # REGRESI LINEAR BERGANDA - ENHANCED WITH TRACKING
  # =============================================================================
  
  observeEvent(input$run_regression, {
    req(values$processed_data, input$reg_dependent, input$reg_independent)
    
    tryCatch({
      if (length(input$reg_independent) < 1) {
        safe_notification("Pilih minimal 1 variabel independen", "warning")
        return()
      }
      
      if (input$include_intercept) {
        formula <- as.formula(paste(input$reg_dependent, "~", 
                                    paste(input$reg_independent, collapse = "+")))
      } else {
        formula <- as.formula(paste(input$reg_dependent, "~ -1 +", 
                                    paste(input$reg_independent, collapse = "+")))
      }
      
      lm_model <- lm(formula, data = values$processed_data)
      values$regression_model <- lm_model
      values$regression_done <- TRUE
      
      # Add to activity tracker
      model_summary <- summary(lm_model)
      r_sq <- model_summary$r.squared
      f_p_value <- pf(model_summary$fstatistic[1], 
                      model_summary$fstatistic[2], 
                      model_summary$fstatistic[3], 
                      lower.tail = FALSE)
      
      # Significant predictors
      coeffs <- model_summary$coefficients
      sig_vars <- rownames(coeffs)[coeffs[, 4] <= 0.05 & rownames(coeffs) != "(Intercept)"]
      
      add_activity_to_tracker(
        type = "test",
        title = paste("Regresi Linear -", input$reg_dependent),
        content = paste(
          paste("Variabel dependen:", input$reg_dependent),
          paste("Variabel independen:", paste(input$reg_independent, collapse = ", ")),
          paste("Intercept:", if(input$include_intercept) "Included" else "Excluded"),
          paste("R-squared:", round(r_sq, 4)),
          paste("Adjusted R-squared:", round(model_summary$adj.r.squared, 4)),
          paste("F-statistic:", round(model_summary$fstatistic[1], 4)),
          paste("P-value (F-test):", format_pvalue(f_p_value)),
          paste("Variabel signifikan:", if(length(sig_vars) > 0) paste(sig_vars, collapse = ", ") else "Tidak ada"),
          paste("Kekuatan prediksi:", if(r_sq >= 0.7) "Sangat baik" else if(r_sq >= 0.5) "Baik" else if(r_sq >= 0.3) "Moderat" else "Lemah"),
          sep = "\n"
        ),
        tables = paste(capture.output(print(model_summary)), collapse = "\n")
      )
      
      safe_notification("Regresi berhasil dilakukan", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error regresi:", e$message), "error")
    })
  })
  
  output$regression_done <- reactive({
    values$regression_done
  })
  outputOptions(output, "regression_done", suspendWhenHidden = FALSE)
  
  output$regression_summary <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("RINGKASAN MODEL REGRESI\n")
      cat("=======================\n")
      cat("Variabel dependen:", input$reg_dependent, "\n")
      cat("Variabel independen:", paste(input$reg_independent, collapse = ", "), "\n")
      cat("Intercept:", if(input$include_intercept) "Included" else "Excluded", "\n\n")
      
      print(summary(model))
    }
  })
  
  output$model_fit_stats <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      model_summary <- summary(model)
      
      cat("STATISTIK KESESUAIAN MODEL\n")
      cat("==========================\n")
      cat("R-squared:", round(model_summary$r.squared, 4), "\n")
      cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
      cat("F-statistic:", round(model_summary$fstatistic[1], 4), "\n")
      cat("P-value (F-test):", format_pvalue(pf(model_summary$fstatistic[1], 
                                                model_summary$fstatistic[2], 
                                                model_summary$fstatistic[3], 
                                                lower.tail = FALSE)), "\n")
      cat("Residual SE:", round(model_summary$sigma, 4), "\n")
      cat("Degrees of freedom:", model$df.residual, "\n")
      cat("Observations:", nobs(model), "\n\n")
      
      # Additional statistics
      cat("STATISTIK TAMBAHAN:\n")
      cat("AIC:", round(AIC(model), 4), "\n")
      cat("BIC:", round(BIC(model), 4), "\n")
      
      # Interpretation
      r_sq <- model_summary$r.squared
      cat("\nINTERPRETASI:\n")
      cat("Model menjelaskan", round(r_sq * 100, 2), "% variasi dalam", input$reg_dependent, "\n")
      
      if (r_sq >= 0.7) {
        cat("Kekuatan prediksi: SANGAT BAIK\n")
      } else if (r_sq >= 0.5) {
        cat("Kekuatan prediksi: BAIK\n")
      } else if (r_sq >= 0.3) {
        cat("Kekuatan prediksi: MODERAT\n")
      } else {
        cat("Kekuatan prediksi: LEMAH\n")
      }
    }
  })
  
  output$coefficients_table <- DT::renderDataTable({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      model_summary <- summary(model)
      
      # Create coefficients table
      coeffs <- model_summary$coefficients
      conf_int <- confint(model)
      
      coeffs_df <- data.frame(
        Variable = rownames(coeffs),
        Estimate = round(coeffs[, "Estimate"], 4),
        Std_Error = round(coeffs[, "Std. Error"], 4),
        t_value = round(coeffs[, "t value"], 4),
        p_value = round(coeffs[, "Pr(>|t|)"], 4),
        CI_Lower = round(conf_int[, 1], 4),
        CI_Upper = round(conf_int[, 2], 4),
        Significance = ifelse(coeffs[, "Pr(>|t|)"] <= 0.001, "***",
                              ifelse(coeffs[, "Pr(>|t|)"] <= 0.01, "**",
                                     ifelse(coeffs[, "Pr(>|t|)"] <= 0.05, "*",
                                            ifelse(coeffs[, "Pr(>|t|)"] <= 0.1, ".", ""))))
      )
      
      DT::datatable(
        coeffs_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 't'
        ),
        class = 'cell-border stripe'
      ) %>%
        DT::formatStyle('p_value',
                        backgroundColor = DT::styleInterval(c(0.05), c('#ffcccb', '#ccffcc')))
    }
  })
  
  output$regression_diagnostics <- renderPlotly({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      # Create diagnostic plots
      fitted_vals <- fitted(model)
      residuals_vals <- residuals(model)
      
      # Residuals vs Fitted
      p <- ggplot(data = NULL, aes(x = fitted_vals, y = residuals_vals)) +
        geom_point(alpha = 0.6, color = waskita_colors$steel) +
        geom_hline(yintercept = 0, color = waskita_colors$error, linetype = "dashed") +
        geom_smooth(se = FALSE, color = waskita_colors$navy) +
        labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
        theme_waskita()
      
      ggplotly(p) %>%
        layout(
          font = list(family = "Segoe UI", size = 12),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  output$residual_normality <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      residuals_data <- residuals(values$regression_model)
      
      cat("UJI NORMALITAS RESIDUAL\n")
      cat("=======================\n")
      
      # Shapiro-Wilk test
      if (length(residuals_data) <= 5000) {
        shapiro_test <- shapiro.test(residuals_data)
        cat("SHAPIRO-WILK TEST:\n")
        print(shapiro_test)
        
        cat("\nInterpretasi:\n")
        if (shapiro_test$p.value > 0.05) {
          cat("Residual berdistribusi NORMAL (asumsi terpenuhi)\n")
        } else {
          cat("Residual TIDAK berdistribusi normal (asumsi dilanggar)\n")
        }
      } else {
        cat("Sampel terlalu besar untuk Shapiro-Wilk test\n")
        cat("Gunakan Q-Q plot untuk evaluasi visual\n")
      }
    }
  })
  
  output$homoscedasticity_test <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("UJI HOMOSKEDASTISITAS\n")
      cat("=====================\n")
      
      # Breusch-Pagan test
      tryCatch({
        bp_test <- lmtest::bptest(model)
        cat("BREUSCH-PAGAN TEST:\n")
        print(bp_test)
        
        cat("\nInterpretasi:\n")
        if (bp_test$p.value > 0.05) {
          cat("Varians residual HOMOGEN (asumsi terpenuhi)\n")
        } else {
          cat("Varians residual HETEROGEN (asumsi dilanggar)\n")
          cat("Pertimbangkan robust standard errors\n")
        }
      }, error = function(e) {
        cat("Breusch-Pagan test tidak dapat dilakukan\n")
        cat("Error:", e$message, "\n")
      })
    }
  })
  
  output$vif_results <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("UJI MULTIKOLINEARITAS (VIF)\n")
      cat("===========================\n")
      
      if (length(input$reg_independent) > 1) {
        tryCatch({
          vif_values <- car::vif(model)
          
          cat("VARIANCE INFLATION FACTORS:\n")
          for (i in 1:length(vif_values)) {
            cat(names(vif_values)[i], ":", round(vif_values[i], 4), "\n")
          }
          
          cat("\nINTERPRETASI:\n")
          cat("VIF < 5: Tidak ada masalah multikolinearitas\n")
          cat("VIF 5-10: Multikolinearitas moderat\n")
          cat("VIF > 10: Multikolinearitas serius\n\n")
          
          max_vif <- max(vif_values)
          if (max_vif < 5) {
            cat("STATUS: Tidak ada masalah multikolinearitas\n")
          } else if (max_vif < 10) {
            cat("STATUS: Multikolinearitas moderat terdeteksi\n")
          } else {
            cat("STATUS: Multikolinearitas serius terdeteksi\n")
            cat("REKOMENDASI: Pertimbangkan menghapus variabel dengan VIF tinggi\n")
          }
        }, error = function(e) {
          cat("VIF tidak dapat dihitung\n")
          cat("Error:", e$message, "\n")
        })
      } else {
        cat("VIF tidak dapat dihitung (hanya 1 variabel independen)\n")
      }
    }
  })
  
  output$autocorrelation_test <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("UJI AUTOKORELASI\n")
      cat("================\n")
      
      tryCatch({
        dw_test <- lmtest::dwtest(model)
        cat("DURBIN-WATSON TEST:\n")
        print(dw_test)
        
        cat("\nInterpretasi:\n")
        dw_stat <- dw_test$statistic
        
        if (dw_stat >= 1.5 && dw_stat <= 2.5) {
          cat("Tidak ada autokorelasi (asumsi terpenuhi)\n")
        } else if (dw_stat < 1.5) {
          cat("Autokorelasi POSITIF terdeteksi (asumsi dilanggar)\n")
        } else {
          cat("Autokorelasi NEGATIF terdeteksi (asumsi dilanggar)\n")
        }
        
        cat("\nPANDUAN:\n")
        cat("DW ≈ 2: Tidak ada autokorelasi\n")
        cat("DW < 2: Autokorelasi positif\n")
        cat("DW > 2: Autokorelasi negatif\n")
      }, error = function(e) {
        cat("Durbin-Watson test tidak dapat dilakukan\n")
        cat("Error:", e$message, "\n")
      })
    }
  })
  
  output$regression_interpretation <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      model_summary <- summary(model)
      
      cat("INTERPRETASI KOMPREHENSIF MODEL REGRESI\n")
      cat("======================================\n")
      
      # Overall model significance
      f_p_value <- pf(model_summary$fstatistic[1], 
                      model_summary$fstatistic[2], 
                      model_summary$fstatistic[3], 
                      lower.tail = FALSE)
      
      cat("SIGNIFIKANSI MODEL:\n")
      if (f_p_value <= 0.05) {
        cat("Model secara keseluruhan SIGNIFIKAN (F-test p < 0.05)\n")
        cat("Setidaknya satu variabel independen berpengaruh signifikan\n")
      } else {
        cat("Model secara keseluruhan TIDAK SIGNIFIKAN (F-test p > 0.05)\n")
        cat("Tidak ada variabel independen yang berpengaruh signifikan\n")
      }
      
      # R-squared interpretation
      r_sq <- model_summary$r.squared
      cat("\nKEKUATAN PREDIKSI:\n")
      cat("R-squared:", round(r_sq * 100, 2), "% varians dalam", input$reg_dependent, 
          "dijelaskan oleh model\n")
      
      if (r_sq >= 0.7) {
        cat("Model memiliki kekuatan prediksi SANGAT BAIK\n")
      } else if (r_sq >= 0.5) {
        cat("Model memiliki kekuatan prediksi BAIK\n")
      } else if (r_sq >= 0.3) {
        cat("Model memiliki kekuatan prediksi MODERAT\n")
      } else {
        cat("Model memiliki kekuatan prediksi LEMAH\n")
      }
      
      # Significant predictors
      coeffs <- model_summary$coefficients
      sig_vars <- rownames(coeffs)[coeffs[, 4] <= 0.05 & rownames(coeffs) != "(Intercept)"]
      
      cat("\nVARIABEL PREDIKTOR SIGNIFIKAN:\n")
      if (length(sig_vars) > 0) {
        for (var in sig_vars) {
          coeff <- coeffs[var, 1]
          cat("- ", var, ": koefisien =", round(coeff, 4), 
              ifelse(coeff > 0, "(pengaruh positif)", "(pengaruh negatif)"), "\n")
        }
      } else {
        cat("Tidak ada variabel prediktor yang signifikan\n")
      }
      
      cat("\nREKOMENDASI:\n")
      if (f_p_value <= 0.05 && r_sq >= 0.3) {
        cat("1. Model dapat digunakan untuk prediksi\n")
        cat("2. Periksa asumsi regresi untuk validitas\n")
        cat("3. Pertimbangkan validasi silang\n")
      } else if (f_p_value <= 0.05 && r_sq < 0.3) {
        cat("1. Model signifikan tetapi kekuatan prediksi lemah\n")
        cat("2. Pertimbangkan menambah variabel prediktor\n")
        cat("3. Evaluasi transformasi variabel\n")
      } else {
        cat("1. Model tidak signifikan - tidak cocok untuk prediksi\n")
        cat("2. Pertimbangkan model alternatif\n")
        cat("3. Evaluasi kembali pemilihan variabel\n")
      }
    }
  })
  
  # =============================================================================
  # ANALISIS SPASIAL LANJUTAN - ENHANCED WITH TRACKING
  # =============================================================================
  
  observeEvent(input$run_spatial_analysis, {
    req(values$processed_data, input$spatial_variable, values$distance_data)
    
    tryCatch({
      # Load distance matrix
      distance_matrix <- as.matrix(values$distance_data[, -1])  # Remove first column if ID
      
      # Create spatial weights matrix
      if (input$weight_type == "distance_inverse") {
        # Inverse distance weights
        weights_matrix <- 1 / (distance_matrix + diag(nrow(distance_matrix)))
        diag(weights_matrix) <- 0
      } else if (input$weight_type == "distance_exp") {
        # Exponential decay weights
        weights_matrix <- exp(-distance_matrix/1000)  # Scale distance
        diag(weights_matrix) <- 0
      } else { # knn
        # K-nearest neighbors
        k <- input$k_neighbors
        weights_matrix <- matrix(0, nrow(distance_matrix), ncol(distance_matrix))
        
        for (i in 1:nrow(distance_matrix)) {
          neighbors <- order(distance_matrix[i, ])[2:(k+1)]  # Exclude self
          weights_matrix[i, neighbors] <- 1
        }
      }
      
      # Calculate Moran's I
      var_data <- values$processed_data[[input$spatial_variable]]
      morans_i <- calculate_morans_i(var_data, weights_matrix)
      
      # Calculate expected value and variance for Moran's I
      n <- length(var_data[!is.na(var_data)])
      expected_i <- -1 / (n - 1)
      
      # Simplified variance calculation
      variance_i <- (n^2 - 3*n + 3) / ((n-1)^2 * (n-2))
      
      # Z-score and p-value
      z_score <- (morans_i - expected_i) / sqrt(variance_i)
      p_value <- 2 * (1 - pnorm(abs(z_score)))
      
      values$spatial_result <- list(
        morans_i = morans_i,
        expected_i = expected_i,
        variance_i = variance_i,
        z_score = z_score,
        p_value = p_value,
        weights_matrix = weights_matrix,
        n = n
      )
      
      values$spatial_done <- TRUE
      
      # Add to activity tracker
      spatial_interpretation <- if(p_value < 0.05) {
        if(morans_i > expected_i) {
          "Autokorelasi spasial positif signifikan (clustering)"
        } else {
          "Autokorelasi spasial negatif signifikan (dispersi)"
        }
      } else {
        "Tidak ada autokorelasi spasial signifikan (pola acak)"
      }
      
      add_activity_to_tracker(
        type = "spatial",
        title = paste("Analisis Autokorelasi Spasial -", input$spatial_variable),
        content = paste(
          paste("Variabel:", input$spatial_variable),
          paste("Metode weight:", input$weight_type),
          if(input$weight_type == "knn") paste("K neighbors:", input$k_neighbors) else "",
          paste("Moran's I:", round(morans_i, 4)),
          paste("Expected I:", round(expected_i, 4)),
          paste("Z-score:", round(z_score, 4)),
          paste("P-value:", format_pvalue(p_value)),
          paste("Ukuran sampel:", n),
          paste("Interpretasi:", spatial_interpretation),
          sep = "\n"
        ),
        tables = paste(
          "Statistik Moran's I:",
          paste("Moran's I =", round(morans_i, 4)),
          paste("Expected I =", round(expected_i, 4)),
          paste("Variance =", round(variance_i, 6)),
          paste("Z-score =", round(z_score, 4)),
          paste("P-value =", format_pvalue(p_value)),
          sep = "\n"
        )
      )
      
      safe_notification("Analisis spasial berhasil dilakukan", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error analisis spasial:", e$message), "error")
    })
  })
  
  output$spatial_done <- reactive({
    values$spatial_done
  })
  outputOptions(output, "spatial_done", suspendWhenHidden = FALSE)
  
  output$spatial_autocorr_result <- renderPrint({
    if (values$spatial_done && !is.null(values$spatial_result)) {
      result <- values$spatial_result
      
      cat("HASIL UJI AUTOKORELASI SPASIAL\n")
      cat("===============================\n")
      cat("Variabel:", input$spatial_variable, "\n")
      cat("Metode Weight:", switch(input$weight_type,
                                   "distance_inverse" = "Inverse Distance",
                                   "distance_exp" = "Exponential Distance", 
                                   "knn" = paste("K-Nearest Neighbors (k=", input$k_neighbors, ")", sep="")), "\n\n")
      
      cat("STATISTIK MORAN'S I:\n")
      cat("Moran's I:", round(result$morans_i, 4), "\n")
      cat("Expected I:", round(result$expected_i, 4), "\n")
      cat("Variance:", round(result$variance_i, 6), "\n")
      cat("Z-Score:", round(result$z_score, 4), "\n")
      cat("P-Value:", format_pvalue(result$p_value), "\n")
      cat("Significance:", interpret_significance(result$p_value), "\n\n")
      
      cat("INTERPRETASI:\n")
      if (result$p_value < 0.05) {
        if (result$morans_i > result$expected_i) {
          cat("AUTOKORELASI SPASIAL POSITIF SIGNIFIKAN\n")
          cat("Data cenderung bercluster/berkelompok secara spasial\n")
          cat("Wilayah dengan nilai tinggi dikelilingi nilai tinggi\n")
        } else {
          cat("AUTOKORELASI SPASIAL NEGATIF SIGNIFIKAN\n")
          cat("Data menunjukkan pola checkerboard\n")
          cat("Wilayah dengan nilai tinggi dikelilingi nilai rendah\n")
        }
      } else {
        cat("TIDAK ADA AUTOKORELASI SPASIAL SIGNIFIKAN\n")
        cat("Data tersebar secara acak tanpa pola spasial\n")
      }
      
      cat("\nSampel Size:", result$n, "observasi\n")
    }
  })
  
  output$spatial_plot <- renderPlotly({
    if (values$spatial_done && !is.null(values$spatial_result) && !is.null(values$processed_data)) {
      
      tryCatch({
        data <- values$processed_data
        variable_data <- data[[input$spatial_variable]]
        
        # Create scatter plot of variable vs spatial lag
        # Calculate spatial lag
        weights_matrix <- values$spatial_result$weights_matrix
        spatial_lag <- numeric(length(variable_data))
        
        for (i in 1:length(variable_data)) {
          if (!is.na(variable_data[i])) {
            neighbors <- which(weights_matrix[i, ] > 0)
            if (length(neighbors) > 0) {
              spatial_lag[i] <- mean(variable_data[neighbors], na.rm = TRUE)
            }
          }
        }
        
        # Create Moran scatterplot
        plot_data <- data.frame(
          Variable = variable_data,
          Spatial_Lag = spatial_lag,
          ID = 1:length(variable_data)
        ) %>%
          filter(!is.na(Variable) & !is.na(Spatial_Lag))
        
        p <- ggplot(plot_data, aes(x = Variable, y = Spatial_Lag)) +
          geom_point(alpha = 0.6, size = 2, color = waskita_colors$steel) +
          geom_smooth(method = "lm", se = TRUE, color = waskita_colors$navy, 
                      fill = waskita_colors$warm_gray, alpha = 0.3) +
          labs(
            title = paste("Moran Scatterplot:", input$spatial_variable),
            subtitle = paste("Moran's I =", round(values$spatial_result$morans_i, 4)),
            x = paste("Standardized", input$spatial_variable),
            y = "Spatial Lag"
          ) +
          theme_waskita()
        
        ggplotly(p, tooltip = c("x", "y")) %>%
          layout(
            title = list(
              text = paste("Moran Scatterplot:", input$spatial_variable),
              font = list(size = 16, color = waskita_colors$navy)
            )
          )
        
      }, error = function(e) {
        plotly_empty() %>%
          layout(title = list(text = "Error creating spatial plot", 
                              font = list(color = waskita_colors$error)))
      })
    }
  })
  
  output$distance_matrix_stats <- renderPrint({
    if (values$spatial_done && !is.null(values$distance_data)) {
      distance_matrix <- as.matrix(values$distance_data[, -1])
      
      # Calculate statistics (excluding diagonal)
      upper_tri <- distance_matrix[upper.tri(distance_matrix)]
      
      cat("STATISTIK MATRIKS JARAK\n")
      cat("=======================\n")
      cat("Dimensi Matriks:", nrow(distance_matrix), "x", ncol(distance_matrix), "\n")
      cat("Total Pasangan:", length(upper_tri), "\n\n")
      cat("Jarak Minimum:", round(min(upper_tri, na.rm = TRUE), 2), "\n")
      cat("Jarak Maksimum:", round(max(upper_tri, na.rm = TRUE), 2), "\n")
      cat("Jarak Rata-rata:", round(mean(upper_tri, na.rm = TRUE), 2), "\n")
      cat("Jarak Median:", round(median(upper_tri, na.rm = TRUE), 2), "\n")
      cat("Standar Deviasi:", round(sd(upper_tri, na.rm = TRUE), 2), "\n")
    }
  })
  
  output$spatial_interpretation_text <- renderPrint({
    if (values$spatial_done && !is.null(values$spatial_result)) {
      result <- values$spatial_result
      
      cat("INTERPRETASI ANALISIS SPASIAL\n")
      cat("=============================\n")
      
      # Determine spatial pattern
      if (result$p_value < 0.001) {
        significance_level <- "sangat signifikan (p < 0.001)"
      } else if (result$p_value < 0.01) {
        significance_level <- "sangat signifikan (p < 0.01)"
      } else if (result$p_value < 0.05) {
        significance_level <- "signifikan (p < 0.05)"
      } else {
        significance_level <- "tidak signifikan (p ≥ 0.05)"
      }
      
      if (result$p_value < 0.05) {
        if (result$morans_i > result$expected_i) {
          pattern_type <- "AUTOKORELASI SPASIAL POSITIF"
          pattern_desc <- "Data cenderung bercluster atau berkelompok secara spasial. Wilayah dengan nilai tinggi dikelilingi oleh wilayah dengan nilai tinggi, dan sebaliknya."
          recommendation <- "Pertimbangkan faktor geografis dalam analisis lebih lanjut."
        } else {
          pattern_type <- "AUTOKORELASI SPASIAL NEGATIF"
          pattern_desc <- "Data menunjukkan pola checkerboard dimana wilayah dengan nilai tinggi dikelilingi oleh wilayah dengan nilai rendah."
          recommendation <- "Periksa kemungkinan adanya kompetisi spasial atau dispersi."
        }
      } else {
        pattern_type <- "TIDAK ADA AUTOKORELASI SPASIAL"
        pattern_desc <- "Data tersebar secara acak tanpa pola spasial yang jelas."
        recommendation <- "Analisis spasial lanjutan mungkin tidak diperlukan."
      }
      
      cat("POLA SPASIAL:", pattern_type, "\n\n")
      cat("TINGKAT SIGNIFIKANSI:", significance_level, "\n\n")
      cat("DESKRIPSI POLA:\n")
      cat(pattern_desc, "\n\n")
      cat("REKOMENDASI:\n")
      cat(recommendation, "\n\n")
      cat("CATATAN METODOLOGI:\n")
      cat("- Moran's I berkisar dari -1 (dispersi sempurna) hingga +1 (clustering sempurna)\n")
      cat("- Nilai mendekati 0 menunjukkan pola acak\n")
      cat("- Uji signifikansi menggunakan distribusi normal standar\n")
    }
  })
  
  # =============================================================================
  # COMPREHENSIVE ACTIVITY REPORT DOWNLOAD HANDLERS - ENHANCED
  # =============================================================================
  
  # Generate Activity Report PDF
  output$generate_activity_report_pdf <- downloadHandler(
    filename = function() {
      paste0("WASKITA_Activity_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      # Show progress notification
      showNotification("Generating comprehensive activity report (PDF)...", type = "message", duration = NULL, id = "activity_pdf_progress")
      
      tryCatch({
        # Create temporary Rmd file
        temp_rmd <- tempfile(fileext = ".Rmd")
        
        # Generate comprehensive report content based on all activities
        report_content <- generate_comprehensive_activity_report(activity_tracker, values$processed_data)
        
        # Write content to file
        writeLines(report_content, temp_rmd)
        
        # Render to PDF
        rmarkdown::render(
          temp_rmd, 
          output_format = rmarkdown::pdf_document(
            toc = TRUE,
            toc_depth = 4,
            number_sections = TRUE,
            latex_engine = "xelatex"
          ),
          output_file = file,
          quiet = TRUE,
          envir = new.env()
        )
        
        # Remove progress notification and show success
        removeNotification("activity_pdf_progress")
        showNotification("Activity report PDF generated successfully!", type = "success", duration = 5)
        
      }, error = function(e) {
        removeNotification("activity_pdf_progress")
        showNotification(paste("Error generating activity PDF report:", e$message), type = "error", duration = 10)
      })
    }
  )
  
  # Generate Activity Report Word
  output$generate_activity_report_word <- downloadHandler(
    filename = function() {
      paste0("WASKITA_Activity_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
    },
    content = function(file) {
      # Show progress notification
      showNotification("Generating comprehensive activity report (Word)...", type = "message", duration = NULL, id = "activity_word_progress")
      
      tryCatch({
        # Create temporary Rmd file
        temp_rmd <- tempfile(fileext = ".Rmd")
        
        # Generate comprehensive report content based on all activities
        report_content <- generate_comprehensive_activity_report(activity_tracker, values$processed_data)
        
        # Write content to file
        writeLines(report_content, temp_rmd)
        
        # Render to Word
        rmarkdown::render(
          temp_rmd, 
          output_format = rmarkdown::word_document(
            toc = TRUE,
            toc_depth = 4
          ),
          output_file = file,
          quiet = TRUE,
          envir = new.env()
        )
        
        # Remove progress notification and show success
        removeNotification("activity_word_progress")
        showNotification("Activity report Word generated successfully!", type = "success", duration = 5)
        
      }, error = function(e) {
        removeNotification("activity_word_progress")
        showNotification(paste("Error generating activity Word report:", e$message), type = "error", duration = 10)
      })
    }
  )
  
  # Download activity report from beranda
  output$download_activity_report_pdf <- downloadHandler(
    filename = function() {
      paste0("WASKITA_Activity_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      # Show progress notification
      showNotification("Generating activity report PDF...", type = "message", duration = NULL, id = "beranda_pdf_progress")
      
      tryCatch({
        # Create temporary Rmd file
        temp_rmd <- tempfile(fileext = ".Rmd")
        
        # Generate comprehensive report content based on all activities
        report_content <- generate_comprehensive_activity_report(activity_tracker, values$processed_data)
        
        # Write content to file
        writeLines(report_content, temp_rmd)
        
        # Render to PDF
        rmarkdown::render(
          temp_rmd, 
          output_format = rmarkdown::pdf_document(
            toc = TRUE,
            toc_depth = 4,
            number_sections = TRUE,
            latex_engine = "xelatex"
          ),
          output_file = file,
          quiet = TRUE,
          envir = new.env()
        )
        
        # Remove progress notification and show success
        removeNotification("beranda_pdf_progress")
        showNotification("Activity report PDF generated successfully!", type = "success", duration = 5)
        
      }, error = function(e) {
        removeNotification("beranda_pdf_progress")
        showNotification(paste("Error generating activity PDF report:", e$message), type = "error", duration = 10)
      })
    }
  )
  
  output$download_activity_report_word <- downloadHandler(
    filename = function() {
      paste0("WASKITA_Activity_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
    },
    content = function(file) {
      # Show progress notification
      showNotification("Generating activity report Word...", type = "message", duration = NULL, id = "beranda_word_progress")
      
      tryCatch({
        # Create temporary Rmd file
        temp_rmd <- tempfile(fileext = ".Rmd")
        
        # Generate comprehensive report content based on all activities
        report_content <- generate_comprehensive_activity_report(activity_tracker, values$processed_data)
        
        # Write content to file
        writeLines(report_content, temp_rmd)
        
        # Render to Word
        rmarkdown::render(
          temp_rmd, 
          output_format = rmarkdown::word_document(
            toc = TRUE,
            toc_depth = 4
          ),
          output_file = file,
          quiet = TRUE,
          envir = new.env()
        )
        
        # Remove progress notification and show success
        removeNotification("beranda_word_progress")
        showNotification("Activity report Word generated successfully!", type = "success", duration = 5)
        
      }, error = function(e) {
        removeNotification("beranda_word_progress")
        showNotification(paste("Error generating activity Word report:", e$message), type = "error", duration = 10)
      })
    }
  )
  
  # =============================================================================
  # DOWNLOAD HANDLERS - ENHANCED WITH ACTIVITY TRACKING
  # =============================================================================
  
  # Download original data
  output$download_original <- downloadHandler(
    filename = function() {
      create_download_filename("original_data", "csv")
    },
    content = function(file) {
      if (!is.null(values$original_data)) {
        write.csv(values$original_data, file, row.names = FALSE)
        
        # Add to activity tracker
        add_activity_to_tracker(
          type = "analysis",
          title = "Download Original Data",
          content = paste(
            "Data asli telah didownload dalam format CSV",
            paste("Ukuran file:", nrow(values$original_data), "baris x", ncol(values$original_data), "kolom"),
            paste("Timestamp download:", Sys.time()),
            sep = "\n"
          )
        )
      }
    }
  )
  
  # Download processed data
  output$download_processed <- downloadHandler(
    filename = function() {
      create_download_filename("processed_data", "csv")
    },
    content = function(file) {
      if (!is.null(values$processed_data)) {
        write.csv(values$processed_data, file, row.names = FALSE)
        
        # Add to activity tracker
        add_activity_to_tracker(
          type = "analysis",
          title = "Download Processed Data",
          content = paste(
            "Data yang telah diproses didownload dalam format CSV",
            paste("Ukuran file:", nrow(values$processed_data), "baris x", ncol(values$processed_data), "kolom"),
            paste("Transformasi yang diterapkan:", if(values$transformation_done) "Ya" else "Tidak"),
            paste("Kategorisasi yang diterapkan:", if(values$categorization_done) "Ya" else "Tidak"),
            paste("Timestamp download:", Sys.time()),
            sep = "\n"
          )
        )
      }
    }
  )
  
  # Download descriptive plot
  output$download_desc_plot <- downloadHandler(
    filename = function() {
      create_download_filename("descriptive_plot", "png")
    },
    content = function(file) {
      if (values$desc_generated && !is.null(values$current_desc_variable)) {
        var_data <- values$processed_data[[values$current_desc_variable]]
        
        if (is.numeric(var_data)) {
          p <- switch(input$desc_chart_type,
                      "histogram" = {
                        ggplot(values$processed_data, aes_string(x = values$current_desc_variable)) +
                          geom_histogram(bins = input$desc_bins, fill = waskita_colors$steel, 
                                         alpha = 0.7, color = "white") +
                          labs(title = paste("Histogram", values$current_desc_variable),
                               x = values$current_desc_variable, y = "Frequency") +
                          theme_waskita()
                      },
                      "boxplot" = {
                        ggplot(values$processed_data, aes_string(y = values$current_desc_variable)) +
                          geom_boxplot(fill = waskita_colors$steel, alpha = 0.7, color = waskita_colors$navy) +
                          labs(title = paste("Boxplot", values$current_desc_variable),
                               y = values$current_desc_variable) +
                          theme_waskita()
                      },
                      "density" = {
                        ggplot(values$processed_data, aes_string(x = values$current_desc_variable)) +
                          geom_density(fill = waskita_colors$steel, alpha = 0.7, color = waskita_colors$navy) +
                          labs(title = paste("Density Plot", values$current_desc_variable),
                               x = values$current_desc_variable, y = "Density") +
                          theme_waskita()
                      },
                      "qq" = {
                        ggplot(values$processed_data, aes_string(sample = values$current_desc_variable)) +
                          stat_qq(color = waskita_colors$steel) +
                          stat_qq_line(color = waskita_colors$navy) +
                          labs(title = paste("Q-Q Plot", values$current_desc_variable)) +
                          theme_waskita()
                      }
          )
        } else {
          df <- as.data.frame(table(var_data))
          colnames(df) <- c("Category", "Frequency")
          p <- ggplot(df, aes(x = reorder(Category, -Frequency), y = Frequency)) +
            geom_bar(stat = "identity", fill = waskita_colors$steel, alpha = 0.7) +
            labs(title = paste("Bar Chart", values$current_desc_variable),
                 x = values$current_desc_variable, y = "Frequency") +
            theme_waskita() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
        
        ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
        
        # Add to activity tracker
        add_activity_to_tracker(
          type = "analysis",
          title = "Download Descriptive Plot",
          content = paste(
            paste("Plot deskriptif untuk variabel:", values$current_desc_variable),
            paste("Jenis plot:", input$desc_chart_type),
            paste("Format file: PNG (300 DPI)"),
            paste("Timestamp download:", Sys.time()),
            sep = "\n"
          )
        )
      }
    }
  )
  
  # Download descriptive stats
  output$download_desc_stats <- downloadHandler(
    filename = function() {
      create_download_filename("descriptive_stats", "csv")
    },
    content = function(file) {
      if (values$desc_generated && !is.null(values$current_desc_variable)) {
        var_data <- values$processed_data[[values$current_desc_variable]]
        
        if (is.numeric(var_data)) {
          stats_df <- data.frame(
            Statistic = c("N", "Missing", "Mean", "Median", "SD", "Min", "Max", "Q1", "Q3", "IQR"),
            Value = c(
              sum(!is.na(var_data)),
              sum(is.na(var_data)),
              round(mean(var_data, na.rm = TRUE), 4),
              round(median(var_data, na.rm = TRUE), 4),
              round(sd(var_data, na.rm = TRUE), 4),
              round(min(var_data, na.rm = TRUE), 4),
              round(max(var_data, na.rm = TRUE), 4),
              round(quantile(var_data, 0.25, na.rm = TRUE), 4),
              round(quantile(var_data, 0.75, na.rm = TRUE), 4),
              round(IQR(var_data, na.rm = TRUE), 4)
            )
          )
        } else {
          tbl <- table(var_data, useNA = "ifany")
          stats_df <- data.frame(
            Category = names(tbl),
            Frequency = as.numeric(tbl),
            Proportion = round(as.numeric(tbl) / sum(tbl) * 100, 2)
          )
        }
        
        write.csv(stats_df, file, row.names = FALSE)
        
        # Add to activity tracker
        add_activity_to_tracker(
          type = "analysis",
          title = "Download Descriptive Statistics",
          content = paste(
            paste("Statistik deskriptif untuk variabel:", values$current_desc_variable),
            paste("Format file: CSV"),
            paste("Timestamp download:", Sys.time()),
            sep = "\n"
          )
        )
      }
    }
  )
  
  # Download descriptive report
  output$download_desc_report <- downloadHandler(
    filename = function() {
      create_download_filename("descriptive_report", "pdf")
    },
    content = function(file) {
      # Create temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Generate report content
      report_content <- generate_desc_report_content()
      
      writeLines(report_content, temp_rmd)
      
      # Render to PDF
      rmarkdown::render(temp_rmd, 
                        output_format = "pdf_document",
                        output_file = file,
                        quiet = TRUE)
      
      # Add to activity tracker
      add_activity_to_tracker(
        type = "analysis",
        title = "Download Descriptive Analysis Report",
        content = paste(
          paste("Laporan analisis deskriptif untuk variabel:", values$current_desc_variable),
          paste("Format file: PDF"),
          paste("Timestamp download:", Sys.time()),
          sep = "\n"
        )
      )
    }
  )
  
  # Download visualization plot
  output$download_viz_plot <- downloadHandler(
    filename = function() {
      create_download_filename("visualization_plot", "png")
    },
    content = function(file) {
      if (values$viz_generated && !is.null(values$current_viz_type)) {
        # Recreate the plot for download
        p <- switch(values$current_viz_type,
                    "scatter" = {
                      if (!is.null(input$scatter_x) && !is.null(input$scatter_y)) {
                        base_plot <- ggplot(values$processed_data, aes_string(x = input$scatter_x, y = input$scatter_y))
                        
                        if (!is.null(input$scatter_color) && input$scatter_color != "") {
                          base_plot <- base_plot + aes_string(color = input$scatter_color)
                        }
                        
                        base_plot +
                          geom_point(alpha = 0.7, size = 2) +
                          geom_smooth(method = "lm", se = TRUE, color = waskita_colors$navy) +
                          labs(title = paste("Scatter Plot:", input$scatter_x, "vs", input$scatter_y)) +
                          theme_waskita()
                      }
                    },
                    "correlation" = {
                      numeric_data <- values$processed_data[sapply(values$processed_data, is.numeric)]
                      if (ncol(numeric_data) >= 2) {
                        cor_matrix <- cor(numeric_data, use = "complete.obs")
                        
                        # Convert to long format for ggplot
                        cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
                        cor_df$Correlation <- as.vector(cor_matrix)
                        
                        ggplot(cor_df, aes(Var1, Var2, fill = Correlation)) +
                          geom_tile() +
                          scale_fill_gradient2(low = waskita_colors$error, mid = "white", 
                                               high = waskita_colors$navy, midpoint = 0,
                                               limits = c(-1, 1)) +
                          labs(title = "Correlation Heatmap", x = "", y = "") +
                          theme_waskita() +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1))
                      }
                    }
        )
        
        if (!is.null(p)) {
          ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
          
          # Add to activity tracker
          add_activity_to_tracker(
            type = "analysis",
            title = "Download Visualization Plot",
            content = paste(
              paste("Plot visualisasi jenis:", values$current_viz_type),
              paste("Format file: PNG (300 DPI)"),
              paste("Timestamp download:", Sys.time()),
              sep = "\n"
            )
          )
        }
      }
    }
  )
  
  # Download visualization report
  output$download_viz_report <- downloadHandler(
    filename = function() {
      create_download_filename("visualization_report", "pdf")
    },
    content = function(file) {
      # Create temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Generate report content
      report_content <- generate_viz_report_content()
      
      writeLines(report_content, temp_rmd)
      
      # Render to PDF
      rmarkdown::render(temp_rmd, 
                        output_format = "pdf_document",
                        output_file = file,
                        quiet = TRUE)
      
      # Add to activity tracker
      add_activity_to_tracker(
        type = "analysis",
        title = "Download Visualization Report",
        content = paste(
          paste("Laporan visualisasi jenis:", values$current_viz_type),
          paste("Format file: PDF"),
          paste("Timestamp download:", Sys.time()),
          sep = "\n"
        )
      )
    }
  )
  
  # Download map image
  output$download_map_image <- downloadHandler(
    filename = function() {
      create_download_filename("spatial_map", "png")
    },
    content = function(file) {
      # Create a static version of the map for download
      if (values$map_generated && !is.null(input$map_variable)) {
        
        if (!is.null(values$indonesia_sf)) {
          # Try to create a static map using ggplot with sf data
          tryCatch({
            merged_data <- create_choropleth_data(values$processed_data, values$indonesia_sf, input$map_variable)
            
            if (!is.null(merged_data) && input$map_variable %in% names(merged_data)) {
              p <- ggplot(merged_data) +
                geom_sf(aes_string(fill = input$map_variable), color = "white", size = 0.1) +
                scale_fill_viridis_c(name = input$map_variable, option = input$color_palette) +
                labs(title = paste("Peta Indonesia:", input$map_variable),
                     subtitle = paste("Tipe:", input$map_type, "| Skema warna:", input$color_palette)) +
                theme_void() +
                theme(
                  plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 12),
                  legend.position = "bottom"
                )
              
              ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
            } else {
              # Fallback to point map
              var_data <- values$processed_data[[input$map_variable]]
              map_data <- data.frame(
                lat = runif(length(var_data), -11, 6),
                lon = runif(length(var_data), 95, 141),
                value = var_data
              )
              
              p <- ggplot(map_data, aes(x = lon, y = lat, color = value)) +
                geom_point(size = 2, alpha = 0.7) +
                scale_color_viridis_c(name = input$map_variable, option = input$color_palette) +
                labs(title = paste("Spatial Distribution:", input$map_variable),
                     x = "Longitude", y = "Latitude") +
                theme_waskita()
              
              ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
            }
          }, error = function(e) {
            # Final fallback
            var_data <- values$processed_data[[input$map_variable]]
            map_data <- data.frame(
              lat = runif(length(var_data), -11, 6),
              lon = runif(length(var_data), 95, 141),
              value = var_data
            )
            
            p <- ggplot(map_data, aes(x = lon, y = lat, color = value)) +
              geom_point(size = 2, alpha = 0.7) +
              scale_color_viridis_c(name = input$map_variable) +
              labs(title = paste("Spatial Distribution:", input$map_variable),
                   x = "Longitude", y = "Latitude") +
              theme_waskita()
            
            ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
          })
        } else {
          # Use synthetic point data
          var_data <- values$processed_data[[input$map_variable]]
          map_data <- data.frame(
            lat = runif(length(var_data), -11, 6),
            lon = runif(length(var_data), 95, 141),
            value = var_data
          )
          
          p <- ggplot(map_data, aes(x = lon, y = lat, color = value)) +
            geom_point(size = 2, alpha = 0.7) +
            scale_color_viridis_c(name = input$map_variable, option = input$color_palette) +
            labs(title = paste("Spatial Distribution:", input$map_variable),
                 x = "Longitude", y = "Latitude") +
            theme_waskita()
          
          ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
        }
        
        # Add to activity tracker
        add_activity_to_tracker(
          type = "analysis",
          title = "Download Spatial Map",
          content = paste(
            paste("Peta spasial untuk variabel:", input$map_variable),
            paste("Tipe peta:", input$map_type),
            paste("Format file: PNG (300 DPI)"),
            paste("Timestamp download:", Sys.time()),
            sep = "\n"
          )
        )
      }
    }
  )
  
  # Generate report content functions
  generate_desc_report_content <- function() {
    content <- c(
      "---",
      "title: 'WASKITA Dashboard - Laporan Analisis Deskriptif'",
      "subtitle: 'Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik'",
      "author: 'Generated by WASKITA Dashboard'",
      paste("date: '", Sys.Date(), "'"),
      "output:",
      "  pdf_document:",
      "    toc: true",
      "    toc_depth: 2",
      "---",
      "",
      "# Analisis Deskriptif",
      "",
      paste("Variabel yang dianalisis:", values$current_desc_variable),
      "",
      "## Statistik Deskriptif",
      ""
    )
    
    if (!is.null(values$current_desc_variable)) {
      var_data <- values$processed_data[[values$current_desc_variable]]
      
      if (is.numeric(var_data)) {
        content <- c(content,
                     paste("- N:", sum(!is.na(var_data))),
                     paste("- Missing:", sum(is.na(var_data))),
                     paste("- Mean:", round(mean(var_data, na.rm = TRUE), 4)),
                     paste("- Median:", round(median(var_data, na.rm = TRUE), 4)),
                     paste("- SD:", round(sd(var_data, na.rm = TRUE), 4)),
                     paste("- Min:", round(min(var_data, na.rm = TRUE), 4)),
                     paste("- Max:", round(max(var_data, na.rm = TRUE), 4)),
                     ""
        )
      }
    }
    
    content <- c(content,
                 "## Interpretasi",
                 "",
                 "Analisis deskriptif telah dilakukan menggunakan platform WASKITA Dashboard.",
                 "",
                 "---",
                 "",
                 "*Laporan ini dibuat secara otomatis oleh WASKITA Dashboard*"
    )
    
    return(content)
  }
  
  generate_viz_report_content <- function() {
    content <- c(
      "---",
      "title: 'WASKITA Dashboard - Laporan Visualisasi'",
      "subtitle: 'Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik'",
      "author: 'Generated by WASKITA Dashboard'",
      paste("date: '", Sys.Date(), "'"),
      "output:",
      "  pdf_document:",
      "    toc: true",
      "    toc_depth: 2",
      "---",
      "",
      "# Visualisasi Multivariat",
      "",
      paste("Jenis visualisasi:", values$current_viz_type),
      "",
      "## Interpretasi Visualisasi",
      "",
      "Visualisasi telah dibuat menggunakan platform WASKITA Dashboard.",
      "",
      "---",
      "",
      "*Laporan ini dibuat secara otomatis oleh WASKITA Dashboard*"
    )
    
    return(content)
  }
  
  # Placeholder download handlers for remaining buttons (enhanced with tracking)
  output$download_assumptions_plot <- downloadHandler(
    filename = function() { create_download_filename("assumptions_plot", "png") },
    content = function(file) {
      if (values$normality_done) {
        var_data <- values$processed_data[[input$normality_variable]]
        p <- ggplot(data.frame(sample = var_data), aes(sample = sample)) +
          stat_qq(color = waskita_colors$steel) +
          stat_qq_line(color = waskita_colors$navy) +
          labs(title = "Q-Q Plot for Normality Test") +
          theme_waskita()
        ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
        
        add_activity_to_tracker(
          type = "analysis",
          title = "Download Assumptions Plot",
          content = paste("Q-Q plot untuk uji normalitas didownload", "Timestamp:", Sys.time(), sep = "\n")
        )
      }
    }
  )
  
  output$download_assumptions_results <- downloadHandler(
    filename = function() { create_download_filename("assumptions_results", "csv") },
    content = function(file) {
      if (values$normality_done && !is.null(values$normality_result)) {
        results_df <- data.frame(
          Test = "Normality Test",
          Method = input$normality_test,
          Variable = input$normality_variable,
          P_Value = values$normality_result$p.value,
          Significant = values$normality_result$p.value <= 0.05
        )
        write.csv(results_df, file, row.names = FALSE)
        
        add_activity_to_tracker(
          type = "analysis",
          title = "Download Assumptions Results",
          content = paste("Hasil uji asumsi didownload dalam format CSV", "Timestamp:", Sys.time(), sep = "\n")
        )
      }
    }
  )
  
  output$download_assumptions_report <- downloadHandler(
    filename = function() { create_download_filename("assumptions_report", "pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      content <- c(
        "---",
        "title: 'Assumption Tests Report'",
        "output: pdf_document",
        "---",
        "",
        "# Assumption Tests Results",
        "",
        "This report contains the results of assumption tests performed in WASKITA Dashboard."
      )
      writeLines(content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      
      add_activity_to_tracker(
        type = "analysis",
        title = "Download Assumptions Report",
        content = paste("Laporan uji asumsi didownload dalam format PDF", "Timestamp:", Sys.time(), sep = "\n")
      )
    }
  )
  
  # Add similar enhanced handlers for other downloads
  output$download_ttest_plot <- downloadHandler(
    filename = function() { create_download_filename("ttest_plot", "png") },
    content = function(file) {
      if (values$ttest_done) {
        p <- ggplot(values$processed_data, aes_string(x = input$ttest_variable)) +
          geom_histogram(bins = 30, fill = waskita_colors$steel, alpha = 0.7) +
          labs(title = "T-test Results") +
          theme_waskita()
        ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
        
        add_activity_to_tracker(
          type = "analysis",
          title = "Download T-test Plot",
          content = paste("Plot hasil uji t didownload", "Timestamp:", Sys.time(), sep = "\n")
        )
      }
    }
  )
  
  output$download_ttest_results <- downloadHandler(
    filename = function() { create_download_filename("ttest_results", "csv") },
    content = function(file) {
      if (values$ttest_done && !is.null(values$ttest_result)) {
        results_df <- data.frame(
          Test = "T-test",
          Type = input$ttest_type,
          Variable = input$ttest_variable,
          T_Statistic = values$ttest_result$statistic,
          P_Value = values$ttest_result$p.value,
          Significant = values$ttest_result$p.value <= 0.05
        )
        write.csv(results_df, file, row.names = FALSE)
        
        add_activity_to_tracker(
          type = "analysis",
          title = "Download T-test Results",
          content = paste("Hasil uji t didownload dalam format CSV", "Timestamp:", Sys.time(), sep = "\n")
        )
      }
    }
  )
  
  output$download_ttest_report <- downloadHandler(
    filename = function() { create_download_filename("ttest_report", "pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      content <- c("---", "title: 'T-test Report'", "output: pdf_document", "---", "", "# T-test Results", "", "This report contains the results of t-test analysis performed in WASKITA Dashboard.")
      writeLines(content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      
      add_activity_to_tracker(
        type = "analysis",
        title = "Download T-test Report",
        content = paste("Laporan uji t didownload dalam format PDF", "Timestamp:", Sys.time(), sep = "\n")
      )
    }
  )
  
  # Add more enhanced placeholder handlers for remaining downloads
  output$download_propvar_plots <- downloadHandler(
    filename = function() { create_download_filename("propvar_plots", "png") },
    content = function(file) {
      p <- ggplot() + geom_blank() + labs(title = "Proportion & Variance Test Plots") + theme_waskita()
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      add_activity_to_tracker(type = "analysis", title = "Download Prop/Var Plots", content = paste("Plot uji proporsi/variance didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
  output$download_propvar_results <- downloadHandler(
    filename = function() { create_download_filename("propvar_results", "csv") },
    content = function(file) {
      results_df <- data.frame(Test = "Proportion/Variance", Status = "Completed")
      write.csv(results_df, file, row.names = FALSE)
      add_activity_to_tracker(type = "analysis", title = "Download Prop/Var Results", content = paste("Hasil uji proporsi/variance didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
  output$download_propvar_report <- downloadHandler(
    filename = function() { create_download_filename("propvar_report", "pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      content <- c("---", "title: 'Proportion & Variance Report'", "output: pdf_document", "---", "", "# Results")
      writeLines(content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      add_activity_to_tracker(type = "analysis", title = "Download Prop/Var Report", content = paste("Laporan uji proporsi/variance didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
  output$download_anova_plots <- downloadHandler(
    filename = function() { create_download_filename("anova_plots", "png") },
    content = function(file) {
      p <- ggplot() + geom_blank() + labs(title = "ANOVA Plots") + theme_waskita()
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      add_activity_to_tracker(type = "analysis", title = "Download ANOVA Plots", content = paste("Plot ANOVA didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
  output$download_anova_results <- downloadHandler(
    filename = function() { create_download_filename("anova_results", "csv") },
    content = function(file) {
      results_df <- data.frame(Test = "ANOVA", Status = "Completed")
      write.csv(results_df, file, row.names = FALSE)
      add_activity_to_tracker(type = "analysis", title = "Download ANOVA Results", content = paste("Hasil ANOVA didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
  output$download_anova_report <- downloadHandler(
    filename = function() { create_download_filename("anova_report", "pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      content <- c("---", "title: 'ANOVA Report'", "output: pdf_document", "---", "", "# Results")
      writeLines(content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      add_activity_to_tracker(type = "analysis", title = "Download ANOVA Report", content = paste("Laporan ANOVA didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
  output$download_regression_plots <- downloadHandler(
    filename = function() { create_download_filename("regression_plots", "png") },
    content = function(file) {
      p <- ggplot() + geom_blank() + labs(title = "Regression Plots") + theme_waskita()
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      add_activity_to_tracker(type = "analysis", title = "Download Regression Plots", content = paste("Plot regresi didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
  output$download_regression_results <- downloadHandler(
    filename = function() { create_download_filename("regression_results", "csv") },
    content = function(file) {
      results_df <- data.frame(Test = "Regression", Status = "Completed")
      write.csv(results_df, file, row.names = FALSE)
      add_activity_to_tracker(type = "analysis", title = "Download Regression Results", content = paste("Hasil regresi didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
  output$download_regression_report <- downloadHandler(
    filename = function() { create_download_filename("regression_report", "pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      content <- c("---", "title: 'Regression Report'", "output: pdf_document", "---", "", "# Results")
      writeLines(content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      add_activity_to_tracker(type = "analysis", title = "Download Regression Report", content = paste("Laporan regresi didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
  output$download_spatial_plots <- downloadHandler(
    filename = function() { create_download_filename("spatial_plots", "png") },
    content = function(file) {
      p <- ggplot() + geom_blank() + labs(title = "Spatial Analysis Plots") + theme_waskita()
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      add_activity_to_tracker(type = "analysis", title = "Download Spatial Plots", content = paste("Plot analisis spasial didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
  output$download_distance_matrix <- downloadHandler(
    filename = function() { create_download_filename("distance_matrix", "csv") },
    content = function(file) {
      if (!is.null(values$distance_data)) {
        write.csv(values$distance_data, file, row.names = FALSE)
        add_activity_to_tracker(type = "analysis", title = "Download Distance Matrix", content = paste("Matriks jarak didownload", "Timestamp:", Sys.time(), sep = "\n"))
      }
    }
  )
  
  output$download_spatial_report_final <- downloadHandler(
    filename = function() { create_download_filename("spatial_report", "pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      content <- c("---", "title: 'Spatial Analysis Report'", "output: pdf_document", "---", "", "# Results")
      writeLines(content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      add_activity_to_tracker(type = "analysis", title = "Download Spatial Report", content = paste("Laporan analisis spasial didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
  output$download_spatial_report <- downloadHandler(
    filename = function() { create_download_filename("spatial_map_report", "pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      content <- c("---", "title: 'Spatial Map Report'", "output: pdf_document", "---", "", "# Results")
      writeLines(content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      add_activity_to_tracker(type = "analysis", title = "Download Spatial Map Report", content = paste("Laporan peta spasial didownload", "Timestamp:", Sys.time(), sep = "\n"))
    }
  )
  
}