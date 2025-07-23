# =============================================================================
# WASKITA Dashboard - Server Logic
# Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik
# =============================================================================

source("global.R")

server <- function(input, output, session) {
  
  # =============================================================================
  # REACTIVE VALUES
  # =============================================================================
  
  values <- reactiveValues(
    # Data storage
    original_data = NULL,
    processed_data = NULL,
    distance_data = NULL,
    map_data = NULL,
    
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
  
  # =============================================================================
  # STATUS UPDATE FUNCTIONS FOR LAPORAN LENGKAP
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
  # DATA LOADING
  # =============================================================================
  
  observeEvent(input$load_data, {
    tryCatch({
      # Load data using global function
      data_result <- load_waskita_data()
      
      if ("error" %in% names(data_result)) {
        safe_notification(data_result$error, "error")
        return()
      }
      
      values$original_data <- data_result$sovi
      values$processed_data <- data_result$sovi
      values$distance_data <- data_result$distance
      values$data_loaded <- TRUE
      
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
      
      # Calculate spatial statistics for interpretation
      map_data <- values$processed_data
      if (nrow(map_data) > 0) {
        # Generate synthetic coordinates for demonstration
        set.seed(123)
        lat_range <- c(-11, 6)  # Indonesia latitude range
        lon_range <- c(95, 141)  # Indonesia longitude range
        
        map_data$lat <- runif(nrow(map_data), lat_range[1], lat_range[2])
        map_data$lon <- runif(nrow(map_data), lon_range[1], lon_range[2])
        
        # Create proper district identifier
        if("DISTRICTCODE" %in% names(values$processed_data)) {
          map_data$district_id <- values$processed_data$DISTRICTCODE
        } else {
          map_data$district_id <- paste0("District_", 1:nrow(map_data))
        }
        
        values$map_data <- map_data
        
        # Log spatial analysis info
        cat("=== SPATIAL DATA LOADED ===\n")
        cat("   - Total observasi:", nrow(map_data), "\n")
        cat("   - Koordinat latitude range:", round(min(map_data$lat), 3), "to", round(max(map_data$lat), 3), "\n")
        cat("   - Koordinat longitude range:", round(min(map_data$lon), 3), "to", round(max(map_data$lon), 3), "\n")
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
      cat("Dataset WASKITA\n")
      cat("================\n")
      cat("Baris:", nrow(values$processed_data), "\n")
      cat("Kolom:", ncol(values$processed_data), "\n")
      cat("Missing Values:", sum(is.na(values$processed_data)), "\n")
      cat("Numerik:", sum(sapply(values$processed_data, is.numeric)), "\n")
      cat("Kategorikal:", sum(sapply(values$processed_data, function(x) is.character(x) | is.factor(x))), "\n")
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
  # TRANSFORMASI DATA
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
  # KATEGORISASI DATA
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
  # ANALISIS DESKRIPTIF
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
  # VISUALISASI MULTIVARIAT
  # =============================================================================
  
  observeEvent(input$generate_viz, {
    req(values$processed_data, input$viz_type)
    
    values$current_viz_type <- input$viz_type
    values$viz_generated <- TRUE
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
  # PETA SPASIAL - FIXED
  # =============================================================================
  
  observeEvent(input$generate_map, {
    req(values$processed_data, input$map_variable)
    
    tryCatch({
      # Use existing map data with coordinates
      if (is.null(values$map_data)) {
        # Generate synthetic coordinates for demonstration
        n_obs <- nrow(values$processed_data)
        set.seed(123)
        
        # Create Indonesia-like coordinates
        lat_range <- c(-11, 6)  # Indonesia latitude range
        lon_range <- c(95, 141)  # Indonesia longitude range
        
        map_data <- data.frame(
          lat = runif(n_obs, lat_range[1], lat_range[2]),
          lon = runif(n_obs, lon_range[1], lon_range[2]),
          value = values$processed_data[[input$map_variable]],
          district_id = if("DISTRICTCODE" %in% names(values$processed_data)) {
            values$processed_data$DISTRICTCODE 
          } else { 
            paste0("District_", 1:n_obs) 
          },
          stringsAsFactors = FALSE
        )
        values$map_data <- map_data
      } else {
        # Update existing map data with new variable
        values$map_data$value <- values$processed_data[[input$map_variable]]
      }
      
      values$map_generated <- TRUE
      
      safe_notification("Peta berhasil dibuat!", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error membuat peta:", e$message), "error")
    })
  })
  
  output$map_generated <- reactive({
    values$map_generated
  })
  outputOptions(output, "map_generated", suspendWhenHidden = FALSE)
  
  # Fixed spatial map output
  output$spatial_map <- renderLeaflet({
    if (values$map_generated && !is.null(values$map_data)) {
      map_data <- values$map_data
      
      # Create color palette
      pal <- colorNumeric(palette = input$color_palette, domain = map_data$value)
      
      # Create leaflet map
      m <- leaflet(map_data) %>%
        addTiles() %>%
        setView(lng = mean(map_data$lon, na.rm = TRUE), lat = mean(map_data$lat, na.rm = TRUE), zoom = 5)
      
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
      } else { # choropleth
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
  })
  
  output$spatial_stats <- renderPrint({
    if (values$map_generated && !is.null(values$map_data)) {
      map_data <- values$map_data
      
      cat("STATISTIK DISTRIBUSI SPASIAL\n")
      cat("=============================\n")
      cat("Variabel:", input$map_variable, "\n")
      cat("Jumlah lokasi:", nrow(map_data), "\n")
      cat("Range koordinat:\n")
      cat("- Latitude:", round(min(map_data$lat), 3), "to", round(max(map_data$lat), 3), "\n")
      cat("- Longitude:", round(min(map_data$lon), 3), "to", round(max(map_data$lon), 3), "\n")
      cat("\nDistribusi nilai:\n")
      cat("- Min:", round(min(map_data$value, na.rm = TRUE), 3), "\n")
      cat("- Max:", round(max(map_data$value, na.rm = TRUE), 3), "\n")
      cat("- Mean:", round(mean(map_data$value, na.rm = TRUE), 3), "\n")
      cat("- SD:", round(sd(map_data$value, na.rm = TRUE), 3), "\n")
      
      # Calculate spatial statistics
      value_data <- map_data$value[!is.na(map_data$value)]
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
    if (values$map_generated && !is.null(values$map_data)) {
      cat("INTERPRETASI POLA SPASIAL\n")
      cat("=========================\n")
      cat("Peta menunjukkan distribusi geografis dari", input$map_variable, "\n\n")
      
      cat("Pola yang dapat diamati:\n")
      cat("1. DISTRIBUSI GEOGRAFIS:\n")
      cat("   - Peta menampilkan sebaran nilai across Indonesia\n")
      cat("   - Warna menunjukkan intensitas/magnitude variabel\n")
      cat("   - Variasi spasial menunjukkan heterogenitas geografis\n")
      
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
  # UJI NORMALITAS
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
  # UJI HOMOGENITAS
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
  # UJI BEDA RATA-RATA (t-test)
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
  # UJI PROPORSI & VARIANCE
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
  # ANOVA
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
  # REGRESI LINEAR BERGANDA
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
  # ANALISIS SPASIAL LANJUTAN
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
  # COMPREHENSIVE REPORT GENERATOR FUNCTION
  # =============================================================================
  
  generate_comprehensive_rmd_report <- function() {
    report_content <- c(
      "---",
      "title: 'LAPORAN ANALISIS STATISTIK KOMPREHENSIF'",
      "subtitle: 'Dashboard WASKITA - Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik'",
      paste0("author: 'Generated by WASKITA Dashboard - ", Sys.Date(), "'"),
      paste0("date: '", format(Sys.Date(), "%d %B %Y"), "'"),
      "output:",
      "  pdf_document:",
      "    toc: true",
      "    toc_depth: 3",
      "    number_sections: true",
      "    latex_engine: xelatex",
      "  word_document:",
      "    toc: true",
      "    toc_depth: 3",
      "header-includes:",
      "  - \\usepackage[indonesian]{babel}",
      "  - \\usepackage{booktabs}",
      "  - \\usepackage{longtable}",
      "  - \\usepackage{array}",
      "  - \\usepackage{multirow}",
      "  - \\usepackage{wrapfig}",
      "  - \\usepackage{float}",
      "  - \\usepackage{colortbl}",
      "  - \\usepackage{pdflscape}",
      "  - \\usepackage{tabu}",
      "  - \\usepackage{threeparttable}",
      "  - \\usepackage{threeparttablex}",
      "  - \\usepackage[normalem]{ulem}",
      "  - \\usepackage{makecell}",
      "  - \\usepackage{xcolor}",
      "---",
      "",
      "\\newpage",
      "",
      "# RINGKASAN EKSEKUTIF",
      "",
      "Laporan ini menyajikan hasil analisis statistik komprehensif dari dataset **Social Vulnerability Index (SoVI)** menggunakan platform WASKITA Dashboard. Analisis dilakukan untuk memahami pola kerentanan sosial di Indonesia dengan menggunakan berbagai metode statistik modern.",
      ""
    )
    
    # Dataset Overview
    if (values$data_loaded) {
      data_info <- values$processed_data
      report_content <- c(report_content,
                          "## Informasi Dataset",
                          "",
                          paste("- **Total Observasi:** ", nrow(data_info)),
                          paste("- **Total Variabel:** ", ncol(data_info)),
                          paste("- **Missing Values:** ", sum(is.na(data_info))),
                          paste("- **Variabel Numerik:** ", sum(sapply(data_info, is.numeric))),
                          paste("- **Variabel Kategorikal:** ", sum(sapply(data_info, function(x) is.character(x) | is.factor(x)))),
                          ""
      )
    }
    
    # Analysis Status
    report_content <- c(report_content,
                        "## Status Analisis yang Dilakukan",
                        "",
                        "Berikut adalah ringkasan analisis yang telah berhasil dilakukan:",
                        ""
    )
    
    analysis_status <- data.frame(
      Analisis = c("Manajemen Data", "Analisis Deskriptif", "Visualisasi Multivariat", 
                   "Peta Distribusi Spasial", "Uji Asumsi", "Uji Beda Rata-rata",
                   "Uji Proporsi & Variance", "ANOVA", "Regresi Linear", "Analisis Spasial"),
      Status = c(
        if(values$data_loaded) "✓ Completed" else "○ Pending",
        if(values$desc_generated) "✓ Completed" else "○ Pending",
        if(values$viz_generated) "✓ Completed" else "○ Pending",
        if(values$map_generated) "✓ Completed" else "○ Pending",
        if(values$normality_done || values$homogeneity_done) "✓ Completed" else "○ Pending",
        if(values$ttest_done) "✓ Completed" else "○ Pending",
        if(values$prop_test_done || values$var_test_done) "✓ Completed" else "○ Pending",
        if(values$anova_done) "✓ Completed" else "○ Pending",
        if(values$regression_done) "✓ Completed" else "○ Pending",
        if(values$spatial_done) "✓ Completed" else "○ Pending"
      )
    )
    
    # Add status table
    report_content <- c(report_content,
                        "| Jenis Analisis | Status |",
                        "|---|---|")
    
    for (i in 1:nrow(analysis_status)) {
      report_content <- c(report_content,
                          paste("|", analysis_status$Analisis[i], "|", analysis_status$Status[i], "|"))
    }
    
    report_content <- c(report_content, "", "\\newpage", "")
    
    # Main Content Sections
    report_content <- c(report_content,
                        "# METADATA DATASET",
                        "",
                        "Dataset yang digunakan dalam analisis ini adalah **Social Vulnerability Index (SoVI)** yang merupakan indeks kerentanan sosial untuk wilayah Indonesia.",
                        "",
                        "## Sumber Data",
                        "",
                        "- **Journal:** Data in Brief",
                        "- **Publisher:** Elsevier",  
                        "- **DOI:** 10.1016/j.dib.2021.107618",
                        "- **Link:** https://www.sciencedirect.com/science/article/pii/S2352340921010180",
                        "",
                        "## Deskripsi Variabel",
                        ""
    )
    
    # Add metadata table
    for (i in 1:nrow(sovi_metadata)) {
      report_content <- c(report_content,
                          paste("**", sovi_metadata$Variabel[i], "**"),
                          paste("- Keterangan:", sovi_metadata$Keterangan[i]),
                          paste("- Tipe Data:", sovi_metadata$Tipe_Data[i]),
                          ""
      )
    }
    
    report_content <- c(report_content, "\\newpage", "")
    
    # Descriptive Analysis Results
    if (values$desc_generated && !is.null(values$current_desc_variable)) {
      report_content <- c(report_content,
                          "# HASIL ANALISIS DESKRIPTIF",
                          "",
                          paste("## Analisis Variabel:", values$current_desc_variable),
                          ""
      )
      
      var_data <- values$processed_data[[values$current_desc_variable]]
      if (is.numeric(var_data)) {
        report_content <- c(report_content,
                            "### Statistik Deskriptif",
                            "",
                            paste("- **N (Valid):** ", sum(!is.na(var_data))),
                            paste("- **Missing:** ", sum(is.na(var_data))),  
                            paste("- **Mean:** ", round(mean(var_data, na.rm = TRUE), 4)),
                            paste("- **Median:** ", round(median(var_data, na.rm = TRUE), 4)),
                            paste("- **Standard Deviation:** ", round(sd(var_data, na.rm = TRUE), 4)),
                            paste("- **Minimum:** ", round(min(var_data, na.rm = TRUE), 4)),
                            paste("- **Maximum:** ", round(max(var_data, na.rm = TRUE), 4)),
                            paste("- **Q1:** ", round(quantile(var_data, 0.25, na.rm = TRUE), 4)),
                            paste("- **Q3:** ", round(quantile(var_data, 0.75, na.rm = TRUE), 4)),
                            paste("- **IQR:** ", round(IQR(var_data, na.rm = TRUE), 4)),
                            ""
        )
        
        # Add interpretation
        mean_val <- mean(var_data, na.rm = TRUE)
        median_val <- median(var_data, na.rm = TRUE)
        sd_val <- sd(var_data, na.rm = TRUE)
        cv <- sd_val / abs(mean_val) * 100
        
        report_content <- c(report_content,
                            "### Interpretasi Statistik",
                            "",
                            "#### Tendensi Sentral",
                            ""
        )
        
        if (abs(mean_val - median_val) / sd_val < 0.1) {
          report_content <- c(report_content, "- Distribusi relatif **simetris** (mean ≈ median)")
        } else if (mean_val > median_val) {
          report_content <- c(report_content, "- Distribusi **condong kanan** (mean > median)")
        } else {
          report_content <- c(report_content, "- Distribusi **condong kiri** (mean < median)")
        }
        
        report_content <- c(report_content,
                            "",
                            "#### Variabilitas", 
                            "",
                            paste("- **Coefficient of Variation:** ", round(cv, 2), "%")
        )
        
        if (cv < 15) {
          report_content <- c(report_content, "- Variabilitas **rendah** (data homogen)")
        } else if (cv < 30) {
          report_content <- c(report_content, "- Variabilitas **sedang**")
        } else {
          report_content <- c(report_content, "- Variabilitas **tinggi** (data heterogen)")
        }
      }
    }
    
    # Visualization Results
    if (values$viz_generated && !is.null(values$current_viz_type)) {
      report_content <- c(report_content,
                          "",
                          "\\newpage",
                          "",
                          "# HASIL VISUALISASI MULTIVARIAT",
                          "",
                          paste("## Jenis Visualisasi:", switch(values$current_viz_type,
                                                                "scatter" = "Scatter Plot",
                                                                "correlation" = "Correlation Heatmap",
                                                                "boxplot_group" = "Box Plot by Group", 
                                                                "histogram_overlay" = "Histogram Overlay")),
                          ""
      )
      
      # Add visualization interpretation
      if (values$current_viz_type == "scatter" && !is.null(input$scatter_x) && !is.null(input$scatter_y)) {
        x_data <- values$processed_data[[input$scatter_x]]
        y_data <- values$processed_data[[input$scatter_y]]
        cor_val <- cor(x_data, y_data, use = "complete.obs")
        
        report_content <- c(report_content,
                            paste("### Analisis Korelasi: ", input$scatter_x, "vs", input$scatter_y),
                            "",
                            paste("- **Koefisien Korelasi:** ", round(cor_val, 4)),
                            paste("- **Kekuatan Hubungan:** ", interpret_correlation(cor_val)),
                            paste("- **Arah Hubungan:** ", if(cor_val > 0) "Positif" else "Negatif"),
                            ""
        )
        
        if (abs(cor_val) > 0.7) {
          report_content <- c(report_content, "**Kesimpulan:** Hubungan yang **kuat** ditemukan antara kedua variabel.")
        } else if (abs(cor_val) > 0.3) {
          report_content <- c(report_content, "**Kesimpulan:** Hubungan yang **sedang** ditemukan antara kedua variabel.")
        } else {
          report_content <- c(report_content, "**Kesimpulan:** Hubungan yang **lemah** ditemukan antara kedua variabel.")
        }
      }
    }
    
    # Spatial Map Results
    if (values$map_generated && !is.null(values$map_data)) {
      report_content <- c(report_content,
                          "",
                          "\\newpage",
                          "", 
                          "# HASIL PETA DISTRIBUSI SPASIAL",
                          "",
                          paste("## Peta Variabel:", input$map_variable),
                          "",
                          "### Statistik Distribusi Spasial",
                          ""
      )
      
      map_data <- values$map_data
      report_content <- c(report_content,
                          paste("- **Jumlah Lokasi:** ", nrow(map_data)),
                          paste("- **Range Latitude:** ", round(min(map_data$lat), 3), "hingga", round(max(map_data$lat), 3)),
                          paste("- **Range Longitude:** ", round(min(map_data$lon), 3), "hingga", round(max(map_data$lon), 3)),
                          paste("- **Nilai Minimum:** ", round(min(map_data$value, na.rm = TRUE), 3)),
                          paste("- **Nilai Maksimum:** ", round(max(map_data$value, na.rm = TRUE), 3)),
                          paste("- **Nilai Rata-rata:** ", round(mean(map_data$value, na.rm = TRUE), 3)),
                          paste("- **Standard Deviasi:** ", round(sd(map_data$value, na.rm = TRUE), 3)),
                          "",
                          "### Interpretasi Pola Spasial",
                          "",
                          paste("Peta distribusi spasial menunjukkan sebaran geografis variabel **", input$map_variable, "** di seluruh Indonesia. Analisis pola spasial mengindikasikan:"),
                          "",
                          "1. **Distribusi Geografis:** Variasi nilai menunjukkan heterogenitas geografis",
                          "2. **Klasterisasi:** Area dengan nilai serupa mengindikasikan pola lokal",
                          "3. **Outlier Spasial:** Lokasi dengan nilai berbeda dari sekitarnya perlu investigasi lebih lanjut",
                          "4. **Implikasi Kebijakan:** Identifikasi area prioritas berdasarkan pola spasial",
                          ""
      )
    }
    
    # Assumption Tests Results
    if (values$normality_done || values$homogeneity_done) {
      report_content <- c(report_content,
                          "",
                          "\\newpage",
                          "",
                          "# HASIL UJI ASUMSI DATA",
                          ""
      )
      
      if (values$normality_done && !is.null(values$normality_result)) {
        report_content <- c(report_content,
                            "## Uji Normalitas",
                            "",
                            paste("**Variabel:** ", input$normality_variable),
                            paste("**Metode:** ", switch(input$normality_test,
                                                         "shapiro" = "Shapiro-Wilk Test",
                                                         "ks" = "Kolmogorov-Smirnov Test", 
                                                         "ad" =  "Anderson-Darling Test")),
                            "",
                            "### Hipotesis",
                            "",
                            "- **H₀:** Data berdistribusi normal",
                            "- **H₁:** Data tidak berdistribusi normal", 
                            "- **α:** 0.05",
                            "",
                            "### Hasil",
                            "",
                            paste("- **Statistik Uji:** ", round(values$normality_result$statistic, 4)),
                            paste("- **P-value:** ", format_pvalue(values$normality_result$p.value)),
                            ""
        )
        
        p_value <- values$normality_result$p.value
        if (p_value > 0.05) {
          report_content <- c(report_content,
                              "### Kesimpulan",
                              "",
                              "- **Keputusan:** Gagal menolak H₀",
                              "- **Interpretasi:** Data dapat dianggap berdistribusi normal (α = 0.05)",
                              "- **Implikasi:** Dapat menggunakan uji parametrik",
                              ""
          )
        } else {
          report_content <- c(report_content,
                              "### Kesimpulan", 
                              "",
                              "- **Keputusan:** Tolak H₀",
                              "- **Interpretasi:** Data tidak berdistribusi normal (α = 0.05)",
                              "- **Implikasi:** Pertimbangkan transformasi atau uji non-parametrik",
                              ""
          )
        }
      }
      
      if (values$homogeneity_done && !is.null(values$homogeneity_result)) {
        report_content <- c(report_content,
                            "## Uji Homogenitas Varians",
                            "",
                            paste("**Variabel Numerik:** ", input$homogeneity_numeric),
                            paste("**Variabel Grup:** ", input$homogeneity_group),
                            paste("**Metode:** ", switch(input$homogeneity_test,
                                                         "levene" = "Levene's Test",
                                                         "bartlett" = "Bartlett's Test",
                                                         "fligner" = "Fligner-Killeen Test")),
                            "",
                            "### Hipotesis",
                            "",
                            "- **H₀:** Varians antar grup homogen",
                            "- **H₁:** Varians antar grup tidak homogen",
                            "- **α:** 0.05",
                            ""
        )
        
        p_value <- if(input$homogeneity_test == "levene") values$homogeneity_result$`Pr(>F)`[1] else values$homogeneity_result$p.value
        
        report_content <- c(report_content,
                            "### Hasil",
                            "",
                            paste("- **P-value:** ", format_pvalue(p_value)),
                            ""
        )
        
        if (p_value > 0.05) {
          report_content <- c(report_content,
                              "### Kesimpulan",
                              "",
                              "- **Keputusan:** Gagal menolak H₀",
                              "- **Interpretasi:** Varians antar grup homogen (α = 0.05)",
                              "- **Implikasi:** Asumsi homogenitas terpenuhi untuk ANOVA",
                              ""
          )
        } else {
          report_content <- c(report_content,
                              "### Kesimpulan",
                              "",
                              "- **Keputusan:** Tolak H₀",
                              "- **Interpretasi:** Varians antar grup tidak homogen (α = 0.05)",
                              "- **Implikasi:** Gunakan Welch's test atau transformasi data",
                              ""
          )
        }
      }
    }
    
    # T-test Results
    if (values$ttest_done && !is.null(values$ttest_result)) {
      report_content <- c(report_content,
                          "",
                          "\\newpage",
                          "",
                          "# HASIL UJI BEDA RATA-RATA (T-TEST)",
                          "",
                          paste("## Jenis Uji:", switch(input$ttest_type,
                                                        "one_sample" = "One Sample t-test",
                                                        "independent" = "Independent Samples t-test",
                                                        "paired" = "Paired Samples t-test")),
                          "",
                          paste("**Variabel:** ", input$ttest_variable),
                          paste("**Confidence Level:** ", input$confidence_level * 100, "%"),
                          ""
      )
      
      # Add hypothesis
      if (input$ttest_type == "one_sample") {
        report_content <- c(report_content,
                            "### Hipotesis",
                            "",
                            paste("- **H₀:** μ =", input$test_value),
                            paste("- **H₁:** μ", switch(input$alternative,
                                                        "two.sided" = "≠",
                                                        "greater" = ">",
                                                        "less" = "<"), input$test_value),
                            ""
        )
      } else if (input$ttest_type == "independent") {
        report_content <- c(report_content,
                            "### Hipotesis",
                            "",
                            "- **H₀:** μ₁ = μ₂",
                            paste("- **H₁:** μ₁", switch(input$alternative,
                                                         "two.sided" = "≠",
                                                         "greater" = ">",
                                                         "less" = "<"), "μ₂"),
                            ""
        )
      } else {
        report_content <- c(report_content,
                            "### Hipotesis",
                            "",
                            "- **H₀:** μd = 0",
                            paste("- **H₁:** μd", switch(input$alternative,
                                                         "two.sided" = "≠",
                                                         "greater" = ">",
                                                         "less" = "<"), "0"),
                            ""
        )
      }
      
      # Add results
      result <- values$ttest_result
      report_content <- c(report_content,
                          "### Hasil",
                          "",
                          paste("- **t-statistic:** ", round(result$statistic, 4)),
                          paste("- **Degrees of Freedom:** ", result$parameter),
                          paste("- **P-value:** ", format_pvalue(result$p.value)),
                          ""
      )
      
      # Add confidence interval if available
      if (!is.null(result$conf.int)) {
        ci <- result$conf.int
        report_content <- c(report_content,
                            paste("- **Confidence Interval (", input$confidence_level * 100, "%):** [", 
                                  round(ci[1], 4), ", ", round(ci[2], 4), "]"),
                            ""
        )
      }
      
      # Add interpretation
      p_value <- result$p.value
      report_content <- c(report_content,
                          "### Interpretasi",
                          "",
                          paste("- **P-value:** ", format_pvalue(p_value)),
                          paste("- **Signifikansi:** ", interpret_significance(p_value)),
                          ""
      )
      
      if (p_value <= 0.05) {
        report_content <- c(report_content,
                            "### Kesimpulan",
                            "",
                            "- **Keputusan:** Tolak H₀",
                            "- **Interpretasi:** Terdapat perbedaan yang signifikan secara statistik",
                            "- **Rekomendasi:** Hasil dapat dipublikasikan sebagai temuan signifikan",
                            ""
        )
      } else {
        report_content <- c(report_content,
                            "### Kesimpulan",
                            "",
                            "- **Keputusan:** Gagal menolak H₀",
                            "- **Interpretasi:** Tidak ada perbedaan yang signifikan secara statistik",
                            "- **Rekomendasi:** Pertimbangkan power analysis dan kemungkinan Type II error",
                            ""
        )
      }
    }
    
    # ANOVA Results
    if (values$anova_done && !is.null(values$anova_result)) {
      report_content <- c(report_content,
                          "",
                          "\\newpage",
                          "",
                          "# HASIL ANALYSIS OF VARIANCE (ANOVA)",
                          "",
                          paste("## Jenis ANOVA:", if(values$anova_result$type == "one_way") "One-Way ANOVA" else "Two-Way ANOVA"),
                          "",
                          paste("**Variabel Dependen:** ", input$anova_dependent),
                          paste("**Faktor 1:** ", input$anova_factor1)
      )
      
      if (values$anova_result$type == "two_way") {
        report_content <- c(report_content,
                            paste("**Faktor 2:** ", input$anova_factor2),
                            paste("**Interaksi:** ", if(input$include_interaction) "Ya" else "Tidak")
        )
      }
      
      report_content <- c(report_content,
                          paste("**Alpha Level:** ", input$anova_alpha),
                          "",
                          "### Hasil ANOVA",
                          ""
      )
      
      anova_table <- values$anova_result$summary[[1]]
      
      if (values$anova_result$type == "one_way") {
        f_stat <- anova_table$`F value`[1]
        p_value <- anova_table$`Pr(>F)`[1]
        eta_sq <- values$anova_result$eta_squared
        
        report_content <- c(report_content,
                            paste("- **F-statistic:** ", round(f_stat, 4)),
                            paste("- **P-value:** ", format_pvalue(p_value)),
                            paste("- **Eta Squared (η²):** ", round(eta_sq, 4)),
                            paste("- **Effect Size:** ", interpret_effect_size(eta_sq)),
                            ""
        )
        
        if (p_value <= input$anova_alpha) {
          report_content <- c(report_content,
                              "### Kesimpulan",
                              "",
                              "- **Status:** SIGNIFIKAN",
                              "- **Interpretasi:** Terdapat perbedaan rata-rata yang signifikan antar grup",
                              paste("- **Proporsi Varians Dijelaskan:** ", round(eta_sq * 100, 2), "%"),
                              "- **Rekomendasi:** Lakukan post-hoc test untuk identifikasi grup yang berbeda",
                              ""
          )
        } else {
          report_content <- c(report_content,
                              "### Kesimpulan",
                              "",
                              "- **Status:** TIDAK SIGNIFIKAN",
                              "- **Interpretasi:** Tidak ada perbedaan rata-rata yang signifikan antar grup",
                              "- **Rekomendasi:** Pertimbangkan power analysis dan evaluasi kemungkinan Type II error",
                              ""
          )
        }
      }
    }
    
    # Regression Results
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      model_summary <- summary(model)
      
      report_content <- c(report_content,
                          "",
                          "\\newpage",
                          "",
                          "# HASIL ANALISIS REGRESI LINEAR",
                          "",
                          paste("**Variabel Dependen:** ", input$reg_dependent),
                          paste("**Variabel Independen:** ", paste(input$reg_independent, collapse = ", ")),
                          paste("**Intercept:** ", if(input$include_intercept) "Included" else "Excluded"),
                          "",
                          "## Statistik Kesesuaian Model",
                          "",
                          paste("- **R-squared:** ", round(model_summary$r.squared, 4)),
                          paste("- **Adjusted R-squared:** ", round(model_summary$adj.r.squared, 4)),
                          paste("- **F-statistic:** ", round(model_summary$fstatistic[1], 4)),
                          paste("- **P-value (F-test):** ", format_pvalue(pf(model_summary$fstatistic[1], 
                                                                             model_summary$fstatistic[2], 
                                                                             model_summary$fstatistic[3], 
                                                                             lower.tail = FALSE))),
                          paste("- **Residual Standard Error:** ", round(model_summary$sigma, 4)),
                          paste("- **Degrees of Freedom:** ", model$df.residual),
                          paste("- **Observations:** ", nobs(model)),
                          ""
      )
      
      # Model interpretation
      r_sq <- model_summary$r.squared
      f_p_value <- pf(model_summary$fstatistic[1], 
                      model_summary$fstatistic[2], 
                      model_summary$fstatistic[3], 
                      lower.tail = FALSE)
      
      report_content <- c(report_content,
                          "## Interpretasi Model",
                          "",
                          paste("Model menjelaskan **", round(r_sq * 100, 2), "%** variasi dalam variabel ", input$reg_dependent, "."),
                          ""
      )
      
      if (r_sq >= 0.7) {
        report_content <- c(report_content, "**Kekuatan Prediksi:** SANGAT BAIK")
      } else if (r_sq >= 0.5) {
        report_content <- c(report_content, "**Kekuatan Prediksi:** BAIK")
      } else if (r_sq >= 0.3) {
        report_content <- c(report_content, "**Kekuatan Prediksi:** MODERAT")
      } else {
        report_content <- c(report_content, "**Kekuatan Prediksi:** LEMAH")
      }
      
      # Significant predictors
      coeffs <- model_summary$coefficients
      sig_vars <- rownames(coeffs)[coeffs[, 4] <= 0.05 & rownames(coeffs) != "(Intercept)"]
      
      if (length(sig_vars) > 0) {
        report_content <- c(report_content,
                            "",
                            "## Variabel Prediktor Signifikan",
                            ""
        )
        
        for (var in sig_vars) {
          coeff <- coeffs[var, 1]
          p_val <- coeffs[var, 4]
          report_content <- c(report_content,
                              paste("- **", var, ":** koefisien = ", round(coeff, 4), 
                                    " (", ifelse(coeff > 0, "pengaruh positif", "pengaruh negatif"), 
                                    ", p = ", format_pvalue(p_val), ")")
          )
        }
      } else {
        report_content <- c(report_content,
                            "",
                            "**Catatan:** Tidak ada variabel prediktor yang signifikan pada α = 0.05",
                            ""
        )
      }
      
      # Model recommendations
      report_content <- c(report_content,
                          "",
                          "## Rekomendasi",
                          ""
      )
      
      if (f_p_value <= 0.05 && r_sq >= 0.3) {
        report_content <- c(report_content,
                            "1. Model dapat digunakan untuk prediksi",
                            "2. Periksa asumsi regresi untuk validitas",
                            "3. Pertimbangkan validasi silang untuk evaluasi performa"
        )
      } else if (f_p_value <= 0.05 && r_sq < 0.3) {
        report_content <- c(report_content,
                            "1. Model signifikan tetapi kekuatan prediksi lemah",
                            "2. Pertimbangkan menambah variabel prediktor",
                            "3. Evaluasi transformasi variabel"
        )
      } else {
        report_content <- c(report_content,
                            "1. Model tidak signifikan - tidak cocok untuk prediksi",
                            "2. Pertimbangkan model alternatif",
                            "3. Evaluasi kembali pemilihan variabel"
        )
      }
    }
    
    # Spatial Analysis Results
    if (values$spatial_done && !is.null(values$spatial_result)) {
      result <- values$spatial_result
      
      report_content <- c(report_content,
                          "",
                          "\\newpage",
                          "",
                          "# HASIL ANALISIS SPASIAL",
                          "",
                          paste("**Variabel:** ", input$spatial_variable),
                          paste("**Metode Weight:** ", switch(input$weight_type,
                                                              "distance_inverse" = "Inverse Distance",
                                                              "distance_exp" = "Exponential Distance",
                                                              "knn" = paste("K-Nearest Neighbors (k=", input$k_neighbors, ")"))),
                          "",
                          "## Statistik Moran's I",
                          "",
                          paste("- **Moran's I:** ", round(result$morans_i, 4)),
                          paste("- **Expected I:** ", round(result$expected_i, 4)),
                          paste("- **Variance:** ", round(result$variance_i, 6)),
                          paste("- **Z-Score:** ", round(result$z_score, 4)),
                          paste("- **P-Value:** ", format_pvalue(result$p_value)),
                          paste("- **Significance:** ", interpret_significance(result$p_value)),
                          ""
      )
      
      # Spatial interpretation
      if (result$p_value < 0.05) {
        if (result$morans_i > result$expected_i) {
          pattern_type <- "AUTOKORELASI SPASIAL POSITIF SIGNIFIKAN"
          pattern_desc <- "Data cenderung bercluster atau berkelompok secara spasial. Wilayah dengan nilai tinggi dikelilingi oleh wilayah dengan nilai tinggi, dan sebaliknya."
        } else {
          pattern_type <- "AUTOKORELASI SPASIAL NEGATIF SIGNIFIKAN"
          pattern_desc <- "Data menunjukkan pola checkerboard dimana wilayah dengan nilai tinggi dikelilingi oleh wilayah dengan nilai rendah."
        }
      } else {
        pattern_type <- "TIDAK ADA AUTOKORELASI SPASIAL SIGNIFIKAN"
        pattern_desc <- "Data tersebar secara acak tanpa pola spasial yang jelas."
      }
      
      report_content <- c(report_content,
                          "## Interpretasi Pola Spasial",
                          "",
                          paste("**Pola Terdeteksi:** ", pattern_type),
                          "",
                          paste("**Deskripsi:** ", pattern_desc),
                          "",
                          "**Implikasi:**",
                          "",
                          "- Moran's I berkisar dari -1 (dispersi sempurna) hingga +1 (clustering sempurna)",
                          "- Nilai mendekati 0 menunjukkan pola acak",
                          "- Hasil ini penting untuk memahami pola geografis kerentanan sosial",
                          ""
      )
    }
    
    # Final Conclusions
    report_content <- c(report_content,
                        "",
                        "\\newpage",
                        "",
                        "# KESIMPULAN DAN REKOMENDASI",
                        "",
                        "## Ringkasan Temuan Utama",
                        "",
                        "Berdasarkan analisis statistik komprehensif yang telah dilakukan terhadap dataset Social Vulnerability Index (SoVI), beberapa temuan utama dapat disimpulkan:",
                        ""
    )
    
    # Add specific conclusions based on completed analyses
    conclusion_points <- c()
    
    if (values$desc_generated) {
      conclusion_points <- c(conclusion_points, "1. **Analisis Deskriptif:** Karakteristik distribusi data telah diidentifikasi dengan detail statistik yang komprehensif")
    }
    
    if (values$viz_generated) {
      conclusion_points <- c(conclusion_points, "2. **Visualisasi Multivariat:** Pola hubungan antar variabel telah divisualisasikan untuk memahami struktur data")
    }
    
    if (values$map_generated) {
      conclusion_points <- c(conclusion_points, "3. **Distribusi Spasial:** Pola geografis kerentanan sosial telah dipetakan untuk identifikasi area prioritas")
    }
    
    if (values$normality_done || values$homogeneity_done) {
      conclusion_points <- c(conclusion_points, "4. **Validasi Asumsi:** Asumsi statistik telah diuji untuk memastikan validitas analisis lanjutan")
    }
    
    if (values$ttest_done) {
      conclusion_points <- c(conclusion_points, "5. **Uji Beda Rata-rata:** Perbedaan signifikan antar grup telah diidentifikasi")
    }
    
    if (values$anova_done) {
      conclusion_points <- c(conclusion_points, "6. **ANOVA:** Analisis varians telah mengidentifikasi faktor-faktor yang berpengaruh signifikan")
    }
    
    if (values$regression_done) {
      conclusion_points <- c(conclusion_points, "7. **Regresi Linear:** Model prediktif telah dikembangkan untuk memahami hubungan antar variabel")
    }
    
    if (values$spatial_done) {
      conclusion_points <- c(conclusion_points, "8. **Analisis Spasial:** Pola autokorelasi spasial telah dianalisis untuk memahami clustering geografis")
    }
    
    report_content <- c(report_content, conclusion_points, "")
    
    # General recommendations
    report_content <- c(report_content,
                        "## Rekomendasi Umum",
                        "",
                        "1. **Validasi Hasil:** Lakukan validasi silang untuk memastikan robustness temuan",
                        "2. **Analisis Lanjutan:** Pertimbangkan metode machine learning untuk analisis prediktif yang lebih kompleks",
                        "3. **Implementasi Kebijakan:** Gunakan temuan spasial untuk targeting program intervensi",
                        "4. **Monitoring Berkelanjutan:** Lakukan pemantauan berkala untuk tracking perubahan pola kerentanan",
                        "5. **Integrasi Data:** Pertimbangkan integrasi dengan data sekunder untuk analisis yang lebih komprehensif",
                        "",
                        "## Limitasi Penelitian",
                        "",
                        "1. **Data Cross-sectional:** Analisis ini menggunakan data cross-sectional sehingga tidak dapat menangkap perubahan temporal",
                        "2. **Koordinat Sintetis:** Koordinat geografis yang digunakan bersifat sintetis untuk keperluan demonstrasi",
                        "3. **Asumsi Model:** Hasil analisis bergantung pada terpenuhinya asumsi-asumsi statistik yang telah diuji",
                        "",
                        "---",
                        "",
                        "**Laporan ini dibuat secara otomatis oleh WASKITA Dashboard**",
                        "",
                        paste("*Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "WIB*"),
                        "",
                        "**Referensi:**",
                        "",
                        "- Dataset: Social Vulnerability Index (SoVI)",
                        "- Journal: Data in Brief, Elsevier",
                        "- DOI: 10.1016/j.dib.2021.107618",
                        "- Platform: WASKITA Dashboard - Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik"
    )
    
    return(report_content)
  }
  
  # =============================================================================
  # COMPREHENSIVE REPORT DOWNLOAD HANDLERS
  # =============================================================================
  
  # Generate Comprehensive PDF Report
  output$generate_comprehensive_report_pdf <- downloadHandler(
    filename = function() {
      paste0("WASKITA_Comprehensive_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      # Show progress notification
      showNotification("Generating comprehensive PDF report...", type = "message", duration = NULL, id = "pdf_progress")
      
      tryCatch({
        # Create temporary Rmd file
        temp_rmd <- tempfile(fileext = ".Rmd")
        
        # Generate comprehensive report content
        report_content <- generate_comprehensive_rmd_report()
        
        # Write content to file
        writeLines(report_content, temp_rmd)
        
        # Render to PDF
        rmarkdown::render(
          temp_rmd, 
          output_format = rmarkdown::pdf_document(
            toc = TRUE,
            toc_depth = 3,
            number_sections = TRUE,
            latex_engine = "xelatex"
          ),
          output_file = file,
          quiet = TRUE,
          envir = new.env()
        )
        
        # Remove progress notification and show success
        removeNotification("pdf_progress")
        showNotification("Comprehensive PDF report generated successfully!", type = "success", duration = 5)
        
      }, error = function(e) {
        removeNotification("pdf_progress")
        showNotification(paste("Error generating PDF report:", e$message), type = "error", duration = 10)
      })
    }
  )
  
  # Generate Comprehensive Word Report
  output$generate_comprehensive_report_word <- downloadHandler(
    filename = function() {
      paste0("WASKITA_Comprehensive_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
    },
    content = function(file) {
      # Show progress notification
      showNotification("Generating comprehensive Word report...", type = "message", duration = NULL, id = "word_progress")
      
      tryCatch({
        # Create temporary Rmd file
        temp_rmd <- tempfile(fileext = ".Rmd")
        
        # Generate comprehensive report content
        report_content <- generate_comprehensive_rmd_report()
        
        # Write content to file
        writeLines(report_content, temp_rmd)
        
        # Render to Word
        rmarkdown::render(
          temp_rmd, 
          output_format = rmarkdown::word_document(
            toc = TRUE,
            toc_depth = 3
          ),
          output_file = file,
          quiet = TRUE,
          envir = new.env()
        )
        
        # Remove progress notification and show success
        removeNotification("word_progress")
        showNotification("Comprehensive Word report generated successfully!", type = "success", duration = 5)
        
      }, error = function(e) {
        removeNotification("word_progress")
        showNotification(paste("Error generating Word report:", e$message), type = "error", duration = 10)
      })
    }
  )
  
  # =============================================================================
  # DOWNLOAD HANDLERS
  # =============================================================================
  
  # Download original data
  output$download_original <- downloadHandler(
    filename = function() {
      create_download_filename("original_data", "csv")
    },
    content = function(file) {
      if (!is.null(values$original_data)) {
        write.csv(values$original_data, file, row.names = FALSE)
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
    }
  )
  
  # Download full report PDF
  output$download_full_report_pdf <- downloadHandler(
    filename = function() {
      create_download_filename("full_report", "pdf")
    },
    content = function(file) {
      # Create temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Generate report content
      report_content <- generate_full_report_content()
      
      writeLines(report_content, temp_rmd)
      
      # Render to PDF
      rmarkdown::render(temp_rmd, 
                        output_format = "pdf_document",
                        output_file = file,
                        quiet = TRUE)
    }
  )
  
  # Download full report Word
  output$download_full_report_word <- downloadHandler(
    filename = function() {
      create_download_filename("full_report", "docx")
    },
    content = function(file) {
      # Create temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Generate report content
      report_content <- generate_full_report_content()
      
      writeLines(report_content, temp_rmd)
      
      # Render to Word
      rmarkdown::render(temp_rmd, 
                        output_format = "word_document",
                        output_file = file,
                        quiet = TRUE)
    }
  )
  
  # Additional download handlers for other sections
  output$download_map_image <- downloadHandler(
    filename = function() {
      create_download_filename("spatial_map", "png")
    },
    content = function(file) {
      # Create a static version of the map for download
      if (values$map_generated && !is.null(values$map_data)) {
        # Create a simple ggplot version of the map
        p <- ggplot(values$map_data, aes(x = lon, y = lat, color = value)) +
          geom_point(size = 2, alpha = 0.7) +
          scale_color_viridis_c(name = input$map_variable) +
          labs(title = paste("Spatial Distribution:", input$map_variable),
               x = "Longitude", y = "Latitude") +
          theme_waskita()
        
        ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
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
  
  # Generate full report content
  generate_full_report_content <- function() {
    content <- c(
      "---",
      "title: 'WASKITA Dashboard - Laporan Analisis Lengkap'",
      "subtitle: 'Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik'",
      "author: 'Generated by WASKITA Dashboard'",
      paste("date: '", Sys.Date(), "'"),
      "output:",
      "  pdf_document:",
      "    toc: true",
      "    toc_depth: 3",
      "  word_document:",
      "    toc: true",
      "    toc_depth: 3",
      "---",
      "",
      "# Executive Summary",
      "",
      "Laporan ini berisi analisis komprehensif dari dataset Social Vulnerability Index (SoVI) menggunakan platform WASKITA Dashboard.",
      "",
      "## Dataset Overview",
      ""
    )
    
    if (!is.null(values$processed_data)) {
      content <- c(content,
                   paste("- Total observasi:", nrow(values$processed_data)),
                   paste("- Total variabel:", ncol(values$processed_data)),
                   paste("- Missing values:", sum(is.na(values$processed_data))),
                   paste("- Variabel numerik:", sum(sapply(values$processed_data, is.numeric))),
                   paste("- Variabel kategorikal:", sum(sapply(values$processed_data, function(x) is.character(x) | is.factor(x)))),
                   ""
      )
    }
    
    content <- c(content,
                 "# Metadata Variabel",
                 "",
                 "Berikut adalah deskripsi lengkap variabel dalam dataset:",
                 ""
    )
    
    # Add metadata table
    for (i in 1:nrow(sovi_metadata)) {
      content <- c(content,
                   paste("**", sovi_metadata$Variabel[i], "**:", sovi_metadata$Keterangan[i], 
                         "(", sovi_metadata$Tipe_Data[i], ")")
      )
    }
    
    content <- c(content,
                 "",
                 "# Analisis yang Telah Dilakukan",
                 ""
    )
    
    if (values$desc_generated) {
      content <- c(content,
                   "## Analisis Deskriptif",
                   paste("- Variabel yang dianalisis:", values$current_desc_variable),
                   ""
      )
    }
    
    if (values$normality_done) {
      content <- c(content,
                   "## Uji Normalitas",
                   paste("- Metode:", input$normality_test),
                   paste("- Variabel:", input$normality_variable),
                   ""
      )
    }
    
    if (values$ttest_done) {
      content <- c(content,
                   "## Uji t",
                   paste("- Jenis:", input$ttest_type),
                   paste("- Variabel:", input$ttest_variable),
                   ""
      )
    }
    
    if (values$regression_done) {
      content <- c(content,
                   "## Analisis Regresi",
                   paste("- Variabel dependen:", input$reg_dependent),
                   paste("- Variabel independen:", paste(input$reg_independent, collapse = ", ")),
                   ""
      )
    }
    
    if (values$spatial_done) {
      content <- c(content,
                   "## Analisis Spasial",
                   paste("- Variabel:", input$spatial_variable),
                   paste("- Metode weight:", input$weight_type),
                   ""
      )
    }
    
    content <- c(content,
                 "# Kesimpulan",
                 "",
                 "Analisis telah dilakukan menggunakan platform WASKITA Dashboard dengan berbagai metode statistik yang sesuai untuk data Social Vulnerability Index.",
                 "",
                 "---",
                 "",
                 "*Laporan ini dibuat secara otomatis oleh WASKITA Dashboard*"
    )
    
    return(content)
  }
  
  # Placeholder download handlers for remaining buttons
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
    }
  )
  
  # Add similar placeholder handlers for other downloads
  output$download_ttest_plot <- downloadHandler(
    filename = function() { create_download_filename("ttest_plot", "png") },
    content = function(file) {
      if (values$ttest_done) {
        p <- ggplot(values$processed_data, aes_string(x = input$ttest_variable)) +
          geom_histogram(bins = 30, fill = waskita_colors$steel, alpha = 0.7) +
          labs(title = "T-test Results") +
          theme_waskita()
        ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
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
    }
  )
  
  # Add more placeholder handlers
  output$download_propvar_plots <- downloadHandler(
    filename = function() { create_download_filename("propvar_plots", "png") },
    content = function(file) {
      p <- ggplot() + geom_blank() + labs(title = "Proportion & Variance Test Plots") + theme_waskita()
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_propvar_results <- downloadHandler(
    filename = function() { create_download_filename("propvar_results", "csv") },
    content = function(file) {
      results_df <- data.frame(Test = "Proportion/Variance", Status = "Completed")
      write.csv(results_df, file, row.names = FALSE)
    }
  )
  
  output$download_propvar_report <- downloadHandler(
    filename = function() { create_download_filename("propvar_report", "pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      content <- c("---", "title: 'Proportion & Variance Report'", "output: pdf_document", "---", "", "# Results")
      writeLines(content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
    }
  )
  
  output$download_anova_plots <- downloadHandler(
    filename = function() { create_download_filename("anova_plots", "png") },
    content = function(file) {
      p <- ggplot() + geom_blank() + labs(title = "ANOVA Plots") + theme_waskita()
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_anova_results <- downloadHandler(
    filename = function() { create_download_filename("anova_results", "csv") },
    content = function(file) {
      results_df <- data.frame(Test = "ANOVA", Status = "Completed")
      write.csv(results_df, file, row.names = FALSE)
    }
  )
  
  output$download_anova_report <- downloadHandler(
    filename = function() { create_download_filename("anova_report", "pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      content <- c("---", "title: 'ANOVA Report'", "output: pdf_document", "---", "", "# Results")
      writeLines(content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
    }
  )
  
  output$download_regression_plots <- downloadHandler(
    filename = function() { create_download_filename("regression_plots", "png") },
    content = function(file) {
      p <- ggplot() + geom_blank() + labs(title = "Regression Plots") + theme_waskita()
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_regression_results <- downloadHandler(
    filename = function() { create_download_filename("regression_results", "csv") },
    content = function(file) {
      results_df <- data.frame(Test = "Regression", Status = "Completed")
      write.csv(results_df, file, row.names = FALSE)
    }
  )
  
  output$download_regression_report <- downloadHandler(
    filename = function() { create_download_filename("regression_report", "pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      content <- c("---", "title: 'Regression Report'", "output: pdf_document", "---", "", "# Results")
      writeLines(content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
    }
  )
  
  output$download_spatial_plots <- downloadHandler(
    filename = function() { create_download_filename("spatial_plots", "png") },
    content = function(file) {
      p <- ggplot() + geom_blank() + labs(title = "Spatial Analysis Plots") + theme_waskita()
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_distance_matrix <- downloadHandler(
    filename = function() { create_download_filename("distance_matrix", "csv") },
    content = function(file) {
      if (!is.null(values$distance_data)) {
        write.csv(values$distance_data, file, row.names = FALSE)
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
    }
  )
  
  output$download_spatial_report <- downloadHandler(
    filename = function() { create_download_filename("spatial_map_report", "pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      content <- c("---", "title: 'Spatial Map Report'", "output: pdf_document", "---", "", "# Results")
      writeLines(content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
    }
  )
  
}