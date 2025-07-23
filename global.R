# =============================================================================
# WASKITA Dashboard - Global Configuration
# Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik
# =============================================================================

# Package Loading dengan Error Handling
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinycssloaders",
  "DT", "plotly", "ggplot2", "leaflet", "sf",
  "dplyr", "readr", "tidyr", "stringr",
  "nortest", "car", "lmtest", "moments", "psych",
  "corrplot", "RColorBrewer", "viridis", "scales",
  "knitr", "rmarkdown", "officer", "flextable",
  "htmltools", "htmlwidgets", "webshot",
  "MASS", "forecast", "broom", "geojsonio", "terra",
  "sp", "sf", "tmap", "tmaptools", "rsconnect"
)

options (repos = c(CRAN = "https://cran.rstudio.com/"))
# Install missing packages automatically
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(missing_packages)) install.packages(missing_packages, dependencies = TRUE)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
writeManifest()
# =============================================================================
# KONFIGURASI GLOBAL
# =============================================================================

# Color Palette WASKITA
waskita_colors <- list(
  navy = "#1B3C53",        # Primary dark
  steel = "#456882",       # Primary medium 
  warm_gray = "#D2C1B6",   # Secondary light
  cream = "#F9F3EF",       # Background light
  white = "#FFFFFF",       # Pure white
  text_dark = "#1B3C53",   # Text primary
  text_muted = "#64748b",  # Text secondary
  border = "#e2e8f0",      # Border color
  success = "#10b981",     # Success color
  warning = "#f59e0b",     # Warning color
  error = "#ef4444"        # Error color
)

# Path Konfigurasi Data
local_data_dir <- "D:/Perkuliahan Tingkat 2 Semester 4/WASKITA2/data"
local_sovi_path <- file.path(local_data_dir, "sovi_data.csv")
local_distance_path <- file.path(local_data_dir, "distance.csv")
local_geojson_path <- file.path(local_data_dir, "indonesia511.geojson")

# Backup URLs jika data lokal tidak tersedia
backup_urls <- list(
  sovi = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv",
  distance = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv",
  geojson = "https://raw.githubusercontent.com/superpikar/indonesia-geojson/master/indonesia.geojson"
)

# =============================================================================
# REPORT TRACKING SYSTEM
# =============================================================================

# Global report tracker
report_tracker <- reactiveValues(
  analyses_performed = list(),
  data_transformations = list(),
  visualizations_created = list(),
  statistical_tests = list(),
  spatial_analyses = list(),
  timestamp = Sys.time()
)

# Function to add analysis to report tracker
add_to_report <- function(tracker, type, title, content, plots = NULL, tables = NULL) {
  new_entry <- list(
    timestamp = Sys.time(),
    type = type,
    title = title,
    content = content,
    plots = plots,
    tables = tables
  )
  
  # Add to appropriate category
  if (type == "transformation") {
    tracker$data_transformations[[length(tracker$data_transformations) + 1]] <- new_entry
  } else if (type == "visualization") {
    tracker$visualizations_created[[length(tracker$visualizations_created) + 1]] <- new_entry
  } else if (type == "test") {
    tracker$statistical_tests[[length(tracker$statistical_tests) + 1]] <- new_entry
  } else if (type == "spatial") {
    tracker$spatial_analyses[[length(tracker$spatial_analyses) + 1]] <- new_entry
  } else {
    tracker$analyses_performed[[length(tracker$analyses_performed) + 1]] <- new_entry
  }
}

# =============================================================================
# FUNGSI UTILITAS
# =============================================================================

# Format p-value
format_pvalue <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  if (p < 0.01) return(sprintf("%.3f", p))
  return(sprintf("%.3f", p))
}

# Interpretasi signifikansi
interpret_significance <- function(p_value, alpha = 0.05) {
  if (p_value < 0.001) {
    return("Sangat signifikan (p < 0.001)")
  } else if (p_value < 0.01) {
    return("Sangat signifikan (p < 0.01)")
  } else if (p_value < alpha) {
    return(paste("Signifikan (p <", alpha, ")"))
  } else {
    return("Tidak signifikan")
  }
}

# Interpretasi Effect Size (Cohen's d)
interpret_cohens_d <- function(d) {
  if (is.na(d)) return("Tidak dapat dihitung")
  abs_d <- abs(d)
  if (abs_d < 0.2) return("Sangat kecil")
  if (abs_d < 0.5) return("Kecil")
  if (abs_d < 0.8) return("Sedang")
  return("Besar")
}

# Interpretasi korelasi
interpret_correlation <- function(r) {
  abs_r <- abs(r)
  if (abs_r < 0.1) return("Sangat lemah")
  if (abs_r < 0.3) return("Lemah")
  if (abs_r < 0.5) return("Sedang")
  if (abs_r < 0.7) return("Kuat")
  return("Sangat kuat")
}

# Interpretasi Effect Size (Eta squared)
interpret_effect_size <- function(eta_sq) {
  if (is.na(eta_sq)) return("Tidak dapat dihitung")
  if (eta_sq < 0.01) return("Negligible")
  if (eta_sq < 0.06) return("Small")
  if (eta_sq < 0.14) return("Medium")
  return("Large")
}

# =============================================================================
# TEMA GGPLOT CUSTOM
# =============================================================================

theme_waskita <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(
        size = 14, face = "bold", hjust = 0.5,
        color = waskita_colors$navy, margin = margin(b = 15)
      ),
      plot.subtitle = element_text(
        size = 11, hjust = 0.5,
        color = waskita_colors$text_muted, margin = margin(b = 10)
      ),
      axis.title = element_text(size = 11, color = waskita_colors$text_dark),
      axis.text = element_text(size = 9, color = waskita_colors$text_muted),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = waskita_colors$border, linewidth = 0.3),
      panel.border = element_rect(fill = NA, color = waskita_colors$border),
      strip.background = element_rect(fill = waskita_colors$cream, color = waskita_colors$border),
      strip.text = element_text(size = 10, face = "bold", color = waskita_colors$navy),
      plot.background = element_rect(fill = waskita_colors$white, color = NA),
      panel.background = element_rect(fill = waskita_colors$white, color = NA)
    )
}

# Set default theme
theme_set(theme_waskita())

# =============================================================================
# FUNGSI LOADING DATA
# =============================================================================

load_waskita_data <- function() {
  tryCatch({
    # Coba load dari lokal terlebih dahulu
    if (file.exists(local_sovi_path) && file.exists(local_distance_path)) {
      sovi_data <- read_csv(local_sovi_path, show_col_types = FALSE)
      distance_data <- read_csv(local_distance_path, show_col_types = FALSE)
      return(list(sovi = sovi_data, distance = distance_data, source = "local"))
    } else {
      # Fallback ke URL
      sovi_data <- read_csv(backup_urls$sovi, show_col_types = FALSE)
      distance_data <- read_csv(backup_urls$distance, show_col_types = FALSE)
      return(list(sovi = sovi_data, distance = distance_data, source = "url"))
    }
  }, error = function(e) {
    return(list(error = paste("Gagal memuat data:", e$message)))
  })
}

# =============================================================================
# FUNGSI LOADING GEOJSON
# =============================================================================

load_indonesia_geojson <- function() {
  tryCatch({
    # Coba load dari lokal terlebih dahulu
    if (file.exists(local_geojson_path)) {
      indonesia_sf <- st_read(local_geojson_path, quiet = TRUE)
      return(list(geojson = indonesia_sf, source = "local"))
    } else {
      # Fallback ke URL
      indonesia_sf <- st_read(backup_urls$geojson, quiet = TRUE)
      return(list(geojson = indonesia_sf, source = "url"))
    }
  }, error = function(e) {
    # Create synthetic spatial data if geojson fails
    tryCatch({
      set.seed(123)
      n_districts <- 511
      lat_range <- c(-11, 6)
      lon_range <- c(95, 141)
      
      # Create synthetic polygons
      synthetic_polygons <- list()
      for (i in 1:n_districts) {
        # Create small rectangular polygons
        center_lat <- runif(1, lat_range[1], lat_range[2])
        center_lon <- runif(1, lon_range[1], lon_range[2])
        
        # Small polygon around center
        delta <- 0.1
        coords <- matrix(c(
          center_lon - delta, center_lat - delta,
          center_lon + delta, center_lat - delta,
          center_lon + delta, center_lat + delta,
          center_lon - delta, center_lat + delta,
          center_lon - delta, center_lat - delta
        ), ncol = 2, byrow = TRUE)
        
        synthetic_polygons[[i]] <- st_polygon(list(coords))
      }
      
      synthetic_sf <- st_sf(
        DISTRICTCODE = paste0("DIST_", sprintf("%03d", 1:n_districts)),
        NAME = paste("District", 1:n_districts),
        geometry = st_sfc(synthetic_polygons, crs = 4326)
      )
      
      return(list(geojson = synthetic_sf, source = "synthetic"))
    }, error = function(e2) {
      return(list(error = paste("Gagal memuat data geografis:", e2$message)))
    })
  })
}

# =============================================================================
# METADATA VARIABEL
# =============================================================================

sovi_metadata <- data.frame(
  Variabel = c(
    "DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE",
    "LOWEDU", "POVERTY", "ILLITERATE", "NOTRAINING", "GROWTH", "NOELECTRIC",
    "RENTED", "NOSEWER", "TAPWATER", "DPHONE", "SOVI"
  ),
  Keterangan = c(
    "Kode Kabupaten/Kota",
    "Proporsi Anak Usia < 5 tahun",
    "Proporsi Perempuan",
    "Proporsi Lansia 65+ tahun",
    "Proporsi KRT Perempuan",
    "Rata-rata Ukuran Keluarga",
    "Proporsi Pendidikan Rendah",
    "Tingkat Kemiskinan",
    "Tingkat Buta Huruf",
    "Proporsi Tanpa Pelatihan Vokasi",
    "Tingkat Pertumbuhan Penduduk",
    "Proporsi Tanpa Akses Listrik",
    "Proporsi Rumah Sewa/Kontrak",
    "Proporsi Tanpa Sistem Sanitasi",
    "Proporsi Akses Air Bersih",
    "Indeks Rawan Bencana Alam",
    "Social Vulnerability Index"
  ),
  Tipe_Data = c(
    "Kategorikal", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik",
    "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik",
    "Numerik", "Numerik", "Numerik", "Numerik", "Numerik"
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# FUNGSI NOTIFIKASI AMAN
# =============================================================================

safe_notification <- function(message, type = "default", duration = 3) {
  tryCatch({
    showNotification(message, type = type, duration = duration)
  }, error = function(e) {
    cat("Notification:", message, "\n")
  })
}

# =============================================================================
# FUNGSI VALIDASI DATA
# =============================================================================

validate_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "Data kosong atau tidak valid"))
  }
  
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) < 2) {
    return(list(valid = FALSE, message = "Data harus memiliki minimal 2 kolom numerik"))
  }
  
  return(list(valid = TRUE, message = "Data valid"))
}

# =============================================================================
# FUNGSI DOWNLOAD HELPER
# =============================================================================

create_download_filename <- function(prefix, extension = "csv") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  paste0("WASKITA_", prefix, "_", timestamp, ".", extension)
}

# =============================================================================
# COMPREHENSIVE REPORT GENERATION FUNCTIONS
# =============================================================================

# Generate comprehensive report based on all user activities
generate_comprehensive_activity_report <- function(tracker, data_info = NULL) {
  report_content <- c(
    "---",
    "title: 'WASKITA DASHBOARD - LAPORAN AKTIVITAS ANALISIS KOMPREHENSIF'",
    "subtitle: 'Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik'",
    paste0("author: 'Generated by WASKITA Dashboard - ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "'"),
    paste0("date: '", format(Sys.Date(), "%d %B %Y"), "'"),
    "output:",
    "  pdf_document:",
    "    toc: true",
    "    toc_depth: 4",
    "    number_sections: true",
    "    latex_engine: xelatex",
    "  word_document:",
    "    toc: true",
    "    toc_depth: 4",
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
    "Laporan ini menyajikan hasil analisis statistik komprehensif yang telah dilakukan menggunakan platform **WASKITA Dashboard**. Semua aktivitas analisis yang dilakukan oleh pengguna telah didokumentasikan secara detail termasuk transformasi data, visualisasi, uji statistik, dan analisis spasial.",
    "",
    paste("**Waktu Generate Laporan:** ", format(Sys.time(), "%Y-%m-%d %H:%M:%S WIB")),
    "",
    "## Ringkasan Aktivitas Analisis",
    ""
  )
  
  # Dataset Overview
  if (!is.null(data_info)) {
    report_content <- c(report_content,
                        "### Informasi Dataset",
                        "",
                        paste("- **Total Observasi:** ", nrow(data_info)),
                        paste("- **Total Variabel:** ", ncol(data_info)),
                        paste("- **Missing Values:** ", sum(is.na(data_info))),
                        paste("- **Variabel Numerik:** ", sum(sapply(data_info, is.numeric))),
                        paste("- **Variabel Kategorikal:** ", sum(sapply(data_info, function(x) is.character(x) | is.factor(x)))),
                        ""
    )
  }
  
  # Summary of performed analyses
  total_analyses <- length(tracker$analyses_performed) + 
    length(tracker$data_transformations) + 
    length(tracker$visualizations_created) + 
    length(tracker$statistical_tests) + 
    length(tracker$spatial_analyses)
  
  report_content <- c(report_content,
                      "### Total Aktivitas Analisis",
                      "",
                      paste("- **Total Analisis Dilakukan:** ", total_analyses),
                      paste("- **Transformasi Data:** ", length(tracker$data_transformations)),
                      paste("- **Visualisasi Dibuat:** ", length(tracker$visualizations_created)),
                      paste("- **Uji Statistik:** ", length(tracker$statistical_tests)),
                      paste("- **Analisis Spasial:** ", length(tracker$spatial_analyses)),
                      paste("- **Analisis Lainnya:** ", length(tracker$analyses_performed)),
                      "",
                      "\\newpage",
                      ""
  )
  
  # DATA TRANSFORMATIONS SECTION
  if (length(tracker$data_transformations) > 0) {
    report_content <- c(report_content,
                        "# TRANSFORMASI DATA",
                        "",
                        "Berikut adalah semua transformasi data yang telah dilakukan:",
                        ""
    )
    
    for (i in seq_along(tracker$data_transformations)) {
      trans <- tracker$data_transformations[[i]]
      report_content <- c(report_content,
                          paste("## Transformasi", i, ":", trans$title),
                          "",
                          paste("**Waktu:** ", format(trans$timestamp, "%Y-%m-%d %H:%M:%S")),
                          "",
                          "### Detail Transformasi",
                          "",
                          trans$content,
                          "",
                          if (!is.null(trans$tables)) {
                            c("### Hasil Statistik", "", trans$tables, "")
                          } else { "" }
      )
    }
    report_content <- c(report_content, "\\newpage", "")
  }
  
  # VISUALIZATIONS SECTION
  if (length(tracker$visualizations_created) > 0) {
    report_content <- c(report_content,
                        "# VISUALISASI DATA",
                        "",
                        "Berikut adalah semua visualisasi yang telah dibuat:",
                        ""
    )
    
    for (i in seq_along(tracker$visualizations_created)) {
      viz <- tracker$visualizations_created[[i]]
      report_content <- c(report_content,
                          paste("## Visualisasi", i, ":", viz$title),
                          "",
                          paste("**Waktu:** ", format(viz$timestamp, "%Y-%m-%d %H:%M:%S")),
                          "",
                          "### Deskripsi Visualisasi",
                          "",
                          viz$content,
                          "",
                          if (!is.null(viz$tables)) {
                            c("### Statistik Terkait", "", viz$tables, "")
                          } else { "" }
      )
    }
    report_content <- c(report_content, "\\newpage", "")
  }
  
  # STATISTICAL TESTS SECTION
  if (length(tracker$statistical_tests) > 0) {
    report_content <- c(report_content,
                        "# UJI STATISTIK",
                        "",
                        "Berikut adalah semua uji statistik yang telah dilakukan:",
                        ""
    )
    
    for (i in seq_along(tracker$statistical_tests)) {
      test <- tracker$statistical_tests[[i]]
      report_content <- c(report_content,
                          paste("## Uji", i, ":", test$title),
                          "",
                          paste("**Waktu:** ", format(test$timestamp, "%Y-%m-%d %H:%M:%S")),
                          "",
                          "### Hasil Uji Statistik",
                          "",
                          test$content,
                          "",
                          if (!is.null(test$tables)) {
                            c("### Tabel Hasil", "", test$tables, "")
                          } else { "" }
      )
    }
    report_content <- c(report_content, "\\newpage", "")
  }
  
  # SPATIAL ANALYSES SECTION
  if (length(tracker$spatial_analyses) > 0) {
    report_content <- c(report_content,
                        "# ANALISIS SPASIAL",
                        "",
                        "Berikut adalah semua analisis spasial yang telah dilakukan:",
                        ""
    )
    
    for (i in seq_along(tracker$spatial_analyses)) {
      spatial <- tracker$spatial_analyses[[i]]
      report_content <- c(report_content,
                          paste("## Analisis Spasial", i, ":", spatial$title),
                          "",
                          paste("**Waktu:** ", format(spatial$timestamp, "%Y-%m-%d %H:%M:%S")),
                          "",
                          "### Hasil Analisis Spasial",
                          "",
                          spatial$content,
                          "",
                          if (!is.null(spatial$tables)) {
                            c("### Statistik Spasial", "", spatial$tables, "")
                          } else { "" }
      )
    }
    report_content <- c(report_content, "\\newpage", "")
  }
  
  # OTHER ANALYSES SECTION
  if (length(tracker$analyses_performed) > 0) {
    report_content <- c(report_content,
                        "# ANALISIS LAINNYA",
                        "",
                        "Berikut adalah analisis tambahan yang telah dilakukan:",
                        ""
    )
    
    for (i in seq_along(tracker$analyses_performed)) {
      analysis <- tracker$analyses_performed[[i]]
      report_content <- c(report_content,
                          paste("## Analisis", i, ":", analysis$title),
                          "",
                          paste("**Waktu:** ", format(analysis$timestamp, "%Y-%m-%d %H:%M:%S")),
                          "",
                          "### Hasil Analisis",
                          "",
                          analysis$content,
                          "",
                          if (!is.null(analysis$tables)) {
                            c("### Data Pendukung", "", analysis$tables, "")
                          } else { "" }
      )
    }
    report_content <- c(report_content, "\\newpage", "")
  }
  
  # METADATA SECTION
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
  
  # CONCLUSIONS AND RECOMMENDATIONS
  report_content <- c(report_content,
                      "\\newpage",
                      "",
                      "# KESIMPULAN DAN REKOMENDASI",
                      "",
                      "## Ringkasan Temuan Utama",
                      "",
                      "Berdasarkan semua analisis yang telah dilakukan menggunakan platform WASKITA Dashboard, berikut adalah ringkasan aktivitas:",
                      ""
  )
  
  # Dynamic conclusions based on what was actually performed
  if (length(tracker$data_transformations) > 0) {
    report_content <- c(report_content,
                        paste("1. **Transformasi Data:** Telah dilakukan", length(tracker$data_transformations), "transformasi data untuk optimalisasi analisis"))
  }
  
  if (length(tracker$visualizations_created) > 0) {
    report_content <- c(report_content,
                        paste("2. **Visualisasi:** Telah dibuat", length(tracker$visualizations_created), "visualisasi untuk eksplorasi data"))
  }
  
  if (length(tracker$statistical_tests) > 0) {
    report_content <- c(report_content,
                        paste("3. **Uji Statistik:** Telah dilakukan", length(tracker$statistical_tests), "uji statistik untuk validasi hipotesis"))
  }
  
  if (length(tracker$spatial_analyses) > 0) {
    report_content <- c(report_content,
                        paste("4. **Analisis Spasial:** Telah dilakukan", length(tracker$spatial_analyses), "analisis spasial untuk memahami pola geografis"))
  }
  
  report_content <- c(report_content,
                      "",
                      "## Rekomendasi Umum",
                      "",
                      "1. **Validasi Hasil:** Lakukan validasi silang untuk memastikan robustness temuan",
                      "2. **Analisis Lanjutan:** Pertimbangkan metode machine learning untuk analisis prediktif yang lebih kompleks",
                      "3. **Implementasi Kebijakan:** Gunakan temuan spasial untuk targeting program intervensi",
                      "4. **Monitoring Berkelanjutan:** Lakukan pemantauan berkala untuk tracking perubahan pola kerentanan",
                      "",
                      "## Limitasi Penelitian",
                      "",
                      "1. **Data Cross-sectional:** Analisis ini menggunakan data cross-sectional sehingga tidak dapat menangkap perubahan temporal",
                      "2. **Asumsi Model:** Hasil analisis bergantung pada terpenuhinya asumsi-asumsi statistik yang telah diuji",
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
# KONFIGURASI SHINY
# =============================================================================

options(
  shiny.maxRequestSize = 100*1024^2,  # 100MB max file size
  shiny.sanitize.errors = FALSE,
  warn = -1
)

# =============================================================================
# SETUP WEBSHOT (JIKA DIPERLUKAN)
# =============================================================================

if (!webshot::is_phantomjs_installed()) {
  tryCatch({
    webshot::install_phantomjs()
  }, error = function(e) {
    message("PhantomJS installation gagal. PDF generation mungkin tidak berfungsi.")
  })
}

# =============================================================================
# FUNGSI MORAN'S I
# =============================================================================

calculate_morans_i <- function(x, weights_matrix) {
  # Remove missing values
  na_indices <- is.na(x)
  if (any(na_indices)) {
    x <- x[!na_indices]
    weights_matrix <- weights_matrix[!na_indices, !na_indices, drop = FALSE]
  }
  
  n <- length(x)
  if (n < 2) {
    stop("Data tidak cukup untuk perhitungan Moran's I")
  }
  
  # Standardize values
  x_mean <- mean(x)
  x_centered <- x - x_mean
  
  # Calculate numerator and denominator
  numerator <- 0
  denominator <- sum(x_centered^2)
  total_weight <- 0
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j && !is.na(weights_matrix[i, j])) {
        numerator <- numerator + weights_matrix[i, j] * x_centered[i] * x_centered[j]
        total_weight <- total_weight + weights_matrix[i, j]
      }
    }
  }
  
  if (total_weight > 0 && denominator > 0) {
    morans_i <- (n * numerator) / (total_weight * denominator)
  } else {
    morans_i <- 0
  }
  
  return(morans_i)
}

# =============================================================================
# SPATIAL DATA FUNCTIONS
# =============================================================================

# Function to create choropleth map data
create_choropleth_data <- function(sovi_data, indonesia_sf, variable) {
  tryCatch({
    # Ensure DISTRICTCODE exists in both datasets
    if (!"DISTRICTCODE" %in% names(sovi_data)) {
      sovi_data$DISTRICTCODE <- paste0("DIST_", sprintf("%03d", 1:nrow(sovi_data)))
    }
    
    if (!"DISTRICTCODE" %in% names(indonesia_sf)) {
      indonesia_sf$DISTRICTCODE <- paste0("DIST_", sprintf("%03d", 1:nrow(indonesia_sf)))
    }
    
    # Join spatial data with variable data
    merged_data <- merge(indonesia_sf, sovi_data, by = "DISTRICTCODE", all.x = TRUE)
    
    return(merged_data)
  }, error = function(e) {
    return(NULL)
  })
}

cat("WASKITA Dashboard Enhanced Global Configuration loaded successfully!\n")
cat("Color palette, utilities, and comprehensive reporting system ready.\n")
cat("Spatial mapping capabilities enabled.\n")