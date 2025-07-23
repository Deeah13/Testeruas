# =============================================================================
# WASKITA Dashboard - Global Configuration
# Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik
# =============================================================================
options (repos = c(CRAN = "https://cran.rstudio.com/"))
# Package Loading dengan Error Handling
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinycssloaders",
  "DT", "plotly", "ggplot2", "leaflet", "sf",
  "dplyr", "readr", "tidyr", "stringr",
  "nortest", "car", "lmtest", "moments", "psych",
  "corrplot", "RColorBrewer", "viridis", "scales",
  "knitr", "rmarkdown", "officer", "flextable",
  "htmltools", "htmlwidgets", "webshot",
  "MASS", "forecast", "broom", "rsconnect"
)

# Install missing packages automatically
#missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
#if(length(missing_packages)) install.packages(missing_packages, dependencies = TRUE)

for (pkg in required_packages) {
  #if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
   # install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  #}
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

# Cek dan baca SOVI data
if (file.exists("sovi_data.csv")) {
  sovi_data <- readr::read_csv("sovi_data.csv")
} else {
  sovi_data <- readr::read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv")
}

# Distance data
if (file.exists("distance.csv")) {
  distance_data <- readr::read_csv("distance.csv")
} else {
  distance_data <- readr::read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv")
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

cat("WASKITA Dashboard Global Configuration loaded successfully!\n")
cat("Color palette and utilities ready.\n")