# =============================================================================
# WASKITA Dashboard - Main Application
# Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik
# =============================================================================

# Load UI and Server
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
  