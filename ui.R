# =============================================================================
# WASKITA Dashboard - User Interface
# Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik
# =============================================================================

source("global.R")

# =============================================================================
# CUSTOM CSS
# =============================================================================

custom_css <- tags$head(
  tags$style(HTML(paste0("
    /* Global Styling */
    body, .content-wrapper, .right-side {
      background: linear-gradient(135deg, ", waskita_colors$cream, " 0%, ", waskita_colors$white, " 100%) !important;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif !important;
      color: ", waskita_colors$text_dark, " !important;
    }
    
    /* Header Styling */
    .main-header .navbar {
      background: linear-gradient(135deg, ", waskita_colors$navy, " 0%, ", waskita_colors$steel, " 100%) !important;
      border: none !important;
      box-shadow: 0 4px 15px rgba(27, 60, 83, 0.3) !important;
    }
    
    .main-header .navbar-brand {
      color: ", waskita_colors$white, " !important;
      font-size: 24px !important;
      font-weight: bold !important;
      letter-spacing: 1px !important;
    }
    
    /* Sidebar Styling */
    .main-sidebar {
      background: ", waskita_colors$navy, " !important;
    }
    
    .sidebar-menu > li > a {
      color: rgba(255, 255, 255, 0.9) !important;
      padding: 15px 20px !important;
      margin: 3px 10px !important;
      border-radius: 8px !important;
      transition: all 0.3s ease !important;
      font-weight: 500 !important;
    }
    
    .sidebar-menu > li:hover > a,
    .sidebar-menu > li.active > a {
      background: rgba(255, 255, 255, 0.15) !important;
      color: ", waskita_colors$white, " !important;
      transform: translateX(5px) !important;
    }
    
    .sidebar-menu > li > a > .fa,
    .sidebar-menu > li > a > .fas {
      margin-right: 12px !important;
      font-size: 16px !important;
    }
    
    /* Content Area */
    .content {
      padding: 25px !important;
    }
    
    /* Box Styling */
    .box {
      border: none !important;
      border-radius: 15px !important;
      box-shadow: 0 8px 25px rgba(0, 0, 0, 0.08) !important;
      background: ", waskita_colors$white, " !important;
      margin-bottom: 25px !important;
      overflow: hidden !important;
      transition: all 0.3s ease !important;
    }
    
    .box:hover {
      box-shadow: 0 12px 35px rgba(0, 0, 0, 0.12) !important;
      transform: translateY(-3px) !important;
    }
    
    .box-header {
      background: linear-gradient(135deg, ", waskita_colors$navy, " 0%, ", waskita_colors$steel, " 100%) !important;
      color: ", waskita_colors$white, " !important;
      padding: 20px 25px !important;
      border: none !important;
      font-weight: 600 !important;
      font-size: 20px !important;
    }
    
    .box-body {
      padding: 25px !important;
      background: ", waskita_colors$white, " !important;
    }
    
    /* Load Data Section */
    .load-data-container {
      background: linear-gradient(135deg, ", waskita_colors$white, " 0%, ", waskita_colors$cream, " 100%) !important;
      border: 2px solid ", waskita_colors$border, " !important;
      border-radius: 20px !important;
      padding: 30px !important;
      text-align: center !important;
      transition: all 0.3s ease !important;
      position: relative !important;
      overflow: hidden !important;
    }
    
    .load-data-container::before {
      content: '' !important;
      position: absolute !important;
      top: -50% !important;
      right: -30% !important;
      width: 150px !important;
      height: 150px !important;
      background: rgba(27, 60, 83, 0.05) !important;
      border-radius: 50% !important;
      opacity: 0.8 !important;
    }
    
    .load-data-container:hover {
      border-color: ", waskita_colors$steel, " !important;
      box-shadow: 0 10px 30px rgba(27, 60, 83, 0.1) !important;
      transform: translateY(-2px) !important;
    }
    
    .load-data-icon {
      width: 80px !important;
      height: 80px !important;
      background: linear-gradient(135deg, ", waskita_colors$navy, ", ", waskita_colors$steel, ") !important;
      border-radius: 50% !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      margin: 0 auto 20px auto !important;
      transition: all 0.3s ease !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    .load-data-container:hover .load-data-icon {
      transform: scale(1.1) !important;
      box-shadow: 0 8px 25px rgba(27, 60, 83, 0.3) !important;
    }
    
    .load-data-icon i {
      color: ", waskita_colors$white, " !important;
      font-size: 32px !important;
    }
    
    .load-data-title {
      font-size: 3rem !important;
      font-weight: 700 !important;
      color: ", waskita_colors$navy, " !important;
      margin-bottom: 10px !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    .load-data-description {
      color: ", waskita_colors$text_muted, " !important;
      font-size: 1.5rem !important;
      margin-bottom: 25px !important;
      line-height: 1.5 !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    .load-data-button {
      background: linear-gradient(135deg, ", waskita_colors$navy, " 0%, ", waskita_colors$steel, " 100%) !important;
      color: ", waskita_colors$white, " !important;
      border: none !important;
      padding: 15px 40px !important;
      border-radius: 30px !important;
      font-size: 16px !important;
      font-weight: 600 !important;
      transition: all 0.3s ease !important;
      box-shadow: 0 5px 15px rgba(27, 60, 83, 0.3) !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    .load-data-button:hover {
      transform: translateY(-2px) !important;
      box-shadow: 0 8px 25px rgba(27, 60, 83, 0.4) !important;
      background: linear-gradient(135deg, ", waskita_colors$steel, " 0%, ", waskita_colors$navy, " 100%) !important;
    }
    
    /* Hero Section */
    .hero-section {
      background: linear-gradient(135deg, ", waskita_colors$navy, " 0%, ", waskita_colors$steel, " 100%) !important;
      border-radius: 20px !important;
      padding: 50px 40px !important;
      margin: 20px 0 40px 0 !important;
      text-align: center !important;
      color: ", waskita_colors$white, " !important;
      position: relative !important;
      overflow: hidden !important;
      box-shadow: 0 20px 40px rgba(27, 60, 83, 0.3) !important;
    }
    
    .hero-section::before {
      content: '' !important;
      position: absolute !important;
      top: -50% !important;
      right: -20% !important;
      width: 200px !important;
      height: 200px !important;
      background: rgba(255,255,255,0.1) !important;
      border-radius: 50% !important;
      opacity: 0.8 !important;
    }
    
    .hero-section h1 {
      font-size: 5rem !important;
      font-weight: 800 !important;
      margin: 20px 0 !important;
      letter-spacing: 2px !important;
      text-shadow: 0 4px 15px rgba(0,0,0,0.3) !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    .hero-section h2 {
      font-size: 2.5rem !important;
      font-weight: 400 !important;
      margin-bottom: 25px !important;
      opacity: 0.95 !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    .hero-section p {
      font-size: 1.5rem !important;
      line-height: 1.7 !important;
      max-width: 800px !important;
      margin: 0 auto !important;
      opacity: 0.9 !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    .hero-badge {
      display: inline-flex !important;
      align-items: center !important;
      background: rgba(255,255,255,0.2) !important;
      padding: 8px 20px !important;
      border-radius: 25px !important;
      margin-bottom: 20px !important;
      backdrop-filter: blur(10px) !important;
      font-size: 14px !important;
      font-weight: 600 !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    /* Menu Cards */
    .menu-card {
      background: ", waskita_colors$white, " !important;
      border-radius: 20px !important;
      padding: 35px 25px !important;
      margin-bottom: 25px !important;
      box-shadow: 0 8px 25px rgba(0,0,0,0.08) !important;
      border: 2px solid transparent !important;
      transition: all 0.4s cubic-bezier(0.4, 0.0, 0.2, 1) !important;
      cursor: pointer !important;
      text-align: center !important;
      position: relative !important;
      overflow: hidden !important;
    }
    
    .menu-card::before {
      content: '' !important;
      position: absolute !important;
      top: 0 !important;
      left: 0 !important;
      right: 0 !important;
      bottom: 0 !important;
      background: linear-gradient(135deg, rgba(27, 60, 83, 0.02) 0%, rgba(69, 104, 130, 0.02) 100%) !important;
      opacity: 0 !important;
      transition: opacity 0.4s ease !important;
    }
    
    .menu-card:hover::before {
      opacity: 1 !important;
    }
    
    .menu-card:hover {
      box-shadow: 0 15px 40px rgba(27, 60, 83, 0.15) !important;
      transform: translateY(-8px) scale(1.02) !important;
      border-color: ", waskita_colors$steel, " !important;
    }
    
    .menu-icon {
      width: 70px !important;
      height: 70px !important;
      background: linear-gradient(135deg, ", waskita_colors$navy, ", ", waskita_colors$steel, ") !important;
      border-radius: 18px !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      margin: 0 auto 20px auto !important;
      transition: all 0.4s ease !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    .menu-card:hover .menu-icon {
      transform: scale(1.1) rotate(5deg) !important;
      box-shadow: 0 10px 25px rgba(27, 60, 83, 0.3) !important;
    }
    
    .menu-icon i {
      color: ", waskita_colors$white, " !important;
      font-size: 28px !important;
    }
    
    .menu-card h4 {
      font-size: 2.5rem !important;
      font-weight: 700 !important;
      color: ", waskita_colors$navy, " !important;
      margin-bottom: 12px !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    .menu-card p {
      color: ", waskita_colors$text_muted, " !important;
      font-size: 1.3rem !important;
      line-height: 1.5 !important;
      margin: 0 !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    /* Overview Cards */
    .overview-card {
      background: ", waskita_colors$white, " !important;
      border-radius: 18px !important;
      padding: 30px 25px !important;
      margin-bottom: 25px !important;
      box-shadow: 0 6px 20px rgba(0,0,0,0.06) !important;
      border: 1px solid ", waskita_colors$border, " !important;
      transition: all 0.3s ease !important;
      text-align: center !important;
    }
    
    .overview-card:hover {
      box-shadow: 0 10px 30px rgba(0,0,0,0.1) !important;
      transform: translateY(-3px) !important;
    }
    
    .overview-icon {
      width: 60px !important;
      height: 60px !important;
      border-radius: 15px !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      margin: 0 auto 15px auto !important;
    }
    
    .overview-icon i {
      color: ", waskita_colors$white, " !important;
      font-size: 24px !important;
    }
    
    .overview-card h3 {
      font-size: 3rem !important;
      font-weight: 800 !important;
      margin: 10px 0 !important;
      color: ", waskita_colors$navy, " !important;
    }
    
    .overview-card h5 {
      font-size: 1.7rem !important;
      font-weight: 600 !important;
      color: ", waskita_colors$text_dark, " !important;
      margin-bottom: 8px !important;
      text-transform: uppercase !important;
      letter-spacing: 0.5px !important;
    }
    
    .overview-card p {
      font-size: 1.3rem !important;
      color: ", waskita_colors$text_muted, " !important;
      margin: 0 !important;
    }
    
    /* Report Generation Section */
    .report-generation-section {
      background: linear-gradient(135deg, ", waskita_colors$cream, " 0%, ", waskita_colors$white, " 100%) !important;
      border: 2px solid ", waskita_colors$success, " !important;
      border-radius: 20px !important;
      padding: 40px !important;
      text-align: center !important;
      transition: all 0.3s ease !important;
      position: relative !important;
      overflow: hidden !important;
      margin: 30px 0 !important;
    }
    
    .report-generation-section::before {
      content: '' !important;
      position: absolute !important;
      top: -50% !important;
      right: -30% !important;
      width: 200px !important;
      height: 200px !important;
      background: rgba(16, 185, 129, 0.1) !important;
      border-radius: 50% !important;
      opacity: 0.8 !important;
    }
    
    .report-generation-section:hover {
      border-color: ", waskita_colors$navy, " !important;
      box-shadow: 0 15px 35px rgba(16, 185, 129, 0.2) !important;
      transform: translateY(-3px) !important;
    }
    
    .report-icon {
      width: 100px !important;
      height: 100px !important;
      background: linear-gradient(135deg, ", waskita_colors$success, ", #059669) !important;
      border-radius: 50% !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      margin: 0 auto 25px auto !important;
      transition: all 0.3s ease !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    .report-generation-section:hover .report-icon {
      transform: scale(1.1) !important;
      box-shadow: 0 10px 30px rgba(16, 185, 129, 0.4) !important;
    }
    
    .report-icon i {
      color: ", waskita_colors$white, " !important;
      font-size: 36px !important;
    }
    
    .report-title {
      font-size: 2rem !important;
      font-weight: 700 !important;
      color: ", waskita_colors$navy, " !important;
      margin-bottom: 15px !important;
      position: relative !important;
      z-index: 2 !important;
    }
    
    .report-description {
      color: ", waskita_colors$text_muted, " !important;
      font-size: 1.5rem !important;
      margin-bottom: 30px !important;
      line-height: 1.6 !important;
      position: relative !important;
      z-index: 2 !important;
      max-width: 600px !important;
      margin-left: auto !important;
      margin-right: auto !important;
    }
    
    .report-button {
      background: linear-gradient(135deg, ", waskita_colors$success, " 0%, #059669 100%) !important;
      color: ", waskita_colors$white, " !important;
      border: none !important;
      padding: 18px 45px !important;
      border-radius: 35px !important;
      font-size: 16px !important;
      font-weight: 600 !important;
      transition: all 0.3s ease !important;
      box-shadow: 0 6px 20px rgba(16, 185, 129, 0.3) !important;
      position: relative !important;
      z-index: 2 !important;
      margin: 0 10px !important;
    }
    
    .report-button:hover {
      transform: translateY(-2px) !important;
      box-shadow: 0 10px 30px rgba(16, 185, 129, 0.4) !important;
      background: linear-gradient(135deg, #059669 0%, ", waskita_colors$success, " 100%) !important;
    }
    
    .analysis-status-grid {
      display: grid !important;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)) !important;
      gap: 20px !important;
      margin: 30px 0 !important;
    }
    
    .status-card {
      background: ", waskita_colors$white, " !important;
      border-radius: 12px !important;
      padding: 20px !important;
      border-left: 4px solid ", waskita_colors$border, " !important;
      box-shadow: 0 4px 15px rgba(0,0,0,0.05) !important;
      transition: all 0.3s ease !important;
    }
    
    .status-card.completed {
      border-left-color: ", waskita_colors$success, " !important;
      background: linear-gradient(135deg, rgba(16, 185, 129, 0.05) 0%, ", waskita_colors$white, " 100%) !important;
    }
    
    .status-card:hover {
      box-shadow: 0 8px 25px rgba(0,0,0,0.1) !important;
      transform: translateY(-2px) !important;
    }
    
    .status-icon {
      width: 40px !important;
      height: 40px !important;
      border-radius: 8px !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      margin-bottom: 15px !important;
      background: ", waskita_colors$border, " !important;
    }
    
    .status-card.completed .status-icon {
      background: ", waskita_colors$success, " !important;
    }
    
    .status-icon i {
      color: ", waskita_colors$white, " !important;
      font-size: 18px !important;
    }
    
    .status-title {
      font-size: 1.5rem !important;
      font-weight: 600 !important;
      color: ", waskita_colors$navy, " !important;
      margin-bottom: 5px !important;
    }
    
    .status-description {
      font-size: 1.2rem !important;
      color: ", waskita_colors$text_muted, " !important;
    }
    
    .status-badge {
      display: inline-block !important;
      padding: 4px 12px !important;
      border-radius: 20px !important;
      font-size: 1rem !important;
      font-weight: 600 !important;
      margin-top: 10px !important;
    }
    
    .badge-completed {
      background: rgba(16, 185, 129, 0.1) !important;
      color: ", waskita_colors$success, " !important;
    }
    
    .badge-pending {
      background: rgba(245, 158, 11, 0.1) !important;
      color: ", waskita_colors$warning, " !important;
    }
    
    /* Footer */
    .footer-section {
      background: ", waskita_colors$navy, " !important;
      color: ", waskita_colors$white, " !important;
      padding: 40px 30px !important;
      margin: 40px -25px -25px -25px !important;
      border-radius: 0 0 15px 15px !important;
    }
    
    .footer-grid {
      display: grid !important;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)) !important;
      gap: 30px !important;
      margin-bottom: 20px !important;
    }
    
    .footer-column h5 {
      font-size: 2rem !important;
      font-weight: 600 !important;
      margin-bottom: 15px !important;
      display: flex !important;
      align-items: center !important;
    }
    
    .footer-column h5 i {
      margin-right: 8px !important;
      color: ", waskita_colors$warm_gray, " !important;
    }
    
    .footer-column p {
      font-size: 0.9rem !important!
      margin-bottom: 5px !important;
      opacity: 0.9 !important;
    }
    
    .footer-column a {
      color: ", waskita_colors$warm_gray, " !important;
      text-decoration: none !important;
      font-weight: 500 !important;
    }
    
    .footer-column a:hover {
      color: ", waskita_colors$white, " !important;
    }
    
    .footer-bottom {
      text-align: center !important;
      padding-top: 20px !important;
      border-top: 1px solid rgba(255,255,255,0.2) !important;
      font-size: 1.5rem !important;
      opacity: 0.8 !important;
    }
    
    /* Modern Buttons */
    .btn {
      border-radius: 10px !important;
      padding: 12px 25px !important;
      font-weight: 500 !important;
      font-size: 14px !important;
      transition: all 0.3s ease !important;
      border: none !important;
      letter-spacing: 0.5px !important;
    }
    
    .btn-primary {
      background: linear-gradient(135deg, ", waskita_colors$navy, " 0%, ", waskita_colors$steel, " 100%) !important;
      color: ", waskita_colors$white, " !important;
      box-shadow: 0 4px 15px rgba(27, 60, 83, 0.3) !important;
    }
    
    .btn-primary:hover {
      box-shadow: 0 6px 20px rgba(27, 60, 83, 0.4) !important;
      transform: translateY(-2px) !important;
    }
    
    .btn-success {
      background: linear-gradient(135deg, ", waskita_colors$success, " 0%, #059669 100%) !important;
      color: ", waskita_colors$white, " !important;
      box-shadow: 0 4px 15px rgba(16, 185, 129, 0.3) !important;
    }
    
    .btn-success:hover {
      box-shadow: 0 6px 20px rgba(16, 185, 129, 0.4) !important;
      transform: translateY(-2px) !important;
    }
    
    .btn-warning {
      background: linear-gradient(135deg, ", waskita_colors$warning, " 0%, #d97706 100%) !important;
      color: ", waskita_colors$white, " !important;
      box-shadow: 0 4px 15px rgba(245, 158, 11, 0.3) !important;
    }
    
    .btn-warning:hover {
      box-shadow: 0 6px 20px rgba(245, 158, 11, 0.4) !important;
      transform: translateY(-2px) !important;
    }
    
    /* Form Controls */
    .form-control, .selectize-input {
      border-radius: 10px !important;
      border: 2px solid ", waskita_colors$border, " !important;
      padding: 12px 15px !important;
      font-size: 14px !important;
      transition: all 0.3s ease !important;
      background: ", waskita_colors$white, " !important;
    }
    
    .form-control:focus, .selectize-input.focus {
     border-color: ", waskita_colors$steel, " !important;
     box-shadow: 0 0 0 3px rgba(69, 104, 130, 0.1) !important;
     outline: none !important;
   }
   
   /* Tables */
   .table {
     background: ", waskita_colors$white, " !important;
     border-radius: 10px !important;
     overflow: hidden !important;
     box-shadow: 0 4px 15px rgba(0,0,0,0.05) !important;
   }
   
   .table th {
     background: linear-gradient(135deg, ", waskita_colors$navy, " 0%, ", waskita_colors$steel, " 100%) !important;
     color: ", waskita_colors$white, " !important;
     font-weight: 600 !important;
     padding: 15px 12px !important;
     border: none !important;
     font-size: 13px !important;
   }
   
   .table td {
     padding: 12px !important;
     border-bottom: 1px solid ", waskita_colors$border, " !important;
     font-size: 13px !important;
   }
   
   .table tbody tr:hover {
     background: ", waskita_colors$cream, " !important;
   }
   
   /* Interpretation Box */
   .interpretation-box {
     background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%) !important;
     border: none !important;
     border-left: 4px solid ", waskita_colors$steel, " !important;
     padding: 20px !important;
     margin-top: 20px !important;
     border-radius: 10px !important;
     box-shadow: 0 2px 10px rgba(69, 104, 130, 0.1) !important;
   }
   
   .interpretation-box h5 {
     color: ", waskita_colors$navy, " !important;
     font-weight: 600 !important;
     margin-bottom: 10px !important;
     font-size: 16px !important;
   }
   
   /* Statistics Cards */
   .stat-card {
     background: ", waskita_colors$white, " !important;
     border-radius: 12px !important;
     padding: 20px !important;
     margin-bottom: 20px !important;
     box-shadow: 0 4px 15px rgba(0,0,0,0.05) !important;
     border: 1px solid ", waskita_colors$border, " !important;
     transition: all 0.3s ease !important;
   }
   
   .stat-card:hover {
     box-shadow: 0 8px 25px rgba(0,0,0,0.1) !important;
     transform: translateY(-2px) !important;
   }
   
   .stat-card h5 {
     color: ", waskita_colors$navy, " !important;
     font-weight: 600 !important;
     font-size: 16px !important;
     margin-bottom: 15px !important;
   }
   
   /* Download Section */
   .download-section {
     background: linear-gradient(135deg, ", waskita_colors$cream, " 0%, ", waskita_colors$white, " 100%) !important;
     padding: 20px !important;
     border-radius: 12px !important;
     margin-top: 25px !important;
     border: 1px solid ", waskita_colors$border, " !important;
   }
   
   .download-section h5 {
     color: ", waskita_colors$navy, " !important;
     font-weight: 600 !important;
     margin-bottom: 15px !important;
     font-size: 16px !important;
   }
   
   /* Loading Spinner */
   .shiny-spinner-output-container {
     background: rgba(255, 255, 255, 0.9) !important;
     border-radius: 10px !important;
   }
   
   /* Empty State */
   .empty-state {
     text-align: center !important;
     padding: 60px 20px !important;
     color: ", waskita_colors$text_muted, " !important;
   }
   
   .empty-state h4 {
     color: ", waskita_colors$text_muted, " !important;
     font-weight: 400 !important;
     font-size: 18px !important;
   }
   
   /* Responsive Design */
   @media (max-width: 768px) {
     .hero-section h1 { font-size: 2.5rem !important; }
     .hero-section h2 { font-size: 1.4rem !important; }
     .hero-section p { font-size: 1rem !important; }
     .menu-card { padding: 25px 20px !important; }
     .overview-card { padding: 20px 15px !important; }
     .footer-grid { grid-template-columns: 1fr !important; }
     .analysis-status-grid { grid-template-columns: 1fr !important; }
   }
 ")))
)

# =============================================================================
# DASHBOARD UI
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = tags$span(
      tags$i(class = "fas fa-globe-asia", style = "margin-right: 10px;"),
      "WASKITA",
      style = "font-weight: bold; font-size: 20px; letter-spacing: 1px;"
    ),
    titleWidth = 280
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 280,
    custom_css,
    
    sidebarMenu(
      id = "sidebar_menu",
      
      menuItem("Beranda", 
               tabName = "beranda", 
               icon = icon("home")),
      
      menuItem("Manajemen Data", 
               tabName = "manajemen_data", 
               icon = icon("database")),
      
      menuItem("Eksplorasi Data", 
               tabName = "eksplorasi_data", 
               icon = icon("chart-bar"),
               menuSubItem("Analisis Deskriptif", tabName = "deskriptif"),
               menuSubItem("Visualisasi", tabName = "visualisasi"),
               menuSubItem("Peta Spasial", tabName = "peta_spasial")),
      
      menuItem("Uji Asumsi Data", 
               tabName = "uji_asumsi", 
               icon = icon("check-circle")),
      
      menuItem("Statistik Inferensia", 
               icon = icon("calculator"),
               menuSubItem("Uji Beda Rata-rata", tabName = "uji_rata_rata"),
               menuSubItem("Uji Proporsi & Variance", tabName = "uji_proporsi"),
               menuSubItem("ANOVA", tabName = "anova")),
      
      menuItem("Regresi Linear", 
               tabName = "regresi", 
               icon = icon("chart-line")),
      
      menuItem("Analisis Spasial", 
               tabName = "analisis_spasial", 
               icon = icon("map-marked-alt")),
      
      # NEW MENU FOR COMPREHENSIVE REPORT
      menuItem("Laporan Lengkap", 
               tabName = "laporan_lengkap", 
               icon = icon("file-pdf"))
    )
  ),
  
  # Body
  dashboardBody(
    custom_css,
    
    tabItems(
      # =============================================================================
      # TAB BERANDA
      # =============================================================================
      tabItem(
        tabName = "beranda",
        
        # Hero Section
        fluidRow(
          column(12,
                 div(class = "hero-section",
                     div(class = "hero-badge",
                         tags$i(class = "fas fa-graduation-cap", style = "margin-right: 8px;"),
                         "UAS Komputasi Statistik 2025"
                     ),
                     h1("WASKITA"),
                     h2("Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik"),
                     p("Platform analisis modern untuk penelitian kerentanan sosial dengan tools statistik komprehensif, visualisasi interaktif, dan analisis spasial yang terintegrasi menggunakan data Social Vulnerability Index dari penelitian ilmiah.")
                 )
          )
        ),
        
        # Menu Analisis
        div(style = "text-align: center; margin: 40px 0 30px 0;",
            h2(style = paste0("color: ", waskita_colors$navy, "; font-weight: 700; font-size: 3rem;"),
               tags$i(class = "fas fa-rocket", style = paste0("margin-right: 15px; color: ", waskita_colors$steel, ";")),
               "Menu Analisis"),
            p(style = paste0("color: ", waskita_colors$text_muted, "; font-size: 1.5rem; max-width: 600px; margin: 0 auto;"),
              "Pilihan menu analisis yang bisa Anda gunakan untuk memulai eksplorasi data kerentanan sosial berdasarkan penelitian ilmiah")
        ),
        
        # Menu Cards Grid
        fluidRow(
          column(4,
                 div(class = "menu-card", onclick = "Shiny.setInputValue('sidebar_menu', 'manajemen_data', {priority: 'event'});",
                     div(class = "menu-icon",
                         tags$i(class = "fas fa-database")
                     ),
                     h4("Kelola Data"),
                     p("Preprocessing dan transformasi dataset")
                 )
          ),
          column(4,
                 div(class = "menu-card", onclick = "Shiny.setInputValue('sidebar_menu', 'deskriptif', {priority: 'event'});",
                     div(class = "menu-icon",
                         tags$i(class = "fas fa-chart-bar")
                     ),
                     h4("Eksplorasi"),
                     p("Analisis deskriptif dan visualisasi data")
                 )
          ),
          column(4,
                 div(class = "menu-card", onclick = "Shiny.setInputValue('sidebar_menu', 'uji_asumsi', {priority: 'event'});",
                     div(class = "menu-icon",
                         tags$i(class = "fas fa-check-circle")
                     ),
                     h4("Uji Asumsi"),
                     p("Validasi normalitas dan homogenitas")
                 )
          )
        ),
        
        fluidRow(
          column(4,
                 div(class = "menu-card", onclick = "Shiny.setInputValue('sidebar_menu', 'uji_rata_rata', {priority: 'event'});",
                     div(class = "menu-icon",
                         tags$i(class = "fas fa-calculator")
                     ),
                     h4("Inferensia"),
                     p("Uji hipotesis dan estimasi")
                 )
          ),
          column(4,
                 div(class = "menu-card", onclick = "Shiny.setInputValue('sidebar_menu', 'regresi', {priority: 'event'});",
                     div(class = "menu-icon",
                         tags$i(class = "fas fa-chart-line")
                     ),
                     h4("Regresi"),
                     p("Analisis hubungan antar variabel")
                 )
          ),
          column(4,
                 div(class = "menu-card", onclick = "Shiny.setInputValue('sidebar_menu', 'analisis_spasial', {priority: 'event'});",
                     div(class = "menu-icon",
                         tags$i(class = "fas fa-globe")
                     ),
                     h4("Spasial"),
                     p("Analisis geografis dan autokorelasi")
                 )
          )
        ),
        
        # Overview Dataset
        div(style = "text-align: center; margin: 50px 0 30px 0;",
            h2(style = paste0("color: ", waskita_colors$navy, "; font-weight: 700; font-size: 3rem;"),
               tags$i(class = "fas fa-chart-line", style = paste0("margin-right: 15px; color: ", waskita_colors$steel, ";")),
               "Overview Dataset"),
            p(style = paste0("color: ", waskita_colors$text_muted, "; font-size: 1.5rem; max-width: 700px; margin: 0 auto;"),
              "Ringkasan komprehensif dari dataset Social Vulnerability Index yang digunakan untuk analisis berdasarkan penelitian ilmiah")
        ),
        
        # Dataset Statistics
        fluidRow(
          column(4,
                 div(class = "overview-card",
                     div(class = "overview-icon", 
                         style = paste0("background: linear-gradient(135deg, ", waskita_colors$navy, ", ", waskita_colors$steel, ");"),
                         tags$i(class = "fas fa-table")
                     ),
                     h3("511"),
                     h5("Total Observasi"),
                     p("Kabupaten/Kota dalam dataset")
                 )
          ),
          column(4,
                 div(class = "overview-card",
                     div(class = "overview-icon", 
                         style = "background: linear-gradient(135deg, #10B981, #047857);",
                         tags$i(class = "fas fa-list-ul")
                     ),
                     h3("17"),
                     h5("Indikator SoVI"),
                     p("Variabel kerentanan sosial")
                 )
          ),
          column(4,
                 div(class = "overview-card",
                     div(class = "overview-icon", 
                         style = paste0("background: linear-gradient(135deg, ", waskita_colors$warning, ", #d97706);"),
                         tags$i(class = "fas fa-map-marked-alt")
                     ),
                     h3("34"),
                     h5("Provinsi Indonesia"),
                     p("Cakupan wilayah dalam data")
                 )
          )
        ),
        
        # Metadata Section
        div(style = "text-align: center; margin: 50px 0 30px 0;",
            h2(style = paste0("color: ", waskita_colors$navy, "; font-weight: 700; font-size: 3rem;"),
               tags$i(class = "fas fa-info-circle", style = paste0("margin-right: 15px; color: ", waskita_colors$steel, ";")),
               "Metadata Ilmiah"),
            p(style = paste0("color: ", waskita_colors$text_muted, "; font-size: 1.5rem; max-width: 700px; margin: 0 auto;"),
              "Informasi lengkap mengenai dataset Social Vulnerability Index berdasarkan publikasi ilmiah")
        ),
        
        # Metadata Table
        fluidRow(
          column(12,
                 box(
                   title = tagList(icon("table"), "Metadata Variabel"),
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   
                   DT::dataTableOutput("metadata_table"),
                   
                   # Footer Information
                   div(class = "footer-section",
                       div(class = "footer-grid",
                           div(class = "footer-column",
                               h5(tags$i(class = "fas fa-university"), "Informasi Akademis"),
                               p("Politeknik Statistika STIS"),
                               p("Program Studi Komputasi Statistik D-IV"),
                               p("Mata Kuliah: Komputasi Statistik"),
                               p("Semester Genap TA. 2024/2025")
                           ),
                           div(class = "footer-column",
                               h5(tags$i(class = "fas fa-calendar-alt"), "Jadwal UAS"),
                               p("Tanggal: 23 Juli 2025"),
                               p("Waktu: 10:30 - 12:30 WIB"),
                               p("Durasi: 120 menit"),
                               p("Sifat: Tidak Terstruktur")
                           ),
                           div(class = "footer-column",
                               h5(tags$i(class = "fas fa-database"), "Data Source"),
                               p("Dataset: Social Vulnerability Index"),
                               p("Format: CSV Files"),
                               p("Local: D:/Perkuliahan/.../data/"),
                               p("Backup: GitHub Repository")
                           ),
                           div(class = "footer-column",
                               h5(tags$i(class = "fas fa-book-open"), "Referensi Ilmiah"),
                               p("Journal: Data in Brief"),
                               p("Publisher: Elsevier"),
                               p("DOI: 10.1016/j.dib.2021.107618"),
                               tags$a("View Publication", 
                                      href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180",
                                      target = "_blank")
                           )
                       ),
                       div(class = "footer-bottom",
                           p("© 2025 Dashboard WASKITA - Final Project using SoVI Data with Scientific Metadata")
                       )
                   )
                 )
          )
        ),
        
        # Global Download Section
        conditionalPanel(
          condition = "output.data_loaded == true",
          fluidRow(
            column(12,
                   div(class = "download-section",
                       h5("Download Laporan Lengkap"),
                       fluidRow(
                         column(6,
                                downloadButton("download_full_report_pdf", "Download Full Report (PDF)",
                                               class = "btn btn-primary", style = "width: 100%;")
                         ),
                         column(6,
                                downloadButton("download_full_report_word", "Download Full Report (Word)",
                                               class = "btn btn-primary", style = "width: 100%;")
                         )
                       )
                   )
            )
          )
        )
      ),
      
      # =============================================================================
      # TAB LAPORAN LENGKAP (NEW)
      # =============================================================================
      tabItem(
        tabName = "laporan_lengkap",
        
        box(
          title = tagList(icon("file-pdf"), "Generator Laporan Lengkap WASKITA"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          # Hero Section for Report Generation
          fluidRow(
            column(12,
                   div(class = "report-generation-section",
                       div(class = "report-icon",
                           tags$i(class = "fas fa-file-medical-alt")
                       ),
                       h3(class = "report-title", "Laporan Analisis Statistik Komprehensif"),
                       p(class = "report-description", 
                         "Generate laporan PDF lengkap yang berisi semua hasil analisis statistik, visualisasi, dan interpretasi ahli dari semua fitur WASKITA Dashboard. Laporan akan disusun secara profesional dengan format akademis yang sesuai untuk penelitian dan publikasi ilmiah."),
                       
                       conditionalPanel(
                         condition = "output.data_loaded == true",
                         div(style = "margin: 20px 0;",
                             downloadButton("generate_comprehensive_report_pdf", 
                                            HTML("<i class='fas fa-file-pdf'></i> Generate Laporan PDF Lengkap"), 
                                            class = "report-button"),
                             downloadButton("generate_comprehensive_report_word", 
                                            HTML("<i class='fas fa-file-word'></i> Generate Laporan Word Lengkap"), 
                                            class = "report-button")
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "output.data_loaded == false",
                         div(style = "background: rgba(245, 158, 11, 0.1); color: #d97706; padding: 15px; border-radius: 10px; margin: 20px 0;",
                             tags$i(class = "fas fa-exclamation-triangle", style = "margin-right: 10px;"),
                             "Silakan load data terlebih dahulu di menu Manajemen Data untuk menggenerate laporan lengkap."
                         )
                       )
                   )
            )
          ),
          
          # Analysis Status Overview
          conditionalPanel(
            condition = "output.data_loaded == true",
            fluidRow(
              column(12,
                     h3("Status Analisis yang Tersedia", 
                        style = paste0("color: ", waskita_colors$navy, "; text-align: center; margin: 40px 0 20px 0;")),
                     p("Berikut adalah ringkasan analisis yang telah dilakukan dan akan disertakan dalam laporan:",
                       style = paste0("color: ", waskita_colors$text_muted, "; text-align: center; margin-bottom: 30px;"))
              )
            ),
            
            # Analysis Status Grid
            fluidRow(
              column(12,
                     div(class = "analysis-status-grid",
                         div(class = "status-card",
                             div(class = "status-icon", 
                                 id = "status-data",
                                 tags$i(class = "fas fa-database")
                             ),
                             div(class = "status-title", "Manajemen Data"),
                             div(class = "status-description", "Loading dan preprocessing dataset SoVI"),
                             div(class = "status-badge badge-pending", id = "badge-data", "Pending")
                         ),
                         div(class = "status-card",
                             div(class = "status-icon", 
                                 id = "status-descriptive",
                                 tags$i(class = "fas fa-chart-bar")
                             ),
                             div(class = "status-title", "Analisis Deskriptif"),
                             div(class = "status-description", "Statistik deskriptif dan visualisasi dasar"),
                             div(class = "status-badge badge-pending", id = "badge-descriptive", "Pending")
                         ),
                         div(class = "status-card",
                             div(class = "status-icon", 
                                 id = "status-visualization",
                                 tags$i(class = "fas fa-chart-area")
                             ),
                             div(class = "status-title", "Visualisasi Multivariat"),
                             div(class = "status-description", "Scatter plots, heatmaps, dan analisis korelasi"),
                             div(class = "status-badge badge-pending", id = "badge-visualization", "Pending")
                         ),
                         div(class = "status-card",
                             div(class = "status-icon", 
                                 id = "status-spatial-map",
                                 tags$i(class = "fas fa-map")
                             ),
                             div(class = "status-title", "Peta Distribusi Spasial"),
                             div(class = "status-description", "Visualisasi geografis dan pola spasial"),
                             div(class = "status-badge badge-pending", id = "badge-spatial-map", "Pending")
                         ),
                         div(class = "status-card",
                             div(class = "status-icon", 
                                 id = "status-assumptions",
                                 tags$i(class = "fas fa-check-circle")
                             ),
                             div(class = "status-title", "Uji Asumsi"),
                             div(class = "status-description", "Normalitas dan homogenitas data"),
                             div(class = "status-badge badge-pending", id = "badge-assumptions", "Pending")
                         ),
                         div(class = "status-card",
                             div(class = "status-icon", 
                                 id = "status-ttest",
                                 tags$i(class = "fas fa-calculator")
                             ),
                             div(class = "status-title", "Uji Beda Rata-rata"),
                             div(class = "status-description", "T-test dan analisis inferensia"),
                             div(class = "status-badge badge-pending", id = "badge-ttest", "Pending")
                         ),
                         div(class = "status-card",
                             div(class = "status-icon", 
                                 id = "status-proportion",
                                 tags$i(class = "fas fa-pie-chart")
                             ),
                             div(class = "status-title", "Uji Proporsi & Variance"),
                             div(class = "status-description", "Analisis proporsi dan variabilitas"),
                             div(class = "status-badge badge-pending", id = "badge-proportion", "Pending")
                         ),
                         div(class = "status-card",
                             div(class = "status-icon", 
                                 id = "status-anova",
                                 tags$i(class = "fas fa-chart-area")
                             ),
                             div(class = "status-title", "ANOVA"),
                             div(class = "status-description", "Analysis of Variance antar grup"),
                             div(class = "status-badge badge-pending", id = "badge-anova", "Pending")
                         ),
                         div(class = "status-card",
                             div(class = "status-icon", 
                                 id = "status-regression",
                                 tags$i(class = "fas fa-chart-line")
                             ),
                             div(class = "status-title", "Regresi Linear"),
                             div(class = "status-description", "Analisis hubungan dan prediksi"),
                             div(class = "status-badge badge-pending", id = "badge-regression", "Pending")
                         ),
                         div(class = "status-card",
                             div(class = "status-icon", 
                                 id = "status-spatial",
                                 tags$i(class = "fas fa-map-marked-alt")
                             ),
                             div(class = "status-title", "Analisis Spasial"),
                             div(class = "status-description", "Autokorelasi dan pola geografis"),
                             div(class = "status-badge badge-pending", id = "badge-spatial", "Pending")
                         )
                     )
              )
            ),
            
            # Report Features Information
            fluidRow(
              column(12,
                     div(class = "interpretation-box", style = "margin: 40px 0;",
                         h5(tags$i(class = "fas fa-info-circle"), " Fitur Laporan Lengkap"),
                         HTML("
                         <ul style='margin: 15px 0; padding-left: 20px;'>
                           <li><strong>Executive Summary:</strong> Ringkasan eksekutif hasil analisis</li>
                           <li><strong>Metodologi Lengkap:</strong> Penjelasan metode statistik yang digunakan</li>
                           <li><strong>Hasil Analisis Detail:</strong> Output statistik dengan interpretasi ahli</li>
                           <li><strong>Visualisasi Berkualitas Tinggi:</strong> Grafik dan chart dengan resolusi publikasi</li>
                           <li><strong>Interpretasi Statistik:</strong> Penjelasan teknis dan praktis setiap hasil</li>
                           <li><strong>Kesimpulan dan Rekomendasi:</strong> Implikasi temuan untuk kebijakan</li>
                           <li><strong>Referensi dan Metadata:</strong> Informasi sumber data dan metodologi</li>
                           <li><strong>Appendix:</strong> Data tambahan dan output statistik lengkap</li>
                         </ul>
                         ")
                     )
              )
            ),
            
            # Report Template Options
            fluidRow(
              column(12,
                     div(class = "stat-card",
                         h5(tags$i(class = "fas fa-cogs"), " Pengaturan Laporan"),
                         
                         fluidRow(
                           column(4,
                                  selectInput("report_language", "Bahasa Laporan:",
                                              choices = list(
                                                "Bahasa Indonesia" = "id",
                                                "English" = "en"
                                              ),
                                              selected = "id",
                                              width = "100%")
                           ),
                           column(4,
                                  selectInput("report_detail_level", "Level Detail:",
                                              choices = list(
                                                "Executive Summary" = "summary",
                                                "Standard Report" = "standard", 
                                                "Comprehensive Report" = "comprehensive",
                                                "Technical Report" = "technical"
                                              ),
                                              selected = "comprehensive",
                                              width = "100%")
                           ),
                           column(4,
                                  selectInput("report_format_style", "Style Template:",
                                              choices = list(
                                                "Academic Paper" = "academic",
                                                "Business Report" = "business",
                                                "Technical Documentation" = "technical"
                                              ),
                                              selected = "academic",
                                              width = "100%")
                           )
                         ),
                         
                         fluidRow(
                           column(6,
                                  checkboxInput("include_raw_output", "Sertakan Output Mentah R", value = FALSE)
                           ),
                           column(6,
                                  checkboxInput("include_code_snippets", "Sertakan Code Snippets", value = FALSE)
                           )
                         )
                     )
              )
            )
          )
        )
      ),
      
      # =============================================================================
      # TAB MANAJEMEN DATA
      # =============================================================================
      tabItem(
        tabName = "manajemen_data",
        
        box(
          title = tagList(icon("database"), "Manajemen Data"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          fluidRow(
            # Enhanced Load Data Section
            column(12,
                   div(class = "load-data-container",
                       div(class = "load-data-icon",
                           tags$i(class = "fas fa-cloud-download-alt")
                       ),
                       h3(class = "load-data-title", "Load Dataset WASKITA"),
                       p(class = "load-data-description", 
                         "Memuat dataset Social Vulnerability Index (SoVI) dan matriks jarak untuk analisis spasial. Data akan dimuat dari sumber lokal atau backup online."),
                       br(),
                       actionButton("load_data", 
                                    HTML("<i class='fas fa-play'></i> Mulai Load Dataset"), 
                                    class = "load-data-button")
                   )
            ),
            
            # Data Summary (shows after loading)
            conditionalPanel(
              condition = "output.data_loaded == true",
              column(12,
                     br(), br(),
                     div(class = "stat-card",
                         h5(tags$i(class = "fas fa-info-circle"), " Informasi Dataset"),
                         verbatimTextOutput("data_summary")
                     )
              )
            )
          ),
          
          # Transformation and Categorization (shows after data is loaded)
          conditionalPanel(
            condition = "output.data_loaded == true",
            hr(),
            fluidRow(
              column(6,
                     h4("Transformasi Data", style = "margin-bottom: 20px;"),
                     
                     selectInput("transform_variable", "Pilih Variabel:",
                                 choices = NULL, width = "100%"),
                     
                     selectInput("transform_method", "Metode Transformasi:",
                                 choices = list(
                                   "Log Natural" = "log",
                                   "Log10" = "log10",
                                   "Akar Kuadrat" = "sqrt",
                                   "Kuadrat" = "square",
                                   "Box-Cox" = "boxcox"
                                 ), width = "100%"),
                     
                     actionButton("apply_transformation", "Terapkan Transformasi",
                                  class = "btn btn-success",
                                  style = "width: 100%; margin-bottom: 20px;"),
                     
                     conditionalPanel(
                       condition = "output.transformation_done == true",
                       div(class = "interpretation-box",
                           h5("Hasil Transformasi"),
                           verbatimTextOutput("transformation_result")
                       )
                     )
              ),
              
              column(6,
                     h4("Kategorisasi Data", style = "margin-bottom: 20px;"),
                     
                     selectInput("categorize_variable", "Pilih Variabel:",
                                 choices = NULL, width = "100%"),
                     
                     selectInput("categorize_method", "Metode:",
                                 choices = list(
                                   "Quartile" = "quartile",
                                   "Tertile" = "tertile",
                                   "Custom" = "custom"
                                 ), width = "100%"),
                     
                     conditionalPanel(
                       condition = "input.categorize_method == 'custom'",
                       textInput("custom_thresholds", "Threshold (pisah koma):",
                                 value = "25,50,75", width = "100%")
                     ),
                     
                     actionButton("apply_categorization", "Terapkan Kategorisasi",
                                  class = "btn btn-success",
                                  style = "width: 100%; margin-bottom: 20px;"),
                     
                     conditionalPanel(
                       condition = "output.categorization_done == true",
                       div(class = "interpretation-box",
                           h5("Hasil Kategorisasi"),
                           verbatimTextOutput("categorization_result")
                       )
                     )
              )
            ),
            
            hr(),
            h4("Preview Data"),
            withSpinner(DT::dataTableOutput("data_preview"), color = waskita_colors$steel),
            
            div(class = "download-section",
                h5("Download Data"),
                fluidRow(
                  column(6,
                         downloadButton("download_original", "Download Original (CSV)",
                                        class = "btn btn-success", style = "width: 100%;")
                  ),
                  column(6,
                         downloadButton("download_processed", "Download Processed (CSV)",
                                        class = "btn btn-success", style = "width: 100%;")
                  )
                )
            )
          )
        )
      ),
      
      # =============================================================================
      # TAB EKSPLORASI DATA - DESKRIPTIF
      # =============================================================================
      tabItem(
        tabName = "deskriptif",
        
        box(
          title = tagList(icon("chart-bar"), "Analisis Deskriptif"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          conditionalPanel(
            condition = "output.data_loaded == true",
            
            fluidRow(
              column(4,
                     selectInput("desc_variable", "Pilih Variabel:",
                                 choices = NULL, width = "100%"),
                     
                     conditionalPanel(
                       condition = "output.is_numeric_var == true",
                       selectInput("desc_chart_type", "Tipe Visualisasi:",
                                   choices = list(
                                     "Histogram" = "histogram",
                                     "Boxplot" = "boxplot", 
                                     "Density Plot" = "density",
                                     "Q-Q Plot" = "qq"
                                   ), width = "100%"),
                       
                       conditionalPanel(
                         condition = "input.desc_chart_type == 'histogram'",
                         numericInput("desc_bins", "Jumlah Bins:",
                                      value = 30, min = 10, max = 100, width = "100%")
                       )
                     ),
                     
                     conditionalPanel(
                       condition = "output.is_numeric_var == false",
                       selectInput("desc_chart_type", "Tipe Visualisasi:",
                                   choices = list("Bar Chart" = "barchart"), 
                                   width = "100%")
                     ),
                     
                     actionButton("generate_desc", "Generate Analisis",
                                  class = "btn btn-primary", style = "width: 100%;")
              ),
              
              column(8,
                     conditionalPanel(
                       condition = "output.desc_generated == true",
                       withSpinner(plotOutput("desc_plot", height = "400px"), 
                                   color = waskita_colors$steel)
                     )
              )
            ),
            
            conditionalPanel(
              condition = "output.desc_generated == true",
              hr(),
              fluidRow(
                column(6,
                       div(class = "stat-card",
                           h5("Statistik Deskriptif"),
                           verbatimTextOutput("desc_stats")
                       )
                ),
                column(6,
                       div(class = "interpretation-box",
                           h5("Interpretasi"),
                           verbatimTextOutput("desc_interpretation")
                       )
                )
              ),
              
              div(class = "download-section",
                  h5("Download Hasil"),
                  fluidRow(
                    column(4,
                           downloadButton("download_desc_plot", "Download Plot (PNG)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_desc_stats", "Download Stats (CSV)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_desc_report", "Download Report (PDF)",
                                          class = "btn btn-warning", style = "width: 100%;")
                    )
                  )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.data_loaded == false",
            div(class = "empty-state",
                tags$i(class = "fas fa-database fa-3x"),
                h4("Silakan load data terlebih dahulu di menu Manajemen Data")
            )
          )
        )
      ),
      
      # =============================================================================
      # TAB EKSPLORASI DATA - VISUALISASI
      # =============================================================================
      tabItem(
        tabName = "visualisasi",
        
        box(
          title = tagList(icon("chart-area"), "Visualisasi Multivariat"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          conditionalPanel(
            condition = "output.data_loaded == true",
            
            fluidRow(
              column(4,
                     selectInput("viz_type", "Jenis Visualisasi:",
                                 choices = list(
                                   "Scatter Plot" = "scatter",
                                   "Correlation Heatmap" = "correlation",
                                   "Box Plot by Group" = "boxplot_group",
                                   "Histogram Overlay" = "histogram_overlay"
                                 ), width = "100%"),
                     
                     conditionalPanel(
                       condition = "input.viz_type == 'scatter'",
                       selectInput("scatter_x", "Variabel X:", choices = NULL, width = "100%"),
                       selectInput("scatter_y", "Variabel Y:", choices = NULL, width = "100%"),
                       selectInput("scatter_color", "Color by (Optional):", 
                                   choices = NULL, width = "100%")
                     ),
                     
                     conditionalPanel(
                       condition = "input.viz_type == 'boxplot_group'",
                       selectInput("boxplot_numeric", "Variabel Numerik:", 
                                   choices = NULL, width = "100%"),
                       selectInput("boxplot_group", "Variabel Grup:", 
                                   choices = NULL, width = "100%")
                     ),
                     
                     conditionalPanel(
                       condition = "input.viz_type == 'histogram_overlay'",
                       selectInput("hist_variables", "Pilih Variabel (Max 4):",
                                   choices = NULL, multiple = TRUE, width = "100%")
                     ),
                     
                     actionButton("generate_viz", "Generate Visualisasi",
                                  class = "btn btn-primary", style = "width: 100%;")
              ),
              
              column(8,
                     conditionalPanel(
                       condition = "output.viz_generated == true",
                       withSpinner(plotlyOutput("viz_plot", height = "500px"), 
                                   color = waskita_colors$steel)
                     )
              )
            ),
            
            conditionalPanel(
              condition = "output.viz_generated == true",
              hr(),
              div(class = "interpretation-box",
                  h5("Interpretasi Visualisasi"),
                  verbatimTextOutput("viz_interpretation")
              ),
              
              div(class = "download-section",
                  h5("Download Visualisasi"),
                  fluidRow(
                    column(6,
                           downloadButton("download_viz_plot", "Download Plot (PNG)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(6,
                           downloadButton("download_viz_report", "Download Report (PDF)",
                                          class = "btn btn-warning", style = "width: 100%;")
                    )
                  )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.data_loaded == false",
            div(class = "empty-state",
                tags$i(class = "fas fa-database fa-3x"),
                h4("Silakan load data terlebih dahulu di menu Manajemen Data")
            )
          )
        )
      ),
      
      # =============================================================================
      # TAB EKSPLORASI DATA - PETA SPASIAL
      # =============================================================================
      tabItem(
        tabName = "peta_spasial",
        
        box(
          title = tagList(icon("map"), "Peta Distribusi Spasial"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          conditionalPanel(
            condition = "output.data_loaded == true",
            
            fluidRow(
              column(4,
                     selectInput("map_variable", "Variabel untuk Dipetakan:",
                                 choices = NULL, width = "100%"),
                     
                     selectInput("map_type", "Tipe Peta:",
                                 choices = list(
                                   "Choropleth" = "choropleth",
                                   "Point Map" = "point",
                                   "Heat Map" = "heat"
                                 ), width = "100%"),
                     
                     selectInput("color_palette", "Skema Warna:",
                                 choices = list(
                                   "Viridis" = "viridis",
                                   "Blues" = "Blues",
                                   "Reds" = "Reds",
                                   "Greens" = "Greens"
                                 ), width = "100%"),
                     
                     numericInput("map_bins", "Jumlah Kelas:",
                                  value = 5, min = 3, max = 10, width = "100%"),
                     
                     actionButton("generate_map", "Generate Peta",
                                  class = "btn btn-primary", style = "width: 100%;")
              ),
              
              column(8,
                     conditionalPanel(
                       condition = "output.map_generated == true",
                       withSpinner(leafletOutput("spatial_map", height = "500px"), 
                                   color = waskita_colors$steel)
                     )
              )
            ),
            
            conditionalPanel(
              condition = "output.map_generated == true",
              hr(),
              fluidRow(
                column(6,
                       div(class = "stat-card",
                           h5("Statistik Spasial"),
                           verbatimTextOutput("spatial_stats")
                       )
                ),
                column(6,
                       div(class = "interpretation-box",
                           h5("Interpretasi Pola Spasial"),
                           verbatimTextOutput("spatial_interpretation")
                       )
                )
              ),
              
              div(class = "download-section",
                  h5("Download Peta"),
                  fluidRow(
                    column(6,
                           downloadButton("download_map_image", "Download Map (PNG)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(6,
                           downloadButton("download_spatial_report", "Download Report (PDF)",
                                          class = "btn btn-warning", style = "width: 100%;")
                    )
                  )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.data_loaded == false",
            div(class = "empty-state",
                tags$i(class = "fas fa-database fa-3x"),
                h4("Silakan load data terlebih dahulu di menu Manajemen Data")
            )
          )
        )
      ),
      
      # =============================================================================
      # TAB UJI ASUMSI DATA
      # =============================================================================
      tabItem(
        tabName = "uji_asumsi",
        
        box(
          title = tagList(icon("check-circle"), "Uji Asumsi Data"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          conditionalPanel(
            condition = "output.data_loaded == true",
            
            fluidRow(
              column(6,
                     h4("Uji Normalitas", style = "margin-bottom: 20px;"),
                     
                     selectInput("normality_variable", "Pilih Variabel:",
                                 choices = NULL, width = "100%"),
                     
                     selectInput("normality_test", "Metode Uji:",
                                 choices = list(
                                   "Shapiro-Wilk Test" = "shapiro",
                                   "Kolmogorov-Smirnov Test" = "ks",
                                   "Anderson-Darling Test" = "ad"
                                 ), width = "100%"),
                     
                     actionButton("run_normality", "Jalankan Uji",
                                  class = "btn btn-primary", style = "width: 100%;"),
                     
                     conditionalPanel(
                       condition = "output.normality_done == true",
                       div(class = "stat-card",
                           h5("Hasil Uji Normalitas"),
                           verbatimTextOutput("normality_result")
                       )
                     )
              ),
              
              column(6,
                     h4("Uji Homogenitas Varians", style = "margin-bottom: 20px;"),
                     
                     selectInput("homogeneity_numeric", "Variabel Numerik:",
                                 choices = NULL, width = "100%"),
                     
                     selectInput("homogeneity_group", "Variabel Grup:",
                                 choices = NULL, width = "100%"),
                     
                     selectInput("homogeneity_test", "Metode Uji:",
                                 choices = list(
                                   "Levene Test" = "levene",
                                   "Bartlett Test" = "bartlett",
                                   "Fligner-Killeen Test" = "fligner"
                                 ), width = "100%"),
                     
                     actionButton("run_homogeneity", "Jalankan Uji",
                                  class = "btn btn-primary", style = "width: 100%;"),
                     
                     conditionalPanel(
                       condition = "output.homogeneity_done == true",
                       div(class = "stat-card",
                           h5("Hasil Uji Homogenitas"),
                           verbatimTextOutput("homogeneity_result")
                       )
                     )
              )
            ),
            
            conditionalPanel(
              condition = "output.normality_done == true || output.homogeneity_done == true",
              hr(),
              h4("Plot Diagnostik"),
              withSpinner(plotlyOutput("diagnostic_plots", height = "400px"), 
                          color = waskita_colors$steel),
              
              div(class = "interpretation-box",
                  h5("Interpretasi Asumsi"),
                  verbatimTextOutput("assumptions_interpretation")
              ),
              
              div(class = "download-section",
                  h5("Download Hasil Uji"),
                  fluidRow(
                    column(4,
                           downloadButton("download_assumptions_plot", "Download Plot (PNG)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_assumptions_results", "Download Results (CSV)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_assumptions_report", "Download Report (PDF)",
                                          class = "btn btn-warning", style = "width: 100%;")
                    )
                  )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.data_loaded == false",
            div(class = "empty-state",
                tags$i(class = "fas fa-database fa-3x"),
                h4("Silakan load data terlebih dahulu di menu Manajemen Data")
            )
          )
        )
      ),
      
      # =============================================================================
      # TAB UJI BEDA RATA-RATA
      # =============================================================================
      tabItem(
        tabName = "uji_rata_rata",
        
        box(
          title = tagList(icon("calculator"), "Uji Beda Rata-rata"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          conditionalPanel(
            condition = "output.data_loaded == true",
            
            fluidRow(
              column(4,
                     selectInput("ttest_type", "Jenis Uji:",
                                 choices = list(
                                   "One Sample t-test" = "one_sample",
                                   "Independent t-test" = "independent", 
                                   "Paired t-test" = "paired"
                                 ), width = "100%"),
                     
                     selectInput("ttest_variable", "Variabel Numerik:",
                                 choices = NULL, width = "100%"),
                     
                     conditionalPanel(
                       condition = "input.ttest_type == 'one_sample'",
                       numericInput("test_value", "Nilai Uji (μ₀):",
                                    value = 0, width = "100%")
                     ),
                     
                     conditionalPanel(
                       condition = "input.ttest_type == 'independent'",
                       selectInput("ttest_group", "Variabel Grup:",
                                   choices = NULL, width = "100%"),
                       checkboxInput("equal_variance", "Asumsi Varians Sama", value = TRUE)
                     ),
                     
                     conditionalPanel(
                       condition = "input.ttest_type == 'paired'",
                       selectInput("ttest_paired_var", "Variabel Kedua:",
                                   choices = NULL, width = "100%")
                     ),
                     
                     selectInput("alternative", "Hipotesis Alternatif:",
                                 choices = list(
                                   "Two-sided" = "two.sided",
                                   "Greater" = "greater",
                                   "Less" = "less"
                                 ), width = "100%"),
                     
                     numericInput("confidence_level", "Confidence Level:",
                                  value = 0.95, min = 0.8, max = 0.99, step = 0.01, width = "100%"),
                     
                     actionButton("run_ttest", "Jalankan Uji",
                                  class = "btn btn-primary", style = "width: 100%;")
              ),
              
              column(8,
                     conditionalPanel(
                       condition = "output.ttest_done == true",
                       div(class = "stat-card",
                           h5("Hasil Uji t"),
                           verbatimTextOutput("ttest_result")
                       ),
                       
                       withSpinner(plotlyOutput("ttest_plot", height = "350px"), 
                                   color = waskita_colors$steel)
                     )
              )
            ),
            
            conditionalPanel(
              condition = "output.ttest_done == true",
              hr(),
              fluidRow(
                column(6,
                       div(class = "stat-card",
                           h5("Effect Size & Power"),
                           verbatimTextOutput("ttest_effect_size")
                       )
                ),
                column(6,
                       div(class = "interpretation-box",
                           h5("Interpretasi Praktis"),
                           verbatimTextOutput("ttest_interpretation")
                       )
                )
              ),
              
              div(class = "download-section",
                  h5("Download Hasil"),
                  fluidRow(
                    column(4,
                           downloadButton("download_ttest_plot", "Download Plot (PNG)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_ttest_results", "Download Results (CSV)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_ttest_report", "Download Report (PDF)",
                                          class = "btn btn-warning", style = "width: 100%;")
                    )
                  )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.data_loaded == false",
            div(class = "empty-state",
                tags$i(class = "fas fa-database fa-3x"),
                h4("Silakan load data terlebih dahulu di menu Manajemen Data")
            )
          )
        )
      ),
      
      # =============================================================================
      # TAB UJI PROPORSI & VARIANCE
      # =============================================================================
      tabItem(
        tabName = "uji_proporsi",
        
        box(
          title = tagList(icon("pie-chart"), "Uji Proporsi & Variance"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          conditionalPanel(
            condition = "output.data_loaded == true",
            
            tabsetPanel(
              tabPanel("Uji Proporsi",
                       br(),
                       fluidRow(
                         column(4,
                                selectInput("prop_test_type", "Jenis Uji:",
                                            choices = list(
                                              "One Sample Proportion" = "one_prop",
                                              "Two Sample Proportion" = "two_prop"
                                            ), width = "100%"),
                                
                                selectInput("prop_variable", "Variabel Kategorikal:",
                                            choices = NULL, width = "100%"),
                                
                                conditionalPanel(
                                  condition = "input.prop_test_type == 'one_prop'",
                                  numericInput("prop_test_value", "Proporsi Uji (p₀):",
                                               value = 0.5, min = 0, max = 1, step = 0.01, width = "100%")
                                ),
                                
                                conditionalPanel(
                                  condition = "input.prop_test_type == 'two_prop'",
                                  selectInput("prop_group", "Variabel Grup:",
                                              choices = NULL, width = "100%")
                                ),
                                
                                actionButton("run_prop_test", "Jalankan Uji",
                                             class = "btn btn-primary", style = "width: 100%;")
                         ),
                         
                         column(8,
                                conditionalPanel(
                                  condition = "output.prop_test_done == true",
                                  div(class = "stat-card",
                                      h5("Hasil Uji Proporsi"),
                                      verbatimTextOutput("prop_test_result")
                                  ),
                                  
                                  withSpinner(plotlyOutput("prop_test_plot", height = "300px"), 
                                              color = waskita_colors$steel)
                                )
                         )
                       )
              ),
              
              tabPanel("Uji Variance",
                       br(),
                       fluidRow(
                         column(4,
                                selectInput("var_test_type", "Jenis Uji:",
                                            choices = list(
                                              "One Sample Variance" = "one_var",
                                              "Two Sample Variance (F-test)" = "two_var"
                                            ), width = "100%"),
                                
                                selectInput("var_variable", "Variabel Numerik:",
                                            choices = NULL, width = "100%"),
                                
                                conditionalPanel(
                                  condition = "input.var_test_type == 'one_var'",
                                  numericInput("var_test_value", "Variance Uji (σ²₀):",
                                               value = 1, min = 0, step = 0.01, width = "100%")
                                ),
                                
                                conditionalPanel(
                                  condition = "input.var_test_type == 'two_var'",
                                  selectInput("var_group", "Variabel Grup:",
                                              choices = NULL, width = "100%")
                                ),
                                
                                actionButton("run_var_test", "Jalankan Uji",
                                             class = "btn btn-primary", style = "width: 100%;")
                         ),
                         
                         column(8,
                                conditionalPanel(
                                  condition = "output.var_test_done == true",
                                  div(class = "stat-card",
                                      h5("Hasil Uji Variance"),
                                      verbatimTextOutput("var_test_result")
                                  ),
                                  
                                  withSpinner(plotlyOutput("var_test_plot", height = "300px"), 
                                              color = waskita_colors$steel)
                                )
                         )
                       )
              )
            ),
            
            conditionalPanel(
              condition = "output.prop_test_done == true || output.var_test_done == true",
              hr(),
              div(class = "interpretation-box",
                  h5("Interpretasi Gabungan"),
                  verbatimTextOutput("prop_var_interpretation")
              ),
              
              div(class = "download-section",
                  h5("Download Hasil"),
                  fluidRow(
                    column(4,
                           downloadButton("download_propvar_plots", "Download Plots (PNG)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_propvar_results", "Download Results (CSV)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_propvar_report", "Download Report (PDF)",
                                          class = "btn btn-warning", style = "width: 100%;")
                    )
                  )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.data_loaded == false",
            div(class = "empty-state",
                tags$i(class = "fas fa-database fa-3x"),
                h4("Silakan load data terlebih dahulu di menu Manajemen Data")
            )
          )
        )
      ),
      
      # =============================================================================
      # TAB ANOVA
      # =============================================================================
      tabItem(
        tabName = "anova",
        
        box(
          title = tagList(icon("chart-area"), "Analysis of Variance (ANOVA)"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          conditionalPanel(
            condition = "output.data_loaded == true",
            
            fluidRow(
              column(4,
                     selectInput("anova_type", "Jenis ANOVA:",
                                 choices = list(
                                   "One-Way ANOVA" = "one_way",
                                   "Two-Way ANOVA" = "two_way"
                                 ), width = "100%"),
                     
                     selectInput("anova_dependent", "Variabel Dependen:",
                                 choices = NULL, width = "100%"),
                     
                     selectInput("anova_factor1", "Faktor 1:",
                                 choices = NULL, width = "100%"),
                     
                     conditionalPanel(
                       condition = "input.anova_type == 'two_way'",
                       selectInput("anova_factor2", "Faktor 2:",
                                   choices = NULL, width = "100%"),
                       checkboxInput("include_interaction", "Include Interaction", value = TRUE)
                     ),
                     
                     numericInput("anova_alpha", "Alpha Level:",
                                  value = 0.05, min = 0.01, max = 0.1, step = 0.01, width = "100%"),
                     
                     actionButton("run_anova", "Jalankan ANOVA",
                                  class = "btn btn-primary", style = "width: 100%;")
              ),
              
              column(8,
                     conditionalPanel(
                       condition = "output.anova_done == true",
                       div(class = "stat-card",
                           h5("Tabel ANOVA"),
                           verbatimTextOutput("anova_result")
                       ),
                       
                       withSpinner(plotlyOutput("anova_plot", height = "350px"), 
                                   color = waskita_colors$steel)
                     )
              )
            ),
            
            conditionalPanel(
              condition = "output.anova_done == true",
              hr(),
              fluidRow(
                column(6,
                       div(class = "stat-card",
                           h5("Post-hoc Tests"),
                           verbatimTextOutput("posthoc_result")
                       )
                ),
                column(6,
                       div(class = "stat-card",
                           h5("Effect Size"),
                           verbatimTextOutput("effect_size_result")
                       )
                )
              ),
              
              div(class = "interpretation-box",
                  h5("Interpretasi ANOVA"),
                  verbatimTextOutput("anova_interpretation")
              ),
              
              div(class = "download-section",
                  h5("Download Hasil"),
                  fluidRow(
                    column(4,
                           downloadButton("download_anova_plots", "Download Plots (PNG)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_anova_results", "Download Results (CSV)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_anova_report", "Download Report (PDF)",
                                          class = "btn btn-warning", style = "width: 100%;")
                    )
                  )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.data_loaded == false",
            div(class = "empty-state",
                tags$i(class = "fas fa-database fa-3x"),
                h4("Silakan load data terlebih dahulu di menu Manajemen Data")
            )
          )
        )
      ),
      
      # =============================================================================
      # TAB REGRESI LINEAR
      # =============================================================================
      tabItem(
        tabName = "regresi",
        
        box(
          title = tagList(icon("chart-line"), "Regresi Linear Berganda"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          conditionalPanel(
            condition = "output.data_loaded == true",
            
            fluidRow(
              column(4,
                     h4("Spesifikasi Model", style = "margin-bottom: 20px;"),
                     
                     selectInput("reg_dependent", "Variabel Dependen (Y):",
                                 choices = NULL, width = "100%"),
                     
                     selectInput("reg_independent", "Variabel Independen (X):",
                                 choices = NULL, multiple = TRUE, width = "100%"),
                     
                     checkboxInput("include_intercept", "Include Intercept", value = TRUE),
                     
                     actionButton("run_regression", "Jalankan Regresi",
                                  class = "btn btn-primary", style = "width: 100%;")
              ),
              
              column(8,
                     conditionalPanel(
                       condition = "output.regression_done == true",
                       div(class = "stat-card",
                           h5("Ringkasan Model"),
                           verbatimTextOutput("regression_summary")
                       )
                     )
              )
            ),
            
            conditionalPanel(
              condition = "output.regression_done == true",
              hr(),
              
              tabsetPanel(
                tabPanel("Model Summary",
                         br(),
                         fluidRow(
                           column(6,
                                  div(class = "stat-card",
                                      h5("Model Fit Statistics"),
                                      verbatimTextOutput("model_fit_stats")
                                  )
                           ),
                           column(6,
                                  div(class = "stat-card",
                                      h5("Coefficients"),
                                      DT::dataTableOutput("coefficients_table")
                                  )
                           )
                         )
                ),
                
                tabPanel("Diagnostic Plots",
                         br(),
                         withSpinner(plotlyOutput("regression_diagnostics", height = "600px"), 
                                     color = waskita_colors$steel)
                ),
                
                tabPanel("Assumption Tests",
                         br(),
                         fluidRow(
                           column(6,
                                  div(class = "stat-card",
                                      h5("Normalitas Residual"),
                                      verbatimTextOutput("residual_normality")
                                  )
                           ),
                           column(6,
                                  div(class = "stat-card",
                                      h5("Homoskedastisitas"),
                                      verbatimTextOutput("homoscedasticity_test")
                                  )
                           )
                         ),
                         fluidRow(
                           column(6,
                                  div(class = "stat-card",
                                      h5("Multikolinearitas (VIF)"),
                                      verbatimTextOutput("vif_results")
                                  )
                           ),
                           column(6,
                                  div(class = "stat-card",
                                      h5("Autokorelasi"),
                                      verbatimTextOutput("autocorrelation_test")
                                  )
                           )
                         )
                )
              ),
              
              div(class = "interpretation-box",
                  h5("Interpretasi Model Regresi"),
                  verbatimTextOutput("regression_interpretation")
              ),
              
              div(class = "download-section",
                  h5("Download Hasil"),
                  fluidRow(
                    column(4,
                           downloadButton("download_regression_plots", "Download Plots (PNG)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_regression_results", "Download Results (CSV)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_regression_report", "Download Report (PDF)",
                                          class = "btn btn-warning", style = "width: 100%;")
                    )
                  )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.data_loaded == false",
            div(class = "empty-state",
                tags$i(class = "fas fa-database fa-3x"),
                h4("Silakan load data terlebih dahulu di menu Manajemen Data")
            )
          )
        )
      ),
      
      # =============================================================================
      # TAB ANALISIS SPASIAL
      # =============================================================================
      tabItem(
        tabName = "analisis_spasial",
        
        box(
          title = tagList(icon("map-marked-alt"), "Analisis Spasial & Autokorelasi"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          conditionalPanel(
            condition = "output.data_loaded == true",
            
            fluidRow(
              column(4,
                     h4("Pengaturan Analisis Spasial", style = "margin-bottom: 20px;"),
                     
                     selectInput("spatial_variable", "Variabel untuk Analisis:",
                                 choices = NULL, width = "100%"),
                     
                     selectInput("weight_type", "Tipe Matriks Penimbang:",
                                 choices = list(
                                   "Distance-based (Inverse)" = "distance_inverse",
                                   "Distance-based (Exponential)" = "distance_exp",
                                   "K-Nearest Neighbors" = "knn"
                                 ), width = "100%"),
                     
                     conditionalPanel(
                       condition = "input.weight_type == 'knn'",
                       numericInput("k_neighbors", "Jumlah Tetangga (k):",
                                    value = 5, min = 3, max = 20, width = "100%")
                     ),
                     
                     actionButton("run_spatial_analysis", "Jalankan Analisis",
                                  class = "btn btn-primary", style = "width: 100%;")
              ),
              
              column(8,
                     conditionalPanel(
                       condition = "output.spatial_done == true",
                       div(class = "stat-card",
                           h5("Hasil Autokorelasi Spasial"),
                           verbatimTextOutput("spatial_autocorr_result")
                       ),
                       
                       withSpinner(plotlyOutput("spatial_plot", height = "350px"), 
                                   color = waskita_colors$steel)
                     )
              )
            ),
            
            conditionalPanel(
              condition = "output.spatial_done == true",
              hr(),
              fluidRow(
                column(6,
                       div(class = "stat-card",
                           h5("Statistik Matriks Jarak"),
                           verbatimTextOutput("distance_matrix_stats")
                       )
                ),
                column(6,
                       div(class = "interpretation-box",
                           h5("Interpretasi Spasial"),
                           verbatimTextOutput("spatial_interpretation_text")
                       )
                )
              ),
              
              div(class = "download-section",
                  h5("Download Hasil"),
                  fluidRow(
                    column(4,
                           downloadButton("download_spatial_plots", "Download Plots (PNG)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_distance_matrix", "Download Matrix (CSV)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_spatial_report_final", "Download Report (PDF)",
                                          class = "btn btn-warning", style = "width: 100%;")
                    )
                  )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.data_loaded == false",
            div(class = "empty-state",
                tags$i(class = "fas fa-database fa-3x"),
                h4("Silakan load data terlebih dahulu di menu Manajemen Data")
            )
          )
        )
      )
    )
  )
)