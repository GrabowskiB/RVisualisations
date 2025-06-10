# R Data Analysis Project - Package Requirements
# Author: Grabowski Bartek
# Description: Install and load all required packages for the Project

# Set CRAN repository if not already set
if (getOption("repos")["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org/"))
}

# Define all required packages across all projects
required_packages <- c(
  # Core data manipulation and analysis
  "readr",           # Fast and friendly data import
  "dplyr",           # Data manipulation grammar
  "tidyr",           # Tidy data principles
  "stringr",         # String manipulation
  "lubridate",       # Date and time manipulation
  "forcats",         # Factor manipulation
  
  # Visualization - Static
  "ggplot2",         # Grammar of graphics
  "scales",          # Scale functions for ggplot2
  "RColorBrewer",    # Color palettes
  "viridis",         # Perceptually uniform color scales
  "patchwork",       # Combine ggplot2 plots
  
  # Visualization - Interactive
  "plotly",          # Interactive web graphics
  "gganimate",       # Animated ggplot2 plots
  "networkD3",       # Interactive network graphs
  "htmlwidgets",     # HTML widgets framework
  
  # Geospatial analysis
  "sf",              # Simple features for spatial data
  "rnaturalearth",   # World map data
  "rnaturalearthdata", # Natural earth data
  
  # Network analysis
  "igraph",          # Network analysis and visualization
  "ggraph",          # Grammar of graphics for networks
  
  # Text mining and NLP
  "tidytext",        # Text mining using tidy principles
  "wordcloud2",      # Interactive word clouds
  
  # Web technologies
  "webshot2"         # Web page screenshots
)

# Function to install missing packages
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages) > 0) {
    cat("Installing", length(new_packages), "missing packages:\n")
    cat(paste(new_packages, collapse = ", "), "\n\n")
    
    # Install packages with progress indication
    for(pkg in new_packages) {
      cat("Installing:", pkg, "...")
      tryCatch({
        install.packages(pkg, quiet = TRUE)
        cat(" ✓\n")
      }, error = function(e) {
        cat(" ✗ Failed:", e$message, "\n")
      })
    }
    
    # Special case for webshot2 - install chromium if needed
    if("webshot2" %in% new_packages) {
      cat("\nInstalling Chromium for webshot2...\n")
      tryCatch({
        webshot2::install_chromium()
        cat("Chromium installed successfully ✓\n")
      }, error = function(e) {
        cat("Chromium installation failed. Screenshots may not work.\n")
      })
    }
    
  } else {
    cat("All required packages are already installed ✓\n")
  }
}

# Function to load all packages
load_packages <- function(packages) {
  cat("\nLoading packages...\n")
  
  failed_packages <- c()
  
  for(pkg in packages) {
    tryCatch({
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }, error = function(e) {
      failed_packages <<- c(failed_packages, pkg)
    })
  }
  
  if(length(failed_packages) > 0) {
    cat("Failed to load packages:", paste(failed_packages, collapse = ", "), "\n")
    cat("Try reinstalling these packages.\n")
  } else {
    cat("All packages loaded successfully ✓\n")
  }
}

# Function to check R version compatibility
check_r_version <- function() {
  r_version <- R.version.string
  cat("R Version:", r_version, "\n")
  
  if(getRversion() < "4.0.0") {
    warning("This Project requires R version 4.0.0 or higher for optimal compatibility.")
  }
}

# Function to display package information
display_package_info <- function() {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("R DATA ANALYSIS PROJECT - PACKAGE REQUIREMENTS\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  cat("\nProject Dependencies:\n")
  cat("• Chess Championships Analysis: ggplot2, plotly, networkD3, dplyr, tidyr\n")
  cat("• Geomagnetic Data Analysis: sf, rnaturalearth, ggplot2, viridis, patchwork\n")
  cat("• Social Media Analytics: tidytext, wordcloud2, lubridate, stringr, scales\n")
  
  cat("\nTotal packages required:", length(required_packages), "\n")
  cat("Package categories:\n")
  cat("  - Core data manipulation: 6 packages\n")
  cat("  - Static visualization: 5 packages\n")
  cat("  - Interactive visualization: 4 packages\n")
  cat("  - Geospatial analysis: 3 packages\n")
  cat("  - Network analysis: 2 packages\n")
  cat("  - Text mining: 2 packages\n")
  cat("  - Web technologies: 1 package\n")
}

# Main execution
main <- function() {
  display_package_info()
  check_r_version()
  install_missing_packages(required_packages)
  load_packages(required_packages)
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("SETUP COMPLETE! You can now run the analysis scripts.\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  cat("\nNext steps:\n")
  cat("1. Run: source('Chess/chess-analysis.r')\n")
  cat("2. Run: source('NorthPoleLocations/geomagnetic-analysis.r')\n")
  cat("3. Run: source('TrumpTweets/social-media-analysis.r')\n")
}

# Helper function to create directory structure if needed
setup_directories <- function() {
  dirs_to_create <- c(
    "ChessPlots",
    "ChessVisualizations", 
    "NorthPoleLocationsPlots",
    "TrumpTweetsPlots"
  )
  
  for(dir in dirs_to_create) {
    if(!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("Created directory:", dir, "\n")
    }
  }
}

# Export functions for use in other scripts
if(!exists("REQUIREMENTS_LOADED")) {
  # Run main setup
  main()
  setup_directories()
  
  # Set flag to prevent re-running
  REQUIREMENTS_LOADED <- TRUE
}

# Utility function for scripts to check if packages are loaded
check_package_loaded <- function(package_name) {
  package_name %in% (.packages())
}

cat("\nrequirements.R loaded. All functions available for use.\n")