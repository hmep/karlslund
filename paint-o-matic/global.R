# Paint-o-matic -- blanda din egen linoljefärg!
# Copyright 2025 Tobias Hagberg
# Licens: GNU General Public License v3.0
# https://github.com/hmep/karlslund/tree/main/paint-o-matic

# Global data and utilities shared across the application
# This file is sourced by app.R and loads all required data and helper functions

# === LIBRARY LOADING ===
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(jsonlite)
library(memoise)

# === SWEDISH LOCALE CONFIGURATION === 
# Set Swedish locale for number formatting (cross-platform)
swedish_locale_set <- FALSE
locale_options <- c("sv_SE.UTF-8", "sv_SE", "Swedish_Sweden.1252", "Swedish")

for(locale in locale_options) {
  result <- tryCatch({
    Sys.setlocale("LC_NUMERIC", locale)
    TRUE
  }, warning = function(w) FALSE, error = function(e) FALSE)
  
  if(result && result != "C") {
    swedish_locale_set <- TRUE
    message("Swedish locale set successfully: ", locale)
    break
  }
}

if(!swedish_locale_set) {
  message("Could not set Swedish locale, using manual formatting only")
}

# === LOAD UTILITY FUNCTIONS ===
source("R/utils/formatting.R")
source("R/utils/color_mixing.R")
source("R/utils/calculations.R")
source("R/utils/km_compensation.R")
source("R/ui/ui_helpers.R")

# === LOAD DATA ===
source("R/data/constants.R")
source("R/data/pigment_database.R")
source("R/data/supplier_data.R")

# === KULTURKULÖR PRESET SYSTEM ===
source("kulturkulor_recipes.r")
source("kulturkulor_recipes_part2.r")
source("kulturkulor_recipes_part3.r")
kulturkulor_complete <- c(kulturkulor, kulturkulor_part2, kulturkulor_part3)
