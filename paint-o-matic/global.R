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

# Source unified pigment database
source("R/data/pigments_unified.R")

# === BACKWARD COMPATIBILITY LAYER ===
# Allows existing code using km, suppliers, raa_pigments to work unchanged

# Extract km-compatible structure (properties only)
km <- lapply(pigments_db, function(p) {
  props <- p$properties
  # Add id and name at top level for compatibility
  c(list(name = p$name), props)
})
names(km) <- names(pigments_db)

# Extract suppliers (filter out NULL suppliers)
suppliers <- lapply(pigments_db, function(p) {
  if(is.null(p$suppliers)) return(NULL)
  
  # Flatten structure to match old format
  list(
    name = p$name,
    kremer_match = if(!is.null(p$suppliers$kremer)) paste0(p$suppliers$kremer$match, " match") else NULL,
    kremer_id = p$suppliers$kremer$id %||% NULL,
    kremer_url = p$suppliers$kremer$url %||% NULL,
    ottosson_match = p$suppliers$ottosson$name %||% NULL,
    ottosson_url = p$suppliers$ottosson$url %||% NULL,
    claessons_match = p$suppliers$claessons$name %||% NULL,
    claessons_url = p$suppliers$claessons$url %||% NULL,
    pigmentum_id = p$suppliers$pigmentum$id %||% NULL,
    notes = p$notes %||% ""
  )
})
suppliers <- suppliers[!sapply(suppliers, is.null)]

# Extract RAÄ pigments list
raa_pigments <- names(pigments_db)[
  sapply(pigments_db, function(p) isTRUE(p$metadata$is_raa))
]

# Auto-generate name-to-ID lookup (eliminates pigment_name_to_id duplication)
pigment_name_to_id <- setNames(
  names(pigments_db), 
  sapply(pigments_db, function(p) p$name)
)

# Load tar and miscellaneous materials data
source("R/data/tar_and_materials.R")

# === TAR BACKWARD COMPATIBILITY LAYER ===
# Auto-generate tar_colors and tar_suppliers from tars_db for backward compatibility
tar_colors <- lapply(tars_db, function(tar) {
  list(
    rgb = tar$properties$rgb,
    hex = sprintf("#%02X%02X%02X", tar$properties$rgb[1], tar$properties$rgb[2], tar$properties$rgb[3]),
    description = tar$metadata$description
  )
})

# Auto-generate tar_suppliers from tars_db
# For each supplier in each tar, create entries keyed by tar name + supplier
tar_suppliers <- list()
for(tar_name in names(tars_db)) {
  tar <- tars_db[[tar_name]]
  if(!is.null(tar$suppliers)) {
    for(supplier_key in names(tar$suppliers)) {
      supplier_info <- tar$suppliers[[supplier_key]]
      # Create unique key: tar_name + supplier_key
      entry_key <- paste0(gsub("[^[:alnum:]]", "_", tolower(tar_name)), "_", supplier_key)
      tar_suppliers[[entry_key]] <- list(
        name = supplier_info$match,
        category = tar_name,  # Use tar name as category for backward compatibility
        supplier = supplier_key,
        description = tar$metadata$description,
        url = supplier_info$url,
        notes = supplier_info$notes
      )
    }
  }
}

# Load helper functions for unified pigment database
source("R/utils/pigment_helpers.R")

# === KULTURKULÖR PRESET SYSTEM ===
source("kulturkulor_recipes.r")
source("kulturkulor_recipes_part2.r")
source("kulturkulor_recipes_part3.r")
kulturkulor_complete <- c(kulturkulor, kulturkulor_part2, kulturkulor_part3)
