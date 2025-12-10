# Paint-o-matic -- blanda din egen linoljefärg!
# Copyright 2025 Tobias Hagberg
# Licens: GNU General Public License v3.0
# https://github.com/hmep/karlslund/tree/main/paint-o-matic

# generate_palettes.R
# Run this manually when pigments change
# Standalone version that doesn't require loading the full app

library(jsonlite)

# Define %||% operator for null coalescing
`%||%` <- function(a, b) if(is.null(a)) b else a

# Stub out memoise if not available (we don't need caching for generation)
if(!requireNamespace("memoise", quietly = TRUE)) {
  message("Package 'memoise' not found, stubbing it out (not needed for palette generation)")
  memoise <- function(f) f
} else {
  library(memoise)
}

# Load minimal dependencies
source("R/data/constants.R")
source("R/data/pigments_unified.R")
source("R/utils/color_mixing.R")
source("R/utils/swatch_generation.R")

# === LOAD DISPLAY ORDER (SINGLE SOURCE OF TRUTH) ===
source("R/data/pigment_display_order.R")
# This defines PIGMENT_DISPLAY_ORDER which contains the sorted pigment groups

if(!dir.exists("www/data")) {
  dir.create("www/data", recursive = TRUE)
}

# Helper function to find which display group a pigment belongs to
find_display_group <- function(pigment_id) {
  for(group_name in names(PIGMENT_DISPLAY_ORDER)) {
    if(pigment_id %in% PIGMENT_DISPLAY_ORDER[[group_name]]) {
      return(group_name)
    }
  }
  return("Övrigt")  # Fallback for unmapped pigments
}

generate_static_palette <- function(palette_type = "raa", shade_pigment = "J318") {
  cat("Generating", palette_type, "palette with", shade_pigment, "...\n")
  
  if(palette_type == "raa") {
    swatches <- generate_all_raa_swatches(shade_pigment)
  } else {
    swatches <- generate_all_extended_swatches(shade_pigment)
  }
  
  cat("Generated", length(swatches), "swatch recipes, calculating colors...\n")
  
  palette_data <- lapply(names(swatches), function(code) {
    recipe <- swatches[[code]]
    
    ids <- c()
    pcts <- c()
    if(recipe$base_pct > 0) {
      ids <- c(ids, recipe$base_pigment)
      pcts <- c(pcts, recipe$base_pct)
    }
    if(recipe$vitbas_pct > 0) {
      ids <- c(ids, "vitbas")
      pcts <- c(pcts, recipe$vitbas_pct)
    }
    if(recipe$shade_pct > 0) {
      ids <- c(ids, recipe$shade_pigment)
      pcts <- c(pcts, recipe$shade_pct)
    }
    
    color_rgb <- mix_colors(ids, pcts, pigments_db, use_tinting = TRUE)
    hex_color <- sprintf("#%02X%02X%02X", round(color_rgb[1]), round(color_rgb[2]), round(color_rgb[3]))
    
    # Look up display group from PIGMENT_DISPLAY_ORDER
    display_group <- find_display_group(recipe$base_pigment)
    
    list(
      code = code,
      hex = hex_color,
      base = recipe$base_pigment,
      base_pct = recipe$base_pct,
      vitbas_pct = recipe$vitbas_pct,
      shade_pct = recipe$shade_pct,
      shade_pigment = recipe$shade_pigment,
      pigment_name = pigments_db[[recipe$base_pigment]]$name,
      display_group = display_group  # Now correctly looked up from PIGMENT_DISPLAY_ORDER
    )
  })
  
  # Sort palette_data to match PIGMENT_DISPLAY_ORDER
  # This ensures swatches appear in the same order as dropdown menus
  cat("Sorting swatches by display order...\n")
  
  # Create a lookup for display order (group + position within group)
  display_order_lookup <- list()
  for(group_idx in seq_along(PIGMENT_DISPLAY_ORDER)) {
    group_name <- names(PIGMENT_DISPLAY_ORDER)[group_idx]
    group_pigments <- PIGMENT_DISPLAY_ORDER[[group_name]]
    
    for(pigment_idx in seq_along(group_pigments)) {
      pigment_id <- group_pigments[pigment_idx]
      # Store both group index and position within group
      # Format: "group_index.pigment_index" for easy sorting
      display_order_lookup[[pigment_id]] <- group_idx + (pigment_idx / 1000)
    }
  }
  
  # Sort palette_data by display order
  palette_data_sorted <- palette_data[order(sapply(palette_data, function(item) {
    base_id <- item$base
    # Default to end (999) if pigment not in display order
    display_order_lookup[[base_id]] %||% 999
  }))]
  
  output_file <- paste0("www/data/palette_", palette_type, ".json")
  write_json(palette_data_sorted, output_file, auto_unbox = TRUE, pretty = TRUE)
  
  cat("✓ Saved", length(palette_data_sorted), "swatches to", output_file, "\n")
  
  # Verify display groups
  unique_groups <- unique(sapply(palette_data_sorted, function(x) x$display_group))
  cat("  Display groups in palette:", paste(unique_groups, collapse=", "), "\n\n")
}

cat("\n=== Generating Paint-o-matic Palette Files ===\n\n")
generate_static_palette("raa", "J318")
generate_static_palette("extended", "J318")
cat("=== Done! ===\n")
cat("\nPalette files are now sorted according to R/data/pigment_display_order.R\n")
cat("This ensures dropdown menus and swatch displays use the same ordering.\n")