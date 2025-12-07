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

if(!dir.exists("www/data")) {
  dir.create("www/data", recursive = TRUE)
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
    
    list(
      code = code,
      hex = hex_color,
      base = recipe$base_pigment,
      base_pct = recipe$base_pct,
      vitbas_pct = recipe$vitbas_pct,
      shade_pct = recipe$shade_pct,
      shade_pigment = recipe$shade_pigment,
      pigment_name = pigments_db[[recipe$base_pigment]]$name
    )
  })
  
  output_file <- paste0("www/data/palette_", palette_type, ".json")
  write_json(palette_data, output_file, auto_unbox = TRUE, pretty = TRUE)
  
  cat("✓ Saved", length(palette_data), "swatches to", output_file, "\n\n")
}

cat("\n=== Generating Paint-o-matic Palette Files ===\n\n")
generate_static_palette("raa", "J318")
generate_static_palette("extended", "J318")
cat("=== Done! ===\n")
