# Paint-o-matic -- blanda din egen linoljefärg!
# Copyright 2025 Tobias Hagberg
# Licens: GNU General Public License v3.0
# https://github.com/hmep/karlslund/tree/main/paint-o-matic

# Swatch generation utilities
# Functions for generating color swatches for different palette types

# Define shading pigments available for user selection (ONLY existing pigments)
shading_pigments <- list(
  "Järnoxidsvart nr 318" = "J318",
  "Bensvart nr 98"  = "BS98",
  "Kimrök" = "47250",
  "Spinel-svart" = "47400",
  "Mangansvart" = "47501",
  "Svartoxid PBk11" = "44450",
  "Träkolspulver"= "47800",
  "Järnoxidsvart neutralt"= "48401",
  "Grafitpulver silver"= "47700"
)

# Get list of shading pigment IDs (to exclude from base colors)
shading_pigment_ids <- unlist(shading_pigments, use.names = FALSE)

# Generic function to generate swatch matrices for any pigment set
# Parameters:
#   pigments: vector of pigment IDs to generate swatches for
#   vitbas_increments: vector of vitbas percentages (e.g., c(0, 15, 30, 45, 60, 75, 90))
#   shade_increments: vector of shading percentages
#   shade_pigment: ID of shading pigment to use
#   code_prefix: prefix for swatch codes (e.g., "RAA" or "EXT")
#   mask: optional mask to filter swatches (matrix, vector, function, or single logical)
generate_swatch_matrix <- function(pigments, vitbas_increments, shade_increments, 
                                   shade_pigment, code_prefix = "MAT", mask = NULL) {
  all_swatches <- list()
  
  for(base_id in pigments) {
    swatch_index <- 0
    
    for(i_shade in seq_along(shade_increments)) {
      shade_pct <- shade_increments[i_shade]
      
      for(i_vitbas in seq_along(vitbas_increments)) {
        vitbas_pct <- vitbas_increments[i_vitbas]
        swatch_index <- swatch_index + 1
        
        # Calculate base percentage
        base_pct <- 100 - vitbas_pct - shade_pct
        
        if(base_pct >= 0) {
          # Apply mask if provided
          include_swatch <- TRUE
          
          if(!is.null(mask)) {
            if(is.function(mask)) {
              # Mask is a function - call it
              include_swatch <- mask(base_id, vitbas_pct, shade_pct, base_pct)
            } else if(is.matrix(mask) || is.data.frame(mask)) {
              # Mask is a matrix - check dimensions and index
              if(i_shade <= nrow(mask) && i_vitbas <= ncol(mask)) {
                include_swatch <- mask[i_shade, i_vitbas]
              } else {
                include_swatch <- FALSE  # Out of bounds = exclude
              }
            } else if(is.logical(mask) && length(mask) == 1) {
              # Single TRUE/FALSE applies to all
              include_swatch <- mask
            } else if(is.logical(mask)) {
              # Vector mask - use by index
              if(swatch_index <= length(mask)) {
                include_swatch <- mask[swatch_index]
              } else {
                include_swatch <- FALSE
              }
            }
          }
          
          # Only create swatch if mask allows it
          if(include_swatch) {
            # Generate unique code (use %g for numeric - handles both integers and decimals)
            swatch_code <- sprintf("%s_%s_%gV%gS", code_prefix, 
                                   toupper(substr(base_id, 1, 5)), vitbas_pct, shade_pct)
            
            all_swatches[[swatch_code]] <- list(
              base_pigment = base_id,
              base_pct = base_pct,
              vitbas_pct = vitbas_pct,
              shade_pigment = shade_pigment,
              shade_pct = shade_pct
            )
          }
        }
      }
    }
  }
  
  all_swatches
}

# Get list of ALL pigments for extended swatches (including RAÄ, excluding whites, fillers, and shading pigments)
get_extended_base_pigments <- function() {
  filler_ids <- c("599930", "58000", "58010", "58162", "58900", "58250")
  white_ids <- c("vitbas", "44100", "44400")
  # Only exclude fillers, whites, and shading pigments - INCLUDE RAÄ pigments as base colors
  exclude_ids <- c(filler_ids, white_ids, shading_pigment_ids)
  
  # Get all pigment IDs except excluded ones
  all_ids <- names(pigments_db)
  base_pigments <- setdiff(all_ids, exclude_ids)
  
  base_pigments
}

# Generate all swatches for all base pigments (including RAÄ) - uses generic matrix generator  
generate_all_extended_swatches <- function(shade_pigment_id = "J318") {
  base_pigments <- get_extended_base_pigments()
  
  # Extended pattern: Logarithmic spacing for perceptually uniform steps
  # More granular in light ranges where human perception is most sensitive
  vitbas_increments <- c(0, 15, 30, 45, 60, 70, 78, 85, 90)  # 9 levels, logarithmic
  shade_increments <- c(0, 8, 18, 32, 50)  # 5 levels
  
  # Mask matrix: TRUE = show this swatch, FALSE = skip it
  # Rows = shade levels (0, 8, 18, 32, 50)
  # Cols = vitbas levels (0, 15, 30, 45, 60, 70, 78, 85, 90)
  extended_mask <- matrix(c(
    # Row 1: 0% shade
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    # Row 2: 8% shade
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
    # Row 3: 18% shade
    TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
    # Row 4: 32% shade
    TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
    # Row 5: 50% shade
    TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  ), nrow = 5, byrow = TRUE)
  
  generate_swatch_matrix(base_pigments, vitbas_increments, shade_increments, 
                         shade_pigment_id, "EXT", mask = extended_mask)
}

# === RAÄ KULTURKULÖR EXACT RECIPE SPECIFICATION ===

# RAÄ exact vitbas and svart increments (from their published data)
RAA_VITBAS_INCREMENTS <- c(0, 14.28, 15, 29.27, 30, 41.86, 42.85, 45, 57.14, 60, 73.17, 75, 85.71, 90)
RAA_SHADE_INCREMENTS <- c(0, 2.44, 4.76, 6.97)

# RAÄ Mask Matrix (14 vitbas × 4 shade)
# TRUE = RAÄ publishes this combination, FALSE = not published
# Rows = shade levels (0, 2.44, 4.76, 6.97)
# Cols = vitbas levels (0, 14.28, 15, 29.27, 30, 41.86, 42.85, 45, 57.14, 60, 73.17, 75, 85.71, 90)
RAA_MASK_PATTERN_A <- matrix(c(
  # Shade 0% (A series: 1A, 2A, 3A, 4A, 5A, 6A, 7A)
  TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,
  # Shade 2.44% (B series: 1B, 3B, 6B)
  TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE,
  # Shade 4.76% (C series: 1C, 2C, 4C, 5C, 7C)
  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE,
  # Shade 6.97% (D series: 1D, 4D)
  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
), nrow = 4, byrow = TRUE)

# Pattern B: Pure tinting only (7 combinations - only row 1)
RAA_MASK_PATTERN_B <- matrix(c(
  # Shade 0% only (A series: 1A through 7A)
  TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,
  # No other shade levels
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
), nrow = 4, byrow = TRUE)

# Generate RAÄ swatches using exact masks
generate_all_raa_swatches <- function(shade_pigment_id = "J318") {
  # Pattern A pigments (17 recipes each - full mask)
  pattern_a_pigments <- c("J225", "J180M", "J120N", "ER48A", "J663", "J686", "J920",
                          "LO92", "GO94", "OU103", "BU100", "BRU39", 
                          "BT44", "OT46", "KG83", "UB88", "KB28")
  
  # Pattern B pigments (7 recipes each - tinting only mask)
  pattern_b_pigments <- c("J318", "GO94_GU30", "GU30", "GRAU36", "BS98")
  
  all_swatches <- list()
  
  # Generate Pattern A swatches (with Pattern A mask)
  for(pigment in pattern_a_pigments) {
    if(pigment %in% shading_pigment_ids) next
    
    swatches <- generate_swatch_matrix(
      c(pigment), 
      RAA_VITBAS_INCREMENTS, 
      RAA_SHADE_INCREMENTS, 
      shade_pigment_id, 
      "RAA",
      mask = RAA_MASK_PATTERN_A  # Apply Pattern A mask
    )
    all_swatches <- c(all_swatches, swatches)
  }
  
  # Generate Pattern B swatches (with Pattern B mask)
  for(pigment in pattern_b_pigments) {
    if(pigment %in% shading_pigment_ids) next
    
    swatches <- generate_swatch_matrix(
      c(pigment), 
      RAA_VITBAS_INCREMENTS, 
      RAA_SHADE_INCREMENTS, 
      shade_pigment_id, 
      "RAA",
      mask = RAA_MASK_PATTERN_B  # Apply Pattern B mask
    )
    all_swatches <- c(all_swatches, swatches)
  }
  
  all_swatches
}
