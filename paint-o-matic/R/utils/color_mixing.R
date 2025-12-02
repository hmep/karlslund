# Color Mixing Utilities
# Functions for mixing colors and rendering previews

# Mix colors using vectorized operations
# use_tinting: if TRUE, weights colors by K+S tinting strength for realistic mixing
mix_colors <- function(ids, weights, pigment_list, use_tinting = TRUE) {
  if(length(ids) == 0) return(c(255, 255, 255))
  
  # Build color matrix - manually to avoid sapply issues
  n <- length(ids)
  cols <- matrix(0, nrow = n, ncol = 3)
  
  for(i in seq_along(ids)) {
    rgb_val <- pigment_list[[ids[i]]]$rgb
    if(is.null(rgb_val)) {
      cols[i, ] <- c(255, 255, 255)
    } else {
      cols[i, ] <- as.numeric(rgb_val)
    }
  }
  
  # Calculate weights
  if(use_tinting) {
    # REALISTIC MIXING: Weight by tinting strength (K+S values)
    tinting_strengths <- numeric(n)
    
    for(i in seq_along(ids)) {
      pigment <- pigment_list[[ids[i]]]
      K <- pigment$K %||% 0
      S <- pigment$S %||% 0
      
      # Combined optical power - use square root to soften the effect
      # This prevents ultra-high tinting pigments from completely dominating
      tinting_strengths[i] <- sqrt(K + S + 0.1)  # +0.1 prevents zero division
    }
    
    # Adjust weights by tinting strength
    adjusted_weights <- as.numeric(weights) * tinting_strengths
    
    # Normalize
    if(sum(adjusted_weights) > 0) {
      w <- adjusted_weights / sum(adjusted_weights)
    } else {
      # Fallback to simple weighting
      w <- as.numeric(weights) / sum(as.numeric(weights))
    }
  } else {
    # SIMPLE MIXING: Equal tinting strength for all pigments
    w <- as.numeric(weights) / sum(as.numeric(weights))
  }
  
  # Mix RGB values
  r <- sum(cols[, 1] * w)
  g <- sum(cols[, 2] * w)
  b <- sum(cols[, 3] * w)
  
  c(r, g, b)
}

# Render color preview with fullscreen zoom icon
render_preview <- function(color_hex, preview_id) {
  tags$div(class = "preview-container",
           tags$div(class = "preview", style = paste0("background:", color_hex)),
           tags$span(class = "zoom-icon", 
                     onclick = paste0("openFullscreen('", preview_id, "')"),
                     title = "Visa i helskärm",
                     HTML("+"))
  )
}

# Pigment name to ID mapping for kulturkulör recipes
pigment_name_to_id <- list(
  "Järnoxidrött nr 225" = "J225", "Järnoxidrött nr 180 M (Caput Mortuum)" = "J180M",
  "Järnoxidrött nr 120 N" = "J120N", "Engelskt rött nr 48 A" = "ER48A",
  "Järnoxidbrunt nr 663" = "J663", "Järnoxidbrunt nr 686" = "J686",
  "Järnoxidgult nr 920" = "J920", "Järnoxidsvart nr 318" = "J318",
  "Ljusockra nr 92" = "LO92", "Guldockra nr 94" = "GO94",
  "50% Guldockra nr 94 + 50% Grön umbra nr 30" = "GO94_GU30",
  "Obränd Umbra nr 103" = "OU103", "Bränd Umbra nr 100" = "BU100",
  "Brun Umbra nr 39" = "BRU39", "Grön Umbra nr 30" = "GU30", "Grå Umbra nr 36" = "GRAU36",
  "Bränd Terra nr 44" = "BT44", "Obränd Terra nr 46" = "OT46", "Bensvart nr 98" = "BS98",
  "Kromoxidgrönt nr GN 83" = "KG83", "Ultramarinblått nr 88" = "UB88", "Koboltblått nr 28" = "KB28"
)

# Calculate preview colors for each recipe using same method as main preview
calculate_recipe_color <- function(recipe, use_tinting = FALSE) {
  base_id <- pigment_name_to_id[[recipe$pigment]]
  if(is.null(base_id) || !base_id %in% names(km)) return(c(200, 200, 200))
  
  # Build list of pigments with their percentages
  ids <- character()
  pcts <- numeric()
  
  if(recipe$basfarg > 0) {
    ids <- c(ids, base_id)
    pcts <- c(pcts, recipe$basfarg)
  }
  if(recipe$vit > 0) {
    ids <- c(ids, "vitbas")
    pcts <- c(pcts, recipe$vit)
  }
  if(recipe$svart > 0) {
    ids <- c(ids, "J318")
    pcts <- c(pcts, recipe$svart)
  }
  
  if(length(ids) == 0) return(c(200, 200, 200))
  
  # Use tinting strength setting from toggle
  mix_colors(ids, pcts, km, use_tinting = use_tinting)
}
