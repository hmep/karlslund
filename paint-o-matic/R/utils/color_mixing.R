# Color Mixing Utilities
# Functions for mixing colors and rendering previews using Kubelka-Munk theory

# Mix colors using Kubelka-Munk theory
# Implements proper subtractive color mixing for paint pigments
mix_colors <- function(ids, weights, pigment_list, use_tinting = TRUE) {
  if(length(ids) == 0) return(c(255, 255, 255))
  
  n <- length(ids)
  
  # Normalize weights
  w <- as.numeric(weights) / sum(as.numeric(weights))
  
  if(use_tinting) {
    # KUBELKA-MUNK MIXING: Physically accurate paint mixing
    
    # Initialize mixed K and S for each RGB channel
    K_r <- 0; S_r <- 0
    K_g <- 0; S_g <- 0
    K_b <- 0; S_b <- 0
    
    for(i in seq_along(ids)) {
      pigment <- pigment_list[[ids[i]]]
      
      # Get K and S values for each RGB channel
      # If not available, fall back to estimating from RGB color
      if(!is.null(pigment$properties$K_r) && !is.null(pigment$properties$S_r)) {
        # Use stored K and S values (preferred)
        K_r <- K_r + w[i] * (pigment$properties$K_r %||% 0)
        S_r <- S_r + w[i] * (pigment$properties$S_r %||% 0)
        K_g <- K_g + w[i] * (pigment$properties$K_g %||% 0)
        S_g <- S_g + w[i] * (pigment$properties$S_g %||% 0)
        K_b <- K_b + w[i] * (pigment$properties$K_b %||% 0)
        S_b <- S_b + w[i] * (pigment$properties$S_b %||% 0)
      } else {
        # Fallback: estimate K and S from RGB color
        # This is less accurate but maintains backward compatibility
        rgb_val <- pigment$properties$rgb
        if(is.null(rgb_val)) {
          rgb_val <- c(255, 255, 255)
        }
        
        # Convert RGB to approximate reflectance
        rgb_norm <- as.numeric(rgb_val) / 255
        
        # Inverse gamma correction (sRGB to linear)
        R_linear <- ifelse(rgb_norm <= 0.04045,
                           rgb_norm / 12.92,
                           ((rgb_norm + 0.055) / 1.055)^2.4)
        
        # Estimate K and S from reflectance using inverse K-M
        # For reflectance R: K/S = (1 - R)^2 / (2*R)
        # Assume S is proportional to opacity, K from K/S ratio
        for(ch in 1:3) {
          R <- max(0.01, min(0.99, R_linear[ch]))  # Clamp to avoid division issues
          
          # Estimate S from legacy K+S tinting strength
          K_legacy <- pigment$properties$K %||% 1
          S_legacy <- pigment$properties$S %||% 10
          S_est <- S_legacy * 0.1  # Scale to reasonable range
          
          # Calculate K from K/S ratio
          ratio <- (1 - R)^2 / (2 * R)
          K_est <- ratio * S_est
          
          # Add weighted contribution
          if(ch == 1) { K_r <- K_r + w[i] * K_est; S_r <- S_r + w[i] * S_est }
          if(ch == 2) { K_g <- K_g + w[i] * K_est; S_g <- S_g + w[i] * S_est }
          if(ch == 3) { K_b <- K_b + w[i] * K_est; S_b <- S_b + w[i] * S_est }
        }
      }
    }
    
    # Apply Kubelka-Munk equation to get reflectance for each channel
    calculate_reflectance <- function(K, S) {
      if(S <= 0) return(0.5)  # Fallback
      ratio <- K / S
      R <- 1 + ratio - sqrt(ratio^2 + 2*ratio)
      max(0, min(1, R))  # Clamp to [0, 1]
    }
    
    R_r <- calculate_reflectance(K_r, S_r)
    R_g <- calculate_reflectance(K_g, S_g)
    R_b <- calculate_reflectance(K_b, S_b)
    
    # Apply sRGB gamma correction
    srgb_gamma <- function(R) {
      R <- max(0, min(1, R))
      if(R <= 0.0031308) {
        12.92 * R
      } else {
        1.055 * R^(1/2.4) - 0.055
      }
    }
    
    r <- round(srgb_gamma(R_r) * 255)
    g <- round(srgb_gamma(R_g) * 255)
    b <- round(srgb_gamma(R_b) * 255)
    
  } else {
    # SIMPLE MIXING: Direct RGB averaging (legacy mode)
    # Used when use_tinting = FALSE
    
    cols <- matrix(0, nrow = n, ncol = 3)
    
    for(i in seq_along(ids)) {
      rgb_val <- pigment_list[[ids[i]]]$properties$rgb
      if(is.null(rgb_val)) {
        cols[i, ] <- c(255, 255, 255)
      } else {
        cols[i, ] <- as.numeric(rgb_val)
      }
    }
    
    r <- sum(cols[, 1] * w)
    g <- sum(cols[, 2] * w)
    b <- sum(cols[, 3] * w)
  }
  
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

# Calculate preview colors for each recipe using same method as main preview
calculate_recipe_color <- function(recipe, use_tinting = TRUE) {
  base_id <- pigment_name_to_id[[recipe$pigment]]
  if(is.null(base_id) || !base_id %in% names(pigments_db)) return(c(200, 200, 200))
  
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
  mix_colors(ids, pcts, pigments_db, use_tinting = use_tinting)
}