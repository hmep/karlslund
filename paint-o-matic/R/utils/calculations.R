# Calculation Utilities
# Generic recipe helper functions for paint calculations

# Calculate base oil absorption and density for pigment mix
calculate_base_properties <- function(m, compensated_pcts, zinc_ratio, km) {
  base_oil_absorption <- 0
  total_density <- 0
  
  for(i in seq_along(m$ids)) {
    id <- m$ids[i]
    weight_fraction <- compensated_pcts[i] / 100
    
    if(id == "vitbas") {
      base_oil_absorption <- base_oil_absorption + 
        weight_fraction * (zinc_ratio * 0.20 + (1-zinc_ratio) * 0.15)
      total_density <- total_density + 
        weight_fraction * (zinc_ratio * 5.6 + (1-zinc_ratio) * 4.2)
    } else {
      base_oil_absorption <- base_oil_absorption + 
        weight_fraction * (km[[id]]$oil / 100)
      total_density <- total_density + 
        weight_fraction * km[[id]]$density
    }
  }
  
  list(oil_absorption = base_oil_absorption, density = total_density)
}

# Calculate pigment amounts for target volume
calculate_pigment_amounts <- function(target_liters, oil_absorption, density) {
  V_pigment_per_gram <- 1 / density
  V_oil_per_gram_min <- oil_absorption / 0.92
  pvc_base <- V_pigment_per_gram / (V_pigment_per_gram + V_oil_per_gram_min)
  
  pigment_volume_L <- target_liters * pvc_base
  total_pigment_g <- pigment_volume_L * 1000 * density
  base_oil_g <- total_pigment_g * oil_absorption
  
  list(
    total_pigment_g = total_pigment_g,
    base_oil_g = base_oil_g,
    pvc_base = pvc_base,
    pigment_volume_L = pigment_volume_L
  )
}

# Distribute pigments according to compensated percentages
distribute_pigments <- function(m, compensated_pcts, total_pigment_g, zinc_ratio) {
  zn_g <- ti_g <- 0
  color_g <- numeric()
  
  for(i in seq_along(m$ids)){
    id <- m$ids[i]
    weight_fraction <- compensated_pcts[i] / 100
    weight_g <- total_pigment_g * weight_fraction
    
    if(id == "vitbas"){
      zn_g <- zn_g + weight_g * zinc_ratio
      ti_g <- ti_g + weight_g * (1-zinc_ratio)
    } else {
      color_g[id] <- weight_g
    }
  }
  
  list(zn = zn_g, ti = ti_g, color = color_g)
}

# Calculate average density for a pigment mix (used in volume calculations)
calculate_avg_density <- function(m, compensated_pcts, zinc_ratio, km) {
  total_density <- 0
  for(i in seq_along(m$ids)) {
    id <- m$ids[i]
    weight_fraction <- compensated_pcts[i] / 100
    
    if(id == "vitbas") {
      total_density <- total_density + 
        weight_fraction * (zinc_ratio * 5.6 + (1-zinc_ratio) * 4.2)
    } else {
      total_density <- total_density + weight_fraction * km[[id]]$density
    }
  }
  total_density
}

# Consolidated recipe calculator for all paint types
# Eliminates duplication between linseed, egg-oil, and tar recipes
calculate_recipe_generic <- function(paint_type = "linseed", 
                                     target_liters, 
                                     m, 
                                     zinc_ratio,
                                     extra_params = list()) {
  # Normalize and compensate
  normalized_pcts <- (m$pct / m$total) * 100
  compensated_pcts <- km_compensate_vitbas(normalized_pcts, m$ids, zinc_ratio)
  
  # Common calculations (all paint types)
  props <- calculate_base_properties(m, compensated_pcts, zinc_ratio, km)
  amounts <- calculate_pigment_amounts(target_liters, props$oil_absorption, props$density)
  pigments <- distribute_pigments(m, compensated_pcts, amounts$total_pigment_g, zinc_ratio)
  
  # Base result structure
  result <- list(
    zn = smart_round(pigments$zn), 
    ti = smart_round(pigments$ti), 
    color = sapply(pigments$color, smart_round)
  )
  
  # Paint-type-specific additions
  if (paint_type == "linseed") {
    extra_oil_factor <- extra_params$extra_oil %||% 1.6
    result$oil <- smart_round(amounts$base_oil_g * extra_oil_factor)
    
  } else if (paint_type == "egg_oil") {
    filler_id <- extra_params$filler_id %||% "58000"
    extra_filler_volume_L <- amounts$pigment_volume_L * 0.20
    extra_filler_g <- extra_filler_volume_L * 1000 * km[[filler_id]]$density
    
    linseed_oil_g <- amounts$base_oil_g * 0.5
    eggs_g <- amounts$base_oil_g * 0.5
    water_g <- amounts$base_oil_g
    
    result$filler_id <- filler_id
    result$filler_g <- smart_round(extra_filler_g)
    result$oil <- smart_round(linseed_oil_g)
    result$eggs <- smart_round(eggs_g)
    result$eggs_count <- round(eggs_g / 50, 1)
    result$water <- smart_round(water_g)
    
  } else if (paint_type == "tar") {
    tar_extra_oil_factor <- extra_params$tar_extra_oil %||% 1.6
    total_oil_with_factor <- amounts$base_oil_g * tar_extra_oil_factor
    
    tar_g <- total_oil_with_factor * 0.5
    linseed_oil_g <- total_oil_with_factor * 0.5 * 1.2
    balsamterpentin_g <- tar_g
    
    result$tar_id <- extra_params$tar_id
    result$tar <- smart_round(tar_g)
    result$oil <- smart_round(linseed_oil_g)
    result$balsamterpentin <- smart_round(balsamterpentin_g)
  }
  
  result$hex <- extra_params$hex %||% "#FFFFFF"
  return(result)
}
