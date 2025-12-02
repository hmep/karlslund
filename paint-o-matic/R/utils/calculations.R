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
