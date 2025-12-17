# Kubelka-Munk Compensation Utilities
# Functions for compensating vitbas (white base) based on zinc/titanium ratio

# Kubelka-Munk compensation for vitbas with different zinc/titanium ratios
km_compensate_vitbas <- function(normalized_pcts, ids, zinc_ratio) {
  # Only compensate if vitbas is present
  if (!("vitbas" %in% ids)) return(normalized_pcts)
  
  # K and S values for whites
  K_zinc <- 0.03 #0.00
  S_zinc <- 1.66
  K_titanium <- 0.03 #0.00
  S_titanium <- 2.2 #2.55
  
  # REFERENCE POINT: 25% zinc baseline
  zinc_ratio_ref <- 0.25
  S_vitbas_ref <- zinc_ratio_ref * S_zinc + (1 - zinc_ratio_ref) * S_titanium
  S_vitbas_current <- zinc_ratio * S_zinc + (1 - zinc_ratio) * S_titanium
  
  # Find vitbas index (vectorized)
  vitbas_idx <- which(ids == "vitbas")[1]
  if(is.na(vitbas_idx) || length(ids) == 1) return(normalized_pcts)
  
  # Split into vitbas and colored pigments
  vitbas_pct <- normalized_pcts[vitbas_idx]
  colored_mask <- seq_along(ids) != vitbas_idx
  colored_pcts <- normalized_pcts[colored_mask]
  colored_ids <- ids[colored_mask]
  
  c_vitbas <- vitbas_pct / 100
  c_colored <- colored_pcts / 100
  
  # VECTORIZED: Calculate K and S for all colored pigments at once
  K_vals <- sapply(colored_ids, function(id) pigments_db[[id]]$properties$K)
  S_vals <- sapply(colored_ids, function(id) pigments_db[[id]]$properties$S)
  
  K_colored <- sum(c_colored * K_vals)
  S_colored <- sum(c_colored * S_vals)
  
  # Calculate target K/S ratio
  K_mix_ref <- K_colored  # Vitbas contributes K=0
  S_mix_ref <- c_vitbas * S_vitbas_ref + S_colored
  
  if(S_mix_ref <= 0) return(normalized_pcts)
  
  KS_ratio_target <- K_mix_ref / S_mix_ref
  
  # Calculate alpha scaling factor
  denominator <- K_colored - KS_ratio_target * S_colored
  if(abs(denominator) < 1e-10) return(normalized_pcts)
  
  alpha <- (KS_ratio_target * c_vitbas * S_vitbas_current) / denominator
  
  # Safety check
  if(alpha < 0.3 || alpha > 3.0) return(normalized_pcts)
  
  # Scale colored pigments
  colored_pcts_compensated <- colored_pcts * alpha
  vitbas_pct_compensated <- 100 - sum(colored_pcts_compensated)
  
  # Validate result
  if(vitbas_pct_compensated < 0 || vitbas_pct_compensated > 100) return(normalized_pcts)
  
  # Reconstruct result (vectorized assignment)
  compensated_pcts <- normalized_pcts
  compensated_pcts[vitbas_idx] <- vitbas_pct_compensated
  compensated_pcts[colored_mask] <- colored_pcts_compensated
  
  return(compensated_pcts)
}
