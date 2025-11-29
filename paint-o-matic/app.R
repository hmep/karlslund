# Paint-o-matic -- blanda din egen linoljefärg!
# Copyright 2025 Tobias Hagberg
# Licens: GNU General Public License v3.0
# https://github.com/hmep/karlslund/tree/main/paint-o-matic

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

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

# Helper functions for Swedish number formatting
format_swe <- function(x, digits = 1) {
  if(is.null(x) || is.na(x)) return("0")
  
  # If value is whole number (or very close to it), omit decimal
  if(abs(x - round(x, 0)) < 0.01) {
    formatted <- format(round(x, 0), 
                        decimal.mark = ",", 
                        big.mark = " ",
                        trim = TRUE)
  } else {
    formatted <- format(round(x, digits), 
                        nsmall = digits, 
                        decimal.mark = ",", 
                        big.mark = " ",
                        trim = TRUE)
  }
  return(formatted)
}

# Locale-independent numeric parser (handles both dots and commas from any source)
parse_numeric <- function(x, default = NA) {
  if(is.null(x) || length(x) == 0) return(default)
  if(is.numeric(x)) return(x)
  
  # Convert to character and replace comma with dot
  x_char <- as.character(x)
  x_char <- gsub(",", ".", x_char)
  x_char <- gsub(" ", "", x_char)  # Remove spaces (thousand separators)
  
  result <- suppressWarnings(as.numeric(x_char))
  if(is.na(result)) return(default)
  return(result)
}

# Smart rounding based on weight - improves readability and practicality
smart_round <- function(weight) {
  if(weight < 10) {
    # Small amounts need precision (e.g., 3.5g, 8.2g)
    return(round(weight, 1))
  } else if(weight < 100) {
    # Medium amounts: whole grams (e.g., 45g, 87g)
    return(round(weight, 0))
  } else if(weight < 500) {
    # Large amounts: round to 5g (e.g., 235g, 340g)
    return(round(weight / 5) * 5)
  } else {
    # Very large amounts: round to 10g (e.g., 780g, 1250g)
    return(round(weight / 10) * 10)
  }
}

# === GENERIC RECIPE HELPER FUNCTIONS ===

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

# === HELPER FUNCTIONS ===
# Safe input retrieval with validation
safe_input <- function(input, name, default, test = function(x) TRUE) {
  val <- input[[name]]
  if(isTRUE(!is.null(val) && !is.na(val) && test(val))) 
    as.numeric(val) 
  else 
    default
}

# Null-coalescing operator
`%||%` <- function(a, b) if(is.null(a)) b else a

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

# Generate sharing URL from current recipe parameters
generate_share_url <- function(session, input = NULL, mix_data = NULL) {
  params <- list()
  
  # If mix_data provided (from mix() reactive), use it
  # Otherwise use input directly (for real-time updates)
  if(!is.null(mix_data)) {
    # From mix() - use for exports
    if(length(mix_data$ids) > 0) {
      for(i in 1:min(4, length(mix_data$ids))) {
        params[[paste0("p", i)]] <- mix_data$ids[i]
        params[[paste0("pct", i)]] <- mix_data$pct[i]
      }
    }
  } else if(!is.null(input)) {
    # From input - use for live updates
    if(isTRUE(!is.null(input$p1) && input$p1 != "")) {
      params$p1 <- input$p1
      params$pct1 <- input$pct1
    }
    if(isTRUE(!is.null(input$p2) && input$p2 != "")) {
      params$p2 <- input$p2
      params$pct2 <- input$pct2
    }
    if(isTRUE(!is.null(input$p3) && input$p3 != "")) {
      params$p3 <- input$p3
      params$pct3 <- input$pct3
    }
    if(isTRUE(!is.null(input$p4) && input$p4 != "")) {
      params$p4 <- input$p4
      params$pct4 <- input$pct4
    }
  }
  
  # Add other parameters (only if not default values)
  if(!is.null(input)) {
    if(isTRUE(!is.null(input$area) && !is.na(input$area) && input$area != 20)) 
      params$area <- input$area
    if(isTRUE(!is.null(input$substrate) && input$substrate != 1.0)) 
      params$substrate <- input$substrate
    if(isTRUE(!is.null(input$zinc_ratio) && !is.na(input$zinc_ratio) && input$zinc_ratio != 15)) 
      params$zinc_ratio <- input$zinc_ratio
    if(isTRUE(!is.null(input$extra_oil) && !is.na(input$extra_oil) && input$extra_oil != 1.8)) 
      params$extra_oil <- input$extra_oil
    if(isTRUE(!is.null(input$use) && input$use != 3)) 
      params$use <- input$use
    
    # Add paint type (only if not default linseed)
    if(isTRUE(!is.null(input$paint_type) && input$paint_type != "linseed")) {
      params$paint_type <- input$paint_type
      
      # Add paint-type-specific parameters
      if(input$paint_type == "egg_oil" && !is.null(input$egg_filler)) {
        params$egg_filler <- input$egg_filler
      }
      if(input$paint_type == "tar") {
        if(!is.null(input$tar_category)) params$tar_category <- input$tar_category
        if(!is.null(input$tar_extra_oil) && input$tar_extra_oil != 1.6) params$tar_extra_oil <- input$tar_extra_oil
      }
    }
  }
  
  # Build URL if we have parameters
  if(length(params) == 0) return(NULL)
  
  # Build base URL
  base_url <- session$clientData$url_protocol
  base_url <- paste0(base_url, "//", session$clientData$url_hostname)
  if(!is.null(session$clientData$url_port) && session$clientData$url_port != "") {
    base_url <- paste0(base_url, ":", session$clientData$url_port)
  }
  base_url <- paste0(base_url, session$clientData$url_pathname)
  
  # Build query string
  query_parts <- sapply(names(params), function(key) {
    paste0(key, "=", URLencode(as.character(params[[key]]), reserved = TRUE))
  })
  query_string <- paste(query_parts, collapse = "&")
  
  paste0(base_url, "?", query_string)
}

# === PIGMENTDATABAS ===
# Extended with RAÄ Kulturkulör pigments
# K and S values estimated based on pigment type and characteristics
# Oil absorption values from Kremer datablad and industry standards

km <- list(
  # BASE WHITES
  "vitbas" = list(name = "Vitbas (K-M-kompenserad titan/zink-blandning)", oil = 17, K = 0.00, S = 2.20, density = 4.2, rgb = c(245, 245, 245)),
  #"44100" = list(name = "Zinkvitt PW4", oil = 20, K = 0.00, S = 1.66, density = 5.6),
  #"44400" = list(name = "Titanvitt Rutile PW6", oil = 15, K = 0.00, S = 2.55, density = 4.2),
  
  # === FILLERS ===
  "599930" = list(name = "Kiselgur (diatoméjord)", oil = 70, K = 0.00, S = 0.05, density = 2.2, rgb = c(250, 248, 245)),
  "58000"  = list(name = "Krita från Champagne", oil = 12, K = 0.00, S = 0.10, density = 2.7, rgb = c(255, 255, 255)),
  "58010"  = list(name = "Krita från Ruegen", oil = 14, K = 0.00, S = 0.12, density = 2.7, rgb = c(248, 248, 246)),
  "58162"  = list(name = "Stenkrita vit", oil = 10, K = 0.00, S = 0.15, density = 2.7, rgb = c(255, 255, 255)),
  "58900"  = list(name = "Bentonit", oil = 180, K = 0.00, S = 0.08, density = 2.5, rgb = c(235, 232, 220)),
  "58250"  = list(name = "Kaolin gulaktig", oil = 45, K = 0.00, S = 0.20, density = 2.6, rgb = c(245, 242, 230)),
  
  # GREENS
  "40400" = list(name = "Viridian PG18", oil = 40, K = 1.20, S = 1.50, density = 3.5, rgb = c(30, 120, 80)),
  "41700" = list(name = "Malakit naturlig", oil = 45, K = 0.90, S = 0.80, density = 4.0, rgb = c(70, 160, 100)),
  "11100" = list(name = "Phthalogrön PG7", oil = 50, K = 1.50, S = 1.40, density = 2.0, rgb = c(0, 100, 50)),
  "KG83" = list(name = "Kromoxidgrönt nr GN 83 (RAÄ)", oil = 18, K = 1.15, S = 1.75, density = 5.2, rgb = c(74, 117, 82)),
  "ZG65" = list(name = "Zinkgrönt nr 65 (RAÄ)", oil = 19, K = 1.00, S = 1.60, density = 4.8, rgb = c(110, 145, 105)),
  "40850" = list(name = "Grön jord Böhmen", oil = 35, K = 0.60, S = 0.55, density = 3.2, rgb = c(90, 120, 70)),
  "40860" = list(name = "Grön jord Verona", oil = 35, K = 0.65, S = 0.60, density = 3.2, rgb = c(100, 130, 80)),
  "GU30" = list(name = "Grön umbra nr 30 (RAÄ)", oil = 50, K = 0.85, S = 0.48, density = 3.5, rgb = c(95, 100, 70)),
  
  # BLACKS
  "44450" = list(name = "Svartoxid PBk11", oil = 15, K = 2.40, S = 1.10, density = 5.21, rgb = c(28, 38, 38)),
  "J318" = list(name = "Järnoxidsvart nr 318 (RAÄ)", oil = 16, K = 2.35, S = 1.08, density = 5.1, rgb = c(35, 35, 38)),
  "BS98" = list(name = "Bensvart nr 98 (RAÄ)", oil = 50, K = 2.60, S = 0.95, density = 2.0, rgb = c(28, 28, 32)),
  "47501" = list(name = "Mangansvart", oil = 22, K = 2.50, S = 1.15, density = 4.8, rgb = c(32, 32, 32)),
  "47400" = list(name = "Spinel-svart", oil = 33, K = 2.80, S = 1.25, density = 4.5, rgb = c(20, 20, 20)),
  
  # BLUES
  "11670" = list(name = "Phthaloblå PB15:3", oil = 45, K = 1.80, S = 1.20, density = 2.0, rgb = c(0, 70, 130)),
  "UB88" = list(name = "Ultramarinblått nr 88 (RAÄ)", oil = 42, K = 1.65, S = 0.88, density = 2.4, rgb = c(45, 60, 130)),
  "KB28" = list(name = "Koboltblått nr 28 (RAÄ)", oil = 35, K = 1.40, S = 0.92, density = 4.0, rgb = c(70, 95, 155)),
  
  # EARTH COLORS - TERRA & POZZUOLI
  "40820" = list(name = "Terra di Pozzuoli", oil = 40, K = 0.70, S = 0.55, density = 3.3, rgb = c(180, 80, 60)),
  "40800" = list(name = "Terra di Siena natur", oil = 40, K = 0.60, S = 0.50, density = 3.3, rgb = c(170, 110, 70)),
  "40830" = list(name = "Terra di Ercolano", oil = 40, K = 0.75, S = 0.55, density = 3.3, rgb = c(175, 85, 65)),
  "BT44" = list(name = "Bränd terra nr 44 (RAÄ)", oil = 38, K = 0.78, S = 0.52, density = 3.4, rgb = c(170, 110, 70)),
  "OT46" = list(name = "Obränd terra nr 46 (RAÄ)", oil = 38, K = 0.62, S = 0.48, density = 3.3, rgb = c(180, 130, 80)),
  
  # YELLOWS & OCHRES
  "44082" = list(name = "Gul ockra ljus", oil = 20, K = 0.48, S = 0.38, density = 3.5, rgb = c(210, 180, 120)),
  "44086" = list(name = "Gul ockra mörk", oil = 20, K = 0.55, S = 0.45, density = 3.5, rgb = c(160, 120, 70)),
  "44150" = list(name = "Naples Yellow light", oil = 35, K = 0.40, S = 0.70, density = 6.0, rgb = c(240, 220, 130)),
  "44160" = list(name = "Naples Yellow dark", oil = 35, K = 0.50, S = 0.65, density = 6.0, rgb = c(220, 190, 100)),
  "J920" = list(name = "Järnoxidgult nr 920 (RAÄ)", oil = 22, K = 0.52, S = 0.42, density = 4.0, rgb = c(195, 165, 85)),
  "LO92" = list(name = "Ljusockra nr 92 (RAÄ)", oil = 21, K = 0.46, S = 0.40, density = 3.5, rgb = c(210, 185, 135)),
  "GO94" = list(name = "Guldockra nr 94 (RAÄ)", oil = 23, K = 0.58, S = 0.46, density = 3.6, rgb = c(185, 155, 90)),
  "GO94_GU30" = list(name = "50% Guldockra + 50% Grön umbra (RAÄ)", oil = 40, K = 0.72, S = 0.47, density = 3.5, rgb = c(135, 130, 85)),
  
  # SIENNAS & UMBERS
  "44650" = list(name = "Raw Sienna Italien", oil = 45, K = 0.55, S = 0.45, density = 3.3, rgb = c(180, 130, 70)),
  "44620" = list(name = "Burnt Sienna Italien", oil = 50, K = 0.75, S = 0.50, density = 3.5, rgb = c(160, 82, 45)),
  "OU103" = list(name = "Obränd umbra nr 103 (RAÄ)", oil = 52, K = 0.92, S = 0.46, density = 3.4, rgb = c(115, 95, 80)),
  "BU100" = list(name = "Bränd umbra nr 100 (RAÄ)", oil = 56, K = 1.12, S = 0.52, density = 3.5, rgb = c(90, 60, 45)),
  "BRU39" = list(name = "Brun umbra nr 39 (RAÄ)", oil = 54, K = 1.05, S = 0.48, density = 3.4, rgb = c(105, 85, 70)),
  "GRAU36" = list(name = "Grå umbra nr 36 (RAÄ)", oil = 48, K = 1.20, S = 0.55, density = 3.5, rgb = c(100, 95, 90)),
  
  # REDS & ORANGES - IRON OXIDES
  "44300" = list(name = "Transparent brunoxid", oil = 50, K = 0.80, S = 0.22, density = 5.0, rgb = c(139, 69, 19)),
  "44200" = list(name = "Röd järnoxid transparent", oil = 47, K = 0.90, S = 0.12, density = 5.2, rgb = c(178, 34, 34)),
  "44210" = list(name = "Röd järnoxid ljus", oil = 47, K = 0.80, S = 0.25, density = 5.1, rgb = c(200, 70, 60)),
  "44220" = list(name = "Röd järnoxid mörk", oil = 47, K = 1.00, S = 0.35, density = 5.2, rgb = c(160, 35, 35)),
  "44510" = list(name = "Orange järnoxid", oil = 47, K = 0.55, S = 0.85, density = 4.8, rgb = c(232, 97, 0)),
  "J225" = list(name = "Järnoxidrött nr 225 (RAÄ)", oil = 48, K = 0.95, S = 0.32, density = 5.1, rgb = c(142, 52, 52)),
  "J180M" = list(name = "Järnoxidrött nr 180M Caput Mortuum (RAÄ)", oil = 48, K = 1.15, S = 0.28, density = 5.2, rgb = c(105, 45, 55)),
  "J120N" = list(name = "Järnoxidrött nr 120N (RAÄ)", oil = 47, K = 0.85, S = 0.30, density = 5.0, rgb = c(155, 65, 60)),
  "ER48A" = list(name = "Engelskt rött nr 48A (RAÄ)", oil = 30, K = 0.75, S = 0.40, density = 4.9, rgb = c(175, 80, 70)),
  
  # BROWNS - IRON OXIDES
  "J663" = list(name = "Järnoxidbrunt nr 663 (RAÄ)", oil = 50, K = 0.88, S = 0.38, density = 5.0, rgb = c(120, 80, 60)),
  "J686" = list(name = "Järnoxidbrunt nr 686 (RAÄ)", oil = 52, K = 0.92, S = 0.35, density = 5.1, rgb = c(105, 70, 55)),
  "48330" = list(name = "Järnmanganbrunt 645 T", oil = 50, K = 0.90, S = 0.40, density = 4.8, rgb = c(95, 65, 45))
  
)

# RAÄ KULTURKULÖR PIGMENTS
# Updated to include all RAÄ pigments with harmonized keys and NCS-based RGB values
raa_pigments <- c(
  # Base whites (always included)
  "vitbas", #"44100", "44400",
  
  # RAÄ specific pigments (from Kulturkulör system with NCS codes)
  "J225",       # Järnoxidrött nr 225
  "J180M",      # Caput Mortuum 180M
  "J120N",      # Järnoxidrött nr 120 N
  "ER48A",      # Engelskt rött nr 48 A
  "J663",       # Järnoxidbrunt nr 663
  "J686",       # Järnoxidbrunt nr 686
  "J920",       # Järnoxidgult nr 920
  "J318",       # Järnoxidsvart nr 318
  "LO92",       # Ljusockra nr 92
  "GO94",       # Guldockra nr 94
  "GO94_GU30",  # 50% Guldockra + 50% Grön umbra
  "OU103",      # Obränd Umbra nr 103
  "BU100",      # Bränd Umbra nr 100
  "BRU39",      # Brun Umbra nr 39
  "GU30",       # Grön Umbra nr 30
  "GRAU36",     # Grå Umbra nr 36
  "BT44",       # Bränd Terra nr 44
  "OT46",       # Obränd Terra nr 46
  "BS98",       # Bensvart nr 98
  "KG83",       # Kromoxidgrönt nr GN 83
  "ZG65",       # Zinkgrönt nr 65
  "UB88",       # Ultramarinblått nr 88
  "KB28"        # Koboltblått nr 28
)

# CONSOLIDATED SUPPLIER LINKS (Kremer + RAÄ matches)
suppliers <- list(
  # WHITES
  "44100" = list(
    name = "Zinkvitt PW4",
    kremer_match = "Zinc White (PW4)",
    kremer_id = "46300",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/46300-zinc-white",
    ottosson_match = "Zinkvitt",
    ottosson_url = "https://ottossonfarg.com/produkt/zinkvitt/",
    notes = "Högkvalitativt zinkvitt för linoljefärg. Finns hos både Kremer och svenska leverantörer."
  ),
  
  "44400" = list(
    name = "Titanvitt Rutile PW6",
    kremer_match = "Titanium White Rutile (PW6)",
    kremer_id = "46200",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/46200-titanium-white-rutile",
    ottosson_match = "Titanvitt",
    ottosson_url = "https://ottossonfarg.com/produkt/titanvitt/",
    notes = "Rutiltyp titanvitt med högsta täckförmåga. Standard vitpigment för linoljefärg."
  ),
  
  # FILLERS
  "599930" = list(
    name = "Tripoli, Rotten Stone, light",
    kremer_match = "Tripoli, Rotten Stone, light",
    kremer_id = "599930",
    kremer_url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/599930-tripoli-rotten-stone-light.html",
    notes = "Kiselgur (diatoméjord), mycket fin poleringsfyllnad. Mycket hög oljeabsorption. Används för fin polering och som mattande tillsats."
  ),
  
  "58000" = list(
    name = "Chalk from Champagne",
    kremer_match = "Chalk from Champagne",
    kremer_id = "58000",
    kremer_url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58000-chalk-from-champagne.html",
    notes = "Naturlig kalciumkarbonat från Frankrike (CaCO3). Används för grundningar, stuckatur och som fyllmedel i färg. Färgindex: PW 18.77220. Låg oljeabsorption."
  ),
  
  "58010" = list(
    name = "Chalk from Ruegen",
    kremer_match = "Chalk from Ruegen",
    kremer_id = "58010",
    kremer_url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58010-chalk-from-ruegen.html",
    notes = "Naturlig kalciumkarbonat från Tyskland, ca 40 µ. Något grövre och mer gråaktig än Champagnekrita. Färgindex: PW 18.77220. Används i väggfärg och grundningar."
  ),
  
  "58162" = list(
    name = "Stone Chalk, white",
    kremer_match = "Stone Chalk, white",
    kremer_id = "58162",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/fillers-building-materials/58162-stone-chalk-white.html",
    notes = "Mycket fin stenkrita, ca 4 µ. Finaste kvalitet krita för högkvalitativa applikationer. Lägst oljeabsorption av alla kritor."
  ),
  
  "58900" = list(
    name = "Bentonite",
    kremer_match = "Bentonite",
    kremer_id = "58900",
    kremer_url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58900-bentonite.html",
    notes = "Förtjockningsmedel, särskilt för oljefärg. Mycket hög oljeabsorption (180%). Lera som sväller i kontakt med olja. Färgindex: PW 19.77004. Används sparsamt (1-5%)."
  ),
  
  "58250" = list(
    name = "Kaolin, yellowish",
    kremer_match = "Kaolin, yellowish",
    kremer_id = "58250",
    kremer_url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58250-kaolin-yellowish.html",
    notes = "Vit bolus, gulaktig kaolin-lera. Används som fyllmedel och för att öka opacitet. Färgindex: PW 19. Måttlig oljeabsorption (45%)."
  ),
  
  # GREENS
  "40400" = list(
    name = "Viridian PG18",
    kremer_match = "Viridian Green (PG18)",
    kremer_id = "44250",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/44250-viridian-green",
    notes = "Klassisk viridiangrön, kall transparent grön. Främst från internationella leverantörer."
  ),
  
  "41700" = list(
    name = "Malakit naturlig",
    kremer_match = "Malachite Synthetic (PG19)",
    kremer_id = "44400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/44400-malachite-synthetic",
    notes = "Syntetisk malakit som alternativ till naturlig. Speciellt pigment från Kremer."
  ),
  
  "11100" = list(
    name = "Phthalogrön PG7",
    kremer_match = "Phthalo Green Bluish (PG7)",
    kremer_id = "23000",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/23000-phthalo-green-bluish-pg-7",
    notes = "Stark blåaktig phthalogrön med hög färgstyrka. Organiskt pigment."
  ),
  
  "40850" = list(
    name = "Grön jord Böhmen",
    kremer_match = "Green Earth Bohemian (PG23)",
    kremer_id = "40850",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40850-green-earth-bohemian",
    notes = "EXAKT MATCHNING - Samma produktnummer! Böhmisk grön jord från Kremer."
  ),
  
  "40860" = list(
    name = "Grön jord Verona",
    kremer_match = "Green Earth Verona (PG23)",
    kremer_id = "40860",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40860-green-earth-verona",
    notes = "EXAKT MATCHNING - Samma produktnummer! Veronese grön jord från Kremer."
  ),
  
  # BLACKS
  "44450" = list(
    name = "Svartoxid PBk11",
    kremer_match = "Black Iron Oxide (PBk11)",
    kremer_id = "47000",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/47000-black-iron-oxide",
    ottosson_match = "Järnoxidsvart",
    ottosson_url = "https://ottossonfarg.com/produkt/jarnoxidsvart/",
    claessons_match = "Järnoxidsvart 9313",
    claessons_url = "https://claessons.com/svarta/jarnoxidsvart-9313-losvikt/",
    notes = "Järnoxidsvart med hög täckförmåga. Finns hos Kremer, Ottosson och Claessons."
  ),
  
  "47400" = list(
    name = "Spinel Black",
    kremer_match = "Spinel Black",
    kremer_id = "47400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/47400-spinel-black.html",
    notes = "Enda 'sanna' svarta - jämnt icke-reflekterande över hela spektrumet. Djupaste svarta pigmentet tillgängligt (förutom Vanta Black). Järn-mangan spinell (Fe,Mn)₃O₄. Färgindex: PBk 26.77494. Utmärkt ljusäkthet (8/8/8). Värmebeständig >500°C. Mycket fin partikelstorlek (~0.5 µm). Kräver hög oljeabsorption (65-70%). Säker att använda. Premium kvalitet."
  ),
  
  # BLUES
  "11670" = list(
    name = "Phthaloblå PB15:3",
    kremer_match = "Phthalo Blue Royal Blue (PB15:3)",
    kremer_id = "23060",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/23060-phthalo-blue-royal-blue-pb-15-3",
    notes = "Royal blue variant av phthaloblått med hög färgstyrka. Organiskt pigment."
  ),
  
  # EARTH COLORS - TERRA & POZZUOLI
  "40820" = list(
    name = "Terra di Pozzuoli",
    kremer_match = "Terra Pozzuoli (PY43)",
    kremer_id = "41550",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/41550-terra-pozzuoli",
    notes = "Italiensk vulkanisk jord från Pozzuoli. Speciellt pigment från Kremer."
  ),
  
  "40800" = list(
    name = "Terra di Siena natur",
    kremer_match = "Raw Sienna Italian (PBr7)",
    kremer_id = "40400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian",
    notes = "Klassisk obränd sienna från Italien. Naturligt jordpigment."
  ),
  
  "40830" = list(
    name = "Terra di Ercolano",
    kremer_match = "Terra di Ercolano (PBr7)",
    kremer_id = "40835",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40835-terra-di-ercolano",
    notes = "EXAKT MATCHNING - Terra från Herculaneum. Italienskt specialpigment."
  ),
  
  # YELLOWS & OCHRES
  "44082" = list(
    name = "Gul ockra ljus",
    kremer_match = "Yellow Ochre Light (PY43)",
    kremer_id = "40010",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40010-yellow-ochre-light",
    ottosson_match = "Gul ockra",
    ottosson_url = "https://ottossonfarg.com/produkt/gul-ockra/",
    notes = "Ljus gul ockra, ett av de mest använda pigmenten i svensk tradition."
  ),
  
  "44086" = list(
    name = "Gul ockra mörk",
    kremer_match = "Yellow Ochre Dark (PY43)",
    kremer_id = "40030",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40030-yellow-ochre-dark",
    notes = "Mörk gul ockra med högre färgstyrka än ljus variant."
  ),
  
  "44150" = list(
    name = "Naples Yellow light",
    kremer_match = "Naples Yellow Light (PY41/PW4)",
    kremer_id = "43010",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/43010-naples-yellow-light",
    notes = "Ljus neapelgul, blybaserad variant. Historiskt pigment."
  ),
  
  "44160" = list(
    name = "Naples Yellow dark",
    kremer_match = "Naples Yellow Dark (PY41/PW4)",
    kremer_id = "43000",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/43000-naples-yellow-dark",
    notes = "Mörk neapelgul, blybaserad variant. Historiskt pigment."
  ),
  
  # SIENNAS
  "44650" = list(
    name = "Raw Sienna Italien",
    kremer_match = "Raw Sienna Italian (PBr7)",
    kremer_id = "40400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian",
    notes = "Klassisk obränd sienna från Italien. Naturligt jordpigment."
  ),
  
  "44620" = list(
    name = "Burnt Sienna Italien",
    kremer_match = "Burnt Sienna Italian (PR101)",
    kremer_id = "40450",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40450-burnt-sienna-italian",
    ottosson_match = "Järnoxidrött bränd",
    ottosson_url = "https://ottossonfarg.com/produkt/jarnoxidrott-brand/",
    notes = "Bränd sienna med varm rödbrun nyans. Liknande bränt järnoxid."
  ),
  
  # REDS & ORANGES - IRON OXIDES
  "44300" = list(
    name = "Transparent brunoxid",
    kremer_match = "Transparent Brown Oxide (PBr7)",
    kremer_id = "48000",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48000-transparent-brown-oxide",
    notes = "Transparent järnoxid för lasering och transparenta skikt."
  ),
  
  "44200" = list(
    name = "Röd järnoxid transparent",
    kremer_match = "Red Iron Oxide Transparent (PR101)",
    kremer_id = "48100",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48100-red-iron-oxide-transparent",
    notes = "Transparent röd järnoxid för lasering och transparenta skikt."
  ),
  
  "44210" = list(
    name = "Röd järnoxid ljus",
    kremer_match = "Red Iron Oxide Light (PR101)",
    kremer_id = "48200",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48200-red-iron-oxide-light",
    ottosson_match = "Järnoxidrött ljus",
    ottosson_url = "https://ottossonfarg.com/produkt/jarnoxidrott-ljus/",
    claessons_match = "Järnoxidrött 9509",
    claessons_url = "https://claessons.com/roda/jarnoxidrott-9509/",
    notes = "Ljus röd järnoxid med god täckförmåga. Klassisk svensk rödfärgsnyans."
  ),
  
  "44220" = list(
    name = "Röd järnoxid mörk",
    kremer_match = "Red Iron Oxide Dark (PR101)",
    kremer_id = "48300",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48300-red-iron-oxide-dark",
    ottosson_match = "Järnoxidrött mörk",
    ottosson_url = "https://ottossonfarg.com/produkt/jarnoxidrott-mork/",
    notes = "Mörk röd järnoxid med hög färgstyrka och täckförmåga."
  ),
  
  "44510" = list(
    name = "Orange järnoxid",
    kremer_match = "Orange Iron Oxide (PO20)",
    kremer_id = "48500",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48500-orange-iron-oxide",
    notes = "Orange järnoxid för varma toner mellan gult och rött."
  ),
  
  # === RAÄ PIGMENTS ===
  # GREENS
  "KG83" = list(
    name = "Kromoxidgrönt nr GN 83 (RAÄ)",
    kremer_match = "Chrome Oxide Green (PG17)",
    kremer_id = "44200",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/44200-chrome-oxide-green",
    notes = "Kall grön, opak, samma pigmenttyp (PG17). MYCKET GOD MATCHNING - samma krompigment som RAÄ använder."
  ),
  
  "ZG65" = list(
    name = "Zinkgrönt nr 65 (RAÄ)",
    kremer_match = "Cobalt Green Dark (PG19) or Cobalt Zinc Silicate",
    kremer_id = "44350",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/44350-cobalt-green-dark",
    notes = "Zinkbaserad grön, närmaste matchning för traditionell zinkgrön"
  ),
  
  "GU30" = list(
    name = "Grön umbra nr 30 (RAÄ)",
    kremer_match = "Raw Umber, greenish (PBr8)",
    kremer_id = "40630",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40630-raw-umber-greenish",
    notes = "Tysk obränd umbra med grönaktig nyans"
  ),
  
  # BLACKS
  "J318" = list(
    name = "Järnoxidsvart nr 318 (RAÄ)",
    kremer_match = "Iron Oxide Black 318 (PBk11)",
    kremer_id = "48400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48400-iron-oxide-black-318-high-tinting",
    ottosson_match = "Järnoxidsvart",
    ottosson_url = "https://ottossonfarg.com/produkt/jarnoxidsvart/",
    claessons_match = "Järnoxidsvart CM-5D",
    claessons_url = "https://claessons.com/svarta/jarnoxidsvart-cm-5d-1-kg/",
    notes = "EXAKT MATCHNING - Samma produktnummer 318! Finns hos Kremer, Ottosson och Claessons."
  ),
  
  "BS98" = list(
    name = "Bensvart nr 98 (RAÄ)",
    kremer_match = "Bone Black (PBk9)",
    kremer_id = "47100",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/47100-bone-black",
    ottosson_match = "Bensvart",
    ottosson_url = "https://ottossonfarg.com/produkt/bensvart/",
    notes = "Traditionell bensvart från ben. MYCKET GOD MATCHNING - finns hos både Kremer och svenska leverantörer."
  ),
  
  # BLUES
  "UB88" = list(
    name = "Ultramarinblått nr 88 (RAÄ)",
    kremer_match = "Ultramarine Blue, very dark (PB29)",
    kremer_id = "45000",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/45000-ultramarine-blue-very-dark",
    notes = "Djupt ultramarin, matchar mörkt NCS-värde. MYCKET GOD MATCHNING - samma pigment (PB29)."
  ),
  
  "KB28" = list(
    name = "Koboltblått nr 28 (RAÄ)",
    kremer_match = "Cobalt Blue Medium (PB28)",
    kremer_id = "45710",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/45710-cobalt-blue-medium",
    notes = "Mellannyans koboltblått med violett underton"
  ),
  
  # TERRA & EARTH COLORS
  "BT44" = list(
    name = "Bränd terra nr 44 (RAÄ)",
    kremer_match = "Burnt Sienna, Italian (PR101)",
    kremer_id = "44620",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40450-burnt-sienna-italian",
    notes = "Bränd röd jord, liknande bränd terra"
  ),
  
  "OT46" = list(
    name = "Obränd terra nr 46 (RAÄ)",
    kremer_match = "Raw Sienna, Italian (PY43)",
    kremer_id = "40400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian",
    notes = "Naturlig gulbrun jord"
  ),
  
  # YELLOWS & OCHRES
  "J920" = list(
    name = "Järnoxidgult nr 920 (RAÄ)",
    kremer_match = "Yellow Ochre, dark (PY42/43)",
    kremer_id = "40030",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40030-yellow-ochre-dark",
    notes = "Mörkare gulockra med god mättnad"
  ),
  
  "LO92" = list(
    name = "Ljusockra nr 92 (RAÄ)",
    kremer_match = "Yellow Ochre, light (PY42/43)",
    kremer_id = "40010",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40010-yellow-ochre-light",
    notes = "Ljus gulockra"
  ),
  
  "GO94" = list(
    name = "Guldockra nr 94 (RAÄ)",
    kremer_match = "Yellow Ochre Golden, Italian (PY43)",
    kremer_id = "40015",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40015-yellow-ochre-golden-italian",
    ottosson_match = "Guldockra",
    ottosson_url = "https://ottossonfarg.com/produkt/guldockra/",
    notes = "Guldtonad ockra, varmare än ljusockra. Klassiskt svensk pigment."
  ),
  
  "GO94_GU30" = list(
    name = "50% Guldockra + 50% Grön umbra (RAÄ)",
    kremer_match = "Mix Yellow Ochre Golden + Raw Umber greenish",
    kremer_id = "40015 + 40630",
    kremer_url = c(
      "https://www.kremer-pigmente.com/en/shop/pigments/40015-yellow-ochre-golden-italian",
      "https://www.kremer-pigmente.com/en/shop/pigments/40630-raw-umber-greenish"
    ),
    notes = "Specialblandning: 50/50 viktprocent av båda pigmenten"
  ),
  
  # UMBERS
  "OU103" = list(
    name = "Obränd umbra nr 103 (RAÄ)",
    kremer_match = "Raw Umber, Cyprus (PBr8)",
    kremer_id = "40610",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40610-raw-umber",
    notes = "Traditionell cypriotisk obränd umbra, mörkbrun-grön. MYCKET GOD MATCHNING - samma pigment (PBr8)."
  ),
  
  "BU100" = list(
    name = "Bränd umbra nr 100 (RAÄ)",
    kremer_match = "Burnt Umber, dark brown (PBr7)",
    kremer_id = "40720",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40720-burnt-umber-dark-brown",
    ottosson_match = "Umbra bränd",
    ottosson_url = "https://ottossonfarg.com/produkt/umbra-brand/",
    claessons_match = "Bränd umbra 1783",
    claessons_url = "https://claessons.com/umbra/brand-umbra-1783-25-kg/",
    notes = "Cypriotisk bränd umbra, mycket mörk. Finns hos Kremer, Ottosson och Claessons."
  ),
  
  "BRU39" = list(
    name = "Brun umbra nr 39 (RAÄ)",
    kremer_match = "Burnt Umber, reddish (PBr7)",
    kremer_id = "40700",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40700-burnt-umber-reddish",
    notes = "Italiensk rödaktig bränd umbra, varmare ton"
  ),
  
  "GRAU36" = list(
    name = "Grå umbra nr 36 (RAÄ)",
    kremer_match = "Raw Umber, dark + small amount of blue pigment",
    kremer_id = "40660",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40660-raw-umber-dark",
    notes = "Använd Raw Umber dark; tillsätt en nypa ultramarin för gråton"
  ),
  
  # IRON OXIDE REDS
  "J225" = list(
    name = "Järnoxidrött nr 225 (RAÄ)",
    kremer_match = "Red Iron Oxide, medium (PR101)",
    kremer_id = "48200",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48200-red-iron-oxide-medium",
    notes = "Mellannyans röd järnoxid"
  ),
  
  "J180M" = list(
    name = "Caput Mortuum 180M (RAÄ)",
    kremer_match = "Caput Mortuum Violet (PR101)",
    kremer_id = "48280",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48280-caput-mortuum-violet",
    notes = "Mörkt lila-brunt järnoxid, klassiskt caput mortuum"
  ),
  
  "J120N" = list(
    name = "Järnoxidrött nr 120N (RAÄ)",
    kremer_match = "Red Iron Oxide, light (PR101)",
    kremer_id = "48220",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48220-red-iron-oxide-light",
    notes = "Ljusare rött järnoxid"
  ),
  
  "ER48A" = list(
    name = "Engelskt rött nr 48A (RAÄ)",
    kremer_match = "English Red (PR101)",
    kremer_id = "42100",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/42100-english-red",
    notes = "Traditionellt engelskt rött, ljust orange-rött järnoxid"
  ),
  
  # IRON OXIDE BROWNS
  "J663" = list(
    name = "Järnoxidbrunt nr 663 (RAÄ)",
    kremer_match = "Brown Iron Oxide 610 (PBr6/7)",
    kremer_id = "48610",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48610-brown-iron-oxide-610",
    notes = "Syntetiskt brunt järnoxid, mycket mörkt"
  ),
  
  "J686" = list(
    name = "Järnoxidbrunt nr 686 (RAÄ)",
    kremer_match = "Brown Iron Oxide 686 (PBr6/7)",
    kremer_id = "48686",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48686-brown-iron-oxide-686",
    notes = "EXAKT MATCHNING - Samma produktnummer 686, troligen identiskt pigment."
  )
)


# === MISC MATERIALS (SOLVENTS, ADDITIVES) ===

misc_materials <- list(
  "balsamterpentin_biltema" = list(
    name = "Balsamterpentin 1 liter",
    category = "Lösningsmedel",
    supplier = "Biltema",
    description = "Naturlig terpentin från tallharts. Lösningsmedel för oljefärg och lack.",
    url = "https://www.biltema.se/bygg/farg/rengoringsmedel/balsamterpentin-1-liter-2000063842",
    notes = "Prisvärd, lättillgänglig i butik"
  ),
  
  "balsamterpentin_claessons" = list(
    name = "Balsamterpentin",
    category = "Lösningsmedel",
    supplier = "Claessons",
    description = "Ren balsamterpentin från tallharts. Traditionellt lösningsmedel.",
    url = "https://claessons.com/balsamterpentin/balsamterpentin/",
    notes = "Hög kvalitet, naturprodukt"
  ),
  
  "balsamterpentin_kremer" = list(
    name = "Pine Turpentine",
    category = "Lösningsmedel",
    supplier = "Kremer Pigmente",
    description = "Pure pine turpentine. Professional quality solvent.",
    url = "https://www.kremer-pigmente.com/en/shop/solvents-chemicals-additives/70010-pine-turpentine.html",
    notes = "Premium quality, international supplier"
  )
)


# === TRÄTJÄRA (WOOD TAR) SUPPLIERS & COLORS ===

# Tar masstone RGB values (research documented in TAR_RGB_RESEARCH.txt)
tar_colors <- list(
  "Dalbränd trätjära (finast)" = list(
    rgb = c(140, 95, 45),
    hex = "#8C5F2D",
    description = "Ljus och ren, gyllengul"
  ),
  "Ljus trätjära" = list(
    rgb = c(90, 60, 35),
    hex = "#5A3C23",
    description = "Honungs- eller bärnstenfärg"
  ),
  "Mörk trätjära (billigast)" = list(
    rgb = c(50, 35, 22),
    hex = "#32231B",
    description = "Mycket mörk brun, nästan svart"
  )
)

# Swedish wood tar suppliers with products
tar_suppliers <- list(
  
  # DALBRÄND TRÄTJÄRA (FINEST)
  "dalbrands_finest" = list(
    name = "Fintjära extra prima dalbränd",
    category = "Dalbränd trätjära (finast)",
    supplier = "Claessons Trätjära",
    description = "Traditionellt dalbränd trätjära av högsta kvalitet",
    url = "https://claessons.com/tratjaror/",
    notes = "Finaste kvaliteten, lämplig för alla ändamål"
  ),
  
  "dalbrands_prima" = list(
    name = "Fintjära prima dalbränd",
    category = "Dalbränd trätjära (finast)",
    supplier = "Claessons Trätjära",
    description = "Dalbränd trätjära, prima kvalitet",
    url = "https://claessons.com/tratjaror/",
    notes = "Utmärkt kvalitet, något mörkare än extra prima"
  ),
  
  "ottosson_dalbrands" = list(
    name = "Svensk dalbränd trätjära",
    category = "Dalbränd trätjära (finast)",
    supplier = "Ottosson Färgmakeri",
    description = "Äkta svensk dalbränd trätjära",
    url = "https://ottossonfarg.com/produkt/svensk-dalbrand-tratjara/",
    notes = "Svensktillverkad, ekologiskt hållbar"
  ),
  
  # LJUS TRÄTJÄRA
  "claessons_light" = list(
    name = "Fintjära prima dalbränd (ljus)",
    category = "Ljus trätjära",
    supplier = "Claessons Trätjära",
    description = "Ljusare variant för grundbehandling",
    url = "https://claessons.com/tratjaror/",
    notes = "God genomträngning, penetrerar djupt"
  ),
  
  # MÖRK TRÄTJÄRA
  "claessons_dark" = list(
    name = "Furutjära",
    category = "Mörk trätjära",
    supplier = "Claessons Trätjära",
    description = "Mörkare trätjära från furu",
    url = "https://claessons.com/tratjaror/",
    notes = "Utmärkt väderskydd för exponerade ytor"
  ),
  
  "biltema_dark" = list(
    name = "Äkta trätjära 1 liter",
    category = "Mörk trätjära",
    supplier = "Biltema",
    description = "Äkta trätjära för ytbehandling av trä",
    url = "https://www.biltema.se/bygg/farg/utomhusfarg/asfalt/akta-tratjara-1-liter-2000053045",
    notes = "Prisvärd, lättillgänglig i butik"
  ),
  
  "tjarfarg_dark" = list(
    name = "Äkta trätjära",
    category = "Mörk trätjära",
    supplier = "Tjärfärg.se",
    description = "Traditionell trätjära för träbehandling",
    url = "https://www.tjarfarg.se/produkter/klassiker/akta-tratjara/",
    notes = "Specialiserad leverantör av tjärprodukter"
  )
)

# Helper: Get tars by category
get_tars_by_category <- function(category = NULL) {
  if(is.null(category)) return(tar_suppliers)
  Filter(function(tar) tar$category == category, tar_suppliers)
}

# Helper: Create tar choices for dropdown
create_tar_choices <- function(category = NULL) {
  tars <- get_tars_by_category(category)
  choices <- setNames(
    names(tars),
    sapply(names(tars), function(id) {
      tar <- tars[[id]]
      paste0(tar$name, " - ", tar$supplier)
    })
  )
  as.list(choices)
}

# Helper: Create grouped tar choices (with optgroups)
create_grouped_tar_choices <- function() {
  list(
    "Dalbränd trätjära (finast)" = create_tar_choices("Dalbränd trätjära (finast)"),
    "Ljus trätjära" = create_tar_choices("Ljus trätjära"),
    "Mörk trätjära" = create_tar_choices("Mörk trätjära")
  )
}


# Enkel och säker choices-lista
make_choices <- function(ids) {
  setNames(ids, sapply(ids, function(id) paste0(km[[id]]$name, " (#", id, ")")))
}

# Create filler choices (extracts from Fyllmedel category)
create_filler_choices <- function() {
  filler_ids <- c("599930", "58000", "58010", "58162", "58900", "58250")
  # Use make_choices but return as simple list (not named for optgroup)
  choices <- make_choices(filler_ids)
  as.list(choices)
}

# Create grouped choices for optgroups (Swedish categories)
create_grouped_choices <- function() {
  list(
    "Vitbas" = make_choices(c("vitbas")),
    
    "Fyllmedel" = make_choices(c(
      "599930", "58000", "58010", "58162", "58900", "58250"
    )),
    
    "Gröna" = make_choices(c(
      "40400", "41700", "11100", "KG83", "ZG65", "40850", "40860", "GU30"
    )),
    
    "Svarta" = make_choices(c(
      "44450", "J318", "BS98", "47501", "47400"
    )),
    
    "Blåa" = make_choices(c(
      "11670", "UB88", "KB28"
    )),
    
    "Terra & Pozzuoli" = make_choices(c(
      "40820", "40800", "40830", "BT44", "OT46"
    )),
    
    "Gula & Ockror" = make_choices(c(
      "44082", "44086", "44150", "44160", "J920", "LO92", "GO94", "GO94_GU30"
    )),
    
    "Siennas & Umbror" = make_choices(c(
      "44650", "44620", "OU103", "BU100", "BRU39", "GRAU36"
    )),
    
    "Röda & Orange" = make_choices(c(
      "44300", "44200", "44210", "44220", "44510", "J225", "J180M", "J120N", "ER48A"
    )),
    
    "Bruna" = make_choices(c(
      "J663", "J686", "48330"
    ))
  )
}

all_choices <- c("Välj pigment" = "", create_grouped_choices())


# === KULTURKULÖR PRESET SYSTEM ===
source("kulturkulor_recipes.r")
source("kulturkulor_recipes_part2.r")
source("kulturkulor_recipes_part3.r")
kulturkulor_complete <- c(kulturkulor, kulturkulor_part2, kulturkulor_part3)

# === UNIFIED SWATCH MATRIX SYSTEM ===

# Generic function to generate swatch matrices for any pigment set
# Parameters:
#   pigments: vector of pigment IDs to generate swatches for
#   vitbas_increments: vector of vitbas percentages (e.g., c(0, 15, 30, 45, 60, 75, 90))
#   shade_increments: vector of shading percentages
#   shade_pigment: ID of shading pigment to use
#   code_prefix: prefix for swatch codes (e.g., "RAA" or "EXT")
#   mask: optional logical matrix or function to filter combinations
#         - matrix: TRUE/FALSE for each (vitbas, shade) combination
#         - function: takes (pigment_id, vitbas_pct, shade_pct, basfarg_pct) and returns TRUE/FALSE
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
                                   toupper(substr(base_id, 1, 4)), vitbas_pct, shade_pct)
            
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

# === DYNAMIC SWATCH GENERATOR FOR NON-RAÄ PIGMENTS ===

# Define shading pigments available for user selection (ONLY existing pigments)
shading_pigments <- list(
  "Svartoxid PBk11 (#44450)" = "44450",
  "Järnoxidsvart nr 318 (#J318)" = "J318",
  "Bensvart nr 98 (#BS98)" = "BS98",
  "Mangansvart (#47501)" = "47501",
  "Spinel-svart (#47400)" = "47400",
  "Obränd umbra PBr7 (#OU103)" = "OU103",
  "Bränd umbra PBr7 (#BU100)" = "BU100",
  "Brun umbra (#BRU39)" = "BRU39",
  "Grön umbra nr 30 (#GU30)" = "GU30",
  "Ljusockra PY43 (#LO92)" = "LO92",
  "Guldockra PY43 (#GO94)" = "GO94"
)

# Get list of shading pigment IDs (to exclude from base colors)
shading_pigment_ids <- unlist(shading_pigments, use.names = FALSE)

# Get list of ALL pigments for extended swatches (including RAÄ, excluding whites, fillers, and shading pigments)
get_extended_base_pigments <- function() {
  filler_ids <- c("599930", "58000", "58010", "58162", "58900", "58250")
  white_ids <- c("vitbas", "44100", "44400")
  # Only exclude fillers, whites, and shading pigments - INCLUDE RAÄ pigments as base colors
  exclude_ids <- c(filler_ids, white_ids, shading_pigment_ids)
  
  # Get all pigment IDs except excluded ones
  all_ids <- names(km)
  base_pigments <- setdiff(all_ids, exclude_ids)
  
  base_pigments
}

# Generate swatch code for a pigment
# Generate all swatches for all base pigments (including RAÄ) - uses generic matrix generator  
generate_all_extended_swatches <- function(shade_pigment_id = "44450") {
  base_pigments <- get_extended_base_pigments()
  
  # Extended pattern: 10% steps for vitbas (x-axis, more light variants)
  #                   20% steps for shade (y-axis, fewer dark variants)
  vitbas_increments <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)  # 10 levels
  shade_increments <- c(0, 20, 40, 60, 80)  # 5 levels
  
  generate_swatch_matrix(base_pigments, vitbas_increments, shade_increments, shade_pigment_id, "EXT")
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

# Note: extended_swatches generated reactively in server function (not here)

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

ui <- dashboardPage(
  dashboardHeader(
    title = "Paint-o-matic",
    # Version number (right side, small text)
    tags$li(
      class = "dropdown",
      tags$a(href = "https://github.com/hmep/karlslund/blob/main/paint-o-matic/LICENSE", class = "version-text", "version 0.9.1-swatches, © 2025 Tobias Hagberg, licens GPLv3")
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
      .content-wrapper {background: #ccc !important;}
      .step { padding:24px; padding-bottom:64px; background:#fff; border-radius:12px; margin:20px 20px 80px 20px; position:relative; min-width: 360px; max-width:840px;margin:auto;}
      .footer-ref { position:relative; bottom:-44px; left:0; right:0; font-size:12px; color:#555; text-align:center; 
                    padding:12px 12px 0; border-top:1px solid #ddd; }
      .preview { display:block; height:300px; width:300px; border:8px solid #333; border-radius:150px; margin: auto; }
      .normalized-box, .info-box, .alert { background:#eee; drop-shadow: 0 0; color:black; border: 0; padding:12px; border-radius:6px;margin:1em 0;}
      .normalized-box { margin:10px 0;}
      .ready-box {padding: 20px;}
      .ready-box h3 {margin-top:0; }
      .rmargin-box {margin-right:20px;}
      .btn {margin: .5em .5em 0 0;}
      .btn-primary { color:white;}
      .kulturkulor-swatch { 
        display:inline-block; width:24px; height:24px; border-radius:50%; 
        margin:3px; cursor:pointer; border:2px solid #999;
        transition: transform 0.1s, border-color 0.1s;
      }
      .kulturkulor-swatch:hover { 
        transform:scale(1.3); border-color:#333; z-index:10; position:relative;
      }
      .kulturkulor-gallery { 
        max-height:200px; overflow-y:auto; overflow-x:hidden;
        padding:8px; background:#fff; border:1px solid #ddd; border-radius:4px;
        margin-top:8px;
      }
      table tr td { white-space: nowrap; }
      table tr td:first-of-type { white-space: wrap; }
      h2 {margin: 0 0 .5em;padding:0}
      .navbar-custom-menu .navbar-nav > li > a.version-text { font-size: 11px; color: #aaa; padding-top: 15px; padding-bottom: 15px;}
      
      /* Fullscreen preview styles */
      .preview-container {
        position: relative;
        display: inline-block;
      }
      .zoom-icon {
        position: absolute;
        top: 4px;
        right: 4px;
        background: white;
        border: none;
        border-radius: 50%;
        width: 36px;
        height: 36px;
        font-size: 24px;
        cursor: pointer;
        color: #333;
        box-shadow: 0 2px 4px rgba(0,0,0,0.3);
        transition: all 0.2s;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: 300;
        line-height: 1;
      }
      .zoom-icon:hover {
        background: black;
        color: white;
        transform: scale(1.1);
      }
      .fullscreen-overlay {
        display: none;
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0, 0, 0, 0.95);
        z-index: 9999;
        justify-content: center;
        align-items: center;
      }
      .fullscreen-overlay.active {
        display: flex;
      }
      .fullscreen-preview {
        width: 100%;
        height: 100%;
        border: 0px solid white;
      }
      .fullscreen-close {
        position: absolute;
        top: 20px;
        right: 30px;
        background: white;
        border: none;
        border-radius: 50%;
        width: 50px;
        height: 50px;
        font-size: 30px;
        cursor: pointer;
        color: #333;
        box-shadow: 0 4px 8px rgba(0,0,0,0.3);
        transition: all 0.2s;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .fullscreen-close:hover {
        background: black;
        color: white;
        transform: scale(1.1);
      }
      
      /* Paint type box styling */
      .paint-type-box {
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        margin-top: 15px;
      }
    "))),
    
    tags$script(HTML('
      // Fullscreen preview functionality
      function openFullscreen(previewId) {
        var preview = document.querySelector("#" + previewId + " .preview");
        if (!preview) return;
        
        var color = window.getComputedStyle(preview).backgroundColor;
        var overlay = document.getElementById("fullscreen-overlay");
        var fullPreview = document.getElementById("fullscreen-preview");
        
        fullPreview.style.background = color;
        overlay.classList.add("active");
        document.body.style.overflow = "hidden"; // Prevent scrolling
      }
      
      function closeFullscreen() {
        var overlay = document.getElementById("fullscreen-overlay");
        overlay.classList.remove("active");
        document.body.style.overflow = ""; // Restore scrolling
      }
      
      // Close on ESC key
      document.addEventListener("keydown", function(e) {
        if (e.key === "Escape") {
          closeFullscreen();
        }
      });
    ')),
    
    # Fullscreen overlay (shared for both previews)
    tags$div(id = "fullscreen-overlay", class = "fullscreen-overlay", onclick = "closeFullscreen()",
             tags$button(class = "fullscreen-close", onclick = "closeFullscreen()", 
                         HTML("&times;")),
             tags$div(id = "fullscreen-preview", class = "fullscreen-preview")
    ),
    
    hidden(div(id="step1", class="step",
               h2("Blanda pigment"),
               fluidRow(
                 column(6,
                        h5(style="font-weight:bold;","Inställningar"),
                        checkboxInput("raa_only", "Använd endast Kulturkulör-pigment (RAÄ)", TRUE),
                        #checkboxInput("use_tinting_strength","Avancerad färgblandning",TRUE),
                        #tags$small(style="color:#666; margin-left:20px; display:block; margin-top:-1em; margin-bottom:10px;","Väger pigment efter faktiska färgstyrka (K- och S-värden)"),
                        hr(),
                        pickerInput("p1", "Pigment 1", choices = all_choices, selected = "vitbas",
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p1", sliderInput("pct1","Andel (%)",0,100,70,1)),
                        pickerInput("p2", "Pigment 2", choices = all_choices, selected = "J920",
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p2", sliderInput("pct2","Andel (%)",0,100,30,1)),
                        pickerInput("p3", "Pigment 3", choices = all_choices, selected = "",
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p3", sliderInput("pct3","Andel (%)",0,100,0,1)),
                        pickerInput("p4", "Pigment 4", choices = all_choices, selected = "",
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p4", sliderInput("pct4","Andel (%)",0,100,0,1)),
                        hr(),
                        actionButton("reset_pigments", "Nollställ pigment", class="btn-default"),
                 ),
                 column(6,
                        h3("Färgprov"),
                        uiOutput("preview1"), br(),
                        tags$b("Total andel: "), textOutput("total_pct",inline=TRUE), " %", 
                        uiOutput("total_warning"), 
                        tags$div(style="margin-top:2em;",
                                 h5(style="font-weight:bold;","Samlingar med fördefinierade recept"),
                                 selectInput("recipe_set", NULL,
                                             choices = list(
                                               "RAÄ Kulturkulör" = "raa",
                                               "Paint-o-matic-recept" = "extended"
                                             ),
                                             selected = "raa"),
                                 
                                 # Show description based on selected set
                                 conditionalPanel(
                                   condition = "input.recipe_set == 'raa'",
                                   tags$small(a("Kulturkulör från Riksantikvarieämbetet (RAÄ)", href="https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/")," är ett färgsystem för historiskt trogen färgsättning. Den rätta skuggningsfärgen för Kulturkulör är ", tags$b("Järnoxidsvart nr 318 (RAÄ) (#J318)"), " men du kan också välja en annan om du vill blanda ett eget recept."),
                                   br(), br(),
                                   selectInput("shading_pigment_raa", "Skuggningsfärg",
                                               choices = shading_pigments,
                                               selected = "J318")
                                 ),
                                 
                                 conditionalPanel(
                                   condition = "input.recipe_set == 'extended'",
                                   tags$small("Receptpaletter med toning- och skuggningsserier för alla pigment som är tillgängliga i Paint-o-matic."),
                                   br(), br(),
                                   selectInput("shading_pigment", "Skuggningsfärg",
                                               choices = shading_pigments,
                                               selected = "J318")
                                 ),
                                 
                                 div(style = "width: 100%; height: 300px; overflow-y: auto; overflow-x: auto; border: 1px solid #ddd; padding: 10px;",
                                     uiOutput("recipe_swatches")
                                 )
                        )
                 )
               ),
               hr(),
               actionButton("to_step2","Nästa", class="btn-primary next-btn"),
               div(class="footer-ref", "Masstone baserad på data från Riksantikvarieämbetet (RAÄ) Kulturkulör, Kremer Pigmente, m. fl. Färgmixningen tar hänsyn till olika pigments fysikaliska egenskaper (K- och S-värden). Trots detta, notera att en skärm inte exakt kan simulera hur ljus som absorberas av eller reflekteras från en målad yta uppfattas.")
    )),
    
    hidden(div(id="step2", class="step",
               h2("Blanda vitbas"),
               fluidRow(column(
                 12,
                 p("Ange förhållandet mellan zinkoxid (zinkvitt) och titaniumdioxid (titanvitt) i vitbasen."),
                 p("För ", tags$b("utomhusfärg"), "– välj en högre andel zinkvitt i vitbasen (gärna 30 %, om det fungerar med den önskade kulören), så blir den färdiga färgen mer motståndskraftig mot alger och mögelpåväxt."),
                 p("För ", tags$b("inomhusfärg"), "– välj en lägre andel zinkvitt i vitbasen (0–15 %). Zink gör å ena sidan färgfilmen hårdare, men å den andra blir den också sprödare och känsligare över tid."),
                 #p("Oavsett vilket förhållande du väljer blir den färdiga färgpastan kulörmässigt identisk, eftersom alla färgande pigment automatiskt justeras med Kubelka-Munk-kompensationen."),
                 br(),
                 sliderInput("zinc_ratio","Andel zinkvitt i vitbasen (%)",0,100,15,5,post="% zinkoxid"),
               ), ),
               hr(),
               actionButton("back1","Föregående", class="btn-default back-btn"),
               actionButton("to_step3","Nästa", class="btn-primary next-btn"),
               div(class="footer-ref", "Kubelka-Munk-funktionen används för att bibehålla färgande pigments styrka i vitbasen konstant")
    )),
    
    hidden(div(id="step3", class="step",
               h2("Beräkna åtgång och spara recept"),
               fluidRow(
                 column(6,
                        numericInput("area","Yta att måla (m²)",10,1,2000,1),
                        
                        # Common setting: Number of coats (applies to all paint types)
                        radioButtons("use","Antal strykningar",
                                     choices=list("1 strykning"=1,
                                                  "2 strykningar"=2,
                                                  "3 strykningar"=3),
                                     selected=2),
                        
                        # Paint type selector
                        selectInput("paint_type", "Typ av färg",
                                    choices = list(
                                      "Linoljefärg" = "linseed",
                                      "Äggoljetemperafärg" = "egg_oil",
                                      "Tjäroljefärg" = "tar"
                                    ),
                                    selected = "linseed"),
                        
                        # Linoljefärg settings (default, wrapped in box)
                        conditionalPanel(
                          condition = "input.paint_type == 'linseed'",
                          tags$div(class = "paint-type-box",
                                   selectInput("substrate","Underlag (absorptionsfaktor)",
                                               choices=list(
                                                 "Metall, grundmålad (lägst åtgång)" = 1.3,  # Primed metal (very smooth)
                                                 "Tidigare målat trä " =	1.2,	              # Previously painted wood
                                                 "Hyvlat trä (normal åtgång)" = 1.0,	        # Planed wood (baseline)
                                                 "Sågat trä" = 0.8,                          # Rough sawn wood
                                                 "Porös puts, gips (högst åtgång)" = 0.45    # Porous (gypsum, rough masonry)
                                               ),
                                               selected = 1.0),
                                   hr(),
                                   sliderInput("extra_oil","Extra kokt linolja (CPVC-faktor)",1,2.5,1.6,0.05,post="× CPVC"),
                                   p("Reglaget ökar endast mängden kokt linolja i receptet (pigmentmängderna är fixerade). En viss mängd extra bindmedel, utöver den minsta mängd som krävs för pigmenten, underlättar både tillredningen av färgen med blandare i borrmaskin och dess strykbarhet med penseln. Du kan utan problem lägga till 1,6–2,2 gånger av CPVC av bindmedel."),
                                   hr(),
                                   p("Pastan du blandar är lämplig direkt som ", tags$b("grundstrykning"), " med gnuggande målningsstil (enligt principen från magert till fett) och utgör basen för ett komplett system för linoljefärgsmålning."),
                                   p("Till färg för ", tags$b("mellanstrykning"), " kan du tillföra ytterligare kokt linolja, precis upp till den maximala mängd som fortfarande medger att färgen struken på en glasskiva förblir ogenomskinlig."),
                                   p("Till färg för ", tags$b("slutstrykning"), " kan du därutöver med fördel tillsätta 10% kokt eller ännu hellre soloxiderad olja."),
                                   p("En burk till alla strykningar – tillsätt bara lite mer linolja efter hand!")
                          )
                        ),
                        
                        # Äggoljetemperafärg settings
                        conditionalPanel(
                          condition = "input.paint_type == 'egg_oil'",
                          tags$div(class = "paint-type-box",
                                   selectInput("egg_filler", "Val av fyllmedel (ger matt färg)",
                                               choices = create_filler_choices(),
                                               selected = "58000")
                          )
                        ),
                        
                        # Tjäroljefärg settings
                        conditionalPanel(
                          condition = "input.paint_type == 'tar'",
                          tags$div(class = "paint-type-box",
                                   selectInput("tar_category", "Typ av trätjära",
                                               choices = names(tar_colors),
                                               selected = names(tar_colors)[1]),
                                   hr(),
                                   sliderInput("tar_extra_oil", "Extra olja/tjära (CPVC-faktor)", 
                                               1, 2.5, 1.6, 0.05, post = "× CPVC"),
                                   p("Reglaget ökar mängden olja och tjära proportionellt. Högre värde ger mer flytande färg och bättre strykbarhet. Du kan utan problem lägga till 1,6–2,2 gånger av CPVC av bindmedel."),
                          )
                        )
                        
                 ),
                 column(6,class="ready-box",
                        uiOutput("recipe_header"),
                        uiOutput("recipe_description"),
                        uiOutput("final_preview"),br(),
                        tableOutput("final_recipe"),
                        downloadButton("download_txt","Spara som textfil",class="btn btn-primary"),
                        actionButton("copy_share_link","Kopiera delningslänk",class="btn btn-default"),
                        tags$input(id="share_url_hidden", type="hidden", value="")
                 )
               ),
               hr(),
               actionButton("back2","Föregående", class="btn-default back-btn"),
               actionButton("restart","Börja om från början", class="btn-default"),
               div(class="footer-ref", "Åtgång per m²: praktiska test, data från RAÄ Byggnadsvård, m. fl., uppskattningarna är ungefärliga och beror också på målningsstil")
    ))
  )
)

server <- function(input, output, session) {
  # === STORE LAST VALID INPUT VALUES ===
  # This prevents crashes when user clears text boxes temporarily
  last_valid <- reactiveValues(
    area = 10,
    extra_oil = 1.8,
    zinc_ratio = 15
  )
  
  # Update stored values when inputs are valid (combined for efficiency)
  observe({
    checks <- list(
      area = list(val = input$area, test = function(x) x > 0),
      extra_oil = list(val = input$extra_oil, test = function(x) x >= 1),
      zinc_ratio = list(val = input$zinc_ratio, test = function(x) x >= 0)
    )
    for(name in names(checks)) {
      check <- checks[[name]]
      if(isTRUE(!is.null(check$val) && !is.na(check$val) && check$test(check$val))) {
        last_valid[[name]] <- check$val
      }
    }
  })
  
  # === LOAD RECIPE FROM URL ===
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(length(query) > 0 && any(c("p1", "p2", "p3", "p4") %in% names(query))) {
      # Wait for app to be ready
      delay(500, {
        # Load pigments and percentages
        if("p1" %in% names(query) && query$p1 != "") {
          updatePickerInput(session, "p1", selected = query$p1)
          if("pct1" %in% names(query)) updateSliderInput(session, "pct1", value = as.numeric(query$pct1))
        }
        if("p2" %in% names(query) && query$p2 != "") {
          updatePickerInput(session, "p2", selected = query$p2)
          if("pct2" %in% names(query)) updateSliderInput(session, "pct2", value = as.numeric(query$pct2))
        }
        if("p3" %in% names(query) && query$p3 != "") {
          updatePickerInput(session, "p3", selected = query$p3)
          if("pct3" %in% names(query)) updateSliderInput(session, "pct3", value = as.numeric(query$pct3))
        }
        if("p4" %in% names(query) && query$p4 != "") {
          updatePickerInput(session, "p4", selected = query$p4)
          if("pct4" %in% names(query)) updateSliderInput(session, "pct4", value = as.numeric(query$pct4))
        }
        
        # Load other parameters
        if("area" %in% names(query)) updateNumericInput(session, "area", value = as.numeric(query$area))
        if("zinc_ratio" %in% names(query)) updateSliderInput(session, "zinc_ratio", value = as.numeric(query$zinc_ratio))
        if("extra_oil" %in% names(query)) updateSliderInput(session, "extra_oil", value = as.numeric(query$extra_oil))
        if("use" %in% names(query)) updateRadioButtons(session, "use", selected = query$use)
        
        # Load paint type and related parameters
        if("paint_type" %in% names(query)) {
          updateSelectInput(session, "paint_type", selected = query$paint_type)
          
          # Load paint-type-specific parameters
          if(query$paint_type == "egg_oil" && "egg_filler" %in% names(query)) {
            updateSelectInput(session, "egg_filler", selected = query$egg_filler)
          }
          if(query$paint_type == "tar") {
            if("tar_category" %in% names(query)) updateSelectInput(session, "tar_category", selected = query$tar_category)
            if("tar_extra_oil" %in% names(query)) updateSliderInput(session, "tar_extra_oil", value = as.numeric(query$tar_extra_oil))
          }
        }
        
        showNotification("Recept laddades från länk", type = "message", duration = 3)
      })
    }
  })
  
  show("step1")
  final_hex <- reactiveVal("#FFFFFF")
  
  # Helper function already defined at top of file, available here
  # parse_numeric() and format_swe() are already in global scope
  
  # Reset pigments
  observeEvent(input$reset_pigments, {
    updatePickerInput(session, "p1", selected = "vitbas")
    updatePickerInput(session, "p2", selected = "")  # Empty string instead of character(0)
    updatePickerInput(session, "p3", selected = "")  # Empty string instead of character(0)
    updatePickerInput(session, "p4", selected = "")  # Empty string instead of character(0)
    updateSliderInput(session, "pct1", value = 70)
    updateSliderInput(session, "pct2", value = 0)
    updateSliderInput(session, "pct3", value = 0)
    updateSliderInput(session, "pct4", value = 0)
  })
  
  # RAÄ-filter
  # Uncheck RAÄ-only when user selects extended recipes
  observeEvent(input$recipe_set, {
    if(!is.null(input$recipe_set) && input$recipe_set == "extended") {
      updateCheckboxInput(session, "raa_only", value = FALSE)
    }
  })
  
  observeEvent(input$raa_only, {
    ids <- if(input$raa_only) raa_pigments else names(km)
    
    # Create grouped choices based on filter
    create_filtered_grouped_choices <- function(filter_ids) {
      list(
        "Vitbas" = make_choices(intersect(c("vitbas"), filter_ids)),
        
        "Fyllmedel" = make_choices(intersect(c(
          "599930", "58000", "58010", "58162", "58900", "58250"
        ), filter_ids)),
        
        "Gröna" = make_choices(intersect(c(
          "40400", "41700", "11100", "KG83", "ZG65", "40850", "40860", "GU30"
        ), filter_ids)),
        
        "Svarta" = make_choices(intersect(c(
          "44450", "J318", "BS98", "47501", "47400"
        ), filter_ids)),
        
        "Blåa" = make_choices(intersect(c(
          "11670", "UB88", "KB28"
        ), filter_ids)),
        
        "Terra & Pozzuoli" = make_choices(intersect(c(
          "40820", "40800", "40830", "BT44", "OT46"
        ), filter_ids)),
        
        "Gula & Ockror" = make_choices(intersect(c(
          "44082", "44086", "44150", "44160", "J920", "LO92", "GO94", "GO94_GU30"
        ), filter_ids)),
        
        "Siennas & Umbror" = make_choices(intersect(c(
          "44650", "44620", "OU103", "BU100", "BRU39", "GRAU36"
        ), filter_ids)),
        
        "Röda & Orange" = make_choices(intersect(c(
          "44300", "44200", "44210", "44220", "44510", "J225", "J180M", "J120N", "ER48A"
        ), filter_ids)),
        
        "Bruna" = make_choices(intersect(c(
          "J663", "J686", "48330"
        ), filter_ids))
      )
    }
    
    # Filter out empty groups
    grouped <- create_filtered_grouped_choices(ids)
    grouped <- grouped[sapply(grouped, length) > 0]
    
    choices_list <- c("Välj pigment" = "", grouped)
    current_p1 <- input$p1 %||% "vitbas"
    updatePickerInput(session, "p1", choices = choices_list, selected = current_p1)
    updatePickerInput(session, "p2", choices = choices_list, selected = input$p2)
    updatePickerInput(session, "p3", choices = choices_list, selected = input$p3)
    updatePickerInput(session, "p4", choices = choices_list, selected = input$p4)
  })
  
  # Reactive for extended swatches - regenerate when shading pigment changes
  extended_swatches_reactive <- reactive({
    shade_pigment <- input$shading_pigment %||% "44450"
    generate_all_extended_swatches(shade_pigment)
  })
  
  # Reactive for RAÄ swatches - regenerate when shading pigment changes
  raa_swatches_reactive <- reactive({
    shade_pigment <- input$shading_pigment_raa %||% "J318"
    generate_all_raa_swatches(shade_pigment)
  })
  
  # Generic function to render swatch matrix
  render_swatch_matrix <- function(recipes, base_pigments, vitbas_increments, shade_increments, shade_pigment, use_tinting) {
    shade_name <- km[[shade_pigment]]$name
    
    if(length(recipes) == 0) {
      return(tags$p("Inga recept tillgängliga."))
    }
    
    matrix_elements <- list()
    
    for(base_id in base_pigments) {
      base_name <- km[[base_id]]$name
      
      # Add pigment heading
      matrix_elements[[length(matrix_elements) + 1]] <- tags$div(
        style = "margin-top: 1em; margin-bottom: 0.5em; font-weight: bold;",
        sprintf("%s med %s", base_name, shade_name)
      )
      
      matrix_rows <- list()
      
      # Build matrix: rows = shade levels, columns = vitbas levels
      for(shade_pct in shade_increments) {
        row_swatches <- list()
        
        for(vitbas_pct in vitbas_increments) {
          # Find the matching swatch
          matching_swatch <- NULL
          for(code in names(recipes)) {
            recipe <- recipes[[code]]
            if(recipe$base_pigment == base_id && 
               recipe$vitbas_pct == vitbas_pct && 
               recipe$shade_pct == shade_pct) {
              matching_swatch <- list(code = code, recipe = recipe)
              break
            }
          }
          
          if(!is.null(matching_swatch)) {
            recipe <- matching_swatch$recipe
            code <- matching_swatch$code
            base_pct <- recipe$base_pct
            
            # Build mix for color calculation
            ids <- c()
            pcts <- c()
            
            if(base_pct > 0) {
              ids <- c(ids, base_id)
              pcts <- c(pcts, base_pct)
            }
            if(vitbas_pct > 0) {
              ids <- c(ids, "vitbas")
              pcts <- c(pcts, vitbas_pct)
            }
            if(shade_pct > 0) {
              ids <- c(ids, shade_pigment)
              pcts <- c(pcts, shade_pct)
            }
            
            # Calculate color
            if(length(ids) > 0) {
              color_rgb <- mix_colors(ids, pcts, km, use_tinting = use_tinting)
              hex_color <- rgb(color_rgb[1], color_rgb[2], color_rgb[3], maxColorValue = 255)
            } else {
              hex_color <- "#FFFFFF"
            }
            
            paint_name <- sprintf("%s: %s (%g%% + %gV + %gS)", 
                                  code, base_name, base_pct, vitbas_pct, shade_pct)
            
            # Add swatch to row
            row_swatches[[length(row_swatches) + 1]] <- tags$span(
              class = "kulturkulor-swatch",
              style = sprintf("background-color:%s;", hex_color),
              title = paint_name,
              onclick = sprintf("Shiny.setInputValue('swatch_click', '%s', {priority: 'event'});", code)
            )
          }
        }
        
        # Add row to matrix (only if it has swatches)
        if(length(row_swatches) > 0) {
          matrix_rows[[length(matrix_rows) + 1]] <- tags$div(
            class = "swatch-row",
            style = "white-space: nowrap;",
            row_swatches
          )
        }
      }
      
      # Add matrix to elements (only if it has rows)
      if(length(matrix_rows) > 0) {
        matrix_elements[[length(matrix_elements) + 1]] <- tags$div(
          class = "swatch-matrix",
          matrix_rows
        )
      }
    }
    
    tags$div(class = "swatch-matrices", matrix_elements)
  }
  
  output$recipe_swatches <- renderUI({
    recipe_set <- input$recipe_set %||% "raa"
    use_tinting <- TRUE
    
    if(recipe_set == "raa") {
      # RAÄ swatches with matrix display
      recipes_to_show <- raa_swatches_reactive()
      shade_pigment <- input$shading_pigment_raa %||% "J318"
      
      # RAÄ base pigments (excluding shading pigments)
      pattern_a_pigments <- c("J225", "J180M", "J120N", "ER48A", "J663", "J686", "J920",
                              "LO92", "GO94", "OU103", "BU100", "BRU39", 
                              "BT44", "OT46", "KG83", "UB88", "KB28")
      pattern_b_pigments <- c("J318", "GO94_GU30", "GU30", "GRAU36", "BS98")
      
      # Filter out shading pigments
      pattern_a_pigments <- setdiff(pattern_a_pigments, shading_pigment_ids)
      pattern_b_pigments <- setdiff(pattern_b_pigments, shading_pigment_ids)
      
      raa_base_pigments <- c(pattern_a_pigments, pattern_b_pigments)
      
      # RAÄ uses per-pigment increments - will be determined from recipes
      # For render_swatch_matrix, we need to extract unique values per pigment
      vitbas_all <- c(0, 14.28, 15, 29.27, 30, 41.86, 42.85, 45, 57.14, 60, 73.17, 75, 85.71, 90)
      shade_all <- c(0, 2.44, 4.76, 6.97)
      
      return(render_swatch_matrix(recipes_to_show, raa_base_pigments, vitbas_all, 
                                  shade_all, shade_pigment, use_tinting))
    }
    
    if(recipe_set == "extended") {
      # Extended swatches with matrix display
      recipes_to_show <- extended_swatches_reactive()
      shade_pigment <- input$shading_pigment %||% "44450"
      
      base_pigments <- get_extended_base_pigments()
      
      # Extended pattern: 10% vitbas steps, 20% shade steps
      vitbas_increments <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)  # 10 levels
      shade_increments <- c(0, 20, 40, 60, 80)  # 5 levels
      
      return(render_swatch_matrix(recipes_to_show, base_pigments, vitbas_increments, 
                                  shade_increments, shade_pigment, use_tinting))
    }
  })
  
  # Handle swatch clicks
  observeEvent(input$swatch_click, {
    req(input$swatch_click)
    
    recipe_set <- input$recipe_set %||% "raa"
    code <- input$swatch_click
    
    # Both RAÄ and extended now use the same recipe structure
    if(recipe_set == "extended") {
      recipes <- extended_swatches_reactive()
    } else {
      recipes <- raa_swatches_reactive()
    }
    
    recipe <- recipes[[code]]
    if(is.null(recipe)) return()
    
    # Load recipe: base_pigment + vitbas + shade_pigment
    base_id <- recipe$base_pigment
    base_pct <- round(recipe$base_pct)
    vitbas_pct <- round(recipe$vitbas_pct)
    shade_pct <- round(recipe$shade_pct)
    shade_id <- recipe$shade_pigment
    
    # Ensure they sum to 100
    total <- base_pct + vitbas_pct + shade_pct
    if(total != 100) {
      diff <- 100 - total
      base_pct <- base_pct + diff
    }
    
    # CRITICAL: Clear ALL slots first (p2, p3, p4) before loading anything
    updatePickerInput(session, "p2", selected = character(0))
    updateSliderInput(session, "pct2", value = 0)
    updatePickerInput(session, "p3", selected = character(0))
    updateSliderInput(session, "pct3", value = 0)
    updatePickerInput(session, "p4", selected = character(0))
    updateSliderInput(session, "pct4", value = 0)
    
    # Load base pigment (always present in p1)
    updatePickerInput(session, "p1", selected = base_id)
    updateSliderInput(session, "pct1", value = base_pct)
    
    # Load vitbas if needed (always in p2)
    if(vitbas_pct > 0) {
      updatePickerInput(session, "p2", selected = "vitbas")
      updateSliderInput(session, "pct2", value = vitbas_pct)
    }
    
    # Load shading pigment if needed (always in p3)
    if(shade_pct > 0) {
      updatePickerInput(session, "p3", selected = shade_id)
      updateSliderInput(session, "pct3", value = shade_pct)
    }
  })
  
  # Blandning
  mix <- reactive({
    ids <- c(input$p1, input$p2, input$p3, input$p4)
    pct <- c(input$pct1 %||% 0, input$pct2 %||% 0, input$pct3 %||% 0, input$pct4 %||% 0)
    
    # Filter: must have valid ID AND percentage > 0
    valid <- sapply(seq_along(ids), function(i) {
      !is.na(ids[i]) && 
        !is.null(ids[i]) && 
        length(ids[i]) > 0 && 
        nchar(as.character(ids[i])) > 0 &&  # Extra check for empty strings
        ids[i] != "" && 
        !is.na(pct[i]) &&
        pct[i] > 0
    })
    
    ids_valid <- ids[valid]
    pct_valid <- pct[valid]
    
    # Remove duplicates: if same pigment appears multiple times, sum the percentages
    if(length(ids_valid) > 0) {
      unique_ids <- unique(ids_valid)
      if(length(unique_ids) < length(ids_valid)) {
        # There are duplicates - combine them
        combined_pct <- sapply(unique_ids, function(id) {
          sum(pct_valid[ids_valid == id])
        })
        ids_valid <- unique_ids
        pct_valid <- combined_pct
      }
    }
    
    total <- sum(pct_valid)
    list(ids = ids_valid, pct = pct_valid, total = ifelse(total==0,1,total),
         has_white = "vitbas" %in% ids_valid)
  })
  
  current_color <- reactive({
    m <- mix()
    if(length(m$ids) == 0) return("#FFFFFF")
    
    # Use tinting strength if checkbox is enabled
    use_tinting <- TRUE #isTRUE(input$use_tinting_strength)
    cols <- mix_colors(m$ids, m$pct, km, use_tinting = use_tinting)
    
    hex <- sprintf("#%02X%02X%02X", round(cols[1]), round(cols[2]), round(cols[3]))
    final_hex(hex)
    hex
  })
  
  output$total_pct <- renderText(format_swe(mix()$total, 1))
  output$hex1 <- renderText(current_color())
  output$preview1 <- renderUI(render_preview(current_color(), "preview1"))
  
  output$total_warning <- renderUI({
    m <- mix()
    if (m$total > 100 && length(m$ids) > 0) {
      # Calculate normalized percentages
      normalized <- (m$pct / m$total) * 100
      
      # Filter out any entries with 0 or near-0 normalized percentages
      keep <- normalized > 0.05
      if(sum(keep) == 0) return(NULL)
      
      ids_filtered <- m$ids[keep]
      normalized_filtered <- round(normalized[keep], 1)
      
      # Get pigment names and format
      pigment_names <- sapply(ids_filtered, function(id) km[[id]]$name)
      normalized_swe <- sapply(normalized_filtered, function(x) format_swe(x, 1))
      
      text_lines <- paste0(pigment_names, ": ", normalized_swe, " %", collapse = " • ")
      icon_type <- "exclamation-triangle"
      msg <- "Totalen överstiger 100 %. Normaliserade procentsatser som används:"
      tags$div(
        class = "alert",
        icon(icon_type),
        " ", msg, text_lines,
        tags$br(),
        tags$div(
          style = "margin-top: 0.5em;",
          actionButton("normalize_values", "Snabbjustera reglage till normaliserade värden", class = "btn-default btn-sm")
        )
      )
    }
  })
  
  # Display tar product information
  
  # Handle normalize button click
  observeEvent(input$normalize_values, {
    m <- mix()
    if (m$total > 100 && length(m$ids) > 0) {
      # Calculate normalized percentages
      normalized <- (m$pct / m$total) * 100
      
      # Round to integers (sliders use step=1)
      normalized_int <- round(normalized)
      
      # Ensure they sum to exactly 100 by adjusting largest
      total_normalized <- sum(normalized_int)
      if(total_normalized != 100) {
        diff <- 100 - total_normalized
        max_idx <- which.max(normalized_int)
        normalized_int[max_idx] <- normalized_int[max_idx] + diff
      }
      
      # Map back to p1, p2, p3, p4 inputs
      # CRITICAL: Use a list to preserve empty slots, not c() which drops them!
      current_inputs <- list(
        p1 = if(is.null(input$p1) || length(input$p1) == 0) NA else input$p1,
        p2 = if(is.null(input$p2) || length(input$p2) == 0) NA else input$p2,
        p3 = if(is.null(input$p3) || length(input$p3) == 0) NA else input$p3,
        p4 = if(is.null(input$p4) || length(input$p4) == 0) NA else input$p4
      )
      
      # Update sliders for each pigment
      for(i in seq_along(m$ids)) {
        pigment_id <- m$ids[i]
        new_pct <- normalized_int[i]
        
        # Find which input slot this pigment is in
        slot_idx <- which(sapply(current_inputs, function(x) !is.na(x) && x == pigment_id))[1]
        
        if(!is.na(slot_idx) && slot_idx <= 4) {
          input_name <- paste0("pct", slot_idx)
          updateSliderInput(session, input_name, value = new_pct)
        }
      }
      
      showNotification("Värdena har normaliserats till 100%", type = "message", duration = 2)
    }
  })
  
  # Navigation
  observeEvent(input$to_step2, { hide("step1"); if(mix()$has_white) show("step2") else show("step3") })
  observeEvent(input$back1, { hide("step2"); show("step1") })
  observeEvent(input$back2, { hide("step3"); if(mix()$has_white) show("step2") else show("step1") })
  observeEvent(input$to_step3, { hide("step2"); show("step3") })
  
  # Simply use input values directly with req() to ensure they're available
  # No need for intermediate reactive values
  calc <- reactive({
    # Use last valid values to prevent crashes when inputs are temporarily empty
    # Use isTRUE() to safely handle NA values in conditions
    area_num <- safe_input(input, "area", last_valid$area, function(x) x > 0)
    
    use_num <- as.numeric(input$use)  # Radio button, always has value
    substrate_num <- as.numeric(input$substrate)  # selectInput, always has value
    
    extra_oil_num <- safe_input(input, "extra_oil", last_valid$extra_oil, function(x) x >= 1)
    
    zinc_ratio_num <- safe_input(input, "zinc_ratio", last_valid$zinc_ratio, function(x) x >= 0)
    
    # Validate they're all numeric (should always be true now)
    req(is.numeric(area_num), is.numeric(use_num), is.numeric(substrate_num),
        is.numeric(extra_oil_num), is.numeric(zinc_ratio_num))
    
    # Paint coverage calculation
    # Baseline: 15 m²/L per coat for smooth wood
    # Substrate factor adjusts coverage:
    #   > 1.0 = smoother surface = better coverage (less paint needed)
    #   < 1.0 = rougher/porous = worse coverage (more paint needed)
    coverage_m2_per_liter <- 15  # baseline m²/L per coat
    
    # Total area to cover (area × number of coats)
    total_area_m2 <- area_num * use_num
    
    # Adjust coverage by substrate factor
    # Higher substrate factor = better coverage = less paint needed
    adjusted_coverage <- coverage_m2_per_liter * substrate_num
    
    # Calculate liters needed
    target_liters <- total_area_m2 / adjusted_coverage
    
    list(
      target_liters = round(target_liters, 2),
      area = area_num,
      use = use_num,
      substrate = substrate_num,
      extra_oil = extra_oil_num,
      zinc_ratio = zinc_ratio_num
    )
  })
  
  output$needed_volume <- renderText({
    tryCatch({
      format_swe(calc()$target_liters, 2)
    }, error = function(e) {
      paste("ERROR in needed_volume:", e$message)
    })
  })
  
  output$needed_pigment <- renderText({
    tryCatch({
      recipe <- final_recipe()
      paint_type <- input$paint_type %||% "linseed"
      
      if(paint_type == "egg_oil") {
        # Include extra filler in total for egg-oil tempera
        total_pigment <- recipe$zn + recipe$ti + sum(recipe$color) + recipe$filler_g
      } else {
        total_pigment <- recipe$zn + recipe$ti + sum(recipe$color)
      }
      
      format_swe(total_pigment, 0)
    }, error = function(e) {
      paste("ERROR in needed_pigment:", e$message)
    })
  })
  
  # Dynamic recipe header based on paint type
  output$recipe_header <- renderUI({
    paint_type <- input$paint_type %||% "linseed"
    
    if(paint_type == "egg_oil") {
      h3("Recept för äggoljetemperafärg")
    } else if(paint_type == "tar") {
      h3("Recept för tjäroljefärg")
    } else {
      h3("Recept för linoljefärgspasta")
    }
  })
  
  # Dynamic recipe description based on paint type
  output$recipe_description <- renderUI({
    paint_type <- input$paint_type %||% "linseed"
    
    tags$p("Du blandar cirka ", textOutput("total_volume", inline=TRUE), 
           " liter färdig färg, med sammanlagt ", textOutput("needed_pigment", inline=TRUE), 
           " g pigment.")
  })
  
  output$total_volume <- renderText({
    tryCatch({
      format_swe(total_paint_volume(), 2)
    }, error = function(e) {
      paste("ERROR in total_volume:", e$message)
    })
  })
  
  recipe_df <- reactive({
    r <- final_recipe()
    paint_type <- input$paint_type %||% "linseed"
    
    rows <- list()
    
    if(paint_type == "egg_oil") {
      # Egg-oil tempera recipe format
      rows <- c(rows, list(list("Kallpressad kokt linolja", r$oil)))
      rows <- c(rows, list(list(paste0("Ägg (", r$eggs_count, " st à 50 g)"), r$eggs)))
      rows <- c(rows, list(list("Vatten", r$water)))
      
      if(r$zn > 0.1) rows <- c(rows, list(list("Zinkvitt PW4 (#44100)", r$zn)))
      if(r$ti > 0.1) rows <- c(rows, list(list("Titanvitt Rutile PW6 (#44400)", r$ti)))
      
      for(id in names(r$color)) {
        rows <- c(rows, list(list(paste0(km[[id]]$name, " (#", id, ")"), as.numeric(r$color[id]))))
      }
      
      # Add extra filler last
      rows <- c(rows, list(list(paste0(km[[r$filler_id]]$name, " - extra fyllmedel (#", r$filler_id, ")"), r$filler_g)))
      
    } else if(paint_type == "tar") {
      # Tar oil paint recipe format
      rows <- c(rows, list(list(r$tar_category, r$tar)))
      rows <- c(rows, list(list("Kallpressad kokt linolja", r$oil)))
      rows <- c(rows, list(list("Balsamterpentin", r$balsamterpentin)))
      
      if(r$zn > 0.1) rows <- c(rows, list(list("Zinkvitt PW4 (#44100)", r$zn)))
      if(r$ti > 0.1) rows <- c(rows, list(list("Titanvitt Rutile PW6 (#44400)", r$ti)))
      
      for(id in names(r$color)) {
        rows <- c(rows, list(list(paste0(km[[id]]$name, " (#", id, ")"), as.numeric(r$color[id]))))
      }
      
    } else {
      # Linseed oil paint recipe format (original)
      rows <- c(rows, list(list("Kallpressad kokt linolja", r$oil)))
      if(r$zn > 0.1) rows <- c(rows, list(list("Zinkvitt PW4 (#44100)", r$zn)))
      if(r$ti > 0.1) rows <- c(rows, list(list("Titanvitt Rutile PW6 (#44400)", r$ti)))
      for(id in names(r$color)) {
        rows <- c(rows, list(list(paste0(km[[id]]$name, " (#", id, ")"), as.numeric(r$color[id]))))
      }
    }
    
    df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    colnames(df) <- c("Ingrediens", "Gram")
    df
  })
  
  # Calculate actual total volume of finished paint (pigment + oil)
  total_paint_volume <- reactive({
    recipe <- final_recipe()
    c <- calc()
    paint_type <- input$paint_type %||% "linseed"
    m <- mix()
    zinc_ratio <- c$zinc_ratio / 100
    
    # Common calculations (used by all paint types)
    normalized_pcts <- (m$pct / m$total) * 100
    compensated_pcts <- km_compensate_vitbas(normalized_pcts, m$ids, zinc_ratio)
    avg_density <- calculate_avg_density(m, compensated_pcts, zinc_ratio, km)
    
    # Packing factor constant
    PACKING_FACTOR <- 0.85
    
    if(paint_type == "egg_oil") {
      # Include filler density in weighted average
      filler_density <- km[[recipe$filler_id]]$density
      base_pigment_g <- recipe$zn + recipe$ti + sum(recipe$color)
      pigment_total_g <- base_pigment_g + recipe$filler_g
      
      if(pigment_total_g > 0) {
        avg_density <- (base_pigment_g * avg_density + recipe$filler_g * filler_density) / pigment_total_g
      }
      
      # Convert to volumes
      total_L <- (pigment_total_g / (avg_density * 1000) +
                    recipe$oil / 920 +
                    recipe$eggs / 1030 +
                    recipe$water / 1000) * PACKING_FACTOR
      
      return(round(total_L, 2))
    }
    
    if(paint_type == "tar") {
      pigment_total_g <- recipe$zn + recipe$ti + sum(recipe$color)
      
      total_L <- (pigment_total_g / (avg_density * 1000) +
                    recipe$tar / 1080 +
                    recipe$oil / 920 +
                    recipe$balsamterpentin / 868) * PACKING_FACTOR
      
      return(round(total_L, 2))
    }
    
    # Linseed oil paint (default)
    pigment_total_g <- recipe$zn + recipe$ti + sum(recipe$color)
    total_L <- (pigment_total_g / (avg_density * 1000) + recipe$oil / 920) * PACKING_FACTOR
    
    round(total_L, 2)
  })
  
  # === TAR OIL PAINT RECIPE CALCULATOR ===
  calculate_tar_oil_recipe <- function(c, m, zinc_ratio) {
    # Normalize and compensate
    normalized_pcts <- (m$pct / m$total) * 100
    compensated_pcts <- km_compensate_vitbas(normalized_pcts, m$ids, zinc_ratio)
    
    # Use generic helper for base properties
    props <- calculate_base_properties(m, compensated_pcts, zinc_ratio, km)
    
    # Use generic helper for pigment amounts
    amounts <- calculate_pigment_amounts(c$target_liters, props$oil_absorption, props$density)
    
    # Apply CPVC factor
    tar_extra_oil <- input$tar_extra_oil %||% 1.6
    total_oil_with_factor <- amounts$base_oil_g * tar_extra_oil
    
    # Split: 50% tar, 50% oil (+20% compensation)
    tar_g <- total_oil_with_factor * 0.5
    linseed_oil_g <- total_oil_with_factor * 0.5 * 1.2
    balsamterpentin_g <- tar_g
    
    # Use generic helper to distribute pigments
    pigments <- distribute_pigments(m, compensated_pcts, amounts$total_pigment_g, zinc_ratio)
    
    list(
      zn = smart_round(pigments$zn), 
      ti = smart_round(pigments$ti), 
      color = sapply(pigments$color, smart_round),
      tar_category = input$tar_category,
      tar = smart_round(tar_g),
      oil = smart_round(linseed_oil_g),
      balsamterpentin = smart_round(balsamterpentin_g),
      hex = final_hex()
    )
  }
  
  # === EGG-OIL TEMPERA RECIPE CALCULATOR ===
  calculate_egg_oil_recipe <- function(c, m, zinc_ratio) {
    # Normalize and compensate
    normalized_pcts <- (m$pct / m$total) * 100
    compensated_pcts <- km_compensate_vitbas(normalized_pcts, m$ids, zinc_ratio)
    
    # Use generic helpers for base properties
    props <- calculate_base_properties(m, compensated_pcts, zinc_ratio, km)
    amounts <- calculate_pigment_amounts(c$target_liters, props$oil_absorption, props$density)
    
    # Add extra filler (20% overshoot)
    filler_id <- input$egg_filler
    extra_filler_volume_L <- amounts$pigment_volume_L * 0.20
    extra_filler_g <- extra_filler_volume_L * 1000 * km[[filler_id]]$density
    
    # Split oil into oil + eggs (50/50)
    linseed_oil_g <- amounts$base_oil_g * 0.5
    eggs_g <- amounts$base_oil_g * 0.5
    eggs_count <- eggs_g / 50
    water_g <- amounts$base_oil_g
    
    # Use generic helper to distribute pigments
    pigments <- distribute_pigments(m, compensated_pcts, amounts$total_pigment_g, zinc_ratio)
    
    list(
      zn = smart_round(pigments$zn), 
      ti = smart_round(pigments$ti), 
      color = sapply(pigments$color, smart_round),
      filler_id = filler_id,
      filler_g = smart_round(extra_filler_g),
      oil = smart_round(linseed_oil_g),
      eggs = smart_round(eggs_g),
      eggs_count = round(eggs_count, 1),
      water = smart_round(water_g),
      hex = final_hex()
    )
  }
  
  # === KUBELKA-MUNK COMPENSATION FOR VITBAS ===
  # When vitbas is present, adjust colored pigment amounts to maintain constant color
  # as zinc/titanium ratio changes from a reference point
  km_compensate_vitbas <- function(normalized_pcts, ids, zinc_ratio) {
    # Only compensate if vitbas is present
    if (!("vitbas" %in% ids)) return(normalized_pcts)
    
    # K and S values for whites
    K_zinc <- 0.00
    S_zinc <- 1.66
    K_titanium <- 0.00
    S_titanium <- 2.55
    
    # REFERENCE POINT: 15% zinc baseline
    zinc_ratio_ref <- 0.15
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
    K_vals <- sapply(colored_ids, function(id) km[[id]]$K)
    S_vals <- sapply(colored_ids, function(id) km[[id]]$S)
    
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
  
  
  final_recipe <- reactive({
    c <- calc()  # Get all values from calc()
    m <- mix()
    zinc_ratio <- c$zinc_ratio / 100
    
    # Check paint type and route to appropriate calculator
    paint_type <- input$paint_type %||% "linseed"  # Default to linseed
    
    if(paint_type == "egg_oil") {
      # Egg-oil tempera recipe
      return(calculate_egg_oil_recipe(c, m, zinc_ratio))
    }
    
    if(paint_type == "tar") {
      # Tar oil paint recipe
      return(calculate_tar_oil_recipe(c, m, zinc_ratio))
    }
    
    # === LINSEED OIL PAINT RECIPE (original logic) ===
    target_liters <- c$target_liters
    
    # CRITICAL FIX: Always normalize percentages to 100%
    # Regardless of what user entered, treat their ratios as parts of 100%
    normalized_pcts <- (m$pct / m$total) * 100
    
    # === APPLY KUBELKA-MUNK COMPENSATION ===
    compensated_pcts <- km_compensate_vitbas(normalized_pcts, m$ids, zinc_ratio)
    
    # Use generic helpers for base properties and amounts
    props <- calculate_base_properties(m, compensated_pcts, zinc_ratio, km)
    amounts <- calculate_pigment_amounts(target_liters, props$oil_absorption, props$density)
    
    # Apply CPVC factor to oil
    final_oil_g <- amounts$base_oil_g * c$extra_oil
    
    # Use generic helper to distribute pigments
    pigments <- distribute_pigments(m, compensated_pcts, amounts$total_pigment_g, zinc_ratio)
    
    list(zn=smart_round(pigments$zn), ti=smart_round(pigments$ti), 
         color=sapply(pigments$color, smart_round), 
         oil=smart_round(final_oil_g), hex=final_hex())
  })
  
  output$final_recipe <- renderTable({
    df <- recipe_df()
    # Format the Gram column with Swedish decimals
    df$Gram <- sapply(df$Gram, function(x) format_swe(parse_numeric(x), 1))
    df
  }, striped=TRUE, bordered=TRUE, width="100%", align="lr", sanitize.text.function = function(x) x)
  output$final_preview <- renderUI({
    # Force explicit dependencies on color-affecting reactives
    m <- mix()  # Depend on pigment mix
    current_color()  # Depend on current color calculation
    
    # Re-render preview with current final_hex value
    render_preview(final_hex(), "final_preview")
  })
  
  output$download_txt <- downloadHandler(
    filename = function() paste0("fargrecept_", Sys.Date(), ".txt"),
    content = function(file) {
      df <- recipe_df()
      recipe <- final_recipe()
      c <- calc()  # Get calc values
      
      # Get paint type
      paint_type <- input$paint_type %||% "linseed"
      paint_type_name <- switch(paint_type,
                                "linseed" = "Linoljefärg",
                                "egg_oil" = "Äggoljetemperafärg",
                                "tar" = "Tjäroljefärg",
                                "Linoljefärg")  # fallback
      
      txt <- paste0(strrep("=", 60), "\n")
      txt <- paste0(txt, "Paint-o-matic – recept ", Sys.Date(), "\n")
      txt <- paste0(txt, strrep("=", 60), "\n\n",
                    "Typ av färg: ", paint_type_name, "\n",
                    "Färgkod: ", final_hex(), "\n",
                    "Yta: ", format_swe(c$area, 0), " m²\n\n")
      
      # Recipe ingredients
      for(i in 1:nrow(df)) {
        gram_val <- format_swe(parse_numeric(df[i,2]), 1)
        txt <- paste0(txt, df[i,1], ": ", gram_val, " g\n")
      }
      
      # Add sharing URL section
      txt <- paste0(txt, "\n", strrep("=", 60), "\n")
      txt <- paste0(txt, "Dela och samarbeta om receptet med andra\n")
      txt <- paste0(txt, strrep("=", 60), "\n\n")
      
      # Generate share URL using helper
      share_url <- generate_share_url(session, input = input, mix_data = mix())
      
      if(!is.null(share_url)) {
        txt <- paste0(txt, "Återskapa detta recept genom att öppna följande länk, som återställer alla ingredienser, vikter och inställningar:\n")
        txt <- paste0(txt, share_url, "\n")
      } else {
        txt <- paste0(txt, "Ingen delningslänk tillgänglig.\n")
      }
      
      # Add sourcing section
      txt <- paste0(txt, "\n", strrep("=", 60), "\n")
      txt <- paste0(txt, "Skaffa ingredienserna till din färg här\n")
      txt <- paste0(txt, strrep("=", 60), "\n\n")
      
      # Collect all pigment IDs used in recipe
      pigment_ids <- c()
      if(recipe$zn > 0.1) pigment_ids <- c(pigment_ids, "44100")
      if(recipe$ti > 0.1) pigment_ids <- c(pigment_ids, "44400")
      if(length(recipe$color) > 0) pigment_ids <- c(pigment_ids, names(recipe$color))
      
      # Add supplier links for each pigment
      suppliers_found <- FALSE
      for(id in pigment_ids) {
        # Check both suppliers and suppliers
        match_info <- NULL
        if(id %in% names(suppliers)) {
          match_info <- suppliers[[id]]
        } else if(id %in% names(suppliers)) {
          match_info <- suppliers[[id]]
        }
        
        if(!is.null(match_info)) {
          suppliers_found <- TRUE
          
          txt <- paste0(txt, km[[id]]$name, "\n")
          
          # Kremer Pigmente
          if(!is.null(match_info$kremer_match)) {
            txt <- paste0(txt, "  Kremer Pigmente:\n")
            txt <- paste0(txt, "    - Matchning: ", match_info$kremer_match, "\n")
            txt <- paste0(txt, "    - Produkt-ID: ", match_info$kremer_id, "\n")
            
            # Handle multiple URLs (e.g., for GO94_GU30)
            if(length(match_info$kremer_url) > 1) {
              txt <- paste0(txt, "    - Webbadresser:\n")
              for(url in match_info$kremer_url) {
                txt <- paste0(txt, "      ", url, "\n")
              }
            } else {
              txt <- paste0(txt, "    - Webbadress: ", match_info$kremer_url, "\n")
            }
          }
          
          # Ottosson Färgmakeri
          if(!is.null(match_info$ottosson_match)) {
            txt <- paste0(txt, "  Ottosson Färgmakeri (Sverige):\n")
            txt <- paste0(txt, "    - Produkt: ", match_info$ottosson_match, "\n")
            txt <- paste0(txt, "    - Webbadress: ", match_info$ottosson_url, "\n")
          }
          
          # Claessons Trätjära
          if(!is.null(match_info$claessons_match)) {
            txt <- paste0(txt, "  Claessons Trätjära (Sverige):\n")
            txt <- paste0(txt, "    - Produkt: ", match_info$claessons_match, "\n")
            txt <- paste0(txt, "    - Webbadress: ", match_info$claessons_url, "\n")
          }
          
          # Gysinge
          if(!is.null(match_info$gysinge_match)) {
            txt <- paste0(txt, "  Gysinge (Sverige):\n")
            txt <- paste0(txt, "    - Produkt: ", match_info$gysinge_match, "\n")
            txt <- paste0(txt, "    - Webbadress: ", match_info$gysinge_url, "\n")
          }
          
          txt <- paste0(txt, "  Notering: ", match_info$notes, "\n\n")
        }
      }
      
      # Add general supplier info
      if(suppliers_found) {
        #txt <- paste0(txt, strrep("-", 60), "\n")
        txt <- paste0(txt, "\n", strrep("=", 60), "\n")
        txt <- paste0(txt, "Pålitliga leverantörer till byggnadsvårdare\n")
        txt <- paste0(txt, strrep("=", 60), "\n\n")
        txt <- paste0(txt, "Kremer Pigmente GmbH & Co. KG (Tyskland)\n")
        txt <- paste0(txt, "  Webbplats: https://www.kremer-pigmente.com/en/shop/pigments\n")
        txt <- paste0(txt, "  Internationell leverantör av högkvalitativa pigment\n\n")
        txt <- paste0(txt, "Ottosson Färgmakeri (Sverige)\n")
        txt <- paste0(txt, "  Webbplats: https://ottossonfarg.com/\n")
        txt <- paste0(txt, "  Svensktillverkad linoljefärg och pigment\n\n")
        txt <- paste0(txt, "Claessons Trätjära (Sverige)\n")
        txt <- paste0(txt, "  Webbplats: https://claessons.com/pigment/\n")
        txt <- paste0(txt, "  Byggnadsvård och pigment\n\n")
        txt <- paste0(txt, "Andra svenska leverantörer:\n")
        txt <- paste0(txt, "  - Gysinge: https://gysinge.se/\n")
      } else {
        txt <- paste0(txt, "Inga leverantörslänkar tillgängliga för dessa pigment.\n")
        txt <- paste0(txt, "Kontakta Kremer Pigmente, Ottosson, Claessons eller Gysinge.\n")
      }
      
      con <- file(file, open = "wt", encoding = "UTF-8")
      writeLines(txt, con)
      close(con)
    }
  )
  
  # === GENERATE AND UPDATE SHARE URL ===
  observe({
    # Generate share URL using helper
    share_url <- generate_share_url(session, input = input)
    
    # Update hidden input with share URL
    if(!is.null(share_url)) {
      runjs(sprintf("document.getElementById('share_url_hidden').value = '%s';", share_url))
    }
  })
  
  # === COPY SHARE LINK BUTTON ===
  observeEvent(input$copy_share_link, {
    runjs("
      var url = document.getElementById('share_url_hidden').value;
      if(url) {
        navigator.clipboard.writeText(url).then(function() {
          alert('Delningslänk kopierad till urklipp');
        }).catch(function(err) {
          prompt('Kopiera denna länk:', url);
        });
      } else {
        alert('Välj pigment först för att skapa en delningslänk.');
      }
    ")
  })
  
  observeEvent(input$restart, {
    final_hex("#FFFFFF")
    hide("step3"); hide("step2"); show("step1")
  })
}

shinyApp(ui, server)