# Paint-o-matic -- blanda din egen linoljefärg!
# Copyright 2025 Tobias Hagberg
# Licens: GNU General Public License v3.0
# https://github.com/hmep/karlslund/tree/main/paint-o-matic

# Main app file - data and utilities loaded from global.R
source("global.R")

# === APP-SPECIFIC HELPER FUNCTIONS ===
# Functions that are tightly coupled to Shiny UI/server context

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
        # Use tar ID directly
        if(!is.null(input$tar_id)) {
          params$tar_id <- input$tar_id
        }
        if(!is.null(input$tar_extra_oil) && input$tar_extra_oil != 1.6) 
          params$tar_extra_oil <- input$tar_extra_oil
      }
    }
    
    # Add color name if provided (from either step 1 or step 3)
    color_name <- input$color_name_step3 %||% input$color_name %||% ""
    if(nchar(color_name) > 0) {
      params$name <- color_name
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

make_choices <- function(ids) {
  setNames(ids, sapply(ids, function(id) paste0(pigments_db[[id]]$name, " (#", id, ")")))
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
  all_ids <- names(pigments_db)
  base_pigments <- setdiff(all_ids, exclude_ids)
  
  base_pigments
}

# Generate swatch code for a pigment
# Generate all swatches for all base pigments (including RAÄ) - uses generic matrix generator  
generate_all_extended_swatches <- function(shade_pigment_id = "44450") {
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

# Note: extended_swatches generated reactively in server function (not here)

ui <- dashboardPage(
  dashboardHeader(
    title = "Paint-o-matic",
    # Version number (right side, small text)
    tags$li(
      class = "dropdown",
      tags$a(href = "https://github.com/hmep/karlslund/blob/main/paint-o-matic/LICENSE", class = "version-text", "v0.10.4-db, © 2025 Tobias Hagberg, licens GPLv3")
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
      /* Layout */
      .content-wrapper {background:#ccc !important;}
      .step {padding:24px 24px 64px; background:#fff; border-radius:12px; margin:20px auto 80px; position:relative; min-width:360px; max-width:840px;}
      .footer-ref {position:relative; bottom:-44px; left:0; right:0; font-size:12px; color:#555; text-align:center; padding:12px 12px 0; border-top:1px solid #ddd;}
      .ready-box {padding:20px;}
      .ready-box h3, h2 {margin:0 0 .5em; padding:0;}
      .rmargin-box {margin-right:20px;}
      
      /* Preview and swatches */
      .preview {display:block; height:300px; width:300px; border:8px solid #333; border-radius:150px; margin:auto;}
      .kulturkulor-swatch {display:inline-block; width:24px; height:24px; border-radius:50%; margin:3px; cursor:pointer; border:2px solid #999; transition:transform 0.1s, border-color 0.1s;}
      .kulturkulor-swatch:hover {transform:scale(1.3); border-color:#333; z-index:10; position:relative;}
      .kulturkulor-gallery {max-height:200px; overflow-y:auto; overflow-x:hidden; padding:8px; background:#fff; border:1px solid #ddd; border-radius:4px; margin-top:8px;}
      
      /* Boxes and alerts */
      .normalized-box, .info-box, .alert {background:#eee; color:black; border:0; padding:12px; border-radius:6px; margin:1em 0;}
      .normalized-box {margin:10px 0;}
      .paint-type-box {background:#f8f9fa; border:1px solid #dee2e6; border-radius:8px; padding:20px; margin-top:15px;}
      
      /* Buttons */
      .btn {margin:.5em .5em 0 0;}
      .btn-x {margin:0;}
      .btn-primary {color:white;}
      .btn, .back-btn {display:inline-flex; flex-direction:row; align-items:center;}
      .btn i, .back-btn i {margin-right:6px; margin-left:0;}
      .next-btn {display:inline-flex; flex-direction:row-reverse; align-items:center;}
      .next-btn i {margin-left:6px; margin-right:0;}
      
      /* Tables */
      table tr td {white-space:nowrap;}
      table tr td:first-of-type {white-space:wrap;}
      .navbar-custom-menu .navbar-nav > li > a.version-text {font-size:11px; color:#aaa; padding:15px 0;}
      
      /* Fullscreen preview - shared button styles */
      .preview-container {position:relative; display:inline-block;}
      .zoom-icon, .fullscreen-close {background:white; border:none; border-radius:50%; cursor:pointer; color:#333; transition:all 0.2s; display:flex; align-items:center; justify-content:center;}
      .zoom-icon {position:absolute; top:4px; right:4px; width:36px; height:36px; font-size:24px; font-weight:300; line-height:1; box-shadow:0 2px 4px rgba(0,0,0,0.3);}
      .zoom-icon:hover, .fullscreen-close:hover {background:black; color:white; transform:scale(1.1);}
      .fullscreen-overlay {display:none; position:fixed; top:0; left:0; width:100%; height:100%; background:rgba(0,0,0,0.95); z-index:9999; justify-content:center; align-items:center;}
      .fullscreen-overlay.active {display:flex;}
      .fullscreen-preview {width:100%; height:100%; border:0; position:relative;}
      .fullscreen-color-name {position:absolute; bottom:60px; left:0; right:0; text-align:center; font-size:16px; font-weight:300; letter-spacing:0.5px; padding:0 20px; transition:color 0.3s;}
      .fullscreen-close {position:absolute; top:20px; right:30px; width:50px; height:50px; font-size:30px; box-shadow:0 4px 8px rgba(0,0,0,0.3); z-index:10000;}
    "))),
    
    tags$script(HTML('
      // Helper function to calculate luminance and choose text color
      function getTextColorForBackground(bgColor) {
        // Parse RGB from background color string
        var rgb = bgColor.match(/\\d+/g);
        if (!rgb || rgb.length < 3) return "white";
        
        var r = parseInt(rgb[0]);
        var g = parseInt(rgb[1]);
        var b = parseInt(rgb[2]);
        
        // Calculate relative luminance (WCAG formula)
        var luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
        
        // Return black for light backgrounds, white for dark backgrounds
        return luminance > 0.5 ? "black" : "white";
      }
      
      // Fullscreen preview functionality
      function openFullscreen(previewId) {
        var preview = document.querySelector("#" + previewId + " .preview");
        if (!preview) return;
        
        var color = window.getComputedStyle(preview).backgroundColor;
        var overlay = document.getElementById("fullscreen-overlay");
        var fullPreview = document.getElementById("fullscreen-preview");
        var colorNameDiv = document.getElementById("fullscreen-color-name");
        
        fullPreview.style.background = color;
        overlay.classList.add("active");
        document.body.style.overflow = "hidden"; // Prevent scrolling
        
        // CRITICAL: Always clear the div first to prevent stale content
        if (colorNameDiv) {
          colorNameDiv.textContent = "";
          colorNameDiv.style.display = "none";
        }
        
        // Get color name from input field (try step 3 first, then step 1)
        var colorName = "";
        var colorNameStep3 = document.getElementById("color_name_step3");
        var colorNameStep1 = document.getElementById("color_name");
        
        if (colorNameStep3 && colorNameStep3.value) {
          colorName = colorNameStep3.value;
        } else if (colorNameStep1 && colorNameStep1.value) {
          colorName = colorNameStep1.value;
        }
        
        // Update color name display (only if there is a name)
        if (colorName && colorNameDiv) {
          colorNameDiv.textContent = colorName;
          colorNameDiv.style.display = "block";
          
          // Set text color based on background luminance
          colorNameDiv.style.color = getTextColorForBackground(color);
        }
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
    
    # Favorites localStorage JavaScript
    tags$script(HTML('
      // Favorites management with localStorage
      const MAX_FAVORITES = 50;
      const STORAGE_KEY = "paintomatic_favorites";
      
      // Get all favorites from localStorage
      function getFavorites() {
        try {
          const data = localStorage.getItem(STORAGE_KEY);
          if (!data) return [];
          return JSON.parse(data);
        } catch(e) {
          console.error("Error loading favorites:", e);
          localStorage.removeItem(STORAGE_KEY);
          return [];
        }
      }
      
      // Save all favorites to localStorage
      function saveFavorites(favorites) {
        try {
          localStorage.setItem(STORAGE_KEY, JSON.stringify(favorites));
          return true;
        } catch(e) {
          console.error("Error saving favorites:", e);
          return false;
        }
      }
      
      // Add a new favorite
      function addFavorite(favorite) {
        let favorites = getFavorites();
        
        // Check limit
        if (favorites.length >= MAX_FAVORITES) {
          alert("Du har nått gränsen på " + MAX_FAVORITES + " sparade favoriter. Ta bort några för att spara fler.");
          return false;
        }
        
        // Add timestamp and ID
        favorite.id = Date.now().toString();
        favorite.timestamp = new Date().toISOString();
        
        // Add to beginning of array (most recent first)
        favorites.unshift(favorite);
        
        return saveFavorites(favorites);
      }
      
      // Delete a favorite by ID
      function deleteFavorite(id) {
        let favorites = getFavorites();
        favorites = favorites.filter(f => f.id !== id);
        saveFavorites(favorites);
        
        // Update Shiny with new list
        Shiny.setInputValue("favorites_list", JSON.stringify(getFavorites()));
        Shiny.setInputValue("favorites_updated", Math.random(), {priority: "event"});
      }
      
      // Clear all favorites
      function clearAllFavorites() {
        if (confirm("Är du säker på att du vill ta bort alla sparade favoriter?")) {
          localStorage.removeItem(STORAGE_KEY);
          Shiny.setInputValue("favorites_list", JSON.stringify([]));
          Shiny.setInputValue("favorites_updated", Math.random(), {priority: "event"});
        }
      }
      
      // Send favorites to Shiny when connected
      $(document).on("shiny:connected", function() {
        Shiny.setInputValue("favorites_list", JSON.stringify(getFavorites()));
      });
      
      // Custom message handlers
      Shiny.addCustomMessageHandler("save_favorite", function(favorite) {
        if (addFavorite(favorite)) {
          Shiny.setInputValue("favorites_list", JSON.stringify(getFavorites()));
        }
      });
      
      Shiny.addCustomMessageHandler("clear_all_favorites", function(msg) {
        clearAllFavorites();
        Shiny.setInputValue("favorites_list", JSON.stringify(getFavorites()));
      });
    ')),
    
    # Fullscreen overlay (shared for both previews)
    tags$div(id = "fullscreen-overlay", class = "fullscreen-overlay", onclick = "closeFullscreen()",
             tags$button(class = "fullscreen-close", onclick = "closeFullscreen()", 
                         HTML("&times;")),
             tags$div(id = "fullscreen-preview", class = "fullscreen-preview",
                      tags$div(id = "fullscreen-color-name", class = "fullscreen-color-name", 
                               style = "display: none;")
             )
    ),
    
    hidden(div(id="step1", class="step",
               h2("Blanda pigment till önskad kulör"),
               fluidRow(
                 column(6,
                        h5(style="font-weight:bold;","Inställningar"),
                        checkboxInput("raa_only", "Använd endast Kulturkulör-pigment (RAÄ)", FALSE),
                        #checkboxInput("use_tinting_strength","Avancerad färgblandning",TRUE),
                        #tags$small(style="color:#666; margin-left:20px; display:block; margin-top:-1em; margin-bottom:10px;","Väger pigment efter faktiska färgstyrka (K- och S-värden)"),
                        hr(),
                        pickerInput("p1", "Pigment 1", choices = all_choices, selected = "vitbas",
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p1", sliderInput("pct1","Andel (%)",0,100,20,1)),
                        pickerInput("p2", "Pigment 2", choices = all_choices, selected = "J225",
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p2", sliderInput("pct2","Andel (%)",0,100,80,1)),
                        pickerInput("p3", "Pigment 3", choices = all_choices, selected = "",
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p3", sliderInput("pct3","Andel (%)",0,100,0,1)),
                        pickerInput("p4", "Pigment 4", choices = all_choices, selected = "",
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p4", sliderInput("pct4","Andel (%)",0,100,0,1)),
                        hr(),
                        textInput("color_name", "Valfritt kulörnamn", 
                                  value = "", 
                                  placeholder = "Dörrkarm 1923"),
                        actionButton("save_favorite", "Spara som favoritkulör", class="btn-default btn-sm btn-x", 
                                     icon = icon("star")),
                        hr(),
                        actionButton("reset_pigments", "Nollställ pigment", class="btn-default", icon = icon("refresh")),
                 ),
                 column(6,
                        h3("Färgprov"),
                        uiOutput("preview1"), br(),
                        tags$b("Total andel: "), textOutput("total_pct",inline=TRUE), " %", 
                        uiOutput("total_warning"), 
                        tags$div(style="margin-top:2em;",
                                 h5(style="font-weight:bold;","Favoritkulörer och färdiga mixer/paletter"),
                                 selectInput("recipe_set", NULL,
                                             choices = list(
                                               "Riksantikvarieämbetet (RAÄ) Kulturkulör" = "raa",
                                               "Paint-o-matic-kulörer" = "extended",
                                               "Sparade favoritkulörer" = "saved"
                                             ),
                                             selected = "raa"),
                                 
                                 # Show description based on selected set
                                 conditionalPanel(
                                   condition = "input.recipe_set == 'raa'",
                                   tags$small(a("Kulturkulör från Riksantikvarieämbetet (RAÄ)", href="https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/")," är ett system för historiskt trogen färgsättning med jordpigment och järnoxider."),
                                   br(), br(),
                                   #selectInput("shading_pigment_raa", "Skuggningspigment",
                                   #             choices = shading_pigments,
                                   #             selected = "J318")
                                 ),
                                 
                                 conditionalPanel(
                                   condition = "input.recipe_set == 'extended'",
                                   tags$small("Kulörpaletter med tonings- och skuggningsmixer för alla pigment som är tillgängliga i Paint-o-matic. Modifiera gärna mixen efter eget tycke!"),
                                   br(), br(),
                                   #selectInput("shading_pigment", "Skuggningspigment",
                                   #             choices = shading_pigments,
                                   #             selected = "J318")
                                 ),
                                 
                                 conditionalPanel(
                                   condition = "input.recipe_set == 'saved'",
                                   tags$small("Alla dina sparade favorit-kulörblandningar lagras på din enhet."),
                                   br(), br()
                                 ),
                                 
                                 div(style = "width: 100%; height: 300px; overflow-y: auto; overflow-x: auto; border: 1px solid #ddd; padding: 10px;",
                                     uiOutput("recipe_swatches")
                                 )
                        )
                 )
               ),
               hr(),
               actionButton("to_step2","Nästa", class="btn-primary next-btn", icon = icon(class="icon-next","circle-arrow-right")),
               div(class="footer-ref", "Masstone baserad på data från Riksantikvarieämbetet (RAÄ) Kulturkulör, Kremer Pigmente, m. fl. Mixningen tar hänsyn till pigmentens ljusbrytande egenskaper. Tänk på att en skärm inte exakt kan återge hur ögat uppfattar ljus som träffar en målad yta.")
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
                 sliderInput("zinc_ratio","Andel zinkvitt i vitbasen (%)",0,100,15,1,post="% zinkoxid"),
               ), ),
               hr(),
               actionButton("back1","Föregående", class="btn-default back-btn", icon = icon("circle-arrow-left")),
               actionButton("to_step3","Nästa", class="btn-primary next-btn", icon = icon(class="icon-next","circle-arrow-right")),
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
                                      "Äggoljetempera" = "egg_oil",
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
                                                 "Tidigare målat trä " =	1.2,	            # Previously painted wood
                                                 "Hyvlat trä (normal åtgång)" = 1.0,	       # Planed wood (baseline)
                                                 "Sågat trä" = 0.8,                          # Rough sawn wood
                                                 "Porös puts, gips (högst åtgång)" = 0.45    # Porous (gypsum, rough masonry)
                                               ),
                                               selected = 1.0),
                                   hr(),
                                   sliderInput("extra_oil","Extra kokt linolja (CPVC-faktor)",1,2.5,1.6,0.05,post="× CPVC"),
                                   p("Reglaget ökar endast mängden kokt linolja i receptet (pigmentmängderna är fixerade). En viss mängd extra bindmedel, utöver den minsta mängd som krävs för pigmenten, underlättar både tillredningen av färgen med blandare i borrmaskin och dess strykbarhet med penseln. Du kan utan problem lägga till olja upp till 1,6–2,2 gånger av CPVC."),
                                   hr(),
                                   p("Pastan du blandar är lämplig direkt som ", tags$b("grundstrykning"), " med gnuggande målningsstil (enligt principen från magert till fett) och utgör basen för ett komplett system för linoljefärgsmålning."),
                                   p("Till färg för ", tags$b("mellanstrykning"), " kan du tillföra ytterligare kokt linolja, precis upp till den maximala mängd som fortfarande medger att färgen struken på en glasskiva förblir ogenomskinlig."),
                                   p("Till färg för ", tags$b("slutstrykning"), " kan du därutöver med fördel tillsätta 10% kokt eller ännu hellre soloxiderad olja."),
                                   p("En burk till alla strykningar – tillsätt bara lite mer linolja efter hand!"),
                                   p("Var medveten om brandrisken, särskilt när du hanterar trasor och material som innehåller kokt linolja. Blöt dem i vatten och förvara dem i en tät behållare när du målat klart.")
                                   )
                        ),
                        
                        # Äggoljetemperafärg settings
                        conditionalPanel(
                          condition = "input.paint_type == 'egg_oil'",
                          tags$div(class = "paint-type-box",
                                   selectInput("egg_filler", "Val av fyllmedel",
                                               choices = create_filler_choices(),
                                               selected = "58000"),
                                   p("Alla fyllmedel gör äggoljetemperan matt och behaglig för inomhusbruk, men valet du gör kan också påverka ytterligare egenskaper hos temperan."),
                                   p("För att ge den målade ytan en fin putskänsla och fylla upp ojämnheter, välj " ,tags$b("kiselgur (diatoméjord)"), " som består av mikroskopiska, vassa fragment av fossiliserade kiselalger."),
                                   p("De olika " ,tags$b("kritorna"), " gör färgen matt men med olika effekt. Kritan från Ruegen är aningen grövre än den från Champagne, och den vita av sten har minst kornstorlek. Krita är det vanligaste fyllmedlet eftersom det är näst intill genomskinligt i oljan och därför endast knappt förändrar kulören."),
                                   p("För att bättre än annars fylla i små ojämnheter i underlaget och få en sammetslen yta, testa " ,tags$b("bentonit"), " som sväller i äggoljetemperan och hindrar att färgen rinner. Det hänger samman med att färgen blir tixotop, det vill säga lättflytande i penseldragen men strax formstabil när penseln lyfts."),
                                   p("En annan lera som också gör färgen tixotrop och ger en len yta är " ,tags$b("kaolin"), " – som sväller mindre och blir mindre geléartad än bentonit."),
                                   hr(),
                                   p("Måla äggoljetemperan med platt och bred pensel som håller mycket färg, i svepande rörelser i olika riktningar, eller med en fin roller (alltid vått i vått). Vänta till nästa strykning med att rätta till misstag eller luckor i färgen, om du går tillbaka och gör om arbetar du bara fram olja till ytan som blir flammig.")
                          )
                        ),
                        
                        # Tjäroljefärg settings
                        conditionalPanel(
                          condition = "input.paint_type == 'tar'",
                          tags$div(class = "paint-type-box",
                                   selectInput("tar_id", "Typ av trätjära",
                                               choices = setNames(
                                                 c("TAR01", "TAR02", "TAR03"),
                                                 sapply(c("TAR01", "TAR02", "TAR03"), function(id) misc_db[[id]]$name)
                                               ),
                                               selected = "TAR01"),
                                   p("Tjärfärg lämpar sig bäst med inte alltför ljusa kulörer, eftersom tjäran i sig kan vara ganska mörk. Om du vill blanda en ljus tjärfärg, välj den finaste och ljusaste trätjäran, den är ljust honungsgul. För svarta eller andra mörka eller klara (blå, gröna, röda) kulörer går det lika bra med de billigare alternativen."),
                                   sliderInput("tar_extra_oil", "Extra olja och tjära (CPVC-faktor)", 
                                               1, 2.5, 1.6, 0.05, post = "× CPVC"),
                                   p("Reglaget ökar mängden olja och tjära proportionellt, utöver den minsta mängd som de ingående pigmenten kräver. Högre värde ger mer flytande färg och bättre strykbarhet. Du kan utan problem lägga till olje- och tjärblandning upp till 1,6–2,2 gånger av CPVC."),
                                   hr(),
                                   p("När du målar med tjäroljefärg (som man också kan kalla pigmenterad roslagsmahogny, eftersom receptet också innehåller balsamterpentin), tänk på följande:"),
                                   p("För bästa strykbarhet, måla i sol och värme och värm gärna också tjärfärgen till 50–70° C (inte högre!) i ett vattenbad eller med en termostatstyrd oljevärmare."),
                                   p("Använd en pensel för att stryka ut den färgen i träets längdriktning. Torktiden kan variera från några dagar till flera veckor beroende på temperatur och fukt."),
                                   p("Var medveten om brandrisken, särskilt när du hanterar trasor och material som innehåller kokt linolja. Blöt dem i vatten och förvara dem i en tät behållare när du målat klart.")
                                   )
                        )
                        
                 ),
                 column(6,class="ready-box",
                        uiOutput("recipe_header"),
                        uiOutput("recipe_description"),
                        uiOutput("final_preview"),br(),
                        tableOutput("final_recipe"),
                        hr(),
                        textInput("color_name_step3", "Valfritt kulörnamn", 
                                  value = "", 
                                  placeholder = "Dörrkarm 1923"),
                        actionButton("save_favorite", "Spara som favoritkulör", class="btn-default btn-sm btn-x", 
                                     icon = icon("star")),
                        hr(),
                        downloadButton("download_txt","Spara recept som textfil",class="btn btn-primary"),
                        actionButton("copy_share_link", "Dela Länk", class="btn btn-default", icon=icon("link")),
                        tags$input(id="share_url_hidden", type="hidden", value="")
                 )
               ),
               hr(),
               actionButton("back2","Föregående", class="btn-default back-btn", icon = icon("circle-arrow-left")),
               actionButton("restart","Börja om från början", class="btn-default back-btn", icon = icon("fast-backward")),
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
  
  # === HELPER FUNCTIONS ===
  
  # Clear all pigment slots
  clear_all_pigments <- function() {
    updatePickerInput(session, "p1", selected = character(0))
    updateSliderInput(session, "pct1", value = 0)
    updatePickerInput(session, "p2", selected = character(0))
    updateSliderInput(session, "pct2", value = 0)
    updatePickerInput(session, "p3", selected = character(0))
    updateSliderInput(session, "pct3", value = 0)
    updatePickerInput(session, "p4", selected = character(0))
    updateSliderInput(session, "pct4", value = 0)
  }
  
  # Safe value extraction from list/favorites
  safe_get <- function(obj, key, default = "") {
    if(is.list(obj) && key %in% names(obj)) {
      val <- obj[[key]]
      if(is.null(val) || length(val) == 0) return(default)
      return(val)
    }
    return(default)
  }
  
  # Safe input value extraction with validation
  # Args:
  #   input: Shiny input object
  #   name: Input field name
  #   default: Default value to return if validation fails
  #   validator: Function that returns TRUE for valid values
  safe_input <- function(input, name, default, validator) {
    val <- input[[name]]
    if(is.null(val) || is.na(val) || !validator(val)) {
      return(default)
    }
    return(val)
  }
  
  # Create filtered grouped choices for picker inputs
  # Used when switching between RAÄ-only and all pigments modes
  create_filtered_grouped_choices <- function(filter_ids) {
    list(
      "Vitbas" = make_choices(intersect(c("vitbas"), filter_ids)),
      "Gröna" = make_choices(intersect(c("40400", "41700", "11100", "KG83", "ZG65", "40850", "40860", "GU30"), filter_ids)),
      "Svarta" = make_choices(intersect(c("44450", "J318", "BS98", "47501", "47400"), filter_ids)),
      "Blåa" = make_choices(intersect(c("11670", "UB88", "KB28"), filter_ids)),
      "Terra & Pozzuoli" = make_choices(intersect(c("40820", "40800", "40830", "BT44", "OT46"), filter_ids)),
      "Gula & Ockror" = make_choices(intersect(c("44082", "44086", "44150", "44160", "J920", "LO92", "GO94", "GO94_GU30"), filter_ids)),
      "Siennas & Umbror" = make_choices(intersect(c("44650", "44620", "OU103", "BU100", "BRU39", "GRAU36"), filter_ids)),
      "Röda & Orange" = make_choices(intersect(c("44300", "44200", "44210", "44220", "44510", "J225", "J180M", "J120N", "ER48A"), filter_ids)),
      "Bruna" = make_choices(intersect(c("J663", "J686", "48330"), filter_ids)),
      "Fyllmedel" = make_choices(intersect(c("599930", "58000", "58010", "58162", "58900", "58250"), filter_ids))
    )
  }
  
  # Update all picker input choices
  # Helper to ensure consistent choice updates across all pickers
  update_all_picker_choices <- function(filter_ids) {
    grouped <- create_filtered_grouped_choices(filter_ids)
    grouped <- grouped[sapply(grouped, length) > 0]
    choices_list <- c("Välj pigment" = "", grouped)
    
    updatePickerInput(session, "p1", choices = choices_list)
    updatePickerInput(session, "p2", choices = choices_list)
    updatePickerInput(session, "p3", choices = choices_list)
    updatePickerInput(session, "p4", choices = choices_list)
  }
  
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
            # Handle tar_id parameter
            if("tar_id" %in% names(query) && query$tar_id %in% names(misc_db)) {
              updateSelectInput(session, "tar_id", selected = query$tar_id)
            } else if("tar_cat" %in% names(query)) {
              # Backward compatibility: old index format
              tar_index <- as.numeric(query$tar_cat)
              tar_ids <- c("TAR01", "TAR02", "TAR03")
              if(!is.na(tar_index) && tar_index >= 1 && tar_index <= length(tar_ids)) {
                updateSelectInput(session, "tar_id", selected = tar_ids[tar_index])
              }
            } else if("tar_category" %in% names(query)) {
              # Backward compatibility: old name format
              old_name <- query$tar_category
              tar_name_map <- c(
                "Dalbränd trätjära (finast)" = "TAR01",
                "Dalbränd trätjära (finast, ljusast)" = "TAR01",
                "Ljus trätjära" = "TAR02",
                "Mörk trätjära (billigast)" = "TAR03",
                "Mörk trätjära" = "TAR03"
              )
              if(old_name %in% names(tar_name_map)) {
                updateSelectInput(session, "tar_id", selected = tar_name_map[[old_name]])
              }
            }
            if("tar_extra_oil" %in% names(query)) 
              updateSliderInput(session, "tar_extra_oil", value = as.numeric(query$tar_extra_oil))
          }
        }
        
        # Load color name if present
        if("name" %in% names(query) && nchar(query$name) > 0) {
          updateTextInput(session, "color_name", value = query$name)
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
    updateTextInput(session, "color_name", value = "")  # Clear color name
  })
  
  # RAÄ-filter
  # Uncheck RAÄ-only when user selects extended recipes
  observeEvent(input$recipe_set, {
    if(!is.null(input$recipe_set) && input$recipe_set == "extended") {
      updateCheckboxInput(session, "raa_only", value = FALSE)
    }
  })
  
  observeEvent(input$raa_only, {
    # Get RAÄ pigment IDs directly from pigments_db
    raa_ids <- names(pigments_db)[sapply(pigments_db, function(p) isTRUE(p$metadata$is_raa))]
    ids <- if(input$raa_only) raa_ids else names(pigments_db)
    
    # Update picker choices based on filter
    grouped <- create_filtered_grouped_choices(ids)
    grouped <- grouped[sapply(grouped, length) > 0]
    
    choices_list <- c("Välj pigment" = "", grouped)
    current_p1 <- input$p1 %||% "vitbas"
    updatePickerInput(session, "p1", choices = choices_list, selected = current_p1)
    updatePickerInput(session, "p2", choices = choices_list, selected = input$p2)
    updatePickerInput(session, "p3", choices = choices_list, selected = input$p3)
    updatePickerInput(session, "p4", choices = choices_list, selected = input$p4)
  })
  
  # === SWATCH GENERATION CACHING ===
  # Swatch generation is expensive (thousands of color calculations)
  # Caching by shade_pigment provides instant results when switching between recipe sets
  # Cache is automatically invalidated when shade_pigment changes
  
  # Cache swatch generation - these are expensive and don't change often
  generate_all_extended_swatches_cached <- memoise(generate_all_extended_swatches)
  generate_all_raa_swatches_cached <- memoise(generate_all_raa_swatches)
  
  # Reactive for extended swatches - regenerate when shading pigment changes
  extended_swatches_reactive <- reactive({
    shade_pigment <- input$shading_pigment %||% "44450"
    generate_all_extended_swatches_cached(shade_pigment)
  })
  
  # Reactive for RAÄ swatches - regenerate when shading pigment changes
  raa_swatches_reactive <- reactive({
    shade_pigment <- input$shading_pigment_raa %||% "J318"
    generate_all_raa_swatches_cached(shade_pigment)
  })
  
  # Generic function to render swatch matrix
  render_swatch_matrix <- function(recipes, base_pigments, vitbas_increments, shade_increments, shade_pigment, use_tinting) {
    shade_name <- pigments_db[[shade_pigment]]$name
    
    if(length(recipes) == 0) {
      return(tags$p("Inga recept tillgängliga."))
    }
    
    matrix_elements <- list()
    
    for(base_id in base_pigments) {
      base_name <- pigments_db[[base_id]]$name
      
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
              color_rgb <- mix_colors(ids, pcts, pigments_db, use_tinting = use_tinting)
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
      
      # Extended pattern: Logarithmic spacing (10 vitbas × 5 shade levels)
      vitbas_increments <- c(0, 15, 30, 45, 60, 70, 78, 85, 90)  # 9 levels
      shade_increments <- c(0, 8, 18, 32, 50)  # 5 levels (added heavy shade)
      
      return(render_swatch_matrix(recipes_to_show, base_pigments, vitbas_increments, 
                                  shade_increments, shade_pigment, use_tinting))
    }
    
    if(recipe_set == "saved") {
      # Saved favorites from localStorage
      favorites_raw <- input$favorites_list
      
      # Parse JSON if it's a string
      favorites <- if(is.character(favorites_raw) && nchar(favorites_raw) > 0) {
        tryCatch({
          jsonlite::fromJSON(favorites_raw, simplifyVector = FALSE)
        }, error = function(e) {
          cat("Error parsing favorites JSON:", e$message, "\n")
          list()
        })
      } else if(is.list(favorites_raw)) {
        favorites_raw
      } else {
        list()
      }
      
      if(is.null(favorites) || length(favorites) == 0) {
        return(tags$div(
          style = "text-align: center; padding: 40px; color: #666;",
          icon("star", style = "font-size: 48px; color: #ddd;"), br(), br(),
          tags$p("Inga sparade favoritkulörer än."),
          tags$p(tags$small("Blanda en egen kulör och klicka på 'Spara som favoritkulör' för att spara den här."))
        ))
      }
      
      # Convert favorites to a simple list format for rendering
      tryCatch({
        swatch_elements <- list()
        
        for(i in seq_along(favorites)) {
          # Get favorite - handle both list and atomic vector cases
          fav_item <- favorites[[i]]
          
          # Skip if not a list
          if(!is.list(fav_item)) next
          
          # Safe extraction using names
          get_safe <- function(name, default = "") {
            if(name %in% names(fav_item)) {
              val <- fav_item[[name]]
              if(is.null(val) || length(val) == 0) return(default)
              return(as.character(val))
            }
            return(default)
          }
          
          # Extract values
          p1 <- get_safe("p1", "")
          pct1 <- as.numeric(get_safe("pct1", "0"))
          p2 <- get_safe("p2", "")
          pct2 <- as.numeric(get_safe("pct2", "0"))
          p3 <- get_safe("p3", "")
          pct3 <- as.numeric(get_safe("pct3", "0"))
          p4 <- get_safe("p4", "")
          pct4 <- as.numeric(get_safe("pct4", "0"))
          fav_name <- get_safe("name", "")
          fav_id <- get_safe("id", as.character(i))
          
          # Build pigment mix
          ids <- c()
          pcts <- c()
          
          if(p1 != "" && !is.na(pct1) && pct1 > 0) {
            ids <- c(ids, p1)
            pcts <- c(pcts, pct1)
          }
          if(p2 != "" && !is.na(pct2) && pct2 > 0) {
            ids <- c(ids, p2)
            pcts <- c(pcts, pct2)
          }
          if(p3 != "" && !is.na(pct3) && pct3 > 0) {
            ids <- c(ids, p3)
            pcts <- c(pcts, pct3)
          }
          if(p4 != "" && !is.na(pct4) && pct4 > 0) {
            ids <- c(ids, p4)
            pcts <- c(pcts, pct4)
          }
          
          # Calculate color
          hex_color <- "#FFFFFF"
          if(length(ids) > 0) {
            tryCatch({
              color_rgb <- mix_colors(ids, pcts, pigments_db, use_tinting = TRUE)
              hex_color <- rgb(color_rgb[1], color_rgb[2], color_rgb[3], maxColorValue = 255)
            }, error = function(e) {
              hex_color <<- "#FFFFFF"
            })
          }
          
          # Create swatch with delete button
          display_name <- if(fav_name != "") fav_name else "Namnlös"
          
          swatch_elements[[length(swatch_elements) + 1]] <- tags$span(
            style = "position: relative; display: inline-block; margin: 5px;",
            tags$span(
              class = "kulturkulor-swatch",
              style = sprintf("background-color:%s; width: 48px; height: 48px;", hex_color),
              title = display_name,
              onclick = sprintf("Shiny.setInputValue('favorite_click', '%s', {priority: 'event'});", fav_id)
            ),
            # Delete button (small circle with X)
            tags$span(
              class = "favorite-delete-btn",
              style = "position: absolute; top: -4px; right: -4px; width: 20px; height: 20px; background: white; border: 1px solid #ccc; border-radius: 50%; cursor: pointer; display: flex; align-items: center; justify-content: center; font-size: 14px; color: #000; box-shadow: 0 2px 4px rgba(0,0,0,0.3);z-index:20;",
              onclick = sprintf("event.stopPropagation(); deleteFavorite('%s'); return false;", fav_id),
              title = "Ta bort favorit",
              "×"
            )
          )
        }
        
        tagList(
          tags$div(
            style = "margin-bottom: 20px;",
            swatch_elements
          ),
          tags$div(
            style = "margin-top: 20px; text-align: center;",
            actionButton("clear_all_favorites", "Rensa alla favoriter", 
                         class = "btn btn-default btn-sm",
                         icon = icon("trash-alt"))
          )
        )
      }, error = function(e) {
        return(tags$div(
          style = "text-align: center; padding: 40px; color: #d9534f;",
          icon("exclamation-triangle", style = "font-size: 48px;"), br(), br(),
          tags$p("Fel vid laddning av favoriter."),
          tags$p(tags$small(paste("Felmeddelande:", e$message)))
        ))
      })
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
    
    # Check if swatch contains non-RAÄ pigments
    all_pigments <- c(base_id, shade_id)
    all_pigments <- all_pigments[all_pigments != ""]
    
    raa_ids <- names(pigments_db)[sapply(pigments_db, function(p) isTRUE(p$metadata$is_raa))]
    has_non_raa <- any(!all_pigments %in% c("vitbas", raa_ids))
    
    # If RAÄ-only is checked but swatch has non-RAÄ pigments, uncheck it
    # and update picker choices BEFORE loading pigments
    if(has_non_raa && isTRUE(input$raa_only)) {
      updateCheckboxInput(session, "raa_only", value = FALSE)
      
      # Manually update picker choices to include all pigments immediately
      # This ensures the pigments are available when we try to select them below
      update_all_picker_choices(names(pigments_db))
      
      showNotification("RAÄ-filter avaktiverad för att visa icke-Kulturkulör pigment", 
                       type = "warning", duration = 3)
    }
    
    # Ensure they sum to 100
    total <- base_pct + vitbas_pct + shade_pct
    if(total != 100) {
      diff <- 100 - total
      base_pct <- base_pct + diff
    }
    
    # Clear all slots first before loading
    clear_all_pigments()
    
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
    
    # Generate and populate color name
    # Get pigment names
    base_name <- if(!is.null(pigments_db[[base_id]]$name)) pigments_db[[base_id]]$name else base_id
    shade_name <- if(!is.null(pigments_db[[shade_id]]$name)) pigments_db[[shade_id]]$name else shade_id
    
    # Create descriptive name
    color_name <- paste0(base_name)
    
    if(vitbas_pct > 0) {
      color_name <- paste0(color_name, " + ", vitbas_pct, "% vit")
    }
    
    if(shade_pct > 0) {
      color_name <- paste0(color_name, " + ", shade_pct, "% ", shade_name)
    }
    
    # Update the color name field
    updateTextInput(session, "color_name", value = color_name)
  })
  
  # Save favorite
  observeEvent(input$save_favorite, {
    # Get current mix
    m <- mix()
    if(length(m$ids) == 0 || m$total == 0) {
      showNotification("Ingen färg att spara. Välj pigment först.", type = "warning", duration = 3)
      return()
    }
    
    # Build favorite object
    favorite <- list(
      p1 = input$p1 %||% "",
      pct1 = input$pct1 %||% 0,
      p2 = input$p2 %||% "",
      pct2 = input$pct2 %||% 0,
      p3 = input$p3 %||% "",
      pct3 = input$pct3 %||% 0,
      p4 = input$p4 %||% "",
      pct4 = input$pct4 %||% 0,
      name = input$color_name %||% ""
    )
    
    # Send to JavaScript to save in localStorage
    session$sendCustomMessage("save_favorite", favorite)
    
    showNotification("Favorit sparad", type = "message", duration = 2)
  })
  
  # Handle favorite click (load from saved)
  observeEvent(input$favorite_click, {
    req(input$favorite_click)
    fav_id <- input$favorite_click
    
    # Get favorite from localStorage via JavaScript
    # The favorites_list input contains all favorites
    favorites_raw <- input$favorites_list
    
    # Parse JSON if it's a string
    favorites <- if(is.character(favorites_raw) && nchar(favorites_raw) > 0) {
      tryCatch({
        jsonlite::fromJSON(favorites_raw, simplifyVector = FALSE)
      }, error = function(e) {
        list()
      })
    } else if(is.list(favorites_raw)) {
      favorites_raw
    } else {
      list()
    }
    
    if(is.null(favorites) || length(favorites) == 0) return()
    
    # Find the clicked favorite
    fav <- NULL
    for(i in seq_along(favorites)) {
      f <- favorites[[i]]
      f_id <- if(is.list(f) && !is.null(f[["id"]])) f[["id"]] else NULL
      if(!is.null(f_id) && f_id == fav_id) {
        fav <- f
        break
      }
    }
    
    if(is.null(fav)) return()
    
    # Safe access function
    get_val <- function(key, default = "") {
      val <- fav[[key]]
      if(is.null(val) || length(val) == 0) return(default)
      return(val)
    }
    
    # Check if favorite contains non-RAÄ pigments
    p1 <- get_val("p1", "")
    p2 <- get_val("p2", "")
    p3 <- get_val("p3", "")
    p4 <- get_val("p4", "")
    
    all_pigments <- c(p1, p2, p3, p4)
    all_pigments <- all_pigments[all_pigments != ""]
    
    # Check if any pigment is not in RAÄ list
    raa_ids <- names(pigments_db)[sapply(pigments_db, function(p) isTRUE(p$metadata$is_raa))]
    has_non_raa <- any(!all_pigments %in% c("vitbas", raa_ids))
    
    # If RAÄ-only is checked but favorite has non-RAÄ pigments, uncheck it
    # and update picker choices BEFORE loading pigments
    if(has_non_raa && isTRUE(input$raa_only)) {
      updateCheckboxInput(session, "raa_only", value = FALSE)
      
      # Manually update picker choices to include all pigments immediately
      # This ensures the pigments are available when we try to select them below
      update_all_picker_choices(names(pigments_db))
      
      showNotification("RAÄ-filter avaktiverad för att ladda alla pigment i favoriten", 
                       type = "warning", duration = 3)
    }
    
    # Clear all slots first
    clear_all_pigments()
    
    # Load favorite data
    pct1 <- as.numeric(get_val("pct1", 0))
    if(p1 != "" && pct1 > 0) {
      updatePickerInput(session, "p1", selected = p1)
      updateSliderInput(session, "pct1", value = pct1)
    }
    
    pct2 <- as.numeric(get_val("pct2", 0))
    if(p2 != "" && pct2 > 0) {
      updatePickerInput(session, "p2", selected = p2)
      updateSliderInput(session, "pct2", value = pct2)
    }
    
    pct3 <- as.numeric(get_val("pct3", 0))
    if(p3 != "" && pct3 > 0) {
      updatePickerInput(session, "p3", selected = p3)
      updateSliderInput(session, "pct3", value = pct3)
    }
    
    pct4 <- as.numeric(get_val("pct4", 0))
    if(p4 != "" && pct4 > 0) {
      updatePickerInput(session, "p4", selected = p4)
      updateSliderInput(session, "pct4", value = pct4)
    }
    
    # Load color name if exists
    fav_name <- get_val("name", "")
    if(fav_name != "") {
      updateTextInput(session, "color_name", value = fav_name)
    }
    
    showNotification("Favorit laddad", type = "message", duration = 2)
  })
  
  # Clear all favorites
  observeEvent(input$clear_all_favorites, {
    session$sendCustomMessage("clear_all_favorites", list())
  })
  
  # Refresh favorites list when updated
  observeEvent(input$favorites_updated, {
    # Re-render will happen automatically via favorites_list reactive
  })
  
  # Blandning
  mix <- reactive({
    # CRITICAL: Don't use c() because it drops empty values and misaligns the arrays!
    # Keep as explicit vectors to maintain alignment
    ids <- character(4)
    ids[1] <- if(!is.null(input$p1) && length(input$p1) > 0 && input$p1 != "") input$p1 else ""
    ids[2] <- if(!is.null(input$p2) && length(input$p2) > 0 && input$p2 != "") input$p2 else ""
    ids[3] <- if(!is.null(input$p3) && length(input$p3) > 0 && input$p3 != "") input$p3 else ""
    ids[4] <- if(!is.null(input$p4) && length(input$p4) > 0 && input$p4 != "") input$p4 else ""
    
    pct <- c(input$pct1 %||% 0, input$pct2 %||% 0, input$pct3 %||% 0, input$pct4 %||% 0)
    
    # Filter: must have valid ID AND percentage > 0
    valid <- sapply(seq_along(ids), function(i) {
      !is.na(ids[i]) && 
        !is.null(ids[i]) && 
        length(ids[i]) > 0 && 
        nchar(as.character(ids[i])) > 0 &&
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
    cols <- mix_colors(m$ids, m$pct, pigments_db, use_tinting = use_tinting)
    
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
      pigment_names <- sapply(ids_filtered, function(id) {
        name <- pigments_db[[id]]$name
        if(is.null(name) || length(name) == 0) {
          return(id)  # Fallback to ID if name not found
        }
        return(name)
      })
      normalized_swe <- sapply(normalized_filtered, function(x) format_swe(x, 1))
      
      text_lines <- paste0(pigment_names, ": ", normalized_swe, " %", collapse = " • ")
      msg <- "Totalen överstiger 100 %. Normaliserade procentsatser som används:"
      
      info_box(
        tagList(
          msg, text_lines,
          tags$br(),
          tags$div(
            style = "margin-top: 0.5em;",
            actionButton("normalize_values", "Snabbjustera reglage till normaliserade värden", class = "btn-default btn-sm", icon = icon("sliders"))
          )
        ),
        type = "warning",
        icon_name = "exclamation-triangle"
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
        p1 = if(is.null(input$p1) || length(input$p1) == 0 || input$p1 == "") NA else input$p1,
        p2 = if(is.null(input$p2) || length(input$p2) == 0 || input$p2 == "") NA else input$p2,
        p3 = if(is.null(input$p3) || length(input$p3) == 0 || input$p3 == "") NA else input$p3,
        p4 = if(is.null(input$p4) || length(input$p4) == 0 || input$p4 == "") NA else input$p4
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
  observeEvent(input$to_step2, { 
    hide("step1"); 
    if(mix()$has_white) show("step2") else {
      show("step3")
      # Sync color name to step 3
      updateTextInput(session, "color_name_step3", value = input$color_name %||% "")
    }
  })
  observeEvent(input$back1, { hide("step2"); show("step1") })
  observeEvent(input$back2, { 
    hide("step3"); 
    if(mix()$has_white) show("step2") else show("step1")
    # Sync color name back to step 1
    updateTextInput(session, "color_name", value = input$color_name_step3 %||% "")
  })
  observeEvent(input$to_step3, { 
    hide("step2"); 
    show("step3")
    # Sync color name to step 3
    updateTextInput(session, "color_name_step3", value = input$color_name %||% "")
  })
  
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
      h3("Recept för äggoljetempera")
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
      rows <- c(rows, list(list(paste0("Ägg (", format_swe(r$eggs_count), " st à 50 g)"), r$eggs)))
      rows <- c(rows, list(list("Vatten", r$water)))
      
      if(r$zn > 0.1) rows <- c(rows, list(list("Zinkvitt PW4 (#44100)", r$zn)))
      if(r$ti > 0.1) rows <- c(rows, list(list("Titanvitt Rutile PW6 (#44400)", r$ti)))
      
      for(id in names(r$color)) {
        rows <- c(rows, list(list(paste0(pigments_db[[id]]$name, " (#", id, ")"), as.numeric(r$color[id]))))
      }
      
      # Add extra filler last
      rows <- c(rows, list(list(paste0(pigments_db[[r$filler_id]]$name, " (#", r$filler_id, ")"), r$filler_g)))
      
    } else if(paint_type == "tar") {
      # Tar oil paint recipe format
      tar_name <- if(!is.null(r$tar_id) && r$tar_id %in% names(misc_db)) {
        misc_db[[r$tar_id]]$name
      } else {
        "Trätjära"
      }
      rows <- c(rows, list(list(tar_name, r$tar)))
      rows <- c(rows, list(list("Kallpressad kokt linolja", r$oil)))
      rows <- c(rows, list(list("Balsamterpentin", r$balsamterpentin)))
      
      if(r$zn > 0.1) rows <- c(rows, list(list("Zinkvitt PW4 (#44100)", r$zn)))
      if(r$ti > 0.1) rows <- c(rows, list(list("Titanvitt Rutile PW6 (#44400)", r$ti)))
      
      for(id in names(r$color)) {
        rows <- c(rows, list(list(paste0(pigments_db[[id]]$name, " (#", id, ")"), as.numeric(r$color[id]))))
      }
      
    } else {
      # Linseed oil paint recipe format (original)
      rows <- c(rows, list(list("Kallpressad kokt linolja", r$oil)))
      if(r$zn > 0.1) rows <- c(rows, list(list("Zinkvitt PW4 (#44100)", r$zn)))
      if(r$ti > 0.1) rows <- c(rows, list(list("Titanvitt Rutile PW6 (#44400)", r$ti)))
      for(id in names(r$color)) {
        rows <- c(rows, list(list(paste0(pigments_db[[id]]$name, " (#", id, ")"), as.numeric(r$color[id]))))
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
    avg_density <- calculate_avg_density(m, compensated_pcts, zinc_ratio)
    
    # Packing factor constant
    PACKING_FACTOR <- 0.85
    
    if(paint_type == "egg_oil") {
      # Include filler density in weighted average
      filler_density <- pigments_db[[recipe$filler_id]]$properties$density
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
  
  final_recipe <- reactive({
    c <- calc()  # Get all values from calc()
    m <- mix()
    zinc_ratio <- c$zinc_ratio / 100
    paint_type <- input$paint_type %||% "linseed"
    
    # Build extra parameters based on paint type
    extra_params <- list(hex = final_hex())
    
    if (paint_type == "linseed") {
      extra_params$extra_oil <- c$extra_oil
    } else if (paint_type == "egg_oil") {
      extra_params$filler_id <- input$egg_filler
    } else if (paint_type == "tar") {
      extra_params$tar_id <- input$tar_id
      extra_params$tar_extra_oil <- input$tar_extra_oil %||% 1.6
    }
    
    calculate_recipe_generic(paint_type, c$target_liters, m, zinc_ratio, extra_params)
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
      txt <- paste0(txt, strrep("=", 60), "\n\n")
      
      # Add color name if provided
      color_name <- input$color_name_step3 %||% input$color_name %||% ""
      if(nchar(color_name) > 0) {
        txt <- paste0(txt, "Färgnamn: ", color_name, "\n")
      }
      
      txt <- paste0(txt,
                    "Färgkod: ", final_hex(), "\n",
                    "Typ av färg: ", paint_type_name, "\n",
                    "Yta: ", format_swe(c$area, 0), " m²\n",
                    "Antal strykningar: ", input$use, "\n\n")
      
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
        pigment <- pigments_db[[id]]
        if(is.null(pigment) || is.null(pigment$suppliers)) next
        
        suppliers_found <- TRUE
        
        txt <- paste0(txt, pigments_db[[id]]$name, "\n")

        # Kremer Pigmente
        if(!is.null(pigment$suppliers$kremer)) {
          txt <- paste0(txt, "  Kremer Pigmente:\n")
          txt <- paste0(txt, "    - Matchning: ", pigment$suppliers$kremer$match, " match\n")
          txt <- paste0(txt, "    - Produkt-ID: ", pigment$suppliers$kremer$id, "\n")
          
          # Handle multiple URLs (e.g., for GO94_GU30)
          if(length(pigment$suppliers$kremer$url) > 1) {
            txt <- paste0(txt, "    - Webbadresser:\n")
            for(url in pigment$suppliers$kremer$url) {
              txt <- paste0(txt, "      ", url, "\n")
            }
          } else {
            txt <- paste0(txt, "    - Webbadress: ", pigment$suppliers$kremer$url, "\n")
          }
        }
        
        # Ottosson Färgmakeri
        if(!is.null(pigment$suppliers$ottosson)) {
          txt <- paste0(txt, "  Ottosson Färgmakeri (Sverige):\n")
          txt <- paste0(txt, "    - Produkt: ", pigment$suppliers$ottosson$name, "\n")
          txt <- paste0(txt, "    - Webbadress: ", pigment$suppliers$ottosson$url, "\n")
        }
        
        # Claessons Trätjära
        if(!is.null(pigment$suppliers$claessons)) {
          txt <- paste0(txt, "  Claessons Trätjära (Sverige):\n")
          txt <- paste0(txt, "    - Produkt: ", pigment$suppliers$claessons$name, "\n")
          txt <- paste0(txt, "    - Webbadress: ", pigment$suppliers$claessons$url, "\n")
        }
        
        # Gysinge
        if(!is.null(pigment$suppliers$gysinge)) {
          txt <- paste0(txt, "  Gysinge (Sverige):\n")
          txt <- paste0(txt, "    - Produkt: ", pigment$suppliers$gysinge$name, "\n")
          txt <- paste0(txt, "    - Webbadress: ", pigment$suppliers$gysinge$url, "\n")
        }
        
        txt <- paste0(txt, "  Notering: ", pigment$notes, "\n\n")
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