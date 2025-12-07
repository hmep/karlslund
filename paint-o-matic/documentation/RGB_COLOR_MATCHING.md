# RGB Color Matching Feature - Analysis and Implementation Approaches

**Date:** 2025-12-07  
**Status:** Planning / Design Document

## Overview

This document outlines approaches for adding RGB color matching functionality to Paint-o-matic, allowing users to:
1. Input an RGB color code and find matching pigment mixes
2. Extract colors from uploaded photos
3. Get practical paint mixing recommendations based on available pigments

## Current Architecture

Paint-o-matic uses:
- **Pigment database** with RGB values, K/S coefficients (Kubelka-Munk theory)
- **Tinting strength-based mixing** that weights pigments by their optical power
- **Three paint types**: Linseed oil, egg oil tempera, and tar oil
- Well-structured code with color mixing in `R/utils/color_mixing.R`

The existing `mix_colors()` function already implements physically accurate color mixing using Kubelka-Munk coefficients, which is perfect for reverse-engineering color matches.

---

## Approach 1: Reverse Optimization (Recommended for Initial Implementation)

### Description
Use optimization algorithms to find pigment combinations that best match a target RGB value. This approach leverages the existing physics-based mixing logic.

### How It Works
1. User inputs target RGB (or from photo)
2. Use optimization algorithm (e.g., `optim()` in R) to find pigment combinations
3. Search across existing pigment inventory with different mix ratios
4. Minimize Delta E distance between target and mixed result using existing `mix_colors()` function

### Implementation Sketch

```r
# Add to R/utils/color_matching.R (new file)

#' Find pigment mix that matches target RGB color
#'
#' Uses optimization to find the best combination of pigments that produces
#' a color close to the target RGB value. Uses existing mix_colors() function
#' with tinting strength for realistic results.
#'
#' @param target_rgb Numeric vector of length 3 (R, G, B values 0-255)
#' @param pigments_db List of pigment data (from global environment)
#' @param max_pigments Maximum number of pigments in mix (default 3)
#' @param common_pigments Optional character vector of pigment IDs to prioritize
#' @return List with pigments, percentages, delta_e, and preview_rgb
find_pigment_mix_for_rgb <- function(target_rgb, 
                                      pigments_db, 
                                      max_pigments = 3,
                                      common_pigments = NULL) {
  
  # Convert RGB to LAB for perceptual matching
  target_lab <- convertColor(matrix(target_rgb/255, ncol=3), "sRGB", "Lab")
  
  # Determine pigment pool
  if(!is.null(common_pigments)) {
    pigment_ids <- common_pigments
  } else {
    pigment_ids <- names(pigments_db)
  }
  
  # Try combinations of pigments
  best_match <- NULL
  best_delta_e <- Inf
  
  # Generate combinations
  for(combo in combn(pigment_ids, max_pigments, simplify = FALSE)) {
    
    # Use optimization to find best percentages
    result <- optim(
      par = rep(100/max_pigments, max_pigments),  # Initial equal weights
      fn = function(weights) {
        # Ensure weights sum to 100 and are positive
        weights <- pmax(weights, 0)
        if(sum(weights) == 0) return(1e6)  # Penalty for invalid weights
        weights <- weights * 100 / sum(weights)
        
        # Mix colors using existing function
        mixed_rgb <- mix_colors(combo, weights, pigments_db, use_tinting = TRUE)
        mixed_lab <- convertColor(matrix(mixed_rgb/255, ncol=3), "sRGB", "Lab")
        
        # Calculate Delta E 2000 distance
        deltaE(target_lab, mixed_lab, metric = "2000")
      },
      method = "L-BFGS-B",
      lower = rep(0, max_pigments),
      upper = rep(100, max_pigments),
      control = list(maxit = 100)
    )
    
    if(result$value < best_delta_e) {
      best_delta_e <- result$value
      # Normalize weights to sum to 100
      normalized_weights <- result$par * 100 / sum(result$par)
      best_match <- list(
        pigments = combo, 
        percentages = round(normalized_weights, 1),
        delta_e = round(result$value, 2),
        preview_rgb = mix_colors(combo, normalized_weights, pigments_db, TRUE)
      )
    }
  }
  
  return(best_match)
}

#' Find multiple alternative matches for target RGB
#' 
#' Returns top N matching pigment combinations sorted by Delta E distance
#' 
#' @param target_rgb Numeric vector of length 3 (R, G, B values 0-255)
#' @param pigments_db List of pigment data
#' @param n_results Number of alternative matches to return (default 5)
#' @param max_pigments Maximum pigments per mix (default 3)
#' @return List of match results sorted by quality
find_multiple_matches <- function(target_rgb, 
                                   pigments_db, 
                                   n_results = 5,
                                   max_pigments = 3) {
  
  # Could be parallelized for speed
  matches <- list()
  pigment_ids <- names(pigments_db)
  
  for(combo in combn(pigment_ids, max_pigments, simplify = FALSE)) {
    match <- find_pigment_mix_for_rgb(target_rgb, pigments_db, max_pigments)
    if(!is.null(match)) {
      matches[[length(matches) + 1]] <- match
    }
  }
  
  # Sort by delta_e and return top n
  matches[order(sapply(matches, function(m) m$delta_e))][1:min(n_results, length(matches))]
}
```

### Pros
- Uses existing physics-based mixing (`mix_colors` with K/S values)
- Results are practically achievable with actual paints
- Respects tinting strength differences between pigments
- Can constrain to available inventory
- No pre-computation required

### Cons
- Computationally intensive (can be optimized with pre-filtering)
- May not find exact match (some RGB values impossible with available pigments)
- May need timeout for complex searches

### Optimization Strategies
- Pre-filter to most common/versatile pigments (vitbas, common earth colors, etc.)
- Start with 2-pigment combos, only try 3-pigment if delta_e > threshold
- Use cached results for repeated queries
- Consider parallel processing for multiple combinations

---

## Approach 2: Pre-computed Color Database

### Description
Pre-generate all possible pigment combinations at various ratios and store in a lookup database for instant retrieval.

### How It Works
1. One-time generation of all 2-pigment and 3-pigment combinations at 10% increments
2. Store in SQLite database or RDS file with RGB/LAB values
3. Use k-d tree or spatial indexing for fast nearest-neighbor lookup
4. Query returns closest pre-computed mixes instantly

### Implementation Sketch

```r
# Add to tools/generate_color_database.R (new file)

#' Generate pre-computed color database
#' 
#' Creates lookup table of all pigment combinations for fast color matching.
#' This is a one-time operation that should be re-run when pigments are added.
#' 
#' @param pigments_db List of pigment data
#' @param output_file Path to save RDS file (default: "data/color_lookup.rds")
#' @param increment Percentage increment for mixing ratios (default: 10)
generate_color_database <- function(pigments_db, 
                                     output_file = "data/color_lookup.rds",
                                     increment = 10) {
  library(data.table)
  
  message("Generating color database...")
  combos <- list()
  pigment_ids <- names(pigments_db)
  
  # 2-pigment mixes
  message("Generating 2-pigment combinations...")
  for(p1 in pigment_ids) {
    for(p2 in pigment_ids) {
      if(p1 >= p2) next  # Avoid duplicates
      
      for(pct1 in seq(increment, 100 - increment, increment)) {
        pct2 <- 100 - pct1
        rgb <- mix_colors(c(p1, p2), c(pct1, pct2), pigments_db, TRUE)
        lab <- convertColor(matrix(rgb/255, ncol=3), "sRGB", "Lab")
        
        combos[[length(combos) + 1]] <- list(
          p1 = p1, p2 = p2, p3 = NA,
          pct1 = pct1, pct2 = pct2, pct3 = 0,
          rgb_r = rgb[1], rgb_g = rgb[2], rgb_b = rgb[3],
          lab_l = lab[1], lab_a = lab[2], lab_b = lab[3],
          hex = rgb(rgb[1]/255, rgb[2]/255, rgb[3]/255)
        )
      }
    }
  }
  
  # 3-pigment mixes (optional - increases database size significantly)
  message("Generating 3-pigment combinations...")
  for(p1 in pigment_ids) {
    for(p2 in pigment_ids) {
      if(p2 <= p1) next
      for(p3 in pigment_ids) {
        if(p3 <= p2) next
        
        # Sample ratios more sparsely for 3-pigment mixes
        for(pct1 in seq(20, 60, 20)) {
          for(pct2 in seq(20, 80 - pct1, 20)) {
            pct3 <- 100 - pct1 - pct2
            if(pct3 < 20) next
            
            rgb <- mix_colors(c(p1, p2, p3), c(pct1, pct2, pct3), pigments_db, TRUE)
            lab <- convertColor(matrix(rgb/255, ncol=3), "sRGB", "Lab")
            
            combos[[length(combos) + 1]] <- list(
              p1 = p1, p2 = p2, p3 = p3,
              pct1 = pct1, pct2 = pct2, pct3 = pct3,
              rgb_r = rgb[1], rgb_g = rgb[2], rgb_b = rgb[3],
              lab_l = lab[1], lab_a = lab[2], lab_b = lab[3],
              hex = rgb(rgb[1]/255, rgb[2]/255, rgb[3]/255)
            )
          }
        }
      }
    }
  }
  
  # Convert to data.table and save
  dt <- rbindlist(combos)
  message(sprintf("Generated %d color combinations", nrow(dt)))
  saveRDS(dt, output_file)
  message(sprintf("Database saved to %s", output_file))
  
  invisible(dt)
}

#' Find closest pre-computed color match
#' 
#' @param target_rgb Numeric vector of length 3 (R, G, B values 0-255)
#' @param database data.table loaded from color_lookup.rds
#' @param n_results Number of matches to return (default: 5)
#' @return data.table with top matches sorted by Delta E distance
find_closest_mix <- function(target_rgb, database, n_results = 5) {
  target_lab <- convertColor(matrix(target_rgb/255, ncol=3), "sRGB", "Lab")
  
  # Calculate Delta E for all entries (vectorized - very fast!)
  database[, delta_e := sqrt(
    (lab_l - target_lab[1])^2 + 
    (lab_a - target_lab[2])^2 + 
    (lab_b - target_lab[3])^2
  )]
  
  # Return top N matches
  result <- database[order(delta_e)][1:min(n_results, nrow(database))]
  result
}
```

### Pros
- Instant lookup (milliseconds)
- Can generate millions of combinations offline
- Works well with existing architecture
- Consistent results
- No runtime optimization complexity

### Cons
- Requires initial computation time (one-time cost)
- Database file size (can be several MB, use compression)
- Limited to pre-computed ratios (10% increments = good enough for most cases)
- Needs regeneration when pigments added/changed

---

## Approach 3: Photo Color Extraction

### Description
Extract representative RGB values from uploaded photos using image processing techniques.

### 3a: Blur + Mode/Median (Simpler, Recommended)

```r
# Add to R/utils/photo_color_extraction.R (new file)

#' Extract dominant color from photo using blur and median
#' 
#' Applies heavy Gaussian blur to average out texture and grain,
#' then extracts median color from center region of image.
#' 
#' @param image_path Path to uploaded image file
#' @return Numeric vector of length 3 (R, G, B values 0-255)
extract_color_from_photo <- function(image_path) {
  library(magick)
  
  img <- image_read(image_path)
  
  # Heavy Gaussian blur to average out texture/grain
  # Larger radius = more averaging
  img_blurred <- image_blur(img, radius = 50, sigma = 20)
  
  # Extract center region (avoid edges which may have lighting issues)
  info <- image_info(img_blurred)
  crop_size <- min(info$width, info$height) * 0.3  # 30% of image from center
  x_offset <- (info$width - crop_size) / 2
  y_offset <- (info$height - crop_size) / 2
  
  img_cropped <- image_crop(img_blurred, 
                            geometry = paste0(crop_size, "x", crop_size, 
                                            "+", x_offset, "+", y_offset))
  
  # Convert to raster and get median color
  raster_data <- as.integer(img_cropped[[1]])
  median_rgb <- c(
    median(raster_data[,,1]),
    median(raster_data[,,2]),
    median(raster_data[,,3])
  )
  
  return(median_rgb)
}

#' Extract color with preview showing processed image
#' 
#' @param image_path Path to uploaded image file
#' @return List with rgb vector and preview image object
extract_color_with_preview <- function(image_path) {
  library(magick)
  
  img <- image_read(image_path)
  img_blurred <- image_blur(img, radius = 50, sigma = 20)
  
  # Get median color
  info <- image_info(img_blurred)
  crop_size <- min(info$width, info$height) * 0.3
  x_offset <- (info$width - crop_size) / 2
  y_offset <- (info$height - crop_size) / 2
  
  img_cropped <- image_crop(img_blurred, 
                            geometry = paste0(crop_size, "x", crop_size, 
                                            "+", x_offset, "+", y_offset))
  
  raster_data <- as.integer(img_cropped[[1]])
  median_rgb <- c(
    median(raster_data[,,1]),
    median(raster_data[,,2]),
    median(raster_data[,,3])
  )
  
  # Create side-by-side preview
  preview <- image_append(c(
    image_scale(img, "300"),
    image_scale(img_blurred, "300"),
    image_border(
      image_scale(img_cropped, "300"),
      color = rgb(median_rgb[1]/255, median_rgb[2]/255, median_rgb[3]/255),
      geometry = "10x10"
    )
  ))
  
  list(rgb = median_rgb, preview = preview)
}
```

### 3b: Color Quantization (More Sophisticated)

```r
#' Extract multiple dominant colors from photo
#' 
#' Uses color quantization to find the N most prominent colors in an image.
#' User can then select which color to match.
#' 
#' @param image_path Path to uploaded image file
#' @param n_colors Number of dominant colors to extract (default 5)
#' @return List of colors with rgb values and relative frequencies
extract_dominant_colors <- function(image_path, n_colors = 5) {
  library(magick)
  
  img <- image_read(image_path)
  
  # Use ImageMagick's built-in color quantization
  img_quantized <- image_quantize(img, max = n_colors, colorspace = "sRGB")
  
  # Get histogram of quantized colors
  hist <- image_histogram(img_quantized)
  
  # Extract RGB values and frequencies
  colors <- lapply(seq_len(min(n_colors, length(hist))), function(i) {
    col <- hist[i]
    rgb_vals <- col2rgb(names(col))[,1]
    list(
      rgb = as.numeric(rgb_vals),
      hex = rgb(rgb_vals[1]/255, rgb_vals[2]/255, rgb_vals[3]/255),
      frequency = as.numeric(col),
      frequency_pct = round(as.numeric(col) / sum(sapply(hist, as.numeric)) * 100, 1)
    )
  })
  
  # Sort by frequency
  colors[order(sapply(colors, function(x) x$frequency), decreasing = TRUE)]
}

#' Render color palette from dominant colors
#' 
#' Creates visual swatches for user selection
#' 
#' @param colors_list Output from extract_dominant_colors()
#' @return HTML div with color swatches
render_color_palette <- function(colors_list) {
  swatches <- lapply(seq_along(colors_list), function(i) {
    col <- colors_list[[i]]
    tags$div(
      class = "color-swatch clickable",
      style = sprintf("background-color: %s; width: 80px; height: 80px; 
                       display: inline-block; margin: 5px; cursor: pointer;
                       border: 2px solid #ccc; border-radius: 4px;",
                      col$hex),
      onclick = sprintf("selectPhotoColor(%d, '%s')", i, col$hex),
      title = sprintf("%s (%s%%)", col$hex, col$frequency_pct),
      tags$div(
        style = "padding: 5px; background: rgba(255,255,255,0.8); 
                 font-size: 11px; margin-top: 55px;",
        sprintf("%s%%", col$frequency_pct)
      )
    )
  })
  
  tags$div(class = "dominant-colors-palette", swatches)
}
```

### Recommended: Use Approach 3a
- Simpler implementation
- Matches your original suggestion of using heavily blurred image
- Works well for paint swatches, painted surfaces, and color samples
- Approach 3b is better for complex images with multiple colors

---

## Implementation Plan

### Phase 1: Add Basic RGB Input (Minimal viable feature)

**UI Changes in `app.R`:**

```r
# Add to Step 1, after color_name input (around line 240)

textInput("target_rgb", 
          "RGB-färgkod (valfritt)", 
          value = "",
          placeholder = "255,128,64 eller #FF8040"),
actionButton("match_rgb", 
             "Hitta matchande pigmentblandning", 
             class = "btn-primary btn-sm", 
             icon = icon("search")),
br(), br()
```

**Server Logic:**

```r
# Add observer for RGB matching button
observeEvent(input$match_rgb, {
  req(input$target_rgb)
  
  # Parse RGB input (handle both formats)
  rgb_input <- trimws(input$target_rgb)
  
  tryCatch({
    if(grepl("^#", rgb_input)) {
      # Hex format
      target_rgb <- col2rgb(rgb_input)[,1]
    } else {
      # Comma-separated format
      target_rgb <- as.numeric(strsplit(rgb_input, ",")[[1]])
      if(length(target_rgb) != 3 || any(target_rgb < 0) || any(target_rgb > 255)) {
        stop("Invalid RGB values")
      }
    }
    
    # Find matching pigment mix
    match <- find_pigment_mix_for_rgb(target_rgb, pigments_db, max_pigments = 3)
    
    if(!is.null(match)) {
      # Auto-populate pigment selectors
      updatePickerInput(session, "p1", selected = match$pigments[1])
      updateSliderInput(session, "pct1", value = match$percentages[1])
      
      if(length(match$pigments) >= 2) {
        updatePickerInput(session, "p2", selected = match$pigments[2])
        updateSliderInput(session, "pct2", value = match$percentages[2])
      }
      
      if(length(match$pigments) >= 3) {
        updatePickerInput(session, "p3", selected = match$pigments[3])
        updateSliderInput(session, "pct3", value = match$percentages[3])
      }
      
      showNotification(
        sprintf("Hittade matchning med Delta E = %.2f", match$delta_e),
        type = "message",
        duration = 5
      )
    } else {
      showNotification("Kunde inte hitta matchning", type = "warning")
    }
    
  }, error = function(e) {
    showNotification(
      paste("Ogiltigt RGB-format. Använd '255,128,64' eller '#FF8040'"),
      type = "error",
      duration = 5
    )
  })
})
```

### Phase 2: Add Photo Upload

```r
# Add to UI after RGB input
fileInput("color_photo", 
          "Eller ladda upp färgfoto",
          accept = c("image/jpeg", "image/png", "image/jpg"),
          placeholder = "Välj bild..."),

# Server logic
observeEvent(input$color_photo, {
  req(input$color_photo)
  
  tryCatch({
    # Extract color from photo
    result <- extract_color_with_preview(input$color_photo$datapath)
    
    # Update RGB input field with extracted color
    rgb_text <- paste(result$rgb, collapse = ",")
    updateTextInput(session, "target_rgb", value = rgb_text)
    
    # Show preview (could be in a modal or inline)
    showModal(modalDialog(
      title = "Extraherad färg från foto",
      tags$img(src = result$preview, style = "max-width: 100%;"),
      tags$p(sprintf("RGB: %s", rgb_text)),
      tags$p(sprintf("Hex: %s", rgb(result$rgb[1]/255, result$rgb[2]/255, result$rgb[3]/255))),
      footer = tagList(
        actionButton("use_photo_color", "Använd denna färg"),
        modalButton("Avbryt")
      )
    ))
    
  }, error = function(e) {
    showNotification("Kunde inte behandla bilden", type = "error")
  })
})

observeEvent(input$use_photo_color, {
  removeModal()
  # Trigger RGB matching with extracted color
  shinyjs::click("match_rgb")
})
```

### Phase 3: Enhance with Multiple Suggestions

```r
# Show top 5 matches instead of just one
observeEvent(input$match_rgb, {
  # ... parse RGB as before ...
  
  matches <- find_multiple_matches(target_rgb, pigments_db, n_results = 5)
  
  # Display in modal with clickable options
  showModal(modalDialog(
    title = "Matchande pigmentblandningar",
    size = "l",
    tags$p("Klicka på en blandning för att använda den:"),
    lapply(seq_along(matches), function(i) {
      m <- matches[[i]]
      tags$div(
        class = "match-option",
        style = "border: 1px solid #ddd; padding: 10px; margin: 5px; cursor: pointer;",
        onclick = sprintf("Shiny.setInputValue('select_match', %d, {priority: 'event'})", i),
        tags$div(
          style = sprintf("width: 60px; height: 60px; background: %s; 
                          display: inline-block; margin-right: 10px;",
                         rgb(m$preview_rgb[1]/255, m$preview_rgb[2]/255, m$preview_rgb[3]/255)),
          ""
        ),
        tags$div(
          style = "display: inline-block; vertical-align: top;",
          tags$b(sprintf("Matchning %d (ΔE = %.2f)", i, m$delta_e)),
          tags$br(),
          sprintf("%s: %.1f%%", m$pigments[1], m$percentages[1]),
          if(length(m$pigments) >= 2) tags$br(),
          if(length(m$pigments) >= 2) sprintf("%s: %.1f%%", m$pigments[2], m$percentages[2]),
          if(length(m$pigments) >= 3) tags$br(),
          if(length(m$pigments) >= 3) sprintf("%s: %.1f%%", m$pigments[3], m$percentages[3])
        )
      )
    }),
    footer = modalButton("Stäng")
  ))
})
```

### Phase 4: Optional Enhancements

1. **Pre-computed database fallback** for instant suggestions of "similar colors"
2. **Color similarity visualization** showing Delta E scale
3. **Manual adjustment sliders** in results modal
4. **Save matched colors** directly to favorites
5. **Color history** of recent matches
6. **Export color palette** for multiple photos

---

## Required R Packages

### Core Dependencies
- `grDevices` - Base R, for `col2rgb()`, `rgb()`, `convertColor()`
- `stats` - Base R, for `optim()`

### Additional (may need installation)
- `colorspace` - For advanced Delta E 2000 calculations
- `magick` - For image processing (photo upload feature)
- `data.table` - For pre-computed database (optional, Approach 2)

### Installation Script

```r
# Check and install required packages
required_packages <- c("colorspace", "magick", "data.table")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}
```

---

## Integration Checklist

- [ ] Create `R/utils/color_matching.R` with optimization functions
- [ ] Create `R/utils/photo_color_extraction.R` with image processing functions
- [ ] Add RGB input field to Step 1 UI in `app.R`
- [ ] Add "Match RGB" button and observer
- [ ] Add file upload for photo feature
- [ ] Add modal dialog for displaying match results
- [ ] Test with various RGB values across color spectrum
- [ ] Test with photos of different quality and lighting
- [ ] Add error handling for edge cases
- [ ] Update documentation/README with new feature
- [ ] Consider adding example RGB codes or sample photos
- [ ] Validate Delta E thresholds (< 2.0 = excellent match, < 5.0 = good match)

---

## Testing Strategy

### RGB Matching Tests
1. **Pure colors**: Test primary RGB corners (255,0,0), (0,255,0), (0,0,255)
2. **Grays**: Test achromatic scale (128,128,128), (200,200,200)
3. **Earth tones**: Test common paint colors in your pigment range
4. **Edge cases**: Pure white (255,255,255), pure black (0,0,0)
5. **Impossible colors**: Neon/fluorescent colors not achievable with pigments

### Photo Extraction Tests
1. **Clean paint swatches**: Solid color samples
2. **Textured surfaces**: Wood, plaster, rough paint
3. **Various lighting**: Daylight, indoor, shadow
4. **Image quality**: High-res vs phone camera
5. **Multiple colors**: Verify centering and blur work correctly

### Performance Tests
- Measure optimization time for 2-pigment vs 3-pigment searches
- Test with full pigment database vs filtered subset
- Validate response time is acceptable (< 3 seconds preferred)

---

## Color Science Notes

### Delta E Interpretation
- **ΔE < 1.0**: Not perceptible by human eye
- **ΔE 1.0-2.0**: Perceptible only by experienced observer
- **ΔE 2.0-3.5**: Perceptible at a glance
- **ΔE 3.5-5.0**: Clear difference
- **ΔE > 5.0**: Completely different colors

For paint matching, target ΔE < 3.0 for "good match", flag ΔE > 5.0 as "approximate".

### RGB vs LAB Color Space
- **RGB**: Device-dependent, non-perceptual (Euclidean distance doesn't match visual perception)
- **LAB**: Device-independent, perceptually uniform (equal distance = equal perceived difference)
- Always convert to LAB for color comparison/matching

### Kubelka-Munk Theory
Your existing `mix_colors()` function already accounts for:
- Absorption coefficient (K)
- Scattering coefficient (S)
- Tinting strength differences between pigments

This is more sophisticated than simple RGB averaging and will produce realistic matches.

---

## Future Enhancements

1. **Machine learning refinement**: Train on actual mixed paint samples to improve predictions
2. **Spectral matching**: Use spectrophotometer data instead of RGB for professional accuracy
3. **Metamerism warning**: Flag color matches that may look different under various lighting
4. **Cost optimization**: Find cheapest pigment combination within Delta E threshold
5. **Batch color matching**: Upload multiple photos and generate palette
6. **Historical color matching**: Match to specific period-appropriate palettes
7. **Integration with Kulturkulor recipes**: Suggest traditional Swedish paint colors close to target

---

## References

- Kubelka-Munk theory: https://en.wikipedia.org/wiki/Kubelka%E2%80%93Munk_theory
- Delta E color difference: https://en.wikipedia.org/wiki/Color_difference
- R colorspace package: https://colorspace.r-forge.r-project.org/
- Magick package: https://docs.ropensci.org/magick/

---

## Conclusion

**Recommended approach for initial implementation:**
- **Use Approach 1 (Reverse Optimization)** for RGB matching
  - Leverages existing color mixing infrastructure
  - Produces realistic, achievable results
  - Can be optimized incrementally

- **Use Approach 3a (Blur + Median)** for photo extraction
  - Simple and effective
  - Matches original suggestion
  - Good for paint color photos

- **Consider Approach 2 (Pre-computed Database)** as future enhancement
  - Add for instant "similar colors" suggestions
  - Useful for browsing/exploration even without exact match

The existing Paint-o-matic architecture is well-suited for this feature. The physics-based color mixing with Kubelka-Munk coefficients will provide more accurate results than simple RGB averaging.