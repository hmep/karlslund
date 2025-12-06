# How to Add New Pigments to Paint-o-matic

## Quick Start Guide

### Prerequisites
You need to know:
- Pigment ID (Kremer catalog number or custom code)
- Pigment name (Swedish)
- Oil absorption percentage
- K and S values (Kubelka-Munk coefficients)
- Density (g/cm³)
- RGB color values
- Category (white, black, blue, green, yellow, earth, oxide, filler)
- Supplier information (optional)

### Step-by-Step Process

#### 1. Add Pigment Definition to Database

Edit `paint-o-matic/R/data/pigments_unified.R` and add your pigment entry in the appropriate section:

```r
# Find the appropriate section comment (e.g., # EARTH COLORS - TERRA & POZZUOLI)
# Add your entry:

"40123" = list(
  id = "40123",  # Must match the key above
  name = "French Ochre Golden",  # Swedish name
  properties = list(
    oil = 28,      # Oil absorption %
    K = 0.62,      # Absorption coefficient
    S = 0.48,      # Scattering coefficient  
    density = 3.4, # g/cm³
    rgb = c(195, 145, 85)  # RGB values for preview
  ),
  metadata = list(
    is_raa = FALSE,              # Is RAÄ approved?
    is_tar_compatible = TRUE,    # Safe for tar oil paint?
    category = "yellow"           # Main category
  ),
  suppliers = list(
    kremer = list(
      id = "40123",
      match = "exact",  # or "equivalent", "mix"
      url = "https://www.kremer-pigmente.com/en/shop/pigments/40123-..."
    )
  ),
  notes = "Traditional French ochre with golden undertone"
)
```

#### 2. Add Pigment to Dropdown Display Groups

Edit `paint-o-matic/app.R` and find the `PIGMENT_DISPLAY_GROUPS` constant (around line 118):

```r
PIGMENT_DISPLAY_GROUPS <- list(
  "Vitbas" = c("vitbas"),
  "Fyllmedel" = c("599930", "58000", "58010", "58162", "58900", "58250"),
  "Gröna" = c("40400", "41700", "11100", "KG83", "ZG65", "40850", "40860", "GU30"),
  "Svarta" = c("44450", "J318", "BS98", "47501", "47400"),
  "Blåa" = c("11670", "UB88", "KB28"),
  "Terra & Pozzuoli" = c("40820", "40800", "40830", "BT44", "OT46"),
  "Gula & Ockror" = c("44082", "44086", "44150", "44160", "J920", "LO92", "GO94", "GO94_GU30", "40123"),  # ← Added here
  "Siennas & Umbror" = c("44650", "44620", "OU103", "BU100", "BRU39", "GRAU36"),
  "Röda & Orange" = c("44300", "44200", "44210", "44220", "44510", "J225", "J180M", "J120N", "ER48A"),
  "Bruna" = c("J663", "J686", "48330")
)
```

**Display Group Reference:**
- `"Vitbas"` - White base pigments
- `"Fyllmedel"` - Fillers and extenders
- `"Gröna"` - Green pigments
- `"Svarta"` - Black pigments
- `"Blåa"` - Blue pigments
- `"Terra & Pozzuoli"` - Earth colors (terra di siena, pozzuoli)
- `"Gula & Ockror"` - Yellow ochres and yellow pigments
- `"Siennas & Umbror"` - Siennas and umbers (burnt variants)
- `"Röda & Orange"` - Red and orange iron oxides
- `"Bruna"` - Brown iron oxides

#### 3. Test and Validate

Start the Shiny app:
```r
shiny::runApp("paint-o-matic")
```

**Check for validation warnings:**
- ✅ No warnings = Success! Your pigment is properly configured
- ⚠️ "Pigment in database but not in any display group" = You forgot step 2
- ⚠️ "Display group references missing pigment" = Typo in step 2 or pigment doesn't exist

**Visual verification:**
1. Open the pigment dropdowns (Pigment 1, 2, 3, 4)
2. Find your category (e.g., "Gula & Ockror")
3. Verify your pigment appears with correct name
4. Select it and verify color preview shows correct color
5. Check "Extended swatches" tab - your pigment should appear there too

## Troubleshooting

### Pigment doesn't appear in dropdowns

**Symptoms:** Pigment exists in `pigments_db` but isn't in any dropdown menu

**Causes:**
1. Forgot to add to `PIGMENT_DISPLAY_GROUPS` in app.R
2. Added to wrong category in `PIGMENT_DISPLAY_GROUPS`
3. Typo in pigment ID in `PIGMENT_DISPLAY_GROUPS`

**Solution:** Check startup warnings. The app will tell you which pigments are in the database but not in display groups.

### Warning: "Display group references missing pigment"

**Symptoms:** Warning message at app startup

**Causes:**
- Typo in pigment ID in `PIGMENT_DISPLAY_GROUPS`
- Pigment was removed from `pigments_db` but not from `PIGMENT_DISPLAY_GROUPS`
- ID mismatch between database and display groups

**Solution:** Check the warning message for the specific pigment ID. Verify spelling in both files.

### Color preview is wrong

**Causes:**
- Incorrect RGB values in `properties$rgb`
- RGB values don't match actual pigment color

**Solution:** Update RGB values in `pigments_unified.R`. You can:
- Use a color picker to sample from reference images
- Convert from LAB/Munsell values
- Mix small amount of paint and photograph it

### K and S values unknown

**Options:**
1. **Measure:** Use spectrophotometer and apply Kubelka-Munk theory
2. **Estimate:** Based on similar pigments in database
3. **Calculate:** From mixture experiments with known pigments
4. **Reference:** Check scientific papers or manufacturer data

**Typical ranges:**
- **K (absorption):** 0.0 (transparent) to 3.0+ (very absorbing)
- **S (scattering):** 0.0 (transparent) to 3.0+ (very opaque)
- White pigments: High S, low K
- Black pigments: High K, medium S  
- Colored pigments: Variable K and S
- Earth pigments: Usually K: 0.3-0.9, S: 0.3-0.8

## Advanced Topics

### Adding Multiple Pigments at Once

When adding many pigments (e.g., an entire Kremer catalog section):

1. Prepare a spreadsheet with all data
2. Write an R script to generate the entries
3. Add all entries to `pigments_unified.R` in one session
4. Add all IDs to appropriate categories in `PIGMENT_DISPLAY_GROUPS`
5. Test thoroughly - start app and check all warnings

### Creating Computed/Blend Pigments

For pigments that are mixtures (like "GO94_GU30" = 50/50 mix of GO94 and GU30):

```r
"BLEND_ID" = list(
  id = "BLEND_ID",
  name = "Custom Blend Name",
  properties = list(
    oil = 25,  # Average or measured
    K = 0.5,   # Calculated or measured
    S = 0.5,   # Calculated or measured
    density = 3.5,  # Average or measured
    rgb = c(120, 100, 80)  # Mixed color
  ),
  metadata = list(
    is_raa = FALSE,
    is_tar_compatible = TRUE,
    category = "computed",  # Use "computed" category
    is_computed = TRUE,     # Mark as computed
    components = list("PIGMENT1", "PIGMENT2")  # List component IDs
  ),
  suppliers = NULL,
  notes = "50/50 mixture of PIGMENT1 and PIGMENT2"
)
```

### Organizing by Section in pigments_unified.R

Keep the file organized:
- Use clear section comments (e.g., `# EARTH COLORS - TERRA & POZZUOLI`)
- Group related pigments together
- Sort within sections (by ID or by name)
- Add blank lines between pigments for readability

### Display Group Strategy

**Principles:**
- Group by color family or traditional use
- Keep groups reasonable size (5-15 pigments per group)
- Consider Swedish cultural/traditional names
- Match RAÄ cultural heritage categories when relevant

**Examples:**
- "Terra & Pozzuoli" - Natural earth colors from specific regions
- "Siennas & Umbror" - Burnt earth pigments
- "Järnoxider" - Synthetic iron oxides (if you want to separate from natural)

## Common Pigment Sources

### Kremer Pigmente
- Most pigments use Kremer catalog numbers as IDs
- Format: 5-digit numbers (e.g., "40010", "47800")
- Check their website for K/S values (sometimes available)
- Note: Some numbers may not be official catalog numbers

### RAÄ Kulturkulör System
- Uses letter codes (e.g., "KB28", "UB88", "GO94")
- These are official RAÄ (Riksantikvarieämbetet) color codes
- Mark with `is_raa = TRUE` in metadata
- These pigments are cultural heritage approved

### Custom Pigments
- Use descriptive IDs (e.g., "CUSTOM_GREEN", "MIX_01")
- Document thoroughly in notes
- Ensure unique IDs

## Related Documentation

- `database_structure.md` - Complete database structure reference
- `README_STRUCTURE.md` - Project organization overview
- `REFACTORING_SUMMARY.md` - Historical context for current structure

## Questions?

If you're unsure about:
- **K/S values:** Start with similar pigments and adjust based on testing
- **Category:** Choose the closest match; use metadata for precise filtering
- **Display group:** Choose based on traditional Swedish color naming
- **Supplier info:** Can be NULL if not available

The validation system will catch most errors at startup!
