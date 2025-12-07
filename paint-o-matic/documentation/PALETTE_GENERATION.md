# Palette Generation Documentation

## Overview

Paint-o-matic uses pre-computed static palette JSON files for instant palette loading, instead of generating thousands of color calculations on-the-fly. This approach provides:

- ✅ **Instant loading**: ~50ms JSON parsing vs 2-5 seconds of calculations
- ✅ **Better organization**: Swatches grouped by categories matching dropdown structure
- ✅ **Filter functionality**: Search/filter pigments by name
- ✅ **Simpler code**: No complex caching, pagination, or lazy loading needed
- ✅ **Easy maintenance**: Regenerate only when pigments change
- ✅ **Version control**: Palette files are committed to the repository

## When to Regenerate Palettes

Regenerate the palette files whenever:

- **Pigments are added** to the database
- **Pigments are removed** from the database
- **Pigment colors/properties are changed** (RGB values, K+S values, etc.)
- **Pigment names are changed** (displayed in swatch tooltips)
- **Shading pigment changes** (currently J318 by default)

## How to Regenerate Palettes

### From Command Line

Run the generation script from the `paint-o-matic` directory:

```bash
cd paint-o-matic
Rscript generate_palettes.R
```

### Output

The script generates two palette files:

- **`www/data/palette_raa.json`**: RAÄ Kulturkulör palette (~310 swatches, 68KB)
- **`www/data/palette_extended.json`**: Extended palette with all pigments (~1,836 swatches, 393KB)

### Generation Process

1. **Load dependencies**: Pigment database, color mixing utilities, swatch generation functions
2. **Generate swatch recipes**: Create swatch specifications for each palette type
3. **Calculate colors**: Mix colors using K+S tinting strength weighing
4. **Export JSON**: Save palette data with pre-calculated hex colors

### Example Output

```json
[
  {
    "code": "RAA_J225_0V0S",
    "hex": "#7D342B",
    "base": "J225",
    "base_pct": 100,
    "vitbas_pct": 0,
    "shade_pct": 0,
    "shade_pigment": "J318",
    "pigment_name": "Järnoxidrött nr 225"
  }
]
```

## Palette Structure

### RAÄ Palette (Kulturkulör)

The RAÄ palette follows Riksantikvarieämbetet's exact specifications:

- **Pattern A pigments**: 17 recipes each (full shading matrix)
  - Includes: J225, J180M, J120N, ER48A, J663, J686, J920, LO92, GO94, OU103, BU100, BRU39, BT44, OT46, KG83, UB88, KB28
  - Vitbas increments: 0, 14.28, 15, 29.27, 30, 41.86, 42.85, 45, 57.14, 60, 73.17, 75, 85.71, 90
  - Shade increments: 0, 2.44, 4.76, 6.97
  - Specific combinations published by RAÄ (not all combinations)

- **Pattern B pigments**: 7 recipes each (tinting only - no shading)
  - Includes: J318, GO94_GU30, GU30, GRAU36, BS98
  - Same vitbas increments as Pattern A
  - Only 0% shade level (pure tinting)

### Extended Palette

The extended palette includes all available pigments:

- **All pigments** excluding:
  - Whites: vitbas, 44100, 44400
  - Fillers: 599930, 58000, 58010, 58162, 58900, 58250
  - Shading pigments: J318, BS98, 47250, 47400, 47501, 44450, 47800, 48401, 47700

- **Vitbas increments**: 0, 15, 30, 45, 60, 70, 78, 85, 90 (logarithmic spacing)
- **Shade increments**: 0, 8, 18, 32, 50
- **Masked combinations**: Not all vitbas/shade combinations are included (optimized matrix)

## UI Organization

Palettes are displayed in the UI grouped by `PIGMENT_DISPLAY_GROUPS` categories:

1. **Vitbas**
2. **Fyllmedel**
3. **Gröna**
4. **Svarta**
5. **Blåa**
6. **Terra & Pozzuoli**
7. **Gula & Ockror**
8. **Siennas & Umbror**
9. **Röda & Orange**
10. **Bruna**
11. **Vita**
12. **Moderna syntetiska**

Each category displays:
- Category header (h5)
- Pigment subheader (pigment name)
- Swatch row (clickable color swatches)

## Filter Functionality

Users can filter palettes by pigment name using the text input field at the top of the palette display. The filter searches pigment names case-insensitively.

## Performance Benefits

| Metric | Before (Dynamic) | After (Static) |
|--------|-----------------|----------------|
| Initial load | 2-5 seconds | ~50ms |
| RAÄ swatches | ~310 calculations | 310 pre-computed |
| Extended swatches | ~1,836 calculations | 1,836 pre-computed |
| File size | N/A | 461KB total |
| Caching complexity | High (memoise) | None (just JSON) |

## Technical Details

### Swatch Generation Module

The swatch generation logic is extracted to `R/utils/swatch_generation.R`:

- `generate_swatch_matrix()`: Generic matrix generator
- `get_extended_base_pigments()`: Get pigments for extended palette
- `generate_all_extended_swatches()`: Generate extended palette swatches
- `generate_all_raa_swatches()`: Generate RAÄ palette swatches
- Constants: `RAA_VITBAS_INCREMENTS`, `RAA_SHADE_INCREMENTS`, `RAA_MASK_PATTERN_A`, `RAA_MASK_PATTERN_B`

### Generation Script

The `generate_palettes.R` script:

1. Loads only required dependencies (no Shiny)
2. Defines `%||%` operator for null coalescing
3. Stubs `memoise` if not available
4. Sources: constants, pigments database, color mixing, swatch generation
5. Generates both palettes with J318 as default shade pigment
6. Writes pretty-printed JSON files

### App Integration

The app loads palettes via `jsonlite::read_json()` and renders swatches with:

- Pre-calculated hex colors (no runtime mixing)
- Tooltip with swatch code, pigment name, and recipe
- Click handler to load recipe into mixer
- Filter by pigment name

## Troubleshooting

### Script Fails with "package not found"

Ensure these R packages are installed:

```R
install.packages(c("jsonlite"))
```

The script automatically handles missing `memoise` package by stubbing it out.

### Empty or Corrupted JSON Files

1. Check that pigment database is loaded correctly
2. Verify color mixing functions work properly
3. Re-run the script with verbose output
4. Check file permissions in `www/data/` directory

### Swatches Not Appearing in UI

1. Verify JSON files exist in `www/data/`
2. Check browser console for JavaScript errors
3. Ensure `PIGMENT_DISPLAY_GROUPS` includes all pigments
4. Verify swatch CSS classes are defined

## Future Enhancements

Potential improvements:

- [ ] Support for multiple shade pigment options (currently hardcoded to J318)
- [ ] CLI arguments to generate_palettes.R for custom configurations
- [ ] Automated regeneration in CI/CD when pigments change
- [ ] Compressed/minified JSON for even faster loading
- [ ] Lazy loading of swatch images for very large palettes

## References

- RAÄ Kulturkulör: https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/
- K+S Theory: See `documentation/K_S_values_research.md`
- Pigment Database: See `documentation/database_structure.md`
