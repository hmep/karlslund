# Paint-o-matic Code Structure Guide

Quick reference for developers working on the Paint-o-matic application.

## File Structure

```
paint-o-matic/
├── app.R                          # Main Shiny app (UI + Server)
├── global.R                       # Module loader (sources all dependencies)
├── www/                           # Web resources (CSS, JS, icons, PWA files) [NEW in v0.10.5-pwa]
│   ├── css/
│   │   └── custom.css            # Application styles
│   ├── js/
│   │   ├── utils.js              # Utility functions
│   │   ├── fullscreen.js         # Fullscreen preview
│   │   └── favorites.js          # localStorage favorites
│   ├── icons/
│   │   ├── icon-192.png          # PWA icon (192×192px)
│   │   └── icon-512.png          # PWA icon (512×512px)
│   ├── manifest.json             # PWA manifest
│   ├── service-worker.js         # Service worker for caching
│   ├── service-worker-register.js # Service worker registration
│   └── README.md                 # Web resources documentation
├── R/
│   ├── data/
│   │   ├── constants.R           # Shared constants (coverage rates, K/S values)
│   │   ├── pigments_unified.R    # Unified pigment database (NEW in Phase 2B)
│   │   └── tar_and_materials.R   # Tar colors, suppliers, misc materials
│   ├── utils/
│   │   ├── formatting.R          # Swedish formatting, parsing, rounding
│   │   ├── color_mixing.R        # RGB color mixing and preview rendering
│   │   ├── calculations.R        # Recipe calculations and pigment distribution
│   │   ├── km_compensation.R     # Kubelka-Munk compensation logic
│   │   └── pigment_helpers.R     # Helper functions for unified database (NEW)
│   └── ui/
│       └── ui_helpers.R           # Reusable UI components
├── documentation/
│   └── database_structure.md     # Unified database documentation (NEW)
├── tools/
│   └── verify_unified_db.R       # Database verification tests (NEW)
├── kulturkulor_recipes.r         # Kulturkulör recipe definitions (part 1)
├── kulturkulor_recipes_part2.r   # Kulturkulör recipe definitions (part 2)
└── kulturkulor_recipes_part3.r   # Kulturkulör recipe definitions (part 3)
```

## Module Responsibilities

### `global.R`
- Loads all required packages
- Configures Swedish locale
- Sources all utility and data modules
- **NEW:** Creates backward compatibility layer from unified database
- Loads and merges Kulturkulör recipes
- **When to edit**: Adding new dependencies or modules

### `R/data/constants.R`
Contains shared constants:
- `COVERAGE_M2_PER_LITER` - Coverage rate per liter
- `PACKING_FACTOR` - Packing factor for volume calculations
- `ZINC_REFERENCE_RATIO` - Reference zinc ratio for compensation
- K/S values for white pigments
- **When to edit**: Changing default values or adding new constants

### `R/data/pigments_unified.R` ⭐ NEW in Phase 2B
Contains unified pigment database:
- `pigments_db` - Complete pigment database with all information in one structure
- Each entry contains: id, name, properties, metadata, suppliers, notes
- 56 total pigments, 24 RAÄ approved
- **When to edit**: Adding new pigments or updating pigment properties
- **See also**: `documentation/database_structure.md` for detailed documentation

### `R/data/tar_and_materials.R`
Contains non-pigment materials:
- `misc_materials` - Solvents and additives (terpentine, etc.)
- `tar_colors` - Tar color definitions with RGB values
- `tar_suppliers` - Tar supplier information
- Helper functions for creating dropdown choices
- **When to edit**: Adding suppliers or tar products

### `R/utils/pigment_helpers.R` ⭐ NEW in Phase 2B
Helper functions for unified pigment database:
- `get_pigment(id)` - Get complete pigment entry
- `get_pigment_property(id, property)` - Get specific property
- `is_raa_pigment(id)` - Check RAÄ approval status
- `get_pigments_by_category(category)` - Filter by category
- `get_supplier_info(id, supplier_name)` - Get supplier information
- **When to edit**: Adding new helper functions for database access

### Backward Compatibility (Auto-generated in `global.R`)
The following structures are automatically generated from `pigments_db`:
- `km` - Legacy pigment properties structure
- `suppliers` - Legacy supplier lookup
- `raa_pigments` - List of RAÄ approved pigment IDs
- `pigment_name_to_id` - Name-to-ID reverse lookup

⚠️ **Important**: Do not edit `km`, `suppliers`, `raa_pigments`, or `pigment_name_to_id` directly. 
They are derived from `pigments_db`. Edit `pigments_unified.R` instead.
- `suppliers` - Pigment supplier links and info
- `misc_materials` - Miscellaneous materials (terpentine, etc.)
- `tar_colors` - Tar color definitions with RGB values
- `tar_suppliers` - Tar supplier information
- Helper functions for creating dropdown choices
- **When to edit**: Adding suppliers or tar products

### `R/utils/formatting.R`
Number formatting and parsing:
- `format_swe(x, digits)` - Format number with Swedish locale (comma decimal)
- `parse_numeric(x, default)` - Parse numeric input (handles comma/dot)
- `smart_round(weight)` - Smart rounding based on weight magnitude
- `safe_input(input, name, default, test)` - Safe input retrieval with validation
- `%||%` - Null-coalescing operator
- **When to edit**: Adding new formatting or parsing functions

### `R/utils/color_mixing.R`
Color mixing and preview:
- `mix_colors(ids, weights, pigment_list, use_tinting)` - Mix RGB colors with K+S weighting
- `render_preview(color_hex, preview_id)` - Render color preview with zoom
- `calculate_recipe_color(recipe, use_tinting)` - Calculate Kulturkulör recipe color
- **Note**: `pigment_name_to_id` is now auto-generated in `global.R`
- **When to edit**: Modifying color mixing algorithm or preview rendering

### `R/utils/calculations.R`
Recipe calculations:
- `calculate_base_properties(m, compensated_pcts, zinc_ratio, km)` - Oil absorption & density
- `calculate_pigment_amounts(target_liters, oil_absorption, density)` - Volume calculations
- `distribute_pigments(m, compensated_pcts, total_pigment_g, zinc_ratio)` - Distribute amounts
- `calculate_avg_density(m, compensated_pcts, zinc_ratio, km)` - Average density
- **When to edit**: Modifying recipe calculation logic

### `R/utils/km_compensation.R`
Kubelka-Munk compensation:
- `km_compensate_vitbas(normalized_pcts, ids, zinc_ratio)` - Compensate for zinc/titanium ratio
- **When to edit**: Modifying K-M compensation algorithm

### `app.R`
Main Shiny application:
- `generate_share_url(session, input, mix_data)` - Generate sharing URL
- `make_choices(ids)` - Create pigment dropdown choices
- `create_grouped_choices()` - Create grouped pigment choices
- `all_choices` - Combined choices for dropdown
- Swatch generation functions
- Complete UI definition
- Complete server logic
- **When to edit**: Modifying UI, adding features, changing reactives

### `www/` ⭐ NEW in v0.10.5-pwa
Web resources directory for CSS, JavaScript, icons, and PWA support:
- `css/custom.css` - All application styles (layout, buttons, fullscreen, etc.)
- `js/utils.js` - Color luminance calculations
- `js/fullscreen.js` - Fullscreen preview functionality
- `js/favorites.js` - localStorage favorites management
- `icons/` - PWA app icons (192×192px and 512×512px)
- `manifest.json` - PWA manifest (enables app installation)
- `service-worker.js` - Caches resources for offline use
- `service-worker-register.js` - Registers service worker
- `README.md` - Detailed documentation about web resources
- **When to edit**: Modifying styles, JavaScript behavior, or PWA configuration
- **See also**: `www/README.md` for comprehensive guide to web resources

## Common Tasks

### Adding a New Pigment ⭐ UPDATED for Phase 2B
1. Edit `R/data/pigments_unified.R` and add entry following the structure
2. Include: id, name, properties (oil, K, S, density, rgb), metadata, suppliers, notes
3. All legacy structures (`km`, `suppliers`, `raa_pigments`, `pigment_name_to_id`) update automatically
4. Run `source("tools/verify_unified_db.R")` to verify the addition
5. See `documentation/database_structure.md` for detailed examples

### Modifying Calculations
1. Edit appropriate function in `R/utils/calculations.R`
2. Test with various inputs
3. Verify app behavior remains correct

### Changing UI
1. Edit `ui` definition in `app.R`
2. Ensure reactive dependencies are correct
3. Test all interactions

### Modifying Styles ⭐ NEW in v0.10.5-pwa
1. Edit `www/css/custom.css`
2. Reload app to see changes
3. Verify styles across different screen sizes
4. No need to edit `app.R` unless adding new CSS files

### Modifying JavaScript Behavior ⭐ NEW in v0.10.5-pwa
1. Edit appropriate file in `www/js/` (utils.js, fullscreen.js, or favorites.js)
2. Reload app to see changes
3. Check browser console for errors
4. Test relevant functionality (e.g., fullscreen, favorites)

### Updating PWA Configuration ⭐ NEW in v0.10.5-pwa
1. Edit `www/manifest.json` for app metadata (name, colors, icons)
2. Edit `www/service-worker.js` to change cached resources
3. Update cache version in service worker when making breaking changes
4. Test PWA installation and offline functionality
5. See `www/README.md` for detailed PWA development guide

### Adding New Constants
1. Add to `R/data/constants.R`
2. Use in calculations instead of magic numbers
3. Document purpose in comment

## Development Workflow

1. **Make changes** in appropriate module
2. **Test locally** (if R environment available)
3. **Verify** no unintended side effects
4. **Document** changes in comments
5. **Commit** with clear message

## Debugging Tips

- Check `global.R` sources all required files
- Verify function names match between modules
- Check that data structures (km, suppliers) are accessible
- Use R debugger to trace function calls
- Test utility functions independently

## Testing Checklist

- [ ] App launches without errors
- [ ] All dropdowns populate correctly
- [ ] Color mixing produces expected results
- [ ] Recipe calculations are accurate
- [ ] URL sharing works
- [ ] All paint types function correctly
- [ ] No console errors

## Best Practices

1. **Keep functions focused** - One responsibility per function
2. **Document complex logic** - Add comments explaining "why"
3. **Use descriptive names** - Function/variable names should be clear
4. **Test incrementally** - Test after each change
5. **Follow R conventions** - snake_case for functions, clear parameter names
6. **Maintain backward compatibility** - Don't break existing functionality
7. **Update documentation** - Keep README files current

## Questions?

Refer to:
- `REFACTORING_SUMMARY.md` for detailed refactoring information
- Inline comments in source files for specific implementation details
- Original documentation in `documentation/` folder
