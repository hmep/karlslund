# Paint-o-matic Code Structure Guide

Quick reference for developers working on the Paint-o-matic application.

## File Structure

```
paint-o-matic/
├── app.R                          # Main Shiny app (UI + Server)
├── global.R                       # Module loader (sources all dependencies)
├── R/
│   ├── data/
│   │   ├── constants.R           # Shared constants (coverage rates, K/S values)
│   │   ├── pigment_database.R    # Pigment data (km list, raa_pigments)
│   │   └── supplier_data.R       # Supplier information and helpers
│   └── utils/
│       ├── formatting.R          # Swedish formatting, parsing, rounding
│       ├── color_mixing.R        # RGB color mixing and preview rendering
│       ├── calculations.R        # Recipe calculations and pigment distribution
│       └── km_compensation.R     # Kubelka-Munk compensation logic
├── kulturkulor_recipes.r         # Kulturkulör recipe definitions (part 1)
├── kulturkulor_recipes_part2.r   # Kulturkulör recipe definitions (part 2)
└── kulturkulor_recipes_part3.r   # Kulturkulör recipe definitions (part 3)
```

## Module Responsibilities

### `global.R`
- Loads all required packages
- Configures Swedish locale
- Sources all utility and data modules
- Loads and merges Kulturkulör recipes
- **When to edit**: Adding new dependencies or modules

### `R/data/constants.R`
Contains shared constants:
- `COVERAGE_M2_PER_LITER` - Coverage rate per liter
- `PACKING_FACTOR` - Packing factor for volume calculations
- `ZINC_REFERENCE_RATIO` - Reference zinc ratio for compensation
- K/S values for white pigments
- **When to edit**: Changing default values or adding new constants

### `R/data/pigment_database.R`
Contains pigment data:
- `km` - Complete pigment database (name, oil absorption, K/S values, density, RGB)
- `raa_pigments` - List of RAÄ Kulturkulör pigments
- **When to edit**: Adding new pigments or updating pigment properties

### `R/data/supplier_data.R`
Contains supplier information:
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
- `pigment_name_to_id` - Name-to-ID mapping for recipes
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

## Common Tasks

### Adding a New Pigment
1. Add entry to `km` list in `R/data/pigment_database.R`
2. Optionally add supplier info in `R/data/supplier_data.R`
3. If RAÄ pigment, add to `raa_pigments` list

### Modifying Calculations
1. Edit appropriate function in `R/utils/calculations.R`
2. Test with various inputs
3. Verify app behavior remains correct

### Changing UI
1. Edit `ui` definition in `app.R`
2. Ensure reactive dependencies are correct
3. Test all interactions

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
