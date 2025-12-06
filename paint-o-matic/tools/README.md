# Paint-o-matic Tools

Utility scripts for development, testing, and maintenance.

## Validation Tools

### validate_pigments.R
**Purpose**: Comprehensive validation of pigment database and display group configuration.

**Usage**:
```bash
cd paint-o-matic
Rscript tools/validate_pigments.R
```

**What it checks**:
- ✅ Pigments in database but not in display groups (will not appear in dropdowns)
- ✅ Display groups referencing non-existent pigments (broken references)
- ✅ Duplicate pigment entries in display groups
- ✅ Missing or invalid properties (oil, K, S, density, RGB)
- ✅ Missing category metadata
- ✅ Supplier coverage statistics
- ✅ Category and display group distribution

**When to use**:
- After adding new pigments
- Before committing changes to pigment database
- When troubleshooting dropdown issues
- To get overview of database statistics

**Example output**:
```
=== Pigment Database Validation ===

Total pigments in database: 56
Total pigments in display groups: 54
Unique pigments in display groups: 54

✅ All pigments in database are mapped to display groups
✅ All display group references are valid
✅ All pigments have valid properties

=== Summary ===
✅ Pigment database is healthy!
```

### verify_unified_db.R
**Purpose**: Backward compatibility tests for unified database structure.

**Usage**:
```bash
cd paint-o-matic
Rscript tools/verify_unified_db.R
```

**What it checks**:
- Database structure integrity
- Expected pigment count (56)
- RAÄ pigment identification (24 expected)
- Tar and material database (misc_db)
- Property access patterns
- Computed pigment flags

**When to use**:
- After refactoring database structure
- After migrating from legacy structure
- To verify installation integrity
- Regression testing

## Extraction Tools

### extract_raa_colors_v2.py
**Purpose**: Extract RAÄ Kulturkulör color codes and metadata from source documents.

**Usage**:
```bash
cd paint-o-matic
python tools/extract_raa_colors_v2.py
```

**What it does**:
- Parses RAÄ color documentation
- Extracts color codes, names, and properties
- Generates pigment database entries
- Useful for bulk imports of RAÄ colors

**When to use**:
- Adding new RAÄ Kulturkulör codes
- Updating RAÄ color database
- Verifying RAÄ color accuracy

## Best Practices

### Before Committing Changes
1. Run `validate_pigments.R` to check for issues
2. Fix any errors or warnings
3. Verify pigment counts match expectations
4. Commit changes

### Adding New Pigments
1. Add pigment to `R/data/pigments_unified.R`
2. Add pigment ID to appropriate category in `app.R` → `PIGMENT_DISPLAY_GROUPS`
3. Run `validate_pigments.R`
4. Fix any warnings
5. Test in app (optional)

### Troubleshooting

**Issue**: Pigment doesn't appear in dropdown
- Run `validate_pigments.R`
- Look for "Pigments in database but NOT in any display group"
- Add missing pigment ID to `PIGMENT_DISPLAY_GROUPS`

**Issue**: Warning about missing pigment  
- Run `validate_pigments.R`
- Look for "Display groups but NOT in database"
- Either add the pigment or remove from `PIGMENT_DISPLAY_GROUPS`

**Issue**: Color preview wrong
- Check RGB values in `pigments_unified.R`
- Update `properties$rgb` vector
- RGB values should match actual pigment color

## Related Documentation

- `../documentation/ADDING_PIGMENTS.md` - How to add new pigments
- `../documentation/database_structure.md` - Database structure reference
- `../documentation/PHASE_2B_SUMMARY.md` - Unified database migration

## Future Tools (Ideas)

Potential tools that could be added:
- `generate_pigment_template.R` - Generate template for new pigment
- `check_k_s_values.R` - Validate K/S values are reasonable
- `compare_pigments.R` - Compare two pigment definitions
- `export_pigments.R` - Export database to CSV/JSON
- `import_pigments.R` - Import pigments from CSV/JSON
- `test_color_mixing.R` - Unit tests for mixing calculations
