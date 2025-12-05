# Testing Instructions for Tar Database Migration

## Overview
This document provides instructions for testing the tar database migration to ensure all functionality works correctly.

## Prerequisites
- R environment with required packages (shiny, shinydashboard, shinyjs, shinyWidgets, jsonlite, memoise)
- Access to the paint-o-matic directory

## Test Suite

### 1. Automated Tests

Run the verification script:
```r
cd paint-o-matic
source("tools/verify_unified_db.R")
```

**Expected Output:**
```
=== Phase 2B Verification Tests ===

1. Testing unified database loaded...
   ✓ pigments_db contains 56 pigments

[... tests 2-12 ...]

13. Testing unified tar database structure...
   ✓ tars_db contains 3 tar types

14. Testing tar database structure...
   ✓ Tar entries have correct structure

15. Testing tar helper functions - get_tar...
   ✓ get_tar works correctly

16. Testing tar helper functions - get_tar_property...
   ✓ get_tar_property works correctly

17. Testing tar helper functions - get_tars_by_subcategory...
   ✓ get_tars_by_subcategory works correctly

18. Testing tar backward compatibility - tar_colors...
   ✓ tar_colors auto-generated correctly

19. Testing tar backward compatibility - tar_suppliers...
   ✓ tar_suppliers auto-generated correctly

=== ALL TESTS PASSED ===

Unified pigment database migration successful!
- 56 pigments migrated
- 24 RAÄ pigments identified
- 3 tar types in unified database
- Backward compatibility maintained
- New helper functions available
```

### 2. Manual Application Tests

#### Test 2.1: Application Launch
```r
cd paint-o-matic
shiny::runApp()
```

**Expected:** Application launches without errors

#### Test 2.2: Tar Type Selection
1. Select "Tjäroljefärg" as paint type
2. Check the "Typ av trätjära" dropdown

**Expected:**
- Three options visible:
  - "Dalbränd trätjära (finast, ljusast)"
  - "Ljus trätjära"
  - "Mörk trätjära"
- Default selection works
- Can change between options

#### Test 2.3: Tar Recipe Calculation
1. Select "Tjäroljefärg" as paint type
2. Select a tar type
3. Add pigments (e.g., vitbas 70%, GO94 30%)
4. Set target area (e.g., 10 m²)
5. Click "Beräkna recept"

**Expected:**
- Recipe displays tar amount in grams
- Recipe displays linseed oil amount
- Recipe displays balsamterpentin amount
- No errors in console
- Color preview shows tinted color

#### Test 2.4: Recipe Download with Tar
1. Create a tar recipe as in Test 2.3
2. Click "Spara recept som textfil"
3. Open downloaded .txt file

**Expected:**
- File contains recipe details
- Tar type mentioned (e.g., "Mörk trätjära")
- All ingredients listed with amounts
- Share URL included
- Supplier information section present

#### Test 2.5: Old Recipe URL Loading
Test backward compatibility with old tar names.

Create test URLs (replace BASE_URL with actual app URL):
```
BASE_URL?paint_type=tar&tar_category=Dalbränd+trätjära+(finast)&...
BASE_URL?paint_type=tar&tar_category=Mörk+trätjära+(billigast)&...
```

**Expected:**
- Old URLs load without errors
- Tar type automatically maps to new name:
  - "Dalbränd trätjära (finast)" → "Dalbränd trätjära (finast, ljusast)"
  - "Mörk trätjära (billigast)" → "Mörk trätjära"
- Recipe displays correctly

#### Test 2.6: New Recipe URL Sharing
1. Create a tar recipe
2. Copy the share URL
3. Open URL in new browser/session

**Expected:**
- Recipe loads exactly as saved
- Tar type preserved
- All pigments and amounts restored

### 3. Helper Function Tests

Test the new helper functions in R console:

```r
source("global.R")

# Test get_tar
tar <- get_tar("Ljus trätjära")
print(tar$name)  # Should print: "Ljus trätjära"
print(tar$properties$rgb)  # Should print: 160 120 80

# Test get_tar_property
rgb <- get_tar_property("Mörk trätjära", "rgb")
print(rgb)  # Should print: 80 60 40

K <- get_tar_property("Ljus trätjära", "K")
print(K)  # Should print: 0.35

# Test get_tar_supplier_info
supplier <- get_tar_supplier_info("Mörk trätjära", "biltema")
print(supplier$match)  # Should print: "Äkta trätjära 1 liter"
print(supplier$url)    # Should print URL

# Test get_tars_by_subcategory
light <- get_tars_by_subcategory("light")
print(light)  # Should print: "Dalbränd trätjära (finast, ljusast)"

all_tars <- get_tars_by_subcategory()
print(length(all_tars))  # Should print: 3
```

**Expected:** All commands execute without errors and return correct values

### 4. Backward Compatibility Tests

Test that old code still works:

```r
source("global.R")

# Test tar_colors (auto-generated)
print(names(tar_colors))
# Should print: "Dalbränd trätjära (finast, ljusast)" "Ljus trätjära" "Mörk trätjära"

print(tar_colors[["Ljus trätjära"]]$rgb)
# Should print: 160 120 80

print(tar_colors[["Ljus trätjära"]]$hex)
# Should print: #A07850

# Test tar_suppliers (auto-generated)
print(length(tar_suppliers))
# Should print: 6 (total supplier entries)

# Test legacy function
tars <- get_tars_by_category("Mörk trätjära")
print(length(tars))
# Should print: 3 (Claessons, Biltema, Tjärfärg)
```

**Expected:** All legacy structures work as before

## Troubleshooting

### Issue: "tar_suppliers not found"
**Solution:** Ensure global.R is sourced before using tar-related functions

### Issue: "tar_colors names don't match"
**Solution:** Check that the keys are:
- "Dalbränd trätjära (finast, ljusast)" (not "finast")
- "Ljus trätjära"
- "Mörk trätjära" (not "billigast")

### Issue: Old URLs not loading
**Solution:** Check app.R lines 952-976 for the name mapping code

### Issue: Tests fail
**Solution:** 
1. Check that all files are properly saved
2. Verify no syntax errors in R files
3. Ensure all required packages are installed
4. Check R console for specific error messages

## Success Criteria

All tests pass if:
- ✅ Automated test suite passes (all 19 tests)
- ✅ Application launches without errors
- ✅ Tar type dropdown shows correct options
- ✅ Tar recipes calculate correctly
- ✅ Recipe downloads include tar information
- ✅ Old recipe URLs load with name mapping
- ✅ New recipe URLs work correctly
- ✅ Helper functions return correct values
- ✅ Backward compatibility structures work

## Reporting Issues

If any tests fail, document:
1. Test number/name that failed
2. Error message (if any)
3. Expected vs actual behavior
4. Console output
5. Steps to reproduce

## Next Steps After Testing

Once all tests pass:
1. Mark issue as resolved
2. Update CHANGELOG if present
3. Merge PR
4. Deploy to production
5. Monitor for any issues

## Additional Notes

- The migration is designed to be non-breaking
- Old code will continue to work via backward compatibility layer
- New code should prefer using tars_db and helper functions
- RGB values and K/S estimates are ready for future color mixing enhancements
