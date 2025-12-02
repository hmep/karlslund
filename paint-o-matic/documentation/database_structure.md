# Pigment Database Structure

## Overview
The pigment database is defined in `R/data/pigments_unified.R` and contains all pigment information in a single unified structure.

## Structure
Each pigment entry contains:

- **id**: Unique identifier (Kremer number, RAÄ code, or custom)
- **name**: Display name (e.g., "Gulocker PY43", "Järnoxidsvart nr 318")
- **properties**: Physical and optical properties for calculations
  - `oil`: Oil absorption percentage
  - `K`: Kubelka-Munk absorption coefficient
  - `S`: Kubelka-Munk scattering coefficient
  - `density`: Density in g/cm³
  - `rgb`: RGB color values as vector c(r, g, b)
- **metadata**: Classification and attributes
  - `is_raa`: RAÄ (Riksantikvarieämbetet) approved
  - `is_tar_compatible`: Safe for tar oil paint
  - `category`: Pigment category (earth, synthetic, oxide, etc.)
  - `is_computed`: For computed blends (optional)
  - `components`: List of component pigment IDs (for computed pigments)
- **suppliers**: Where to purchase (optional, can be NULL)
  - `kremer`: Kremer Pigmente product info
    - `id`: Product ID
    - `match`: Type of match ("exact", "equivalent", "mix")
    - `url`: Product URL
  - `ottosson`: Ottosson Färgmakeri product info
  - `claessons`: Claessons product info
  - `pigmentum`: Pigmentum product info (if available)
- **notes**: Additional documentation

## Example Entry

```r
"44450" = list(
  id = "44450",
  name = "Svartoxid PBk11",
  
  properties = list(
    oil = 15,
    K = 2.40,
    S = 1.10,
    density = 5.21,
    rgb = c(28, 38, 38)
  ),
  
  metadata = list(
    is_raa = FALSE,
    is_tar_compatible = TRUE,
    category = "black"
  ),
  
  suppliers = list(
    kremer = list(
      id = "47000",
      match = "equivalent",
      url = "https://www.kremer-pigmente.com/en/shop/pigments/47000-black-iron-oxide"
    ),
    ottosson = list(
      name = "Järnoxidsvart",
      url = "https://ottossonfarg.com/produkt/jarnoxidsvart/"
    )
  ),
  
  notes = "Järnoxidsvart med hög täckförmåga. Finns hos Kremer, Ottosson och Claessons."
)
```

## Categories

Pigments are classified into the following categories:
- `white` - White pigments (titanium, zinc)
- `black` - Black pigments (iron oxide black, bone black, etc.)
- `blue` - Blue pigments (ultramarine, phthalocyanine, cobalt)
- `green` - Green pigments (chromium oxide, earth greens)
- `yellow` - Yellow ochres and synthetic yellows
- `earth` - Earth pigments (terra, sienna, umber)
- `oxide` - Synthetic iron oxides (reds, oranges, browns)
- `filler` - Fillers and extenders (chalk, kaolin, etc.)
- `computed` - Computed blends (vitbas, GO94_GU30)

## Backward Compatibility

The unified database automatically generates the following legacy structures in `global.R`:

### `km` - Legacy pigment properties structure
```r
km <- lapply(pigments_db, function(p) {
  props <- p$properties
  c(list(name = p$name), props)
})
```
Allows existing code using `km[[id]]$oil` to continue working.

### `suppliers` - Legacy supplier lookup
```r
suppliers <- lapply(pigments_db, function(p) {
  # Flatten structure to match old format
  list(
    name = p$name,
    kremer_match = ...,
    kremer_id = ...,
    kremer_url = ...,
    # etc.
  )
})
```
Allows existing code using `suppliers[[id]]` to continue working.

### `raa_pigments` - List of RAÄ approved pigment IDs
```r
raa_pigments <- names(pigments_db)[
  sapply(pigments_db, function(p) isTRUE(p$metadata$is_raa))
]
```
Allows existing code using `id %in% raa_pigments` to continue working.

### `pigment_name_to_id` - Name-to-ID reverse lookup
```r
pigment_name_to_id <- setNames(
  names(pigments_db), 
  sapply(pigments_db, function(p) p$name)
)
```
Auto-generated from unified database, eliminates duplication.

## Helper Functions

See `R/utils/pigment_helpers.R` for clean API:

### Basic Access
- `get_pigment(id)` - Get complete pigment entry
- `get_pigment_property(id, property)` - Get specific property value
- `get_pigment_name(id)` - Get pigment name by ID
- `get_pigment_id(name)` - Get pigment ID by name (reverse lookup)

### Metadata Queries
- `is_raa_pigment(id)` - Check if pigment is RAÄ approved
- `is_tar_compatible(id)` - Check if pigment is safe for tar oil paint

### Filtering
- `get_pigments_by_category(category)` - Get all pigments in a category
- `get_raa_pigments()` - Get all RAÄ approved pigments (returns named vector)

### Supplier Information
- `get_supplier_info(id, supplier_name = NULL)` - Get supplier info for pigment
  - If `supplier_name` is NULL, returns all suppliers
  - If specified, returns info for that specific supplier

## Adding New Pigments

To add a new pigment:

1. Edit `R/data/pigments_unified.R`
2. Add a new entry following the structure above
3. All legacy variables (`km`, `suppliers`, `raa_pigments`, `pigment_name_to_id`) update automatically
4. No need to update multiple files

Example:
```r
"NEW_ID" = list(
  id = "NEW_ID",
  name = "New Pigment Name",
  properties = list(
    oil = 25,
    K = 0.5,
    S = 0.5,
    density = 3.0,
    rgb = c(100, 100, 100)
  ),
  metadata = list(
    is_raa = FALSE,
    is_tar_compatible = TRUE,
    category = "earth"
  ),
  suppliers = NULL,  # or list with supplier info
  notes = "Description of the pigment"
)
```

## Database Statistics

Current database contains:
- **56 total pigments**
- **24 RAÄ approved pigments**
- **2 computed pigments** (vitbas, GO94_GU30)
- **53 pigments with supplier information**

ID formats:
- Numeric IDs (e.g., "44450", "40850") - 32 pigments
- J-prefix codes (e.g., "J225", "J318") - 5 pigments
- RAÄ letter codes (e.g., "KG83", "UB88") - 19 pigments

## Migration Benefits

This unified structure provides:
- ✅ **Single source of truth** - All pigment data in one place
- ✅ **Easier maintenance** - Update in one location instead of three
- ✅ **No duplication** - `pigment_name_to_id` auto-generated
- ✅ **Better organization** - Clear structure with categories and metadata
- ✅ **Backward compatible** - All existing code continues to work
- ✅ **Extensible** - Easy to add new fields (e.g., lightfastness, toxicity)
- ✅ **Ready for SQLite** - Structure can be easily migrated to relational database

## Future Enhancements

The unified structure makes it easy to add:
- Lightfastness ratings (ASTM I-V)
- Toxicity information
- Historical information and dates
- Alternative names and synonyms
- Color index (CI) codes
- Batch tracking and lot numbers
- Price information
- Stock levels
- User ratings and reviews

## Related Files

- `R/data/pigments_unified.R` - Main unified database
- `R/data/tar_and_materials.R` - Tar colors, tar suppliers, miscellaneous materials
- `R/utils/pigment_helpers.R` - Helper functions for database access
- `global.R` - Loads database and creates backward compatibility layer
