# Tar and Miscellaneous Materials Data
# Extracted from supplier_data.R - non-pigment supplier information

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


# === UNIFIED TRÄTJÄRA (WOOD TAR) DATABASE ===

# Unified tar database following pigments_unified.R pattern
# Includes masstone RGB values, K/S estimates, and supplier information
tars_db <- list(
  "Dalbränd trätjära (finast, ljusast)" = list(
    id = "Dalbränd trätjära (finast, ljusast)",
    name = "Dalbränd trätjära (finast, ljusast)",
    properties = list(
      rgb = c(205, 170, 125),  # Approximate light honey color
      K = 0.15,  # Estimated - very translucent
      S = 0.05,  # Estimated - minimal scattering
      density = 1.08  # g/cm³
    ),
    metadata = list(
      category = "tar",
      subcategory = "light",
      description = "Finaste och ljusaste tjäran, ljust honungsgul färg"
    ),
    suppliers = list(
      claessons = list(
        match = "Ljus dalbränd trätjära",
        url = "https://claessons.com/tratjaror/",
        notes = "God genomträngning, penetrerar djupt"
      )
    ),
    notes = "Lämplig för ljusa kulörer. Minst påverkan på färgton."
  ),
  
  "Ljus trätjära" = list(
    id = "Ljus trätjära",
    name = "Ljus trätjära",
    properties = list(
      rgb = c(160, 120, 80),  # Approximate medium amber
      K = 0.35,  # Estimated - moderately translucent
      S = 0.10,  # Estimated - some scattering
      density = 1.08  # g/cm³
    ),
    metadata = list(
      category = "tar",
      subcategory = "medium",
      description = "Ljusare tjära från tall, bättre färgåtergivning"
    ),
    suppliers = list(
      claessons = list(
        match = "Ljus trätjära från tall",
        url = "https://claessons.com/tratjaror/",
        notes = "Bra balans mellan skydd och färgåtergivning"
      ),
      tjarfarg = list(
        match = "Ljus trätjära",
        url = "https://www.tjarfarg.se/produkter/klassiker/ljus-tratjara/",
        notes = "Specialiserad leverantör"
      )
    ),
    notes = "Lämplig för medelmörka kulörer."
  ),
  
  "Mörk trätjära" = list(
    id = "Mörk trätjära",
    name = "Mörk trätjära",
    properties = list(
      rgb = c(80, 60, 40),  # Approximate dark brown
      K = 0.80,  # Estimated - quite opaque
      S = 0.20,  # Estimated - moderate scattering
      density = 1.08  # g/cm³
    ),
    metadata = list(
      category = "tar",
      subcategory = "dark",
      description = "Mörkare trätjära från furu, utmärkt väderskydd"
    ),
    suppliers = list(
      claessons = list(
        match = "Furutjära",
        url = "https://claessons.com/tratjaror/",
        notes = "Utmärkt väderskydd för exponerade ytor"
      ),
      biltema = list(
        match = "Äkta trätjära 1 liter",
        url = "https://www.biltema.se/bygg/farg/utomhusfarg/asfalt/akta-tratjara-1-liter-2000053045",
        notes = "Prisvärd, lättillgänglig i butik"
      ),
      tjarfarg = list(
        match = "Äkta trätjära",
        url = "https://www.tjarfarg.se/produkter/klassiker/akta-tratjara/",
        notes = "Traditionell trätjära"
      )
    ),
    notes = "Lämplig för mörka och klara kulörer (blå, grön, röd). Bäst väderskydd."
  )
)

# === TAR HELPER FUNCTIONS ===

# Get complete tar entry
get_tar <- function(id) {
  if(!id %in% names(tars_db)) return(NULL)
  tars_db[[id]]
}

# Get specific tar property
get_tar_property <- function(id, property) {
  tar <- get_tar(id)
  if(is.null(tar)) return(NULL)
  tar$properties[[property]]
}

# Get tar supplier info
get_tar_supplier_info <- function(id, supplier_name) {
  tar <- get_tar(id)
  if(is.null(tar)) return(NULL)
  tar$suppliers[[supplier_name]]
}

# Get all tars by subcategory
get_tars_by_subcategory <- function(subcategory = NULL) {
  if(is.null(subcategory)) return(names(tars_db))
  
  Filter(function(id) {
    tar <- tars_db[[id]]
    !is.null(tar$metadata$subcategory) && tar$metadata$subcategory == subcategory
  }, names(tars_db))
}

# Legacy helper: Get tars by category (for backward compatibility with old code)
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
    "Dalbränd trätjära (finast, ljusast)" = create_tar_choices("Dalbränd trätjära (finast, ljusast)"),
    "Ljus trätjära" = create_tar_choices("Ljus trätjära"),
    "Mörk trätjära" = create_tar_choices("Mörk trätjära")
  )
}


# Create filler choices (extracts from Fyllmedel category)
create_filler_choices <- function() {
  filler_ids <- c("599930", "58000", "58010", "58162", "58900", "58250")
  # Use make_choices but return as simple list (not named for optgroup)
  # Note: make_choices is defined in app.R since it's used in UI context
  choices <- setNames(filler_ids, sapply(filler_ids, function(id) paste0(km[[id]]$name, " (#", id, ")")))
  as.list(choices)
}
