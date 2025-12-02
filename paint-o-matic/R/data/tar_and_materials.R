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


# === TRÄTJÄRA (WOOD TAR) SUPPLIERS & COLORS ===

# Tar masstone RGB values (research documented in TAR_RGB_RESEARCH.txt)
tar_colors <- list(
  "Dalbränd trätjära (finast)" = list(
    rgb = c(140, 95, 45),
    hex = "#8C5F2D",
    description = "Ljus och ren, gyllengul"
  ),
  "Ljus trätjära" = list(
    rgb = c(90, 60, 35),
    hex = "#5A3C23",
    description = "Honungs- eller bärnstenfärg"
  ),
  "Mörk trätjära (billigast)" = list(
    rgb = c(50, 35, 22),
    hex = "#32231B",
    description = "Mycket mörk brun, nästan svart"
  )
)

# Swedish wood tar suppliers with products
tar_suppliers <- list(
  
  # DALBRÄND TRÄTJÄRA (FINEST)
  "dalbrands_finest" = list(
    name = "Fintjära extra prima dalbränd",
    category = "Dalbränd trätjära (finast)",
    supplier = "Claessons Trätjära",
    description = "Traditionellt dalbränd trätjära av högsta kvalitet",
    url = "https://claessons.com/tratjaror/",
    notes = "Finaste kvaliteten, lämplig för alla ändamål"
  ),
  
  "dalbrands_prima" = list(
    name = "Fintjära prima dalbränd",
    category = "Dalbränd trätjära (finast)",
    supplier = "Claessons Trätjära",
    description = "Dalbränd trätjära, prima kvalitet",
    url = "https://claessons.com/tratjaror/",
    notes = "Utmärkt kvalitet, något mörkare än extra prima"
  ),
  
  "ottosson_dalbrands" = list(
    name = "Svensk dalbränd trätjära",
    category = "Dalbränd trätjära (finast)",
    supplier = "Ottosson Färgmakeri",
    description = "Äkta svensk dalbränd trätjära",
    url = "https://ottossonfarg.com/produkt/svensk-dalbrand-tratjara/",
    notes = "Svensktillverkad, ekologiskt hållbar"
  ),
  
  # LJUS TRÄTJÄRA
  "claessons_light" = list(
    name = "Fintjära prima dalbränd (ljus)",
    category = "Ljus trätjära",
    supplier = "Claessons Trätjära",
    description = "Ljusare variant för grundbehandling",
    url = "https://claessons.com/tratjaror/",
    notes = "God genomträngning, penetrerar djupt"
  ),
  
  # MÖRK TRÄTJÄRA
  "claessons_dark" = list(
    name = "Furutjära",
    category = "Mörk trätjära",
    supplier = "Claessons Trätjära",
    description = "Mörkare trätjära från furu",
    url = "https://claessons.com/tratjaror/",
    notes = "Utmärkt väderskydd för exponerade ytor"
  ),
  
  "biltema_dark" = list(
    name = "Äkta trätjära 1 liter",
    category = "Mörk trätjära",
    supplier = "Biltema",
    description = "Äkta trätjära för ytbehandling av trä",
    url = "https://www.biltema.se/bygg/farg/utomhusfarg/asfalt/akta-tratjara-1-liter-2000053045",
    notes = "Prisvärd, lättillgänglig i butik"
  ),
  
  "tjarfarg_dark" = list(
    name = "Äkta trätjära",
    category = "Mörk trätjära",
    supplier = "Tjärfärg.se",
    description = "Traditionell trätjära för träbehandling",
    url = "https://www.tjarfarg.se/produkter/klassiker/akta-tratjara/",
    notes = "Specialiserad leverantör av tjärprodukter"
  )
)

# Helper: Get tars by category
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
    "Dalbränd trätjära (finast)" = create_tar_choices("Dalbränd trätjära (finast)"),
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
