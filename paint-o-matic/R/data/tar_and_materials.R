# Miscellaneous Materials Database
# Consolidated database for non-pigment materials (tars, solvents, additives)
# Follows the same pattern as pigments_unified.R

misc_db <- list(
  # === WOOD TARS ===
  "TAR01" = list(
    id = "TAR01",
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
  
  "TAR02" = list(
    id = "TAR02",
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
  
  "TAR03" = list(
    id = "TAR03",
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
  ),
  
  # === SOLVENTS ===
  "SOLV01" = list(
    id = "SOLV01",
    name = "Balsamterpentin",
    properties = list(
      density = 0.86  # g/cm³ approximate
    ),
    metadata = list(
      category = "solvent",
      description = "Naturlig terpentin från tallharts. Lösningsmedel för oljefärg och lack."
    ),
    suppliers = list(
      biltema = list(
        match = "Balsamterpentin 1 liter",
        url = "https://www.biltema.se/bygg/farg/rengoringsmedel/balsamterpentin-1-liter-2000063842",
        notes = "Prisvärd, lättillgänglig i butik"
      ),
      claessons = list(
        match = "Balsamterpentin",
        url = "https://claessons.com/balsamterpentin/balsamterpentin/",
        notes = "Hög kvalitet, naturprodukt"
      ),
      kremer = list(
        match = "Pine Turpentine",
        url = "https://www.kremer-pigmente.com/en/shop/solvents-chemicals-additives/70010-pine-turpentine.html",
        notes = "Premium quality, international supplier"
      )
    ),
    notes = "Traditionellt lösningsmedel för linoljefärg."
  ),
  # === LINSEED OIL ===
  "OIL01" = list(
    id = "OIL01",
    name = "Kokt kallpressad linolja",
    properties = list(
      density = 0.92  # g/cm³ approximate
    ),
    metadata = list(
      category = "binder",
      description = "Kokt kallpressad linolja."
    ),
    suppliers = list(
      biltema = list(
        match = "Balsamterpentin 1 liter",
        url = "https://www.biltema.se/bygg/farg/rengoringsmedel/balsamterpentin-1-liter-2000063842",
        notes = "Prisvärd, lättillgänglig i butik, tveksam kvalitet (undvik helst)"
      ),
      claessons = list(
        match = "Balsamterpentin",
        url = "https://claessons.com/balsamterpentin/balsamterpentin/",
        notes = "Hög kvalitet, naturprodukt"
      ),
      kremer = list(
        match = "Pine Turpentine",
        url = "https://www.kremer-pigmente.com/en/shop/solvents-chemicals-additives/70010-pine-turpentine.html",
        notes = "Premium quality, international supplier"
      )
    ),
    notes = "Traditionellt bindemedel för linoljefärg, emulsionsfärg, äggoljetemperad, kitt, tjäroljefärg med mera."
  )
)
