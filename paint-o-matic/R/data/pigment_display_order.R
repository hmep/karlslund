# Paint-o-matic -- blanda din egen linoljefärg!
# Copyright 2025 Tobias Hagberg
# Licens: GNU General Public License v3.0
# https://github.com/hmep/karlslund/tree/main/paint-o-matic

# pigment_display_order.R
# Single source of truth for pigment grouping and display order
# Used by app.R for dropdown menus and generate_palettes.R for swatch grouping
#
# SORTING STRATEGY:
# - Equivalents grouped together (RAÄ → Kremer → ODF)
# - Groups sorted by visual progression (light→dark for ochres/blacks, hue→value for others)
# - Maintains supplier relationships while creating intuitive color order
#
# GROUPS BY CATEGORY:
# - Greens: Hue progression (yellow-green → blue-green)
# - Blacks: Value progression (lightest grey → deepest black)
# - Yellows/Ochres: Value progression (light → dark)
# - Siennas/Umbers: Hue then value (yellow-brown → red-brown → dark brown)
# - Reds/Oranges: Hue (orange → red) then value (light → dark)
# - Whites: Coverage power (low → high)

PIGMENT_DISPLAY_ORDER <- list(
  "Vitbas (rekommenderas för optimal kulörkontroll)" = c("vitbas"),
  
  # GREENS: Hue progression (yellow-green → blue-green)
  "Gröna" = c(
    # Yellow-green umbers
    #"GU30", "40630", 
    "ODF_T_OMBRE_V",  # Greenish umber group (H:40-80°)
    # True greens - Verona group (lightest)
    "40860", "11000", "41750", "40850",  # Verona green earths (identical/equivalent)
    "40830",  # Green earth from France
    "ODF_TV_BRENT",  # Brentonico (Italian, similar to Verona)
    "ODF_TV_NICO",  # Nicosia green (Cyprus)
    "ODF_TV_ANC",  # Ancient green earth stock
    # Blue-greens (chromium)
    "44250", "KG83", "ZG65"  # Chrome oxide greens
  ),
  
  # BLACKS: Value progression (light grey → deep black)
  "Svarta" = c(
    "47700",  # Graphite silver (lightest, grey)
    "47800",  # Charcoal powder
    "ODF_VIGNE",  # Vine black
    "48401", "J318",  # Iron oxide blacks (equivalent)
    "ODF_IVOIRE",  # Ivory black (modern)
    "ODF_ROME",  # Roman black
    "BS98",  # Bone black
    "ODF_INDIEN_N",  # Indian black
    "47250",  # Lamp black (furnace black)
    "47400",  # Carbon black
    "44450",  # Mars black
    "47501"   # Manganese black (darkest)
  ),
  
  "Blåa" = c("UB88", "KB28"),
  
  # TERRA & POZZUOLI: Value progression (light → dark)
  "Terra & Pozzuoli" = c(
    "ODF_TERRE_J",  # Yellow earth from Italy (lightest)
    "BT44",  # Bright terra
    "40820", "40830", "40800",  # Terra variants
    "OT46",  # Orange terra
    "11620",  # Brown earth from Otranto
    "41600", "ODF_ERCOLANO",  # Terra/Rouge Ercolano (IDENTICAL)
    "ODF_VENITIEN",  # Venetian red
    "ODF_CASSEL"  # Cassel brown (darkest)
  ),
  
  # YELLOWS & OCHRES: Value progression (light → dark)
  "Gula & Ockror" = c(
    # Lightest yellows
    "44150",  # Naples Yellow light (lightest)
    "44160",  # Naples Yellow dark
    # Light ochres group
    "44082", "40010",  # Light ochre equivalents (RAÄ, Kremer)
    "LO92",  # Light ochre (RAÄ)
    "ODF_OXY_J",  # Oxy Apt yellow
    # Medium-light ochres
    "44086",  # Yellow ochre medium (RAÄ)
    "40130",  # Sahara ochre (Kremer)
    "ODF_ICLES",  # Iclès-Sof
    # Gold ochres group
    "GO94", "40030", "ODF_JFLES",  # Gold ochre equivalents (RAÄ, Kremer, ODF star)
    "40214",  # Gold ochre DD
    "GO94_GU30",  # Gold + green umber mix
    # Medium-dark ochres
    "J920",  # Yellow oxide
    "40020",  # Jaune Foncé (dark yellow)
    "40050",  # Havane style
    # Orange-toned ochres
    "40060",  # Jaune Orangé
    "ODF_INDIEN_J",  # Indian yellow
    # Dark ochres (moving toward brown)
    "40070",  # Sofo d'Or (gold-brown)
    "40080",  # Havane (havana brown)
    "40090"   # Sofo Rouge (red-brown, darkest)
  ),
  
  # SIENNAS & UMBERS: Hue then value (yellow-brown → red-brown → dark brown)
  "Siennas & Umbror" = c(
    # Natural siennas (yellow-brown, lightest)
    "ODF_SAHARA",  # Sienna light Sahara (lightest)
    "ODF_SIENNA",  # Natural sienna
    "44650",  # Raw sienna (RAÄ)
    # Raw umbers (neutral brown)
    "OU103", "40610", "ODF_OMBRE_D",  # Raw umber equivalents (RAÄ, Kremer, ODF)
    "ODF_OMBRE_FL", "ODF_OMBRE_NAT",  # Raw umber variants
    # Greenish umber (already in Greens, but reference here)
    "GU30", "40630",  # Greenish umber (cross-reference)
    # Grey umber
    "GRAU36",  # Grey umber
    # Burnt siennas (red-brown)
    "44620", "40470", "ODF_SI_CAL",  # Burnt sienna equivalents (RAÄ, Kremer, ODF)
    # Burnt umbers (dark brown)
    "BU100", "40720", "ODF_OMBRE_B",  # Burnt umber equivalents (RAÄ, Kremer, ODF)
    "ODF_OMBRE_CAL",  # Burnt umber from sienna
    "BRU39"   # Brown umber (darkest)
  ),
  
  # REDS & ORANGES: Hue (orange → red) then value (light → dark within hue)
  "Röda & Orange" = c(
    # Orange (hue ~25°)
    "44510",  # Orange iron oxide (brightest)
    # Light reds / English red group
    "40542", "ER48A",  # English red light/standard equivalents
    # Medium reds
    "44210",  # Red iron oxide light (RAÄ)
    "48289",  # Iron oxide red micronized
    "ODF_OXY_R",  # Oxy Apt red
    "ODF_INDIEN_R",  # Indian red
    "17280",  # Persian red
    "48651",  # Hematite intense
    # Red ochres
    "44300", "ODF_RFLES",  # Red ochre equivalents
    # Deep reds
    "44200",  # Red iron oxide transparent
    "J225",  # Iron oxide red 225
    "44220",  # Red iron oxide dark
    # Very dark reds
    "J120N",  # Iron oxide red 120N
    "48250",  # Iron oxide red dark (for rust protection)
    "J180M"   # Caput Mortuum (darkest red-brown)
  ),
  
  "Bruna" = c(
    "ODF_MARRON",  # Brown ochre
    "J663",  # Iron oxide brown
    "J686",  # Iron oxide brown dark
    "48330"  # Iron oxide brown transparent
  ),
  
  "Rostskyddande" = c("48250", "48651"),
  
  "Moderna syntetiska" = c(
    "43300",  # Titanium orange
    "23720",  # Quinacridone magenta
    "11670",  # Phthalo blue
    "23050",  # Phthalo blue primary
    "23000"   # Phthalo green
  ),
  
  "Specialfärger & Patina" = c("ODF_PATINE"),
  
  # WHITES: Coverage power (low → high)
  "Vita" = c(
    "46280",  # Buff titanium (warm, low coverage)
    "44100",  # Zinc white
    "44400",  # Titanium white rutile
    "vitbas"  # Optimized blend (recommended)
  ),
  
  "Fyllmedel" = c("599930", "58000", "58010", "58162", "58900", "58250")
)

# ============================================================================
# SUPPLIER EQUIVALENCE GUIDE
# ============================================================================
# For users selecting pigments, these are the key equivalences:
#
# CONFIRMED IDENTICAL (same source):
# - 41600 (Kremer) = ODF_ERCOLANO (Ocres de France)
#   → San Giovanni Ilarione quarry, Italy
# - 40850 = 41700 = 11000 (all Kremer Verona Green Earth)
#   → Verona region, Italy
#
# HIGH-CONFIDENCE EQUIVALENTS:
# - ER48A ≈ 40542 (English Red / English Red Light)
# - 44620 ≈ 40470 ≈ ODF_SI_CAL (Burnt Sienna)
# - GU30 ≈ 40630 ≈ ODF_T_OMBRE_V (Greenish Umber)
# - OU103 ≈ 40610 ≈ ODF_OMBRE_D (Raw Umber, Cyprus)
# - BU100 ≈ 40720 ≈ ODF_OMBRE_B (Burnt Umber, Cyprus)
# - 44082 ≈ 40010 (Light Yellow Ochre)
#
# MEDIUM-CONFIDENCE EQUIVALENTS:
# - GO94 ≈ 40030 ≈ ODF_JFLES (Gold Ochre)
# - 44300 ≈ ODF_RFLES (Red Ochre)
# - 44650 ≈ ODF_SIENNA (Natural Sienna)
#
# Users can choose between suppliers based on:
# - Availability (RAÄ/Swedish vs Kremer/German vs Ocres de France/French)
# - Price
# - Minimum order quantities
# - Shipping considerations
# - Certification requirements (e.g., Ocres de France EPV certification)
# ============================================================================
