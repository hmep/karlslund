library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

# === SWEDISH LOCALE CONFIGURATION === 
# Set Swedish locale for number formatting (cross-platform)
swedish_locale_set <- FALSE
locale_options <- c("sv_SE.UTF-8", "sv_SE", "Swedish_Sweden.1252", "Swedish")

for(locale in locale_options) {
  result <- tryCatch({
    Sys.setlocale("LC_NUMERIC", locale)
    TRUE
  }, warning = function(w) FALSE, error = function(e) FALSE)
  
  if(result && result != "C") {
    swedish_locale_set <- TRUE
    message("Swedish locale set successfully: ", locale)
    break
  }
}

if(!swedish_locale_set) {
  message("Could not set Swedish locale, using manual formatting only")
}

# Helper functions for Swedish number formatting
format_swe <- function(x, digits = 1) {
  if(is.null(x) || is.na(x)) return("0")
  formatted <- format(round(x, digits), 
                      nsmall = digits, 
                      decimal.mark = ",", 
                      big.mark = " ",
                      trim = TRUE)
  return(formatted)
}

# Locale-independent numeric parser (handles both dots and commas from any source)
parse_numeric <- function(x, default = NA) {
  if(is.null(x) || length(x) == 0) return(default)
  if(is.numeric(x)) return(x)
  
  # Convert to character and replace comma with dot
  x_char <- as.character(x)
  x_char <- gsub(",", ".", x_char)
  x_char <- gsub(" ", "", x_char)  # Remove spaces (thousand separators)
  
  result <- suppressWarnings(as.numeric(x_char))
  if(is.na(result)) return(default)
  return(result)
}

# === PIGMENTDATABAS ===
# Extended with RAÄ Kulturkulör pigments
# K and S values estimated based on pigment type and characteristics
# Oil absorption values from Kremer datablad and industry standards
# Substrate factors based on Swedish linseed oil paint literature:
# - Tidigare målat trä: 0.90 (already oil-saturated, needs less)
# - Hyvlat obehandlat: 1.20 (baseline smooth wood + 20% absorption)
# - Sågad råspont: 1.50 (rough surface requires 50% more)
# - Metall/plåt: 1.00 (non-absorbent, standard coverage)
# - Puts/betong: 2.00 (very absorbent, typically 4 coats vs 2)
# - Gips/sten: 1.30 (moderate absorption)
# Coverage rates (use): 3 L/100m² = full 3-coat system (12 m²/L each coat)
#                       2 L/100m² = 2-coat maintenance (25 m²/L each, optimistic)
#                       1.5 L/100m² = single touch-up coat (15 m²/L)

km <- list(
  # BASE WHITES
  "vitbas" = list(name = "Kubelka-Munk vitbas (titan/zink)", oil = 17, K = 0.00, S = 2.20),
  "44100" = list(name = "Zinkvitt PW4", oil = 20, K = 0.00, S = 1.66),
  "44400" = list(name = "Titanvitt Rutile PW6", oil = 15, K = 0.00, S = 2.55),
  
  # GREENS
  "40400" = list(name = "Viridian PG18", oil = 40, K = 1.20, S = 1.50),
  "41700" = list(name = "Malakit naturlig", oil = 45, K = 0.90, S = 0.80),
  "11100" = list(name = "Phthalogrön PG7", oil = 50, K = 1.50, S = 1.40),
  "KG83" = list(name = "Kromoxidgrönt nr GN 83 (RAÄ)", oil = 18, K = 1.15, S = 1.75),
  "ZG65" = list(name = "Zinkgrönt nr 65 (RAÄ)", oil = 19, K = 1.00, S = 1.60),
  "40850" = list(name = "Grön jord Böhmen", oil = 35, K = 0.60, S = 0.55),
  "40860" = list(name = "Grön jord Verona", oil = 35, K = 0.65, S = 0.60),
  "GU30" = list(name = "Grön umbra nr 30 (RAÄ)", oil = 50, K = 0.85, S = 0.48),
  
  # BLACKS
  "44450" = list(name = "Svartoxid PBk11", oil = 15, K = 2.40, S = 1.10),
  "J318" = list(name = "Järnoxidsvart nr 318 (RAÄ)", oil = 16, K = 2.35, S = 1.08),
  "BS98" = list(name = "Bensvart nr 98 (RAÄ)", oil = 50, K = 2.60, S = 0.95),
  
  # BLUES
  "11670" = list(name = "Phthaloblå PB15:3", oil = 45, K = 1.80, S = 1.20),
  "UB88" = list(name = "Ultramarinblått nr 88 (RAÄ)", oil = 42, K = 1.65, S = 0.88),
  "KB28" = list(name = "Koboltblått nr 28 (RAÄ)", oil = 35, K = 1.40, S = 0.92),
  
  # EARTH COLORS - TERRA & POZZUOLI
  "40820" = list(name = "Terra di Pozzuoli", oil = 40, K = 0.70, S = 0.55),
  "40800" = list(name = "Terra di Siena natur", oil = 40, K = 0.60, S = 0.50),
  "40830" = list(name = "Terra di Ercolano", oil = 40, K = 0.75, S = 0.55),
  "BT44" = list(name = "Bränd terra nr 44 (RAÄ)", oil = 38, K = 0.78, S = 0.52),
  "OT46" = list(name = "Obränd terra nr 46 (RAÄ)", oil = 38, K = 0.62, S = 0.48),
  
  # YELLOWS & OCHRES
  "44082" = list(name = "Gul ockra ljus", oil = 20, K = 0.48, S = 0.38),
  "44086" = list(name = "Gul ockra mörk", oil = 20, K = 0.55, S = 0.45),
  "44150" = list(name = "Naples Yellow light", oil = 35, K = 0.40, S = 0.70),
  "44160" = list(name = "Naples Yellow dark", oil = 35, K = 0.50, S = 0.65),
  "J920" = list(name = "Järnoxidgult nr 920 (RAÄ)", oil = 22, K = 0.52, S = 0.42),
  "LO92" = list(name = "Ljusockra nr 92 (RAÄ)", oil = 21, K = 0.46, S = 0.40),
  "GO94" = list(name = "Guldockra nr 94 (RAÄ)", oil = 23, K = 0.58, S = 0.46),
  "GO94_GU30" = list(name = "50% Guldockra + 50% Grön umbra (RAÄ)", oil = 40, K = 0.72, S = 0.47),
  
  # SIENNAS & UMBERS
  "44650" = list(name = "Raw Sienna Italien", oil = 45, K = 0.55, S = 0.45),
  "44620" = list(name = "Burnt Sienna Italien", oil = 50, K = 0.75, S = 0.50),
  "OU103" = list(name = "Obränd umbra nr 103 (RAÄ)", oil = 52, K = 0.92, S = 0.46),
  "BU100" = list(name = "Bränd umbra nr 100 (RAÄ)", oil = 56, K = 1.12, S = 0.52),
  "BRU39" = list(name = "Brun umbra nr 39 (RAÄ)", oil = 54, K = 1.05, S = 0.48),
  "GRAU36" = list(name = "Grå umbra nr 36 (RAÄ)", oil = 48, K = 1.20, S = 0.55),
  
  # REDS & ORANGES - IRON OXIDES
  "44300" = list(name = "Transparent brunoxid", oil = 50, K = 0.80, S = 0.22),
  "44200" = list(name = "Röd järnoxid transparent", oil = 47, K = 0.90, S = 0.12),
  "44210" = list(name = "Röd järnoxid ljus", oil = 47, K = 0.80, S = 0.25),
  "44220" = list(name = "Röd järnoxid mörk", oil = 47, K = 1.00, S = 0.35),
  "44510" = list(name = "Orange järnoxid", oil = 47, K = 0.55, S = 0.85),
  "J225" = list(name = "Järnoxidrött nr 225 (RAÄ)", oil = 48, K = 0.95, S = 0.32),
  "J180M" = list(name = "Järnoxidrött nr 180M Caput Mortuum (RAÄ)", oil = 48, K = 1.15, S = 0.28),
  "J120N" = list(name = "Järnoxidrött nr 120N (RAÄ)", oil = 47, K = 0.85, S = 0.30),
  "ER48A" = list(name = "Engelskt rött nr 48A (RAÄ)", oil = 30, K = 0.75, S = 0.40),
  
  # BROWNS - IRON OXIDES
  "J663" = list(name = "Järnoxidbrunt nr 663 (RAÄ)", oil = 50, K = 0.88, S = 0.38),
  "J686" = list(name = "Järnoxidbrunt nr 686 (RAÄ)", oil = 52, K = 0.92, S = 0.35)
)

# RGB MASSTONE VALUES
# All RAÄ pigments marked with "NCS [code]" are converted from official RAÄ specifications
# Conversions performed using NCS color theory algorithm (blackness, chromaticness, hue)
# NCS codes sourced from RAÄ "Vårda väl" PDFs (Riksantikvarieämbetet 2013-2014)
# Non-RAÄ pigments use industry-standard color values
rgb <- list(
  # WHITES
  "vitbas" = c(255, 255, 255),
  "44100" = c(255, 255, 255),
  "44400" = c(255, 255, 255),
  
  # GREENS
  "40400" = c(30, 120, 80),      # Viridian - transparent bluish green
  "41700" = c(70, 160, 100),     # Malachite - bright mineral green
  "11100" = c(0, 100, 50),       # Phthalo green - intense dark green
  "KG83" = c(68, 133, 46),       # Kromoxidgrönt nr GN 83 - NCS 4834-G26Y
  "ZG65" = c(55, 138, 26),       # Zinkgrönt nr 65 - NCS 4644-G26Y
  "40850" = c(90, 120, 70),      # Grön jord Böhmen - earthy green
  "40860" = c(100, 130, 80),     # Grön jord Verona
  "GU30" = c(56, 56, 36),        # Grön Umbra nr 30 - NCS 7808-Y02R
  
  # BLACKS
  "44450" = c(28, 38, 38),       # Svartoxid - warm black
  "J318" = c(27, 26, 27),        # Järnoxidsvart nr 318 - NCS 8901-R48B
  "BS98" = c(23, 23, 25),        # Bensvart nr 98 - NCS 9001-B16G
  
  # BLUES
  "11670" = c(0, 70, 130),       # Phthalo blue - deep cyan blue
  "UB88" = c(41, 15, 71),        # Ultramarinblått nr 88 - NCS 6232-R68B
  "KB28" = c(48, 13, 138),       # Koboltblått nr 28 - NCS 3263-R78B
  
  # EARTH COLORS - TERRA
  "40820" = c(180, 80, 60),      # Terra di Pozzuoli - reddish earth
  "40800" = c(170, 110, 70),     # Terra di Siena natur - warm brown
  "40830" = c(175, 85, 65),      # Terra di Ercolano - dark red earth
  "BT44" = c(120, 53, 26),       # Bränd Terra nr 44 - NCS 5337-Y71R
  "OT46" = c(130, 107, 38),      # Obränd Terra nr 46 - NCS 4936-Y25R
  
  # YELLOWS & OCHRES
  "44082" = c(210, 180, 120),    # Gul ockra ljus - light yellow ochre
  "44086" = c(160, 120, 70),     # Gul ockra mörk - dark yellow ochre
  "44150" = c(240, 220, 130),    # Naples Yellow light
  "44160" = c(220, 190, 100),    # Naples Yellow dark
  "J920" = c(181, 154, 38),      # Järnoxidgult nr 920 - NCS 2956-Y19R
  "LO92" = c(166, 134, 38),      # Ljusockra nr 92 - NCS 3550-Y25R [FIXED]
  "GO94" = c(161, 131, 38),      # Guldockra nr 94 - NCS 3748-Y24R [FIXED]
  "GO94_GU30" = c(99, 92, 41),   # 50% Guldockra + 50% Grön umbra - NCS 6123-Y12R
  
  # SIENNAS & UMBERS
  "44650" = c(180, 130, 70),     # Raw Sienna - warm orange brown
  "44620" = c(160, 82, 45),      # Burnt Sienna - deep red brown
  "OU103" = c(46, 41, 26),       # Obränd Umbra nr 103 - NCS 8208-Y26R
  "BU100" = c(43, 34, 31),       # Bränd Umbra nr 100 - NCS 8305-Y73R
  "BRU39" = c(64, 58, 31),       # Brun Umbra nr 39 - NCS 7513-Y17R
  "GRAU36" = c(79, 77, 87),      # Grå Umbra nr 36 - NCS 6505-R80B
  
  # REDS & ORANGES - IRON OXIDES
  "44300" = c(139, 69, 19),      # Transparent brunoxid
  "44200" = c(178, 34, 34),      # Röd järnoxid transparent
  "44210" = c(200, 70, 60),      # Röd järnoxid ljus
  "44220" = c(160, 35, 35),      # Röd järnoxid mörk
  "44510" = c(232, 97, 0),       # Orange järnoxid
  "J225" = c(130, 42, 23),       # Järnoxidrött nr 225 - NCS 4942-Y82R
  "J180M" = c(99, 31, 26),       # Caput Mortuum 180M - NCS 6129-Y93R
  "J120N" = c(128, 43, 18),      # Järnoxidrött nr 120 N - NCS 5043-Y77R
  "ER48A" = c(153, 50, 18),      # Engelskt rött nr 48 A - NCS 4053-Y76R
  
  # BROWNS - IRON OXIDES
  "J663" = c(51, 35, 31),        # Järnoxidbrunt nr 663 - NCS 8008-Y80R
  "J686" = c(41, 31, 28)         # Järnoxidbrunt nr 686 - NCS 8405-Y73R
)

# RAÄ KULTURKULÖR PIGMENTS
# Updated to include all RAÄ pigments with harmonized keys and NCS-based RGB values
raa_pigments <- c(
  # Base whites (always included)
  "vitbas", "44100", "44400",
  
  # RAÄ specific pigments (from Kulturkulör system with NCS codes)
  "J225",       # Järnoxidrött nr 225
  "J180M",      # Caput Mortuum 180M
  "J120N",      # Järnoxidrött nr 120 N
  "ER48A",      # Engelskt rött nr 48 A
  "J663",       # Järnoxidbrunt nr 663
  "J686",       # Järnoxidbrunt nr 686
  "J920",       # Järnoxidgult nr 920
  "J318",       # Järnoxidsvart nr 318
  "LO92",       # Ljusockra nr 92
  "GO94",       # Guldockra nr 94
  "GO94_GU30",  # 50% Guldockra + 50% Grön umbra
  "OU103",      # Obränd Umbra nr 103
  "BU100",      # Bränd Umbra nr 100
  "BRU39",      # Brun Umbra nr 39
  "GU30",       # Grön Umbra nr 30
  "GRAU36",     # Grå Umbra nr 36
  "BT44",       # Bränd Terra nr 44
  "OT46",       # Obränd Terra nr 46
  "BS98",       # Bensvart nr 98
  "KG83",       # Kromoxidgrönt nr GN 83
  "ZG65",       # Zinkgrönt nr 65
  "UB88",       # Ultramarinblått nr 88
  "KB28"        # Koboltblått nr 28
)

# KREMER PRODUCT LINKS FOR NON-RAÄ PIGMENTS
kremer_links <- list(
  # WHITES
  "44100" = "https://www.kremer-pigmente.com/en/shop/pigments/46300-zinc-white",
  "44400" = "https://www.kremer-pigmente.com/en/shop/pigments/46200-titanium-white-rutile",
  
  # GREENS
  "40400" = "https://www.kremer-pigmente.com/en/shop/pigments/44250-viridian-green",
  "41700" = "https://www.kremer-pigmente.com/en/shop/pigments/44400-malachite-synthetic",
  "11100" = "https://www.kremer-pigmente.com/en/shop/pigments/23000-phthalo-green-bluish-pg-7",
  "40850" = "https://www.kremer-pigmente.com/en/shop/pigments/40850-green-earth-bohemian",
  "40860" = "https://www.kremer-pigmente.com/en/shop/pigments/40860-green-earth-verona",
  
  # BLACKS
  "44450" = "https://www.kremer-pigmente.com/en/shop/pigments/47000-black-iron-oxide",
  
  # BLUES
  "11670" = "https://www.kremer-pigmente.com/en/shop/pigments/23060-phthalo-blue-royal-blue-pb-15-3",
  
  # EARTH COLORS - TERRA
  "40820" = "https://www.kremer-pigmente.com/en/shop/pigments/41550-terra-pozzuoli",
  "40800" = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian",
  "40830" = "https://www.kremer-pigmente.com/en/shop/pigments/40835-terra-di-ercolano",
  
  # YELLOWS & OCHRES
  "44082" = "https://www.kremer-pigmente.com/en/shop/pigments/40010-yellow-ochre-light",
  "44086" = "https://www.kremer-pigmente.com/en/shop/pigments/40030-yellow-ochre-dark",
  "44150" = "https://www.kremer-pigmente.com/en/shop/pigments/43010-naples-yellow-light",
  "44160" = "https://www.kremer-pigmente.com/en/shop/pigments/43000-naples-yellow-dark",
  
  # SIENNAS
  "44650" = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian",
  "44620" = "https://www.kremer-pigmente.com/en/shop/pigments/40450-burnt-sienna-italian",
  
  # REDS & ORANGES - IRON OXIDES
  "44300" = "https://www.kremer-pigmente.com/en/shop/pigments/48000-transparent-brown-oxide",
  "44200" = "https://www.kremer-pigmente.com/en/shop/pigments/48100-red-iron-oxide-transparent",
  "44210" = "https://www.kremer-pigmente.com/en/shop/pigments/48200-red-iron-oxide-light",
  "44220" = "https://www.kremer-pigmente.com/en/shop/pigments/48300-red-iron-oxide-dark",
  "44510" = "https://www.kremer-pigmente.com/en/shop/pigments/48500-orange-iron-oxide"
)

# KREMER MATCHES FOR RAÄ PIGMENTS
raa_kremer_matches <- list(
  
  # GREENS
  "KG83" = list(
    name = "Kromoxidgrönt nr GN 83 (RAÄ)",
    kremer_match = "Chrome Oxide Green (PG17)",
    kremer_id = "44200",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/44200-chrome-oxide-green",
    notes = "Kall grön, opak, samma pigmenttyp (PG17). MYCKET GOD MATCHNING - samma krompigment som RAÄ använder."
  ),
  
  "ZG65" = list(
    name = "Zinkgrönt nr 65 (RAÄ)",
    kremer_match = "Cobalt Green Dark (PG19) or Cobalt Zinc Silicate",
    kremer_id = "44350",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/44350-cobalt-green-dark",
    notes = "Zinkbaserad grön, närmaste matchning för traditionell zinkgrön"
  ),
  
  "GU30" = list(
    name = "Grön umbra nr 30 (RAÄ)",
    kremer_match = "Raw Umber, greenish (PBr8)",
    kremer_id = "40630",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/40630-raw-umber-greenish",
    notes = "Tysk obränd umbra med grönaktig nyans"
  ),
  
  # BLACKS
  "J318" = list(
    name = "Järnoxidsvart nr 318 (RAÄ)",
    kremer_match = "Iron Oxide Black 318 (PBk11)",
    kremer_id = "48400",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/48400-iron-oxide-black-318-high-tinting",
    notes = "EXAKT MATCHNING - Samma produktnummer 318! Identiskt pigment."
  ),
  
  "BS98" = list(
    name = "Bensvart nr 98 (RAÄ)",
    kremer_match = "Bone Black (PBk9)",
    kremer_id = "47100",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/47100-bone-black",
    notes = "Traditionell bensvart, tillverkad av ben. MYCKET GOD MATCHNING - samma pigment (PBk9) som RAÄ."
  ),
  
  # BLUES
  "UB88" = list(
    name = "Ultramarinblått nr 88 (RAÄ)",
    kremer_match = "Ultramarine Blue, very dark (PB29)",
    kremer_id = "45000",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/45000-ultramarine-blue-very-dark",
    notes = "Djupt ultramarin, matchar mörkt NCS-värde. MYCKET GOD MATCHNING - samma pigment (PB29)."
  ),
  
  "KB28" = list(
    name = "Koboltblått nr 28 (RAÄ)",
    kremer_match = "Cobalt Blue Medium (PB28)",
    kremer_id = "45710",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/45710-cobalt-blue-medium",
    notes = "Mellannyans koboltblått med violett underton"
  ),
  
  # TERRA & EARTH COLORS
  "BT44" = list(
    name = "Bränd terra nr 44 (RAÄ)",
    kremer_match = "Burnt Sienna, Italian (PR101)",
    kremer_id = "44620",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/40450-burnt-sienna-italian",
    notes = "Bränd röd jord, liknande bränd terra"
  ),
  
  "OT46" = list(
    name = "Obränd terra nr 46 (RAÄ)",
    kremer_match = "Raw Sienna, Italian (PY43)",
    kremer_id = "40400",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian",
    notes = "Naturlig gulbrun jord"
  ),
  
  # YELLOWS & OCHRES
  "J920" = list(
    name = "Järnoxidgult nr 920 (RAÄ)",
    kremer_match = "Yellow Ochre, dark (PY42/43)",
    kremer_id = "40030",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/40030-yellow-ochre-dark",
    notes = "Mörkare gulockra med god mättnad"
  ),
  
  "LO92" = list(
    name = "Ljusockra nr 92 (RAÄ)",
    kremer_match = "Yellow Ochre, light (PY42/43)",
    kremer_id = "40010",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/40010-yellow-ochre-light",
    notes = "Ljus gulockra"
  ),
  
  "GO94" = list(
    name = "Guldockra nr 94 (RAÄ)",
    kremer_match = "Yellow Ochre Golden, Italian (PY43)",
    kremer_id = "40015",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/40015-yellow-ochre-golden-italian",
    notes = "Guldtonad ockra, varmare än ljusockra"
  ),
  
  "GO94_GU30" = list(
    name = "50% Guldockra + 50% Grön umbra (RAÄ)",
    kremer_match = "Mix Yellow Ochre Golden + Raw Umber greenish",
    kremer_id = "40015 + 40630",
    url = c(
      "https://www.kremer-pigmente.com/en/shop/pigments/40015-yellow-ochre-golden-italian",
      "https://www.kremer-pigmente.com/en/shop/pigments/40630-raw-umber-greenish"
    ),
    notes = "Specialblandning: 50/50 viktprocent av båda pigmenten"
  ),
  
  # UMBERS
  "OU103" = list(
    name = "Obränd umbra nr 103 (RAÄ)",
    kremer_match = "Raw Umber, Cyprus (PBr8)",
    kremer_id = "40610",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/40610-raw-umber",
    notes = "Traditionell cypriotisk obränd umbra, mörkbrun-grön. MYCKET GOD MATCHNING - samma pigment (PBr8)."
  ),
  
  "BU100" = list(
    name = "Bränd umbra nr 100 (RAÄ)",
    kremer_match = "Burnt Umber, dark brown (PBr7)",
    kremer_id = "40720",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/40720-burnt-umber-dark-brown",
    notes = "Cypriotisk bränd umbra, mycket mörk. MYCKET GOD MATCHNING - samma pigment (PBr7)."
  ),
  
  "BRU39" = list(
    name = "Brun umbra nr 39 (RAÄ)",
    kremer_match = "Burnt Umber, reddish (PBr7)",
    kremer_id = "40700",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/40700-burnt-umber-reddish",
    notes = "Italiensk rödaktig bränd umbra, varmare ton"
  ),
  
  "GRAU36" = list(
    name = "Grå umbra nr 36 (RAÄ)",
    kremer_match = "Raw Umber, dark + small amount of blue pigment",
    kremer_id = "40660",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/40660-raw-umber-dark",
    notes = "Använd Raw Umber dark; tillsätt en nypa ultramarin för gråton"
  ),
  
  # IRON OXIDE REDS
  "J225" = list(
    name = "Järnoxidrött nr 225 (RAÄ)",
    kremer_match = "Red Iron Oxide, medium (PR101)",
    kremer_id = "48200",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/48200-red-iron-oxide-medium",
    notes = "Mellannyans röd järnoxid"
  ),
  
  "J180M" = list(
    name = "Caput Mortuum 180M (RAÄ)",
    kremer_match = "Caput Mortuum Violet (PR101)",
    kremer_id = "48280",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/48280-caput-mortuum-violet",
    notes = "Mörkt lila-brunt järnoxid, klassiskt caput mortuum"
  ),
  
  "J120N" = list(
    name = "Järnoxidrött nr 120N (RAÄ)",
    kremer_match = "Red Iron Oxide, light (PR101)",
    kremer_id = "48220",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/48220-red-iron-oxide-light",
    notes = "Ljusare rött järnoxid"
  ),
  
  "ER48A" = list(
    name = "Engelskt rött nr 48A (RAÄ)",
    kremer_match = "English Red (PR101)",
    kremer_id = "42100",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/42100-english-red",
    notes = "Traditionellt engelskt rött, ljust orange-rött järnoxid"
  ),
  
  # IRON OXIDE BROWNS
  "J663" = list(
    name = "Järnoxidbrunt nr 663 (RAÄ)",
    kremer_match = "Brown Iron Oxide 610 (PBr6/7)",
    kremer_id = "48610",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/48610-brown-iron-oxide-610",
    notes = "Syntetiskt brunt järnoxid, mycket mörkt"
  ),
  
  "J686" = list(
    name = "Järnoxidbrunt nr 686 (RAÄ)",
    kremer_match = "Brown Iron Oxide 686 (PBr6/7)",
    kremer_id = "48686",
    url = "https://www.kremer-pigmente.com/en/shop/pigments/48686-brown-iron-oxide-686",
    notes = "EXAKT MATCHNING - Samma produktnummer 686, troligen identiskt pigment."
  )
)

# Safe null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

# Enkel och säker choices-lista
make_choices <- function(ids) {
  setNames(ids, sapply(ids, function(id) paste0(km[[id]]$name, " (#", id, ")")))
}
all_choices <- c("Välj pigment" = "", make_choices(names(km)))

ui <- dashboardPage(
  dashboardHeader(
    title = "Paint-o-matic",
    # Version number (right side, small text)
    tags$li(
      class = "dropdown",
      tags$a(href = "#", class = "version-text", "version 0.3.0")
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
      .step { padding:24px; padding-bottom:64px; background:#f9f9f9; border-radius:12px; margin:20px 20px 80px 20px; position:relative; min-width: 360px; max-width:840px;margin:auto;}
      .footer-ref { position:relative; bottom:-44px; left:0; right:0; font-size:12px; color:#555; text-align:center; 
                    padding:12px 12px 0; border-top:1px solid #ddd; background:#f9f9f9; }
      .preview { display:block; height:300px; width:300px; border:8px solid #333; border-radius:150px; margin: auto; }
      .normalized-box, .info-box, .ready-box { background:#eee; color:black; padding:12px; border-radius:6px;}
      .normalized-box, .info-box { background:#eee; color:black; padding:12px; border-radius:6px;}
      .normalized-box { margin:10px 0;}
      .ready-box {padding: 20px; margin: 0 -12px;}
      .ready-box h3 {margin-top:0; }
      .rmargin-box {margin-right:20px;}
      .btn {margin: .12px 12px 0 0;}
      .btn-primary { color:white;}
      h2 {margin: 0 0 .5em;padding:0}
      .navbar-custom-menu .navbar-nav > li > a.version-text { font-size: 11px; color: #aaa; padding-top: 15px; padding-bottom: 15px;}
    "))),
    
    # JavaScript for clipboard
    tags$script(HTML("
      Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
        navigator.clipboard.writeText(message).then(function() {
          console.log('Kopierat!');
        }, function(err) {
          console.error('Kunde inte kopiera: ', err);
        });
      });
    ")),
    
    hidden(div(id="step1", class="step",
               h2("Steg 1 – Blanda din linoljefärg"),
               fluidRow(
                 column(6,
                        checkboxInput("raa_only", "Visa endast Kulturkulör-pigment (RAÄ)", TRUE),
                        pickerInput("p1", "Pigment 1", choices = all_choices, selected = "vitbas",
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p1", sliderInput("pct1","Andel (%)",0,100,70,1)),
                        pickerInput("p2", "Pigment 2", choices = all_choices, selected = character(0),
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p2", sliderInput("pct2","Andel (%)",0,100,0,1)),
                        pickerInput("p3", "Pigment 3", choices = all_choices, selected = character(0),
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p3", sliderInput("pct3","Andel (%)",0,100,0,1)),
                        pickerInput("p4", "Pigment 4", choices = all_choices, selected = character(0),
                                    options = pickerOptions(`live-search` = TRUE, size = 12)),
                        conditionalPanel("input.p4", sliderInput("pct4","Andel (%)",0,100,0,1)),
                        actionButton("reset_pigments", "Nollställ pigment", class="btn-default"),
                 ),
                 column(6,
                        h3("Färgprov"),
                        uiOutput("preview1"), br(),
                        tags$b("Total andel: "), textOutput("total_pct",inline=TRUE), " %", 
                        uiOutput("total_warning"), br(),
                 )
               ),
               hr(),
               actionButton("to_step2","Nästa", class="btn-primary next-btn"),
               div(class="footer-ref", "Masstone baserad på Kulturkulör NCS-koder från Riksantikvarieämbetet (RAÄ) och data från Kremer Pigmente")
    )),
    
    hidden(div(id="step2", class="step",
               h2("Steg 2 – Välj pigment för Kubelka-Munk-vitbasen"),
               fluidRow(column(
                 12,
                 p(
                   "Ange förhållandet mellan zinkoxid (zinkvitt) och titaniumdioxid (Titanvitt) i vitbasen. Kubelka-Munk-funktionen garanterar ett konsekvent resultat när färgande pigment blandas med vitbasen, oavsett blandningsförhållandet mellan de vita pigmenten."
                 ),
                 p(
                   "För utomhusfärg, välj en högre andel zinkvitt i vitbasen (gärna 30 %, om det fungerar med den önskade kulören), så blir den färdiga färgen mer motståndskraftig mot alger och mögelpåväxt."
                 ),
                 p(
                   "För inomhusfärg, välj en lägre andel zinkvitt i vitbasen (0–15 %). Zink gör å ena sidan färgfilmen hårdare, men å den andra blir den också känsligare för krackelering över tid."
                 ),
                 br(),
                 sliderInput("zinc_ratio","Zinkvitt i vitbas (%)",0,100,15,5,post="% zinkoxid"),
               ), ),
               hr(),
               actionButton("back1","Tillbaka till färgblandning", class="btn-default back-btn"),
               actionButton("to_step3","Beräkna mängd linolja", class="btn-primary next-btn"),
               div(class="footer-ref", "Kubelka-Munk-funktionen används för att bibehålla färgande pigments styrka i vitbasen konstant")
    )),
    
    hidden(div(id="step3", class="step",
               h2("Steg 3 – Beräkna mängd linolja"),
               fluidRow(
                 column(6,class="rmargin-box",
                        numericInput("area","Yta att måla (m²)",10,1,2000,1),
                        selectInput("substrate","Underlag (absorptionsfaktor)",
                                    choices=list(
                                      "Tidigare målat trä (lägst åtgång)" = 0.90,
                                      "Hyvlat obehandlat trä" = 1.20,
                                      "Sågad råspont (ej hyvlad)" = 1.50,
                                      "Metall / plåt (grundmålad)" = 1.00,
                                      "Puts / betong" = 2.00,
                                      "Gips / sten" = 1.30
                                    ),
                                    selected = 1.20),
                        radioButtons("use","Antal strykningar",choices=list("1 strykning"=1,"2 strykningar (rekommenderas inomhus)"=2,"3 strykningar (rekommenderas utomhus)"=3),selected=3),
                        hr(),
                        sliderInput("extra_oil","Extra olja (CPV-faktor)",1,2.2,1.8,0.05,post="× CPV"),
                        p(class="info-box","För blandning med färgblandare i borrmaskin och bra strykbarhet, öka gärna mängden linolja till 1,6–2,2× det kritiska oljetalet (CPV)."),
                 ),
                 column(6,class="ready-box",
                        h3("Färdigt recept"),
                        tags$p("Du blandar cirka ",textOutput("total_volume",inline=TRUE)," liter färdig färg, med sammanlagt ",textOutput("needed_pigment",inline=TRUE)," g pigment."),uiOutput("volume_warning"),
                        uiOutput("final_preview"),br(),
                        tableOutput("final_recipe"),
                        downloadButton("download_txt","Spara som textfil",class="btn-primary")
                 )
               ),
               hr(),
               actionButton("back2","Tillbaka till vitbasblandning", class="btn-default back-btn"),
               actionButton("restart","Börja om", class="btn-default"),
               div(class="footer-ref", "Åtgång per m²: RAÄ Byggnadsvård (2018). Pigmentvikt/liter: 1070 g.")
    ))
  )
)

server <- function(input, output, session) {
  show("step1")
  final_hex <- reactiveVal("#FFFFFF")
  
  # Helper function already defined at top of file, available here
  # parse_numeric() and format_swe() are already in global scope
  
  # Reset pigments
  observeEvent(input$reset_pigments, {
    updatePickerInput(session, "p1", selected = "vitbas")
    updatePickerInput(session, "p2", selected = character(0))
    updatePickerInput(session, "p3", selected = character(0))
    updatePickerInput(session, "p4", selected = character(0))
    updateSliderInput(session, "pct1", value = 70)
    updateSliderInput(session, "pct2", value = 0)
    updateSliderInput(session, "pct3", value = 0)
    updateSliderInput(session, "pct4", value = 0)
  })
  
  # RAÄ-filter
  observeEvent(input$raa_only, {
    ids <- if(input$raa_only) raa_pigments else names(km)
    choices_list <- c("Välj pigment" = "", make_choices(ids))
    current_p1 <- input$p1 %||% "vitbas"
    updatePickerInput(session, "p1", choices = choices_list, selected = current_p1)
    updatePickerInput(session, "p2", choices = choices_list, selected = input$p2)
    updatePickerInput(session, "p3", choices = choices_list, selected = input$p3)
    updatePickerInput(session, "p4", choices = choices_list, selected = input$p4)
  })
  
  # Blandning
  mix <- reactive({
    ids <- c(input$p1, input$p2, input$p3, input$p4)
    pct <- c(input$pct1 %||% 0, input$pct2 %||% 0, input$pct3 %||% 0, input$pct4 %||% 0)
    # Additional safety: check each id individually
    valid <- sapply(seq_along(ids), function(i) {
      !is.na(ids[i]) && !is.null(ids[i]) && length(ids[i]) > 0 && ids[i] != "" && pct[i] > 0
    })
    total <- sum(pct[valid])
    list(ids = ids[valid], pct = pct[valid], total = ifelse(total==0,1,total),
         has_white = "vitbas" %in% ids[valid])
  })
  
  current_color <- reactive({
    m <- mix()
    r <- g <- b <- 0
    for(i in seq_along(m$ids)){
      col <- rgb[[m$ids[i]]] %||% c(255,255,255)
      w <- m$pct[i]/m$total
      r <- r + w*col[1]; g <- g + w*col[2]; b <- b + w*col[3]
    }
    hex <- sprintf("#%02X%02X%02X", round(r), round(g), round(b))
    final_hex(hex); hex
  })
  
  output$total_pct <- renderText(format_swe(mix()$total, 1))
  output$hex1 <- renderText(current_color())
  output$preview1 <- renderUI(tags$div(class="preview", style=paste0("background:", current_color())))
  
  output$total_warning <- renderUI({
    m <- mix()
    if (m$total > 100 && length(m$ids) > 0) {
      normalized <- round((m$pct / m$total) * 100, 1)
      pigment_names <- sapply(m$ids, function(id) km[[id]]$name)
      # Format with Swedish decimals
      normalized_swe <- sapply(normalized, function(x) format_swe(x, 1))
      text_lines <- paste0(pigment_names, ": ", normalized_swe, " %", collapse = " • ")
      icon_type <- "exclamation-triangle"
      color <- "#ddd"
      border <- "#ddd"
      msg <- "Totalen överstiger 100 %. Normaliserade procentsatser som används:"
      tags$div(
        class = "alert",
        style = sprintf("margin-top: 10px; padding: 10px; background-color: %s; border: 1px solid %s; border-radius: 4px;", 
                        color, border),
        icon(icon_type),
        " ", msg, text_lines
      )
    }
  })
  
  # Navigation
  observeEvent(input$to_step2, { hide("step1"); if(mix()$has_white) show("step2") else show("step3") })
  observeEvent(input$back1, { hide("step2"); show("step1") })
  observeEvent(input$back2, { hide("step3"); if(mix()$has_white) show("step2") else show("step1") })
  observeEvent(input$to_step3, { hide("step2"); show("step3") })
  
  values <- reactiveValues(area=50, use=3, substrate=1.20, extra_oil=1.1, zinc_ratio=60)
  observe({
    values$area <- parse_numeric(input$area, 50)
    values$use <- parse_numeric(input$use, 3)
    values$substrate <- parse_numeric(input$substrate, 1.20)
    values$extra_oil <- parse_numeric(input$extra_oil, 1.1)
    values$zinc_ratio <- parse_numeric(input$zinc_ratio, 60)
  })
  
  calc <- reactive({
    # use values are in L/100m², need to convert to L/m²
    # substrate factor: 1.0 = baseline, <1.0 = less material, >1.0 = more material
    liters <- values$area * (values$use / 100) * values$substrate
    list(liters = round(liters, 2), pigment = round(liters * 1070, 0))
  })
  
  output$needed_volume <- renderText(format_swe(calc()$liters, 2))
  output$needed_pigment <- renderText(format_swe(calc()$pigment, 0))
  output$total_volume <- renderText(format_swe(total_paint_volume(), 2))
  output$volume_warning <- renderUI({
    comp <- volume_comparison()
    
    if(comp$significant) {
      icon_type <- if(comp$diff_L > 0) "exclamation-triangle" else "info-circle"
      color <- if(comp$diff_L > 0) "#fff" else "#fff"
      border <- if(comp$diff_L > 0) "#ddd" else "#ddd"
      
      msg <- if(comp$diff_L > 0) {
        sprintf("Receptets faktiska volym (%s L) är %s%% större än målvolymen (%s L). Detta beror på den höga oljeabsorptionen i pigmentblandningen.",
                format_swe(comp$actual, 2), 
                format_swe(comp$diff_pct, 0), 
                format_swe(comp$target, 2))
      } else {
        sprintf("Receptets faktiska volym (%s L) är %s%% mindre än målvolymen (%s L). Detta beror på låg oljeabsorption i pigmentblandningen.",
                format_swe(comp$actual, 2), 
                format_swe(comp$diff_pct, 0), 
                format_swe(comp$target, 2))
      }
      
      tags$div(
        class = "alert",
        style = sprintf("margin-top: 10px; padding: 10px; background-color: %s; border: 1px solid %s; border-radius: 4px;", 
                        color, border),
        icon(icon_type),
        " ", msg
      )
    }
  })
  
  recipe_df <- reactive({
    r <- final_recipe()
    df <- data.frame(Ingrediens = "Kallpressad kokt linolja", Gram = r$oil, stringsAsFactors = FALSE)
    if(r$zn > 0.1) df <- rbind(df, c("Zinkvitt PW4 (#44100)", r$zn))
    if(r$ti > 0.1) df <- rbind(df, c("Titanvitt Rutile PW6 (#44400)", r$ti))
    for(id in names(r$color))
      df <- rbind(df, c(paste0(km[[id]]$name," (#",id,")"), r$color[id]))
    df
  })
  
  # Calculate actual total volume of finished paint (pigment + oil)
  total_paint_volume <- reactive({
    recipe <- final_recipe()
    
    # Total pigment weight (grams)
    pigment_total_g <- recipe$zn + recipe$ti + sum(recipe$color)
    oil_g <- recipe$oil
    
    # Convert to volumes using densities
    # Average pigment density: ~4.0 g/cm³ (varies by type: 3.5-5.5 g/cm³)
    # Linseed oil density: ~0.92 g/cm³
    pigment_volume_L <- pigment_total_g / 4000  # Conservative estimate
    oil_volume_L <- oil_g / 920
    
    # Note: Total volume is less than sum of parts due to oil filling pigment voids
    # Empirical reduction factor ~0.85 for oil-pigment packing
    total_L <- (pigment_volume_L + oil_volume_L) * 0.85
    
    round(total_L, 2)
  })
  
  # Calculate volume difference and provide warning if significant
  volume_comparison <- reactive({
    target <- calc()$liters
    actual <- total_paint_volume()
    diff_L <- actual - target
    diff_pct <- abs(diff_L) / target * 100
    
    list(
      target = target,
      actual = actual,
      diff_L = round(diff_L, 2),
      diff_pct = round(diff_pct, 1),
      significant = diff_pct > 15
    )
  })
  
  final_recipe <- reactive({
    total_g <- calc()$pigment
    m <- mix()
    zinc_ratio <- values$zinc_ratio/100
    zn_g <- ti_g <- 0
    color_g <- numeric()
    for(i in seq_along(m$ids)){
      id <- m$ids[i]
      weight_g <- total_g * (m$pct[i]/m$total)
      if(id=="vitbas"){
        zn_g <- zn_g + weight_g * zinc_ratio
        ti_g <- ti_g + weight_g * (1-zinc_ratio)
      } else color_g[id] <- weight_g
    }
    base_oil <- zn_g*0.20 + ti_g*0.15
    for(id in names(color_g)) base_oil <- base_oil + color_g[id]*km[[id]]$oil/100
    final_oil <- round(base_oil*values$extra_oil,1)
    list(zn=round(zn_g,1), ti=round(ti_g,1), color=round(color_g,1), oil=final_oil, hex=final_hex())
  })
  
  output$final_recipe <- renderTable({
    df <- recipe_df()
    # Format the Gram column with Swedish decimals
    df$Gram <- sapply(df$Gram, function(x) format_swe(parse_numeric(x), 1))
    df
  }, striped=TRUE, bordered=F, width="100%", align="lr", sanitize.text.function = function(x) x)
  output$final_preview <- renderUI(tags$div(class="preview", style=paste0("background:", final_hex())))
  
  output$download_txt <- downloadHandler(
    filename = function() paste0("fargrecept_", Sys.Date(), ".txt"),
    content = function(file) {
      df <- recipe_df()
      txt <- paste0("FÄRGLABBET PRO – RECEPT ", Sys.Date(), "\n\n",
                    "Färgkod: ", final_hex(), "\n",
                    "Yta: ", format_swe(values$area, 0), " m²\n\n")
      for(i in 1:nrow(df)) {
        # Format gram values with Swedish decimals
        gram_val <- format_swe(parse_numeric(df[i,2]), 1)
        txt <- paste0(txt, df[i,1], ": ", gram_val, " g\n")
      }
      writeLines(txt, file, useBytes = TRUE)
    }
  )
  
  observeEvent(input$copy_clip, {
    df <- recipe_df()
    txt <- paste0("Färgkod: ", final_hex(), "\nYta: ", format_swe(values$area, 0), " m²\n\n")
    for(i in 1:nrow(df)) {
      gram_val <- format_swe(parse_numeric(df[i,2]), 1)
      txt <- paste0(txt, df[i,1], ": ", gram_val, " g\n")
    }
    session$sendCustomMessage("copyToClipboard", txt)
    showNotification("Recept kopierat till klippbord!", type = "message")
  })
  
  observeEvent(input$restart, {
    final_hex("#FFFFFF")
    hide("step3"); hide("step2"); show("step1")
  })
}

shinyApp(ui, server)