# Pigment Database
# Extended with RAÄ Kulturkulör pigments
# K and S values estimated based on pigment type and characteristics
# Oil absorption values from Kremer datablad and industry standards

km <- list(
  # BASE WHITES
  "vitbas" = list(name = "Vitbas (K-M-kompenserad titan/zink-blandning)", oil = 17, K = 0.00, S = 2.20, density = 4.2, rgb = c(245, 245, 245)),
  "44100" = list(name = "Zinkvitt PW4", oil = 20, K = 0.00, S = 1.66, density = 5.6, rgb = c(248, 248, 248)),
  "44400" = list(name = "Titanvitt Rutile PW6", oil = 15, K = 0.00, S = 2.55, density = 4.2, rgb = c(252, 252, 250)),
  
  # === FILLERS ===
  "599930" = list(name = "Kiselgur (diatoméjord)", oil = 70, K = 0.00, S = 0.05, density = 2.2, rgb = c(250, 248, 245)),
  "58000"  = list(name = "Krita från Champagne", oil = 12, K = 0.00, S = 0.10, density = 2.7, rgb = c(255, 255, 255)),
  "58010"  = list(name = "Krita från Ruegen", oil = 14, K = 0.00, S = 0.12, density = 2.7, rgb = c(248, 248, 246)),
  "58162"  = list(name = "Stenkrita vit", oil = 10, K = 0.00, S = 0.15, density = 2.7, rgb = c(255, 255, 255)),
  "58900"  = list(name = "Bentonit", oil = 180, K = 0.00, S = 0.08, density = 2.5, rgb = c(235, 232, 220)),
  "58250"  = list(name = "Kaolin gulaktig", oil = 45, K = 0.00, S = 0.20, density = 2.6, rgb = c(245, 242, 230)),
  
  # GREENS
  "40400" = list(name = "Viridian PG18", oil = 40, K = 1.20, S = 1.50, density = 3.5, rgb = c(30, 120, 80)),
  "41700" = list(name = "Malakit naturlig", oil = 45, K = 0.90, S = 0.80, density = 4.0, rgb = c(70, 160, 100)),
  "11100" = list(name = "Phthalogrön PG7", oil = 50, K = 1.50, S = 1.40, density = 2.0, rgb = c(0, 100, 50)),
  "KG83" = list(name = "Kromoxidgrönt nr GN 83", oil = 18, K = 1.15, S = 1.75, density = 5.2, rgb = c(74, 117, 82)),
  "ZG65" = list(name = "Zinkgrönt nr 65", oil = 19, K = 1.00, S = 1.60, density = 4.8, rgb = c(110, 145, 105)),
  "40850" = list(name = "Grön jord Böhmen", oil = 35, K = 0.60, S = 0.55, density = 3.2, rgb = c(90, 120, 70)),
  "40860" = list(name = "Grön jord Verona", oil = 35, K = 0.65, S = 0.60, density = 3.2, rgb = c(100, 130, 80)),
  "GU30" = list(name = "Grön umbra nr 30", oil = 50, K = 0.85, S = 0.48, density = 3.5, rgb = c(95, 100, 70)),
  
  # BLACKS
  "44450" = list(name = "Svartoxid PBk11", oil = 15, K = 2.40, S = 1.10, density = 5.21, rgb = c(28, 38, 38)),
  "J318" = list(name = "Järnoxidsvart nr 318", oil = 16, K = 2.35, S = 1.08, density = 5.1, rgb = c(35, 35, 38)),
  "BS98" = list(name = "Bensvart nr 98", oil = 50, K = 2.60, S = 0.95, density = 2.0, rgb = c(28, 28, 32)),
  "47501" = list(name = "Mangansvart", oil = 22, K = 2.50, S = 1.15, density = 4.8, rgb = c(32, 32, 32)),
  "47400" = list(name = "Spinel-svart", oil = 33, K = 2.80, S = 1.25, density = 4.5, rgb = c(20, 20, 20)),
  
  # BLUES
  "11670" = list(name = "Phthaloblå PB15:3", oil = 45, K = 1.80, S = 1.20, density = 2.0, rgb = c(0, 70, 130)),
  "UB88" = list(name = "Ultramarinblått nr 88", oil = 42, K = 1.65, S = 0.88, density = 2.4, rgb = c(45, 60, 130)),
  "KB28" = list(name = "Koboltblått nr 28", oil = 35, K = 1.40, S = 0.92, density = 4.0, rgb = c(70, 95, 155)),
  
  # EARTH COLORS - TERRA & POZZUOLI
  "40820" = list(name = "Terra di Pozzuoli", oil = 40, K = 0.70, S = 0.55, density = 3.3, rgb = c(180, 80, 60)),
  "40800" = list(name = "Terra di Siena natur", oil = 40, K = 0.60, S = 0.50, density = 3.3, rgb = c(170, 110, 70)),
  "40830" = list(name = "Terra di Ercolano", oil = 40, K = 0.75, S = 0.55, density = 3.3, rgb = c(175, 85, 65)),
  "BT44" = list(name = "Bränd terra nr 44", oil = 38, K = 0.78, S = 0.52, density = 3.4, rgb = c(170, 110, 70)),
  "OT46" = list(name = "Obränd terra nr 46", oil = 38, K = 0.62, S = 0.48, density = 3.3, rgb = c(180, 130, 80)),
  
  # YELLOWS & OCHRES
  "44082" = list(name = "Gul ockra ljus", oil = 20, K = 0.48, S = 0.38, density = 3.5, rgb = c(210, 180, 120)),
  "44086" = list(name = "Gul ockra mörk", oil = 20, K = 0.55, S = 0.45, density = 3.5, rgb = c(160, 120, 70)),
  "44150" = list(name = "Naples Yellow light", oil = 35, K = 0.40, S = 0.70, density = 6.0, rgb = c(240, 220, 130)),
  "44160" = list(name = "Naples Yellow dark", oil = 35, K = 0.50, S = 0.65, density = 6.0, rgb = c(220, 190, 100)),
  "J920" = list(name = "Järnoxidgult nr 920", oil = 22, K = 0.52, S = 0.42, density = 4.0, rgb = c(195, 165, 85)),
  "LO92" = list(name = "Ljusockra nr 92", oil = 21, K = 0.46, S = 0.40, density = 3.5, rgb = c(210, 185, 135)),
  "GO94" = list(name = "Guldockra nr 94", oil = 23, K = 0.58, S = 0.46, density = 3.6, rgb = c(185, 155, 90)),
  "GO94_GU30" = list(name = "50% Guldockra + 50% Grön umbra", oil = 40, K = 0.72, S = 0.47, density = 3.5, rgb = c(135, 130, 85)),
  
  # SIENNAS & UMBERS
  "44650" = list(name = "Raw Sienna Italien", oil = 45, K = 0.55, S = 0.45, density = 3.3, rgb = c(180, 130, 70)),
  "44620" = list(name = "Burnt Sienna Italien", oil = 50, K = 0.75, S = 0.50, density = 3.5, rgb = c(160, 82, 45)),
  "OU103" = list(name = "Obränd umbra nr 103", oil = 52, K = 0.92, S = 0.46, density = 3.4, rgb = c(115, 95, 80)),
  "BU100" = list(name = "Bränd umbra nr 100", oil = 56, K = 1.12, S = 0.52, density = 3.5, rgb = c(90, 60, 45)),
  "BRU39" = list(name = "Brun umbra nr 39", oil = 54, K = 1.05, S = 0.48, density = 3.4, rgb = c(105, 85, 70)),
  "GRAU36" = list(name = "Grå umbra nr 36", oil = 48, K = 1.20, S = 0.55, density = 3.5, rgb = c(100, 95, 90)),
  
  # REDS & ORANGES - IRON OXIDES
  "44300" = list(name = "Transparent brunoxid", oil = 50, K = 0.80, S = 0.22, density = 5.0, rgb = c(139, 69, 19)),
  "44200" = list(name = "Röd järnoxid transparent", oil = 47, K = 0.90, S = 0.12, density = 5.2, rgb = c(178, 34, 34)),
  "44210" = list(name = "Röd järnoxid ljus", oil = 47, K = 0.80, S = 0.25, density = 5.1, rgb = c(200, 70, 60)),
  "44220" = list(name = "Röd järnoxid mörk", oil = 47, K = 1.00, S = 0.35, density = 5.2, rgb = c(160, 35, 35)),
  "44510" = list(name = "Orange järnoxid", oil = 47, K = 0.55, S = 0.85, density = 4.8, rgb = c(232, 97, 0)),
  "J225" = list(name = "Järnoxidrött nr 225", oil = 48, K = 0.95, S = 0.32, density = 5.1, rgb = c(142, 52, 52)),
  "J180M" = list(name = "Järnoxidrött nr 180M Caput Mortuum", oil = 48, K = 1.15, S = 0.28, density = 5.2, rgb = c(105, 45, 55)),
  "J120N" = list(name = "Järnoxidrött nr 120N", oil = 47, K = 0.85, S = 0.30, density = 5.0, rgb = c(155, 65, 60)),
  "ER48A" = list(name = "Engelskt rött nr 48A", oil = 30, K = 0.75, S = 0.40, density = 4.9, rgb = c(175, 80, 70)),
  
  # BROWNS - IRON OXIDES
  "J663" = list(name = "Järnoxidbrunt nr 663", oil = 50, K = 0.88, S = 0.38, density = 5.0, rgb = c(120, 80, 60)),
  "J686" = list(name = "Järnoxidbrunt nr 686", oil = 52, K = 0.92, S = 0.35, density = 5.1, rgb = c(105, 70, 55)),
  "48330" = list(name = "Järnmanganbrunt 645 T", oil = 50, K = 0.90, S = 0.40, density = 4.8, rgb = c(95, 65, 45))
  
)

# RAÄ KULTURKULÖR PIGMENTS
# Updated to include all RAÄ pigments with harmonized keys and NCS-based RGB values
raa_pigments <- c(
  # Base whites (always included)
  "vitbas", #"44100", "44400",
  
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
