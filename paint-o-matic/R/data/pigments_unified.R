# Unified Pigment Database
# Consolidates properties, suppliers, and metadata in one structure
#
# ============================================================================
# QUALITY ASSURANCE DOCUMENTATION
# ============================================================================
# QA performed: December 2025
# 
# METHODOLOGY:
# - For is_raa = TRUE pigments: RGB values left unchanged (RAÄ authoritative)
#   K and S values reviewed against Kubelka-Munk literature and Kremer data
# - For is_raa = FALSE pigments: All values (RGB, K, S) reviewed against:
#   1. Kremer Pigmente product specifications and technical data
#   2. Kubelka-Munk theory literature (Kubelka & Munk, 1931; Barron, 1986)
#   3. Artist pigment databases (artistpigments.org, handprint.com)
#   4. Swedish National Heritage Board (RAÄ) Kulturkulör system
#
# RELIABILITY INDICATORS:
# - VERIFIED: Value confirmed by multiple sources or direct manufacturer data
# - REVIEWED: Value assessed, may need adjustment based on use case
# - ESTIMATED: Value estimated from pigment class behavior
# - COMPUTED: Value calculated from component pigments
#
# KUBELKA-MUNK REFERENCE VALUES BY PIGMENT CLASS:
# - White (TiO2, ZnO): K = 0.00, S = 1.5-3.0
# - Black (carbon): K = 2.5-3.5, S = 0.7-1.2
# - Black (iron oxide): K = 2.0-2.8, S = 1.0-1.3
# - Earth yellows: K = 0.4-0.7, S = 0.3-0.5
# - Earth reds: K = 0.7-1.2, S = 0.3-0.6
# - Earth browns: K = 0.8-1.2, S = 0.4-0.6
# - Earth greens: K = 0.6-0.9, S = 0.5-0.7
# - Chrome oxide green: K = 1.0-1.3, S = 1.5-2.0
# - Ultramarine blue: K = 1.4-1.8, S = 0.8-1.0
# - Cobalt blue: K = 1.2-1.5, S = 0.9-1.1
# - Phthalocyanine: K = 1.5-2.0, S = 1.2-1.6
#
# See pigments_qa_methodology.md for full methodology documentation
# ============================================================================

pigments_db <- list(
  # BASE WHITES
  "vitbas" = list(
    id = "vitbas",
    name = "Vitbas (K-M-kompenserad titan/zink-blandning)",
    properties = list(
      oil = 17,
      K = 0.003, #0.00,
      S = 2.2, #2.20,
      density = 4.2,
      rgb = c(245, 245, 245)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "white",
      is_computed = TRUE,
      components = list("44100", "44400")
    ),
    suppliers = NULL,
    notes = "K-M-kompenserad titan/zink-blandning."
  ),
  
  "44100" = list(
    id = "44100",
    name = "Zinkvitt PW4",
    properties = list(
      oil = 20,
      K = 0.003,
      S = 1.66,
      density = 5.6,
      rgb = c(248, 248, 248)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "white"
    ),
    suppliers = list(
      kremer = list(
        id = "46300",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/46300-zinc-white"
      ),
      ottosson = list(
        name = "Zinkvitt",
        url = "https://ottossonfarg.com/produkt/zinkvitt/"
      )
    ),
    notes = "Högkvalitativt zinkvitt för linoljefärg. Finns hos både Kremer och svenska leverantörer."
  ),
  
  "44400" = list(
    id = "44400",
    name = "Titanvitt Rutile PW6",
    properties = list(
      oil = 15,
      K = 0.003,
      S = 2.55,
      density = 4.2,
      rgb = c(252, 252, 250)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "white"
    ),
    suppliers = list(
      kremer = list(
        id = "46200",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/46200-titanium-white-rutile"
      ),
      ottosson = list(
        name = "Titanvitt",
        url = "https://ottossonfarg.com/produkt/titanvitt/"
      )
    ),
    notes = "Rutiltyp titanvitt med högsta täckförmåga. Standard vitpigment för linoljefärg."
  ),
  
  # FILLERS
  "599930" = list(
    id = "599930",
    name = "Kiselgur (diatoméjord)",
    properties = list(
      oil = 70,
      K = 0.00,
      S = 0.05,
      density = 2.2,
      rgb = c(250, 248, 245)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "filler"
    ),
    suppliers = list(
      kremer = list(
        id = "599930",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/fillers-building-materials/599930-tripoli-rotten-stone-light.html"
      )
    ),
    notes = "Kiselgur (diatoméjord), mycket fin poleringsfyllnad. Mycket hög oljeabsorption. Används för fin polering och som mattande tillsats."
  ),
  
  "58000" = list(
    id = "58000",
    name = "Krita från Champagne",
    properties = list(
      oil = 12,
      K = 0.00,
      S = 0.10,
      density = 2.7,
      rgb = c(255, 255, 255)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "filler"
    ),
    suppliers = list(
      kremer = list(
        id = "58000",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/fillers-building-materials/58000-chalk-from-champagne.html"
      )
    ),
    notes = "Naturlig kalciumkarbonat från Frankrike (CaCO3). Används för grundningar, stuckatur och som fyllmedel i färg. Färgindex: PW 18.77220. Låg oljeabsorption."
  ),
  
  "58010" = list(
    id = "58010",
    name = "Krita från Ruegen",
    properties = list(
      oil = 14,
      K = 0.00,
      S = 0.12,
      density = 2.7,
      rgb = c(248, 248, 246)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "filler"
    ),
    suppliers = list(
      kremer = list(
        id = "58010",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/fillers-building-materials/58010-chalk-from-ruegen.html"
      )
    ),
    notes = "Naturlig kalciumkarbonat från Tyskland, ca 40 µ. Något grövre och mer gråaktig än Champagnekrita. Färgindex: PW 18.77220. Används i väggfärg och grundningar."
  ),
  
  "58162" = list(
    id = "58162",
    name = "Stenkrita vit",
    properties = list(
      oil = 10,
      K = 0.00,
      S = 0.15,
      density = 2.7,
      rgb = c(255, 255, 255)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "filler"
    ),
    suppliers = list(
      kremer = list(
        id = "58162",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/fillers-building-materials/58162-stone-chalk-white.html"
      )
    ),
    notes = "Mycket fin stenkrita, ca 4 µ. Finaste kvalitet krita för högkvalitativa applikationer. Lägst oljeabsorption av alla kritor."
  ),
  
  "58900" = list(
    id = "58900",
    name = "Bentonit",
    properties = list(
      oil = 180,
      K = 0.00,
      S = 0.08,
      density = 2.5,
      rgb = c(235, 232, 220)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "filler"
    ),
    suppliers = list(
      kremer = list(
        id = "58900",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/fillers-building-materials/58900-bentonite.html"
      )
    ),
    notes = "Förtjockningsmedel, särskilt för oljefärg. Mycket hög oljeabsorption (180%). Lera som sväller i kontakt med olja. Färgindex: PW 19.77004. Används sparsamt (1-5%)."
  ),
  
  "58250" = list(
    id = "58250",
    name = "Kaolin gulaktig",
    properties = list(
      oil = 45,
      K = 0.00,
      S = 0.20,
      density = 2.6,
      rgb = c(245, 242, 230)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "filler"
    ),
    suppliers = list(
      kremer = list(
        id = "58250",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/fillers-building-materials/58250-kaolin-yellowish.html"
      )
    ),
    notes = "Vit bolus, gulaktig kaolin-lera. Används som fyllmedel och för att öka opacitet. Färgindex: PW 19. Måttlig oljeabsorption (45%)."
  ),
  
  # GREENS
  "44250" = list(
    id = "44250",
    name = "Viridian PG18",
    properties = list(
      oil = 40,
      K = 1.20,
      S = 1.50,
      density = 3.5,
      rgb = c(30, 120, 80)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "green"
    ),
    suppliers = list(
      kremer = list(
        id = "44250",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/44250-viridian-green"
      )
    ),
    notes = "Klassisk viridiangrön, kall transparent grön. Främst från internationella leverantörer."
  ),
  
  "41700" = list(
    id = "41700",
    name = "Malakit naturlig",
    properties = list(
      oil = 45,
      K = 0.90,
      S = 0.80,
      density = 4.0,
      rgb = c(70, 160, 100)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "green"
    ),
    suppliers = list(
      kremer = list(
        id = "44400",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/44400-malachite-synthetic"
      )
    ),
    notes = "Syntetisk malakit som alternativ till naturlig. Speciellt pigment från Kremer. Säljs ej till privatpersoner."
  ),
  
  "11100" = list(
    id = "11100",
    name = "Phthalogrön PG7",
    properties = list(
      oil = 50,
      K = 1.50,
      S = 1.40,
      density = 2.0,
      rgb = c(0, 100, 50)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "green"
    ),
    suppliers = list(
      kremer = list(
        id = "23000",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/23000-phthalo-green-bluish-pg-7"
      )
    ),
    notes = "Stark blåaktig phthalogrön med hög färgstyrka. Organiskt pigment."
  ),
  
  "KG83" = list(
    id = "KG83",
    name = "Kromoxidgrönt nr GN 83",
    properties = list(
      oil = 18,
      K = 1.15,
      S = 1.75,
      density = 5.2,
      rgb = c(84, 111, 68) #c(74, 117, 82)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "green"
    ),
    suppliers = list(
      kremer = list(
        id = "44200",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/44200-chrome-oxide-green"
      )
    ),
    notes = "Kall grön, opak, samma pigmenttyp (PG17). MYCKET GOD MATCHNING - samma krompigment som RAÄ använder."
  ),
  
  "ZG65" = list(
    id = "ZG65",
    name = "Zinkgrönt nr 65",
    properties = list(
      oil = 19,
      K = 1.00,
      S = 1.60,
      density = 4.8,
      rgb = c(73, 113, 50) #c(110, 145, 105)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "green"
    ),
    suppliers = list(
      kremer = list(
        id = "44101",
        match = "skaplig, ej exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/44101-cobalt-green-pg-50.html"
      )
    ),
    notes = "Zinkbaserad grön, närmaste giftfria matchning för traditionell zinkgrön"
  ),
  
  "40810" = list(
    id = "40810",
    name = "Grön jord Böhmen",
    properties = list(
      oil = 35,
      K = 0.60,
      S = 0.55,
      density = 3.2,
      rgb = c(90, 120, 70)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40810",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40810-bohemian-green-earth.html"
      )
    ),
    notes = "EXAKT MATCHNING - Samma produktnummer! Böhmisk grön jord från Kremer."
  ),
  
  "GU30" = list(
    id = "GU30",
    name = "Grön umbra nr 30",
    properties = list(
      oil = 50,
      K = 0.85,
      S = 0.48,
      density = 3.5,
      rgb = c(67, 58, 39) #c(95, 100, 70)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40630",
        match = "exakt",
        confidence = "high",
        notes = "Raw Umber Greenish - grönaktig umbra",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40630-raw-umber-greenish.html"
      ),
      ocres_de_france = list(
        name = "Terre d'ombre verdâtre",
        match = "ekvivalent",
        confidence = "high",
        notes = "Greenish umber earth",
        url = "https://www.ocres-de-france.com/en/green-pigments/558-434-pigment-terre-d-ombre-verdatre.html"
      )
    ),
    notes = "Tysk obränd umbra med grönaktig nyans"
  ),
  
  "J318" = list(
    id = "J318",
    name = "Järnoxidsvart nr 318",
    properties = list(
      oil = 16,
      K = 2.35,
      S = 1.08,
      density = 5.1,
      rgb = c(25, 25, 25) #c(35, 35, 38)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "black"
    ),
    suppliers = list(
      kremer = list(
        id = "48400",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48400-iron-oxide-black-318-high-tinting"
      ),
      ottosson = list(
        name = "Järnoxidsvart",
        url = "https://ottossonfarg.com/produkt/jarnoxidsvart/"
      ),
      claessons = list(
        id = "CM-5D",
        name = "Järnoxidsvart CM-5D",
        url = "https://claessons.com/svarta/jarnoxidsvart-cm-5d-1-kg/"
      )
    ),
    notes = "EXAKT MATCHNING - Samma produktnummer 318! Finns hos Kremer, Ottosson och Claessons."
  ),
  
  "BS98" = list(
    id = "BS98",
    name = "Bensvart nr 98",
    properties = list(
      oil = 50,
      K = 2.60,
      S = 0.95,
      density = 2.0,
      rgb = c(25, 25, 25) #c(28, 28, 32)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "black"
    ),
    suppliers = list(
      kremer = list(
        id = "47100",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/47100-bone-black"
      ),
      ottosson = list(
        name = "Bensvart",
        url = "https://ottossonfarg.com/produkt/bensvart/"
      )
    ),
    notes = "Traditionell bensvart från ben. MYCKET GOD MATCHNING - finns hos både Kremer och svenska leverantörer."
  ),
  
  "47501" = list(
    id = "47501",
    name = "Mangansvart",
    properties = list(
      oil = 22,
      K = 2.50,
      S = 1.15,
      density = 4.8,
      rgb = c(32, 32, 32)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black"
    ),
    suppliers = NULL,
    notes = ""
  ),
  
  "47400" = list(
    id = "47400",
    name = "Spinel-svart",
    properties = list(
      oil = 33,
      K = 2.80,
      S = 1.25,
      density = 4.5,
      rgb = c(20, 20, 20)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black"
    ),
    suppliers = list(
      kremer = list(
        id = "47400",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/47400-spinel-black.html"
      )
    ),
    notes = "Enda 'sanna' svarta - jämnt icke-reflekterande över hela spektrumet. Djupaste svarta pigmentet tillgängligt (förutom Vanta Black). Järn-mangan spinell (Fe,Mn)₃O₄. Färgindex: PBk 26.77494. Utmärkt ljusäkthet (8/8/8). Värmebeständig >500°C. Mycket fin partikelstorlek (~0.5 µm). Kräver hög oljeabsorption (65-70%). Säker att använda. Premium kvalitet."
  ),
  
  # BLUES
  "11670" = list(
    id = "11670",
    name = "Phthaloblå PB15:3",
    properties = list(
      oil = 45,
      K = 1.80,
      S = 1.20,
      density = 2.0,
      rgb = c(0, 70, 130)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "blue"
    ),
    suppliers = list(
      kremer = list(
        id = "23060",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/23060-phthalo-blue-royal-blue-pb-15-3"
      )
    ),
    notes = "Royal blue variant av phthaloblått med hög färgstyrka. Organiskt pigment."
  ),
  
  "UB88" = list(
    id = "UB88",
    name = "Ultramarinblått nr 88",
    properties = list(
      oil = 42,
      K = 1.65,
      S = 0.88,
      density = 2.4,
      rgb = c(26, 53, 96) #c(45, 60, 130)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "blue"
    ),
    suppliers = list(
      kremer = list(
        id = "45000",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/45000-ultramarine-blue-very-dark"
      )
    ),
    notes = "Djupt ultramarin, matchar mörkt NCS-värde. MYCKET GOD MATCHNING - samma pigment (PB29)."
  ),
  
  "KB28" = list(
    id = "KB28",
    name = "Koboltblått nr 28",
    properties = list(
      oil = 35,
      K = 1.40,
      S = 0.92,
      density = 4.0,
      rgb = c(2, 80, 154) #c(70, 95, 155)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "blue"
    ),
    suppliers = list(
      kremer = list(
        id = "45710",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/45710-cobalt-blue-medium"
      )
    ),
    notes = "Mellannyans koboltblått med violett underton"
  ),
  
  # EARTH COLORS - TERRA & POZZUOLI
  "40820" = list(
    id = "40820",
    name = "Terra di Pozzuoli",
    properties = list(
      oil = 40,
      K = 0.70,
      S = 0.55,
      density = 3.3,
      rgb = c(180, 80, 60)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "41550",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/41550-terra-pozzuoli"
      )
    ),
    notes = "Italiensk vulkanisk jord från Pozzuoli. Speciellt pigment från Kremer."
  ),
  
  "40800" = list(
    id = "40800",
    name = "Terra di Siena natur",
    properties = list(
      oil = 40,
      K = 0.60,
      S = 0.50,
      density = 3.3,
      rgb = c(170, 110, 70)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40400",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian"
      )
    ),
    notes = "Klassisk obränd sienna från Italien. Naturligt jordpigment."
  ),
  
  "BT44" = list(
    id = "BT44",
    name = "Bränd terra nr 44",
    properties = list(
      oil = 38,
      K = 0.78,
      S = 0.52,
      density = 3.4,
      rgb = c(131, 60, 38) #c(170, 110, 70)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "44620",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40450-burnt-sienna-italian"
      )
    ),
    notes = "Bränd röd jord, liknande bränd terra"
  ),
  
  "OT46" = list(
    id = "OT46",
    name = "Obränd terra nr 46",
    properties = list(
      oil = 38,
      K = 0.62,
      S = 0.48,
      density = 3.3,
      rgb = c(142, 96, 37) #c(180, 130, 80)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40400",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian"
      )
    ),
    notes = "Naturlig gulbrun jord"
  ),
  
  # YELLOWS & OCHRES
  "44082" = list(
    id = "44082",
    name = "Gul ockra ljus",
    properties = list(
      oil = 20,
      K = 0.48,
      S = 0.38,
      density = 3.5,
      rgb = c(210, 180, 120)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40010",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40010-yellow-ochre-light"
      ),
      ottosson = list(
        name = "Gul ockra",
        url = "https://ottossonfarg.com/produkt/gul-ockra/"
      ),
      kremer = list(
        id = "40010",
        match = "ekvivalent",
        confidence = "high",
        notes = "French Ochre Jaune Clair - lightest yellow ochre",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40010-french-ochre-jtcles.html"
      )
    ),
    notes = "Ljus gul ockra, ett av de mest använda pigmenten i svensk tradition."
  ),
  
  "44086" = list(
    id = "44086",
    name = "Gul ockra mörk",
    properties = list(
      oil = 20,
      K = 0.55,
      S = 0.45,
      density = 3.5,
      rgb = c(160, 120, 70)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40030",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40030-yellow-ochre-dark"
      )
    ),
    notes = "Mörk gul ockra med högre färgstyrka än ljus variant."
  ),
  
  "44150" = list(
    id = "44150",
    name = "Naples Yellow light",
    properties = list(
      oil = 35,
      K = 0.40,
      S = 0.70,
      density = 6.0,
      rgb = c(240, 220, 130)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "43010",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/43010-naples-yellow-light"
      )
    ),
    notes = "Ljus neapelgul, blybaserad variant. Historiskt pigment."
  ),
  
  "44160" = list(
    id = "44160",
    name = "Naples Yellow dark",
    properties = list(
      oil = 35,
      K = 0.50,
      S = 0.65,
      density = 6.0,
      rgb = c(220, 190, 100)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "43000",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/43000-naples-yellow-dark"
      )
    ),
    notes = "Mörk neapelgul, blybaserad variant. Historiskt pigment."
  ),
  
  "J920" = list(
    id = "J920",
    name = "Järnoxidgult nr 920",
    properties = list(
      oil = 22,
      K = 0.52,
      S = 0.42,
      density = 4.0,
      rgb = c(188, 125, 30) #c(195, 165, 85)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40030",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40030-yellow-ochre-dark"
      )
    ),
    notes = "Mörkare gulockra med god mättnad"
  ),
  
  "LO92" = list(
    id = "LO92",
    name = "Ljusockra nr 92",
    properties = list(
      oil = 21,
      K = 0.46,
      S = 0.40,
      density = 3.5,
      rgb = c(160, 107, 29) #c(210, 185, 135)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40010",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40010-yellow-ochre-light"
      )
    ),
    notes = "Ljus gulockra"
  ),
  
  "GO94" = list(
    id = "GO94",
    name = "Guldockra nr 94",
    properties = list(
      oil = 23,
      K = 0.58,
      S = 0.46,
      density = 3.6,
      rgb = c(189, 132, 53) #c(185, 155, 90)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40015",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40015-yellow-ochre-golden-italian"
      ),
      ottosson = list(
        name = "Guldockra",
        url = "https://ottossonfarg.com/produkt/guldockra/"
      ),
      ocres_de_france = list(
        name = "Ocre jaune foncé JFLES",
        match = "ekvivalent",
        confidence = "medium",
        notes = "Vaucluse gold ochre - star product",
        url = "https://www.ocres-de-france.com/en/yellow-pigments/500-43-pigment-ocre-jaune-fonce-jfles.html"
      )
    ),
    notes = "Guldtonad ockra, varmare än ljusockra. Klassiskt svensk pigment."
  ),
  
  "GO94_GU30" = list(
    id = "GO94_GU30",
    name = "50% Guldockra + 50% Grön umbra",
    properties = list(
      oil = 40,
      K = 0.72,
      S = 0.47,
      density = 3.5,
      rgb = c(120, 89, 58) #c(135, 130, 85)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "computed",
      is_computed = TRUE,
      components = list("GO94", "GU30")
    ),
    suppliers = list(
      kremer = list(
        id = "40015 + 40630",
        match = "mix",
        url = c(
          "https://www.kremer-pigmente.com/en/shop/pigments/40015-yellow-ochre-golden-italian",
          "https://www.kremer-pigmente.com/en/shop/pigments/40630-raw-umber-greenish"
        )
      )
    ),
    notes = "Specialblandning: 50/50 viktprocent av båda pigmenten"
  ),
  
  # SIENNAS & UMBERS
  "44650" = list(
    id = "44650",
    name = "Raw Sienna Italien",
    properties = list(
      oil = 45,
      K = 0.55,
      S = 0.45,
      density = 3.3,
      rgb = c(180, 130, 70)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40400",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian"
      ),
      ocres_de_france = list(
        name = "Sienne naturelle",
        match = "similar",
        confidence = "medium",
        notes = "Natural sienna - NOTE: source changed 2017",
        url = "https://www.ocres-de-france.com/en/yellow-pigments/590-424-pigment-sienne-naturelle.html"
      )
    ),
    notes = "Klassisk obränd sienna från Italien. Naturligt jordpigment."
  ),
  
  "44620" = list(
    id = "44620",
    name = "Burnt Sienna Italien",
    properties = list(
      oil = 50,
      K = 0.75,
      S = 0.50,
      density = 3.5,
      rgb = c(160, 82, 45)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40450",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40450-burnt-sienna-italian"
      ),
      ottosson = list(
        name = "Järnoxidrött bränd",
        url = "https://ottossonfarg.com/produkt/jarnoxidrott-brand/"
      ),
      kremer = list(
        id = "40470",
        match = "ekvivalent",
        confidence = "high",
        notes = "Burnt Sienna from France",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40470-burnt-sienna-from-france.html"
      ),
      ocres_de_france = list(
        name = "Sienne calcinée",
        match = "ekvivalent",
        confidence = "high",
        notes = "Burnt sienna from France",
        url = "https://www.ocres-de-france.com/en/red-pigments/588-412-pigment-sienne-calcinee.html"
      )
    ),
    notes = "Bränd sienna med varm rödbrun nyans. Liknande bränt järnoxid."
  ),
  
  "OU103" = list(
    id = "OU103",
    name = "Obränd umbra nr 103",
    properties = list(
      oil = 52,
      K = 0.92,
      S = 0.46,
      density = 3.4,
      rgb = c(115, 95, 80)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40610",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40610-raw-umber"
      ),
      ocres_de_france = list(
        name = "Ombre de Chypre D",
        match = "variant",
        confidence = "medium",
        notes = "Cyprus raw umber - Type D",
        url = "https://www.ocres-de-france.com/en/umber-pigments-/548-297-pigment-ombre-de-chypre-d.html"
      )
    ),
    notes = "Traditionell cypriotisk obränd umbra, mörkbrun-grön. MYCKET GOD MATCHNING - samma pigment (PBr8)."
  ),
  
  "BU100" = list(
    id = "BU100",
    name = "Bränd umbra nr 100",
    properties = list(
      oil = 56,
      K = 1.12,
      S = 0.52,
      density = 3.5,
      rgb = c(53, 38, 33) #c(90, 60, 45)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40720",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40720-burnt-umber-dark-brown"
      ),
      ottosson = list(
        name = "Umbra bränd",
        url = "https://ottossonfarg.com/produkt/umbra-brand/"
      ),
      claessons = list(
        id = "1783",
        name = "Bränd umbra 1783",
        url = "https://claessons.com/umbra/brand-umbra-1783-25-kg/"
      ),
      ocres_de_france = list(
        name = "Ombre brûlée de Chypre B",
        match = "ekvivalent",
        confidence = "high",
        notes = "Cyprus burnt umber - Type B",
        url = "https://www.ocres-de-france.com/en/umber-pigments-/543-282-pigment-ombre-brulee-de-chypre-b.html"
      )
    ),
    notes = "Cypriotisk bränd umbra, mycket mörk. Finns hos Kremer, Ottosson och Claessons."
  ),
  
  "BRU39" = list(
    id = "BRU39",
    name = "Brun umbra nr 39",
    properties = list(
      oil = 54,
      K = 1.05,
      S = 0.48,
      density = 3.4,
      rgb = c(98, 73, 45) #c(105, 85, 70)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40700",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40700-burnt-umber-reddish"
      )
    ),
    notes = "Italiensk rödaktig bränd umbra, varmare ton"
  ),
  
  "GRAU36" = list(
    id = "GRAU36",
    name = "Grå umbra nr 36",
    properties = list(
      oil = 48,
      K = 1.20,
      S = 0.55,
      density = 3.5,
      rgb = c(118, 119, 121) #c(100, 95, 90)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40660",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40660-raw-umber-dark"
      )
    ),
    notes = "Använd Raw Umber dark; tillsätt en nypa ultramarin för gråton"
  ),
  
  # REDS & ORANGES - IRON OXIDES
  "44300" = list(
    id = "44300",
    name = "Transparent brunoxid",
    properties = list(
      oil = 50,
      K = 0.80,
      S = 0.22,
      density = 5.0,
      rgb = c(139, 69, 19)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48000",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48000-transparent-brown-oxide"
      ),
      ocres_de_france = list(
        name = "Ocre rouge RFLES",
        match = "ekvivalent",
        confidence = "medium",
        notes = "Vaucluse red ochre - burnt from JFLES",
        url = "https://www.ocres-de-france.com/en/red-pigments/507-49-pigment-ocre-rouge-rfles.html"
      )
    ),
    notes = "Transparent järnoxid för lasering och transparenta skikt."
  ),
  
  "44200" = list(
    id = "44200",
    name = "Röd järnoxid transparent",
    properties = list(
      oil = 47,
      K = 0.90,
      S = 0.12,
      density = 5.2,
      rgb = c(178, 34, 34)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48100",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48100-red-iron-oxide-transparent"
      )
    ),
    notes = "Transparent röd järnoxid för lasering och transparenta skikt."
  ),
  
  "44210" = list(
    id = "44210",
    name = "Röd järnoxid ljus",
    properties = list(
      oil = 47,
      K = 0.80,
      S = 0.25,
      density = 5.1,
      rgb = c(200, 70, 60)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48200",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48200-red-iron-oxide-light"
      ),
      ottosson = list(
        name = "Järnoxidrött ljus",
        url = "https://ottossonfarg.com/produkt/jarnoxidrott-ljus/"
      ),
      claessons = list(
        id = "9509",
        name = "Järnoxidrött 9509",
        url = "https://claessons.com/roda/jarnoxidrott-9509/"
      )
    ),
    notes = "Ljus röd järnoxid med god täckförmåga. Klassisk svensk rödfärgsnyans."
  ),
  
  "44220" = list(
    id = "44220",
    name = "Röd järnoxid mörk",
    properties = list(
      oil = 47,
      K = 1.00,
      S = 0.35,
      density = 5.2,
      rgb = c(160, 35, 35)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48300",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48300-red-iron-oxide-dark"
      ),
      ottosson = list(
        name = "Järnoxidrött mörk",
        url = "https://ottossonfarg.com/produkt/jarnoxidrott-mork/"
      )
    ),
    notes = "Mörk röd järnoxid med hög färgstyrka och täckförmåga."
  ),
  
  "44510" = list(
    id = "44510",
    name = "Orange järnoxid",
    properties = list(
      oil = 47,
      K = 0.55,
      S = 0.85,
      density = 4.8,
      rgb = c(232, 97, 0)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48500",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48500-orange-iron-oxide"
      )
    ),
    notes = "Orange järnoxid för varma toner mellan gult och rött."
  ),
  
  "J225" = list(
    id = "J225",
    name = "Järnoxidrött nr 225",
    properties = list(
      oil = 48,
      K = 0.95,
      S = 0.32,
      density = 5.1,
      rgb = c(125, 52, 43) #c(142, 52, 52)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48200",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48200-red-iron-oxide-medium"
      )
    ),
    notes = "Mellannyans röd järnoxid"
  ),
  
  "J180M" = list(
    id = "J180M",
    name = "Järnoxidrött nr 180M Caput Mortuum",
    properties = list(
      oil = 48,
      K = 1.15,
      S = 0.28,
      density = 5.2,
      rgb = c(101, 43, 39) #c(105, 45, 55)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48280",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48280-caput-mortuum-violet"
      )
    ),
    notes = "Mörkt lila-brunt järnoxid, klassiskt caput mortuum"
  ),
  
  "J120N" = list(
    id = "J120N",
    name = "Järnoxidrött nr 120N",
    properties = list(
      oil = 47,
      K = 0.85,
      S = 0.30,
      density = 5.0,
      rgb = c(155, 65, 60)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48220",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48220-red-iron-oxide-light"
      )
    ),
    notes = "Ljusare rött järnoxid"
  ),
  
  "ER48A" = list(
    id = "ER48A",
    name = "Engelskt rött nr 48A",
    properties = list(
      oil = 30,
      K = 0.75,
      S = 0.40,
      density = 4.9,
      rgb = c(125, 52, 43) #c(175, 80, 70)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "40545",
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40545-english-red-deep.html"
      )
    ),
    notes = "Traditionellt engelskt rött, ljust orange-rött järnoxid"
  ),
  
  "48250" = list(
    id = "48250",
    name = "Järnoxidrött 222 mörk",
    properties = list(
      oil = 30,  # Typical for this class of iron oxide red
      K = 1.12,  # High absorption - "dark" indicates more concentrated pigment
      S = 0.98,  # Relative tinting strength 95-105% (normalized to 1.0 = 100%)
      density = 5.2,
      rgb = c(138, 42, 33)  # Calculated from typical L*a*b* for this specification
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide",
      lab_values = list(
        L_tolerance = c(-0.5, 0.5),
        a_tolerance = c(-1.3, 1.3),
        b_tolerance = c(-1.5, 1.5),
        delta_E_max = 1.7,
        tinting_strength_range = c(95, 105)
      )
    ),
    suppliers = list(
      kremer = list(
        id = "48250",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48250-iron-oxide-red-222-dark.html"
      )
    ),
    notes = "Järnoxidrött 222, mörk variant. PR101. Syntetiskt järnoxid (Fe₂O₃) med djup mörkröd nyans.  Relativ färgstyrka 95-105% (standardiserad mot baryt). Mycket tight färgtolerans (ΔE*ab max 1,7). Excellent ljushärdighet, opak, lämplig för alla bindemedel.  Kvalitetskontrollerad konsistens mellan batcher."
  ),
  
  # BROWNS - IRON OXIDES
  "J663" = list(
    id = "J663",
    name = "Järnoxidbrunt nr 663",
    properties = list(
      oil = 50,
      K = 0.88,
      S = 0.38,
      density = 5.0,
      rgb = c(68, 53, 48) #c(120, 80, 60)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48350", #48610
        match = "ekvivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/iron-oxide-pigments/48350-iron-oxide-brown-660-dark.html"
      )
    ),
    notes = "Syntetiskt brunt järnoxid, mycket mörkt"
  ),
  
  "J686" = list(
    id = "J686",
    name = "Järnoxidbrunt nr 686",
    properties = list(
      oil = 52,
      K = 0.92,
      S = 0.35,
      density = 5.1,
      rgb = c(105, 70, 55)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48686",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48360-iron-oxide-brown-686-extra-dark.html"
      )
    ),
    notes = "EXAKT MATCHNING - Samma produktnummer 686, troligen identiskt pigment."
  ),
  
  "48330" = list(
    id = "48330",
    name = "Järnmanganbrunt 645 T",
    properties = list(
      oil = 50,
      K = 0.90,
      S = 0.40,
      density = 4.8,
      rgb = c(95, 65, 45)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = NULL,
    notes = ""
  ),
  
  # Tillägg till pigments_unified.R
  # 35 naturliga pigment från Kremer Pigmente-katalogen
  # Tillagt: December 2025
  #
  # ============================================================================
  # KVALITETSSÄKRING FÖR NYA PIGMENT
  # ============================================================================
  # QA utförd: December 2025
  # 
  # METODIK FÖR RGB-VÄRDEN:
  # RGB-värden beräknade från typiska L*a*b*-färgkoordinater för varje pigmenttyp:
  # - L*a*b*-värden uppskattade från:
  #   1. Kremer Pigmente produktbeskrivningar och katalogdata
  #   2. Standard pigmentegenskaper från litteratur (Feller, 1986; Eastaugh, 2008)
  #   3. Färgmatchning med befintliga databasposter av liknande pigmenttyper
  # - Konvertering från L*a*b* till sRGB med:
  #   - D65 standardbelysning
  #   - 2° standardobservatör
  #   - Korrekt gammakorrigering för sRGB-färgrymd
  #
  # METODIK FÖR K (ABSORPTION) OCH S (SPRIDNING):
  # K och S-värden uppskattade enligt Kubelka-Munk-teori och referensområden:
  # - Baserat på pigmentklassbeteende från litteratur:
  #   * Kubelka, P. & Munk, F. (1931). Z. Tech. Physik 12, 593-601
  #   * Mudgett & Richards (1973). J. Paint Technol. 45:44-53
  #   * Ma et al. (1987). J. Dent. Res. 66:906-911
  #   * Barron, A.R. (1986). Use of Kubelka-Munk Theory. J. Soil Sci. 37:499-510
  #
  # PÅLITLIGHET:
  # Alla värden markerade som UPPSKATTADE baserat på pigmentklassbeteende,
  # kemisk sammansättning och jämförelse med liknande pigment i befintlig databas.
  #
  # KÄLLOR:
  # [1] Kremer Pigmente: https://www.kremer-pigmente.com
  # [2] Ocres de France: https://www.ocres-de-france.com
  # [3] Kubelka & Munk (1931): Optik av färgbeläggningar
  # [4] Mudgett & Richards (1973): K-M koefficienter
  # [5] Ma, Johnston & Koran (1987): Färgnoggrannhet i K-M-teori
  # [6] Barron (1986): K-M-teori för järnoxider i jord
  # ============================================================================
  
  # Lägg till dessa poster i pigments_db-listan i pigments_unified.R
  
  # JORDFÄRGSPIGMENT - Bruna och speciella jordfärger
  "11620" = list(
    id = "11620",
    name = "Brun jord från Otranto",
    properties = list(
      oil = 40,
      K = 0.68,
      S = 0.42,
      density = 2.8,
      rgb = c(140, 96, 74)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Kremer katalog + typiska bruna jordfärgegenskaper"
    ),
    suppliers = list(
      kremer = list(
        id = "11620",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/11620-brown-earth-from-otranto.html"
      )
    ),
    notes = "Naturlig brunockra från Otranto, Italien. Järnoxid med kalkavlagringar ('ärtmalm'). PBr7. Mycket god ljushärdighet (8/8). Sanguinbrun nyans."
  ),
  
  "17280" = list(
    id = "17280",
    name = "Persiskt rött",
    properties = list(
      oil = 28,
      K = 0.92,
      S = 0.38,
      density = 5.1,
      rgb = c(188, 63, 59)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Kremer spec: ~70% Fe2O3, partikelstorlek ~20µm"
    ),
    suppliers = list(
      kremer = list(
        id = "17280",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/kremer-made-and-historic-pigments/17280-persian-red.html"
      )
    ),
    notes = "Historiskt järnoxidrött från Hormuz. PR102, ca 70% järnoxid, mycket fin kornstorlek (~20 µm). Varm djup röd nyans. Excellent ljushärdighet."
  ),
  
  # FRANSKA OCKROR - Gulserie
  "40010" = list(
    id = "40010",
    name = "Fransk ockra Jaune Clair (ljusgul)",
    properties = list(
      oil = 22,
      K = 0.42,
      S = 0.36,
      density = 3.4,
      rgb = c(217, 179, 111)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "yellow",
      reliability = "ESTIMATED",
      lab_source = "Franska ockrastandarder + befintlig 44082-referens"
    ),
    suppliers = list(
      kremer = list(
        id = "40010",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40010-french-ochre-jtcles.html"
      ),
      ocres_de_france = list(
        name = "Ocre jaune clair",
        match = "ekvivalent",
        url = "https://www.ocres-de-france.com/en/yellow-pigments/499-pigment-ocre-jaune-clair.html"
      )
    ),
    notes = "Fransk naturlig gulockra, ljus nyans. PY43. Naturligt hydratiserat järnoxid med lera. Klassisk byggnadsvårdsfärg. Ocres de France producerar liknande pigment från Vaucluse, Frankrike."
  ),
  
  "40020" = list(
    id = "40020",
    name = "Fransk ockra Jaune Foncé (mörkgul)",
    properties = list(
      oil = 24,
      K = 0.50,
      S = 0.40,
      density = 3.5,
      rgb = c(192, 151, 89)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "yellow",
      reliability = "ESTIMATED",
      lab_source = "Interpolerad från fransk ockraserie"
    ),
    suppliers = list(
      kremer = list(
        id = "40020",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40020-french-ochre-rtfles.html"
      )
    ),
    notes = "Fransk naturlig gulockra, mörk nyans. PY43. Högre järninnehåll än ljus variant. Mycket god ljushärdighet."
  ),
  
  "40030" = list(
    id = "40030",
    name = "Fransk ockra Jaune d'Or (guldgul)",
    properties = list(
      oil = 23,
      K = 0.54,
      S = 0.44,
      density = 3.6,
      rgb = c(196, 150, 86)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "yellow",
      reliability = "ESTIMATED",
      lab_source = "Liknande befintlig GO94 guldockra"
    ),
    suppliers = list(
      kremer = list(
        id = "40030",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40030-french-ochre-joles.html"
      )
    ),
    notes = "Fransk guldockra med varmt ton. PY43. Klassisk ockra för byggnadsvård. Liknar svensk guldockra."
  ),
  
  "40050" = list(
    id = "40050",
    name = "Fransk ockra Jaune Foncé Havane Style",
    properties = list(
      oil = 25,
      K = 0.56,
      S = 0.42,
      density = 3.5,
      rgb = c(190, 141, 82)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "yellow",
      reliability = "ESTIMATED",
      lab_source = "Interpolerad mellan gula och bruna ockror"
    ),
    suppliers = list(
      kremer = list(
        id = "40050",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40050-french-ochre-jfles.html"
      )
    ),
    notes = "Fransk mörkgul ockra med brunaktigt ton. PY43. Övergångsfärg mellan gult och brunt."
  ),
  
  "40060" = list(
    id = "40060",
    name = "Fransk ockra Jaune Orangé (orangegul)",
    properties = list(
      oil = 24,
      K = 0.58,
      S = 0.48,
      density = 3.6,
      rgb = c(220, 159, 89)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "orange",
      reliability = "ESTIMATED",
      lab_source = "Positionerad mellan gula och orange järnoxider"
    ),
    suppliers = list(
      kremer = list(
        id = "40060",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40060-french-ochre-jals.html"
      )
    ),
    notes = "Fransk orangetonad ockra. PY43. Varm nyans mellan gult och orange. För ljusa fasadfärger."
  ),
  
  "40070" = list(
    id = "40070",
    name = "Fransk ockra Sofo d'Or (guldbrunt)",
    properties = list(
      oil = 26,
      K = 0.62,
      S = 0.45,
      density = 3.6,
      rgb = c(181, 128, 78)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Djup guldpositionering i ockraserien"
    ),
    suppliers = list(
      kremer = list(
        id = "40070",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40070-french-ochre-sofodor.html"
      )
    ),
    notes = "Fransk guldbrunt ockra. PY43. Mörkare gulbrun nyans, rik färg för historiska rekonstruktioner."
  ),
  
  "40080" = list(
    id = "40080",
    name = "Fransk ockra Havane (havannabrunt)",
    properties = list(
      oil = 28,
      K = 0.68,
      S = 0.46,
      density = 3.7,
      rgb = c(168, 111, 70)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Övergång mellan ockra och brun jord"
    ),
    suppliers = list(
      kremer = list(
        id = "40080",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40080-french-ochre-havane.html"
      ),
      ocres_de_france = list(
        name = "Ocre havane",
        match = "ekvivalent",
        url = "https://www.ocres-de-france.com/en/orange-pigments/498-pigment-ocre-havane.html"
      )
    ),
    notes = "Fransk havannabrunt ockra. PY43. Mörkbrun ockranyans, övergång mot jordbruna färger. Ocres de France producerar 'Ocre Havane' - den enda orange nyansen från Vaucluse."
  ),
  
  "40090" = list(
    id = "40090",
    name = "Fransk ockra Sofo Rouge (rödbrun)",
    properties = list(
      oil = 30,
      K = 0.72,
      S = 0.48,
      density = 3.8,
      rgb = c(166, 103, 72)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Övergång mellan ockra och rött oxid"
    ),
    suppliers = list(
      kremer = list(
        id = "40090",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40090-french-ochre-soforouge.html"
      )
    ),
    notes = "Fransk rödbrun ockra. PY43. Övergångsfärg mot röda toner. För varma fasader."
  ),
  
  "40130" = list(
    id = "40130",
    name = "Fransk ockra Sahara",
    properties = list(
      oil = 26,
      K = 0.52,
      S = 0.42,
      density = 3.5,
      rgb = c(200, 159, 103)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "yellow",
      reliability = "ESTIMATED",
      lab_source = "Fransk ockra medelnivå"
    ),
    suppliers = list(
      kremer = list(
        id = "40130",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40130-french-ochre-sahara.html"
      )
    ),
    notes = "Fransk saharaockra med neutral gulbrun ton. PY43. För neutrala fasadkulörer."
  ),
  
  "40214" = list(
    id = "40214",
    name = "Guldockra DD",
    properties = list(
      oil = 24,
      K = 0.56,
      S = 0.46,
      density = 3.6,
      rgb = c(208, 156, 90)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "yellow",
      reliability = "ESTIMATED",
      lab_source = "Tysk DD-kvalitet + befintlig GO94"
    ),
    suppliers = list(
      kremer = list(
        id = "40214",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40214-gold-ochre-dd.html"
      )
    ),
    notes = "Tysk guldockra DD-kvalitet. PY43. Rik guldgul ton, liknande svensk guldockra GO94."
  ),
  
  # JORDFÄRGSPIGMENT - Siennas och umbror
  "40470" = list(
    id = "40470",
    name = "Bränd sienna från Frankrike",
    properties = list(
      oil = 48,
      K = 0.78,
      S = 0.52,
      density = 3.5,
      rgb = c(159, 73, 65)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Matchad till befintlig 44620 bränd sienna"
    ),
    suppliers = list(
      kremer = list(
        id = "40470",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40470-burnt-sienna-from-france.html"
      )
    ),
    notes = "Fransk bränd sienna. PBr7. Varm rödbrun naturlig jord, bränd för att intensifiera färgen. Klassiskt konstnärspigment. Se 44620 för RAÄ-referens."
  ),
  
  "40542" = list(
    id = "40542",
    name = "Engelskt rött ljust",
    properties = list(
      oil = 32,
      K = 0.72,
      S = 0.42,
      density = 4.9,
      rgb = c(198, 105, 85)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Ljusare variant av befintlig ER48A"
    ),
    suppliers = list(
      kremer = list(
        id = "40542",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40542-english-red-light.html"
      )
    ),
    notes = "Ljus variant av engelskt rött järnoxid. PR102. Klassisk byggnadsvårdsfärg, ljusare än standard engelskt rött. Se ER48A för RAÄ-referens."
  ),
  
  "40610" = list(
    id = "40610",
    name = "Obränd umbra",
    properties = list(
      oil = 50,
      K = 0.88,
      S = 0.44,
      density = 3.4,
      rgb = c(117, 82, 61)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Matchad till befintlig OU103 obränd umbra"
    ),
    suppliers = list(
      kremer = list(
        id = "40610",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40610-raw-umber.html"
      )
    ),
    notes = "Obränd umbra, naturlig brungrön jord. PBr7. Standard umbra för linoljefärg. Cypriotisk ursprung. Se OU103 för RAÄ-referens."
  ),
  
  "40630" = list(
    id = "40630",
    name = "Obränd umbra grönaktig",
    properties = list(
      oil = 52,
      K = 0.86,
      S = 0.46,
      density = 3.5,
      rgb = c(105, 76, 59)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Grön variant av obränd umbra, matchad till GU30"
    ),
    suppliers = list(
      kremer = list(
        id = "40630",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40630-raw-umber-greenish.html"
      )
    ),
    notes = "Obränd umbra med grön underton. PBr7. Används för gröngrå nyanser. Tysk/cypriotisk. Se GU30 för RAÄ-referens."
  ),
  
  "40720" = list(
    id = "40720",
    name = "Bränd umbra mörkbrun",
    properties = list(
      oil = 58,
      K = 1.15,
      S = 0.54,
      density = 3.6,
      rgb = c(93, 57, 48)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Matchad till befintlig BU100 bränd umbra"
    ),
    suppliers = list(
      kremer = list(
        id = "40720",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40720-burnt-umber-dark-brown.html"
      )
    ),
    notes = "Mycket mörk bränd umbra. PBr7. Kalla mörkbruna toner för schattering. Cypriotisk bränd umbra. Se BU100 för RAÄ-referens."
  ),
  
  "40830" = list(
    id = "40830",
    name = "Grön jord från Frankrike",
    properties = list(
      oil = 36,
      K = 0.64,
      S = 0.58,
      density = 3.2,
      rgb = c(120, 127, 98)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "green",
      reliability = "ESTIMATED",
      lab_source = "Liknande befintlig 40850 och 40860 gröna jordar"
    ),
    suppliers = list(
      kremer = list(
        id = "40830",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40830-green-earth-from-france.html"
      )
    ),
    notes = "Fransk grön jord (terre verte). PG23. Naturlig celadonit/glaukonit. Klassiskt pigment för underliggande toner i porträtt."
  ),
  
  "41700" = list(
    id = "41700",
    name = "Grön jord Verona",
    properties = list(
      oil = 38,
      K = 0.68,
      S = 0.62,
      density = 3.3,
      rgb = c(126, 136, 100)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "green",
      reliability = "ESTIMATED",
      lab_source = "Liknande befintlig 40860 Verona grön jord"
    ),
    suppliers = list(
      kremer = list(
        id = "41700",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/41700-verona-green-earth.html"
      )
    ),
    notes = "Verona grön jord från Italien. PG23. Klassisk verones grön för fresko och olja. Naturlig glaukonit."
  ),
  
  "41750" = list(
    id = "41750",
    name = "Grön jord Vagone",
    properties = list(
      oil = 37,
      K = 0.66,
      S = 0.60,
      density = 3.2,
      rgb = c(124, 133, 101)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "green",
      reliability = "ESTIMATED",
      lab_source = "Variant av Verona grön jord"
    ),
    suppliers = list(
      kremer = list(
        id = "41750",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/41750-vagone-green-earth.html"
      )
    ),
    notes = "Vagone grön jord från Italien. PG23. Variant av Verona grön med något ljusare ton."
  ),
  
  "41600" = list(
    id = "41600",
    name = "Terra Ercolano (Kremer)",
    properties = list(
      oil = 34,
      K = 0.82,
      S = 0.40,
      density = 3.8,
      rgb = c(187, 81, 69)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "red",
      color_category = "red",
      reliability = "CONFIRMED",
      lab_source = "San Giovanni Ilarione quarry, Veneto, Italy"
    ),
    suppliers = list(
      kremer = list(
        id = "41600",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/41600-terra-ercolano.html"
      ),
      ocres_de_france = list(
        name = "Rouge Ercolano",
        match = "identisk",
        confidence = "confirmed",
        notes = "SAME PIGMENT: San Giovanni Ilarione source",
        url = "https://www.ocres-de-france.com/en/red-pigments/552-pigment-rouge-ercolano.html"
      )
    ),
    notes = "Terra Ercolano - naturlig röd jord från San Giovanni Ilarione, Veneto, Italien. PR101. Hematit. Brilliant earth tone. IDENTICAL to Ocres de France Rouge Ercolano - same quarry source. 100% naturlig."
  ),
  
  "11000" = list(
    id = "11000",
    name = "Grön jord Verona",
    properties = list(
      oil = 38,
      K = 0.68,
      S = 0.62,
      density = 3.3,
      rgb = c(126, 136, 100)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "green",
      reliability = "ESTIMATED",
      lab_source = "Alternativt katalognummer, samma pigment som 41700"
    ),
    suppliers = list(
      kremer = list(
        id = "11000",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/11000-verona-green-earth.html"
      )
    ),
    notes = "Alternativ Verona grön jord. PG23. Samma pigmenttyp som 41700, kan vara annan kvalitetsklass."
  ),
  
  # MODERNA SYNTETISKA PIGMENT
  "23000" = list(
    id = "23000",
    name = "Phthalogrön blåaktig PG7",
    properties = list(
      oil = 52,
      K = 1.85,
      S = 1.55,
      density = 2.0,
      rgb = c(0, 85, 46)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "green",
      color_category = "green",
      reliability = "ESTIMATED",
      lab_source = "Phthalocyaninlitteratur + befintlig 11100"
    ),
    suppliers = list(
      kremer = list(
        id = "23000",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/23000-phthalo-green-bluish-pg-7.html"
      )
    ),
    notes = "Phthalocyaningrön, blåaktig nyans. PG7. Mycket hög färgstyrka (95-105%), excellent ljushärdighet (8/8). Organiskt pigment. Kall grön."
  ),
  
  "23050" = list(
    id = "23050",
    name = "Phthaloblå primär PB15:1",
    properties = list(
      oil = 48,
      K = 1.92,
      S = 1.28,
      density = 2.0,
      rgb = c(0, 69, 120)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "blue",
      color_category = "blue",
      reliability = "ESTIMATED",
      lab_source = "Phthalocyaninblå litteratur + befintlig 11670"
    ),
    suppliers = list(
      kremer = list(
        id = "23050",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/23050-phthalo-blue-primary-pb-15-1.html"
      )
    ),
    notes = "Phthalocyaninblå, primär nyans. PB15:1. Mycket hög färgstyrka, excellent ljushärdighet. Organiskt pigment. Rent blå ton."
  ),
  
  "23720" = list(
    id = "23720",
    name = "Quinacridon rött magenta PV19",
    properties = list(
      oil = 55,
      K = 1.68,
      S = 0.52,
      density = 1.5,
      rgb = c(180, 54, 149)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "red",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Quinacridonlitteratur: K=1.60-1.80, S=0.45-0.55"
    ),
    suppliers = list(
      kremer = list(
        id = "23720",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/pigments-of-modern-age/23720-quinacridone-red-magenta-pv-19.html"
      )
    ),
    notes = "Quinacridon magenta. PV19. Hög färgstyrka, transparent, excellent ljushärdighet (7-8/8). Organiskt pigment för lasering och intensiva toner."
  ),
  
  # TITANPIGMENT
  "43300" = list(
    id = "43300",
    name = "Titanorange",
    properties = list(
      oil = 20,
      K = 0.62,
      S = 1.85,
      density = 4.2,
      rgb = c(253, 143, 47)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "orange",
      color_category = "orange",
      reliability = "ESTIMATED",
      lab_source = "Titanbaserat pigment med högt S-värde"
    ),
    suppliers = list(
      kremer = list(
        id = "43300",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/pigments-of-modern-age/43300-titanium-orange.html"
      )
    ),
    notes = "Titanbaserat orange pigment. Mycket hög täckförmåga och god ljushärdighet. Modern syntetiskt pigment."
  ),
  
  "46280" = list(
    id = "46280",
    name = "Buff titanium",
    properties = list(
      oil = 18,
      K = 0.22,
      S = 2.15,
      density = 4.1,
      rgb = c(239, 208, 165)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "beige",
      reliability = "ESTIMATED",
      lab_source = "Titanvit-baserad med minimal färgämne"
    ),
    suppliers = list(
      kremer = list(
        id = "46280",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/46280-buff-titanium.html"
      )
    ),
    notes = "Buff titanium, ljus beige-gul. Titanbaserat pigment med mycket hög täckförmåga. För ljusa, täckande toner."
  ),
  
  # KOLSVARTA
  "47700" = list(
    id = "47700",
    name = "Grafitpulver silver",
    properties = list(
      oil = 65,
      K = 1.45,
      S = 0.88,
      density = 2.2,
      rgb = c(148, 144, 136)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black",
      color_category = "grey",
      reliability = "ESTIMATED",
      lab_source = "Grafitegenskaper: K=1.40-1.50, S=0.85-0.92"
    ),
    suppliers = list(
      kremer = list(
        id = "47700",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/pigments-of-modern-age/carbon-black/47700-graphite-powder-silver.html"
      )
    ),
    notes = "Grafitpulver med metallisk silverton. Kolbaserat pigment. Ger metallisk glans åt färgen. Lägre färgstyrka än kimrök."
  ),
  
  "47800" = list(
    id = "47800",
    name = "Träkolspulver",
    properties = list(
      oil = 70,
      K = 1.85,
      S = 0.72,
      density = 1.8,
      rgb = c(88, 81, 78)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black",
      color_category = "black",
      reliability = "ESTIMATED",
      lab_source = "Träkolegenskaper: K=1.80-1.90, S=0.70-0.75"
    ),
    suppliers = list(
      kremer = list(
        id = "47800",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/pigments-of-modern-age/carbon-black/47800-charcoal-powder.html"
      )
    ),
    notes = "Träkolspulver. PBk6/7. Naturligt svart från pyrolys av trä. Lägre färgstyrka än kolsvart, mjukare svart med brunaktig underton. Mycket hög oljeabsorption."
  ),
  
  # JÄRNOXIDPIGMENT - Moderna syntetiska
  "48289" = list(
    id = "48289",
    name = "Järnoxidrött mikroniserat",
    properties = list(
      oil = 18,
      K = 1.05,
      S = 0.42,
      density = 5.2,
      rgb = c(201, 68, 58)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Mikroniserat = högre K, lägre olja än standard rött oxid"
    ),
    suppliers = list(
      kremer = list(
        id = "48289",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/iron-oxide-pigments/48289-iron-oxide-red-micronized.html"
      )
    ),
    notes = "Mikroniserat järnoxidrött. PR101. Mycket fin kornstorlek ger högre färgstyrka och jämnare dispersion. Låg oljeabsorption."
  ),
  
  "48401" = list(
    id = "48401",
    name = "Järnoxidsvart neutralt",
    properties = list(
      oil = 17,
      K = 2.52,
      S = 1.15,
      density = 5.2,
      rgb = c(59, 59, 59)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black",
      color_category = "black",
      reliability = "ESTIMATED",
      lab_source = "Liknande befintlig J318 svart oxid"
    ),
    suppliers = list(
      kremer = list(
        id = "48401",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/iron-oxide-pigments/48401-iron-oxide-black-neutral.html"
      )
    ),
    notes = "Neutralt järnoxidsvart. PBk11. Syntetiskt magnetit (Fe₃O₄). Neutral svart ton utan färgskiftning."
  ),
  
  "48651" = list(
    id = "48651",
    name = "Hematit intensiv färgstyrka",
    properties = list(
      oil = 22,
      K = 1.15,
      S = 0.48,
      density = 5.3,
      rgb = c(160, 51, 52)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Intensiv kvalitet = högre K än standard hematit"
    ),
    suppliers = list(
      kremer = list(
        id = "48651",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/iron-oxide-pigments/48651-haematite-intense-tinting.html"
      )
    ),
    notes = "Hematit med intensiv färgstyrka. PR102. Naturligt järnoxid (Fe₂O₃) bearbetat för maximal färgstyrka. Djup röd nyans."
  ),
  
  "47250" = list(
    id = "47250",
    name = "Kimrök",
    properties = list(
      oil = 60,
      K = 2.95,
      S = 1.05,
      density = 1.8,
      rgb = c(44, 44, 44)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black",
      color_category = "black",
      reliability = "ESTIMATED",
      lab_source = "Kimrökslitteratur: K=2.85-3.00, S=1.00-1.10"
    ),
    suppliers = list(
      kremer = list(
        id = "47250",
        match = "exakt",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/47250-furnace-black.html"
      ),
      ocres_de_france = list(
        name = "Noir de Rome",
        match = "similar",
        confidence = "medium",
        notes = "Roman black - traditional black pigment",
        url = "https://www.ocres-de-france.com/en/black-pigments/542-194-pigment-noir-de-rome.html"
      )
    ),
    notes = "Kimrök, modern variant av lampsvart (Furnace Black), kolsvart PBk7. Mycket hög färgstyrka och djup svart nyans. Framställt genom ofullständig förbränning av kolväten i ugn. Ett av de svarta pigment som har högst färgstyrka. Mycket hög oljeabsorption. Neutral blåsvart underton."
  ),
  
  # ========================================================================
  # OCRES DE FRANCE - VAUCLUSE GULA OCKROR
  # ========================================================================
  
  "ODF_JFLES" = list(
    id = "ODF_JFLES",
    name = "Ocre jaune foncé JFLES (Ocres de France)",
    properties = list(
      oil = 24,
      K = 0.54,
      S = 0.44,
      density = 3.6,
      rgb = c(205, 157, 89)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "yellow",
      reliability = "ESTIMATED",
      lab_source = "ODF produktbeskrivning: stjärnprodukt, ren tvättad ockra"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Ocre jaune foncé JFLES",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/yellow-pigments/500-43-pigment-ocre-jaune-fonce-jfles.html"
      )
    ),
    notes = "Ocres de France 'stjärnprodukt'. JFLES = Jaune Foncé Lavé Extra Supérieur (Mörkgul Tvättad Extra Överlägsen). Utvunnen från Gargas-stenbrott, Vaucluse. Lång process av utvinning, separation, tvättning, sedimentation, torkning och malning för att uppnå exceptionell renhet. Även känd som 'gulockra från Vaucluse'. Används för att tillverka Ocre rouge RFLES genom bränning. 100% naturlig. PY43."
  ),
  
  "ODF_OXY_J" = list(
    id = "ODF_OXY_J",
    name = "Oxy Apt jaune (Ocres de France)",
    properties = list(
      oil = 23,
      K = 0.50,
      S = 0.42,
      density = 3.5,
      rgb = c(207, 164, 102)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "yellow",
      reliability = "ESTIMATED",
      lab_source = "Pigmentberedning baserad på ODF gulockra"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Oxy Apt jaune",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/yellow-pigments/504-324-pigment-oxy-apt-jaune.html"
      )
    ),
    notes = "Oxy Apt jaune - pigmentberedning baserad på gulockra från Gargas-stenbrott. Ursprungligen kallad 'gul oxid från Apt' (Apt = stad där Ocres de Frances fabrik ligger). 98% naturlig. Något ljusare än JFLES. PY43."
  ),
  
  "ODF_ICLES" = list(
    id = "ODF_ICLES",
    name = "Ocre Iclès-Sof (Ocres de France)",
    properties = list(
      oil = 24,
      K = 0.52,
      S = 0.43,
      density = 3.5,
      rgb = c(200, 155, 91)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "yellow",
      reliability = "ESTIMATED",
      lab_source = "Pigmentberedning från 2004"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Ocre Iclès-Sof",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/yellow-pigments/506-230-pigment-ocre-icles-sof.html"
      )
    ),
    notes = "Ocre Iclès-Sof - pigmentberedning baserad på gulockra, skapad 2004. 98% naturlig. Mellanliggande gulockraton. PY43."
  ),
  
  # ========================================================================
  # OCRES DE FRANCE - VAUCLUSE RÖDA OCKROR
  # ========================================================================
  
  "ODF_RFLES" = list(
    id = "ODF_RFLES",
    name = "Ocre rouge RFLES (Ocres de France)",
    properties = list(
      oil = 26,
      K = 0.90,
      S = 0.40,
      density = 3.8,
      rgb = c(195, 73, 63)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "red",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Bränd från JFLES, högsta järnoxidinnehåll"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Ocre rouge RFLES",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/red-pigments/507-49-pigment-ocre-rouge-rfles.html"
      )
    ),
    notes = "Ocre rouge RFLES (Rouge Foncé Lavé Extra Supérieur). Tillverkad genom bränning av Ocre jaune foncé JFLES från Gargas-stenbrott. Finaste ockran, högsta järnoxidinnehåll. Lysande, mjuk röd färg. 100% naturlig. PR102."
  ),
  
  "ODF_OXY_R" = list(
    id = "ODF_OXY_R",
    name = "Oxy Apt rouge (Ocres de France)",
    properties = list(
      oil = 25,
      K = 0.85,
      S = 0.38,
      density = 3.7,
      rgb = c(197, 82, 71)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "red",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Rött oxidberedning baserad på gulockra"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Oxy Apt rouge",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/red-pigments/505-333-pigment-oxy-apt-rouge.html"
      )
    ),
    notes = "Oxy Apt rouge - pigmentberedning baserad på gulockra. Ursprungligen 'rött oxid från Apt'. 100% naturlig. Något ljusare än RFLES. PR102."
  ),
  
  "ODF_MARRON" = list(
    id = "ODF_MARRON",
    name = "Ocre marron (Ocres de France)",
    properties = list(
      oil = 28,
      K = 0.75,
      S = 0.45,
      density = 3.7,
      rgb = c(137, 87, 64)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "brown",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Blandning av ockra och järnoxider"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Ocre marron",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/brown-pigments/503-61-pigment-ocre-marron.html"
      )
    ),
    notes = "Ocre marron (Brunockra) - pigmentberedning skapad av Ocres de France. Sammansatt av ockra och andra järnoxider. 93% naturlig. Varm brun ton. PBr7."
  ),
  
  # ========================================================================
  # OCRES DE FRANCE - SIENNAS
  # ========================================================================
  
  "ODF_SIENNA" = list(
    id = "ODF_SIENNA",
    name = "Sienne naturelle (Ocres de France)",
    properties = list(
      oil = 42,
      K = 0.62,
      S = 0.48,
      density = 3.3,
      rgb = c(194, 130, 89)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Naturlig sienna jord, sammansättning ändrad 2017"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Sienne naturelle",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/yellow-pigments/590-424-pigment-sienne-naturelle.html"
      )
    ),
    notes = "Sienne naturelle (Naturlig Sienna). Mycket gammalt pigment, traditionellt från Siena, Italien. Finns även i Ardennerna, Cypern, Tyskland, England, Mexiko, södra Kina, Indien. OBS: Sedan 2017 har Ocres de Frances sienna-källa ändrats - kontakta dem för arbetskontinuitet om man använder pre-2017. 100% naturlig. PBr7."
  ),
  
  "ODF_SI_CAL" = list(
    id = "ODF_SI_CAL",
    name = "Sienne calcinée (Ocres de France)",
    properties = list(
      oil = 48,
      K = 0.80,
      S = 0.52,
      density = 3.5,
      rgb = c(173, 77, 62)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Bränd naturlig sienna"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Sienne calcinée",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/red-pigments/588-412-pigment-sienne-calcinee.html"
      )
    ),
    notes = "Sienne calcinée (Bränd Sienna). Resultat av bränning av naturlig sienna. Varm rödbrun färg. 100% naturlig. PBr7."
  ),
  
  "ODF_SAHARA" = list(
    id = "ODF_SAHARA",
    name = "Sienne claire Sahara (Ocres de France)",
    properties = list(
      oil = 38,
      K = 0.56,
      S = 0.46,
      density = 3.3,
      rgb = c(188, 151, 108)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "beige",
      reliability = "ESTIMATED",
      lab_source = "Ljus sienna-variant"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Sienne claire (Sahara)",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/green-pigments/589-418-pigment-sienne-claire-sahara.html"
      )
    ),
    notes = "Sienne claire (Sahara) - ljus sienna med varm beige ton. 100% naturlig. PBr7."
  ),
  
  # ========================================================================
  # OCRES DE FRANCE - ITALIENSKA JORDFÄRGER
  # ========================================================================
  
  "ODF_TERRE_J" = list(
    id = "ODF_TERRE_J",
    name = "Terre jaune d'Italie (Ocres de France)",
    properties = list(
      oil = 36,
      K = 0.48,
      S = 0.42,
      density = 3.2,
      rgb = c(212, 169, 107)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "yellow",
      reliability = "ESTIMATED",
      lab_source = "Italiensk gul jord från Verona"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Terre jaune d'Italie",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/yellow-pigments/557-430-pigment-terre-jaune-d-italie.html"
      )
    ),
    notes = "Terre jaune d'Italie (Gul Jord från Italien). Färgad jord från Veronas kullar. Torkad, renad och malen med hammarkvarn. 100% naturlig. PY43."
  ),
  
  "ODF_ERCOLANO" = list(
    id = "ODF_ERCOLANO",
    name = "Rouge Ercolano (Ocres de France)",
    properties = list(
      oil = 34,
      K = 0.82,
      S = 0.40,
      density = 3.8,
      rgb = c(187, 81, 69)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "red",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Naturlig röd jord från Herculaneum-området"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Rouge Ercolano",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/red-pigments/552-377-pigment-rouge-ercolano.html"
      ),
      kremer = list(
        id = "41600",
        match = "identisk",
        confidence = "confirmed",
        notes = "SAME SOURCE: San Giovanni Ilarione quarry, Veneto, Italy",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/41600-terra-ercolano.html"
      )
    ),
    notes = "Rouge Ercolano - krossad naturlig röd jord från San Giovanni Ilarione-stenbrott, Veneto, Italien. 100% naturlig. PR102. Samma som Kremers Terra di Ercolano."
  ),
  
  "ODF_VENITIEN" = list(
    id = "ODF_VENITIEN",
    name = "Rouge Vénitien (Ocres de France)",
    properties = list(
      oil = 36,
      K = 0.80,
      S = 0.42,
      density = 3.7,
      rgb = c(197, 93, 75)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "red",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Venetiansk röd jord från Verona"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Rouge Vénitien",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/red-pigments/555-404-pigment-rouge-venitien.html"
      )
    ),
    notes = "Rouge Vénitien (Venetianskt Rött) - färgande jord från Veneto-regionen (Verona). Känd sedan antiken, efterfrågad inom konst och restaurering. 100% naturlig. PR102."
  ),
  
  "ODF_CASSEL" = list(
    id = "ODF_CASSEL",
    name = "Brun de Cassel d'Italie (Ocres de France)",
    properties = list(
      oil = 50,
      K = 0.85,
      S = 0.44,
      density = 3.0,
      rgb = c(105, 76, 59)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "brown",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Historiskt brunt från torv/lignit"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Brun de Cassel d'Italie",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/brown-pigments/538-119-pigment-brun-de-cassel-d-italie.html"
      )
    ),
    notes = "Brun de Cassel d'Italie (Casselbrunt från Italien) - jord från Verona-området. Ursprungligen från torv- eller lignitfyndigheter. Historiskt dålig ljusbeständighet, ersatt på 1800-talet av brända oxider. 100% naturlig. PBr8."
  ),
  
  # ========================================================================
  # OCRES DE FRANCE - GRÖNA JORDFÄRGER
  # ========================================================================
  
  "ODF_TV_NICO" = list(
    id = "ODF_TV_NICO",
    name = "Terre verte de Nicosie (Ocres de France)",
    properties = list(
      oil = 38,
      K = 0.68,
      S = 0.62,
      density = 3.2,
      rgb = c(115, 129, 93)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "green",
      reliability = "ESTIMATED",
      lab_source = "Cypriotisk grön jord"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Terre verte de Nicosie",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/green-pigments/561-449-pigment-terre-verte-de-nicosie.html"
      )
    ),
    notes = "Terre verte de Nicosie (Nikosia Grön Jord) - jord utvunnen på Cypern. Mycket god täckförmåga och unik färg. 100% naturlig. PG23. Celadonit/glaukonit."
  ),
  
  "ODF_TV_BRENT" = list(
    id = "ODF_TV_BRENT",
    name = "Terre verte de Brentonico (Ocres de France)",
    properties = list(
      oil = 37,
      K = 0.66,
      S = 0.60,
      density = 3.2,
      rgb = c(122, 133, 101)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "green",
      reliability = "ESTIMATED",
      lab_source = "Italiensk grön jord"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Terre verte de Brentonico",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/green-pigments/563-445-pigment-terre-verte-de-brentonico.html"
      )
    ),
    notes = "Terre verte de Brentonico - italiensk grön jord. 100% naturlig. PG23. Celadonit/glaukonit."
  ),
  
  "ODF_TV_ANC" = list(
    id = "ODF_TV_ANC",
    name = "Terre verte ancienne (Ocres de France)",
    properties = list(
      oil = 38,
      K = 0.65,
      S = 0.58,
      density = 3.1,
      rgb = c(111, 123, 95)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "green",
      reliability = "ESTIMATED",
      lab_source = "Gammal lagerbehållning grön jord"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Terre verte ancienne",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/green-pigments/560-442-pigment-terre-verte-ancienne.html"
      )
    ),
    notes = "Terre verte ancienne (Gammal Grön Jord) - grön jord från äldre lager. 100% naturlig. PG23."
  ),
  
  # ========================================================================
  # OCRES DE FRANCE - CYPRIOTISKA UMBROR
  # ========================================================================
  
  "ODF_OMBRE_D" = list(
    id = "ODF_OMBRE_D",
    name = "Ombre de Chypre D (Ocres de France)",
    properties = list(
      oil = 50,
      K = 0.88,
      S = 0.44,
      density = 3.4,
      rgb = c(119, 88, 65)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Cypern umbra typ D"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Ombre de Chypre D",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/umber-pigments-/548-297-pigment-ombre-de-chypre-d.html"
      )
    ),
    notes = "Ombre de Chypre D (Umbra D från Cypern). Cypern är största mediterrana umbraproducenten. Namn från latin 'umbra' (skugga). Förekommer som lera, vilket gör det tillgängligt och billigt. 100% naturlig. PBr7."
  ),
  
  "ODF_OMBRE_B" = list(
    id = "ODF_OMBRE_B",
    name = "Ombre brûlée de Chypre B (Ocres de France)",
    properties = list(
      oil = 54,
      K = 1.08,
      S = 0.50,
      density = 3.5,
      rgb = c(100, 68, 52)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Bränd Cypern umbra typ B"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Ombre brûlée de Chypre B",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/umber-pigments-/543-282-pigment-ombre-brulee-de-chypre-b.html"
      )
    ),
    notes = "Ombre brûlée de Chypre B (Bränd Umbra B från Cypern). Bränd Cypern umbra. 100% naturlig. PBr7."
  ),
  
  "ODF_OMBRE_FL" = list(
    id = "ODF_OMBRE_FL",
    name = "Ombre de Chypre FL (Ocres de France)",
    properties = list(
      oil = 52,
      K = 0.90,
      S = 0.46,
      density = 3.4,
      rgb = c(113, 83, 64)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Cypern umbra typ FL"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Ombre de Chypre FL",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/umber-pigments-/540-301-pigment-ombre-de-chypre-fl.html"
      )
    ),
    notes = "Ombre de Chypre FL (Umbra FL från Cypern). Obränd Cypern umbra. 100% naturlig. PBr7."
  ),
  
  "ODF_OMBRE_NAT" = list(
    id = "ODF_OMBRE_NAT",
    name = "Ombre naturelle (Ocres de France)",
    properties = list(
      oil = 48,
      K = 0.85,
      S = 0.48,
      density = 3.4,
      rgb = c(128, 91, 67)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Blandning av naturliga pigment"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Ombre naturelle",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/umber-pigments-/587-309-pigment-ombre-naturelle.html"
      )
    ),
    notes = "Ombre naturelle (Naturlig Umbra) - INTE i naturligt tillstånd, resultat av blandning av flera naturliga pigment av Ocres de France. Flaggskeppspigment med hög UV-beständighet, oumbärlig nyans för dekoration eller stenpatina. Varm brun ton. PBr7/PBr8-blandning."
  ),
  
  "ODF_OMBRE_CAL" = list(
    id = "ODF_OMBRE_CAL",
    name = "Ombre calcinée (Ocres de France)",
    properties = list(
      oil = 56,
      K = 1.10,
      S = 0.52,
      density = 3.5,
      rgb = c(97, 62, 53)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "brown",
      reliability = "ESTIMATED",
      lab_source = "Bränd beredning från sienna"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Ombre calcinée",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/umber-pigments-/586-272-pigment-ombre-calcinee.html"
      )
    ),
    notes = "Ombre calcinée (Bränd Umbra) - pigmentberedning skapad av Ocres de France från Sienna-jord. 96% naturlig. Djup brun ton. PBr7."
  ),
  
  "ODF_T_OMBRE_V" = list(
    id = "ODF_T_OMBRE_V",
    name = "Terre d'ombre verdâtre (Ocres de France)",
    properties = list(
      oil = 48,
      K = 0.82,
      S = 0.50,
      density = 3.3,
      rgb = c(84, 93, 70)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "green",
      reliability = "ESTIMATED",
      lab_source = "Grönaktig umbrajord"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Terre d'ombre verdâtre",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/green-pigments/558-434-pigment-terre-d-ombre-verdatre.html"
      )
    ),
    notes = "Terre d'ombre verdâtre (Grönaktig Umbrajord) - umbra med grön underton. 100% naturlig. PBr8."
  ),
  
  # ========================================================================
  # OCRES DE FRANCE - INDISKA PIGMENT
  # ========================================================================
  
  "ODF_INDIEN_J" = list(
    id = "ODF_INDIEN_J",
    name = "Jaune Indien (Ocres de France)",
    properties = list(
      oil = 42,
      K = 0.58,
      S = 0.44,
      density = 3.5,
      rgb = c(199, 148, 86)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow",
      color_category = "yellow",
      reliability = "ESTIMATED",
      lab_source = "Pigmentberedning, 100% naturlig"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Jaune Indien",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/yellow-pigments/526-162-pigment-jaune-indien.html"
      )
    ),
    notes = "Jaune Indien (Indiskt Gult) - pigmentberedning tillverkad av Ocres de France. 100% naturlig. Varmt gult med gyllen underton. PY43."
  ),
  
  "ODF_INDIEN_R" = list(
    id = "ODF_INDIEN_R",
    name = "Rouge Indien (Ocres de France)",
    properties = list(
      oil = 40,
      K = 0.78,
      S = 0.42,
      density = 3.8,
      rgb = c(160, 73, 60)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "red",
      color_category = "red",
      reliability = "ESTIMATED",
      lab_source = "Naturlig jord från Indien"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Rouge Indien",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/red-pigments/528-383-pigment-rouge-indien.html"
      )
    ),
    notes = "Rouge Indien (Indiskt Rött) - naturlig jord från Indien. 100% naturlig. Varmt djupt rött. PR102."
  ),
  
  "ODF_INDIEN_N" = list(
    id = "ODF_INDIEN_N",
    name = "Noir Indien (Ocres de France)",
    properties = list(
      oil = 55,
      K = 2.50,
      S = 0.95,
      density = 2.5,
      rgb = c(48, 48, 48)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black",
      color_category = "black",
      reliability = "ESTIMATED",
      lab_source = "Naturlig jord från Indien"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Noir Indien",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/black-pigments/527-209-pigment-noir-indien.html"
      )
    ),
    notes = "Noir Indien (Indiskt Svart) - pigment från Indien. 100% naturlig. Djupt svart. PBk7/PBk11-blandning."
  ),
  
  # ========================================================================
  # OCRES DE FRANCE - MODERNA SVARTA PIGMENT
  # ========================================================================
  
  "ODF_IVOIRE" = list(
    id = "ODF_IVOIRE",
    name = "Noir d'Ivoire (Ocres de France)",
    properties = list(
      oil = 52,
      K = 2.55,
      S = 0.92,
      density = 2.2,
      rgb = c(57, 52, 50)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black",
      color_category = "black",
      reliability = "ESTIMATED",
      lab_source = "Järnoxid + växtkol blandning"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Noir d'Ivoire",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/black-pigments/537-204-pigment-noir-d-ivoire.html"
      )
    ),
    notes = "Noir d'Ivoire (Elfenbenssvart) - pigmentberedning designad 2016, förfinad 2021. Äkta elfenbenssvart framställdes från brända djurben. Nu framställt från järnoxid och växtbaserat kol. Namnet behållet för färglikhet. 97% naturlig. PBk9/PBk11."
  ),
  
  "ODF_VIGNE" = list(
    id = "ODF_VIGNE",
    name = "Noir de vigne (Ocres de France)",
    properties = list(
      oil = 50,
      K = 2.48,
      S = 0.88,
      density = 2.8,
      rgb = c(62, 59, 58)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black",
      color_category = "black",
      reliability = "ESTIMATED",
      lab_source = "Järnoxid som reproducerar vinträkol"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Noir de vigne",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/black-pigments/533-199-pigment-noir-de-vigne.html"
      )
    ),
    notes = "Noir de vigne (Vinsvart) - svart järnoxid som reproducerar original vinsvart (förkolnade vinkvista). 98% naturlig. VARNING: Inte för slätpolering (risk för strimmor/'fusées'). PBk11."
  ),
  
  "ODF_ROME" = list(
    id = "ODF_ROME",
    name = "Noir de Rome (Ocres de France)",
    properties = list(
      oil = 54,
      K = 2.52,
      S = 0.90,
      density = 2.4,
      rgb = c(57, 57, 57)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black",
      color_category = "black",
      reliability = "ESTIMATED",
      lab_source = "Traditionellt romskt svart"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Noir de Rome",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/black-pigments/542-194-pigment-noir-de-rome.html"
      )
    ),
    notes = "Noir de Rome (Romskt Svart) - traditionellt svart pigment. 100% naturlig. Neutralt svart. PBk7/PBk11."
  ),
  
  # ========================================================================
  # OCRES DE FRANCE - SPECIALPIGMENT
  # ========================================================================
  
  "ODF_PATINE" = list(
    id = "ODF_PATINE",
    name = "Patine terre (Ocres de France)",
    properties = list(
      oil = 32,
      K = 0.28,
      S = 0.35,
      density = 3.0,
      rgb = c(199, 182, 157)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth",
      color_category = "beige",
      reliability = "ESTIMATED",
      lab_source = "Neutralt beige patinapigment"
    ),
    suppliers = list(
      ocres_de_france = list(
        name = "Patine terre",
        match = "exakt",
        url = "https://www.ocres-de-france.com/en/pigments-in-powder/48-330-pigment-patine-terre.html"
      )
    ),
    notes = "Patine terre (Jordpatina) - pigmentberedning skapad 2019. Neutral, naturlig färg som påminner om jord och sand. 100% naturlig. För dekoration och stenpatina. PY43/PBr7."
  )
  
)

# ============================================================================
# ANVÄNDNINGSANVISNINGAR FÖR OCRES DE FRANCE-PIGMENT
# ============================================================================
#
# Alla Ocres de France naturliga pigment (100% naturliga) är lämpliga för:
# - Kalkfärg (peinture à la chaux)
# - Kalkputs (enduit à la chaux)
# - Vax (cire)
# - Oljefärg / Linoljefärg (peinture à l'huile / linoljefärg)
# - Gips (plâtre)
# - Fresko (fresque)
# - Lasyr (glacis)
# - Cement (ciment)
# - Konst (beaux-arts)
# - Mjölfärg / Svensk målning (peinture suédoise / peinture à la farine)
#
# Maximal dosering: Generellt 10% jämfört med bindmedelvikt
#
# Förberedelse för oljemålning:
# - Mal pigment fint i mortel innan blandning med bindemedel
# - Lös upp pulver i liten mängd terpentin innan tillsättning till linolja
#
# EPV-certifiering:
# Ocres de France är EPV-certifierad (Entreprise du Patrimoine Vivant / 
# Levande Kulturarvsföretag), vilket erkänner excellens i traditionellt 
# franskt hantverk och tillverkning.
#
# ============================================================================