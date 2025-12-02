# Unified Pigment Database
# Consolidates properties, suppliers, and metadata in one structure
# Replaces: km (pigment_database.R), suppliers (supplier_data.R), raa_pigments list

pigments_db <- list(
  # BASE WHITES
  "vitbas" = list(
    id = "vitbas",
    name = "Vitbas (K-M-kompenserad titan/zink-blandning)",
    properties = list(
      oil = 17,
      K = 0.00,
      S = 2.20,
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
    notes = "K-M compensated blend of zinc and titanium white. Computed pigment."
  ),
  
  "44100" = list(
    id = "44100",
    name = "Zinkvitt PW4",
    properties = list(
      oil = 20,
      K = 0.00,
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
        match = "exact",
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
      K = 0.00,
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
        match = "exact",
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
        match = "exact",
        url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/599930-tripoli-rotten-stone-light.html"
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
        match = "exact",
        url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58000-chalk-from-champagne.html"
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
        match = "exact",
        url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58010-chalk-from-ruegen.html"
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
        match = "exact",
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
        match = "exact",
        url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58900-bentonite.html"
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
        match = "exact",
        url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58250-kaolin-yellowish.html"
      )
    ),
    notes = "Vit bolus, gulaktig kaolin-lera. Används som fyllmedel och för att öka opacitet. Färgindex: PW 19. Måttlig oljeabsorption (45%)."
  ),
  
  # GREENS
  "40400" = list(
    id = "40400",
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
        match = "equivalent",
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
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/44400-malachite-synthetic"
      )
    ),
    notes = "Syntetisk malakit som alternativ till naturlig. Speciellt pigment från Kremer."
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
        match = "equivalent",
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
      rgb = c(74, 117, 82)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "green"
    ),
    suppliers = list(
      kremer = list(
        id = "44200",
        match = "equivalent",
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
      rgb = c(110, 145, 105)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "green"
    ),
    suppliers = list(
      kremer = list(
        id = "44350",
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/44350-cobalt-green-dark"
      )
    ),
    notes = "Zinkbaserad grön, närmaste matchning för traditionell zinkgrön"
  ),
  
  "40850" = list(
    id = "40850",
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
        id = "40850",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40850-green-earth-bohemian"
      )
    ),
    notes = "EXAKT MATCHNING - Samma produktnummer! Böhmisk grön jord från Kremer."
  ),
  
  "40860" = list(
    id = "40860",
    name = "Grön jord Verona",
    properties = list(
      oil = 35,
      K = 0.65,
      S = 0.60,
      density = 3.2,
      rgb = c(100, 130, 80)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40860",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40860-green-earth-verona"
      )
    ),
    notes = "EXAKT MATCHNING - Samma produktnummer! Veronese grön jord från Kremer."
  ),
  
  "GU30" = list(
    id = "GU30",
    name = "Grön umbra nr 30",
    properties = list(
      oil = 50,
      K = 0.85,
      S = 0.48,
      density = 3.5,
      rgb = c(95, 100, 70)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40630",
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40630-raw-umber-greenish"
      )
    ),
    notes = "Tysk obränd umbra med grönaktig nyans"
  ),
  
  # BLACKS
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
      ),
      claessons = list(
        id = "9313",
        name = "Järnoxidsvart 9313",
        url = "https://claessons.com/svarta/jarnoxidsvart-9313-losvikt/"
      )
    ),
    notes = "Järnoxidsvart med hög täckförmåga. Finns hos Kremer, Ottosson och Claessons."
  ),
  
  "J318" = list(
    id = "J318",
    name = "Järnoxidsvart nr 318",
    properties = list(
      oil = 16,
      K = 2.35,
      S = 1.08,
      density = 5.1,
      rgb = c(35, 35, 38)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "black"
    ),
    suppliers = list(
      kremer = list(
        id = "48400",
        match = "exact",
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
      rgb = c(28, 28, 32)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "black"
    ),
    suppliers = list(
      kremer = list(
        id = "47100",
        match = "equivalent",
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
        match = "exact",
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
        match = "equivalent",
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
      rgb = c(45, 60, 130)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "blue"
    ),
    suppliers = list(
      kremer = list(
        id = "45000",
        match = "equivalent",
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
      rgb = c(70, 95, 155)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "blue"
    ),
    suppliers = list(
      kremer = list(
        id = "45710",
        match = "equivalent",
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
        match = "equivalent",
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
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian"
      )
    ),
    notes = "Klassisk obränd sienna från Italien. Naturligt jordpigment."
  ),
  
  "40830" = list(
    id = "40830",
    name = "Terra di Ercolano",
    properties = list(
      oil = 40,
      K = 0.75,
      S = 0.55,
      density = 3.3,
      rgb = c(175, 85, 65)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40835",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40835-terra-di-ercolano"
      )
    ),
    notes = "EXAKT MATCHNING - Terra från Herculaneum. Italienskt specialpigment."
  ),
  
  "BT44" = list(
    id = "BT44",
    name = "Bränd terra nr 44",
    properties = list(
      oil = 38,
      K = 0.78,
      S = 0.52,
      density = 3.4,
      rgb = c(170, 110, 70)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "44620",
        match = "equivalent",
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
      rgb = c(180, 130, 80)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40400",
        match = "equivalent",
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
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40010-yellow-ochre-light"
      ),
      ottosson = list(
        name = "Gul ockra",
        url = "https://ottossonfarg.com/produkt/gul-ockra/"
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
        match = "equivalent",
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
        match = "equivalent",
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
        match = "equivalent",
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
      rgb = c(195, 165, 85)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40030",
        match = "equivalent",
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
      rgb = c(210, 185, 135)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40010",
        match = "equivalent",
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
      rgb = c(185, 155, 90)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40015",
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40015-yellow-ochre-golden-italian"
      ),
      ottosson = list(
        name = "Guldockra",
        url = "https://ottossonfarg.com/produkt/guldockra/"
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
      rgb = c(135, 130, 85)
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
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian"
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
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40450-burnt-sienna-italian"
      ),
      ottosson = list(
        name = "Järnoxidrött bränd",
        url = "https://ottossonfarg.com/produkt/jarnoxidrott-brand/"
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
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40610-raw-umber"
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
      rgb = c(90, 60, 45)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40720",
        match = "equivalent",
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
      rgb = c(105, 85, 70)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40700",
        match = "equivalent",
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
      rgb = c(100, 95, 90)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40660",
        match = "equivalent",
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
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48000-transparent-brown-oxide"
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
        match = "equivalent",
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
        match = "equivalent",
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
        match = "equivalent",
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
        match = "equivalent",
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
      rgb = c(142, 52, 52)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48200",
        match = "equivalent",
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
      rgb = c(105, 45, 55)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48280",
        match = "equivalent",
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
        match = "equivalent",
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
      rgb = c(175, 80, 70)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "42100",
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/42100-english-red"
      )
    ),
    notes = "Traditionellt engelskt rött, ljust orange-rött järnoxid"
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
      rgb = c(120, 80, 60)
    ),
    metadata = list(
      is_raa = TRUE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48610",
        match = "equivalent",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48610-brown-iron-oxide-610"
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
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/48686-brown-iron-oxide-686"
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
  )
)
