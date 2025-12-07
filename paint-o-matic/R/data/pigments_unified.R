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
      rgb = c(73, 113, 50) #c(110, 145, 105)
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
      rgb = c(125, 52, 43) #c(175, 80, 70)
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
        a_tolerance = c(-1.3, 1. 3),
        b_tolerance = c(-1.5, 1.5),
        delta_E_max = 1.7,
        tinting_strength_range = c(95, 105)
      )
    ),
    suppliers = list(
      kremer = list(
        id = "48250",
        match = "exact",
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
  ),
  
  # Extension to pigments_unified.R
  # New pigments from Kremer Pigmente catalog
  # Add these entries to the pigments_db list in pigments_unified.R
  
  # EARTH PIGMENTS - Browns
  "11620" = list(
    id = "11620",
    name = "Brun jord från Otranto",
    properties = list(
      oil = 40,
      K = 0.68,
      S = 0.42,
      density = 2.5,
      rgb = c(130, 75, 60)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "11620",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/11620-brown-earth-from-otranto.html"
      )
    ),
    notes = "Naturlig brunockra från Otranto, Italien. Järnoxid med kalkavlagringar ('ärtmalm'). PBr7.Mycket god ljushärdighet (8/8). Sanguinbrun nyans."
  ),
  
  "17280" = list(
    id = "17280",
    name = "Persiskt rött",
    properties = list(
      oil = 28,
      K = 0.92,
      S = 0.38,
      density = 5.1,
      rgb = c(204, 51, 51)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "17280",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/kremer-made-and-historic-pigments/17280-persian-red.html"
      )
    ),
    notes = "Historiskt järnoxidrött från Hormuz. PR102, ca 70% järnoxid, mycket fin kornstorlek (~20 µm). Varm djup röd nyans."
  ),
  
  # FRENCH OCHRES - Yellow series
  "40010" = list(
    id = "40010",
    name = "Fransk ockra Jaune Clair (ljusgul)",
    properties = list(
      oil = 22,
      K = 0.42,
      S = 0.36,
      density = 3.4,
      rgb = c(218, 185, 125)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40010",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40010-french-ochre-jtcles.html"
      )
    ),
    notes = "Fransk naturlig gulockra, ljus nyans. PY43. Naturligt hydratiserat järnoxid med lera."
  ),
  
  "40020" = list(
    id = "40020",
    name = "Fransk ockra Jaune Foncé (mörkgul)",
    properties = list(
      oil = 24,
      K = 0.50,
      S = 0.40,
      density = 3.5,
      rgb = c(195, 155, 95)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40020",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40020-french-ochre-rtfles.html"
      )
    ),
    notes = "Fransk naturlig gulockra, mörk nyans. PY43. Högre järninnehåll än ljus variant."
  ),
  
  "40030" = list(
    id = "40030",
    name = "Fransk ockra Jaune d'Or (guldgul)",
    properties = list(
      oil = 23,
      K = 0.54,
      S = 0.44,
      density = 3.6,
      rgb = c(200, 165, 90)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40030",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40030-french-ochre-joles.html"
      )
    ),
    notes = "Fransk guldockra med varmt ton. PY43. Klassisk ockra för byggnadsvård."
  ),
  
  "40050" = list(
    id = "40050",
    name = "Fransk ockra Jaune Foncé Havane Style",
    properties = list(
      oil = 25,
      K = 0.56,
      S = 0.42,
      density = 3.5,
      rgb = c(185, 145, 80)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40050",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40050-french-ochre-jfles.html"
      )
    ),
    notes = "Fransk mörkgul ockra med brunaktigt ton. PY43."
  ),
  
  "40060" = list(
    id = "40060",
    name = "Fransk ockra Jaune Orangé (orangegul)",
    properties = list(
      oil = 24,
      K = 0.58,
      S = 0.48,
      density = 3.6,
      rgb = c(210, 150, 70)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40060",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40060-french-ochre-jals.html"
      )
    ),
    notes = "Fransk orangetonad ockra. PY43. Varm nyans mellan gult och orange."
  ),
  
  "40070" = list(
    id = "40070",
    name = "Fransk ockra Sofo d'Or (guldbrunt)",
    properties = list(
      oil = 26,
      K = 0.62,
      S = 0.45,
      density = 3.6,
      rgb = c(175, 125, 65)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40070",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40070-french-ochre-sofodor.html"
      )
    ),
    notes = "Fransk guldbrunt ockra. PY43. Mörkare gulbrun nyans."
  ),
  
  "40080" = list(
    id = "40080",
    name = "Fransk ockra Havane (havannabrunt)",
    properties = list(
      oil = 28,
      K = 0.68,
      S = 0.46,
      density = 3.7,
      rgb = c(160, 110, 60)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40080",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40080-french-ochre-havane.html"
      )
    ),
    notes = "Fransk havannabrunt ockra. PY43. Mörkbrun ockranyans."
  ),
  
  "40090" = list(
    id = "40090",
    name = "Fransk ockra Sofo Rouge (rödbrun)",
    properties = list(
      oil = 30,
      K = 0.72,
      S = 0.48,
      density = 3.8,
      rgb = c(155, 95, 55)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40090",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40090-french-ochre-soforouge.html"
      )
    ),
    notes = "Fransk rödbrun ockra. PY43. Övergångsfärg mot röda toner."
  ),
  
  "40130" = list(
    id = "40130",
    name = "Fransk ockra Sahara",
    properties = list(
      oil = 26,
      K = 0.52,
      S = 0.42,
      density = 3.5,
      rgb = c(195, 160, 105)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40130",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40130-french-ochre-sahara.html"
      )
    ),
    notes = "Fransk saharaockra med neutral gulbrun ton. PY43."
  ),
  
  "40214" = list(
    id = "40214",
    name = "Guldockra DD",
    properties = list(
      oil = 24,
      K = 0.56,
      S = 0.46,
      density = 3.6,
      rgb = c(195, 150, 85)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "40214",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40214-gold-ochre-dd.html"
      )
    ),
    notes = "Tysk guldockra DD-kvalitet. PY43. Rik guldgul ton."
  ),
  
  # EARTH PIGMENTS - Siennas and Umbers
  "40470" = list(
    id = "40470",
    name = "Bränd sienna från Frankrike",
    properties = list(
      oil = 48,
      K = 0.78,
      S = 0.52,
      density = 3.5,
      rgb = c(168, 88, 50)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40470",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40470-burnt-sienna-from-france.html"
      )
    ),
    notes = "Fransk bränd sienna. PBr7. Varm rödbrun naturlig jord, bränd för att intensifiera färgen."
  ),
  
  "40542" = list(
    id = "40542",
    name = "Engelskt rött ljust",
    properties = list(
      oil = 32,
      K = 0.72,
      S = 0.42,
      density = 4.9,
      rgb = c(185, 85, 75)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "40542",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40542-english-red-light.html"
      )
    ),
    notes = "Ljus variant av engelskt rött järnoxid. PR102. Klassisk byggnadsvårdsfärg."
  ),
  
  "40610" = list(
    id = "40610",
    name = "Obränd umbra",
    properties = list(
      oil = 50,
      K = 0.88,
      S = 0.44,
      density = 3.4,
      rgb = c(105, 85, 65)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40610",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/40610-raw-umber.html"
      )
    ),
    notes = "Obränd umbra, naturlig brungrön jord. PBr7. Standard umbra för linoljefärg."
  ),
  
  "40630" = list(
    id = "40630",
    name = "Obränd umbra grönaktig",
    properties = list(
      oil = 52,
      K = 0.86,
      S = 0.46,
      density = 3.5,
      rgb = c(95, 85, 60)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40630",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40630-raw-umber-greenish.html"
      )
    ),
    notes = "Obränd umbra med grön underton. PBr7. Används för gröngrå nyanser."
  ),
  
  "40720" = list(
    id = "40720",
    name = "Bränd umbra mörkbrun",
    properties = list(
      oil = 58,
      K = 1.15,
      S = 0.54,
      density = 3.6,
      rgb = c(75, 50, 40)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40720",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40720-burnt-umber-dark-brown.html"
      )
    ),
    notes = "Mycket mörk bränd umbra. PBr7. Kalla mörkbruna toner för schattering."
  ),
  
  "40830" = list(
    id = "40830",
    name = "Grön jord från Frankrike",
    properties = list(
      oil = 36,
      K = 0.64,
      S = 0.58,
      density = 3.2,
      rgb = c(95, 115, 75)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "40830",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/40830-green-earth-from-france.html"
      )
    ),
    notes = "Fransk grön jord (terre verte). PG23. Naturlig celadonit/glaukonit. Klassiskt pigment för underliggande toner."
  ),
  
  "41700" = list(
    id = "41700",
    name = "Verona grön jord",
    properties = list(
      oil = 38,
      K = 0.68,
      S = 0.62,
      density = 3.3,
      rgb = c(105, 125, 85)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "41700",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/41700-verona-green-earth.html"
      )
    ),
    notes = "Verona grön jord från Italien. PG23. Klassisk verones grön för fresko och olja."
  ),
  
  "41750" = list(
    id = "41750",
    name = "Vagone grön jord",
    properties = list(
      oil = 37,
      K = 0.66,
      S = 0.60,
      density = 3.2,
      rgb = c(100, 120, 80)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "41750",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/41750-vagone-green-earth.html"
      )
    ),
    notes = "Vagone grön jord från Italien. PG23. Variant av Verona grön."
  ),
  
  "11000" = list(
    id = "11000",
    name = "Verona grön jord (alternativ kvalitet)",
    properties = list(
      oil = 38,
      K = 0.68,
      S = 0.62,
      density = 3.3,
      rgb = c(105, 125, 85)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "earth"
    ),
    suppliers = list(
      kremer = list(
        id = "11000",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/11000-verona-green-earth.html"
      )
    ),
    notes = "Alternativ Verona grön jord. PG23. Samma pigmenttyp som 41700."
  ),
  
  # MODERN SYNTHETIC PIGMENTS
  "23000" = list(
    id = "23000",
    name = "Phthalogrön blåaktig PG7",
    properties = list(
      oil = 52,
      K = 1.85,
      S = 1.55,
      density = 2.0,
      rgb = c(0, 110, 65)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "green"
    ),
    suppliers = list(
      kremer = list(
        id = "23000",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/23000-phthalo-green-bluish-pg-7.html"
      )
    ),
    notes = "Phthalocyaningrön, blåaktig nyans. PG7. Mycket hög färgstyrka (95-105%), excellent ljushärdighet (8/8). Organiskt pigment."
  ),
  
  "23050" = list(
    id = "23050",
    name = "Phthaloblå primär PB15:1",
    properties = list(
      oil = 48,
      K = 1.92,
      S = 1.28,
      density = 2.0,
      rgb = c(0, 80, 145)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "blue"
    ),
    suppliers = list(
      kremer = list(
        id = "23050",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/23050-phthalo-blue-primary-pb-15-1.html"
      )
    ),
    notes = "Phthalocyaninblå, primär nyans. PB15:1. Mycket hög färgstyrka, excellent ljushärdighet. Organiskt pigment."
  ),
  
  "23720" = list(
    id = "23720",
    name = "Quinacridon rött magenta PV19",
    properties = list(
      oil = 55,
      K = 1.68,
      S = 0.52,
      density = 1.5,
      rgb = c(200, 50, 120)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "red"
    ),
    suppliers = list(
      kremer = list(
        id = "23720",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/pigments-of-modern-age/23720-quinacridone-red-magenta-pv-19.html"
      )
    ),
    notes = "Quinacridon magenta. PV19. Hög färgstyrka, transparent, excellent ljushärdighet (7-8/8).Organiskt pigment för lasering och intensiva toner."
  ),
  
  # TITANIUM PIGMENTS
  "43300" = list(
    id = "43300",
    name = "Titanorange",
    properties = list(
      oil = 20,
      K = 0.62,
      S = 1.85,
      density = 4.2,
      rgb = c(245, 145, 50)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "orange"
    ),
    suppliers = list(
      kremer = list(
        id = "43300",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/pigments-of-modern-age/43300-titanium-orange.html"
      )
    ),
    notes = "Titanbaserat orange pigment. Mycket hög täckförmåga och god ljushärdighet."
  ),
  
  "46280" = list(
    id = "46280",
    name = "Buff titanium",
    properties = list(
      oil = 18,
      K = 0.22,
      S = 2.15,
      density = 4.1,
      rgb = c(235, 215, 185)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "yellow"
    ),
    suppliers = list(
      kremer = list(
        id = "46280",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/earth-pigments/46280-buff-titanium.html"
      )
    ),
    notes = "Buff titanium, ljus beige-gul. Titanbaserat pigment med mycket hög täckförmåga. För ljusa, täckande toner."
  ),
  
  # CARBON BLACKS
  "47700" = list(
    id = "47700",
    name = "Grafitpulver silver",
    properties = list(
      oil = 65,
      K = 1.45,
      S = 0.88,
      density = 2.2,
      rgb = c(95, 95, 100)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black"
    ),
    suppliers = list(
      kremer = list(
        id = "47700",
        match = "exact",
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
      rgb = c(40, 40, 40)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black"
    ),
    suppliers = list(
      kremer = list(
        id = "47800",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/pigments-of-modern-age/carbon-black/47800-charcoal-powder.html"
      )
    ),
    notes = "Träkolspulver. PBk6/7. Naturligt svart från pyrolys av trä. Lägre färgstyrka än kolsvart, mjukare svart med brunaktig underton. Mycket hög oljeabsorption."
  ),
  
  # IRON OXIDE PIGMENTS - Modern synthetics
  "48289" = list(
    id = "48289",
    name = "Järnoxidrött mikroniserat",
    properties = list(
      oil = 18,
      K = 1.05,
      S = 0.42,
      density = 5.2,
      rgb = c(180, 50, 45)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48289",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/iron-oxide-pigments/48289-iron-oxide-red-micronized.html"
      )
    ),
    notes = "Mikroniserat järnoxidrött. PR101. Mycket fin kornstorlek ger högre färgstyrka och jämnare dispersion.L åg oljeabsorption."
  ),
  
  "48401" = list(
    id = "48401",
    name = "Järnoxidsvart neutralt",
    properties = list(
      oil = 17,
      K = 2.52,
      S = 1.15,
      density = 5.2,
      rgb = c(30, 30, 30)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black"
    ),
    suppliers = list(
      kremer = list(
        id = "48401",
        match = "exact",
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
      rgb = c(155, 40, 40)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "oxide"
    ),
    suppliers = list(
      kremer = list(
        id = "48651",
        match = "exact",
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
      rgb = c(15, 15, 15)
    ),
    metadata = list(
      is_raa = FALSE,
      is_tar_compatible = TRUE,
      category = "black"
    ),
    suppliers = list(
      kremer = list(
        id = "47250",
        match = "exact",
        url = "https://www.kremer-pigmente.com/en/shop/pigments/47250-furnace-black.html"
      )
    ),
    notes = "Kimrök, modern variant av lampsvart (Furnace Black), kolsvart PBk7.  Mycket hög färgstyrka och djup svart nyans. Framställt genom ofullständig förbränning av kolväten i ugn. Ett av de svarta pigment som har högst färgstyrka. Mycket hög oljeabsorption. Neutral blåsvart underton."
  )
)
