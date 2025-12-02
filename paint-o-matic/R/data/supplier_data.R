# Supplier Data
# Supplier information, tar colors, misc materials, and helper functions

# CONSOLIDATED SUPPLIER LINKS (Kremer + RAÄ matches)
suppliers <- list(
  # WHITES
  "44100" = list(
    name = "Zinkvitt PW4",
    kremer_match = "Zinc White (PW4)",
    kremer_id = "46300",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/46300-zinc-white",
    ottosson_match = "Zinkvitt",
    ottosson_url = "https://ottossonfarg.com/produkt/zinkvitt/",
    notes = "Högkvalitativt zinkvitt för linoljefärg. Finns hos både Kremer och svenska leverantörer."
  ),
  
  "44400" = list(
    name = "Titanvitt Rutile PW6",
    kremer_match = "Titanium White Rutile (PW6)",
    kremer_id = "46200",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/46200-titanium-white-rutile",
    ottosson_match = "Titanvitt",
    ottosson_url = "https://ottossonfarg.com/produkt/titanvitt/",
    notes = "Rutiltyp titanvitt med högsta täckförmåga. Standard vitpigment för linoljefärg."
  ),
  
  # FILLERS
  "599930" = list(
    name = "Tripoli, Rotten Stone, light",
    kremer_match = "Tripoli, Rotten Stone, light",
    kremer_id = "599930",
    kremer_url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/599930-tripoli-rotten-stone-light.html",
    notes = "Kiselgur (diatoméjord), mycket fin poleringsfyllnad. Mycket hög oljeabsorption. Används för fin polering och som mattande tillsats."
  ),
  
  "58000" = list(
    name = "Chalk from Champagne",
    kremer_match = "Chalk from Champagne",
    kremer_id = "58000",
    kremer_url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58000-chalk-from-champagne.html",
    notes = "Naturlig kalciumkarbonat från Frankrike (CaCO3). Används för grundningar, stuckatur och som fyllmedel i färg. Färgindex: PW 18.77220. Låg oljeabsorption."
  ),
  
  "58010" = list(
    name = "Chalk from Ruegen",
    kremer_match = "Chalk from Ruegen",
    kremer_id = "58010",
    kremer_url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58010-chalk-from-ruegen.html",
    notes = "Naturlig kalciumkarbonat från Tyskland, ca 40 µ. Något grövre och mer gråaktig än Champagnekrita. Färgindex: PW 18.77220. Används i väggfärg och grundningar."
  ),
  
  "58162" = list(
    name = "Stone Chalk, white",
    kremer_match = "Stone Chalk, white",
    kremer_id = "58162",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/fillers-building-materials/58162-stone-chalk-white.html",
    notes = "Mycket fin stenkrita, ca 4 µ. Finaste kvalitet krita för högkvalitativa applikationer. Lägst oljeabsorption av alla kritor."
  ),
  
  "58900" = list(
    name = "Bentonite",
    kremer_match = "Bentonite",
    kremer_id = "58900",
    kremer_url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58900-bentonite.html",
    notes = "Förtjockningsmedel, särskilt för oljefärg. Mycket hög oljeabsorption (180%). Lera som sväller i kontakt med olja. Färgindex: PW 19.77004. Används sparsamt (1-5%)."
  ),
  
  "58250" = list(
    name = "Kaolin, yellowish",
    kremer_match = "Kaolin, yellowish",
    kremer_id = "58250",
    kremer_url = "https://shop.kremerpigments.com/us/shop/fillers-building-materials/58250-kaolin-yellowish.html",
    notes = "Vit bolus, gulaktig kaolin-lera. Används som fyllmedel och för att öka opacitet. Färgindex: PW 19. Måttlig oljeabsorption (45%)."
  ),
  
  # GREENS
  "40400" = list(
    name = "Viridian PG18",
    kremer_match = "Viridian Green (PG18)",
    kremer_id = "44250",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/44250-viridian-green",
    notes = "Klassisk viridiangrön, kall transparent grön. Främst från internationella leverantörer."
  ),
  
  "41700" = list(
    name = "Malakit naturlig",
    kremer_match = "Malachite Synthetic (PG19)",
    kremer_id = "44400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/44400-malachite-synthetic",
    notes = "Syntetisk malakit som alternativ till naturlig. Speciellt pigment från Kremer."
  ),
  
  "11100" = list(
    name = "Phthalogrön PG7",
    kremer_match = "Phthalo Green Bluish (PG7)",
    kremer_id = "23000",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/23000-phthalo-green-bluish-pg-7",
    notes = "Stark blåaktig phthalogrön med hög färgstyrka. Organiskt pigment."
  ),
  
  "40850" = list(
    name = "Grön jord Böhmen",
    kremer_match = "Green Earth Bohemian (PG23)",
    kremer_id = "40850",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40850-green-earth-bohemian",
    notes = "EXAKT MATCHNING - Samma produktnummer! Böhmisk grön jord från Kremer."
  ),
  
  "40860" = list(
    name = "Grön jord Verona",
    kremer_match = "Green Earth Verona (PG23)",
    kremer_id = "40860",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40860-green-earth-verona",
    notes = "EXAKT MATCHNING - Samma produktnummer! Veronese grön jord från Kremer."
  ),
  
  # BLACKS
  "44450" = list(
    name = "Svartoxid PBk11",
    kremer_match = "Black Iron Oxide (PBk11)",
    kremer_id = "47000",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/47000-black-iron-oxide",
    ottosson_match = "Järnoxidsvart",
    ottosson_url = "https://ottossonfarg.com/produkt/jarnoxidsvart/",
    claessons_match = "Järnoxidsvart 9313",
    claessons_url = "https://claessons.com/svarta/jarnoxidsvart-9313-losvikt/",
    notes = "Järnoxidsvart med hög täckförmåga. Finns hos Kremer, Ottosson och Claessons."
  ),
  
  "47400" = list(
    name = "Spinel Black",
    kremer_match = "Spinel Black",
    kremer_id = "47400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/47400-spinel-black.html",
    notes = "Enda 'sanna' svarta - jämnt icke-reflekterande över hela spektrumet. Djupaste svarta pigmentet tillgängligt (förutom Vanta Black). Järn-mangan spinell (Fe,Mn)₃O₄. Färgindex: PBk 26.77494. Utmärkt ljusäkthet (8/8/8). Värmebeständig >500°C. Mycket fin partikelstorlek (~0.5 µm). Kräver hög oljeabsorption (65-70%). Säker att använda. Premium kvalitet."
  ),
  
  # BLUES
  "11670" = list(
    name = "Phthaloblå PB15:3",
    kremer_match = "Phthalo Blue Royal Blue (PB15:3)",
    kremer_id = "23060",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/23060-phthalo-blue-royal-blue-pb-15-3",
    notes = "Royal blue variant av phthaloblått med hög färgstyrka. Organiskt pigment."
  ),
  
  # EARTH COLORS - TERRA & POZZUOLI
  "40820" = list(
    name = "Terra di Pozzuoli",
    kremer_match = "Terra Pozzuoli (PY43)",
    kremer_id = "41550",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/41550-terra-pozzuoli",
    notes = "Italiensk vulkanisk jord från Pozzuoli. Speciellt pigment från Kremer."
  ),
  
  "40800" = list(
    name = "Terra di Siena natur",
    kremer_match = "Raw Sienna Italian (PBr7)",
    kremer_id = "40400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian",
    notes = "Klassisk obränd sienna från Italien. Naturligt jordpigment."
  ),
  
  "40830" = list(
    name = "Terra di Ercolano",
    kremer_match = "Terra di Ercolano (PBr7)",
    kremer_id = "40835",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40835-terra-di-ercolano",
    notes = "EXAKT MATCHNING - Terra från Herculaneum. Italienskt specialpigment."
  ),
  
  # YELLOWS & OCHRES
  "44082" = list(
    name = "Gul ockra ljus",
    kremer_match = "Yellow Ochre Light (PY43)",
    kremer_id = "40010",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40010-yellow-ochre-light",
    ottosson_match = "Gul ockra",
    ottosson_url = "https://ottossonfarg.com/produkt/gul-ockra/",
    notes = "Ljus gul ockra, ett av de mest använda pigmenten i svensk tradition."
  ),
  
  "44086" = list(
    name = "Gul ockra mörk",
    kremer_match = "Yellow Ochre Dark (PY43)",
    kremer_id = "40030",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40030-yellow-ochre-dark",
    notes = "Mörk gul ockra med högre färgstyrka än ljus variant."
  ),
  
  "44150" = list(
    name = "Naples Yellow light",
    kremer_match = "Naples Yellow Light (PY41/PW4)",
    kremer_id = "43010",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/43010-naples-yellow-light",
    notes = "Ljus neapelgul, blybaserad variant. Historiskt pigment."
  ),
  
  "44160" = list(
    name = "Naples Yellow dark",
    kremer_match = "Naples Yellow Dark (PY41/PW4)",
    kremer_id = "43000",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/43000-naples-yellow-dark",
    notes = "Mörk neapelgul, blybaserad variant. Historiskt pigment."
  ),
  
  # SIENNAS
  "44650" = list(
    name = "Raw Sienna Italien",
    kremer_match = "Raw Sienna Italian (PBr7)",
    kremer_id = "40400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian",
    notes = "Klassisk obränd sienna från Italien. Naturligt jordpigment."
  ),
  
  "44620" = list(
    name = "Burnt Sienna Italien",
    kremer_match = "Burnt Sienna Italian (PR101)",
    kremer_id = "40450",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40450-burnt-sienna-italian",
    ottosson_match = "Järnoxidrött bränd",
    ottosson_url = "https://ottossonfarg.com/produkt/jarnoxidrott-brand/",
    notes = "Bränd sienna med varm rödbrun nyans. Liknande bränt järnoxid."
  ),
  
  # REDS & ORANGES - IRON OXIDES
  "44300" = list(
    name = "Transparent brunoxid",
    kremer_match = "Transparent Brown Oxide (PBr7)",
    kremer_id = "48000",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48000-transparent-brown-oxide",
    notes = "Transparent järnoxid för lasering och transparenta skikt."
  ),
  
  "44200" = list(
    name = "Röd järnoxid transparent",
    kremer_match = "Red Iron Oxide Transparent (PR101)",
    kremer_id = "48100",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48100-red-iron-oxide-transparent",
    notes = "Transparent röd järnoxid för lasering och transparenta skikt."
  ),
  
  "44210" = list(
    name = "Röd järnoxid ljus",
    kremer_match = "Red Iron Oxide Light (PR101)",
    kremer_id = "48200",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48200-red-iron-oxide-light",
    ottosson_match = "Järnoxidrött ljus",
    ottosson_url = "https://ottossonfarg.com/produkt/jarnoxidrott-ljus/",
    claessons_match = "Järnoxidrött 9509",
    claessons_url = "https://claessons.com/roda/jarnoxidrott-9509/",
    notes = "Ljus röd järnoxid med god täckförmåga. Klassisk svensk rödfärgsnyans."
  ),
  
  "44220" = list(
    name = "Röd järnoxid mörk",
    kremer_match = "Red Iron Oxide Dark (PR101)",
    kremer_id = "48300",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48300-red-iron-oxide-dark",
    ottosson_match = "Järnoxidrött mörk",
    ottosson_url = "https://ottossonfarg.com/produkt/jarnoxidrott-mork/",
    notes = "Mörk röd järnoxid med hög färgstyrka och täckförmåga."
  ),
  
  "44510" = list(
    name = "Orange järnoxid",
    kremer_match = "Orange Iron Oxide (PO20)",
    kremer_id = "48500",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48500-orange-iron-oxide",
    notes = "Orange järnoxid för varma toner mellan gult och rött."
  ),
  
  # === RAÄ PIGMENTS ===
  # GREENS
  "KG83" = list(
    name = "Kromoxidgrönt nr GN 83",
    kremer_match = "Chrome Oxide Green (PG17)",
    kremer_id = "44200",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/44200-chrome-oxide-green",
    notes = "Kall grön, opak, samma pigmenttyp (PG17). MYCKET GOD MATCHNING - samma krompigment som RAÄ använder."
  ),
  
  "ZG65" = list(
    name = "Zinkgrönt nr 65",
    kremer_match = "Cobalt Green Dark (PG19) or Cobalt Zinc Silicate",
    kremer_id = "44350",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/44350-cobalt-green-dark",
    notes = "Zinkbaserad grön, närmaste matchning för traditionell zinkgrön"
  ),
  
  "GU30" = list(
    name = "Grön umbra nr 30",
    kremer_match = "Raw Umber, greenish (PBr8)",
    kremer_id = "40630",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40630-raw-umber-greenish",
    notes = "Tysk obränd umbra med grönaktig nyans"
  ),
  
  # BLACKS
  "J318" = list(
    name = "Järnoxidsvart nr 318",
    kremer_match = "Iron Oxide Black 318 (PBk11)",
    kremer_id = "48400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48400-iron-oxide-black-318-high-tinting",
    ottosson_match = "Järnoxidsvart",
    ottosson_url = "https://ottossonfarg.com/produkt/jarnoxidsvart/",
    claessons_match = "Järnoxidsvart CM-5D",
    claessons_url = "https://claessons.com/svarta/jarnoxidsvart-cm-5d-1-kg/",
    notes = "EXAKT MATCHNING - Samma produktnummer 318! Finns hos Kremer, Ottosson och Claessons."
  ),
  
  "BS98" = list(
    name = "Bensvart nr 98",
    kremer_match = "Bone Black (PBk9)",
    kremer_id = "47100",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/47100-bone-black",
    ottosson_match = "Bensvart",
    ottosson_url = "https://ottossonfarg.com/produkt/bensvart/",
    notes = "Traditionell bensvart från ben. MYCKET GOD MATCHNING - finns hos både Kremer och svenska leverantörer."
  ),
  
  # BLUES
  "UB88" = list(
    name = "Ultramarinblått nr 88",
    kremer_match = "Ultramarine Blue, very dark (PB29)",
    kremer_id = "45000",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/45000-ultramarine-blue-very-dark",
    notes = "Djupt ultramarin, matchar mörkt NCS-värde. MYCKET GOD MATCHNING - samma pigment (PB29)."
  ),
  
  "KB28" = list(
    name = "Koboltblått nr 28",
    kremer_match = "Cobalt Blue Medium (PB28)",
    kremer_id = "45710",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/45710-cobalt-blue-medium",
    notes = "Mellannyans koboltblått med violett underton"
  ),
  
  # TERRA & EARTH COLORS
  "BT44" = list(
    name = "Bränd terra nr 44",
    kremer_match = "Burnt Sienna, Italian (PR101)",
    kremer_id = "44620",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40450-burnt-sienna-italian",
    notes = "Bränd röd jord, liknande bränd terra"
  ),
  
  "OT46" = list(
    name = "Obränd terra nr 46",
    kremer_match = "Raw Sienna, Italian (PY43)",
    kremer_id = "40400",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40400-raw-sienna-italian",
    notes = "Naturlig gulbrun jord"
  ),
  
  # YELLOWS & OCHRES
  "J920" = list(
    name = "Järnoxidgult nr 920",
    kremer_match = "Yellow Ochre, dark (PY42/43)",
    kremer_id = "40030",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40030-yellow-ochre-dark",
    notes = "Mörkare gulockra med god mättnad"
  ),
  
  "LO92" = list(
    name = "Ljusockra nr 92",
    kremer_match = "Yellow Ochre, light (PY42/43)",
    kremer_id = "40010",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40010-yellow-ochre-light",
    notes = "Ljus gulockra"
  ),
  
  "GO94" = list(
    name = "Guldockra nr 94",
    kremer_match = "Yellow Ochre Golden, Italian (PY43)",
    kremer_id = "40015",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40015-yellow-ochre-golden-italian",
    ottosson_match = "Guldockra",
    ottosson_url = "https://ottossonfarg.com/produkt/guldockra/",
    notes = "Guldtonad ockra, varmare än ljusockra. Klassiskt svensk pigment."
  ),
  
  "GO94_GU30" = list(
    name = "50% Guldockra + 50% Grön umbra",
    kremer_match = "Mix Yellow Ochre Golden + Raw Umber greenish",
    kremer_id = "40015 + 40630",
    kremer_url = c(
      "https://www.kremer-pigmente.com/en/shop/pigments/40015-yellow-ochre-golden-italian",
      "https://www.kremer-pigmente.com/en/shop/pigments/40630-raw-umber-greenish"
    ),
    notes = "Specialblandning: 50/50 viktprocent av båda pigmenten"
  ),
  
  # UMBERS
  "OU103" = list(
    name = "Obränd umbra nr 103",
    kremer_match = "Raw Umber, Cyprus (PBr8)",
    kremer_id = "40610",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40610-raw-umber",
    notes = "Traditionell cypriotisk obränd umbra, mörkbrun-grön. MYCKET GOD MATCHNING - samma pigment (PBr8)."
  ),
  
  "BU100" = list(
    name = "Bränd umbra nr 100",
    kremer_match = "Burnt Umber, dark brown (PBr7)",
    kremer_id = "40720",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40720-burnt-umber-dark-brown",
    ottosson_match = "Umbra bränd",
    ottosson_url = "https://ottossonfarg.com/produkt/umbra-brand/",
    claessons_match = "Bränd umbra 1783",
    claessons_url = "https://claessons.com/umbra/brand-umbra-1783-25-kg/",
    notes = "Cypriotisk bränd umbra, mycket mörk. Finns hos Kremer, Ottosson och Claessons."
  ),
  
  "BRU39" = list(
    name = "Brun umbra nr 39",
    kremer_match = "Burnt Umber, reddish (PBr7)",
    kremer_id = "40700",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40700-burnt-umber-reddish",
    notes = "Italiensk rödaktig bränd umbra, varmare ton"
  ),
  
  "GRAU36" = list(
    name = "Grå umbra nr 36",
    kremer_match = "Raw Umber, dark + small amount of blue pigment",
    kremer_id = "40660",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/40660-raw-umber-dark",
    notes = "Använd Raw Umber dark; tillsätt en nypa ultramarin för gråton"
  ),
  
  # IRON OXIDE REDS
  "J225" = list(
    name = "Järnoxidrött nr 225",
    kremer_match = "Red Iron Oxide, medium (PR101)",
    kremer_id = "48200",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48200-red-iron-oxide-medium",
    notes = "Mellannyans röd järnoxid"
  ),
  
  "J180M" = list(
    name = "Caput Mortuum 180M",
    kremer_match = "Caput Mortuum Violet (PR101)",
    kremer_id = "48280",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48280-caput-mortuum-violet",
    notes = "Mörkt lila-brunt järnoxid, klassiskt caput mortuum"
  ),
  
  "J120N" = list(
    name = "Järnoxidrött nr 120N",
    kremer_match = "Red Iron Oxide, light (PR101)",
    kremer_id = "48220",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48220-red-iron-oxide-light",
    notes = "Ljusare rött järnoxid"
  ),
  
  "ER48A" = list(
    name = "Engelskt rött nr 48A",
    kremer_match = "English Red (PR101)",
    kremer_id = "42100",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/42100-english-red",
    notes = "Traditionellt engelskt rött, ljust orange-rött järnoxid"
  ),
  
  # IRON OXIDE BROWNS
  "J663" = list(
    name = "Järnoxidbrunt nr 663",
    kremer_match = "Brown Iron Oxide 610 (PBr6/7)",
    kremer_id = "48610",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48610-brown-iron-oxide-610",
    notes = "Syntetiskt brunt järnoxid, mycket mörkt"
  ),
  
  "J686" = list(
    name = "Järnoxidbrunt nr 686",
    kremer_match = "Brown Iron Oxide 686 (PBr6/7)",
    kremer_id = "48686",
    kremer_url = "https://www.kremer-pigmente.com/en/shop/pigments/48686-brown-iron-oxide-686",
    notes = "EXAKT MATCHNING - Samma produktnummer 686, troligen identiskt pigment."
  )
)


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
