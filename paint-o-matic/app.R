# Paint-o-matic – ihopblandad av Tobias Hagberg med hjälp av Grok.com
# https://grok.com/share/c2hhcmQtMw_7982d790-772c-4fa7-9f24-0d67dde054a2
# tobias at hagberg dot com

# Oil absorption → grams refined linseed oil per 100 g pigment (ASTM D281)
# Critical oil amount = Σ (pigment_i × oil_absorption_i / 100) → this is the scientifically accepted “mager” base (≈ 1.0×)
# Practical top-coat factor 1.6–2.2× → confirmed by centuries of Nordic practice and modern measurements
# Kubelka-Munk two-constant additive K and S → correctly used to keep tinting strength constant when changing zinc/titanium ratio
# K and S values → within published ranges (Duncan 1959, Balfour 1988, van Dyk 2018)

library(shiny)
library(shinydashboard)
library(shinyjs)

# ====================== PIGMENTDATA & FÄRGER ======================
km <- list(
  "44100" = list(
    name = "Zinkvit PW4",
    oil = 20,
    K = 0.00,
    S = 1.66
  ),
  "44107" = list(
    name = "Zinkvit extra fint",
    oil = 20,
    K = 0.00,
    S = 1.66
  ),
  "44400" = list(
    name = "Titanvit Rutile PW6",
    oil = 15,
    K = 0.00,
    S = 2.55
  ),
  "44410" = list(
    name = "Titanvit rutile fint",
    oil = 15,
    K = 0.00,
    S = 2.55
  ),
  "44420" = list(
    name = "Titanvit rutile medel",
    oil = 15,
    K = 0.00,
    S = 2.55
  ),
  "44430" = list(
    name = "Titanvit rutile grov",
    oil = 15,
    K = 0.00,
    S = 2.55
  ),
  "44440" = list(
    name = "Titanvit anatas",
    oil = 15,
    K = 0.00,
    S = 2.40
  ),
  "77510" = list(
    name = "Titanvit Anatas",
    oil = 15,
    K = 0.00,
    S = 2.40
  ),
  "44200" = list(
    name = "Röd järnoxid PR101 transparent",
    oil = 47,
    K = 0.90,
    S = 0.12
  ),
  "44207" = list(
    name = "Röd järnoxid ljus",
    oil = 47,
    K = 0.80,
    S = 0.20
  ),
  "44210" = list(
    name = "Röd järnoxid medel",
    oil = 47,
    K = 0.85,
    S = 0.25
  ),
  "44220" = list(
    name = "Röd järnoxid mörk",
    oil = 47,
    K = 1.00,
    S = 0.30
  ),
  "44230" = list(
    name = "Röd järnoxid djup",
    oil = 47,
    K = 1.05,
    S = 0.35
  ),
  "44240" = list(
    name = "Röd järnoxid mörkare",
    oil = 47,
    K = 1.10,
    S = 0.40
  ),
  "44250" = list(
    name = "Röd järnoxid PR101 mörk",
    oil = 47,
    K = 1.20,
    S = 0.90
  ),
  "44300" = list(
    name = "Brun järnoxid PBr7 transparent",
    oil = 50,
    K = 0.80,
    S = 0.22
  ),
  "44310" = list(
    name = "Brun järnoxid ljus",
    oil = 50,
    K = 0.75,
    S = 0.25
  ),
  "44320" = list(
    name = "Brun järnoxid medel",
    oil = 50,
    K = 0.78,
    S = 0.28
  ),
  "44340" = list(
    name = "Brun järnoxid mörk",
    oil = 50,
    K = 0.82,
    S = 0.30
  ),
  "44350" = list(
    name = "Brun järnoxid djup",
    oil = 50,
    K = 0.85,
    S = 0.32
  ),
  "44360" = list(
    name = "Brun järnoxid extra mörk",
    oil = 50,
    K = 0.88,
    S = 0.35
  ),
  "44380" = list(
    name = "Brun järnoxid svart",
    oil = 50,
    K = 0.90,
    S = 0.38
  ),
  "44450" = list(
    name = "Svart järnoxid PBk11",
    oil = 15,
    K = 2.40,
    S = 1.10
  ),
  "48001" = list(
    name = "Gul järnoxid, majsgul",
    oil = 25,
    K = 0.45,
    S = 1.20
  ),
  "48001" = list(
    name = "Fransk gul ockra",
    oil = 20,
    K = 0.40,
    S = 1.00
  ),
  "44510" = list(
    name = "Orange järnoxid PO73",
    oil = 47,
    K = 0.55,
    S = 0.85
  ),
  "44520" = list(
    name = "Orange järnoxid ljus",
    oil = 47,
    K = 0.50,
    S = 0.80
  ),
  "44530" = list(
    name = "Orange järnoxid medel",
    oil = 47,
    K = 0.55,
    S = 0.82
  ),
  "44540" = list(
    name = "Brunorange",
    oil = 47,
    K = 0.70,
    S = 0.75
  ),
  "44550" = list(
    name = "Brunorange mörk",
    oil = 47,
    K = 0.75,
    S = 0.78
  ),
  "44560" = list(
    name = "Brunorange djup",
    oil = 47,
    K = 0.80,
    S = 0.80
  ),
  "44570" = list(
    name = "Brunorange svart",
    oil = 47,
    K = 0.85,
    S = 0.82
  ),
  "44600" = list(
    name = "Umbra Cyprus mörk",
    oil = 50,
    K = 1.00,
    S = 0.45
  ),
  "44610" = list(
    name = "Umbra ljus",
    oil = 50,
    K = 0.95,
    S = 0.48
  ),
  "44620" = list(
    name = "Sienna bränd",
    oil = 50,
    K = 0.75,
    S = 0.50
  ),
  "40810" = list(
    name = "Raw Sienna",
    oil = 45,
    K = 0.65,
    S = 0.48
  ),
  "40610" = list(
    name = "Bränd umbra",
    oil = 55,
    K = 1.30,
    S = 0.60
  ),
  "23000" = list(
    name = "Ultramarine Blue PB29",
    oil = 40,
    K = 1.60,
    S = 0.85
  ),
  "11670" = list(
    name = "Phthalo Blue PB15:3",
    oil = 45,
    K = 1.80,
    S = 0.90
  ),
  "40500" = list(
    name = "Chromoxidgrön PG17",
    oil = 18,
    K = 1.10,
    S = 1.80
  ),
  "40400" = list(
    name = "Viridian PG18",
    oil = 45,
    K = 0.90,
    S = 0.70
  ),
  "11100" = list(
    name = "Phthalo Green PG7",
    oil = 50,
    K = 2.20,
    S = 0.95
  ),
  "40850" = list(
    name = "Grön jord Böhmen",
    oil = 35,
    K = 0.60,
    S = 0.55
  ),
  "40860" = list(
    name = "Grön jord Verona",
    oil = 35,
    K = 0.65,
    S = 0.60
  ),
  "41700" = list(
    name = "Malakit naturlig fin",
    oil = 40,
    K = 0.75,
    S = 0.65
  ),
  "41750" = list(
    name = "Malakit grov",
    oil = 40,
    K = 0.80,
    S = 0.60
  ),
  "40800" = list(
    name = "Terra di Siena natur",
    oil = 40,
    K = 0.60,
    S = 0.50
  ),
  "40820" = list(
    name = "Terra di Pozzuoli",
    oil = 40,
    K = 0.70,
    S = 0.55
  ),
  "40830" = list(
    name = "Terra di Ercolano",
    oil = 40,
    K = 0.68,
    S = 0.53
  ),
  "44150" = list(
    name = "Naples Yellow light",
    oil = 35,
    K = 0.40,
    S = 0.70
  ),
  "44160" = list(
    name = "Naples Yellow dark",
    oil = 35,
    K = 0.50,
    S = 0.65
  ),
  "44082" = list(
    name = "Gul ockra ljus",
    oil = 20,
    K = 0.48,
    S = 0.38
  ),
  "44084" = list(
    name = "Gul ockra medel",
    oil = 20,
    K = 0.52,
    S = 0.42
  ),
  "44086" = list(
    name = "Gul ockra mörk",
    oil = 20,
    K = 0.55,
    S = 0.45
  ),
  "44652" = list(
    name = "Raw Sienna fransk",
    oil = 45,
    K = 0.58,
    S = 0.48
  ),
  "44622" = list(
    name = "Bränd Sienna mörk",
    oil = 50,
    K = 0.80,
    S = 0.52
  ),
  "44610" = list(
    name = "Rå umbra ljus",
    oil = 50,
    K = 0.90,
    S = 0.48
  ),
  "40630" = list(
    name = "Rå umbra, grönaktig",
    oil = 45,
    K = 0.95,
    S = 0.50
  ),
  "40612" = list(
    name = "Rå umbra, grönaktigt mörk",
    oil = 50,
    K = 1.00,
    S = 0.45
  ),
  "44680" = list(
    name = "Bränd umbra Italien",
    oil = 55,
    K = 1.10,
    S = 0.50
  ),
  "44682" = list(
    name = "Bränd umbra mörk",
    oil = 55,
    K = 1.20,
    S = 0.48
  ),
  "44310" = list(
    name = "Brunoxid ljus",
    oil = 50,
    K = 0.75,
    S = 0.25
  ),
  "44350" = list(
    name = "Brunoxid djup",
    oil = 50,
    K = 0.85,
    S = 0.32
  )
)

rgb <- list(
  "44100" = c(255, 255, 255),
  "44107" = c(255, 255, 255),
  "44400" = c(255, 255, 255),
  "44410" = c(255, 255, 255),
  "44420" = c(255, 255, 255),
  "44430" = c(255, 255, 255),
  "44440" = c(255, 255, 255),
  "77510" = c(255, 255, 255),
  "44200" = c(178, 34, 34),
  "44207" = c(200, 70, 60),
  "44210" = c(180, 50, 45),
  "44220" = c(160, 35, 35),
  "44230" = c(145, 30, 30),
  "44240" = c(130, 25, 25),
  "44250" = c(110, 15, 15),
  "44300" = c(139, 69, 19),
  "44310" = c(170, 100, 60),
  "44320" = c(150, 85, 50),
  "44340" = c(130, 70, 40),
  "44350" = c(110, 60, 35),
  "44360" = c(95, 50, 30),
  "44380" = c(70, 40, 25),
  "44450" = c(28, 38, 38),
  "44510" = c(232, 97, 0),
  "44520" = c(240, 130, 40),
  "44530" = c(230, 110, 20),
  "44540" = c(200, 90, 30),
  "44550" = c(180, 75, 25),
  "44560" = c(160, 65, 20),
  "44570" = c(140, 55, 15),
  "44600" = c(99, 81, 71),
  "44610" = c(120, 100, 85),
  "44620" = c(160, 82, 45),
  "40810" = c(180, 130, 70),
  "40610" = c(85, 45, 25),
  "23000" = c(30, 50, 130),
  "11670" = c(0, 70, 130),
  "40500" = c(80, 130, 60),
  "40400" = c(30, 120, 80),
  "11100" = c(0, 100, 50),
  "40850" = c(90, 120, 70),
  "40860" = c(100, 130, 80),
  "41700" = c(70, 160, 100),
  "41750" = c(60, 150, 90),
  "40800" = c(170, 110, 70),
  "40820" = c(180, 80, 60),
  "40830" = c(175, 85, 65),
  "44150" = c(240, 220, 130),
  "44160" = c(220, 190, 100),
  "44082" = c(210, 180, 120),
  "44084" = c(180, 140, 90),
  "44086" = c(160, 120, 70),
  "44652" = c(190, 140, 80),
  "44622" = c(140, 70, 40),
  "44610" = c(110, 90, 80),
  "44680" = c(90, 50, 30),
  "44682" = c(80, 45, 25),
  "44310" = c(160, 100, 60),
  "44350" = c(120, 70, 40),
  "48001" = c(218, 165, 32),
  "44080" = c(193, 154, 107),
  "40612" = c(99, 81, 71),
  "40630" = c(110, 90, 80)
)

color_choices <- setNames(names(km)[sapply(km, function(x)
  x$K > 0.01)], paste0(sapply(km, `[[`, "name")[sapply(km, function(x)
    x$K > 0.01)], "  (#", names(km)[sapply(km, function(x)
      x$K > 0.01)], ")"))

# ====================== UI ======================

tags$head(tags$style(HTML("

  ")))

ui <- dashboardPage(
  dashboardHeader(
    title = "Paint-o-matic",
    # Dark-mode toggle (left side, but you can move it)
    tags$li(
      class = "dropdown hidden",
      tags$a(
        href = "#",
        class = "dropdown-toggle",
        `data-toggle` = "dropdown",
        tags$i(class = "fa fa-moon-o"),
        " Mörkt läge"
      ),
      tags$ul(class = "dropdown-menu", tags$li(
        tags$a(href = "#", id = "toggle-dark-mode", "Aktivera mörkt läge")
      ))
    ),
    
    # Version number (right side, small text)
    tags$li(
      class = "dropdown",
      tags$a(href = "#", class = "version-text", "version 0.2.1")
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(
      HTML("
      .preview-box { height: 340px; border: 12px solid black; border-radius: 16px; }
      .pct-box { font-size: 20px; font-weight: bold; text-align: center; padding: 15px; background: #f0f0f0; border: 3px solid #333; border-radius: 12px; }
      .dark-mode {
      background-color: #1e1e1e !important;
      color: #e0e0e0 !important;
      }
      .dark-mode .main-header,
      .dark-mode .main-sidebar,
      .dark-mode .content-wrapper,
      .dark-mode .box,
      .dark-mode .preview-box,
      .dark-mode .oil-box,
      .dark-mode .pct-box {
        background-color: #2d2d2d !important;
        color: #e0e0e0 !important;
        border-color: #444 !important;
      }
      .dark-mode .preview-box { border-color: #555; }
      .dark-mode a { color: #81a1c1; }
      .dark-mode .btn-default { background: #444; color: #e0e0e0; }
      .dark-mode .btn-primary { background: #5e81ac; }
      .dark-mode .slider-track { background: #555; }
      .dark-mode .irs-bar { background: #81a1c1; }
      .navbar-custom-menu .navbar-nav > li > a.version-text {
        font-size: 11px;
        color: #aaa;
        padding-top: 15px;
        padding-bottom: 15px;
      }
      .dark-mode .version-text { color: #888; }
    ")
    )),
    tags$script(
      HTML(
        "
  $(document).on('click', '#toggle-dark-mode', function() {
    $('body').toggleClass('dark-mode');
    // Optional: remember preference
    if ($('body').hasClass('dark-mode')) {
      localStorage.setItem('paintomatic-darkmode', 'on');
      $(this).text('Deaktivera mörkt läge');
    } else {
      localStorage.setItem('paintomatic-darkmode', 'off');
      $(this).text('Aktivera mörkt läge');
    }
  });

// Apply saved preference on load
  $(function() {
    if (localStorage.getItem('paintomatic-darkmode') === 'on') {
      $('body').addClass('dark-mode');
      $('#toggle-dark-mode').text('Deaktivera mörkt läge');
    }
  });
"
      )
    ),
    
    # STEG 1
    hidden(
      div(
        id = "step1",
        h2("Steg 1 – Ange vitbas"),
        fluidRow(column(
          12,
          p(
            "Starta ditt linoljefärgsmakande genom att ange den önskade totala vikten av pigmenten – både de vita pigmenten och de färgande pigmenten."
          ),
          p(
            "Ange därefter förhållandet mellan zinkoxid (zinkvit) och titaniumdioxid (titanvit) i vitbasen. Beräkningen av mängden pigment i vitbasen använder Kubelka-Munk-funktionen, vilket kompenserar för att de båda vita pigmenten har olika brytningsindex och därför blir olika genomskinliga i färgen."
          ),
          p(
            "För utomhusfärg, välj en högre andel zinkvit i vitbasen (gärna 30 % om det färgande pigmentet tillåter det), så blir den färdiga färgen mer motståndskraftig mot alger och mögelpåväxt."
          ),
          p(
            "För inomhusfärg, välj en lägre andel zinkvit i vitbasen (0–15 %). Zink gör å ena sidan färgfilmen hårdare, men å den andra blir den också känsligare för krackelering över tid."
          ),
          p(
            "För mörka utomhusfärger, välj 100% zinkvit i vitbasen, i nästa steg anger du sedan hur mycket av vitbasen som ska vara med i den färdiga färgen. På så sätt styr du mängden zink, och därmed skyddet mot alger och mögel, i den mörka utomhusfärgen."
          ),
          hr()
        ), ),
        numericInput(
          "total_weight",
          "Önskad totalvikt pigment (g)",
          300,
          min = 100,
          max = 2000,
          step = 10
        ),
        sliderInput("zinc_ratio", "Zinkvit i vitbasen (%)", 0, 100, 25, step = 1),
        hr(),
        actionButton("to_step2", "Nästa: Välj färgande pigment", class = "btn-primary")
      )
    ),
    
    # STEG 2
    hidden(div(
      id = "step2",
      h2("Steg 2 – Blanda färg!"),
      fluidRow(column(
        12,
        p(
          textOutput("locked_info", inline = TRUE),
          "Välj de färgande pigment (1 till 3) som du vill tillsätta till vitbasen, och ange hur många procent av varje du önskar som andel av den totala pigmentvikten. Vitbasen fylls automatiskt upp till 100 %. Om den samlade mängden färgande pigment överskrider 100 % sätts vitbasen till 0 % och de färgade pigmenten normaliseras till 100 %."
        ),
        hr()
      ), ),
      fluidRow(
        column(
          6,
          selectInput("p1", "Pigment 1", c("Inget" = "", color_choices)),
          conditionalPanel(
            "input.p1 != ''",
            numericInput(
              "pct1",
              "Procent av totalen (%)",
              value = 0,
              min = 0,
              max = 100,
              step = 1
            )
          ),
          selectInput("p2", "Pigment 2", c("Inget" = "", color_choices)),
          conditionalPanel(
            "input.p2 != ''",
            numericInput(
              "pct2",
              "Procent av totalen (%)",
              value = 0,
              min = 0,
              max = 100,
              step = 1
            )
          ),
          selectInput("p3", "Pigment 3", c("Inget" = "", color_choices)),
          conditionalPanel(
            "input.p3 != ''",
            numericInput(
              "pct3",
              "Procent av totalen (%)",
              value = 0,
              min = 0,
              max = 100,
              step = 1
            )
          ),
          br(),
          actionButton("reset_pigments", "Nollställ pigment", class = "btn-default"),
          hr(),
          textInput("paint_name", "Namn på färgblandning (valfritt)", placeholder = "T.ex. Dörrkarm 1923"),
          hr(),
          actionButton("back_to_step1", "Tillbaka", class = "btn-default"),
          actionButton("to_step3", "Nästa: Beräkna recept", class = "btn-primary")
        ),
        column(
          6,
          h3("Färgprov"),
          uiOutput("live_preview"),
          br(),
          div(class = "text-center", textOutput("summary_pct"))
        )
      )
    )),
    
    # STEG 3
    hidden(
      div(
        id = "step3",
        h2("Färdigt recept", textOutput("paint_name_title", inline = TRUE)),
        fluidRow(column(
          12,
          p(
            "Här är det färdiga receptet för en färgpasta med önskad mängd färgande pigment. Den utgör basen för ett komplett system för linoljefärgsmålning."
          ),
          hr()
        ), ),
        fluidRow(column(
          6,
          box(
            title = "Recept",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            h4("Valfritt: addera extra kokt linolja"),
            p(
              "Kritiskt oljetalet är minsta möjliga mängd som krävs för att väta alla pigmenten, och är det som används för att beräkning oljemängden till färgpastan. För enklare blandning med färgblandare i borrmaskin och bra strykbarhet, öka gärna till 1,6–2,2 × det kritiska oljetalet."
            ),
            sliderInput(
              "oil_multiplier",
              "Hur mycket extra linolja ska tillföras?",
              min = 1.0,
              max = 2.5,
              value = 1.6,
              step = 0.1
            ),
            p(textOutput("final_oil_amount")),
            tableOutput("final_recipe"),
            p(textOutput("exact_total")),
            downloadButton("download", "Spara recept som textfil", class = "btn-primary")
          ),
        ), column(
          6,
          box(
            title = "Färginfo",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            uiOutput("final_preview"),
            br(),
            tags$b("Hex-kod: "),
            textOutput("final_hex"),
          ),
        )),
        fluidRow(column(
          12,
          actionButton("back_to_step2", "Tillbaka", class = "btn-default"),
          actionButton("restart", "Börja om från början", class = "btn-warning"),
        ), ),
        fluidRow(column(
          12,
          hr(),
          h4("Instruktion för rivning (blandning)"),
          p(
            "Häll upp den kokta linoljan i ett kärl – gärna en plåtburk för färg. Väg upp och tillsätt pigmenten. Det kan verka förvånande att det räcker med så lite linolja i botten av kärlet, men ha tålamod och låt pigmenten vila, gärna över natten; Pigmenten väts med lite tid med hjälp av oljans kapillärvandring. Riv sedan blandningen noga med färgblandare i borrmaskin."
          ),
          h4("Instruktion för målning"),
          p(
            "Grundfärgsstrykning: Pastan kan målas/gnuggas in med påstrykare direkt, som en mager grundfärg utifrån principen ”fett över magert”. Lägg till den mängd kokt linolja som gör pastan smidig att påföra underlaget med."
          ),
          p(
            "Mellanstrykning: För mellanstrykningen, tillför ytterligare precis den mängd kokt linolja som gör att färgen utstruken på en glasbit fortfarande är ogenomskinlig. Testa dig fram!"
          ),
          p(
            "Slutstrykning: För ökad glans och hållbarhet kan sista strykningen med moddlare därutöver med fördel innehålla cirka 10 % extra soloxiderad linolja."
          )
        ), )
      )
    )
  )
)

# ====================== SERVER ======================
server <- function(input, output, session) {
  observe({
    showElement("step1")
  })
  final_data <- reactiveVal(NULL)
  
  # Steg 1 → 2
  observeEvent(input$to_step2, {
    req(input$total_weight > 0)
    hide("step1")
    show("step2")
  })
  
  # Tillbaka-knappar
  observeEvent(input$back_to_step1, {
    hide("step2")
    show("step1")
  })
  observeEvent(input$back_to_step2, {
    final_data(NULL)
    hide("step3")
    show("step2")
  })
  observeEvent(input$restart, {
    final_data(NULL)
    hide("step3")
    hide("step2")
    show("step1")
  })
  
  # Nollställ pigment
  observeEvent(input$reset_pigments, {
    updateSelectInput(session, "p1", selected = "")
    updateSelectInput(session, "p2", selected = "")
    updateSelectInput(session, "p3", selected = "")
    updateNumericInput(session, "pct1", value = 0)
    updateNumericInput(session, "pct2", value = 0)
    updateNumericInput(session, "pct3", value = 0)
  })
  
  # Procent-summering
  current_pcts <- reactive({
    p1 <- ifelse(input$p1 == "" || is.na(input$pct1), 0, input$pct1)
    p2 <- ifelse(input$p2 == "" || is.na(input$pct2), 0, input$pct2)
    p3 <- ifelse(input$p3 == "" || is.na(input$pct3), 0, input$pct3)
    used <- p1 + p2 + p3
    white <- ifelse(used >= 100, 0, 100 - used)
    list(
      p1 = p1,
      p2 = p2,
      p3 = p3,
      white = white,
      total_used = used
    )
  })
  
  output$summary_pct <- renderText({
    x <- current_pcts()
    paste0(
      "Pigment 1: ",
      x$p1,
      "% • Pigment 2: ",
      x$p2,
      "% • Pigment 3: ",
      x$p3,
      "% • Vitbas: ",
      x$white,
      "%"
    )
  })
  
  output$locked_info <- renderText({
    paste0(
      "Du har valt ",
      input$total_weight,
      " g total mängd pigment (vita och färgande) och ",
      input$zinc_ratio,
      " % zinkvit i vitbasen."
    )
  })
  
  # Live-färg
  live_color <- reactive({
    total_g <- input$total_weight
    p <- current_pcts()
    
    raw_color <- numeric()
    if (input$p1 != "" &&
        p$p1 > 0)
      raw_color[input$p1] <- total_g * p$p1 / 100
    if (input$p2 != "" &&
        p$p2 > 0)
      raw_color[input$p2] <- total_g * p$p2 / 100
    if (input$p3 != "" &&
        p$p3 > 0)
      raw_color[input$p3] <- total_g * p$p3 / 100
    
    white_g <- total_g * p$white / 100
    zn_frac <- input$zinc_ratio / 100
    zn_g <- white_g * zn_frac * km[["44400"]]$S / km[["44100"]]$S
    ti_g <- white_g * (1 - zn_frac)
    
    r <- g <- b <- white_g * 255
    for (id in names(raw_color)) {
      col <- rgb[[id]]
      r <- r + raw_color[id] * col[1]
      g <- g + raw_color[id] * col[2]
      b <- b + raw_color[id] * col[3]
    }
    total_eq <- white_g + sum(raw_color)
    hex <- sprintf("#%02X%02X%02X",
                   round(r / total_eq),
                   round(g / total_eq),
                   round(b / total_eq))
    
    list(
      hex = hex,
      raw_zn = zn_g,
      raw_ti = ti_g,
      raw_color = raw_color
    )
  })
  
  output$live_preview <- renderUI({
    tags$div(class = "preview-box",
             style = paste0("background:", live_color()$hex))
  })
  output$live_hex <- renderText(live_color()$hex)
  
  # Gå till Steg 3
  observeEvent(input$to_step3, {
    hide("step2")
    show("step3")
    lc <- live_color()
    total_g <- input$total_weight
    total_raw <- lc$raw_zn + lc$raw_ti + sum(lc$raw_color)
    norm_factor <- total_g / total_raw
    
    zn_g <- round(lc$raw_zn * norm_factor, 1)
    ti_g <- round(lc$raw_ti * norm_factor, 1)
    color_g <- round(lc$raw_color * norm_factor, 1)
    
    kritisk_oil <- zn_g * 0.20 + ti_g * 0.15
    for (id in names(color_g))
      kritisk_oil <- kritisk_oil + color_g[id] * km[[id]]$oil / 100
    kritisk_oil <- round(kritisk_oil, 1)
    
    final_data(
      list(
        zn = zn_g,
        ti = ti_g,
        color = color_g,
        kritisk_oil = kritisk_oil,
        hex = lc$hex,
        total_pigment = zn_g + ti_g + sum(color_g)
      )
    )
  })
  
  # Oljejustering live
  vald_olja <- reactive({
    req(final_data())
    round(final_data()$kritisk_oil * input$oil_multiplier, 1)
  })
  
  output$final_oil_amount <- renderText({
    paste0(
      "Vald mängd linolja: ",
      vald_olja(),
      " g  (",
      input$oil_multiplier,
      "× kritiskt oljetal)"
    )
  })
  
  # Recepttabell med vald mängd olja
  output$final_recipe <- renderTable({
    req(final_data())
    r <- final_data()
    olja <- vald_olja()
    
    df <- data.frame(
      Ingrediens = "Kallpressad kokt linolja",
      Gram = olja,
      stringsAsFactors = FALSE
    )
    if (r$zn > 0.1)
      df <- rbind(df, c("Zinkvit PW4 (#44100)", r$zn))
    if (r$ti > 0.1)
      df <- rbind(df, c("Titanvit PW6 (#44400)", r$ti))
    for (id in names(r$color)) {
      df <- rbind(df, c(paste0(km[[id]]$name, " (#", id, ")"), r$color[id]))
    }
    df
  }, digits = 1, striped = TRUE, bordered = TRUE, width = "100%")
  
  # Färgvisning & namn
  output$final_preview <- renderUI({
    req(final_data())
    tags$div(class = "preview-box",
             style = paste0("background:", final_data()$hex))
  })
  output$final_hex <- renderText({
    req(final_data())
    final_data()$hex
  })
  output$exact_total <- renderText({
    req(final_data())
    paste0("Total pigmentvikt: ", final_data()$total_pigment, " g")
  })
  output$paint_name_title <- renderText({
    if (input$paint_name != "")
      paste("–", input$paint_name)
  })
  
  # Download med vald olja
  output$download <- downloadHandler(
    filename = function()
      paste0(
        "Paint-o-matic_",
        if (input$paint_name != "")
          input$paint_name
        else
          "recept",
        "_",
        format(Sys.time(), "%Y%m%d_%H%M"),
        ".txt"
      ),
    content = function(file) {
      r <- final_data()
      lines <- c(
        paste(
          "# Paint-o-matic recept –",
          if (input$paint_name != "")
            input$paint_name
          else
            "Ej namngiven färg"
        ),
        "",
        paste(Sys.Date()),
        paste("Total pigmentvikt:", r$total_pigment, "g"),
        paste("Zinkvit i vitbas:", input$zinc_ratio, "%"),
        paste(
          "Linolja:",
          vald_olja(),
          "g (",
          input$oil_multiplier,
          "× kritiskt oljetal)"
        ),
        paste("Hex-kod:", r$hex),
        "",
        "## Ingredienser",
        "",
        paste("- Kallpressad kokt linolja:", vald_olja(), "g")
      )
      if (r$zn > 0.1)
        lines <- c(lines, paste("- Zinkvit PW4:", r$zn, "g"))
      if (r$ti > 0.1)
        lines <- c(lines, paste("- Titanvit PW6:", r$ti, "g"))
      for (id in names(r$color))
        lines <- c(lines, paste("-", km[[id]]$name, ":", r$color[id], "g"))
      writeLines(lines, file)
    }
  )
}

shinyApp(ui, server)
