# Färglabbet – SLUTGILTIG med NORMALISERING & EXAKT TOTALVIKT
# Totalvikt pigment = EXAKT det du skrev i Steg 1

library(shiny)
library(shinydashboard)
library(shinyjs)

# === PIGMENTDATA ===
km <- list(
  "44100" = list(name = "Zinkvit PW4", oil = 20, K = 0.00, S = 1.66),
  "44107" = list(name = "Zinkvit extra fint", oil = 20, K = 0.00, S = 1.66),
  "44400" = list(name = "Titanvit Rutile PW6", oil = 15, K = 0.00, S = 2.55),
  "44410" = list(name = "Titanvit rutile fint", oil = 15, K = 0.00, S = 2.55),
  "44420" = list(name = "Titanvit rutile medel", oil = 15, K = 0.00, S = 2.55),
  "44430" = list(name = "Titanvit rutile grov", oil = 15, K = 0.00, S = 2.55),
  "44440" = list(name = "Titanvit anatas", oil = 15, K = 0.00, S = 2.40),
  "77510" = list(name = "Titanvit Anatas", oil = 15, K = 0.00, S = 2.40),
  "44200" = list(name = "Röd järnoxid PR101 transparent", oil = 47, K = 0.90, S = 0.12),
  "44207" = list(name = "Röd järnoxid ljus", oil = 47, K = 0.80, S = 0.20),
  "44210" = list(name = "Röd järnoxid medel", oil = 47, K = 0.85, S = 0.25),
  "44220" = list(name = "Röd järnoxid mörk", oil = 47, K = 1.00, S = 0.30),
  "44230" = list(name = "Röd järnoxid djup", oil = 47, K = 1.05, S = 0.35),
  "44240" = list(name = "Röd järnoxid mörkare", oil = 47, K = 1.10, S = 0.40),
  "44250" = list(name = "Röd järnoxid PR101 mörk", oil = 47, K = 1.20, S = 0.90),
  "44300" = list(name = "Brun järnoxid PBr7 transparent", oil = 50, K = 0.80, S = 0.22),
  "44310" = list(name = "Brun järnoxid ljus", oil = 50, K = 0.75, S = 0.25),
  "44320" = list(name = "Brun järnoxid medel", oil = 50, K = 0.78, S = 0.28),
  "44340" = list(name = "Brun järnoxid mörk", oil = 50, K = 0.82, S = 0.30),
  "44350" = list(name = "Brun järnoxid djup", oil = 50, K = 0.85, S = 0.32),
  "44360" = list(name = "Brun järnoxid extra mörk", oil = 50, K = 0.88, S = 0.35),
  "44380" = list(name = "Brun järnoxid svart", oil = 50, K = 0.90, S = 0.38),
  "44450" = list(name = "Svart järnoxid PBk11", oil = 15, K = 2.40, S = 1.10),
  "44510" = list(name = "Orange järnoxid PO73", oil = 47, K = 0.55, S = 0.85),
  "44520" = list(name = "Orange järnoxid ljus", oil = 47, K = 0.50, S = 0.80),
  "44530" = list(name = "Orange järnoxid medel", oil = 47, K = 0.55, S = 0.82),
  "44540" = list(name = "Brunorange", oil = 47, K = 0.70, S = 0.75),
  "44550" = list(name = "Brunorange mörk", oil = 47, K = 0.75, S = 0.78),
  "44560" = list(name = "Brunorange djup", oil = 47, K = 0.80, S = 0.80),
  "44570" = list(name = "Brunorange svart", oil = 47, K = 0.85, S = 0.82),
  "44600" = list(name = "Umbra Cyprus mörk", oil = 50, K = 1.00, S = 0.45),
  "44610" = list(name = "Umbra ljus", oil = 50, K = 0.95, S = 0.48),
  "44620" = list(name = "Sienna bränd", oil = 50, K = 0.75, S = 0.50),
  "40810" = list(name = "Raw Sienna", oil = 45, K = 0.65, S = 0.48),
  "40610" = list(name = "Burnt Umber", oil = 55, K = 1.30, S = 0.60),
  "23000" = list(name = "Ultramarine Blue PB29", oil = 40, K = 1.60, S = 0.85),
  "11670" = list(name = "Phthalo Blue PB15:3", oil = 45, K = 1.80, S = 0.90),
  "40500" = list(name = "Chromoxidgrön PG17", oil = 18, K = 1.10, S = 1.80),
  "40400" = list(name = "Viridian PG18", oil = 45, K = 0.90, S = 0.70),
  "11100" = list(name = "Phthalo Green PG7", oil = 50, K = 2.20, S = 0.95),
  "40850" = list(name = "Grön jord Böhmen", oil = 35, K = 0.60, S = 0.55),
  "40860" = list(name = "Grön jord Verona", oil = 35, K = 0.65, S = 0.60),
  "41700" = list(name = "Malakit naturlig fin", oil = 40, K = 0.75, S = 0.65),
  "41750" = list(name = "Malakit grov", oil = 40, K = 0.80, S = 0.60),
  "40800" = list(name = "Terra di Siena natur", oil = 40, K = 0.60, S = 0.50),
  "40820" = list(name = "Terra di Pozzuoli", oil = 40, K = 0.70, S = 0.55),
  "40830" = list(name = "Terra di Ercolano", oil = 40, K = 0.68, S = 0.53),
  "44150" = list(name = "Naples Yellow light", oil = 35, K = 0.40, S = 0.70),
  "44160" = list(name = "Naples Yellow dark", oil = 35, K = 0.50, S = 0.65),
  "44082" = list(name = "Gul ockra ljus", oil = 20, K = 0.48, S = 0.38),
  "44084" = list(name = "Gul ockra medel", oil = 20, K = 0.52, S = 0.42),
  "44086" = list(name = "Gul ockra mörk", oil = 20, K = 0.55, S = 0.45),
  "44652" = list(name = "Raw Sienna fransk", oil = 45, K = 0.58, S = 0.48),
  "44622" = list(name = "Burnt Sienna mörk", oil = 50, K = 0.80, S = 0.52),
  "44610" = list(name = "Raw Umber ljus", oil = 50, K = 0.90, S = 0.48),
  "44680" = list(name = "Burnt Umber Italien", oil = 55, K = 1.10, S = 0.50),
  "44682" = list(name = "Burnt Umber mörk", oil = 55, K = 1.20, S = 0.48),
  "44310" = list(name = "Brunoxid ljus", oil = 50, K = 0.75, S = 0.25),
  "44350" = list(name = "Brunoxid djup", oil = 50, K = 0.85, S = 0.32)
)

rgb <- list(
  # ── VITA ─────────────────────────────────────────────────────
  "44100" = c(255, 255, 255),  # Zinkvit PW4
  "44107" = c(255, 255, 255),  # Zinkvit extra fint
  "44400" = c(255, 255, 255),  # Titanvit Rutile PW6
  "44410" = c(255, 255, 255),  # Titanvit rutile fint
  "44420" = c(255, 255, 255),  # Titanvit rutile medel
  "44430" = c(255, 255, 255),  # Titanvit rutile grov
  "44440" = c(255, 255, 255),  # Titanvit anatas
  "77510" = c(255, 255, 255),  # Titanvit Anatas
  
  # ── RÖDA JÄRNOXIDER PR101 ───────────────────────────────────
  "44200" = c(178,  34,  34),  # Röd järnoxid PR101 transparent
  "44207" = c(200,  70,  60),  # Röd järnoxid ljus
  "44210" = c(180,  50,  45),  # Röd järnoxid medel
  "44220" = c(160,  35,  35),  # Röd järnoxid mörk
  "44230" = c(145,  30,  30),  # Röd järnoxid djup
  "44240" = c(130,  25,  25),  # Röd järnoxid mörkare
  "44250" = c(110,  15,  15),  # Röd järnoxid PR101 mörk (nästan svart-röd)
  
  # ── BRUNA JÄRNOXIDER PBr7 ───────────────────────────────────
  "44300" = c(139,  69,  19),  # Brun järnoxid transparent
  "44310" = c(170, 100,  60),  # Brunoxid ljus
  "44320" = c(150,  85,  50),  # Brunoxid medel
  "44340" = c(130,  70,  40),  # Brunoxid mörk
  "44350" = c(110,  60,  35),  # Brunoxid djup
  "44360" = c( 95,  50,  30),  # Brunoxid extra mörk
  "44380" = c( 70,  40,  25),  # Brunoxid svart
  
  # ── SVART & ORANGE ───────────────────────────────────────────
  "44450" = c( 28,  38,  38),  # Svart järnoxid PBk11
  "44510" = c(232,  97,   0),  # Orange järnoxid PO73 (klassisk Kremer-orange)
  "44520" = c(240, 130,  40),
  "44530" = c(230, 110,  20),
  "44540" = c(200,  90,  30),  # Brunorange
  "44550" = c(180,  75,  25),
  "44560" = c(160,  65,  20),
  "44570" = c(140,  55,  15),  # Brunorange svart
  
  # ── UMBRA & SIENNA ───────────────────────────────────────────
  "44600" = c( 99,  81,  71),  # Umbra Cyprus mörk
  "44610" = c(120, 100,  85),  # Umbra ljus
  "44620" = c(160,  82,  45),  # Sienna bränd (Burnt Sienna)
  "40810" = c(180, 130,  70),  # Raw Sienna
  "40610" = c( 85,  45,  25),  # Burnt Umber (mycket mörk, rödbrun)
  
  # ── BLÅA ────────────────────────────────────────────────────
  "23000" = c( 30,  50, 130),  # Ultramarinblå PB29
  "11670" = c(  0,  70, 130),  # Phthalo Blue PB15:3
  
  # ── GRÖNA ───────────────────────────────────────────────────
  "40500" = c( 80, 130,  60),  # Chromoxidgrön PG17 (täckande kall grön)
  "40400" = c( 30, 120,  80),  # Viridian PG18 (transparent kall grön)
  "11100" = c(  0, 100,  50),  # Phthalo Green PG7 (stark blågrön)
  "40850" = c( 90, 120,  70),  # Grön jord Böhmen
  "40860" = c(100, 130,  80),  # Grön jord Verona
  "41700" = c( 70, 160, 100),  # Malakit naturlig fin
  "41750" = c( 60, 150,  90),  # Malakit grov
  
  # ── TERRA & RÖDJORD ─────────────────────────────────────────
  "40800" = c(170, 110,  70),  # Terra di Siena natur
  "40820" = c(180,  80,  60),  # Terra di Pozzuoli (varm rödjord)
  "40830" = c(175,  85,  65),  # Terra di Ercolano
  
  # ── NAPLES YELLOW & GULA OCKRA ───────────────────────────────
  "44150" = c(240, 220, 130),  # Naples Yellow light
  "44160" = c(220, 190, 100),  # Naples Yellow dark
  "44082" = c(210, 180, 120),  # Gul ockra ljus
  "44084" = c(180, 140,  90),  # Gul ockra medel
  "44086" = c(160, 120,  70),  # Gul ockra mörk
  
  # ── SIENNA & UMBRA (franska/italienska varianter) ─────────────
  "44652" = c(190, 140,  80),  # Raw Sienna fransk
  "44622" = c(140,  70,  40),  # Burnt Sienna mörk
  "44610" = c(110,  90,  80),  # Raw Umber ljus
  "44680" = c( 90,  50,  30),  # Burnt Umber Italien
  "44682" = c( 80,  45,  25),  # Burnt Umber mörk
  
  # ── ÖVRIGA BRUNOXIDER ───────────────────────────────────────
  "44310" = c(160, 100,  60),  # Brunoxid ljus
  "44350" = c(120,  70,  40)   # Brunoxid djup
)

color_choices <- setNames(
  names(km)[sapply(km, function(x) x$K > 0.01)],
  paste0(sapply(km, `[[`, "name")[sapply(km, function(x) x$K > 0.01)], "  (#", names(km)[sapply(km, function(x) x$K > 0.01)], ")")
)

ui <- dashboardPage(
  dashboardHeader(title = "Färglabbet – Exakt Vikt"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
      .step { padding: 30px; }
      .next-btn { font-size: 20px; padding: 15px 40px; }
      .preview-box { height: 320px; border: 10px solid black; border-radius: 16px; }
      .total-box { font-size: 24px; font-weight: bold; color: #d9534f; }
    "))),
    
    hidden(div(id = "step1",
               h2("Steg 1 – Totalvikt & Vitbas"),
               numericInput("total_weight", "Önskad totalvikt pigment (g)", 300, min = 100, max = 2000, step = 10),
               sliderInput("zinc_ratio", "Zinkvit i vitbas (%)", 0, 100, 60, step = 1),
               br(), br(),
               actionButton("to_step2", "Gå till färgblandning", class = "btn-primary next-btn")
    )),
    
    hidden(div(id = "step2",
               h2("Steg 2 – Blanda färg (live preview)"),
               fluidRow(
                 column(6,
                        h4("Låst: ", textOutput("locked_info", inline = TRUE)),
                        selectInput("p1", "Pigment 1", c("Inget" = "", color_choices), "23000"),
                        conditionalPanel("input.p1 != ''", sliderInput("pct1", "Andel (%)", 1, 100, 70)),
                        selectInput("p2", "Pigment 2", c("Inget" = "", color_choices)),
                        conditionalPanel("input.p2 != ''", sliderInput("pct2", "Andel (%)", 1, 100, 30)),
                        selectInput("p3", "Pigment 3", c("Inget" = "", color_choices)),
                        conditionalPanel("input.p3 != ''", sliderInput("pct3", "Andel (%)", 1, 100, 20)),
                        br(),
                        actionButton("to_step3", "Lås recept & beräkna olja", class = "btn-success next-btn")
                 ),
                 column(6,
                        h3("Live färgprov"),
                        uiOutput("live_preview"),
                        br(),
                        tags$b("Färgkod: "), textOutput("live_hex", inline = TRUE)
                 )
               )
    )),
    
    hidden(div(id = "step3",
               h2("Färdigt recept – exakt vikt"),
               div(class = "total-box", textOutput("exact_total")),
               br(),
               fluidRow(
                 box(width = 8, title = "Recept", status = "danger", solidHeader = TRUE,
                     tableOutput("final_recipe"), br(),
                     downloadButton("download", "Spara recept", class = "btn-block btn-lg")),
                 box(width = 4, title = "Färg", status = "primary", solidHeader = TRUE,
                     uiOutput("final_preview"), br(),
                     tags$b("Färgkod: "), textOutput("final_hex"))
               ),
               br(),
               actionButton("restart", "Börja om", class = "btn-warning btn-lg")
    ))
  )
)

server <- function(input, output, session) {
  observe({ showElement("step1") })
  
  observeEvent(input$to_step2, {
    req(input$total_weight > 0)
    hide("step1"); show("step2")
  })
  
  output$locked_info <- renderText({
    paste0(input$total_weight, " g totalt • ", input$zinc_ratio, "% zinkvit")
  })
  
  # === LIVE PREVIEW ===
  live_color <- reactive({
    total_g <- input$total_weight
    color_pct <- sum(c(input$pct1 %||% 0, input$pct2 %||% 0, input$pct3 %||% 0))
    color_pct <- min(color_pct, 100)
    
    # Färgande pigment (råa vikter)
    raw_weights <- numeric()
    used <- c(input$p1, input$p2, input$p3)[c(input$p1,input$p2,input$p3) != ""]
    for (id in used) {
      pct <- input[[paste0("pct", which(c(input$p1,input$p2,input$p3) == id))]]
      raw_weights[id] <- total_g * color_pct/100 * pct / max(color_pct, 1)
    }
    
    # Vitbas (rå)
    white_g <- total_g * (100 - color_pct)/100
    zn_frac <- input$zinc_ratio / 100
    zn_g <- white_g * zn_frac * km[["44400"]]$S / km[["44100"]]$S
    ti_g <- white_g * (1 - zn_frac)
    
    # Färgprov
    equiv_white <- white_g
    r <- g <- b <- equiv_white * 255
    for (id in names(raw_weights)) {
      col <- rgb[[id]]
      r <- r + raw_weights[id] * col[1]
      g <- g + raw_weights[id] * col[2]
      b <- b + raw_weights[id] * col[3]
    }
    total_eq <- equiv_white + sum(raw_weights)
    hex <- sprintf("#%02X%02X%02X", round(r/total_eq), round(g/total_eq), round(b/total_eq))
    
    list(hex = hex, raw_zn = zn_g, raw_ti = ti_g, raw_color = raw_weights)
  })
  
  output$live_preview <- renderUI({
    tags$div(class = "preview-box", style = paste0("background:", live_color()$hex))
  })
  output$live_hex <- renderText(live_color()$hex)
  
  # === STEG 3 – NORMALISERING & EXAKT VIKT ===
  final_data <- reactiveVal(NULL)
  
  observeEvent(input$to_step3, {
    hide("step2"); show("step3")
    
    total_g <- input$total_weight
    color_pct <- sum(c(input$pct1 %||% 0, input$pct2 %||% 0, input$pct3 %||% 0))
    color_pct <- min(color_pct, 100)
    white_pct <- 100 - color_pct
    
    # Råa vikter (innan normalisering)
    raw_color <- numeric()
    used <- c(input$p1, input$p2, input$p3)[c(input$p1,input$p2,input$p3) != ""]
    for (id in used) {
      pct <- input[[paste0("pct", which(c(input$p1,input$p2,input$p3) == id))]]
      raw_color[id] <- total_g * color_pct/100 * pct / max(color_pct, 1)
    }
    
    raw_white_g <- total_g * white_pct/100
    zn_frac <- input$zinc_ratio / 100
    raw_zn_g <- raw_white_g * zn_frac * km[["44400"]]$S / km[["44100"]]$S
    raw_ti_g <- raw_white_g * (1 - zn_frac)
    
    # Summan av alla råa pigmentvikter
    total_raw <- raw_zn_g + raw_ti_g + sum(raw_color)
    
    # Normaliseringsfaktor så att totalen blir exakt total_g
    norm_factor <- total_g / total_raw
    
    # Normaliserade vikter
    zn_g <- round(raw_zn_g * norm_factor, 1)
    ti_g <- round(raw_ti_g * norm_factor, 1)
    color_g <- round(raw_color * norm_factor, 1)
    
    # Olja
    oil <- zn_g * 0.20 + ti_g * 0.15
    for (id in names(color_g)) oil <- oil + color_g[id] * km[[id]]$oil / 100
    oil_g <- round(oil, 1)
    
    final_data(list(
      zn = zn_g, ti = ti_g, color = color_g, oil = oil_g,
      hex = live_color()$hex,
      total_pigment = zn_g + ti_g + sum(color_g)
    ))
  })
  
  output$exact_total <- renderText({
    req(final_data())
    paste0("Total pigmentvikt: ", final_data()$total_pigment, " g (exakt!)")
  })
  
  output$final_preview <- renderUI({
    req(final_data())
    tags$div(class = "preview-box", style = paste0("background:", final_data()$hex))
  })
  output$final_hex <- renderText(final_data()$hex)
  
  output$final_recipe <- renderTable({
    req(final_data())
    r <- final_data()
    df <- data.frame(Ingrediens = character(), Gram = numeric(), stringsAsFactors = FALSE)
    df <- rbind(df, data.frame(Ingrediens = "Kallpressad linolja", Gram = r$oil))
    if (r$zn > 0.1) df <- rbind(df, data.frame(Ingrediens = "Zinkvit PW4 (#44100)", Gram = r$zn))
    if (r$ti > 0.1) df <- rbind(df, data.frame(Ingrediens = "Titanvit PW6 (#44400)", Gram = r$ti))
    for (id in names(r$color))
      df <- rbind(df, data.frame(Ingrediens = paste0(km[[id]]$name, " (#", id, ")"), Gram = r$color[id]))
    df
  }, digits = 1, striped = TRUE, bordered = TRUE)
  
  output$download <- downloadHandler(
    filename = "mitt_perfekta_recept.txt",
    content = function(file) {
      r <- final_data()
      lines <- c(
        "=== PERFEKT RECEPT ===",
        paste("Total pigmentvikt:", r$total_pigment, "g (exakt)"),
        paste("Zinkvit i bas:", input$zinc_ratio, "%"),
        paste("Färgkod:", r$hex),
        "", "RECEPT:"
      )
      lines <- c(lines, paste("Linölja:", r$oil, "g"))
      if (r$zn > 0.1) lines <- c(lines, paste("Zinkvit PW4:", r$zn, "g"))
      if (r$ti > 0.1) lines <- c(lines, paste("Titanvit PW6:", r$ti, "g"))
      for (id in names(r$color))
        lines <- c(lines, paste(km[[id]]$name, ":", r$color[id], "g"))
      writeLines(lines, file)
    }
  )
  
  observeEvent(input$restart, {
    final_data(NULL)
    hide("step3"); show("step1")
  })
}

shinyApp(ui, server)