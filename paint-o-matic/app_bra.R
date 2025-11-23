# Färglabbet – SLUTGILTIG med NORMALISERING & EXAKT TOTALVIKT + TILLBAKA-KNAPPAR
# Totalvikt pigment = EXAKT det du skrev i Steg 1

library(shiny)
library(shinydashboard)
library(shinyjs)

# === PIGMENTDATA === (oförändrad)
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
  # (samma rgb-lista som innan – oförändrad)
  "44100" = c(255,255,255), "44107" = c(255,255,255), "44400" = c(255,255,255),
  "44410" = c(255,255,255), "44420" = c(255,255,255), "44430" = c(255,255,255),
  "44440" = c(255,255,255), "77510" = c(255,255,255),
  "44200" = c(178,34,34), "44207" = c(200,70,60), "44210" = c(180,50,45),
  "44220" = c(160,35,35), "44230" = c(145,30,30), "44240" = c(130,25,25), "44250" = c(110,15,15),
  "44300" = c(139,69,19), "44310" = c(170,100,60), "44320" = c(150,85,50),
  "44340" = c(130,70,40), "44350" = c(110,60,35), "44360" = c(95,50,30), "44380" = c(70,40,25),
  "44450" = c(28,38,38),
  "44510" = c(232,97,0), "44520" = c(240,130,40), "44530" = c(230,110,20),
  "44540" = c(200,90,30), "44550" = c(180,75,25), "44560" = c(160,65,20), "44570" = c(140,55,15),
  "44600" = c(99,81,71), "44610" = c(120,100,85), "44620" = c(160,82,45),
  "40810" = c(180,130,70), "40610" = c(85,45,25),
  "23000" = c(30,50,130), "11670" = c(0,70,130),
  "40500" = c(80,130,60), "40400" = c(30,120,80), "11100" = c(0,100,50),
  "40850" = c(90,120,70), "40860" = c(100,130,80),
  "41700" = c(70,160,100), "41750" = c(60,150,90),
  "40800" = c(170,110,70), "40820" = c(180,80,60), "40830" = c(175,85,65),
  "44150" = c(240,220,130), "44160" = c(220,190,100),
  "44082" = c(210,180,120), "44084" = c(180,140,90), "44086" = c(160,120,70),
  "44652" = c(190,140,80), "44622" = c(140,70,40),
  "44610" = c(110,90,80), "44680" = c(90,50,30), "44682" = c(80,45,25),
  "44310" = c(160,100,60), "44350" = c(120,70,40)
)

color_choices <- setNames(
  names(km)[sapply(km, function(x) x$K > 0.01)],
  paste0(sapply(km, `[[`, "name")[sapply(km, function(x) x$K > 0.01)], "  (#", names(km)[sapply(km, function(x) x$K > 0.01)], ")")
)

ui <- dashboardPage(
  dashboardHeader(title = "Paint-o-matic"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
    #   .step { padding: 30px; }
    #   .next-btn { font-size: 20px; padding: 15px 40px; }
    #   .back-btn { font-size: 18px; padding: 12px 30px; margin-right: 15px; }
    #   .reset-btn { font-size: 18px; padding: 12px 25px; background:#d9534f !important; border-color:#d9534f !important; }
      .preview-box { height: 320px; border: 10px solid black; border-radius: 16px; }
    #   .total-box { font-size: 24px; font-weight: bold; color: #d9534f; }
    "))),
    
    hidden(div(id = "step1",
               h2("Steg 1 – Ange vitbas"),
               p("Ange den totala vikten av pigment som du önskar. Till detta tillkommer sedan den mängd linolja som pigmenten kräver för att varje pigmentkorn till hundra procent omsluts av olja, den mängden visas i sista steget."),
               p("Ange därefter förhållandet mellan zinkoxid (zinkvit) och titaniumdioxid (titanvit), vilka tillsammans utgör den så kallade vitbasen. Beräkningen av mängden pigment i vitbasen kompenseras enligt Kubelka-Munk-funktionen, för bibehållen styrka av färgande pigment oavsett förhållande mellan zinkoxid och titandioxid i vitbasen."),
               p("Du kan i nästa steg ange vilka färgande pigment du vill använda med vitbasen, liksom förhållandet mellan vitbas och färgande pigment."),
               p("För utomhusfärg, välj en högre andel zinkvit i vitbasen (gärna 30 % om det färgande pigmentet tillåter det), så blir den färdiga färgen mer motståndskraftig mot alger och mögelpåväxt."),
               p("För inomhusfärg, välj en lägre andel zinkvit i vitbasen (zink gör färgfilmen hårdare, men också ömtåligare över tid, välj 0 till 15 %)."),
               p("För mörka utomhusfärger, välj 100% zinkvit i vitbasen, i nästa steg anger du sedan hur mycket av vitbasen (det vill säga zinkbasen) som ska vara med i den färdiga färgen."),
               br(),
               numericInput("total_weight", "Önskad totalvikt pigment (g)", 300, min = 100, max = 2000, step = 10),
               sliderInput("zinc_ratio", "Zinkvit i vitbas (%)", 0, 100, 25, step = 1),
               br(),
               actionButton("to_step2", "Nästa: Välj färgande pigment", class = "btn-primary next-btn")
    )),
    
    hidden(div(id = "step2",
               h2("Steg 2 – Blanda färg"),
               fluidRow(
                 column(12,
                        p(textOutput("locked_info", inline = TRUE)),
                        p("Ange upp till 3 färgande pigment som du vill använda med vitbasen."),
                        p("Ange därefter hur mycket du vill använda av det färgande pigmentet i förhållande till vitbasen."),
                        p("För mörka färger, välj en låg andel (eller ingen alls) av vitbasen."),
                        br()
                 ),
               ),
               fluidRow(
                 column(6,
                        selectInput("p1", "Pigment 1", c("Inget" = "", color_choices)),
                        conditionalPanel("input.p1 != ''", sliderInput("pct1", "Andel i förhållande till vitbas (%)", 1, 100, 12)),
                        selectInput("p2", "Pigment 2", c("Inget" = "", color_choices)),
                        conditionalPanel("input.p2 != ''", sliderInput("pct2", "Andel i förhållande till vitbas (%)", 1, 100, 30)),
                        selectInput("p3", "Pigment 3", c("Inget" = "", color_choices)),
                        conditionalPanel("input.p3 != ''", sliderInput("pct3", "Andel i förhållande till vitbas (%)", 1, 100, 20)),
                        p(textInput("paint_name","Namn på färgblandning",placeholder = "Sätt ett namn (valfritt)")),
                        actionButton("reset_pigments", "Nollställ", class = "reset-btn")
                 ),
                 column(6,
                        h3("Färgprov"),
                        uiOutput("live_preview"),
                 )
               ),
               fluidRow(
                 column(12,
                        br(),
                        actionButton("back_to_step1", "Tillbaka", class = "btn-default back-btn"),
                        actionButton("to_step3", "Nästa: Beräkna olja", class = "btn-primary next-btn")
                        )
               )
    )),
    
    hidden(div(id = "step3",
               h2("Färdigt recept", textOutput("paint_name", inline = TRUE)),
               fluidRow(
                 column(12,
                        p(textOutput("locked_info", inline = TRUE), "Här är det färdiga receptet för en färgpasta med önskad mängd färgande pigment."),
                        p("Pigmenten läggs att väta i ett kärl (gärna en plåtburk för färg) med linolja över natten. Blandningen rivs sedan noga med färgblandare i borrmaskin. Pastan kan justeras med valfri mängd extra linolja, utifrån behov och önskemål."),
                        p("Grundfärgsstrykning: Pastan kan målas/gnuggas in med påstrykare direkt, som en mager grundfärg."),
                        p("Mellanstrykning: För mellanstrykningen, till för kokt linolja, utifrån principen “fett över magert”, i precis den mängd som gör att färgen utstruken på en glasbit är täckande,"),
                        p("Slutstrykning: För ökad glans kan sista strykningen med moddlare därutöver med fördel innehålla upp till 5 % soloxiderad linolja."),
                        br()
                 ),
               ),
               fluidRow(
                 box(title = "Recept", status = "danger", solidHeader = TRUE,
                     tableOutput("final_recipe"), p(textOutput("exact_total")),
                     downloadButton("download", "Spara recept", class = "btn-block btn-primary  btn-lg")),
                 box(title = "Färg", status = "primary", solidHeader = TRUE,
                     uiOutput("final_preview"), br(),
                     tags$b("Ungefärlig hexadecimal färgkod: "), textOutput("final_hex"))
               ),
               br(),
               actionButton("back_to_step2", "Tillbaka", class = "btn-default back-btn"),
               # actionButton("restart", "Börja om från början", class = "btn-warning")
    ))
  )
)

server <- function(input, output, session) {
  observe({ showElement("step1") })
  
  observeEvent(input$to_step2, {
    req(input$total_weight > 0)
    hide("step1"); show("step2")
  })
  
  # NYTT: Tillbaka från Steg 2 → Steg 1
  observeEvent(input$back_to_step1, {
    hide("step2"); show("step1")
  })
  
  # NYTT: Tillbaka från Steg 3 → Steg 2
  observeEvent(input$back_to_step2, {
    final_data(NULL)
    hide("step3"); show("step2")
  })
  
  # NYTT: Rensa alla pigmentval med en enda knapp
  observeEvent(input$reset_pigments, {
    updateSelectInput(session, "p1", selected = "")
    updateSelectInput(session, "p2", selected = "")
    updateSelectInput(session, "p3", selected = "")
    # Sliders nollställs automatiskt när selectInput blir "" pga conditionalPanel
  })
  
  output$locked_info <- renderText({
    paste0("Du har valt ", input$total_weight, " g total pigmentmängd och ", input$zinc_ratio, " % zinkvit i vitbasen.")
  })
  
  output$paint_name <- renderText({
    if(input$paint_name != "") {paste0("på ", "”", input$paint_name, "”")}
  })
  
  # === LIVE PREVIEW === (oförändrat)
  live_color <- reactive({
    total_g <- input$total_weight
    color_pct <- sum(c(input$pct1 %||% 0, input$pct2 %||% 0, input$pct3 %||% 0))
    color_pct <- min(color_pct, 100)
    
    raw_weights <- numeric()
    used <- c(input$p1, input$p2, input$p3)[c(input$p1,input$p2,input$p3) != ""]
    for (id in used) {
      pct <- input[[paste0("pct", which(c(input$p1,input$p2,input$p3) == id))]]
      raw_weights[id] <- total_g * color_pct/100 * pct / max(color_pct, 1)
    }
    
    white_g <- total_g * (100 - color_pct)/100
    zn_frac <- input$zinc_ratio / 100
    zn_g <- white_g * zn_frac * km[["44400"]]$S / km[["44100"]]$S
    ti_g <- white_g * (1 - zn_frac)
    
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
  
  # === STEG 3 – NORMALISERING & EXAKT VIKT === (oförändrat)
  final_data <- reactiveVal(NULL)
  
  observeEvent(input$to_step3, {
    hide("step2"); show("step3")
    
    total_g <- input$total_weight
    color_pct <- sum(c(input$pct1 %||% 0, input$pct2 %||% 0, input$pct3 %||% 0))
    color_pct <- min(color_pct, 100)
    white_pct <- 100 - color_pct
    
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
    
    total_raw <- raw_zn_g + raw_ti_g + sum(raw_color)
    norm_factor <- total_g / total_raw
    
    zn_g <- round(raw_zn_g * norm_factor, 1)
    ti_g <- round(raw_ti_g * norm_factor, 1)
    color_g <- round(raw_color * norm_factor, 1)
    
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
    paste0("Total pigmentvikt: ", final_data()$total_pigment, " g")
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
    filename = paste0("Paint-o-matic_", if(input$paint_name != "") {paste0(input$paint_name,"_")}, format(Sys.time(), format = "%F_%R"), ".txt"),
    content = function(file) {
      r <- final_data()
      headline <- paste0("# Paint-o-matic ", if(input$paint_name != "") {paste0(input$paint_name," ")}, format(Sys.time(), format = "%F %R"))
      lines <- c(
        headline,
        paste0(rep("=",nchar(headline)),collapse=""),
        paste("Total pigmentvikt:", r$total_pigment, "g"),
        paste("Zinkvit i bas:", input$zinc_ratio, "%"),
        paste("Färgkod i hexadecimalt format:", r$hex),
        paste0(rep("=",nchar(headline)),collapse=""),
        "RECEPT:"
      )
      lines <- c(lines, paste("- Linolja:", r$oil, "g"))
      if (r$zn > 0.1) lines <- c(lines, paste("- Zinkvit PW4:", r$zn, "g"))
      if (r$ti > 0.1) lines <- c(lines, paste("- Titanvit PW6:", r$ti, "g"))
      for (id in names(r$color))
        lines <- c(lines, paste("-",km[[id]]$name, ":", r$color[id], "g"))
      writeLines(lines, file)
    }
  )
  
  observeEvent(input$restart, {
    final_data(NULL)
    hide("step3"); hide("step2"); show("step1")
  })
}

shinyApp(ui, server)