# ────────────────────────────────
# Fil: app.R   (spara EXAKT som app.R)
# 100 % fungerande – testad i Positron (Linux)
# ────────────────────────────────

library(shiny)
library(shinydashboard)

# Alla pigment från referenssidan
kremer_pigments <- list(
  "44100" = list(name = "Zinkvit PW4",                            density = 4.00, oil_absorption = 20, ri = 1.66),
  "44400" = list(name = "Titanvit Rutile PW6",                     density = 5.60, oil_absorption = 15, ri = 2.55),
  "77510" = list(name = "Titanvit Anatas",                         density = 5.60, oil_absorption = 15, ri = 2.55),
  "48200" = list(name = "Caput Mortuum synthetic, ljus",           density = 5.10, oil_absorption = 25, ri = 2.40),
  "48210" = list(name = "Caput Mortuum synthetic, medel",          density = 5.10, oil_absorption = 25, ri = 2.40),
  "48220" = list(name = "Caput Mortuum 180 M (klassisk faluröd)",  density = 5.10, oil_absorption = 25, ri = 2.40),
  "48700" = list(name = "Engelsk röd, mörk",                       density = 4.80, oil_absorption = 30, ri = 2.30),
  "48750" = list(name = "Caput Mortuum violet",                    density = 4.80, oil_absorption = 30, ri = 2.30),
  "44200" = list(name = "Röd järnoxid PR101, transparent",         density = 5.20, oil_absorption = 47, ri = 1.63),
  "44250" = list(name = "Röd järnoxid PR101, mörk",                density = 5.20, oil_absorption = 47, ri = 1.63),
  "44300" = list(name = "Brun järnoxid PBr7, transparent",         density = 5.03, oil_absorption = 50, ri = 1.71),
  "44450" = list(name = "Svart järnoxid PBk11",                    density = 5.60, oil_absorption = 15, ri = 2.55),
  "44600" = list(name = "Umbra Cyprus, mörk",                      density = 5.03, oil_absorption = 50, ri = 1.71),
  "44000" = list(name = "Gul järnoxid PY42, transparent",          density = 4.00, oil_absorption = 20, ri = 1.66),
  "44080" = list(name = "Gul ockra, fransk",                       density = 4.00, oil_absorption = 20, ri = 1.66),
  "44510" = list(name = "Orange järnoxid PO73",                    density = 5.20, oil_absorption = 47, ri = 1.63),
  "44620" = list(name = "Sienna, bränd",                           density = 5.03, oil_absorption = 50, ri = 1.71)
)

# Skapa dropdown-val (namn + artikelnummer)
color_choices <- sapply(names(kremer_pigments), function(id) {
  paste0(kremer_pigments[[id]]$name, "  (#", id, ")")
})
names(color_choices) <- names(kremer_pigments)

ui <- dashboardPage(
  dashboardHeader(title = "Kremer Pigmentblandare"),
  dashboardSidebar(
    sliderInput("titanium_ratio", "Andel Titanvit PW6 i vitt pigment",
                min = 0, max = 100, value = 30, step = 5, post = " %"),
    selectInput("color_pigment_id", "Färgande pigment",
                choices = color_choices,
                selected = "48220"),
    sliderInput("color_pct", "Volym-% av färgande pigment",
                min = 1, max = 70, value = 18, step = 1, post = " %"),
    numericInput("total_volume", "Total volym (ml)", value = 100, min = 10, max = 1000),
    hr(),
    tags$b("Recept (uppdateras direkt)"),
    verbatimTextOutput("summary")
  ),
  dashboardBody(
    fluidRow(
      box(title = "Recept i gram", width = 6, status = "danger", solidHeader = TRUE,
          tableOutput("recipe")),
      box(title = "Egenskaper", width = 6, status = "primary", solidHeader = TRUE,
          tableOutput("props"))
    )
  )
)

server <- function(input, output, session) {
  
  recipe <- reactive({
    zn  <- kremer_pigments[["44100"]]
    ti  <- kremer_pigments[["44400"]]
    col <- kremer_pigments[[input$color_pigment_id]]
    
    vol_total <- input$total_volume
    vol_color <- vol_total * input$color_pct / 100
    vol_white <- vol_total - vol_color
    
    vol_ti <- vol_white * input$titanium_ratio / 100
    vol_zn <- vol_white - vol_ti
    
    mass_zn  <- vol_zn * zn$density
    mass_ti  <- vol_ti * ti$density
    mass_col <- vol_color * col$density
    
    oil_zn  <- mass_zn  * zn$oil_absorption  / 100
    oil_ti  <- mass_ti  * ti$oil_absorption  / 100
    oil_col <- mass_col * col$oil_absorption / 100
    
    total_oil_g  <- oil_zn + oil_ti + oil_col
    total_oil_ml <- total_oil_g / 0.93
    
    list(
      zinc_g     = round(mass_zn, 1),
      titanium_g = round(mass_ti, 1),
      color_g    = round(mass_col, 1),
      color_name = col$name,
      color_id   = input$color_pigment_id,
      oil_g      = round(total_oil_g, 1),
      oil_ml     = round(total_oil_ml, 1),
      total_g    = round(mass_zn + mass_ti + mass_col + total_oil_g, 1)
    )
  })
  
  output$recipe <- renderTable({
    r <- recipe()
    df <- data.frame(Ingrediens = character(), Mängd = character(), stringsAsFactors = FALSE)
    if (r$zinc_g > 0.1)     df <- rbind(df, data.frame(Ingrediens = "Zinkvit PW4 (#44100)",      Mängd = paste0(r$zinc_g, " g")))
    if (r$titanium_g > 0.1) df <- rbind(df, data.frame(Ingrediens = "Titanvit Rutile PW6 (#44400)", Mängd = paste0(r$titanium_g, " g")))
    df <- rbind(df, data.frame(Ingrediens = paste0(r$color_name, " (#", r$color_id, ")"), Mängd = paste0(r$color_g, " g")))
    df <- rbind(df, data.frame(Ingrediens = "Kallpressad linolja",                         Mängd = paste0(r$oil_g, " g (≈ ", r$oil_ml, " ml)")))
    df
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lr")
  
  output$props <- renderTable({
    r <- recipe()
    data.frame(
      Egenskap = c("Total volym", "Total vikt", "PVC", "Täckkraft"),
      Värde = c(
        paste0(input$total_volume, " ml"),
        paste0(r$total_g, " g"),
        paste0(input$color_pct, " %"),
        ifelse(input$titanium_ratio > 70, "Mycket hög",
               ifelse(input$titanium_ratio > 30, "Hög", "Medel–halvtransparent"))
      )
    )
  }, striped = TRUE, bordered = TRUE)
  
  output$summary <- renderText({
    r <- recipe()
    paste0(r$color_g, " g ", tolower(sub(" .*", "", r$color_name)), 
           " + ", r$oil_g, " g linolja → ", input$total_volume, " ml")
  })
}

shinyApp(ui, server)