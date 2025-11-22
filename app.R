# ────────────────────────────────
# app.R – KUBELKA-MUNK-version
# Skapad med hjälp av Grok.com
# ────────────────────────────────

library(shiny)
library(shinydashboard)

km <- list(
  "44100" = list(name = "Zinkvit PW4",           oil = 20, K = 0,     S = 0.45),
  "44400" = list(name = "Titanvit Rutile PW6",   oil = 15, K = 0,     S = 3.40),
  "48220" = list(name = "Caput Mortuum 180 M",   oil = 25, K = 0.85,  S = 0.60),
  "48200" = list(name = "Caput Mortuum ljus",    oil = 25, K = 0.70,  S = 0.65),
  "48750" = list(name = "Caput Mortuum violet",  oil = 30, K = 1.10,  S = 0.55),
  "44200" = list(name = "Rödoxid PR101 transp",  oil = 47, K = 0.90,  S = 0.12),
  "44250" = list(name = "Rödoxid PR101 mörk",    oil = 47, K = 1.20,  S = 0.90),
  "44450" = list(name = "Svartoxid PBk11",       oil = 15, K = 2.40,  S = 1.10),
  "44000" = list(name = "Guloxid PY42 transp",   oil = 20, K = 0.45,  S = 0.18),
  "44080" = list(name = "Gul ockra fransk",      oil = 20, K = 0.50,  S = 0.40),
  "44510" = list(name = "Orangeoxid PO73",       oil = 47, K = 0.55,  S = 0.85),
  "44300" = list(name = "Brunoxid PBr7 transp",  oil = 50, K = 0.80,  S = 0.22),
  "44620" = list(name = "Sienna bränd",          oil = 50, K = 0.75,  S = 0.50),
  "44600" = list(name = "Umbra Cyprus mörk",     oil = 50, K = 1.00,  S = 0.45)
)

rgb <- list(
  "44100" = c(255,255,255), "44400" = c(255,255,255),
  "48220" = c(139,58,58),   "48200" = c(160,82,82),
  "48750" = c(107,58,71),   "44200" = c(178,34,34),
  "44250" = c(139,0,0),     "44450" = c(28,38,38),
  "44000" = c(218,165,32),  "44080" = c(193,154,107),
  "44510" = c(232,97,0),    "44300" = c(139,69,19),
  "44620" = c(160,82,45),   "44600" = c(99,81,71)
)

# Alla färgande pigment i dropdown
color_choices <- setNames(
  names(km)[sapply(km, function(x) x$K > 0.01)],
  paste0(sapply(km, `[[`, "name")[sapply(km, function(x) x$K > 0.01)],
         "  (#", names(km)[sapply(km, function(x) x$K > 0.01)], ")")
)

ui <- dashboardPage(
  dashboardHeader(title = "Färglabbet"),
  dashboardSidebar(
    numericInput("total_weight", "Total vikt pigment (g)", 300, min = 50, max = 5000),
    sliderInput("zinc_ratio", "Zinkvit PW4 i vitt (%)", 0, 100, 60, step = 1),

    tags$h4("Färgande pigment (0–3 st)"),
    selectInput("p1", "Pigment 1", choices = c("Inget" = "", color_choices), selected = ""),
    conditionalPanel("input.p1 != ''", sliderInput("pct1", "Andel (%)", 1, 100, 70)),
    
    selectInput("p2", "Pigment 2", choices = c("Inget" = "", color_choices), selected = ""),
    conditionalPanel("input.p2 != ''", sliderInput("pct2", "Andel (%)", 1, 100, 20)),
    
    selectInput("p3", "Pigment 3", choices = c("Inget" = "", color_choices), selected = ""),
    conditionalPanel("input.p3 != ''", sliderInput("pct3", "Andel (%)", 1, 100, 10))
  ),

  dashboardBody(
    fluidRow(
      box(title = "Recept", width = 8, status = "danger", solidHeader = TRUE,
          tableOutput("recipe"),
          br(), br(),
          downloadButton("download_recipe", "Spara recept som .txt", icon = icon("download"),
                         style = "width:100%; font-size:16px; padding:12px;")
      ),

      box(title = "Färgprov", width = 4, status = "primary", solidHeader = TRUE,
          uiOutput("color_box"),
          uiOutput("color_boxB"),
          br(),
          tags$b("Färgkod:"), textOutput("hex_code"), br(),
          tags$b("Färgande pigment:"), textOutput("color_pct_text"), br(),
          tags$b("Total vikt:"), textOutput("total_weight_text"), br(),
          tags$small(tags$i("Kubelka-Munk-korrekt – kulören är helt stabil oavsett vitbas"))
      )
    )
  )
)

server <- function(input, output, session) {

  total_color_pct <- reactive({
    s <- 0
    if (input$p1 != "" && !is.null(input$pct1)) s <- s + input$pct1
    if (input$p2 != "" && !is.null(input$pct2)) s <- s + input$pct2
    if (input$p3 != "" && !is.null(input$pct3)) s <- s + input$pct3
    min(s, 100)
  })

  recipe_and_color <- reactive({
    total_g <- input$total_weight
    color_pct <- total_color_pct()

    total_K_col <- total_S_col <- 0
    color_weights <- numeric()
    used <- c(input$p1, input$p2, input$p3)[c(input$p1,input$p2,input$p3) != ""]
    for (id in used) {
      pct <- input[[paste0("pct", which(c(input$p1,input$p2,input$p3) == id))]]
      w <- total_g * color_pct/100 * pct / max(total_color_pct(), 1)
      color_weights[id] <- round(w, 1)
      total_K_col <- total_K_col + w * km[[id]]$K
      total_S_col <- total_S_col + w * km[[id]]$S
    }

    target_S_white <- total_g * (100 - color_pct)/100 * km[["44400"]]$S
    zn_frac <- input$zinc_ratio / 100
    zn_g <- (zn_frac * target_S_white) / km[["44100"]]$S
    ti_g <- ((1 - zn_frac) * target_S_white) / km[["44400"]]$S

    oil <- zn_g * 20/100 + ti_g * 15/100
    for (id in names(color_weights)) oil <- oil + color_weights[id] * km[[id]]$oil / 100
    oil_g <- round(oil, 1)
    oil_ml <- round(oil / 0.93, 1)

    # Färgprov – titanvit-ekvivalent vitmängd
    equiv_white_g <- target_S_white / km[["44400"]]$S
    r <- g <- b <- equiv_white_g * 255
    for (id in names(color_weights)) {
      rgb_val <- rgb[[id]]
      r <- r + color_weights[id] * rgb_val[1]
      g <- g + color_weights[id] * rgb_val[2]
      b <- b + color_weights[id] * rgb_val[3]
    }
    total_equiv <- equiv_white_g + sum(color_weights)
    final_hex <- sprintf("#%02X%02X%02X",
                         round(r/total_equiv), round(g/total_equiv), round(b/total_equiv))

    list(zinc_g = round(zn_g,1), titanium_g = round(ti_g,1),
         color_weights = color_weights, oil_g = oil_g, oil_ml = oil_ml,
         final_hex = final_hex, color_pct = color_pct, total_g = total_g)
  })

  output$color_box <- renderUI({
    tags$div(style = paste0(
      "height:200px; background:", recipe_and_color()$final_hex,
      "; border:4em solid #000;border-bottom:0; border-radius:0px;"
    ))
  })

  output$color_boxB <- renderUI({
    tags$div(style = paste0(
      "height:200px; background:", recipe_and_color()$final_hex,
      "; border:4em solid #fff;border-top:0;margin-bottom:-4em; border-radius:0px;"
    ))
  })

  output$hex_code <- renderText(recipe_and_color()$final_hex)
  output$color_pct_text <- renderText(paste0(recipe_and_color()$color_pct, " %"))
  output$total_weight_text <- renderText(paste0(recipe_and_color()$total_g, " g"))

  output$recipe <- renderTable({
    r <- recipe_and_color()
    df <- data.frame(Ingrediens = character(), Gram = character(), stringsAsFactors = FALSE)
    df <- rbind(df, data.frame(Ingrediens = HTML("Kallpressad kokt linolja"),
                               Gram = paste0(r$oil_g, " g ≈ ", r$oil_ml, " ml")))
    if (r$zinc_g > 0.1)     df <- rbind(df, data.frame(Ingrediens = "Zinkvit PW4 (#44100)", Gram = paste0(r$zinc_g, " g")))
    if (r$titanium_g > 0.1) df <- rbind(df, data.frame(Ingrediens = "Titanvit Rutile PW6 (#44400)", Gram = paste0(r$titanium_g, " g")))
    for (id in names(r$color_weights))
      df <- rbind(df, data.frame(Ingrediens = paste0(km[[id]]$name, " (#", id, ")"),
                                 Gram = paste0(r$color_weights[id], " g")))
    if (r$color_pct >= 98)
      df <- rbind(df, data.frame(Ingrediens = HTML("<span style='color:red; font-weight:bold;'>VARNING: 100 % pigment!</span>"), Gram = ""))
    df
  }, sanitize.text.function = function(x) x, striped = TRUE, bordered = TRUE)

  output$download_recipe <- downloadHandler(
    filename = function() paste0("kremer_KM_recept_", Sys.Date(), ".txt"),
    content = function(file) {
      r <- recipe_and_color()
      lines <- c(
        paste("=== Kremer Kubelka-Munk recept", Sys.Date(), "==="),
        paste("Färgkod:", r$final_hex),
        paste("Total vikt:", r$total_g, "g  |  Zinkvit i bas:", input$zinc_ratio, "%"),
        "",
        "RECEPT (väg i denna ordning):"
      )
      i <- 1
      lines <- c(lines, paste0(i, ". Kallpressad kokt linolja: ", r$oil_g, " g (≈ ", r$oil_ml, " ml)")); i <- i + 1
      if (r$zinc_g > 0.1)     lines <- c(lines, paste0(i, ". Zinkvit PW4 (#44100): ", r$zinc_g, " g")); i <- i + 1
      if (r$titanium_g > 0.1) lines <- c(lines, paste0(i, ". Titanvit Rutile PW6 (#44400): ", r$titanium_g, " g")); i <- i + 1
      for (id in names(r$color_weights)) {
        lines <- c(lines, paste0(i, ". ", km[[id]]$name, " (#", id, "): ", r$color_weights[id], " g"))
        i <- i + 1
      }
      writeLines(lines, file)
    }
  )
}

shinyApp(ui, server)