# UI Helper Functions
# Reusable UI components to reduce duplication

# Info/alert box with consistent styling
info_box <- function(content, type = "info", icon_name = "info-circle") {
  class_name <- switch(type,
    "info" = "info-box",
    "warning" = "alert",
    "normalized" = "normalized-box",
    "info-box"
  )
  
  tags$div(
    class = class_name,
    icon(icon_name),
    " ",
    content
  )
}

# Section with header and content
section_box <- function(title, content, class = "ready-box") {
  tags$div(
    class = class,
    if (!is.null(title)) tags$h3(title),
    content
  )
}

# Pigment selector (dropdown + slider combo)
# Returns a tagList with pickerInput and conditionalPanel with slider
pigment_selector_pair <- function(id_prefix, label, choices, selected = "", 
                                   pct_value = 0, pct_label = "Andel (%)") {
  tagList(
    pickerInput(
      paste0(id_prefix, "_pigment"),
      label,
      choices = choices,
      selected = selected,
      options = pickerOptions(`live-search` = TRUE, size = 12)
    ),
    conditionalPanel(
      condition = sprintf("input.%s_pigment", id_prefix),
      sliderInput(
        paste0(id_prefix, "_pct"),
        pct_label,
        min = 0,
        max = 100,
        value = pct_value,
        step = 1
      )
    )
  )
}

# Formatted metric display (number with label)
metric_display <- function(label, value, unit = "", bold_label = TRUE) {
  tags$span(
    if (bold_label) tags$b(label) else label,
    " ",
    value,
    if (nchar(unit) > 0) paste0(" ", unit)
  )
}
