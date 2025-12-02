# Formatting Utilities
# Swedish number formatting and input parsing functions

# Helper functions for Swedish number formatting
format_swe <- function(x, digits = 1) {
  if(is.null(x) || is.na(x)) return("0")
  
  # If value is whole number (or very close to it), omit decimal
  if(abs(x - round(x, 0)) < 0.01) {
    formatted <- format(round(x, 0), 
                        decimal.mark = ",", 
                        big.mark = " ",
                        trim = TRUE)
  } else {
    formatted <- format(round(x, digits), 
                        nsmall = digits, 
                        decimal.mark = ",", 
                        big.mark = " ",
                        trim = TRUE)
  }
  return(formatted)
}

# Locale-independent numeric parser (handles both dots and commas from any source)
parse_numeric <- function(x, default = NA) {
  if(is.null(x) || length(x) == 0) return(default)
  if(is.numeric(x)) return(x)
  
  # Convert to character and replace comma with dot
  x_char <- as.character(x)
  x_char <- gsub(",", ".", x_char)
  x_char <- gsub(" ", "", x_char)  # Remove spaces (thousand separators)
  
  result <- suppressWarnings(as.numeric(x_char))
  if(is.na(result)) return(default)
  return(result)
}

# Smart rounding based on weight - improves readability and practicality
smart_round <- function(weight) {
  if(weight < 10) {
    # Small amounts need precision (e.g., 3.5g, 8.2g)
    return(round(weight, 1))
  } else if(weight < 100) {
    # Medium amounts: whole grams (e.g., 45g, 87g)
    return(round(weight, 0))
  } else if(weight < 500) {
    # Large amounts: round to 5g (e.g., 235g, 340g)
    return(round(weight / 5) * 5)
  } else {
    # Very large amounts: round to 10g (e.g., 780g, 1250g)
    return(round(weight / 10) * 10)
  }
}

# Safe input retrieval with validation
safe_input <- function(input, name, default, test = function(x) TRUE) {
  val <- input[[name]]
  if(isTRUE(!is.null(val) && !is.na(val) && test(val))) 
    as.numeric(val) 
  else 
    default
}

# Null-coalescing operator
`%||%` <- function(a, b) if(is.null(a)) b else a
