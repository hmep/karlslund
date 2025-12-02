# Helper Functions for Unified Pigment Database
# Provides clean API for accessing pigment data

# Get complete pigment entry
get_pigment <- function(id) {
  if(!id %in% names(pigments_db)) {
    warning("Pigment ID not found: ", id)
    return(NULL)
  }
  pigments_db[[id]]
}

# Get specific property value
get_pigment_property <- function(id, property) {
  p <- get_pigment(id)
  if(is.null(p)) return(NULL)
  p$properties[[property]]
}

# Get pigment name by ID
get_pigment_name <- function(id) {
  p <- get_pigment(id)
  if(is.null(p)) return(paste0("Unknown: ", id))
  p$name
}

# Get pigment ID by name (reverse lookup)
get_pigment_id <- function(name) {
  pigment_name_to_id[[name]]
}

# Check if pigment is RAÄ approved
is_raa_pigment <- function(id) {
  p <- get_pigment(id)
  if(is.null(p)) return(FALSE)
  isTRUE(p$metadata$is_raa)
}

# Check if pigment is tar-compatible
is_tar_compatible <- function(id) {
  p <- get_pigment(id)
  if(is.null(p)) return(TRUE)  # Default to TRUE
  isTRUE(p$metadata$is_tar_compatible %||% TRUE)
}

# Get all pigments by category
get_pigments_by_category <- function(category) {
  names(pigments_db)[
    sapply(pigments_db, function(p) p$metadata$category == category)
  ]
}

# Get all RAÄ pigments (returns named vector: id => name)
get_raa_pigments <- function() {
  names(pigments_db)[
    sapply(pigments_db, function(p) isTRUE(p$metadata$is_raa))
  ]
}

# Get supplier info for pigment
get_supplier_info <- function(id, supplier_name = NULL) {
  p <- get_pigment(id)
  if(is.null(p) || is.null(p$suppliers)) return(NULL)
  
  if(is.null(supplier_name)) {
    return(p$suppliers)  # Return all suppliers
  } else {
    return(p$suppliers[[supplier_name]])  # Return specific supplier
  }
}
