#!/usr/bin/env Rscript
# Pigment Database Validation Script
# Checks for common issues with pigment database and display groups
# Run from paint-o-matic directory: Rscript tools/validate_pigments.R

# Load the app environment
source("global.R")
source("app.R", local = TRUE)

cat("=== Pigment Database Validation ===\n\n")

# 1. Count pigments
total_pigments <- length(pigments_db)
cat(sprintf("Total pigments in database: %d\n", total_pigments))

# 2. Check display groups
all_mapped <- unlist(PIGMENT_DISPLAY_GROUPS, use.names = FALSE)
unique_mapped <- unique(all_mapped)
cat(sprintf("Total pigments in display groups: %d\n", length(all_mapped)))
cat(sprintf("Unique pigments in display groups: %d\n", length(unique_mapped)))

# Check for duplicates
if(length(all_mapped) != length(unique_mapped)) {
  duplicates <- all_mapped[duplicated(all_mapped)]
  cat("\n⚠️  WARNING: Duplicate pigments in display groups:\n")
  for(dup in duplicates) {
    cat(sprintf("  - %s\n", dup))
  }
}

# 3. Check for pigments in DB but not in display groups
exclude_from_check <- c("vitbas", "44100", "44400", "599930", "58000", "58010", "58162", "58900", "58250")
db_ids <- names(pigments_db)
unmapped <- setdiff(db_ids, c(unique_mapped, exclude_from_check))

if(length(unmapped) > 0) {
  cat("\n❌ ERROR: Pigments in database but NOT in any display group:\n")
  for(id in unmapped) {
    pigment <- pigments_db[[id]]
    cat(sprintf("  - %s: %s (category: %s)\n", id, pigment$name, pigment$metadata$category))
  }
  cat("\nThese pigments will NOT appear in dropdown menus!\n")
  cat("Add them to PIGMENT_DISPLAY_GROUPS in app.R\n")
} else {
  cat("\n✅ All pigments in database are mapped to display groups\n")
}

# 4. Check for pigments in display groups but not in DB
not_in_db <- setdiff(unique_mapped, db_ids)
if(length(not_in_db) > 0) {
  cat("\n❌ ERROR: Pigments in display groups but NOT in database:\n")
  for(id in not_in_db) {
    # Find which group(s) contain this ID
    groups <- names(PIGMENT_DISPLAY_GROUPS)[sapply(PIGMENT_DISPLAY_GROUPS, function(g) id %in% g)]
    cat(sprintf("  - %s (in groups: %s)\n", id, paste(groups, collapse=", ")))
  }
  cat("\nThese pigments are referenced but don't exist!\n")
  cat("Either add them to pigments_unified.R or remove from PIGMENT_DISPLAY_GROUPS\n")
} else {
  cat("✅ All display group references are valid\n")
}

# 5. Check category distribution
cat("\n=== Category Distribution ===\n")
categories <- sapply(pigments_db, function(p) p$metadata$category)
cat_table <- table(categories)
for(cat in names(sort(cat_table, decreasing = TRUE))) {
  count <- cat_table[[cat]]
  cat(sprintf("  %s: %d pigments\n", cat, count))
}

# 6. Check display group sizes
cat("\n=== Display Group Sizes ===\n")
for(group_name in names(PIGMENT_DISPLAY_GROUPS)) {
  count <- length(PIGMENT_DISPLAY_GROUPS[[group_name]])
  cat(sprintf("  %s: %d pigments\n", group_name, count))
}

# 7. Check for pigments without suppliers
cat("\n=== Supplier Coverage ===\n")
no_suppliers <- sapply(pigments_db, function(p) is.null(p$suppliers))
count_no_suppliers <- sum(no_suppliers)
if(count_no_suppliers > 0) {
  cat(sprintf("⚠️  %d pigments without supplier information:\n", count_no_suppliers))
  ids_no_suppliers <- names(pigments_db)[no_suppliers]
  for(id in ids_no_suppliers) {
    cat(sprintf("  - %s: %s\n", id, pigments_db[[id]]$name))
  }
} else {
  cat("✅ All pigments have supplier information\n")
}

# 8. Check for pigments with missing or invalid properties
cat("\n=== Property Validation ===\n")
issues <- list()

for(id in names(pigments_db)) {
  p <- pigments_db[[id]]
  
  # Check required properties
  if(is.null(p$properties$oil) || !is.numeric(p$properties$oil)) {
    issues[[length(issues) + 1]] <- sprintf("%s: missing or invalid 'oil' property", id)
  }
  if(is.null(p$properties$K) || !is.numeric(p$properties$K)) {
    issues[[length(issues) + 1]] <- sprintf("%s: missing or invalid 'K' property", id)
  }
  if(is.null(p$properties$S) || !is.numeric(p$properties$S)) {
    issues[[length(issues) + 1]] <- sprintf("%s: missing or invalid 'S' property", id)
  }
  if(is.null(p$properties$density) || !is.numeric(p$properties$density)) {
    issues[[length(issues) + 1]] <- sprintf("%s: missing or invalid 'density' property", id)
  }
  if(is.null(p$properties$rgb) || length(p$properties$rgb) != 3) {
    issues[[length(issues) + 1]] <- sprintf("%s: missing or invalid 'rgb' property", id)
  }
  
  # Check metadata
  if(is.null(p$metadata$category)) {
    issues[[length(issues) + 1]] <- sprintf("%s: missing 'category' metadata", id)
  }
}

if(length(issues) > 0) {
  cat("❌ Found property issues:\n")
  for(issue in issues) {
    cat(sprintf("  - %s\n", issue))
  }
} else {
  cat("✅ All pigments have valid properties\n")
}

# 9. Summary
cat("\n=== Summary ===\n")
if(length(unmapped) == 0 && length(not_in_db) == 0 && length(issues) == 0) {
  cat("✅ Pigment database is healthy!\n")
  cat("   - All pigments are properly mapped\n")
  cat("   - All references are valid\n")
  cat("   - All properties are present\n")
} else {
  cat("❌ Issues found - please review above\n")
  if(length(unmapped) > 0) {
    cat(sprintf("   - %d pigments not in display groups\n", length(unmapped)))
  }
  if(length(not_in_db) > 0) {
    cat(sprintf("   - %d invalid display group references\n", length(not_in_db)))
  }
  if(length(issues) > 0) {
    cat(sprintf("   - %d property validation issues\n", length(issues)))
  }
}

cat("\n")
