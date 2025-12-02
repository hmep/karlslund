# Verification Tests for Unified Pigment Database
# These tests verify backward compatibility and new functionality

# This script should be run in R to verify the migration was successful
# Run from paint-o-matic directory: source("tools/verify_unified_db.R")

cat("=== Phase 2B Verification Tests ===\n\n")

# Source global.R to load unified database and compatibility layer
source("global.R")

cat("1. Testing unified database loaded...\n")
stopifnot(exists("pigments_db"))
stopifnot(length(pigments_db) == 56)
cat("   ✓ pigments_db contains 56 pigments\n\n")

cat("2. Testing backward compatibility - km structure...\n")
stopifnot(exists("km"))
stopifnot(length(km) == 56)
stopifnot(!is.null(km[["44450"]]))
stopifnot(km[["44450"]]$name == "Svartoxid PBk11")
stopifnot(km[["44450"]]$oil == 15)
stopifnot(km[["44450"]]$K == 2.40)
stopifnot(all(km[["44450"]]$rgb == c(28, 38, 38)))
cat("   ✓ km structure works correctly\n\n")

cat("3. Testing backward compatibility - suppliers structure...\n")
stopifnot(exists("suppliers"))
stopifnot(length(suppliers) >= 40)  # At least 40 pigments have suppliers
stopifnot(!is.null(suppliers[["44450"]]))
stopifnot(!is.null(suppliers[["44450"]]$kremer_id))
cat("   ✓ suppliers structure works correctly\n\n")

cat("4. Testing backward compatibility - raa_pigments list...\n")
stopifnot(exists("raa_pigments"))
stopifnot(length(raa_pigments) == 24)
stopifnot("vitbas" %in% raa_pigments)
stopifnot("J318" %in% raa_pigments)
stopifnot("KG83" %in% raa_pigments)
stopifnot(!("44450" %in% raa_pigments))  # Not a RAÄ pigment
cat("   ✓ raa_pigments contains correct 24 pigments\n\n")

cat("5. Testing backward compatibility - pigment_name_to_id...\n")
stopifnot(exists("pigment_name_to_id"))
stopifnot(length(pigment_name_to_id) == 56)
stopifnot(pigment_name_to_id[["Svartoxid PBk11"]] == "44450")
stopifnot(pigment_name_to_id[["Järnoxidsvart nr 318"]] == "J318")
cat("   ✓ pigment_name_to_id auto-generated correctly\n\n")

cat("6. Testing helper functions - get_pigment...\n")
p <- get_pigment("44450")
stopifnot(!is.null(p))
stopifnot(p$id == "44450")
stopifnot(p$name == "Svartoxid PBk11")
stopifnot(!is.null(p$properties))
stopifnot(!is.null(p$metadata))
cat("   ✓ get_pigment works correctly\n\n")

cat("7. Testing helper functions - get_pigment_property...\n")
oil <- get_pigment_property("44450", "oil")
stopifnot(oil == 15)
K <- get_pigment_property("44450", "K")
stopifnot(K == 2.40)
cat("   ✓ get_pigment_property works correctly\n\n")

cat("8. Testing helper functions - is_raa_pigment...\n")
stopifnot(is_raa_pigment("vitbas") == TRUE)
stopifnot(is_raa_pigment("J318") == TRUE)
stopifnot(is_raa_pigment("44450") == FALSE)
cat("   ✓ is_raa_pigment works correctly\n\n")

cat("9. Testing helper functions - get_raa_pigments...\n")
raa_list <- get_raa_pigments()
stopifnot(length(raa_list) == 24)
stopifnot("vitbas" %in% names(raa_list))
cat("   ✓ get_raa_pigments works correctly\n\n")

cat("10. Testing helper functions - get_pigments_by_category...\n")
blacks <- get_pigments_by_category("black")
stopifnot(length(blacks) >= 5)
stopifnot("44450" %in% names(blacks))
stopifnot("J318" %in% names(blacks))
cat("   ✓ get_pigments_by_category works correctly\n\n")

cat("11. Testing computed pigments...\n")
vitbas <- get_pigment("vitbas")
stopifnot(isTRUE(vitbas$metadata$is_computed))
stopifnot(all(vitbas$metadata$components == c("44100", "44400")))
go94_gu30 <- get_pigment("GO94_GU30")
stopifnot(isTRUE(go94_gu30$metadata$is_computed))
stopifnot(all(go94_gu30$metadata$components == c("GO94", "GU30")))
cat("   ✓ Computed pigments marked correctly\n\n")

cat("12. Testing tar and materials data...\n")
stopifnot(exists("tar_colors"))
stopifnot(exists("tar_suppliers"))
stopifnot(exists("misc_materials"))
stopifnot(length(tar_colors) == 3)
stopifnot(exists("get_tars_by_category"))
stopifnot(exists("create_filler_choices"))
cat("   ✓ Tar and materials data loaded correctly\n\n")

cat("=== ALL TESTS PASSED ===\n")
cat("\nUnified pigment database migration successful!\n")
cat("- 56 pigments migrated\n")
cat("- 24 RAÄ pigments identified\n")
cat("- Backward compatibility maintained\n")
cat("- New helper functions available\n")
