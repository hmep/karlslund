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
cat("   (Skipped - km removed, using pigments_db directly)\n\n")

cat("3. Testing backward compatibility - suppliers structure...\n")
cat("   (Skipped - suppliers removed, using pigments_db directly)\n\n")

cat("4. Testing backward compatibility - raa_pigments list...\n")
cat("   (Skipped - raa_pigments removed, using pigments_db metadata)\n\n")

cat("5. Testing backward compatibility - pigment_name_to_id...\n")
cat("   (Skipped - pigment_name_to_id removed, using pigments_db directly)\n\n")

cat("6. Testing pigments_db - direct access...\n")
p <- pigments_db[["44450"]]
stopifnot(!is.null(p))
stopifnot(p$id == "44450")
stopifnot(p$name == "Svartoxid PBk11")
stopifnot(!is.null(p$properties))
stopifnot(!is.null(p$metadata))
cat("   ✓ Direct pigments_db access works correctly\n\n")

cat("7. Testing pigments_db - properties access...\n")
oil <- pigments_db[["44450"]]$properties$oil
stopifnot(oil == 15)
K <- pigments_db[["44450"]]$properties$K
stopifnot(K == 2.40)
cat("   ✓ Property access works correctly\n\n")

cat("8. Testing pigments_db - RAÄ identification...\n")
stopifnot(isTRUE(pigments_db[["vitbas"]]$metadata$is_raa))
stopifnot(isTRUE(pigments_db[["J318"]]$metadata$is_raa))
stopifnot(!isTRUE(pigments_db[["44450"]]$metadata$is_raa))
cat("   ✓ RAÄ identification works correctly\n\n")

cat("9. Testing pigments_db - getting all RAÄ pigments...\n")
raa_list <- names(pigments_db)[sapply(pigments_db, function(p) isTRUE(p$metadata$is_raa))]
stopifnot(length(raa_list) == 24)
stopifnot("vitbas" %in% raa_list)
cat("   ✓ RAÄ pigment filtering works correctly\n\n")

cat("10. Testing pigments_db - category filtering...\n")
blacks <- names(pigments_db)[sapply(pigments_db, function(p) p$metadata$category == "black")]
stopifnot(length(blacks) >= 5)
stopifnot("44450" %in% blacks)
stopifnot("J318" %in% blacks)
cat("   ✓ Category filtering works correctly\n\n")

cat("11. Testing computed pigments...\n")
vitbas <- pigments_db[["vitbas"]]
stopifnot(isTRUE(vitbas$metadata$is_computed))
stopifnot(all(vitbas$metadata$components == c("44100", "44400")))
go94_gu30 <- pigments_db[["GO94_GU30"]]
stopifnot(isTRUE(go94_gu30$metadata$is_computed))
stopifnot(all(go94_gu30$metadata$components == c("GO94", "GU30")))
cat("   ✓ Computed pigments marked correctly\n\n")

cat("12. Testing miscellaneous materials data...\n")
stopifnot(exists("misc_db"))
stopifnot(length(misc_db) >= 4)  # At least 3 tars + 1 solvent
stopifnot("TAR01" %in% names(misc_db))
stopifnot("TAR02" %in% names(misc_db))
stopifnot("TAR03" %in% names(misc_db))
stopifnot("SOLV01" %in% names(misc_db))
cat("   ✓ misc_db loaded correctly\n\n")

cat("13. Testing misc_db structure for tars...\n")
tar01 <- misc_db[["TAR01"]]
stopifnot(!is.null(tar01))
stopifnot(tar01$id == "TAR01")
stopifnot(!is.null(tar01$name))
stopifnot(!is.null(tar01$properties$rgb))
stopifnot(!is.null(tar01$properties$K))
stopifnot(!is.null(tar01$properties$S))
stopifnot(!is.null(tar01$suppliers))
stopifnot(!is.null(tar01$metadata))
stopifnot(tar01$metadata$category == "tar")
cat("   ✓ Tar entries have correct structure\n\n")

cat("14. Testing tar properties...\n")
rgb <- misc_db[["TAR02"]]$properties$rgb
stopifnot(!is.null(rgb))
stopifnot(length(rgb) == 3)
K <- misc_db[["TAR02"]]$properties$K
stopifnot(!is.null(K))
stopifnot(K == 0.35)
cat("   ✓ Tar properties accessible correctly\n\n")

cat("15. Testing solvent in misc_db...\n")
solv <- misc_db[["SOLV01"]]
stopifnot(!is.null(solv))
stopifnot(solv$name == "Balsamterpentin")
stopifnot(solv$metadata$category == "solvent")
stopifnot(!is.null(solv$suppliers))
cat("   ✓ Solvent entry has correct structure\n\n")

cat("=== ALL TESTS PASSED ===\n")
cat("\nUnified databases verified successfully!\n")
cat("- 56 pigments in pigments_db\n")
cat("- 24 RAÄ pigments identified\n")
cat("- 4+ materials in misc_db (3 tars, 1+ solvents)\n")
cat("- All structures consistent\n")
