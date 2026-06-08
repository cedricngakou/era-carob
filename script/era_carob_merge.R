
# ERA <-> Carob Merge Script
# Run era_carob.R first — this script requires `dwf` to be in your environment.
#
# Step 1: Identify which ERA studies are already in Carob (by DOI)
# Step 2: Audit and align ERA columns to the Carob schema
# Step 3: Merge ERA and Carob into a single unified dataset (`merged`)

library(caramba)
library(dplyr)

# ============================================================
# Task 1: Identify duplicate datasets
# ============================================================

# One row per ERA study with its DOI
era_index <- dwf %>%
  distinct(dataset_id, uri) %>%
  filter(!is.na(uri) & uri != "")

# All datasets currently in Carob
carob_index <- carob_collection(group = "agronomy")

# ERA uri = journal article DOI (plain, no prefix)
# Carob publication = journal article DOI (doi: prefix, some _ instead of /)
# Normalise Carob: strip "doi:", replace _ with /, lowercase
carob_pub_norm <- tolower(trimws(
  gsub("_", "/", gsub("^doi:", "", carob_index$metadata$publication))
))

# Build lookup: normalised publication DOI -> carob dataset_id
carob_lookup <- data.frame(
  pub_doi   = carob_pub_norm,
  carob_id  = carob_index$metadata$dataset_id,
  carob_uri = carob_index$metadata$uri,
  stringsAsFactors = FALSE
) %>% filter(pub_doi != "")

era_index$uri_norm <- tolower(trimws(era_index$uri))
era_index <- era_index %>%
  left_join(carob_lookup, by = c("uri_norm" = "pub_doi"))

era_index$in_carob <- !is.na(era_index$carob_id)

already_in_carob   <- era_index %>% filter(in_carob)
missing_from_carob <- era_index %>% filter(!in_carob)

cat("=== Task 1: ERA vs Carob duplicate check ===\n")
cat("Total ERA studies:       ", nrow(era_index), "\n")
cat("Already in Carob:        ", nrow(already_in_carob), "\n")
cat("Not yet in Carob:        ", nrow(missing_from_carob), "\n\n")

cat("--- Studies already in Carob (ERA dataset_id | journal DOI | Carob ID) ---\n")
print(already_in_carob[, c("dataset_id", "uri", "carob_id")])

# ============================================================
# Task 2 & 3: Align ERA to Carob schema and merge
# ============================================================

# Full Carob schema comes from the wide table, not a single dataset
carob_cols <- names(carob_index$wide)
era_cols   <- names(dwf)

cat("\n=== Task 2: Schema audit (vs full Carob wide schema) ===\n")
cat("Missing from dwf (added as NA): ", paste(setdiff(carob_cols, era_cols), collapse = ", "), "\n")
cat("ERA-only cols (dropped):         ", paste(setdiff(era_cols, carob_cols), collapse = ", "), "\n")

# ---- Fix case clashes ----
dwf_aligned <- dwf
dwf_aligned$Soil_NO3 <- NULL   # lowercase soil_NO3 already exists
dwf_aligned$Soil_NH4 <- NULL   # lowercase soil_NH4 already exists

# soil_depth in ERA is "upper-lower" string; Carob uses depth_top + depth_bottom
if ("soil_depth" %in% names(dwf_aligned) && !"depth_top" %in% names(dwf_aligned)) {
  dwf_aligned$depth_top    <- as.numeric(gsub("-.*", "", dwf_aligned$soil_depth))
  dwf_aligned$depth_bottom <- as.numeric(gsub(".*-", "", dwf_aligned$soil_depth))
  dwf_aligned$soil_depth   <- NULL
}

# ---- Drop spurious NA-named column if present ----
dwf_aligned <- dwf_aligned[, !is.na(names(dwf_aligned)) & names(dwf_aligned) != "NA"]

# ---- Add missing Carob columns as NA ----
for (col in setdiff(carob_cols, names(dwf_aligned))) {
  dwf_aligned[[col]] <- NA
}

# ---- Keep only Carob schema columns ----
dwf_carob <- dwf_aligned[, carob_cols]

cat("\nRows from ERA:   ", nrow(dwf_carob), "\n")
cat("Rows from Carob: ", nrow(carob_index$wide), "\n")

# ---- Merge ----
merged <- do.call(carobiner::bindr, list(carob_index$wide, dwf_carob))

merged$source <- c(
  rep("carob", nrow(carob_index$wide)),
  rep("era",   nrow(dwf_carob))
)

cat("Total rows after merge:", nrow(merged), "\n")
cat("Columns:               ", ncol(merged), "\n")

