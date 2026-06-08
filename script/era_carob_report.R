
# ERA x Carob — Coverage Report
# Run era_carob.R then era_carob_merge.R first.
# Requires: merged, dwf, carob_index, era_index, already_in_carob

library(dplyr)
library(ggplot2)
library(tidyr)

dir.create("report", showWarnings = FALSE)

# ============================================================
# 1. What ERA added to Carob — headline numbers
# ============================================================

era_rows   <- sum(merged$source == "era",   na.rm = TRUE)
carob_rows <- sum(merged$source == "carob", na.rm = TRUE)

era_studies   <- length(unique(merged$dataset_id[merged$source == "era"]))
carob_studies <- length(unique(merged$dataset_id[merged$source == "carob"]))

era_countries   <- unique(merged$country[merged$source == "era"   & !is.na(merged$country)])
carob_countries <- unique(merged$country[merged$source == "carob" & !is.na(merged$country)])
new_countries   <- setdiff(era_countries, carob_countries)

era_crops   <- unique(tolower(trimws(merged$crop[merged$source == "era"   & !is.na(merged$crop)])))
carob_crops <- unique(tolower(trimws(merged$crop[merged$source == "carob" & !is.na(merged$crop)])))
new_crops   <- setdiff(era_crops, carob_crops)

cat("=== ERA contribution to Carob ===\n")
cat("ERA rows added:         ", era_rows, "\n")
cat("ERA studies added:      ", era_studies, "(of which", nrow(already_in_carob), "already existed in Carob)\n")
cat("New countries from ERA: ", length(new_countries), "\n")
if (length(new_countries) > 0) print(sort(new_countries))
cat("New crops from ERA:     ", length(new_crops), "\n")
if (length(new_crops) > 0) print(sort(new_crops))

# ============================================================
# 2. Studies by source and crop
# ============================================================

crop_summary <- merged %>%
  filter(!is.na(crop) & !is.na(yield)) %>%
  group_by(crop = tolower(trimws(crop)), source) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  group_by(crop) %>%
  mutate(total = sum(n_obs)) %>%
  ungroup() %>%
  filter(total >= 50) %>%
  arrange(desc(total))

p1 <- ggplot(crop_summary, aes(x = reorder(crop, total), y = n_obs, fill = source)) +
  geom_col() +
  scale_fill_manual(values = c("carob" = "#52b788", "era" = "#2d6a4f"),
                    labels = c("carob" = "Carob (existing)", "era" = "ERA (added)")) +
  coord_flip() +
  labs(
    title = "Observations with yield data by crop and source",
    subtitle = "Crops with ≥ 50 observations",
    x = NULL, y = "Number of observations", fill = "Source"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("report/01_crop_coverage.png", p1, width = 8, height = 6, dpi = 150)
cat("Saved: report/01_crop_coverage.png\n")

# ============================================================
# 3. Country coverage
# ============================================================

country_summary <- merged %>%
  filter(!is.na(country)) %>%
  group_by(country, source) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  group_by(country) %>%
  mutate(total = sum(n_obs)) %>%
  ungroup() %>%
  filter(total >= 100) %>%
  arrange(desc(total))

p2 <- ggplot(country_summary, aes(x = reorder(country, total), y = n_obs, fill = source)) +
  geom_col() +
  scale_fill_manual(values = c("carob" = "#52b788", "era" = "#2d6a4f")) +
  coord_flip() +
  labs(
    title = "Observations by country and source",
    subtitle = "Countries with ≥ 100 observations",
    x = NULL, y = "Number of observations", fill = "Source"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("report/02_country_coverage.png", p2, width = 8, height = 7, dpi = 150)
cat("Saved: report/02_country_coverage.png\n")

# ============================================================
# 4. Completeness of ERA rows vs Carob rows
#    How many key Carob fields are populated in ERA-sourced rows?
# ============================================================

key_fields <- c(
  "yield", "crop", "country", "latitude", "longitude",
  "N_fertilizer", "P_fertilizer", "K_fertilizer",
  "irrigated", "land_prep_method", "planting_date",
  "variety", "plant_density", "soil_pH", "soil_SOC",
  "soil_N", "soil_clay", "soil_sand", "rain",
  "record_id", "trial_id", "is_survey", "yield_moisture",
  "geo_from_source", "depth_top", "depth_bottom",
  "fertilizer_type", "fertilizer_amount"
)

completeness <- merged %>%
  filter(source %in% c("era", "carob")) %>%
  select(source, all_of(intersect(key_fields, names(merged)))) %>%
  group_by(source) %>%
  summarise(across(everything(), ~ round(100 * mean(!is.na(.)), 1))) %>%
  pivot_longer(-source, names_to = "field", values_to = "pct_complete")

p3 <- ggplot(completeness, aes(x = reorder(field, pct_complete), y = pct_complete, fill = source)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("carob" = "#52b788", "era" = "#2d6a4f")) +
  coord_flip() +
  labs(
    title = "Field completeness: ERA vs Carob rows",
    subtitle = "% of rows where field is non-NA",
    x = NULL, y = "% complete", fill = "Source"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("report/03_field_completeness.png", p3, width = 9, height = 7, dpi = 150)
cat("Saved: report/03_field_completeness.png\n")

# ============================================================
# 5. Fields entirely missing from ERA (0% complete in ERA rows)
# ============================================================

era_completeness <- completeness %>%
  filter(source == "era") %>%
  arrange(pct_complete)

missing_in_era <- era_completeness %>% filter(pct_complete == 0)
sparse_in_era  <- era_completeness %>% filter(pct_complete > 0 & pct_complete < 25)

cat("\n=== Fields with 0% completeness in ERA rows ===\n")
print(missing_in_era$field)

cat("\n=== Fields with <25% completeness in ERA rows ===\n")
print(sparse_in_era)

# ============================================================
# 6. Study overlap summary table
# ============================================================

overlap_table <- already_in_carob[, c("dataset_id", "uri", "carob_id")]
write.csv(overlap_table, "report/era_carob_overlap.csv", row.names = FALSE)
cat("Saved: report/era_carob_overlap.csv\n",
    " —", nrow(overlap_table), "ERA studies that already existed in Carob\n")

# ============================================================
# 7. ERA-only columns that were dropped (what ERA has that Carob doesn't)
# ============================================================

era_only_cols <- setdiff(names(dwf), names(carob_index$wide))

cat("\n=== ERA columns dropped (not in Carob schema) ===\n")
cat("Total:", length(era_only_cols), "\n\n")

# Classify them
economic   <- era_only_cols[grepl("Cost|Margin|Return|Benefit|Labour|Productivity|Rate_of", era_only_cols)]
climate    <- era_only_cols[grepl("seasonal_prep|total_prec|tmax|tmin", era_only_cols)]
internal   <- era_only_cols[grepl("dsign|control_T|treatment_type|id$", era_only_cols)]
outcomes   <- era_only_cols[grepl("Yield|Ratio|Efficiency|Emissions|Biomass|Erosion|Runoff|Biodiversity|Carbon|Nitrogen|Phosphorus|Potassium|Water_Use|Moisture|Infiltration", era_only_cols)]
other      <- setdiff(era_only_cols, c(economic, climate, internal, outcomes))

dropped_summary <- data.frame(
  category = c("Economic outcomes", "ERA climate fields", "ERA internal fields", "Unmapped outcomes", "Other"),
  n_cols   = c(length(economic), length(climate), length(internal), length(outcomes), length(other)),
  examples = c(
    paste(head(economic, 3), collapse = ", "),
    paste(climate, collapse = ", "),
    paste(internal, collapse = ", "),
    paste(head(outcomes, 3), collapse = ", "),
    paste(head(other, 3), collapse = ", ")
  )
)

cat("--- Dropped columns by category ---\n")
print(dropped_summary)
write.csv(dropped_summary, "report/era_dropped_columns.csv", row.names = FALSE)
cat("Saved: report/era_dropped_columns.csv\n")

# ============================================================
# 8. Export small CSVs for README.Rmd
# ============================================================

# Field completeness — used by README.Rmd
write.csv(completeness, "report/field_completeness.csv", row.names = FALSE)

# Coverage summary headline numbers — used by README.Rmd
coverage_summary <- data.frame(
  metric = c(
    "ERA observations added",
    "ERA studies added",
    "ERA studies already in Carob",
    "New countries from ERA",
    "New crops from ERA",
    "Total observations (merged)",
    "Total columns (merged)"
  ),
  value = c(
    era_rows,
    era_studies,
    nrow(already_in_carob),
    length(new_countries),
    length(new_crops),
    nrow(merged),
    ncol(merged)
  )
)
write.csv(coverage_summary, "report/coverage_summary.csv", row.names = FALSE)
cat("Saved: report/field_completeness.csv\n")
cat("Saved: report/coverage_summary.csv\n")

cat("\n=== Report complete. Files saved to report/ ===\n")
cat("To update README.md with latest figures and stats, run:\n")
cat("  rmarkdown::render('README.Rmd')\n")
