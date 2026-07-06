
# ERA × Carob — Column Mapping
# Generates report/era_carob_mapping.xlsx
#
# Requires carob_index in your environment (run era_carob_merge.R first).
# The full Carob schema (all 320 cols) is taken from carob_index$wide so the
# Excel always reflects the live schema — known ERA mappings are pre-filled,
# and any Carob column not yet mapped is listed as "Not mapped".
#
# Install once: install.packages("openxlsx")

library(openxlsx)
library(dplyr)

if (!exists("carob_index")) stop("Run era_carob_merge.R first to create carob_index.")

# ============================================================
# Step 1 — Known mappings (pre-populated from era_carob.R)
# Columns:
#   carob_variable  — name in the Carob wide schema
#   era_table       — ERA sub-table (or ERA_Compiled / Derived / —)
#   era_variable    — ERA column name(s) used as source
#   mapping_status  — Mapped | Derived | ERA-only | Not mapped
#   notes           — transformation or caveats
# ============================================================

known <- data.frame(stringsAsFactors = FALSE,

  carob_variable = c(
    # --- Identifiers & provenance ---
    "uri",
    "reference",
    "dataset_id",
    "record_id",
    "trial_id",

    # --- Site & location ---
    "location",
    "on_farm",
    "country",
    "latitude",
    "longitude",
    "elevation",
    "rain",
    "geo_from_source",

    # --- Experiment design ---
    "plot_area",
    "rep",
    "is_survey",

    # --- Crop & timing ---
    "crop",
    "variety",
    "maturity_days",
    "intercrops",
    "crop_rotation",
    "planting_date",
    "planting_end",
    "harvest_date",
    "harvest_end",
    "yield",
    "yield_part",
    "yield_moisture",

    # --- Planting management ---
    "treatment",
    "planting_method",
    "planting_implement",
    "plant_density",
    "seed_density",
    "seed_rate",
    "row_spacing",

    # --- Soil preparation ---
    "land_prep_method",
    "land_prep_implement",

    # --- Inorganic fertilizer ---
    "N_fertilizer",
    "P_fertilizer",
    "K_fertilizer",
    "fertilizer_type",
    "fertilizer_amount",

    # --- Organic matter ---
    "N_organic",
    "P_organic",
    "K_organic",
    "OM_used",

    # --- Irrigation ---
    "irrigated",
    "irrigation_amount",
    "irrigation_method",
    "irrigation_date",
    "irrigation_date_end",

    # --- Herbicides ---
    "herbicide_used",
    "herbicide_method",
    "herbicide_implement",
    "herbicide_amount",
    "herbicide_product",

    # --- Insecticides ---
    "insecticide_used",
    "insecticide_method",
    "insecticide_implement",
    "insecticide_amount",
    "insecticide_product",

    # --- Fungicides ---
    "fungicide_used",
    "fungicide_method",
    "fungicide_implement",
    "fungicide_amount",
    "fungicide_product",

    # --- Biopesticides ---
    "pesticide_used",
    "pesticide_used_method",
    "pesticide_used_implement",
    "pesticide_used_amount",
    "pesticide_product",

    # --- Soil properties ---
    "soil_type",
    "soil_clay",
    "soil_sand",
    "soil_silt",
    "soil_pH",
    "soil_EC",
    "soil_bd",
    "soil_P_total",
    "soil_P_available",
    "soil_depth",
    "depth_top",
    "depth_bottom",
    "soil_SOC",
    "soil_ex_SOC",
    "soil_SOM",
    "soil_N",
    "soil_total_N",
    "soil_CO2",

    # --- ERA-only (not in Carob schema) ---
    "year",
    "seasonal_prep",
    "total_prec",
    "temp",
    "tmax",
    "tmin",
    "dsign",
    "control_T",
    "treatment_type"
  ),

  era_table = c(
    # Identifiers & provenance
    "Data.Out", "Data.Out", "Data.Out", "—", "—",
    # Site & location
    "Data.Out", "Data.Out", "Data.Out", "Data.Out", "Data.Out", "Data.Out", "Data.Out", "—",
    # Experiment design
    "Data.Out", "Data.Out + Prod.Out", "—",
    # Crop & timing
    "Data.Out", "Plant.Method + Prod.Out", "Plant.Method", "Data.Out", "Data.Out",
    "Plant.Method", "Plant.Method", "Plant.Method", "Plant.Method",
    "Prod.Out (long→wide)", "Prod.Out", "—",
    # Planting management
    "Data.Out + Fert.Method", "Plant.Method", "Plant.Method", "Plant.Method",
    "Plant.Method", "Plant.Method", "Plant.Method",
    # Soil preparation
    "Till.Out", "Till.Out",
    # Inorganic fertilizer
    "Fert.Method", "Fert.Method", "Fert.Method", "—", "—",
    # Organic matter
    "Fert.Method", "Fert.Method", "Fert.Method", "Fert.Method",
    # Irrigation
    "Irrig.Method", "Irrig.Method", "Irrig.Method", "Irrig.Method", "Irrig.Method",
    # Herbicides
    "Chems.Out", "Chems.Out", "Chems.Out", "Chems.Out", "Chems.Out",
    # Insecticides
    "Chems.Out", "Chems.Out", "Chems.Out", "Chems.Out", "Chems.Out",
    # Fungicides
    "Chems.Out", "Chems.Out", "Chems.Out", "Chems.Out", "Chems.Out",
    # Biopesticides
    "Chems.Out", "Chems.Out", "Chems.Out", "Chems.Out", "Chems.Out",
    # Soil properties
    "Data.Out", "Soil.Out", "Soil.Out", "Soil.Out", "Soil.Out", "Soil.Out",
    "Soil.Out", "Soil.Out", "Soil.Out", "Soil.Out",
    "Soil.Out (derived in merge)", "Soil.Out (derived in merge)",
    "Prod.Out (long→wide)", "Prod.Out (long→wide)", "Prod.Out (long→wide)",
    "Prod.Out (long→wide)", "Prod.Out (long→wide)", "Prod.Out (long→wide)",
    # ERA-only
    "Data.Out", "Data.Out", "Data.Out", "Data.Out", "Data.Out", "Data.Out",
    "Data.Out", "Data.Out", "Data.Out"
  ),

  era_variable = c(
    # Identifiers & provenance
    "B.DOI", "B.Author.Last", "B.Code", "—", "—",
    # Site & location
    "Site.ID", "Site.Type", "Country", "Site.LatD", "Site.LonD",
    "Site.Elevation", "Site.MAP", "—",
    # Experiment design
    "EX.HPlot.Size", "T.Reps / ED.Reps", "—",
    # Crop & timing
    "P.Product", "V.Var / ED.Variety", "V.Maturity", "IN.Prod", "R.Prod.Seq",
    "PD.Plant.Start", "PD.Plant.End", "PD.Harvest.Start", "PD.Harvest.End",
    "Crop_Yield", "ED.Product.Comp / ED.Product.Comp.L1", "—",
    # Planting management
    "T.Name / F.Level.Name", "Plant.Method", "Plant.Mechanization", "Plant.Density",
    "Plant.Density (units: seeds/ha, kg seed/ha)", "Plant.Density (units: kg/ha)", "Plant.Row",
    # Soil preparation
    "T.Method / Till.Other", "T.Mechanization",
    # Inorganic fertilizer
    "F.NI", "F.PI / F.P2O5", "F.KI / F.K2O", "—", "—",
    # Organic matter
    "F.NO", "F.PO", "F.KO", "F.NO + F.PO + F.KO",
    # Irrigation
    "I.Amount (non-zero → TRUE)", "I.Amount", "I.Method",
    "I.Date.Start / I.Date.Gen", "I.Date.End",
    # Herbicides
    "C.Type (contains 'Herbicide')", "C.App.Method", "C.Mechanization", "C.Amount", "C.Name",
    # Insecticides
    "C.Type (contains 'Insecticide')", "C.App.Method", "C.Mechanization", "C.Amount", "C.Name",
    # Fungicides
    "C.Type (contains 'Fungicide')", "C.App.Method", "C.Mechanization", "C.Amount", "C.Name",
    # Biopesticides
    "C.Type (contains 'Biopesticide')", "C.App.Method", "C.Mechanization", "C.Amount", "C.Name",
    # Soil properties
    "Site.Soil.Texture", "CLY", "SND", "SLT", "soil_pH", "soil_EC", "soil_BD",
    "soil_TP", "soil_AP", "Soil.Upper + Soil.Lower",
    "Soil.Upper", "Soil.Lower",
    "Soil_Organic_Carbon", "Soil_Organic_Carbon_(Change)", "Soil_Organic_Matter",
    "Soil_Nitrogen", "Soil_Total_Nitrogen", "Carbon_Dioxide_Emissions",
    # ERA-only
    "Time (substr 1:4)", "Time.Clim.SP", "Time.Clim.TAP", "Time.Clim.Temp.Mean",
    "Time.Clim.Temp.Max", "Time.Clim.Temp.Min",
    "EX.Design", "T.Control", "T.Control (derived)"
  ),

  mapping_status = c(
    # Identifiers & provenance
    "Mapped", "Mapped", "Mapped", "Not mapped", "Not mapped",
    # Site & location
    "Mapped", "Mapped", "Mapped", "Mapped", "Mapped", "Mapped", "Mapped", "Not mapped",
    # Experiment design
    "Mapped", "Mapped", "Not mapped",
    # Crop & timing
    "Mapped", "Mapped", "Mapped", "Mapped", "Mapped",
    "Mapped", "Mapped", "Mapped", "Mapped",
    "Mapped", "Mapped", "Not mapped",
    # Planting management
    "Mapped", "Mapped", "Mapped", "Mapped", "Derived", "Derived", "Mapped",
    # Soil preparation
    "Mapped", "Mapped",
    # Inorganic fertilizer
    "Mapped", "Mapped", "Mapped", "Not mapped", "Not mapped",
    # Organic matter
    "Mapped", "Mapped", "Mapped", "Derived",
    # Irrigation
    "Derived", "Mapped", "Mapped", "Mapped", "Mapped",
    # Herbicides
    "Derived", "Mapped", "Mapped", "Mapped", "Mapped",
    # Insecticides
    "Derived", "Mapped", "Mapped", "Mapped", "Mapped",
    # Fungicides
    "Derived", "Mapped", "Mapped", "Mapped", "Mapped",
    # Biopesticides
    "Derived", "Mapped", "Mapped", "Mapped", "Mapped",
    # Soil properties
    "Mapped", "Mapped", "Mapped", "Mapped", "Mapped", "Mapped", "Mapped",
    "Mapped", "Mapped", "Mapped", "Derived", "Derived",
    "Mapped", "Mapped", "Mapped", "Mapped", "Mapped", "Mapped",
    # ERA-only
    "ERA-only", "ERA-only", "ERA-only", "ERA-only", "ERA-only", "ERA-only",
    "ERA-only", "ERA-only", "ERA-only"
  ),

  notes = c(
    # Identifiers & provenance
    "Journal article DOI", "First author last name", "ERA study code (e.g. DK0071)",
    "No equivalent in ERA", "No equivalent in ERA",
    # Site & location
    "Site/location name", "On-farm vs research station",
    "Multi-country strings cleaned with ifelse chain (e.g. 'Benin..Togo' → 'Benin')",
    "Cleaned: truncated to 6 chars, extra decimal points removed",
    "Cleaned: truncated to 6 chars, extra decimal points removed",
    "", "Mean annual precipitation",
    "No equivalent in ERA",
    # Experiment design
    "Plot size in hectares", "Falls back to ED.Reps if T.Reps is missing",
    "No equivalent in ERA",
    # Crop & timing
    "", "Falls back to ED.Variety if V.Var is missing", "",
    "ERA delimiter '***' replaced with ';'", "ERA delimiter '|' replaced with ';'",
    "", "", "", "",
    "Pivoted from long format (Out.Subind = Crop_Yield)",
    "Harmonised to controlled vocabulary (grain, pod, roots, stems, ...)",
    "No equivalent in ERA",
    # Planting management
    "Falls back to F.Level.Name if T.Name is missing",
    "", "", "Split into seed_density/seed_rate based on Plant.Density.Unit; then set to NA where seed_ fields are populated",
    "Units: seeds/ha, kg seed/ha, seed clusters/ha, or /m2 x10000",
    "Units: kg/ha",
    "",
    # Soil preparation
    "Falls back to Till.Other if T.Method is missing; harmonised to controlled vocabulary",
    "",
    # Inorganic fertilizer
    "Cleaned: sentinel values 999/999999 -> NA; first token extracted as numeric",
    "Falls back to F.P2O5 if F.PI is missing; same cleaning as N",
    "Falls back to F.K2O if F.KI is missing; same cleaning as N",
    "No equivalent in ERA (fertilizer trade name not recorded)",
    "No equivalent in ERA (total combined fertilizer amount not recorded)",
    # Organic matter
    "Organic nitrogen", "Organic phosphorus", "Organic potassium",
    "TRUE if any of N_organic / P_organic / K_organic is non-zero",
    # Irrigation
    "TRUE if I.Amount is non-NA and non-zero",
    "", "",
    "Falls back to I.Date.Gen if I.Date.Start is missing", "",
    # Herbicides
    "C.Type filtered for 'Herbicide'", "", "", "", "",
    # Insecticides
    "C.Type filtered for 'Insecticide' or 'Bioinsecticide'", "", "", "", "",
    # Fungicides
    "C.Type filtered for 'Fungicide' or 'Biofungicide'", "", "", "", "",
    # Biopesticides
    "C.Type filtered for 'Biopesticide'", "", "", "", "",
    # Soil properties
    "", "ERA column CLY", "ERA column SND", "ERA column SLT", "",
    "", "ERA column soil_BD", "ERA column soil_TP", "ERA column soil_AP",
    "Constructed as paste0(Soil.Upper, '-', Soil.Lower); split to depth_top/depth_bottom in era_carob_merge.R",
    "Derived in era_carob_merge.R from soil_depth", "Derived in era_carob_merge.R from soil_depth",
    "Pivoted from long format", "Pivoted from long format (SOC change)",
    "Pivoted from long format", "Pivoted from long format",
    "Pivoted from long format", "Pivoted from long format (CO2 emissions)",
    # ERA-only
    "Year only (not in Carob schema); dropped in merge",
    "Seasonal precipitation; not in Carob schema; dropped in merge",
    "Total annual precipitation; not in Carob schema; dropped in merge",
    "Mean annual temperature; not in Carob schema; dropped in merge",
    "Max temperature; not in Carob schema; dropped in merge",
    "Min temperature; not in Carob schema; dropped in merge",
    "Experimental design code; ERA-internal; dropped in merge",
    "Control flag (Yes/No); ERA-internal; dropped in merge",
    "Derived as 'control'/'treatment'; ERA-internal; dropped in merge"
  )
)

# ============================================================
# Step 2 — Build full mapping against live Carob schema
# All 320 carob_index$wide columns appear; unknown ones → "Not mapped"
# ERA-only fields (in dwf but not in Carob schema) are appended at the bottom.
# ============================================================

carob_all_cols <- names(carob_index$wide)

# Rows from known that are actual Carob schema columns
known_carob <- known[known$mapping_status != "ERA-only", ]

# Rows from known that are ERA-only (no Carob equivalent)
known_era_only <- known[known$mapping_status == "ERA-only", ]

# Any Carob column not yet in known_carob gets a blank "Not mapped" row
missing_carob <- setdiff(carob_all_cols, known_carob$carob_variable)
unmapped_rows <- data.frame(
  carob_variable = missing_carob,
  era_table      = "",
  era_variable   = "",
  mapping_status = "Not mapped",
  notes          = "",
  stringsAsFactors = FALSE
)

# Final table: Carob schema columns in schema order, then ERA-only appended
carob_ordered <- data.frame(carob_variable = carob_all_cols, stringsAsFactors = FALSE)

all_carob_rows <- bind_rows(known_carob, unmapped_rows)
mapping <- carob_ordered %>%
  left_join(all_carob_rows, by = "carob_variable") %>%
  mutate(
    mapping_status = ifelse(is.na(mapping_status), "Not mapped", mapping_status),
    era_table      = ifelse(is.na(era_table), "", era_table),
    era_variable   = ifelse(is.na(era_variable), "", era_variable),
    notes          = ifelse(is.na(notes), "", notes)
  )

# Append ERA-only rows at the bottom (they don't belong in Carob schema)
mapping <- bind_rows(mapping, known_era_only)

cat("Total rows in mapping:", nrow(mapping), "\n")
cat("  Carob schema cols: ", length(carob_all_cols), "\n")
cat(table(mapping$mapping_status), "\n")

# ============================================================
# Step 3 — Build Excel workbook with colour-coded rows
# ============================================================

wb <- createWorkbook()
addWorksheet(wb, "ERA-Carob Mapping")

# Header style
hs <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#2d6a4f",
  halign = "LEFT", textDecoration = "Bold", border = "Bottom"
)

# Status colour fills
fill_mapped     <- createStyle(fgFill = "#d8f3dc")   # light green
fill_derived    <- createStyle(fgFill = "#fff3b0")   # light yellow
fill_era_only   <- createStyle(fgFill = "#ffe8cc")   # light orange
fill_not_mapped <- createStyle(fgFill = "#ffd6d6")   # light red

writeData(wb, sheet = 1, mapping, headerStyle = hs)

# Set column widths
setColWidths(wb, sheet = 1, cols = 1:5,
             widths = c(28, 30, 38, 16, 60))

# Apply row colours by status
for (i in seq_len(nrow(mapping))) {
  row_i <- i + 1  # +1 for header
  style <- switch(mapping$mapping_status[i],
    "Mapped"      = fill_mapped,
    "Derived"     = fill_derived,
    "ERA-only"    = fill_era_only,
    "Not mapped"  = fill_not_mapped
  )
  addStyle(wb, sheet = 1, style = style, rows = row_i, cols = 1:5, gridExpand = TRUE)
}

# Freeze top row
freezePane(wb, sheet = 1, firstRow = TRUE)

# Add a legend sheet
addWorksheet(wb, "Legend")
legend_df <- data.frame(
  Colour  = c("Green", "Yellow", "Orange", "Red"),
  Status  = c("Mapped", "Derived", "ERA-only", "Not mapped"),
  Meaning = c(
    "ERA column directly mapped to Carob column",
    "Value computed from one or more ERA columns",
    "Present in ERA but has no equivalent in Carob schema (dropped during merge)",
    "Carob field with no source in ERA — will always be NA for ERA rows"
  )
)
writeData(wb, sheet = "Legend", legend_df)
for (i in seq_len(nrow(legend_df))) {
  fill_i <- switch(legend_df$Status[i],
    "Mapped"     = fill_mapped,
    "Derived"    = fill_derived,
    "ERA-only"   = fill_era_only,
    "Not mapped" = fill_not_mapped
  )
  addStyle(wb, sheet = "Legend", style = fill_i, rows = i + 1, cols = 1:3, gridExpand = TRUE)
}
setColWidths(wb, sheet = "Legend", cols = 1:3, widths = c(12, 14, 70))

# ============================================================
# Save
# ============================================================

dir.create("report", showWarnings = FALSE)
out_path <- "report/era_carob_mapping.xlsx"
saveWorkbook(wb, out_path, overwrite = TRUE)
cat("Saved:", out_path, "\n")
cat("Rows:", nrow(mapping), "\n")
cat(table(mapping$mapping_status), "\n")
