# ERA → Carob: Transforming ERA Agronomic Data into the Carob Standard Format

This repository contains R scripts that load the ERA agronomic database, reformat it to match the [Carob](https://carob-data.org) data standard, and merge it with the existing Carob dataset to create a unified meta-analysis resource.

---

<details>
<summary><strong>What is ERA?</strong></summary>

ERA (Evidence for Resilient Agriculture) is a peer-reviewed agronomic database maintained by CGIAR. It contains over 110,000 observations from ~1,720 studies across 51 countries, covering crops, soils, management practices, and yield outcomes. ERA is structured as a set of relational tables (treatments, fertilizers, tillage, irrigation, soil, etc.) linked by study and site identifiers.

</details>

<details>
<summary><strong>What is Carob?</strong></summary>

Carob is an open agricultural data platform that standardises experimental datasets into a common schema. It currently holds ~238,000 observations across ~197 datasets. The Carob schema defines canonical column names, units, and vocabularies for fields like `yield`, `N_fertilizer`, `land_prep_method`, `irrigated`, etc.

</details>

---

## Scripts

| Script | Purpose | Output |
|--------|---------|--------|
| `script/era_carob.R` | Transform ERA → Carob format | `dwf` |
| `script/era_carob_merge.R` | Deduplicate, align schema, merge | `merged` |
| `script/era_carob_report.R` | Coverage report + visualisations | Plots + CSV |

Run in order: `era_carob.R` → `era_carob_merge.R` → `era_carob_report.R`

---

## `era_carob.R` — ERA to Carob format

<details>
<summary><strong>Step 1 — Data loading</strong></summary>

ERA data is loaded from a public S3 bucket (`s3://digital-atlas/era`) rather than a local file. The script:
- Finds the latest `era_agronomy_bundle_*.tar.gz` in the S3 bucket
- Downloads and extracts it to `downloaded_data/` (only if not already present — cached)
- Loads three files:
  - `agronomic_*.json` → `era_merge` (the relational table list)
  - `era_master_codes*.json` → `era_master` (vocabulary/lookup codes)
  - `era_compiled*.parquet` → `ERA_Compiled` (compiled flat table)

> `downloaded_data/` is in `.gitignore` — the bundle is too large (~473 MB) for GitHub.

</details>

<details>
<summary><strong>Step 2 — Merging ERA sub-tables</strong></summary>

ERA stores data across 12 relational sub-tables. The script merges them all into a single flat data frame `rb` using `carobiner::bindr` and `merge()` on shared columns:

| Sub-table | Content |
|-----------|---------|
| `Data.Out` | Core outcome and site data (baseline) |
| `Prod.Out` | Production/yield variables |
| `Till.Out` | Tillage method and mechanisation |
| `Plant.Method` | Planting density, method, row spacing |
| `Fert.Method` | Fertilizer type, amount, timing |
| `Chems.Out` | Herbicides, insecticides, fungicides, biopesticides |
| `Res.Method` | Residue management |
| `Res.Comp` | Residue composition |
| `pH.Out` / `pH.Method` | Soil pH measurements and method |
| `Irrig.Method` | Irrigation amount, method, dates |
| `WH.Out` | Water harvesting |
| `Soil.Out` | Soil properties (long format → pivoted to wide per study) |

**Decision — soil pivot:** Soil data in ERA is in long format (one row per soil variable). The script pivots it to wide format per `B.Code` (study) to avoid column conflicts across studies with different soil measurement sets.

**Decision — filtering:**
- Rows where `Site.ID == "All Sites"` are removed (aggregated pseudo-sites with no real coordinates)
- Rows where `Product.Type == "Animal"` are removed (only crop outcomes are retained)

</details>

<details>
<summary><strong>Step 3 — Variable selection and renaming</strong></summary>

The merged table `rb` has 400+ ERA-specific column names. The script builds a new data frame `d` mapping ERA columns to Carob names:

| ERA column | Carob column | Notes |
|------------|-------------|-------|
| `B.DOI` | `uri` | Journal article DOI |
| `B.Author.Last` | `reference` | Last name of first author |
| `B.Code` | `dataset_id` | ERA study code |
| `Site.ID` | `location` | Site name |
| `Site.Type` | `on_farm` | On-farm vs research station |
| `Site.LatD` / `Site.LonD` | `latitude` / `longitude` | Truncated to 6 chars before cleaning |
| `Site.MAP` | `rain` | Mean annual precipitation |
| `Site.Elevation` | `elevation` | |
| `Site.Soil.Texture` | `soil_type` | |
| `Time.Clim.Temp.Mean/Max/Min` | `temp` / `tmax` / `tmin` | ERA-specific — not in Carob schema |
| `Time.Clim.SP` | `seasonal_prep` | ERA-specific — not in Carob schema |
| `EX.Design` | `dsign` | Experimental design |
| `P.Product` | `crop` | Crop species |
| `T.Name` / `F.Level.Name` | `treatment` | Falls back to fertilizer level name if treatment name is missing |
| `T.Control` | `control_T` | TRUE/FALSE control flag |
| `F.NI` | `N_fertilizer` | Inorganic N |
| `F.PI` / `F.P2O5` | `P_fertilizer` | Uses P2O5 if PI is missing |
| `F.KI` / `F.K2O` | `K_fertilizer` | Uses K2O if KI is missing |
| `I.Amount` | `irrigation_amount` | Fixed: was incorrectly assigned `I.Method` |
| `I.Method` | `irrigation_method` | Added — was missing from original script |
| `Plant.Density` | `plant_density` | Split into `seed_density` / `seed_rate` based on units |
| `C.Type` + `C.Amount` etc. | `herbicide_*` / `insecticide_*` / `fungicide_*` / `pesticide_*` | Detected from `C.Type` using `grepl` |

</details>

<details>
<summary><strong>Step 4 — Seed density unit standardisation</strong></summary>

ERA stores planting density with a separate units column. The script splits into three Carob fields:

- `seed_density` — seeds or kg seed per ha (converted from per m² × 10,000 where needed)
- `seed_rate` — kg/ha seeding rate
- `plant_density` — plants per ha (converted from per m² × 10,000 where needed)

**Decision:** Once a row is classified into `seed_density` or `seed_rate`, `plant_density` is set to NA to avoid double-counting. The `units` column is then dropped.

</details>

<details>
<summary><strong>Step 5 — Country name cleaning</strong></summary>

ERA stores some country values as combined strings. The script resolves these via a hardcoded `ifelse` chain:

| ERA value | Resolved to |
|-----------|------------|
| `"Benin..Togo"` | `"Benin"` |
| `"Ghana..Benin"` | `"Ghana"` |
| `"Kenya..Kenya"` | `"Kenya"` |
| `"Drc"` / `"Congo"` | `"Democratic Republic of Congo"` |

> **Known limitation:** Only a subset of multi-country strings are handled. Unmatched values pass through unchanged.

</details>

<details>
<summary><strong>Step 6 — Intercrop parsing</strong></summary>

ERA stores intercrop species as `***`-delimited strings (e.g. `"Maize***Beans***Sorghum"`). The script splits these, standardises a small number of long scientific names (e.g. `"Mangifera indica"` → `"Mangifera"`), and rejoins with `;` as the Carob separator.

</details>

<details>
<summary><strong>Step 7 — Organic matter flag</strong></summary>

`OM_used` is set to `TRUE` if any of `N_organic`, `P_organic`, or `K_organic` is non-zero. This is a derived binary flag not present in the raw ERA data.

</details>

<details>
<summary><strong>Step 8 — Coordinate cleaning</strong></summary>

Latitude and longitude values in ERA sometimes contain multiple decimal points (e.g. `"1.23.45"`). The cleaning pipeline:
1. Truncates to 6 characters
2. Removes extra decimal points (keep only the first)
3. Applies `carobiner::fix_name()` to handle encoding artefacts
4. Strips trailing dots
5. Converts to numeric

</details>

<details>
<summary><strong>Step 9 — Fertilizer value cleaning</strong></summary>

N, P, and K fertilizer values can contain concatenated strings, NA artefacts, and sentinel values. Per nutrient:
1. `carobiner::fix_name()` — fixes encoding
2. Remove `NA.` / `.NA` artefacts
3. Replace multiple dots with spaces
4. Replace ERA sentinel values `999` / `999999` with `NA`
5. Extract first space-delimited token and convert to numeric

> **Fixed bug:** The original script used `substr(..., 1, 3)` which truncated values ≥ 1000 (e.g. 1200 → 120). Replaced with `gsub("\\s.*", "")`.

</details>

<details>
<summary><strong>Step 10 — Long-to-wide pivot (response variables)</strong></summary>

ERA stores outcome variables in long format. The script pivots to wide format per study using `proc()`, then renames key columns:

| ERA variable name | Carob column |
|-------------------|-------------|
| `Crop_Yield` | `yield` |
| `Soil_Organic_Carbon` | `soil_SOC` |
| `Soil_Total_Nitrogen` | `soil_total_N` |
| `Soil_Nitrogen` | `soil_N` |
| `Soil_Organic_Matter` | `soil_SOM` |
| `Carbon_Dioxide_Emissions` | `soil_CO2` |
| `Soil_Organic_Carbon_(Change)` | `soil_ex_SOC` |

</details>

<details>
<summary><strong>Step 11 — Land preparation method harmonisation</strong></summary>

Harmonised from ERA free-text codes to a controlled vocabulary. Fallback: if `T.Method` is missing, `Till.Other` is used.

| ERA codes matched | Carob value |
|-------------------|-------------|
| `CT`, `CONV`, `Conventional` | `"conventional"` |
| `NT`, `ZT`, `No-till`, `zero` | `"zero tillage"` |
| `MT`, `Min Till` | `"minimum tillage"` |
| `RT`, `reduced` | `"reduced tillage"` |
| `Ridge`, `Ridging` | `"ridge tillage"` |
| `Hand Hoe`, `hoe` | `"hoeing"` |
| `Plough`, `Ploughed` | `"ploughing"` |
| `Basins`, `BASINS` | `"basins"` |
| `puddled plots` | `"puddled"` |
| `furrow dikes` | `"open furrows"` |

> **Known limitation:** Terms not matched pass through as-is.

</details>

<details>
<summary><strong>Step 12 — Yield part harmonisation</strong></summary>

| ERA term | Carob value |
|----------|------------|
| `Grain/Seed` | `"grain"` |
| `Pods` / `Nuts` | `"pod"` |
| `Tuber/Root` / `Bulb` | `"roots"` |
| `Stem/Stalks` | `"stems"` |
| `Whole Plant` / `Stalks+Leaves` | `"aboveground biomass"` |
| `Biomass` / `Haulm` | `"biomass"` |
| `Corm` / `Cormel` / `corn` | `"grain"` |
| `Unspecified` / `Cane` | `"none"` |

</details>

**Output — `dwf`:**

| Rows | Columns | Studies | Countries |
|------|---------|---------|-----------|
| ~244,000 | ~163 | 1,720 | 51 |

---

## `era_carob_merge.R` — Deduplication, schema alignment, merge

**Prerequisite:** `dwf` must be in your R environment (run `era_carob.R` first).

<details>
<summary><strong>Step 1 — Duplicate detection</strong></summary>

ERA's `uri` field contains **journal article DOIs** (e.g. `10.1007/s10113-019-01511-w`).
Carob's `metadata$uri` contains **data repository DOIs** (e.g. `doi:10.18167/DVN1/66Z6JP`) — a different identifier for the same study.

Matching is done on `carob_index$metadata$publication` (journal DOI). Both sides are normalised:
- Strip `doi:` prefix from Carob
- Replace `_` with `/` (Carob uses underscores as separators in some DOIs)
- Lowercase both sides

**Result (June 2026):** 21 of 1,528 ERA studies were already in Carob. These are kept in the merge — both the Carob and ERA rows are retained, identifiable via the `source` column.

</details>

<details>
<summary><strong>Step 2 — Schema alignment</strong></summary>

The full Carob schema is taken from `carob_index$wide` (313 columns).

| Issue | Fix |
|-------|-----|
| `Soil_NO3` / `Soil_NH4` (uppercase) | Dropped — lowercase versions already exist |
| `soil_depth` is a `"upper-lower"` string | Split into numeric `depth_top` and `depth_bottom` |
| Column literally named `"NA"` | Dropped |
| Carob columns absent from ERA | Added as `NA` (195 columns) |
| ERA-only columns not in Carob schema | Dropped |

**ERA-only columns dropped:**
- Climate: `seasonal_prep`, `total_prec`, `tmax`, `tmin`
- Economics: `Variable_Cost`, `Gross_Margin`, `Net_Return`, `Benefit_Cost_Ratio_*`, `Labour_*`
- ERA internal: `dsign`, `control_T`, `id`, `treatment_type`
- Unmapped outcomes: `Land_Equivalent_Ratio`, `Biomass_Yield`, `Erosion`, `Runoff`, `Biodiversity`

</details>

<details>
<summary><strong>Step 3 — Merge</strong></summary>

`carobiner::bindr` (via `do.call`) row-binds Carob rows first, ERA rows second. A `source` column identifies provenance:
- `"carob"` — original Carob observations
- `"era"` — ERA observations aligned to Carob schema

</details>

**Output — `merged`:**

| Total rows | Columns | Carob rows | ERA rows | Countries |
|------------|---------|------------|----------|-----------|
| ~482,000 | 314 | ~238,540 | ~244,094 | 51 |

---

## `era_carob_report.R` — Coverage report

Produces the following outputs to `report/`:

| File | Content |
|------|---------|
| `01_crop_coverage.png` | Observations by crop, coloured by source |
| `02_country_coverage.png` | Observations by country, coloured by source |
| `03_field_completeness.png` | % of key fields populated in ERA vs Carob rows |
| `era_carob_overlap.csv` | 21 studies present in both ERA and Carob |
| `era_dropped_columns.csv` | ERA-only columns categorised by type |

---

## Known limitations and future work

- Country harmonisation only covers a subset of multi-country strings
- `land_prep_method` and `yield_part` use hardcoded `ifelse` chains — should be replaced with Carob vocabulary lookup functions when available
- ERA-specific climate fields (`seasonal_prep`, `total_prec`, `tmax`, `tmin`) are dropped when merging with Carob
- 195 Carob schema fields are set to `NA` for ERA rows (e.g. `record_id`, `trial_id`, `is_survey`, `yield_moisture`)
- Climate linkage (ERA5 / CHIRPS matched to planting date and coordinates) is planned as a future phase
