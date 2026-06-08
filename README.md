ERA × Carob
================

- [ERA → Carob: Transforming ERA Agronomic Data into the Carob Standard
  Format](#era--carob-transforming-era-agronomic-data-into-the-carob-standard-format)
  - [What is ERA?](#what-is-era)
  - [What is Carob?](#what-is-carob)
  - [Scripts](#scripts)
  - [Coverage Report](#coverage-report)
  - [`era_carob.R` — ERA to Carob
    format](#era_carobr--era-to-carob-format)
  - [`era_carob_merge.R` — Deduplication, schema alignment,
    merge](#era_carob_merger--deduplication-schema-alignment-merge)
  - [Known limitations and future
    work](#known-limitations-and-future-work)

# ERA → Carob: Transforming ERA Agronomic Data into the Carob Standard Format

This repository contains R scripts that load the ERA agronomic database,
reformat it to match the [Carob](https://carob-data.org) data standard,
and merge it with the existing Carob dataset to create a unified
meta-analysis resource.

------------------------------------------------------------------------

## What is ERA?

ERA (Evidence for Resilient Agriculture) is a peer-reviewed agronomic
database maintained by CGIAR. It contains over 110,000 observations from
~1,720 studies across 51 countries, covering crops, soils, management
practices, and yield outcomes.

## What is Carob?

Carob is an open agricultural data platform that standardises
experimental datasets into a common schema (~238,000 observations, ~197
datasets).

------------------------------------------------------------------------

## Scripts

| Script                      | Purpose                          | Output    |
|-----------------------------|----------------------------------|-----------|
| `script/era_carob.R`        | Transform ERA → Carob format     | `dwf`     |
| `script/era_carob_merge.R`  | Deduplicate, align schema, merge | `merged`  |
| `script/era_carob_report.R` | Coverage report + visualisations | `report/` |

Run in order: `era_carob.R` → `era_carob_merge.R` → `era_carob_report.R`

Then re-knit this file to update the README:

``` r
rmarkdown::render("README.Rmd")
```

------------------------------------------------------------------------

## Coverage Report

### What ERA added to Carob

| Metric                       |  Value |
|:-----------------------------|-------:|
| ERA observations added       | 244094 |
| ERA studies added            |   1720 |
| ERA studies already in Carob |     21 |
| New countries from ERA       |     17 |
| New crops from ERA           |    678 |
| Total observations (merged)  | 491087 |
| Total columns (merged)       |    321 |

### Studies already in both ERA and Carob

**21** ERA studies were already present in Carob (matched on journal
DOI):

| ERA dataset ID | Journal DOI                 | Carob ID                      |
|:---------------|:----------------------------|:------------------------------|
| AG0065         | 10.3389/fpls.2016.01435     | doi_10.7910_DVN_AMAZXA        |
| AN0132         | 10.1017/S0014479715000265   | doi_10.7910_DVN_QLJUY7        |
| CJ0026         | 10.1017/S1742170517000606   | doi_10.7910_DVN_RSGLGB        |
| CJ0026         | 10.1017/S1742170517000606   | doi_10.7910_DVN_UTSWRY        |
| DK0015         | 10.1155/2018/7676058        | doi_10.21421_D2_FATVHT        |
| DK0061         | 10.1016/j.fcr.2017.01.024   | doi_10.18167_DVN1_Y5HADO      |
| DK0071         | 10.1016/j.agee.2017.08.015  | doi_10.25502_a7ex-ea51_d      |
| DK0071         | 10.1016/j.agee.2017.08.015  | doi_10.25502_ac6r-kx93        |
| DK0071         | 10.1016/j.agee.2017.08.015  | doi_10.25502_EZQV-ZZ19        |
| DK0076         | 10.1016/j.agee.2016.05.012  | doi_10.25502_20180814_0923_HJ |
| DK0076         | 10.1016/j.agee.2016.05.012  | doi_10.25502_20180814_1219_HJ |
| DK0076         | 10.1016/j.agee.2016.05.012  | doi_10.25502_20180814_1446_HJ |
| EO0151         | 10.1016/j.fcr.2015.02.013   | doi_10.18167_DVN1_GPZOHO      |
| HK0261         | 10.2134/agronj2012.0063     | doi_10.34725_DVN_25746        |
| JO0038         | 10.1016/j.agee.2022.108207  | doi_10.18167_DVN1_N7GAZF      |
| JO0038         | 10.1016/j.agee.2022.108207  | doi_10.18167_DVN1_VPOCHN      |
| JO0042         | 10.1016/j.fcr.2020.108052   | doi_10.18167_DVN1_IJOA5J      |
| NM0013         | 10.1016/j.fcr.2021.108225   | doi_10.7910_DVN_1A6WMD        |
| NM0013         | 10.1016/j.fcr.2021.108225   | doi_10.7910_DVN_UJIPSW        |
| NM0101         | 10.1016/j.agee.2021.107576  | doi_10.18167_DVN1_XYOHRP      |
| NN0560         | 10.1016/j.agwat.2011.04.002 | doi_10.34725_DVN_FUZDMU       |

### Observations by crop

<img src="report/01_crop_coverage.png" width="768" />

### Observations by country

<img src="report/02_country_coverage.png" width="768" />

### Field completeness: ERA vs Carob

<img src="report/03_field_completeness.png" width="864" />

### Fields with 0% completeness in ERA rows

These Carob fields have no equivalent in ERA and will always be `NA` for
ERA-sourced rows:

- `record_id`
- `trial_id`
- `is_survey`
- `yield_moisture`
- `geo_from_source`
- `fertilizer_type`
- `fertilizer_amount`

### Fields with \<25% completeness in ERA rows

| Field            | % complete in ERA rows |
|:-----------------|-----------------------:|
| soil_N           |                    0.2 |
| soil_SOC         |                    3.2 |
| plant_density    |                    8.7 |
| K_fertilizer     |                   11.8 |
| P_fertilizer     |                   17.6 |
| land_prep_method |                   18.3 |
| soil_pH          |                   21.0 |

### ERA-only columns dropped during merge

| Category            | N columns | Examples                                                 |
|:--------------------|----------:|:---------------------------------------------------------|
| Economic outcomes   |        28 | Variable_Cost, Gross_Margin, Benefit_Cost_Ratio\_(GMVC)  |
| ERA climate fields  |         2 | total_prec, seasonal_prep                                |
| ERA internal fields |         4 | dsign, control_T, treatment_type, id                     |
| Unmapped outcomes   |        47 | Land_Equivalent_Ratio, Biomass_Yield, Crop_Residue_Yield |
| Other               |        27 | uri, year, herbicide_method                              |

------------------------------------------------------------------------

## `era_carob.R` — ERA to Carob format

<details>
<summary>
<strong>Step 1 — Data loading</strong>
</summary>

ERA data is loaded from a public S3 bucket (`s3://digital-atlas/era`)
rather than a local file. The script: - Finds the latest
`era_agronomy_bundle_*.tar.gz` in the S3 bucket - Downloads and extracts
it to `downloaded_data/` (only if not already present — cached) - Loads
three files: - `agronomic_*.json` → `era_merge` (the relational table
list) - `era_master_codes*.json` → `era_master` (vocabulary/lookup
codes) - `era_compiled*.parquet` → `ERA_Compiled` (compiled flat table)

> `downloaded_data/` is in `.gitignore` — the bundle is too large (~473
> MB) for GitHub.

</details>
<details>
<summary>
<strong>Step 2 — Merging ERA sub-tables</strong>
</summary>

ERA stores data across 12 relational sub-tables merged into a single
flat data frame `rb`:

| Sub-table              | Content                                             |
|------------------------|-----------------------------------------------------|
| `Data.Out`             | Core outcome and site data (baseline)               |
| `Prod.Out`             | Production/yield variables                          |
| `Till.Out`             | Tillage method and mechanisation                    |
| `Plant.Method`         | Planting density, method, row spacing               |
| `Fert.Method`          | Fertilizer type, amount, timing                     |
| `Chems.Out`            | Herbicides, insecticides, fungicides, biopesticides |
| `Res.Method`           | Residue management                                  |
| `Res.Comp`             | Residue composition                                 |
| `pH.Out` / `pH.Method` | Soil pH measurements and method                     |
| `Irrig.Method`         | Irrigation amount, method, dates                    |
| `WH.Out`               | Water harvesting                                    |
| `Soil.Out`             | Soil properties (long → wide per study)             |

**Filtering:** rows with `Site.ID == "All Sites"` and
`Product.Type == "Animal"` are removed.

</details>
<details>
<summary>
<strong>Steps 3–12 — Variable mapping, cleaning, harmonisation</strong>
</summary>

See the full pipeline documentation in
[script/era_carob.R](script/era_carob.R).

Key decisions: - **Coordinates:** multiple decimal points removed;
`carobiner::fix_name()` applied - **Fertilizer values:** sentinel values
(999/999999) → NA; first numeric token extracted (fixes truncation
bug) - **Irrigation:** `irrigation_amount` correctly mapped to
`I.Amount` (was `I.Method`); `irrigated` flag uses
`!is.na(I.Amount) & I.Amount != 0` - **Plant density:** split into
`seed_density`, `seed_rate`, `plant_density` based on unit strings -
**Land prep:** free-text ERA codes harmonised to 17 controlled
vocabulary terms - **Yield part:** ERA compound terms mapped to 10 Carob
categories - **Intercrops:** `***`-delimited → `;`-delimited

</details>

------------------------------------------------------------------------

## `era_carob_merge.R` — Deduplication, schema alignment, merge

<details>
<summary>
<strong>Step 1 — Duplicate detection</strong>
</summary>

ERA `uri` = journal DOI; Carob `uri` = data repository DOI. Matching
done on `carob_index$metadata$publication` (journal DOI) after
normalising both sides (strip `doi:`, replace `_` with `/`, lowercase).

</details>
<details>
<summary>
<strong>Step 2 — Schema alignment</strong>
</summary>

Full Carob schema taken from `carob_index$wide` (313 columns). Key
fixes: - `Soil_NO3`/`Soil_NH4` (uppercase duplicates) dropped -
`soil_depth` string split into numeric `depth_top` + `depth_bottom` -
Spurious `"NA"` column dropped - 195 missing Carob columns added as
`NA` - ERA-only columns dropped (economics, ERA-internal, unmapped
outcomes)

</details>
<details>
<summary>
<strong>Step 3 — Merge</strong>
</summary>

`carobiner::bindr` row-binds Carob first, ERA second. `source` column
added (`"carob"` / `"era"`).

</details>

------------------------------------------------------------------------

## Known limitations and future work

- Country harmonisation covers only a subset of multi-country ERA
  strings
- `land_prep_method` and `yield_part` use hardcoded `ifelse` chains —
  replace with Carob vocabulary lookups when available
- 195 Carob schema fields are `NA` for all ERA rows
- Climate linkage (ERA5 / CHIRPS matched to planting date + coordinates)
  planned as next phase
