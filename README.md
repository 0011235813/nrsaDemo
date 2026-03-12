# nrsaqaapp

> A Shiny application for quality assurance review of **National Rivers and Streams Assessment (NRSA)** monitoring data.

<!-- badges: start -->
[![R-CMD-check](https://github.com/0011235813/nrsaqaapp_TEST/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/0011235813/nrsaqaapp_TEST/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

---

## Overview

`nrsaqaapp` helps state environmental agencies QA-review NRSA field data before submission. It supports data loading from **Excel file upload** or a direct **AWQMS ODBC connection**, and provides four review tabs:

| Tab | What it does |
|-----|-------------|
| **Summary** | Site-visit overview — record counts, transect completeness, date range |
| **Site Map** | Interactive map with popup details per site |
| **QA Review** | Per-visit quality score, missing values, outliers, flagged records |
| **Batch QA** | QA metrics across every visit in the filtered dataset at once |

---

## Installation

```r
# Requires the remotes package
install.packages("remotes")

# Install nrsaqaapp from GitHub
remotes::install_github("0011235813/nrsaqaapp_TEST")
```

> **Requirements:** R ≥ 4.1.0. All package dependencies are installed automatically.

---

## Quick Start

### Mode 1 — File upload (no database required)

Works out of the box for any agency. Upload one or more `.xlsx` or `.xls`
files exported from AWQMS or any system that follows the
`results_standard_vw` column schema.

```r
library(nrsaqaapp)
run_app()
```

### Mode 2 — AWQMS / ODBC connection (OWRB)

The package includes the OWRB AWQMS connection script. It is loaded
automatically when the package is installed — no extra setup needed.

```r
library(nrsaqaapp)
run_app()
```

Click **Connect to AWQMS** in the startup dialog, enter a date range,
and click **Connect & Load**.

> Credentials are stored securely in the Windows keyring — never in code
> or config files. On first use, run:
> ```r
> keyring::key_set(service = "awqms_credentials", username = "oklahomawrb")
> ```
> A popup will ask for your password. This only needs to be done once per machine.

### Mode 3 — AWQMS / ODBC connection (other agencies)

Supply the path to your own AWQMS connection script. The script must define
two functions: `awqms_get_con()` which returns an open DBI connection, and
`awqms_disconnect()` which closes it.

```r
library(nrsaqaapp)
run_app(awqms_script = "C:/path/to/your/AWQMS_connection_script.R")
```

To avoid typing the path every session, add it to your `.Renviron`:

```r
# Open your .Renviron file
file.edit("~/.Renviron")
```

Then add:

```
NRSA_AWQMS_SCRIPT=C:/path/to/your/AWQMS_connection_script.R
```

And update your `run_app()` call to:

```r
run_app(awqms_script = Sys.getenv("NRSA_AWQMS_SCRIPT"))
```

---

## Data Requirements

The app expects data aligned to the **`results_standard_vw`** schema.
When uploading Excel files, column names are automatically snake-cased
and common synonyms are mapped.

### Key columns

| Column | Description |
|--------|-------------|
| `monitoring_location_id` | Site identifier |
| `activity_start_date` | Sample date (ISO, M/D/YYYY, D-Mon-YYYY, or Excel serial) |
| `sampling_component_name` | Transect letter (A–K) |
| `characteristic_name` | Parameter name |
| `result_measure` | Numeric result value |
| `parent_activity_id` | Visit group (derived from `activity_id`) |
| `project_id1` … `project_id6` | Project identifiers |

### NRSA Project IDs

Records are filtered to visits containing at least one of these project IDs
across `project_id1`–`project_id6`:

```
FWProb08-12   FWProb13-17   FWProb18-22   FWProb23-27
FWTrend       FWFire2019    FWProbAll     AncillaryHabitat   NRSA-Data
```

---

## Adapting for Your Agency

Most agencies only need to supply their own AWQMS connection script via
`run_app(awqms_script = ...)` as shown above. The rest of the app works
without modification.

To customise QA rules or thresholds:

| What to change | Where |
|---------------|-------|
| QA parameter ranges | `PARAMETER_QA_RULES` in `R/constants.R` |
| Expected transects | `compute_transect_summary()` in `R/fct_qa.R` |
| NRSA project IDs | `NRSA_PROJECT_IDS` in `R/constants.R` |
| Column name synonyms | `synonym_map` in `R/fct_data.R` |

---

## Package Structure

```
nrsaqaapp/
├── R/
│   ├── constants.R          # Column names, project IDs, QA rules
│   ├── utils.R              # %||%, safe_date, safe_min_date, to_snake_case
│   ├── fct_data.R           # Data loading, coercion, harmonization
│   ├── fct_qa.R             # Outlier detection, scoring, QA flags
│   ├── fct_db.R             # AWQMS connection helpers
│   ├── mod_data_source.R    # Module: startup + upload + ODBC modals
│   ├── mod_summary.R        # Module: Summary tab
│   ├── mod_map.R            # Module: Site Map tab
│   ├── mod_qa_review.R      # Module: QA Review tab
│   ├── mod_batch_qa.R       # Module: Batch QA tab
│   ├── app_ui.R             # Top-level UI
│   ├── app_server.R         # Top-level server
│   └── run_app.R            # run_app() entry point
├── inst/awqms/
│   └── AWQMS_Source_Code.R  # Bundled OWRB connection script (no credentials)
├── tests/testthat/          # Unit tests
├── DESCRIPTION
└── README.md
```

---

## Running Tests

```r
devtools::test()
```

Tests cover all pure helper functions in `R/fct_data.R`, `R/fct_qa.R`,
and `R/utils.R`.

---

## Contributing

Pull requests are welcome. Please open an issue first to discuss major changes.

When submitting a PR:
- Add or update tests for any new functions in `tests/testthat/`
- Run `devtools::check()` locally before submitting

---

## License

MIT © Oklahoma Water Resources Board — see [LICENSE](LICENSE).
