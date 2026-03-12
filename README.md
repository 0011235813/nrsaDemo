# nrsaDemo

An R package containing a Shiny application for reviewing and summarising
**National Rivers and Streams Assessment (NRSA)** habitat field data collected
by the Oklahoma Water Resources Board.

---

## Features

- **Dual data sources** — load data from Excel files (`.xlsx` / `.xls`) or
  connect directly to an AWQMS database via ODBC
- **Filters** — date range, protocol (NRSA or State), and project selection
  presented at load time inside the data-source modal
- **Summary tab** — site-visit table with transect-completeness indicators
  (✔ / ✖) and one-click CSV / Excel export
- **Site Map tab** — interactive Leaflet map; clicking a map marker selects
  the matching row in the summary table and vice-versa
- **AWQMS helpers** — a set of exported functions (`awqms_connect`,
  `awqms_tbl`, `awqms_get_results_standard_filtered`, …) that can be used
  independently of the Shiny app

---

## Installation

```r
# install.packages("pak")  # if needed
pak::pkg_install("your-github-username/nrsaDemo")
```

Or with `remotes`:

```r
remotes::install_github("your-github-username/nrsaDemo")
```

### System requirements

The AWQMS connection path requires:

- A **SQL Server ODBC driver** installed on your machine.  
  On Windows the built-in `"SQL Server"` driver works; on macOS/Linux install
  [Microsoft ODBC Driver 17/18](https://learn.microsoft.com/sql/connect/odbc/download-odbc-driver-for-sql-server).
- Network access to `owrb.gselements.com:1433`.

---

## Credential setup

Credentials are resolved in this order:

1. **keyring** (recommended — survives R restarts, never stored in plain text):

   ```r
   keyring::key_set(service = "awqms_credentials", username = "oklahomawrb")
   # Enter your password at the prompt
   ```

2. **Environment variables** (useful in CI / server deployments):

   ```sh
   AWQMS_UID=oklahomawrb
   AWQMS_PWD=your_password
   ```

   Add these to your `.Renviron` (`usethis::edit_r_environ()`) or to the
   server environment before launching the app.

---

## Running the app

```r
library(nrsaDemo)
run_app()
```

Optional arguments are forwarded to `shiny::runApp()`:

```r
run_app(port = 4321, launch.browser = FALSE)
```

---

## Using the AWQMS helpers directly

```r
library(nrsaDemo)

# One-shot filtered pull
df <- awqms_get_results_standard_filtered(
  start_date = "2023-01-01",
  end_date   = "2023-12-31",
  protocol   = "NRSA Protocol",
  projects   = c("FWProb23-27", "FWTrend")
)

# Low-level access via a cached connection
con <- awqms_get_con()
dplyr::tbl(con, "results_standard_vw") %>% dplyr::glimpse()
awqms_disconnect()
```

---

## Package structure

```
nrsaDemo/
├── R/
│   ├── awqms.R        # AWQMS connection & query helpers (exported)
│   └── run_app.R      # run_app() entry point
├── inst/
│   └── app/
│       ├── app.R      # Shiny ui / server
│       └── R/
│           └── helpers.R   # Data-processing & UI helpers (app-internal)
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

---

## Contributing

Pull requests are welcome. Please open an issue first to discuss any
significant changes.

---

## License

MIT — see [LICENSE](LICENSE).
