# Tesla vs BYD Sales Trends Dashboard

An R Shiny dashboard for exploring EV deliveries, prices, battery capacity, and regional sales patterns, mainly across 2019-2024. Charts and written insights compare Tesla models and selected competitors, including BYD.

## Tools

R, Shiny, dplyr, tidyr, readr, lubridate, ggplot2, and Quarto. The report also includes an XGBoost modelling section using tidymodels.

## Main features

- Cleans column names, removes duplicates, handles missing key fields, and groups data for analysis.
- Defines **8 chart outputs** across **5 selectable insight categories**.
- Compares affordable and luxury Tesla deliveries, workforce and sales trends, and price versus estimated gross margin.
- Shows Tesla and BYD model sales distributions, battery-capacity trends, and regional model predictions prepared in the report.
- Presents written interpretations and suggested pricing and market strategies. These are analysis proposals, not measured business improvements.

## Included data

| File | Raw rows | Content |
| --- | ---: | --- |
| `tesla_vehicle_deliveries.csv` | 3,000 | Tesla deliveries, models, regions, prices, and vehicle measures |
| `electric_vehicles_dataset.csv` | 3,022 | EV models, prices, batteries, and 2024 sales fields |
| `market_data.csv` | 54 | Brand-level market fields |
| `relationship_between_tesla_employees_vs_sales.csv` | 6 | Annual workforce and sales figures |

Counts are before cleaning. Data accuracy and source permissions have not been independently verified; dataset results should not be treated as confirmed company statistics.

## Read in this order

1. **`analysis_report.qmd`** - data preparation, analysis, charts, modelling, and written findings.
2. **`App_Tesla.R`** - Shiny layout, category selection, and chart rendering.
3. **CSV files** - the underlying data.
4. **`apa7.csl`** - citation style for the report.

## Run in RStudio

1. Install **R 4.1 or newer** and **RStudio**.
2. In RStudio, choose **File > Open Project** and open `Tesla vs BYD Sales Trends Dashboard.Rproj`. Keep all CSV files in this project folder.
3. Run the package-install commands below in the **RStudio Console** (only needed once).

Install the packages loaded by the app, plus `ggrepel` for chart labels:

```r
install.packages(c(
  "shiny", "shinythemes", "dplyr", "tidyr", "ggplot2", "scales",
  "lubridate", "forcats", "stringr", "readr", "viridis", "DT",
  "shinyjs", "shinyBS", "gridExtra", "ggrepel"
))
```

For the report's analysis and modelling sections, also install:

```r
install.packages(c("tidyverse", "cluster", "tidymodels", "zoo", "xgboost"))
```

4. Open **`analysis_report.qmd`** in RStudio. Use each R chunk's green Run button to execute the data-preparation and analysis chunks in order, stopping before the embedded Shiny app section.

**The app needs these preparation chunks first.** Run them in the same RStudio session. They create objects such as `delivery_summary`, `luxury_summary`, `employee_vs_sales`, `profitability_matrix`, `combined_sales`, `battery_focus`, `tesla_data`, `tesla_peak`, and `best_model_2024`.

5. Open **`App_Tesla.R`**. After preparation finishes successfully, run this in the **RStudio Console** to launch the dashboard using the same session:

```r
shiny::runApp(source("App_Tesla.R")$value)
```

This explicitly launches the Shiny app object returned by the file. Stop the app with the Console's **Stop** button or **Esc**.

Rendering the report in a separate process does not automatically share its objects with the app. A cleaner next step is to move the required preparation code into a shared script loaded by the app.

## Current limits

- The app has category selection and expandable text, but no year, model, price-range, or region filter controls. DT is loaded, but no DT table is displayed.
- Gross margins are estimates based on assumed battery cost of $150/kWh and other costs equal to 30% of price; they are not reported company margins.
- Regional predictions have no reported holdout evaluation. Training features include growth and a moving average containing the current target, creating leakage risk. Prediction quality needs further checking.
- Some chart objects depend on report code. The report also starts with an extra YAML separator and references a missing `references.bib`, which need attention before reliable rendering.
- The app and report were reviewed as source code, not executed end to end during this review.

## Credits

This is a team project. Personal names and student IDs have been removed for privacy; individual contributions should be stated separately on a CV. Third-party source citations and citation-style credits are retained. Confirm dataset permissions before public sharing.
