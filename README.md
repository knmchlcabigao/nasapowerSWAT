# nasapowerSWAT v1.0 <img src="https://img.shields.io/badge/R-package-276DC3?style=flat&logo=r" align="right"/>

**NASA POWER Daily Data Downloader for SWAT and SWAT+**

A Shiny-based desktop app that downloads daily climate data from [NASA POWER](https://power.larc.nasa.gov/) and formats it for use in [SWAT](https://swat.tamu.edu/) and [SWAT+](https://swat.tamu.edu/software/plus/) hydrological models.


---

## What it does

- Downloads daily climate data (Precipitation, Temperature, Solar Radiation, Wind Speed, Relative Humidity) from NASA POWER
- Formats and exports data as `.txt` (SWAT-ready) or `.csv`
- Generates a station lookup table (with optional DEM-based elevations)
- Visualizes timeseries and compares fetched data against observed data

---

## Installation

### Step 1 — Install R and RStudio (beginners)

If you don't have R and RStudio installed yet:

1. Download and install **R** from [https://cran.r-project.org](https://cran.r-project.org)
2. Download and install **RStudio** from [https://posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop)

> Already have R and RStudio? Skip to Step 2.

---

### Step 2 — Install the package

Open RStudio and run the following in the **Console** panel:

```r
# Install devtools if you don't have it yet
install.packages("devtools")

# Install nasapowerSWAT from GitHub
devtools::install_github("knmchlcabigao/nasapowerSWAT")
```

> This may take a few minutes the first time — it will also install all required packages automatically.

---

### Step 3 — Launch the app

```r
nasapowerSWAT::run_app()
```
OR
```r
library(nasapowerSWAT)
run_app()
```

The app will open in your default web browser. That's it!

---

## Usage

| Tab | What to do |
|-----|------------|
| **1. Define Study Area** | Draw a bounding box on the map or enter coordinates manually |
| **2. Filter & Fetch Data** | Select climate variables and year range, then click Fetch Data |
| **3. Export Data** | Set output folder and export as `.txt` (SWAT) or `.csv` |
| **4. Lookup Table** | Optionally import a DEM for elevation and export the station table |
| **5. Visualize** | Plot timeseries, compare with observed data, compute statistics |

---

## Updating

When a new version is released, reinstall with the same command:

```r
devtools::install_github("knmchlcabigao/nasapowerSWAT")
```

---

## Requirements

- R ≥ 4.0
- Internet connection (for fetching NASA POWER data)
- The following R packages are installed automatically:
  `shiny`, `leaflet`, `leaflet.extras`, `nasapower`, `tidyverse`, `terra`

---

## Citation

If you use this tool in your research, please cite it as:

> Cabigao, K.M.F. (2025). *NASA POWER Daily Data Downloader for SWAT Model (v1.0)*. Retrieved from [https://github.com/YourGitHubUsername/nasapowerSWAT](https://github.com/knmchlcabigao/nasapowerSWAT)

You may also cite the underlying NASA POWER R client:

> Sparks, A.H. (2018). nasapower: A NASA POWER Global Meteorology, Surface Solar Energy and Climatology Data Client for R. *Journal of Open Source Software*, 3(30), 1035. https://doi.org/10.21105/joss.01035

---

## Publication Status

A peer-reviewed journal article describing the development and application of **nasapowerSWAT** is currently **being written for submission**. Once published, a full citation will be added here.

In the meantime, please cite the software directly (see [Citation](#citation) above).

## License

MIT © [Kean Michael F. Cabigao](mailto:knmchlcabigao@gmail.com)
