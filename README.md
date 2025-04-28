# Temperature-sensitive incubation, transmissibility, and risk of *Aedes albopictus*-borne chikungunya virus in Europe
> **Temperature-sensitive incubation, transmissibility, and risk of *Aedes albopictus*-borne chikungunya virus in Europe**  
> *Sandeep Tegar*  
> UK Centre for Ecology & Hydrology, Benson Lane, Wallingford, Oxfordshire, UK  
> School of Mathematics and Statistics, University of Glasgow, Glasgow, UK

This code can be used to reproduce the results presented in the paper titled *"Temperature-sensitive incubation, transmissibility, and risk of* *Aedes albopictus*-*borne chikungunya virus in Europe."*
We used two programming languages: **R** (v4.4.1) and **Python** (v3.9 and above) with **Jupyter Notebooks** (7.x series).
Below, we provide a description of the required data and the execution steps.

We used two programming languages:
- **R** (v4.4.1)
- **Python** (v3.9+ with Jupyter notebooks, 7.x series)
  
# CHIKV EIP Code Description

## Part A

### Programming Language
- **R** version 4.4.1

### Required Packages
- `R2jags`
- `HDInterval`
- `ggplot2`
- `mcmcplots`

---

## 1. DPI Model (Example: T = 20°C)

**Main Data Files (see folder `data`):** 
- `vc_chikv.csv` (main data file)
- `T20.csv` (extracted from the main data file)

> Ensure that the data file (`T20.csv`) is in the working directory.

**Code Files:**
- `hills_20_code.R`: Fit an S-shaped function to the data
- `hills_20_eip_vc_samples.R`: Save the samples
- `hills_20_eip_vc_posteriors.R`: Generate EIP and VC posteriors
- `hills_20_pdr_posteriors.R`: Generate PDR posteriors

---

## 2. EIP Model (Example: Gillooly Model)

**Required Data File:**  
- `hills_eip_vc_hdi.vc`

> Ensure that the data file is in the working directory.

**Code Files:**
- `hills_gillooly.R`: Fit the temperature vs. EIP curve using the Gillooly model
- `hills_gillooly_summary.R`: Generate the summary
- `hills_gillooly_multi.R`: Generate the multipanel plot

---

## 3. PDR Model (Example: Briere Model)

**Required Data File:**  
- `hills_pdr.vc`

> Ensure that the data file is in the working directory.

**Code Files:**
- `hills_briere.R`: Fit the temperature vs. PDR curve using the Briere model
- `hills_briere_summary.R`: Generate the summary
- `hills_briere_multi.R`: Generate the multipanel plot

---

## 4. R0 Model (Example: PDR fitted using the Briere model)

**Folder:** `R0_briere_model`

**Required Data Files:**
- `hills_eip_vc_hdi.csv`
- `hills_pdr.csv`
- **Temperature-trait data** for *Ae. albopictus* from [Mordecai et al. (2017)](https://doi.org/10.1371/journal.pntd.0005568)

> Ensure that the data files are in the working directory.

**Code Files:**
- `R0_parameters.R`: Generate posteriors for all parameters in the R₀ expression
- `R0_function.R`: Contains model expressions required by R0_parameters.R
- `R0_analysis.R`: Generate R₀ posteriors
- `R0_post_analysis.R`: Generate the critical temperature range and optimum

> **Note:** To run the next part of the code, you will need the files `R0_out.csv` and `R0_out_norm.csv` generated in this section.

---

## Part B

### Programming Language
- **Python** 3.9+ with **Jupyter Notebook** (version 7.x series)

### Required Packages
- `numpy`
- `xarray`
- `pandas`
- `matplotlib`
- `geopandas`
- `cartopy`

---

## Required Data
- Daily mean temperature ("2m temperature") from [ERA5-Land hourly data from 1950 to present](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land?tab=download)
- [Natural_Earth_shape_files](https://www.naturalearthdata.com/):
  - `sf_naturalearth/ne_10m_admin_0_countries.shp` (to extract countries)
  - `sf_nuts_2021/NUTS_RG_03M_2021_4326.shp` and `sf_naturalearth/ne_50m_ocean.shp` (for boundary corrections)

---

## Data Preparation Steps

1. Define the function `R0(T)` using a 1D spline from `R0_out_norm.csv`
2. Compute `R0(T)` for each grid cell across Europe and create a NetCDF dataset
3. Mask areas outside Europe and smooth the sea-land boundaries using the natural earth sea-land mask
4. Create a binary event dataset from the processed dataset and save it as `chikv_europe_event.nc`

> Ensure that `chikv_europe_event.nc` is in the working directory.

---

## Code Files
- `chikv_months_consecutive.ipynb`: Create Europe risk maps
- `chikv_month_season_annual.ipynb`: Identify seasonal trends

---

## Citation
If you use this code, please cite:  
**"Temperature-sensitive incubation, transmissibility, and risk of *Aedes albopictus*-borne chikungunya virus in Europe"**
