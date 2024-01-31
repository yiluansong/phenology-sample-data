# Phenology dataset for STATS 605 2023 Fall
Prepared by Yiluan Song

---
1. datafield_description.csv
Description: Please refer to this file for the definitions of data fields in all other files.

2. metadata.csv
Description: Metadata about two study sites from the National Ecological Observatory Network (NEON) and tagged individual plants at these sites.
Source: Data were derived from the National Ecological Observatory Network (NEON) DP1.10055.001. More details about sampling design can be found at https://data.neonscience.org/data-products/DP1.10055.001.
Processing: Apart from spatial information, we kept a subset of metadata (species and growth form) that may help understand and classify distinct types of phenology.
Additional information: Data were retrieved and processed on June 5, 2023. We used R packages neonUtilities and geoNEON for organizing data tables and calculating coordinates of individuals. 
 
3. discrete.csv
Description: Discrete data for plant phenology, including phenophase status and phenophase intensity, for tagged individual plants from Jan 1, 2013 to May 1, 2023.
Source: Data were derived from the USA National Phenology Network (USA-NPN) status and intensity data, with NEON as the source dataset. More data product description can be found at https://data.usanpn.org/observations/get-started. Sampling protocol explained in plain language can be found at https://www.usanpn.org/nn/guidelines.
Processing: We selected observations of five phenophases related to leaves or needles (new leaf, young leaf, full leaf, colored leaf, fallen leaf). We checked that no observations were in conflict. We removed all observations with an "uncertain" phenophase status, keeping only the ones indicating "yes" or "no." We removed all intensity observations that were not in the form of percentage bins, and coded the reported percentage bins into six levels. We kept only one record from each observer on each day when there are duplicates. Note that there can be multiple observers giving different assessments.
Additional information: Data were retrieved and processed on June 5, 2023. We used an R package rnpn to retrieve coding of phenophases.

4. continuous_3m.csv
Description: Continuous data for plant phenology, i.e., enhanced vegetation index (EVI), in 3-m pixels corresponding to tagged individual plants from Jan 1, 2017 to Apr 21, 2023.
Source: Data were derived from PlanetScope, atmospherically corrected surface reflectance product (ortho_analytic_4b_sr). More introduction about PlanetScope can be found at https://developers.planet.com/docs/data/planetscope/.
Processing: Longitude and latitude of tagged individual plants was used to locate remote sensing pixels. For quality control, we applied Usable Data Masks (UDM2) to include only pixels that were clear, had no snow, ice, shadow, haze, or cloud, and had algorithmic confidence in classification ≥ 80%. For each date, pixel, and band, we took the mean reflectance if there were multiple visits in a day. We calculated the enhanced vegetation index (EVI) following Liu & Huete (1995). We used the following three criteria to filter out possibly contaminated data points: 1) reflectances in all visible bands were positive values, 2) EVI was between zero and one, and 3) EVI did not deviate more than 0.2 from the climatology of EVI for the corresponding site.
Additional information: Data were retrieved on Apr 21, 2023 and processed on June 5, 2023. We used an R package planetR for data ordering and downloading. 

5. continuous_500m.csv
Description: Continuous data for plant phenology, i.e., enhanced vegetation index (EVI), in 500-m pixels corresponding to two study sites from Jan 1, 2013 to May 1, 2023.
Source: Data were derived from NASA VIIRS/NPP Vegetation Indices 16-Day L3 Global 500 m SIN Grid (VNP13A1 v001). More data product description can be found at https://lpdaac.usgs.gov/products/vnp13a1v001/.
Processing: Mean longitude and latitude of tagged individual plants at each site was used to locate remote sensing pixels. Bands "500 m 16 days EVI" and "500 m 16 days pixel reliability" were used to select EVI values of "excellent," "good," or "acceptable" reliability. Note that although the satellite has a repeat cycle of 16 days, the data product has a quasi 8-day resolution.
Additional information: Data were retrieved and processed on June 6, 2023. We used an R package MODISTools for data downloading.

6. weather.csv
Description: Daily weather variables at two study sites from Jan 1, 2012 to Dec 31, 2022.
Source: Data were derived from Daymet, a gridded data product of estimated daily weather parameters interpolated and extrapolated from daily meteorological observations. More data product description can be found at https://daymet.ornl.gov/overview.
Processing: Note that we computed daily mean temperature by taking the average of daily maximum and minimum temperatures, but this variable does not contain any new information.
Additional information: Data were retrieved and processed on June 5, 2023. We used an R package daymetr for data downloading.