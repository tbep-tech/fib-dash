# fib-dash

[![build](https://github.com/tbep-tech/fib-dash/workflows/build/badge.svg)](https://github.com/tbep-tech/fib-dash/actions)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15083025.svg)](https://doi.org/10.5281/zenodo.13881472)

Materials for the TBEP FIB dashboard

## Automated and regular updates

* Data under the baywide reporting tab are updated annually
  * January each year, create new data objects for `catchprecip` (from [here](https://github.com/tbep-tech/tbeptools/blob/7a9865f4fe9753d5bb645284eea4a4f125305994/R/catchprecip.R#L17)) and `enterodata` (from [here](https://github.com/tbep-tech/tbeptools/blob/7a9865f4fe9753d5bb645284eea4a4f125305994/R/enterodata.R#L28)) in tbeptools
  * Save each to tbeptools and save each in the data folder in this repo, push both back to their repositories.  Once the changes in this repo are pushed to GitHub they will be available for the dashboard.  Updating tbeptools is just to keep the data current.
  * Update State of the Bay graphic for TBEP website [here](https://github.com/tbep-tech/State-of-the-Bay/blob/9ac88af195f8752ec8e57cb78f885eeaef247523/createfigs.R#L1272), push to SOB repo
* County datasets are updated weekly using GitHub Actions as they become available from the sources, with most updates occurring monthly or quarterly.  These are currently pulled from EPA Water Quality Portal which has significant lags.  EPCHC data (`fibdata`) are the exception where data are pulled directly from source.
  * January each year, create new data objects for `fibdata` (from [here](https://github.com/tbep-tech/tbeptools/blob/7a9865f4fe9753d5bb645284eea4a4f125305994/R/fibdata.R#L34)), `hcesdfibdata` (from [here](https://github.com/tbep-tech/tbeptools/blob/7a9865f4fe9753d5bb645284eea4a4f125305994/R/hcesdfibdata.R#L29)), `mancofibdata` (from [here](https://github.com/tbep-tech/tbeptools/blob/7a9865f4fe9753d5bb645284eea4a4f125305994/R/mancofibdata.R#L30)), `pascofibdata` (from [here](https://github.com/tbep-tech/tbeptools/blob/7a9865f4fe9753d5bb645284eea4a4f125305994/R/pascofibdata.R#L29)), and `polcofibdata` (from [here](https://github.com/tbep-tech/tbeptools/blob/7a9865f4fe9753d5bb645284eea4a4f125305994/R/polcofibdata.R#L29)) in tbeptools
  * Push all to the tbeptools repository to keep the data current with those here.  They are not used in this repository since the data pulls are automated weekly. 
* There should be no need to update any of the code for the dashboard when any of the data are updated.