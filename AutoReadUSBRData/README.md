# Auto-Read USBR Data from the HydroData Web protal

This script provides a function (fReadReclamationHydroData) that auto reads USBR Reservoir data from Reclamation's HydroData Web portal
https://www.usbr.gov/uc/water/hydrodata/reservoir_data/site_map.html

The script auto reads all available data fields for reservoirs specified in the Excel file **USBRWebPortalMetaData.xlsx**.

At present, this data is:
- Lake Powell (919) and field (code)
  - Storage - 17
  - Evaporation - 25
  - Inflow - 29
  - Inflow volume - 30
  - Unregulated inflow - 33
  - Unregulated inflow volume - 34
  - Total Release - 42
  - Release volume - 43
  - Area - 89
  - Pool elevation - 49
- Lake Mead (921)
  - Storage - 17
  - Total Release - 42
  - Pool elevation - 49
  - Release volume - 43

The auto read URL command is:
  'https://www.usbr.gov/uc/water/hydrodata/reservoir_data/RESID/csv/FIELDID.csv'

The function takes the arguement FromHydroData (Boolean)

If FromHydroData == TRUE, the script downloads the latest data from the webportal.

The function outputs 3 data frames that aggregate data at three different time stpes:

1. **dfResDataAnnual** - Annual by Water Year
1. **dfResDataMonthly** - Monthly
1. **dfResDataDaily** - Daily

The 3 data frames are also written to 3 csv files in the same working directory.

1. **dfResDataAnnual.csv** - The annual data by Water 
1. **dfResDataMonthly.csv** - Monthly
1. **dfResDataDaily.csv** - Daily

If FromHydroData == FALSE, the script reads in csv files created from the most recent run of the function.


## Use of the function
Include the commands at the top of your script so R knows the correct relative paths for the function with respect to the code you are running.

`library(here)`

`here::i_am("PATH/RCODEFILENAME.R")`

Reference the function in the script

`source("../AutoReadUSBRData/AutoReadUSBRData.r")`

**Read the most recently saved csv files**

`lResData <- fReadReclamationHydroData(FromHydroData = FALSE)`

**Downloaded the data from HydroData**

`lResData <- fReadReclamationHydroData(FromHydroData = TRUE)`

## Explanation of Contents
1. **AutoReadUSBRData.r** - The R script with the function
1. **USBRWebPortalMetaData.xlsx** - Excel file with the reservoir and field codes to use in the download
1. **dfResDataAnnual.csv** - Data aggregated to Water Year in comma seperated values (CSV) form.
1. **dfResDataMonthly.csv** - Data aggregated to Monthly in comma seperated values (CSV) form.
1. **dfResDataDaily.csv** - Daily Data in comma seperated values (CSV) form.

## Recommended Citation
David E. Rosenberg (2026). "Download Reservoir Data from Reclamation HydroData Portal". Utah State University. Logan, Utah. https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/AutoReadUSBRData.
