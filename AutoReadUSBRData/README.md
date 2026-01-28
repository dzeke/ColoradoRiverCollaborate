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

if FromHydroData == TRUE, the script downloads the latest data from the webportal.
If FromHydroData == FALSE, the script reads in csv files created from the most recent run of the function.

The function outputs 3 data frames that aggregate data at three different time stpes.:

1. **dfResDataAnnual** - Annual by Water Year
1. **dfResDataMonthly** - Monthly
1. **dfResDataDaily** - Daily

The 3 data frames are also written to 3 csv files in the same working directory.

1. **dfResDataAnnual.csv** - The annual data by Water 
1. **dfResDataMonthly.csv** - Monthly
1. **dfResDataDaily.csv** - Daily

## Use of the function
Include the commands at the top of your script so R knows the correct relative paths for the function with respect to the code you are running.
`library(here)`
`here::i_am("PATH/RCODEFILENAME.R")`

Read the most recently saved csv files
`lResData <- fReadReclamationHydroData(FromHydroData = FALSE)`

Read the downloaded data from HydroData
`lResData <- fReadReclamationHydroData(FromHydroData = TRUE)`

## Explanation of Contents
1. **DecadePowllRelease.csv** - csv file output by Powell10Year.R with 10-year release volumes and differences
1. **DecadePOwellRease.xlsx** - Excel version with formatting to show in blog post
1. **Powell10Year.R** - R script that reads in raw release data, calculates 10-year sums and differences, dumps results to csv and shows plots of 
         (a) The 10-year values over time at a monthly time spacing, (b) 10-year values over time at an annual time spacing, and
		 (c) the difference between the 10-year sum and 82.5 MCM requirement
1. **PowellDataUSBRMay2020.csv** - Lake Powell release data downloaded from USBR for May 2020 on back
To run any code, download and install R and RStudio. Within the subfolder, open the .R file, highlight all the code, and run. 

## Recommended Citation
David E. Rosenberg (2026). "Download Reservoir Data from Reclamation HydroData Portal". Utah State University. Logan, Utah. https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/AutoReadUSBRData.
