# Minimum Drawdown Elevations for Lake Powell and Lake Mead

This R script downloads reservoir elevation data from the U.S. Bureau of Reclamation
and plots historical Lake Powell elevation along with a Minimum Drawdown and Catastrohpic elevations
for Lake Powell. The script additionally loads reservoir bathymetry and key reservoir elevations
to add to the plot. The script additionally calculates the buffer volume
between the 2 elevati2ons and annotates the two elevations on the plot.

We use data download and aggregate functions in the subfolder **AutoReadUSBRData**.

## Explanation of Contents
1. **MinimumDrawdownElevations.R** - R script that reads in raw Lake Powell and Lake Mead Reservoir data and makes plots such as of inflows, storage, release, evaporation, etc.
2. **MinimumDrawdownElevations.Rmd** - R markdown script that  reads in the reservoir storage data, bathymetry, and generates a plot illustrating Lake Powell storage, Minimum Drawdown, and Catastrophic Elevations.
The script outputs the plot to the pdf file **MinimumDrawdownElevations.pdf**
3. **MinimumDrawdownElevations.pdf** - Pdf file with the output plot created when the markdown file **MinimumDrawdownElevations.Rmd** is run.

All of the files use a script with functions in the folder **AutoReadUSBRData** that auto download reservoir data from Reclamation's data portal
and aggregate that data at the appropriate time spacing to prepare for plotting. See the README.md file in that folder for further explanation of its contents and features.

## Data Sources

1. U.S. Bureau of Reclamation's **HydroData Web portal**. Auto reads USBR Lake Powell and Lake Mead data from 
https://www.usbr.gov/uc/water/hydrodata/reservoir_data/site_map.html.
1. **Reservoir Bathymetry** data for:
     A. Lake Powell from Bradley, D., and Collins, K. (2022). "Lake Powell 2017 Area and Capacity Tables." ENV-2021-98, Reclamation. https://doi.org/10.5066/P9O3IPG3.
			See also the folder **LakePowellNewBathymetry** for the digitized data.
     B. Lake Mead from the Lake Mead object in the Colorado River Simulation System (CRSS) model.

1. **Critical reservoir elevations**. Key selected reservoir elevations such as dead pool, hydropower penstock intakes, protection elevations, capacity, etc. The elevations are in the file **ReservoirElevationDefinitions.xlsx** within the folder **AutoReadUSBRData** and can be added/modified, removed etc.

## Directions to Reproduce Results

1. Download and install R and RStudio. See https://rstudio-education.github.io/hopr/starting.html for a tutorial.
1. Download all the files in the two folders **MinimumDrawdownElevations** and **AutoReadUSBRData** to your local drive. The two folders should be parallel.
1. In your file explorer, open the subfolder (**MinimumDrawdownElevations**). Open the file **MinimumDrawdownElevations.Rproj**. R studio will open.
1. Select the file tab **MinimumDrawdownElevations.Rmd**.
1. Click the **Knit** button (row below the file tabs).
1. R studio will run the blocks of code and markdown and generate the **MinimumDrawdownElevations.pdf** file.
1. Alternatively in step 3, select the file tab **MinimumDrawdownElevations.r**
1. Select all the code in the file (ctrl-A) and click **Run** (row below the file tabs).

## Instructions to use different Minimum Drawdown and Catastrophic Elevations

1. **.Rmd file** - Go to lines 37 to 42 and enter new values.
1. **.r fil** - Go to lines 60 to 65 and enter new values.

## Recommended Citation
David E. Rosenberg (2026). "Minimum Drawdown Elevations for Lake Powell". Utah State University. Logan, Utah. https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/MinimumDrawdownElevations.