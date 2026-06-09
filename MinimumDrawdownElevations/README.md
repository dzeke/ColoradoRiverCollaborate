# Minimum Drawdown Elevations for Lake Powell and Lake Mead

This R script downloads reservoir elevation data from the U.S. Bureau of Reclamation
and plots historical Lake Powell elevation along with a Minimum Drawdown and Catastrohpic elevations
for Lake Powell. The script additionally loads reservoir bathymetry and key reservoir elevations
to add to the plot. The script additionally calculates the buffer volume
between the 2 elevations and annotates the two elevations on the plot.

We use data download and aggregate functions in the subfolder **AutoReadUSBRData**.

## Explanation of Contents
1. **MinimumDrawdownElevations.R** - R script that reads in raw Lake Powell and Lake Mead Reservoir data and makes plots such as of inflows, storage, release, evaporation, etc.
2. **MinimumDrawdownElevations.Rmd** - R markdown script that  reads in the reservoir storage data, bathymetry, and generates a plot illustrating Lake Powell storage, Minimum Drawdown, and Catastrophic Elevations.
The script outputs the plot to the pdf file **MinimumDrawdownElevations.pdf**
3. **MinimumDrawdownElevations.pdf** - Pdf file with the output plot created when the markdown file **MinimumDrawdownElevations.Rmd** is run.

All of the files use a script with functions in the folder **AutoReadUSBRData** that auto download reservoir data from Reclamation's data portal
and aggregate that data at the appropriate time spacing to prepare for plotting. See the README.md file in that folder for further explanation of its contents and features.

## Directions to Reproduce Results

1. Download and install R and RStudio. See https://rstudio-education.github.io/hopr/starting.html for a tutorial.
2. In your file explorer, open the subfolder (**MinimumDrawdownElevations**). Open the file **MinimumDrawdownElevations.Rproj**. R studio will open.
3. Select the file tab **MinimumDrawdownElevations.Rmd**.
4. Click the **Knit** button (row below the file tabs)
5. R studio will run the blocks of code and markdown and generate the **MinimumDrawdownElevations.pdf** file.
6. Alternatively in step 3, select the file tab **MinimumDrawdownElevations.r**
7. Select all the code in the file (ctrl-A) and click **Run** (row below the file tabs).

## Recommended Citation
David E. Rosenberg (2026). "Minimum Drawdown Elevations for Lake Powell". Utah State University. Logan, Utah. https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/HistoricalCurrentReservoirData
