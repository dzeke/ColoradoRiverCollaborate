# Historical and Current Colorado River Reservoir Data

This R script downloads from the U.S. Bureau of Reclamation and plots historical and current reservoir data such as inflows, storage, releases, evaporation, etc.

We use data download and aggregate functions in the subfolder **AutoReadUSBRData**.

## Explanation of Contents
1. **Powell10Year.R** - R script that reads in raw release data, calculates 10-year sums and differences, dumps results to csv and shows plots of 
         (a) The 10-year values over time at a monthly time spacing, (b) 10-year values over time at an annual time spacing, and
		 (c) the difference between the 10-year sum and 82.5 MCM requirement

## Directions to Reproduce Results

1. Download and install R and RStudio. See https://rstudio-education.github.io/hopr/starting.html for a tutorial.
2. In your file explorer, open the subfolder (**HistoricalCurrentReservoirData**). Open the file **________________.Rproject**. R studio will open.
3. Select the file tab **_________________________.Rmd**.
4. Click the **Knit** button (row below the file tabs)
5. R studio will run the blocks of code and markdown and generate the **__________________.pdf** file.
6. Alternatively in step 3, select the file tab **___________________.r**
7. Select all the code in the file (ctrl-A) and click **Run** (row below the file tabs).
8. R studio will run the code, generate the figures, and store the figures as .png files in the same directory.

## Recommended Citation
David E. Rosenberg (2020). "Historical and Current Colorado River Reservoir Data". Utah State University. Logan, Utah. https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/HistoricalCurrentReservoirData
