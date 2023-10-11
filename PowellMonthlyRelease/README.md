# Lake Powell monthly releases by annual release target

This document plots Lake Powell monthly release volumes by month and annual volume. The purpose is to identify the high release months that correspond to high energy load
months. The file with the CRSS data is Powell-MonthlyReleaseSchedule.txt This data comes form the Powell.MonthlyReleaseTable slot in the Colorado River Simulation System (CRSS) model. This data is also presented in the [Long-Term Experimental and Management Plan]https://www.
usbr.gov/uc/progact/amp/ltemp.html) (2016).

A plot is shown for Monthly Release volume vs month for different time series of Annual releases

##Findings
1. The high release volume months are January, February, March, June, July, and August.

## Requested Citation
David E. Rosenberg (2023), “Lake Powell Monthly Release Volumes.” Utah State University. Logan, Utah.
Hydroshare

## View Results
Open the file **[PowellMonthlyRelease.pdf](PowellMonthlyRelease.pdf)

## Requirements to Run
* R version 4.1.1. Download from https://cran.r-project.org/.
* R Studio 1.1.456. Download from https://www.rstudio.com/.

## Directions to Generate Results
1. Download and install R and RStudio (see requirements)
1. Within this subfolder, open the **PowelleMonthlyRelease.Rproject** file. R Studio should open.
1. Select the **PowelleMonthlyRelease.Rmd** tab (R markdown file) within R Studio.
1. Just below the tab, click the **Knit** button.
1. The code will run and generate the file **PowellMonthlyRelease.pdf**. Open the pdf file to view results.

## Explanation of Contents
1. **PowellMonthlyRelease.pdf** - Output file created when knit **PowellMonthlyRelease.Rmd** within R Studion
1. **PowellMonthlyRelease.Rmd** - R markdown file with code to knit (run) to generate primary output file **PowellMonthlyRelease.pdf**.
1. **PowellMonthlyRelease.r** - R file with same code as **PowellMonthlyRelease.Rmd** but pushes results to console. Use for testing code.
1. **PowellMonthlyRelease.Rproject** - R project file. Use to open the project in R Studio.
1. **Powell-MonthlyReleaseSchedule.txt** - Comma seperated values (CSV) file with data downloaded from CRSS slot Powell.MonthlyReleaseTable. Rows are month of the year. Columns are annual release target. All values million acre-feet.

## Requested Citation
David E. Rosenberg (2020). "Powell Monthly Releases". Utah State University. Logan, Utah. https://github.com/dzeke/ColoradoRiverCoding/tree/main/GrandCanyonInterveningFlow.
