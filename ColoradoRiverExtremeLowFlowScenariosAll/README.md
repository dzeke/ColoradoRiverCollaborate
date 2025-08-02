# Colorado River Extreme Low Flow Scenarios All

This document compiles and compares scenarios of extreme low Coloardo River inflows used in 6 different prior studies.
The purpose is to compare the inflow scenarios across the studies that used river flow as an operations criteria.

The data for this comparision are located in the Excel file **ColoradoRiverExtremeLowFlowFlowScenarios.xlsx**.

A line segment plot shows the range of extreme low flows used in each study. The figure additionally shows the strategy
used to stabilize reservoir storage for each study.

The studies are:
    A.
	B.
	C.
	D.
	E.
	F.

## Findings

## Requested Citation
David E. Rosenberg, Anabelle Myers, Erik Porse (2025), “How Extreme Low Colorado River Flows to Use?” Utah State University. Logan, Utah.
https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/ColoradoRiverExtremeLowInflowsAll

## View Results
Open the file **[ColoradoRiverExtremeLowFlowScneariosAll.pdf](ColoradoRiverExtremeLowFlowScneariosAll.pdf)**

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
1. **PowellMonthlyRelease.pdf** - Output file created when knit **PowellMonthlyRelease.Rmd** within R Studio.
1. **PowellMonthlyRelease.Rmd** - R markdown file with code to knit (run) to generate primary output file **PowellMonthlyRelease.pdf**.
1. **PowellMonthlyRelease.r** - R file with same code as **PowellMonthlyRelease.Rmd** but pushes results to console. Use for testing code.
1. **PowellMonthlyRelease.Rproject** - R project file. Use to open the project in R Studio.
1. **Powell-MonthlyReleaseSchedule.txt** - Comma seperated values (CSV) file with data downloaded from CRSS slot Powell.MonthlyReleaseTable. Rows are month of the year. Columns are annual release target. All values million acre-feet.

## Requested Citation
David E. Rosenberg (2020). "Powell Monthly Releases". Utah State University. Logan, Utah. https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/PowellMonthlyRelease.
