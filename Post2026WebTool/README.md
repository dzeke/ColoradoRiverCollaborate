# Reclamation Web Tool - Minimum Glen Canyon Dam Annual Release to protect critical elevations

This repository shares results that summarize analysis in Reclamations Post-2026 Operations Exploration Tool with the goal to identify 
minimum Glen Canyon Dam annual releases needed to protect Lake Powell from drawdown below elevations 3,490 feet (Minimum power pool) and 3,525 feet (Protection elevation defined in 2019 Drought Contingency Plan).
This analysis includes results across the 6 hydrologic ensambles available in the tools.

Link to Reclamation's Web - https://tool.crbpost2026dmdu.org/.

The 5 storyline ensambles are:
1. 11 maf, infrequent high flows (8 traces).
1. Decreasing Trend from 12 to 10 maf (8 traces).
1. Drought through 2028 then 11.5 maf (8 traces).
1. Early High Flows, then 12 maf (8 traces).
1. Late 20230s Drought (8 traces).
A 6th ensamble includes a grouping of other previously used ensambles
1. Default optimization (100s of traces).
 
## Key Findings
1. Need Glen Canyon Dam annual release from 4.0 to 6.0 million acre-feet per year to protect Lake Powell drawdown below elevation 3,490 feet.
1. Only two alternatives within the data base are able to prevent drawdown below elevation 3,490 feet across **all** ensambles.



## View Results
Open the file **[DMDU-Performance.xlsx](DMDU-Performance.xlsx)**
Navigate to the worksheet **PivotTable**.

## Requirements to Run
* R version 4.1.1. Download from https://cran.r-project.org/.
* R Studio 1.1.456. Download from https://www.rstudio.com/.

## Directions to Reproduce Results
1. 
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
David E. Rosenberg (2024), “Reclamation Web Tool - Minimum Glen Canyon Dam Annual Release to protect critical elevations” Utah State University. Logan, Utah.
https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/Post2026WebTool.