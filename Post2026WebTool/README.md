# Reclamation Web Tool - Minimum Glen Canyon Dam Annual Release to protect critical elevations

This repository shares results that summarize analysis in Reclamations Post-2026 Operations Exploration Tool with the goal to identify 
minimum Glen Canyon Dam annual releases needed to protect Lake Powell from drawdown below elevations 3,490 feet (Minimum power pool) and 3,525 feet (Protection elevation defined in 2019 Drought Contingency Plan).
This analysis includes results across the 6 hydrologic ensambles available in the tools. The web tool is also call a decision making under deep uncertainty (DMDU) tool.

Link to Reclamation's Web - https://tool.crbpost2026dmdu.org/.

The 5 storyline hydrologic ensambles are:
1. 11 maf, infrequent high flows (8 traces).
1. Decreasing Trend from 12 to 10 maf (8 traces).
1. Drought through 2028 then 11.5 maf (8 traces).
1. Early High Flows, then 12 maf (8 traces).
1. Late 20230s Drought (8 traces).
A 6th ensamble includes a grouping of other previously used ensambles (Historical record, stress test, CMIP3, CMIP5, Decreasing)
1. Default optimization (100s of traces).
 
## Key Findings
1. Need Glen Canyon Dam annual release from 4.0 to 6.0 million acre-feet per year to protect Lake Powell drawdown below elevation 3,490 feet across the 5 storyline ensambles.
1. Only two alternatives within the database are able to prevent drawdown below elevation 3,490 feet across **all** ensambles. 

## View Results
1. Open the file **[DMDU-Performance.xlsx](DMDU-Performance.xlsx)**
1. Navigate to the worksheet **PivotTable**.

## Requirements to Run
* A web browser that can support the DMDU Tool.
* License for Excel 360.

## Directions to Reproduce Results
1. Navigate to the website https://tool.crbpost2026dmdu.org/.
1. If needed, create a user name and password.
1. Log into the website using credentials creased in Step #2.
1. Select the **Welcome** Tab from the left menu. Read the documentation plus instructions to start (Tool 101, Resource Center, Video, New User, Learn About Paradigms, Explore Operational Strategies).
1. When ready, click the **Upload Session File** on the top.
1.1. Select **Browse** and navigate the the folder on your harddrive where you downloaded this repository.
1.1. Select the file *dmdu-2024-10-11.json*. This file loads the session with the most recent settings used to query the database of 700+ alternatives and show results over various filters.
1. Click the **Operational Strategies** on the left menu. Read the documentation for the strategies (Overview, Operational Paradigms, Lake Powell Release Concepts, Lower Basin Delivery Concepts).
1. Click the **Operational Strategies Suite** tab (top). A table with 5 selected strategies (associated with the session in step 5.2]) appear in a pop-out window. The strategies are:
1.1. STGY85558, 49068, 89269, 98230, 95580, 26992.

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