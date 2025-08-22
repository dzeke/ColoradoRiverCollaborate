# How Low a Colorado River Flow to Go?

This document compiles and compares scenarios of extreme low Coloardo River flows used in 6 different prior studies.
The purpose is to compare the inflow scenarios across the studies that used river flow as an operations criteria.

The data for this comparision are located in the Excel file **ColoradoRiverExtremeLowFlowFlowScenarios.xlsx**.

A line segment plot shows the range of extreme low flows used in each study.

The studies are:
1. Myers, A. (2025). "Immersive Modeling for Lake Mead". https://github.com/Anabelle374/ImmersiveModelLakeMead"
1. Wang, J., and Rosenberg, D. E. (2023). "Adapting Colorado River Basin Depletions to Available Water to Live within Our Means." Journal of Water Resources Planning and Management, 149(7), 04023026. https://doi.org/10.1061/JWRMD5.WRENG-5555.
1. Rosenberg, D. E. (2024a). "Lessons from immersive online collaborative modeling to discuss more adaptive reservoir operations." Journal of Water Resources Planning and Management, 150(7). https://doi.org/10.1061/JWRMD5.WRENG-5893.
1. Abualqumboz, M., Chamberlain, B., and Rosenberg, D. (2024). "Adaptively Managing Lake Powell Releases to Respond to Reservoir Inflow and Evaporation." Utah State University Digital Commons. https://digitalcommons.usu.edu/cee_stures/12/.
1. Rosenberg, D. E. (2024b). "Reclamation Web Tool - Minimum Glen Canyon Dam Annual Release to protect Lake Powell Minimum Power Pool." Github, https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/Post2026WebTool [Accessed on: July 3, 2025].
1. Rosenberg, D. E. (2022). "Adapt Lake Mead Releases to Inflow to Give Managers More Flexibility to Slow Reservoir Drawdown." Journal of Water Resources Planning and Management, 148(10), 02522006. https://doi.org/10.1061/(ASCE)WR.1943-5452.0001592.
 
## View Results
Open the file **[ColoradoRiverExtremeLowFlowScneariosAll.pdf](ColoradoRiverExtremeLowFlowScenariosAll.pdf)**

## Requirements to Run
* R version 4.1.1. Download from https://cran.r-project.org/.
* R Studio 1.1.456. Download from https://www.rstudio.com/.

## Directions to Generate Results
1. Download and install R and RStudio (see requirements)
1. Within this subfolder, open the **ColoradoRiverExtremeLowFlowScenariosAll.Rproject** file. R Studio should open.
1. Select the **ColoradoRiverExtremeLowFlowScenariosAll.Rmd** tab (R markdown file) within R Studio.
1. Just below the tab, click the **Knit** button.
1. The code will run and generate the file **ColoradoRiverExtremeLowFlowScenariosAll.pdf**. Open the pdf file to view results.

## Explanation of Contents
1. **ColoradoRiverExtremeLowFlowScenarios.xlsx** - File with minimum and maximum extreme low flows for each study. All values million acre-feet.
1. **ColoradoRiverExtremeLowFlowScenariosAll.pdf** - Output file created when knit **ColoradoRiverExtremeLowFlowScenariosAll.Rmd** within R Studio.
1. **ColoradoRiverExtremeLowFlowScenariosAll.Rmd** - R markdown file with code to knit (run) to generate primary output file **ColoradoRiverExtremeLowFlowScenariosAll.pdf**.
1. **ColoradoRiverExtremeLowFlowScenariosAll.r** - R file with same code as **ColoradoRiverExtremeLowFlowScenariosAll.Rmd** but pushes results to console. Use for testing code.
1. **ColoradoRiverExtremeLowFlowScenariosAll.Rproject** - R project file. Use to open the project in R Studio.

## Link to Full Blog Post
**[How Low a Colorado River Flow to Go?](https://usu.app.box.com/file/1962917545522?s=1wkm7bfm86cih76zf0ar6z9c548hr0a7)**

## Requested Citation
David E. Rosenberg, Anabelle Myers, Erik Porse (2025), “How Extreme Low Colorado River Flows to Use?” Utah State University. Logan, Utah.
https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/ColoradoRiverExtremeLowInflowsAll
