# Lake Mead Inflow

This folder contains the data/model/code/directions to estimate inflow to Lake Mead. There are 4 methods and each method gives different values.

1. From U.S.G.S. stream gages (1990 to Present)
1.1. Colorado River nr Peach Springs [9404200; https://waterdata.usgs.gov/monitoring-location/09404200/#parameterCode=00065&timeSeriesId=6324&period=P7D] (1990 to present)
1.1. Virgin River at Littlefield [9415000; https://waterdata.usgs.gov/monitoring-location/09415000/#parameterCode=00065&period=P7D] (1930 to present)
1.1. Las Vegas Wash Below LAKE LAS VEGAS NR BOULDER CITY, NV [09419800; https://waterdata.usgs.gov/monitoring-location/09419800/] (2002 to present)
1. From Reclamation Application Programming Interface (2015 to present).
1. Backcalculating from Lake Mead Release, Storage, and evaporation data retrieved via the Reclamation Application Programming Interface (Method #2).
1. Backcalculating from Lake Mead Release, Storage, and evaporation estimated from lake level using pan coefficients.

The code produces figures that compare the inflow values across the method.
The code produces a figure that shows Inflow, Evaporation, Available Water, and Water Conservation Program deposits for which there was sufficient available water.

## View Results
Open the file **LakeMeadInflow.pdf**.

## Requirements to Run
* R version 4.1.1. Download from https://cran.r-project.org/.
* R Studio 1.1.456. Download from https://www.rstudio.com/.

## Directions to Generate Results
1. Download and install R and RStudio (see requirements)
1. Within this subfolder, open the **LakeMeadInflow.Rproject** file. R Studio should open.
1. Select the **LakeMeadInflow.Rmd** tab (R markdown file) within R Studio.
1. Just below the tab, click the **Knit** button.
1. The code will run and generate the file **LakeMeadInflow.pdf**. Open the pdf file to view results.

## Explanation of Contents
1. **SalehabadiEtAl-SequenceAverage** - Folder with code of sequence averaging method. Downloaded from Salehabadi and Tarboton (2020). Code from SeqAvePlot.R was modified and moved in **GrandCanyonTribFlow.Rmd** and **GrandCanyonInterveneFlow.r** files to work with the USGS and Natural flow datasets.
1. **LakeMeadInflow.Rmd** - R markdown file with code to knit (run) to generate primary output file **GrandCanyonTribFlow.pdf**.
1. **LakeMeadInflow.r** - R file with same code as **GrandCanyonTribFlow.Rmd** but pushes results to console. Use for testing code.
1. **LakeMeadInflow.Rproject** - R project file. Use to open the project.
1. **HistoricalNaturalFlow.xlsx** - Excel file with monthly natural flow data from Prairie et al (2020). Only monthly sheet retained. See readme worksheet for more information.
1. **Supplementary_file-WangSchmidt.xlsx** - The excel file from Wang and Schmidt (2020) that compares USGS and Natural flow data for their period of analysis. See readme worksheet for more info.
1. **USGSInterveningFlowData.xlsx** - Excel data with USGS data for Colorado River near Peach Springs [9404200], Colorado River at Lees Feery [9382000], and Virign River at Littlefield [9415000] downloaded from USGS data service.

## Requested Citation
David E. Rosenberg (2020). "Grand Canyon Intervening Flow". Utah State University. Logan, Utah. https://github.com/dzeke/ColoradoRiverCoding/tree/main/GrandCanyonInterveningFlow.

## References
Prairie, J. (2020). "Colorado River Basin Natural Flow and Salt Data." U.S. Bureau of Reclamation. https://www.usbr.gov/lc/region/g4000/NaturalFlow/current.html.

Salehabadi, H., and D. Tarboton (2020), Sequence-Average and Cumulative Flow Loss Analyses for Colorado River Streamflow at Lees Ferry, edited, Hydroshare. http://www.hydroshare.org/resource/bbe8dffacb07458783b2e6924aa615bb.

Wang, J., and Schmidt, J. C. (2020). "Stream flow and Losses of the Colorado River in the Southern Colorado Plateau." Center for Colorado River Studies, Utah State University, Logan, Utah. https://qcnr.usu.edu/coloradoriver/files/WhitePaper5.pdf.
