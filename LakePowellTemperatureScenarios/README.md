# How does Lake Powell water storage influence release temperatures and Grand Canyon fishes?

Data, scripts, and script outputs that support the analysis for How does Lake Powell water storage influence release temperatures and Grand Canyon fishes?
Briefly, create scenarios of release temperatures with different impacts for Grand Canyon fish. Then identify the range of reservoir water surface elevations that will provide those temperatures.
Elevation ranges are estimated for each temperature scenario for release through the turbine (elevation 3490 feet) and release through the river outlets (3370 feet).

## Data Sources
1. Fish temperature suitability - Fish temperature suitability - Table S1, page 39 from Dibble et al (2000, submitted). This is in file EAP20-06...pdf. Digitized version in FishTemperatureRequirements.xlsx, TempDibble worksheet.

2. Dibble/Yackulic Spreadsheet model of monthly release temperature as a function fo surface water elevation. TemperatureModel_GrandCanyonStorage.xlsx (unpublished)

3. Time series of Glen Canyon Dam release temperature provided by Bryce M. 15-minute time step. GCD_release_water_temp.csv

4. Time series of daily Glen Canyon Dam elevation - USBR (2020). Water Operations: Historic Data, Upper Colorado River Division, U.S. Buruea of Reclamation. https://www.usbr.gov/rsvrWater/HistoricalApp.html. LAKEPOWELL06-16-2020T16.32.29.csv.

5. Lake Powell Temperature Profiles. Vernieu, W. S. (2015). "Historical Physical and Chemical Data for Water in Lake Powell and from Glen Canyon Dam Releases, Utah-Arizona, 1964 –2013." Data Series 471, Version 3.0. https://pubs.usgs.gov/ds/471/pdf/ds471.pdf. 'qryProfiles at Primary Stations.csv'.

6. Lake Powell elevation zone definitions exported from the Colorado River Simulation System. PowellZones.xlsx

## Subfolders
1. **TempDataBryce** - csv files of water temperature at several downstream Grand Canyon river mile locations. Data from Milhalevich et al (in press) - https://usu.box.com/s/ur2rme52rs36frcv94xtydiosmnp528v 
2. **VoichickWrightData** - csv files of river temperature data. Voichick, N., and Wright, S. A. (2007). "Water-Temperature Data for the Colorado River and Tributaries Between Glen Canyon Dam and Spencer Canyon, Northern Arizona, 1988-2005." 251. http://pubs.er.usgs.gov/publication/ds251. 
   + GrandCanyonTempCompare.r - R script that loads the data and produces plots.
   + RiverTemperatureRangeByYearCDF.png - Daily range of river temperature (x-axis) by year (trace) and river mile (facet)
   + RiverTemperatureRangeByCDFs.png - Daily range of river temperature (x-axis) by month (trace) and river mile (facet)
   + TempVsRiverMile.png -Daily average water temperature by river mile (x-axis) by month (facet) and year (color). Shows progression of river temperature as water moves down the Grand Canyon.
   + Rosenberg-HowMuchWaterToStoreinPowellToBenefitNativeFish.docx - More edited/formatted version of LakePowellElevationTempUncertainty.pdf. Uses select figures.

## Script Files
1. **LakePowellElevationTempUncertainty.Rmd** -- Markdown document. First reads all five data sources, second specifies release temperature scenarios, third calculates range of release temperatures at specified water surface elevations (using data sources #3, 4, and 5), and finally generates several figures that show 
 reservoir water surface elevations by month to achieve different water release temperatures. Outputs figures and markdown text to the file **LakePowellElevationTempUncertainty.pdf**
 
2. **LakePowellElevationTempModelIntegration.r** -- R file with same code as **LakePowellElevationTempUncertainty.Rmd**. Just generates figures as images not .pdf.

3. **LakePowellElevationTempScenarios.r** -- R file with initial version of code of **LakePowellElevationTempModelIntegration.r**. Generates some of the same figures as **LakePowellElevationTempModelIntegration.r**.
Additionally generates plots that show the daily range of release temperature (PowellReleaseTempRangeByMonthCDF.png, PowellReleaseTempRangeByMonthYearCDF.png, PowellReleaseTempRangeByYearCDF.png). This script also compares Wahweap temperature profile at turbine elevation to turbine release temperature (CompareReleaseWaweap.png). 


## Output Files
1. **LakePowellElevationTempUncertainty.pdf** -- Pdf file generated by R Markdown file **LakePowellElevationTempUncertainty.Rmd**
2. All **.png** files -- figures generated by script files #2 or #3.
3. Rosenberg-HowDoesLakePowellStorageInfluenceReleaseTemperature.docx -- Final text and figures for side bar and appendix.

## Directions to Run
1. Download and install R and RStudio.
2. In your file explorer, open the subfolder (LakePowellTemperatureScenarios). Open the file **LakePowellTemperatureScenarios.Rproject**. R studio will open.
3. Select the file tab **LakePowellElevationTempUncertainty.Rmd**.
4. Click the **Knit** button (row below the file tabs)
5. R studio will run the blocks of code and markdown and generate the **LakePowellElevationTempUncertainty.pdf** file.
6. Alternatively in step 3, select the file tab **LakePowellElevationTempModelIntegration.r**
7. Select all the code in the file (ctrl-A) and click **Run** (row below the file tabs).
8. R studio will run the code, generate the figures, and store the figures as .png files in the same directory. 

## Requested Citation
Rosenberg, D and Lindsey Bruckerhoff(2021) "Appendix 2: How does Lake Powell water storage influence release temperatures and Grand Canyon fishes?" in Wheeler et al (2021) "White Paper 6. Alternative Management Paradigms for the Future of the Colorado and Green Rivers". https://qcnr.usu.edu/coloradoriver/futures.