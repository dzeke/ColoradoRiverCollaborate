# How does Lake Powell Release Temperature Change with small (marginal) increases in lake elevation?

Data, scripts, and script outputs that support the analysis for How does Lake Powell release temperature change with small (marginal) changes in elevation?
This analysis supports the study of small seasonal changes in reservoir elevation to help disrupt Small Mouth Bass in Grand Canyon.
Briefly, at a particular month/year, we want the temperature profile at the dam face. We then calculate the change in temperature from the temperature at the penstock elevation to small elevations below
the penstock elevation, such as 2, 5, 7.5, or 10 meters.

## Data Sources
1. Glen Canyon Dam Release Water Temperature near Page AZ - gcmrc20250709125927.tsv (csv file). Grand Canyon Monitoring and Research Center https://www.gcmrc.gov/discharge_qw_sediment/station/GCDAMP/09379901#.

2. Glen Canyon Dam hourly release data. Reclamation HDB Online Data Query Tool

		You can manually query data using the following page and entering the following info: https://www.usbr.gov/lc/region/g4000/riverops/_HdbWebQuery.html
		A.	Upper Colorado Regional Office
		B.	1862, 1872, 4167, 4166  (copy and paste these ID's;  power release, total release, bypass release, spillway release) 
		C.	Hourly 
		D.	2024-07-01 00:00 and 2024-12-01 00:00 (cool mix period)
		E.	Observed
		F.	CSV
		Then click "build request" and click on the url. 

3. Lake Powell Temperature Profiles. Andrews, C.M., and Deemer, B.R., 2022, Limnology data from Lake Powell, desert southwest USA (ver. 2.1, September 2024): U.S. Geological Survey data release, https://doi.org/10.5066/P9ZIKVYW4. Lake Powell elevation zone definitions exported from the Colorado River Simulation System. PowellZones.xlsx

		A. Temperature profile data - ProfileData_LakePowell_1965-2022.csv (Trip, StationID, stations, Date/Time, Depth, Temperature)
		B. Site visit data - SiteVisitData_LakePowell_1965-2022.csv (TripID, TripTYpe, StationID, CollectionDateTime, Lake Elevation)
		C. Station Data - StationData_LakePowell_1964-2022.csv (StationID, Latitude, Longitude, Description, RiverArm)

## Subfolders
1. **Data** - Where downloaded csv files for sources #1 (Release water temperature) and #3 (temperature profiles) are located.

## Script Files
1. **___________.Rmd** -- Markdown document. First reads all five data sources, second specifies release temperature scenarios, third calculates range of release temperatures at specified water surface elevations (using data sources #3, 4, and 5), and finally generates several figures that show 
 reservoir water surface elevations by month to achieve different water release temperatures. Outputs figures and markdown text to the file **LakePowellElevationTempUncertainty.pdf**
 
2. **_____________.r** -- R file with same code as **LakePowellElevationTempUncertainty.Rmd**. Just generates figures as images not .pdf.

3. **_______________.r** -- R file with initial version of code of **LakePowellElevationTempModelIntegration.r**. Generates some of the same figures as **LakePowellElevationTempModelIntegration.r**.


## Output Files
1. **_____________________** -- Pdf file generated by R Markdown file **LakePowellElevationTempUncertainty.Rmd**
2. All **.png** files -- figures generated by script files #2 or #3.

## Directions to Run
1. Download and install R and RStudio.
2. In your file explorer, open the subfolder (**LakePowellTempElevMarginal). Open the file **________________.Rproject**. R studio will open.
3. Select the file tab **_________________________.Rmd**.
4. Click the **Knit** button (row below the file tabs)
5. R studio will run the blocks of code and markdown and generate the **__________________.pdf** file.
6. Alternatively in step 3, select the file tab **___________________.r**
7. Select all the code in the file (ctrl-A) and click **Run** (row below the file tabs).
8. R studio will run the code, generate the figures, and store the figures as .png files in the same directory. 

## Requested Citation
Rosenberg, D (2025) "How does Lake Powell Release Temperature Change with small (marginal) increases in lake elevation?" Github. https://qcnr.usu.edu/coloradoriver/futures.
