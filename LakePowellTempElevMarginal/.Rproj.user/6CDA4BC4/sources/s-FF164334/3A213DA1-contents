## Lake Powell Temperature Profiles
## 
#
# This script does 3 things:
#
#   1. Ingests temperature profile data from the USGS Lake Powell profiles: 
#         Andrews, C.M., and Deemer, B.R., 2022, Limnology data from Lake Powell, desert southwest USA (ver. 2.1, September 2024): U.S. Geological Survey data release, https://doi.org/10.5066/P9ZIKVYW. 
#       https://www.sciencebase.gov/catalog/item/62b39805d34e8f4977cb788c
#       
#       Files: StationData_LakePowell_1964-2022.csv (Station Meta data)
#              SiteVisitData_LakePowell_1964-2022.csv (Site visit meta data, including Lake Elevation and Intake Depth)
#              ProfileData_LakePOwell_1965-2022.csv (Temperature-Depth data)
#
#   2. Ingest Lake Powell Bathymetry (2017) from Bradley, D., and Collins, K. (2022). "Lake Powell 2017 Area and Capacity Tables." ENV-2021-98, Reclamation. https://doi.org/10.5066/P9O3IPG3.

#        File: Lake_Powell_Area_Capacity_Table_report_Final.xlsx
#       Note: this excel file was created from the raw data published in pdf form. See here for more specifics:
#         https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/LakePowellNewBathymetry
#        
#   3. Joins all the above files together to get a single data frame from which we will perform the analysis.
#
#   https://www.usbr.gov/pn-bin/hdb/hdb.pl?svr=uchdb2&sdi=1862%2C1872%2C4167%2C4166&tstp=HR&t1=2024-05-01T00:00&t2=2025-07-10T00:00&table=R&mrid=0&format=csv
#
# David Rosenberg
# August 22, 2025

#Versions to use
#R version 4.1.1. Download from https://cran.r-project.org/.
#R Studio 2021.09.0. Build 351 - Download from https://www.rstudio.com/.

# Remove everything
rm(list=ls())

cPackages <- c("versions", "googlesheets4", "dygraphs", "tidyquant", "xts", "tidyverse", "tidyquant","lubridate", "stringr", "rvest", "RColorBrewer", "readxl", "ggmap", "pracma" )

# Install packages not yet installed
installed_packages <- cPackages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(cPackages[!installed_packages])
}
# Packages loading
invisible(lapply(cPackages, library, character.only = TRUE))


#Load the required library 

#Load correct versions of libraries
install.versions('cli', '3.4.0')


# New function interp2 to return NAs for values outside interpolation range (from https://stackoverflow.com/questions/47295879/using-interp1-in-r)
interp2 <- function(x, y, xi = x, ...) {
  yi <- rep(NA, length(xi));
  sel <- which(xi >= range(x)[1] & xi <= range(x)[2]);
  yi[sel] <- interp1(x = x, y = y, xi = xi[sel], ...);
  return(yi);
}

### Stations we are interested in:
sStations <- c("LPCRRFG2", "LPCR0004", "LPCR0006", "LPCR0024", "LPCR-0001")


#######
###Read all data data into R

sStationDataFile <- 'data/StationData_LakePowell_1964-2022.csv'
sSiteVisitDataFile <- 'data/SiteVisitData_LakePowell_1964-2022.csv'
sProfileDataFile <- 'data/ProfileData_LakePOwell_1965-2022.csv'

dfStationData <- read.csv(sStationDataFile)
dfSiteVisitData <- read.csv(sSiteVisitDataFile)
dfProfileData <- read.csv(sProfileDataFile)

# Read elevation-storage data in from Excel
sExcelFile <- 'data/Lake_Powell_Area_Capacity_Table_report_Final.xlsx'
dfPowellBathymetry <- read_excel(sExcelFile, sheet = "2017Bathymetry",  range = "A1:D582")

####### Join the data
# Join Profile data to site visit data
# Seems like this statement should work (joining on the datetime), but we lose the intake and elevation data
#dfProfileJoined <- left_join(dfProfileData, dfSiteVisitData, by = c("TripID" = "TripID", "StationID" = "StationID", "CollectionDateTime" = "CollectionDateTime"))
dfProfileJoined <- left_join(dfProfileData, dfSiteVisitData, by = c("TripID" = "TripID", "StationID" = "StationID"))
# Join Profile and StationData
dfProfileJoined <- left_join(dfProfileJoined, dfStationData, by = c("StationID" = "StationID"))

# Convert meters to feet
dfProfileJoined$Depth_ft <- 3.28 * dfProfileJoined$Depth_m
dfProfileJoined$SurfaceElevation_ft <- 3.28 * dfProfileJoined$LakeElevation_m
dfProfileJoined$BottomSounding_ft <- 3.28 * dfProfileJoined$BottomSounding_m
dfProfileJoined$IntakeDepth_ft <- 3.28 * dfProfileJoined$IntakeDepth_m

# Convert Date to POSIXct format R understands
dfProfileJoined$PosDate <- as.POSIXct(dfProfileJoined$CollectionDateTime.x, format = "%m/%d/%Y %H:%M")

# Calculate the year, month, and dat
dfProfileJoined$Year <- year(dfProfileJoined$PosDate)
dfProfileJoined$Month <- month(dfProfileJoined$PosDate)
dfProfileJoined$day <- day(dfProfileJoined$PosDate)

# Calculate the intake elevation in feet to check that elevations are all the same
dfProfileJoined$IntakeElevationMid_ft <- dfProfileJoined$SurfaceElevation_ft - dfProfileJoined$IntakeDepth_ft
dfProfileJoined$MinimumPowerPool_ft <- 3490

dfProfileSummary <- dfProfileJoined %>% group_by(StationID, Description) %>% summarize(MinYear = min(Year), MaxYear = max(Year), Latitude = mean(Latitude), Longitude = mean(Longitude))

# Find the unique stations
cUniqueStats <- unique(dfProfileJoined$StationID)

#Filter on the stations we are interested in
dfProfileJoinedStats <- dfProfileJoined %>% filter(StationID %in% sStations)

# Summarize years of data and locations for those points
dfProfileSummary <- dfProfileJoinedStats %>% group_by(StationID, Description) %>% summarize(MinYear = min(Year), MaxYear = max(Year), Latitude = mean(Latitude), Longitude = mean(Longitude))


# Summarize years/months/days where profile data exists for sites of interest 
dfSiteProfileSummary <- dfProfileJoinedStats %>% group_by(StationID, Description, PosDate) %>% summarize(Latitude = mean(Latitude), Longitude = mean(Longitude))
# Convert SiteId to a factor
dfSiteProfileSummary$StationAsFactor <- as.factor(dfSiteProfileSummary$StationID)

# Plot a figure to show the dates profile data were taken at each site were sites are factors on the Y axis
ggplot(dfSiteProfileSummary %>% filter(year(PosDate) >= 2018), aes(x = PosDate, y = StationAsFactor)) +
  geom_point() +
  
  theme_bw() +
  
  labs(x="", y="Site") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20)) #, legend.title = element_text("Annual Release\nMAF"), legend.text=element_text(size=14), axis.text.x = element_text(size=12))


# Plot a figure to show the dates profile data were taken at each site were sites are factors on the Y axis
ggplot(data = dfSiteProfileSummary, aes(x = PosDate, y = PosDate) ) +
 
  geom_point(data = dfSiteProfileSummary %>% filter(StationAsFactor %in% sStations[4], year(PosDate) <= 1980), color = "blue") +
  geom_point(data = dfSiteProfileSummary %>% filter(StationAsFactor %in% sStations[5], year(PosDate) <= 1980), color = "red") +
  
  theme_bw() +
  
  labs(x="", y="", color = "Station")
  #labs(x="", y="Site") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20)) #, legend.title = element_text("Annual Release\nMAF"), legend.text=element_text(size=14), axis.text.x = element_text(size=12))




##### Print a map of Lake Powell to see where the Locations are

### Work more with Profile data
# Convert depth to reservoir level
dfProfileJoinedStats$Elevation_ft <- dfProfileJoinedStats$SurfaceElevation_ft - dfProfileJoinedStats$Depth_ft
  
#Interpolate reservoir storage from lake elevation
dfProfileJoined$ActiveVolume_maf <- interp2(xi = dfProfileJoined$SurfaceElevation_ft, x=dfPowellBathymetry$`ELEVATION (feet)`, y=dfPowellBathymetry$`CAPACITY (acre-feet)`, method="linear") / 1e6 #Million Acre-feet


# Plot the temperature profiles
ggplot(data = dfProfileJoined %>% filter(Year >= 2018), aes(x= Temperature_C, y = SurfaceElevation_ft - Depth_ft)) +
  geom_line() + 
  facet_wrap ( ~ PosDate) + 
  theme_bw() + 
  labs(x=" Temperature (oC)", y = "Elevation (feet)") +
  theme(text = element_text(size=20)) #, legend.title = element_text("Annual Release\nMAF"), legend.text=element_text(size=14), axis.text.x = element_text(size=12))

  

