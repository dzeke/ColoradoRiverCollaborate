## Lake Powell Temperature Profiles
## 
#
# This script does 3 things:
#
#   1. Injests temperature profile data from the USGS Lake Powell profiles: 
#         Andrews, C.M., and Deemer, B.R., 2022, Limnology data from Lake Powell, desert southwest USA (ver. 2.1, September 2024): U.S. Geological Survey data release, https://doi.org/10.5066/P9ZIKVYW. 
#       https://www.sciencebase.gov/catalog/item/62b39805d34e8f4977cb788c
#       
#       Files: StationData_LakePowell_1964-2022.csv (Station Meta data)
#              SiteVisitData_LakePowell_1964-2022.csv (Site visit meta data, including Lake Elevation and Intake Depth)
#              ProfileData_LakePOwell_1965-2022.csv (Temperature-Depth data)
#
#   2. Lake Powell Bathymetry (2017) from Bradley, D., and Collins, K. (2022). "Lake Powell 2017 Area and Capacity Tables." ENV-2021-98, Reclamation. https://doi.org/10.5066/P9O3IPG3.

#        File: Lake_Powell_Area_Capacity_Table_report_Final.xlsx
#       Note: this excel file was created from the raw data published in pdf form. See here for more specifics:
#         https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/LakePowellNewBathymetry
#        
#   3. Join all the above files together to get a single data frame from which we will perform the analysis.
#
#   https://www.usbr.gov/pn-bin/hdb/hdb.pl?svr=uchdb2&sdi=1862%2C1872%2C4167%2C4166&tstp=HR&t1=2024-05-01T00:00&t2=2025-07-10T00:00&table=R&mrid=0&format=csv
#
# David Rosenberg
# August 14, 2025

#Versions to use
#R version 4.1.1. Download from https://cran.r-project.org/.
#R Studio 2021.09.0. Build 351 - Download from https://www.rstudio.com/.

# Remove everything
rm(list=ls())

cPackages <- c("versions", "googlesheets4", "dygraphs", "tidyquant", "xts", "tidyverse", "tidyquant","lubridate", "stringr", "rvest", "RColorBrewer", "readxl" )

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
dfProfileJoined$LakeElevation_ft <- 3.28 * dfProfileJoined$LakeElevation_m
dfProfileJoined$BottomSounding_ft <- 3.28 * dfProfileJoined$BottomSounding_m
dfProfileJoined$IntakeDepth_ft <- 3.28 * dfProfileJoined$IntakeDepth_m

#Interpolate reservoir storage from lake elevation
###dfProfileJoined$ActiveVolume_acft <- interp2(xi = dfProfileJoined$LakeElevation_ft,x=dfMeadElevStor$`Elevation (ft)` , y=dfMeadElevStor$`Live Storage (ac-ft)`, method="linear")


#Turn to meaningful column names
cColNames <- colnames(dfReleases)
cColNames[2:5] <- c("PowerRelease", "TotalRelease", "BypassRelease", "SpillwayRelease")
colnames(dfReleases) <- cColNames 

#Save to csv
write.csv(dfReleases, "LakePowellReleases.csv")

# Convert API Date Time to POSIXct
dfReleases$DateTimePos <- as.POSIXct(as.character(dfReleases$DATETIME), format = "%m/%d/%Y %H:%M")


# Replace NaNs with NA in all columns
dfReleases <- na.omit(dfReleases)

#Convert to xts
dfXtsRelease <- xts(cbind(dfReleases$PowerRelease, dfReleases$BypassRelease, dfReleases$SpillwayRelease, dfReleases$TotalRelease), order.by=dfReleases$DateTimePos)

#Plot the Release dygraph
dygraph(dfXtsRelease, ylab = "Release (cfs)") %>% dyRangeSelector() %>%
  dySeries("V1", label = "PowerRelease") %>%
  dySeries("V2", label = "BypassRelease") %>%
  dySeries("V3", label = "SpillwayRelease") %>%
  dySeries("V4", label = "TotalRelease") # %>%
# #  dySeries("V5", label = "Outside") %>%
#dyLimit(32, color = "red")
