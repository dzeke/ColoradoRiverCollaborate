## Powell Outlet Releases and plot in dygraphs
## We also load Glen Canyon Dam release temperature
#
# This script does two things:
#   1. Download Glen Canyon Dam Turbine, River Outlet, and Total Releases from Reclamation's Data Portal and plot as an interactive Time-Series (DyGraph)
#   2. Plot Release Temperature Data from Grand Canyon Monitoring and Research Center Website
#
# More details on the Data Sources
#   1. Glen Canyon Dam Release Water Temperature near Page AZ - gcmrc20250709125927.tsv (csv file). Grand Canyon Monitoring and Research Center https://www.gcmrc.gov/discharge_qw_sediment/station/GCDAMP/09379901#.
#   2. Glen Canyon Dam hourly release data. Reclamation HDB Online Data Query Tool

#You can manually query data using the following page and entering the following info: https://www.usbr.gov/lc/region/g4000/riverops/_HdbWebQuery.html
#   A.	Upper Colorado Regional Office
#   B.	1862, 1872, 4167, 4166  (copy and paste these ID's;  power release, total release, bypass release, spillway release) 
#		C.	Hourly 
#		D.	2024-07-01 00:00 and 2024-12-01 00:00 (cool mix period)
#		E.	Observed
#		F.	CSV
#		Then click "build request" and click on the url. 
#
#   URL Build Request with online interactive plot on Reclamation's Website:
#     https://www.usbr.gov/pn-bin/hdb/hdb.pl?svr=uchdb2&sdi=1862%2C1872%2C4167%2C4166&tstp=HR&t1=2024-05-01T00:00&t2=2025-07-10T00:00&table=R&mrid=0&format=graph
#
#   URL Build Request as CSV:
#     https://www.usbr.gov/pn-bin/hdb/hdb.pl?svr=uchdb2&sdi=1862%2C1872%2C4167%2C4166&tstp=HR&t1=2024-05-01T00:00&t2=2025-07-10T00:00&table=R&mrid=0&format=csv
#
# David Rosenberg
# July 10, 2025

#Versions to use
#R version 4.1.1. Download from https://cran.r-project.org/.
#R Studio 2021.09.0. Build 351 - Download from https://www.rstudio.com/.

# Remove everything
rm(list=ls())

cPackages <- c("versions", "googlesheets4", "dygraphs", "tidyquant", "xts", "tidyverse", "tidyquant","lubridate", "stringr", "rvest" )

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


#######
###Read Reclamation API Release data data into R

# Get Today's date as ending time
vDateToday = Sys.Date()
sStartDate = '2024-05-01' # May 1, 2025
sFormat = 'table'

# As CSV
sAPI_Request <- paste0('https://www.usbr.gov/pn-bin/hdb/hdb.pl?svr=uchdb2&sdi=1862%2C1872%2C4167%2C4166&tstp=HR&t1=',sStartDate,'T00:00&t2=',vDateToday,'T00:00&table=R&mrid=0&format=',sFormat)

sWebpage <- read_html(sAPI_Request)

tables <- sWebpage %>% html_table()

dfReleases <- as.data.frame(tables[1])

#Turn to meaningful column names
cColNames <- colnames(dfReleases)
cColNames[2:5] <- c("PowerRelease", "TotalRelease", "BypassRelease", "SpillwayRelease")
colnames(dfReleases) <- cColNames 

#Save to csv
write.csv(dfReleases, "LakePowellReleases.csv")

# Convert API Date Time to POSIXct
dfReleases$DateTimePos <- as.POSIXct(as.character(dfReleases$DATETIME), format = "%m/%d/%Y %H:%M")

#Left Join the Temperature data to the release data so only have hourly data.
#dfReleasesTemp <- left_join(dfReleases, dfTemperature, by = c("DateTimePos" = "DateTimePos"))

# Replace NaNs with NA in all columns
dfReleases <- na.omit(dfReleases)

#Convert to xts
dfXtsRelease <- xts(cbind(dfReleases$PowerRelease, dfReleases$BypassRelease, dfReleases$SpillwayRelease, dfReleases$TotalRelease), order.by=dfReleases$DateTimePos)

#Plot the Release dygraph
dygraph(dfXtsRelease) %>% dyRangeSelector() %>%
  dySeries("V1", label = "PowerRelease") %>%
  dySeries("V2", label = "BypassRelease") %>%
  dySeries("V3", label = "SpillwayRelease") %>%
  dySeries("V4", label = "TotalRelease") # %>%
#  dySeries("V5", label = "Outside") %>%
#dyLimit(32, color = "red")



###### Read in Temperature data from Grand Canyon Monitoring and Research Center
####
dfTemperature <- read.csv(file = "Data/gcmrc20250709125927.tsv", sep = "\t")
colnames(dfTemperature) <- c("DateTime", "TemperatureC")

# Filter for top of the hour (00:00)
dfTemperature$Minute <- minute(dfTemperature$DateTime)
dfTemperature <- dfTemperature %>% filter(Minute == 0)
dfTemperature$TemperatureC <- na_if(dfTemperature$TemperatureC, -999)
#Convert DateTime to POSIXct
dfTemperature$DateTimePos <- as.POSIXct(as.character(dfTemperature$DateTime), format = "%Y-%m-%d %H:%M:%S")
dfTemperature <- na.omit(dfTemperature)

#Convert to xts
dfXtsTemperature <- xts(cbind(dfTemperature$TemperatureC), order.by=dfTemperature$DateTimePos)

dygraph(dfXtsTemperature) %>% dyRangeSelector() %>%
  dySeries("V1", label = "Temperature") #%>%



