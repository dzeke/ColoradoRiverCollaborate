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


# Read Reclamation API data data into R
# Get Today's date as ending time
vDateToday = Sys.Date()
sStartDate = '2024-05-01' # May 1, 2025
sFormat = 'table'

# As CSV
sAPI_Request <- paste0('https://www.usbr.gov/pn-bin/hdb/hdb.pl?svr=uchdb2&sdi=1862%2C1872%2C4167%2C4166&tstp=HR&t1=',sStartDate,'T00:00&t2=',vDateToday,'2025-07-10T00:00&table=R&mrid=0&format=',sFormat)

sWebpage <- read_html(sAPI_Request)

tables <- sWebpage %>% html_table()

dfTable <- tables[1]

dfReleases <- read.csv(file = sAPI_Request, header = FALSE)
x <- read_sheet('https://docs.google.com/spreadsheets/d/1tHLUTlAmzTD9x-3RxqSyf2IwYfvKhWXMp_n0GDVbNDE/edit#gid=0')
#Save Incase need again
x2 <- x

#Turn erroneous values to NA
dfCols <- data.frame(colNum = c(2,4,8,9),
                     minVal = c(40,40,32,-20),
                     maxVal = c(90,90,100,140))

for(i in 1:nrow(dfCols)){
   x[,dfCols$colNum[i]] <- replace(x[,dfCols$colNum[i]], x[,dfCols$colNum[i]] < dfCols$minVal[i], NA)
   x[,dfCols$colNum[i]] <- replace(x[,dfCols$colNum[i]], x[,dfCols$colNum[i]] > dfCols$maxVal[i], NA)
   
}

# Convert Google Date/Time/Zone to POSIXct
x$DateTimeStr <- (str_trunc((x$Time), 24, side="right", ellipsis = ""))

x$DateTime <- as.POSIXct(x$DateTimeStr, format = '%a %b %d %Y %H:%M:%S')

#Calculate difference between date/time in row and subsequent row
x$DateTimeDiff <- c(NA, diff(x$DateTime))
#Assign NA to row with DateTimeDiff > 1000
x$DateTimeDiff[1] <- 0
x[x$DateTimeDiff > 1000,seq(2,9,1)] <- NA

# Split into Temperature and Humidity data frames
dfTemp <- x[,c(11,2,4,6,8,9)]
dfHumidity <- x[,c(11,3,5,7)]

#Convert to xts
xTemp <- xts(cbind(dfTemp$`Indoor Temp`,dfTemp$`Guesthouse Temp`,dfTemp$`Greenhouse Temp`,dfTemp$`Water Temp`,dfTemp$`Outside Temp`), order.by=dfTemp$DateTime)

#Write to csv
write.csv(xTemp, file="GreenhouseData.csv")

#Plot the dygraph

dygraph(xTemp) %>% dyRangeSelector() %>%
  dySeries("V1", label = "Indoor") %>%
  dySeries("V2", label = "Guesthouse") %>%
  dySeries("V3", label = "Greenhouse") %>%
  dySeries("V4", label = "Water barrel") %>%
  dySeries("V5", label = "Outside") %>%
  dyLimit(32, color = "red")