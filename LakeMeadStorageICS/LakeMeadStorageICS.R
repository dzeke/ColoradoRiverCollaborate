# LakeMeadStorageICS.R
#
# Plots that show Lake Mead Storage and Water Conservation Account (ICS) Balances.
#
# 1. Import the reservoir elevation-volume curves
# 2. Import historical reservoir storage data
# 3. Import the conservation account data for Lake Mead
#
# This is a beginning R-programming effort! There could be lurking bugs or basic coding errors that I am not even aware of.
# Please report bugs/feedback to me (contact info below)
#
# David E. Rosenberg
# June 21, 2024
# Utah State University
# david.rosenberg@usu.edu

rm(list = ls())  #Clear history

#Load packages in one go
#List of packages
load.lib <- c("tidyverse", "readxl", "RColorBrewer", "dplyr", "expss", "reshape2", "pracma", "lubridate", "directlabels", "plyr", "stringr", "ggplot2", "ggpubr", "rvest", "tidyr")
# Then we select only the packages that aren't currently installed.
install.lib <- load.lib[!load.lib %in% installed.packages()]
# And finally we install the missing packages, including their dependency.
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
# After the installation process completes, we load all packages.
sapply(load.lib,require,character=TRUE)


# New function interpNA to return NAs for values outside interpolation range (from https://stackoverflow.com/questions/47295879/using-interp1-in-r)
interpNA <- function(x, y, xi = x, ...) {
  yi <- rep(NA, length(xi));
  sel <- which(xi >= range(x)[1] & xi <= range(x)[2]);
  yi[sel] <- interp1(x = x, y = y, xi = xi[sel], ...);
  return(yi);
}

###################################################
#### PLOT for MEAD Storage and Water Conservation Account Balances
###All the input data

## Read elevation-storage data in from Excel
sExcelFile <- 'MeadDroughtContingencyPlan.xlsx'
dfMeadElevStor <- read_excel(sExcelFile, sheet = "Mead-Elevation-Area",  range = "A4:D676")

## Read in ICS account balance data
sExcelFile <- 'IntentionallyCreatedSurplus-Summary.xlsx'
dfICSBalance <- read_excel(sExcelFile, sheet = "Balances")
#Save the most recent year of ICS data
nMaxYearICSData <- max(dfICSBalance$Year)
#Register the largest year of reservoir data. Right now one larger than ICS
nMaxYearResData <- nMaxYearICSData + 1
 
#Duplicate the largest year and set the year to largest value plus 1
dfICSBalance <- rbind(dfICSBalance, dfICSBalance %>% filter(Year == nMaxYearICSData) %>% mutate(Year = nMaxYearICSData+1))
#Order by decreasing year
dfICSBalance <- dfICSBalance[order(-dfICSBalance$Year),]
#Turn time into a index by month. Year 1 = 1, Year 2 = 13
dfICSBalance$MonthIndex <- 12*(dfICSBalance$Year - dfICSBalance$Year[nrow(dfICSBalance)]) + 12

#Turn the ICS year into monthly
dfICSmonths = expand.grid(Year = unique(dfICSBalance$Year), month = 1:12)
dfICSmonths$MonthIndex <- 12*(dfICSmonths$Year - dfICSmonths$Year[nrow(dfICSmonths)]) + dfICSmonths$month
#Filter off first year but keep last month
dfICSmonths <- dfICSmonths %>% filter(dfICSmonths$MonthIndex >= 12)
#Calculate a date
dfICSmonths$Date <- as.Date(sprintf("%d-%d-01",dfICSmonths$Year, dfICSmonths$month))

#Interpolate Lower Basin conservation account balances by Month
dfICSmonths$LowerBasinConserve <- interp1(xi = dfICSmonths$MonthIndex, x=dfICSBalance$MonthIndex, y = dfICSBalance$Total, method="linear" )
#Interpolate Mexico conservation account balance by Month
dfICSmonths$MexicoConserve <- interp1(xi = dfICSmonths$MonthIndex, x=dfICSBalance$MonthIndex, y = dfICSBalance$Mexico, method="linear" )

#Set values above the max ICS date to zero
dfICSmonths[dfICSmonths$Year > nMaxYearICSData, c("LowerBasinConserve", "MexicoConserve")] <- 0


## Data frame of key elevations
nMeadProtectElevation <- 1020
nProtectMead <- interpNA(xi = nMeadProtectElevation, x= dfMeadElevStor$`Elevation (ft)`, y=dfMeadElevStor$`Live Storage (ac-ft)`) / 1e6
nCapacityMead <- 25.9

dfKeyMeadVolumes <- data.frame(Volume = c(nProtectMead, nCapacityMead ), Label = c("Protect","Capacity"))

## Data frame of key dates
dfKeyDates <- data.frame(Date = as.Date(c("2007-01-01", "2027-01-01")), Label = c("Interim\nGuidelines", "Guidelines\nExpire"))

## Data frame of key Mead traces
dfKeyMeadTraceLabels <- data.frame(Label = c("Protect", "Public Pool", "Water\nConservation\nAccounts"),
                               Volume = c(nProtectMead/2, 8, 11.5), xPosition = c(2012, 2012, 2023),
                               Size = c(6, 6, 5))


# ### Load Mead Storage data from cross-tabulated Excel file with year as rows and months as columns
# sMeadHistoricalFile <- 'MeadLevelApril2024.xlsx'
# # Read in the historical Mead data
# dfMeadHistorical <- read_excel(sMeadHistoricalFile)
# 
# #Convert cross-tabulated Mead months into timeseries
# dfMeadHist <- melt(dfMeadHistorical, id.vars = c("Year"))
# dfMeadHist$BeginOfMonStr <- paste(dfMeadHist$Year,dfMeadHist$variable,"1",sep="-")
# dfMeadHist$BeginOfMon <- as.Date(dfMeadHist$BeginOfMonStr, "%Y-%b-%d")
# dfMeadHist$BeginNextMon <- dfMeadHist$BeginOfMon %m+% months(1)
# #Filter out NAs
# dfMeadHist <- dfMeadHist %>% filter(!is.na(dfMeadHist$value))
# #Convert the text values to numerics
# dfMeadHist$value <- as.numeric(dfMeadHist$value)
# #Filter out low storages below min
# dfMeadHist <- dfMeadHist %>% filter(dfMeadHist$value > min(dfMeadElevStor$`Elevation (ft)`))
# dfMeadHist$Stor <- interp1(xi = dfMeadHist$value,y=dfMeadElevStor$`Live Storage (ac-ft)`,x=dfMeadElevStor$`Elevation (ft)`, method="linear")
# 
# #Merge the Mead and Powell Storage Time series
# dfJointStorage <- dfMeadHist
# 
# #Rename columns so they are easier to distinquish
# #dfJointStorage$PowellStorage <- dfJointStorage$Storage..af./1000000
# #dfJointStorage$PowellRelease <- dfJointStorage$Total.Release..cfs.
# dfJointStorage$MeadStorage <- dfJointStorage$Stor/1000000
# #dfJointStorage <- merge(dfPowellHist[,c("DateAsValue","Storage..af.","Total.Release..cfs.")],dfMeadHist[,c("BeginNextMon","Stor")],by.x = "DateAsValue", by.y="BeginNextMon", all.x = TRUE, sort=TRUE)
# 
# dfJointStorage$Date <- dfJointStorage$BeginOfMon

#### Load the Mead data from the USBR API
# 1990 to Present

#Dynamically read to the current date
CurrDate <- as.Date(Sys.Date())
cYear <- year(CurrDate)
cMonth <- month(CurrDate)

#Calculate the prior month
if (cMonth == 1) {
  # We want December of the prior year
  sDate <- sprintf("%d-%d-01", cYear-1, 12)
  } else {
  # We take the prior month of the same year
  sDate <- sprintf("%d-%d-01", cYear, cMonth - 1)
}

#Construct the USBR API call by reading data up to the prior month
usbr_url <- paste0("https://www.usbr.gov/pn-bin/hdb/hdb.pl?svr=lchdb&sdi=1776%2C2091%2C1721%2C1874&tstp=MN&t1=1990-01-01T00:00&t2=", sDate, "T00:00&table=R&mrid=0&format=html")

usbr_MeadData <- read_html(usbr_url)

pkg_data <- usbr_MeadData |>
  html_element("table") |>
  html_table()

dfUSBR_API <- data.frame(pkg_data)

#Save the API data to csv to improve reproducibility and in case no internet
write.csv(dfUSBR_API, "dfUSBR_API.csv")

#Turn the SDID Code # into meaningful variable names
dfSDIDcode <- data.frame(code = c(1776, 2091, 1721, 1874),
                         Field = c("Evaporation", "Inflow", "Storage", "Release"),
                         Units = c("acre-feet", "??", "acre-feet", "cfs"))

cSDID <- colnames(dfUSBR_API)
cSDID[2:5] <-dfSDIDcode$Field
colnames(dfUSBR_API) <- cSDID

dfJointStorage <- dfUSBR_API

### This statement not converting date as string to data as value correctly
dfJointStorage$DateAsValue <- as.Date(dfJointStorage$DATETIME, "%m/%d/%Y %H:%M")
#Add a column for decade
dfJointStorage$decade <- round_any(as.numeric(format(dfJointStorage$DateAsValue,"%Y")),10,f=floor)
#dfJointStorage$DecadeAsClass <- dfJointStorage %>% mutate(category=cut(decade, breaks=seq(1960,2020,by=10), labels=seq(1960,2020,by=10)))

#Calculate the annual volume drop from each October 1
#Calculate month
dfJointStorage$month <- month(dfJointStorage$DateAsValue)

#Left join the ICS data to the joint storage data to get the entire date range
dfJointStorage <- left_join(dfJointStorage, dfICSmonths, by=c("DateAsValue" = "Date"))
#Convert NAs to zeros
dfJointStorage$Year <- year(dfJointStorage$DateAsValue)
#dfJointStorageClean <- dfJointStorage[,2:ncol(dfJointStorage)] %>% filter(Year <= nMaxYearICSData)

#Allow to go one more year
#dfJointStorageClean <- dfJointStorage[,2:ncol(dfJointStorage)] %>% filter(Year <= nMaxYearResData)
dfJointStorageClean <- dfJointStorage %>% filter(Year <= nMaxYearResData)


dfJointStorageClean[is.na(dfJointStorageClean)] <- 0
dfTemp <- dfJointStorage %>% filter(Year <= nMaxYearResData) %>% select(DateAsValue)
dfJointStorageClean$DateAsValue <- dfTemp$DateAsValue
dfJointStorageClean$MeadStorage <- dfJointStorageClean$Storage / 1e6
dfJointStorageClean$Stor <- dfJointStorageClean$MeadStorage


#Add rows for years until 2030 with all zeros
dfYearsAdd <- data.frame(Year = seq(nMaxYearResData+1, nMaxYearICSData + 10, by = 1))
#dfJointStorageZeros <- dfJointStorageClean[1,1:(ncol(dfJointStorageClean)-1)]
dfJointStorageZeros <- dfJointStorageClean[1, ]
#dfJointStorageZeros <- 0
dfJointStorageZeros$MeadStorage <- 0
dfJointStorageZeros$Stor <- 0

dfJointStorageZeros <- as.data.frame(lapply(dfJointStorageZeros,  rep, nrow(dfYearsAdd)))
dfJointStorageZeros$Year <- dfYearsAdd$Year
#Calculate a date
dfJointStorageZeros$DateAsValue <- as.Date(sprintf("%.0f-01-01", dfJointStorageZeros$Year))
#Bind to the Clean data frame
dfJointStorageClean <- rbind(dfJointStorageClean, dfJointStorageZeros)

## Data for the stacked plot
#New data frame for area
dfMeadStorageStack <- dfJointStorageClean

# Stack as protect => Public Pool => Water Conservation Account balance => Reservoir volume
dfMeadStorageStack$Protect <- nProtectMead
dfMeadStorageStack$LowerBasin <- ifelse(dfMeadStorageStack$Year <= nMaxYearResData, dfMeadStorageStack$LowerBasinConserve/1e6, 0)
dfMeadStorageStack$Mexico <- ifelse(dfMeadStorageStack$Year <= nMaxYearResData, dfMeadStorageStack$MexicoConserve/1e6, 0)
dfMeadStorageStack$PublicPool <- ifelse(dfMeadStorageStack$Year <= nMaxYearResData, dfMeadStorageStack$MeadStorage - dfMeadStorageStack$Protect - dfMeadStorageStack$LowerBasin - dfMeadStorageStack$Mexico, 0)

# If the public pool is less than zero, lower the protect volume
dfMeadStorageStack$Protect <- ifelse(dfMeadStorageStack$PublicPool < 0, dfMeadStorageStack$Protect + dfMeadStorageStack$PublicPool,dfMeadStorageStack$Protect )
# Set negative public pool values to 0
dfMeadStorageStack$PublicPool = ifelse(dfMeadStorageStack$PublicPool < 0, 0,dfMeadStorageStack$PublicPool )
# Set public pool values for year nMaxYearResData to zero
dfMeadStorageStack$PublicPool = ifelse(dfMeadStorageStack$Year >= nMaxYearResData, 0, dfMeadStorageStack$PublicPool)

#Calculate the Lake Mead pool level absent the water conservation program
dfMeadStorageStack$MeadLevelWithoutICS <- dfMeadStorageStack$MeadStorage - dfMeadStorageStack$LowerBasin - dfMeadStorageStack$Mexico
dfMeadStorageStack$MeadLevelWithoutICS <- dfMeadStorageStack$Storage / 1e6 - dfMeadStorageStack$LowerBasin - dfMeadStorageStack$Mexico


#Melt the data
dfMeadStorageStackMelt <- melt(dfMeadStorageStack, id.vars = c("DateAsValue"), measure.vars = c("Protect","PublicPool", "LowerBasin", "Mexico"))
#Specify the order of the variables
dfMeadStorageStackMelt$variable <- factor(dfMeadStorageStackMelt$variable, levels=c("Mexico", "LowerBasin", "PublicPool", "Protect"))

#Read in the levels from CSV
dfMeadPoolsPlot2 <- read.csv("dfMeadPoolsPlot2.csv",header=TRUE)
# Add SNWA Intake to the third label at Mead Elevation 1,000 ft
#dfMeadPoolsPlot2$label[3] <- paste0(dfMeadPoolsPlot2$label[3],"\nSNWA Intake #2")

#Get the color palettes
#Get the blue color bar
pBlues <- brewer.pal(9,"Blues")
pReds <- brewer.pal(9,"Reds")

ggplot() +
  #Lake Mead Storage and Water Conservation Account balances as stacked area plot
  #As area
  geom_area(data=dfMeadStorageStackMelt, aes(x=DateAsValue, y=value, fill=variable, group=variable)) +
  #Reservoir level as line
  geom_line(data=dfMeadStorageStack %>% filter(Year < nMaxYearResData + 1),aes(x=DateAsValue,y=MeadStorage, color="Combined"), size=2, color = "Black") +
  #Reservoir level without ICS program
  geom_line(data=dfMeadStorageStack %>% filter(Year <= nMaxYearICSData),aes(x=DateAsValue,y=MeadLevelWithoutICS, color="Combined"), size=1, color = "Black", linetype = "twodash") +
  
  #lines for max capacity and protect elevation
  geom_hline(data=dfKeyMeadVolumes, aes(yintercept = Volume), linetype="longdash", size=1) +
  #lines for Interim Guidelines and Expiry
  geom_vline(data=dfKeyDates, aes(xintercept = Date), linetype = "dashed", size=1, color = pReds[9]) +
  
  #Labels for the areas
#  geom_text(data=dfKeyMeadTraceLabels %>% filter(Label != dfKeyMeadTraceLabels$Label[3]), aes(x=as.Date(sprintf("%.0f-01-01",xPosition)), y=Volume, label=as.character(Label)), size = 6, fontface="bold") +
#  geom_text(data=dfKeyMeadTraceLabels %>% filter(Label == dfKeyMeadTraceLabels$Label[3]), aes(x=as.Date(sprintf("%.0f-01-01",xPosition)), y=Volume, label=as.character(Label)), size = 5, fontface="bold", color = pBlues[3]) +
  geom_text(data=dfKeyMeadTraceLabels, aes(x=as.Date(sprintf("%.0f-01-01",xPosition)), y=Volume, label=as.character(Label)), size = 6, fontface="bold") +
  
  #Scales
  scale_x_date(limits= c(as.Date("2000-01-01"), as.Date("2026-01-01")), date_breaks = "4 year", date_labels = "%Y", sec.axis = sec_axis(~. +0, name = "", breaks = dfKeyDates$Date, labels = as.character(dfKeyDates$Label))) +
  #Secondary axis as Mead level
  scale_y_continuous(limits = c(0, NA),  sec.axis = sec_axis(~. +0, name = "Elevation (feet)", breaks = dfMeadPoolsPlot2$stor_maf, labels = dfMeadPoolsPlot2$label)) +
  
  
  scale_fill_manual(values=c(pBlues[3], pBlues[3], pBlues[5], pBlues[7])) +
  
  #    scale_y_continuous(breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]),labels=c(0,5.98,9.6,12.2,dfMaxStor[2,2]),  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]), labels = c(895,1025,1075,1105,1218.8))) +
  #scale_x_discrete(breaks=cMonths, labels= cMonthsLabels) +
  #scale_x_continuous(breaks=seq(1960,2020,by=10), labels= seq(1960,2020,by=10)) +
  
  
  #scale_fill_manual(breaks=c(1:6),values = palBlues[2:7]) + #,labels = variable) + 
  theme_bw() +
  #coord_fixed() +
  labs(x="", y="Active Storage\n(MAF)", color = "") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.position ="none")
#theme(text = element_text(size=20), legend.text=element_text(size=16)
