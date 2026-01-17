#AutoReadUSBRData.r
#
# This script auto reads USBR Lake Powell and Lake Mead data from Reclamation's HydroData Web portal
# 
# https://www.usbr.gov/uc/water/hydrodata/reservoir_data/site_map.html

# This is a beginning R-programming effort! There could be lurking bugs or basic coding errors that I am not even aware of.
# Please report bugs/feedback to me (contact info below)
#
# We auto-read all Lake Powell (Code 919) and Lake Mead (921) available data at the daily time step.
#
# This data is:
#  Lake Powell (919) and field (code)
#     Storage - 17
#     Evaporation - 25
#     Inflow - 29
#     Inflow volume - 30
#     Unregulated inflow - 33
#     Unregulated inflow volume - 34
#     Total Release - 42
#     Release volume - 43
#     Area - 89
#     Pool elevation - 49
#  Lake Mead (921)
#     Storage - 17
#     Total Release - 42
#     Pool elevation - 49
#     Release volume - 43
#
#  The auto read URL command is:
#  'https://www.usbr.gov/uc/water/hydrodata/reservoir_data/RESID/csv/FIELDID.csv'

#
# The data wrangling strategy is:
#   1. Loop through Reservoirs and Fields
#   2.   Auto download the individual field CSV file format
#   2. Combine into one data frame for each reservoir with columns as fields
#   4. Clean and covert Reclamation's date format to a format R understands
#   5. Move into Narrow format - [Date][Reservoir][Field][Units][Value]
#   6. Calculate Water Year and sum daily values to annual and 10-year running sums.

# David E. Rosenberg
# January 16, 2026
# Utah State University
# david.rosenberg@usu.edu

rm(list = ls())  #Clear history

# Load required libraries in 1 go
# List of packages
load.lib <- c("tidyverse", "readxl", "RColorBrewer", "dplyr", "expss", "reshape2", "pracma", "lubridate", "directlabels", "plyr", "stringr", "ggplot2", "ggpubr", "ggrepel", "zoo")
# Then we select only the packages that aren't currently installed.
install.lib <- load.lib[!load.lib %in% installed.packages()]
# And finally we install the missing packages, including their dependency.
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
# After the installation process completes, we load all packages.
sapply(load.lib,require,character=TRUE)

# Read in meta data from Excel file

sExcelMeta <- 'USBRWebPortalMetaData.xlsx'

dfReservoirs <- read_excel(sExcelMeta, sheet = "Reservoirs")
dfFields <- read_excel(sExcelMeta, sheet = "Fields")


# Create a master data frame of reservoir data

dfResData <- data.frame(datetime = 0, Value = 0, FieldID = 0, ResID = 0)

# Loop over the reservoirs and fields

for (iRes in dfReservoirs$ResID) {
  for (iField in dfFields$FieldID) {
    
     # Create the url call
    sResFieldURL <- paste0('https://www.usbr.gov/uc/water/hydrodata/reservoir_data/', iRes, '/csv/', iField,'.csv')
 
    print(sResFieldURL)
    
    # Read in the data
    tryCatch({
      dfTemp <- read.csv(file=sResFieldURL, 
                         header=TRUE,
                         
                         stringsAsFactors=FALSE,
                         sep=",")
      #Add the Field ID and change the second column from the field name to value
      colnames(dfTemp)[2] <- "Value"
      
      dfTemp$FieldID <- iField
      dfTemp$ResID <- iRes
      
      
      dfResData <- rbind(dfResData,  dfTemp)
      
    },
    error = {print(paste("Did not read ", sResFieldURL ))
    })
  }
}

# Read in daily Lake Powell release from Reclamation's HydroPortal
# Details
# Lake Powell is Reservoir Code 919
# Lake Powell total release is Code 42
#
# Thus the download query to CSV format is https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/42.csv


# File name to read in Mead end of month reservoir level in feet - cross tabulated by year (1st column) and month (subsequent columns)
#    LAKE MEAD AT HOOVER DAM, END OF MONTH ELEVATION (FEET), Lower COlorado River Operations, U.S. Buruea of Reclamation
#    https://www.usbr.gov/lc/region/g4000/hourly/mead-elv.html

# Read in the historical Powell data
dfPowellHistorical <- read.csv(file=sPowellReleaseDataCode, 
                               header=TRUE, 
                               
                               stringsAsFactors=FALSE,
                               sep=",")


#Interpolate Powell storage from level to check
## For 2020 data

dfPowellHist <- dfPowellHistorical
dDateInterval <- 365 # Days

#Convert date text to date value
dfPowellHist$DateValue <- as.POSIXct(dfPowellHist$datetime)

# Convert CFS to Acre-feet per month
nCFStoAFMon <- 60.37
nCFStoAFDay <- nCFStoAFMon/30.5

nCFSToAF <- nCFStoAFDay

#Calculate Day, Month, and Year
dfPowellHist$Year <- year(dfPowellHist$DateValue)
dfPowellHist$Month <- month(dfPowellHist$DateValue)
dfPowellHist$Day <- day(dfPowellHist$DateValue)

#Calculate Water Year
dfPowellHist$WaterYear <- ifelse(dfPowellHist$Month >= 10, dfPowellHist$Year + 1, dfPowellHist$Year)

#Calculate Annual flow in million acre-feet
dfPowellAnnual <- dfPowellHist %>% select(WaterYear, total.release) %>% group_by(WaterYear) %>% dplyr::summarize(AnnualRelease = sum(total.release)*nCFSToAF/1e6)

#Trim first and last years with incomplete data
nFirstYear <- min(dfPowellAnnual$WaterYear) + 1
nLastYear <-  max(dfPowellAnnual$WaterYear)

dfPowellAnnual <- dfPowellAnnual %>% filter(WaterYear > nFirstYear, WaterYear < nLastYear)

#10-year total release
dfPowellAnnual$TenYearRelease <- rollapply(dfPowellAnnual$AnnualRelease, 10,sum, fill=NA, align="right")
#9-year total
dfPowellAnnual$NineYearRelease <- rollapply(dfPowellAnnual$AnnualRelease, 9,sum, fill=NA, align="right")

#7.48 and 8.23 MAF annual targets
dfPowellAnnual$OneYearTarget <- 7.48  # Paria flow (0.02 maf per year adds 0.2 maf over 10 years)
dfPowellAnnual$OneYearTarget82 <- dfPowellAnnual$OneYearTarget + 0.75
dfPowellAnnual$TenYearTarget <- dfPowellAnnual$OneYearTarget * 10  # Paria flow (0.02 maf per year adds 0.2 maf over 10 years)
dfPowellAnnual$TenYearTarget82 <- dfPowellAnnual$TenYearTarget + 10*0.75


# # Difference between 10-year and target
# dfPowellAnnual$Diff75 <- dfPowellAnnual$TenYearRelease - dfPowellAnnual$TenYearTarget
# dfPowellAnnual$Diff82 <- dfPowellAnnual$TenYearRelease - dfPowellAnnual$TenYearTarget82


# # Add text for the decade
# # 10-year values
# dfPowellHistAnnual$Decade <- paste0(dfPowellHistAnnual$Year - 10 + 1," to ",dfPowellHistAnnual$Year)
# dfPowellHistAnnual$TenYearReleaseRnd <- round(dfPowellHistAnnual$TenYearRelease, digits=1)
# dfPowellHistAnnual$TenYearDiffRnd <- round(dfPowellHistAnnual$Diff, digits=1)
# 
# # 9-year value
# dfPowellHistAnnual$NineYearPeriod <- paste0(dfPowellHistAnnual$Year - 9 + 1," to ",dfPowellHistAnnual$Year)
# dfPowellHistAnnual$NineYearReleaseRnd <- round(dfPowellHistAnnual$NineYearRelease, digits=1)
# dfPowellHistAnnual$NineYearDiffRnd <- round(dfPowellHistAnnual$NineYearRelease - 8.23*9, digits=1)
# 
# # Select into two columns and reverse sort
# dfPowellByDecade <- dfPowellHistAnnual %>% arrange(Year, decreasing = TRUE) %>% select(Decade, TenYearReleaseRnd,TenYearDiffRnd, NineYearRelease) 

#Export to CSV
write.csv(dfPowellAnnual,"dfPowellAnnual.csv" )

#Get the color palettes
#Get the blue color bar
pBlues <- brewer.pal(9,"Blues")
pReds <- brewer.pal(9,"Reds")

#### Figure 1. Annual Powell Release compared to 7.5 and 8.23 targets
#Pivot the Wide format to Longer for Plotting with different colors
dfPowellAnnualLong <- pivot_longer(data = dfPowellAnnual, cols = c(AnnualRelease, OneYearTarget, OneYearTarget82), names_to = "DataType", values_to = "Flow")
dfPowellAnnualLong$DataTypeFactor <- factor(dfPowellAnnualLong$DataType, levels = c("AnnualRelease", "OneYearTarget82", "OneYearTarget") )

ggplot(data = dfPowellAnnualLong %>% filter(WaterYear >= 1995), aes(x = WaterYear, y = Flow , color = DataTypeFactor, linetype = DataTypeFactor)) +
  #Powell release 
  geom_line(size=2) +

  scale_color_manual(labels = c("Annual Release", "8.23 Target", "7.48 Target"), values = c("AnnualRelease" = pBlues[8], "OneYearTarget82" =  pReds[7], "OneYearTarget" = pReds[4])) +
  scale_linetype_manual(labels = c("Annual Release", "8.23 Target", "7.48 Target"), values = c("AnnualRelease" = "solid", "OneYearTarget82" =  "dashed", "OneYearTarget" = "twodash")) +
  scale_x_continuous(breaks = seq(1970,2026,5)) +
  theme_bw() +
  #coord_fixed() +
  labs(x="", y="Powell Release\n(million acre-feet per year)") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18))
  #theme(text = element_text(size=20), legend.text=element_text(size=16)

ggsave("PowellAnnualRelease.png", width=9, height = 6.5, units="in")


#### Figure 2. Ten-Year Powell Release compared to 10-year targets

#Pivot the Wide format to Longer for Plotting with different colors
dfPowellTenYearLong <- pivot_longer(data = dfPowellAnnual, cols = c(TenYearRelease, TenYearTarget, TenYearTarget82), names_to = "DataType", values_to = "Flow")
dfPowellTenYearLong$DataTypeFactor <- factor(dfPowellTenYearLong$DataType, levels = c("TenYearRelease", "TenYearTarget82", "TenYearTarget") )

ggplot(data = dfPowellTenYearLong %>% filter(WaterYear >= 1995), aes(x = WaterYear, y = Flow , color = DataTypeFactor, linetype = DataTypeFactor)) +
  #Powell release 
  geom_line(size=2) +
  
  scale_color_manual(labels = c("10-Year Release", "82.3 Target", "74.8 Target"), values = c("TenYearRelease" = pBlues[8], "TenYearTarget82" =  pReds[7], "TenYearTarget" = pReds[4])) +
  scale_linetype_manual(labels = c("10-Year Release", "82.3 Target", "74.8 Target"), values = c("TenYearRelease" = "solid", "TenYearTarget82" =  "dashed", "TenYearTarget" = "twodash")) +
  scale_x_continuous(breaks = seq(1970,2026,5)) +
  theme_bw() +
  #coord_fixed() +
  labs(x="", y="Powell Release\n(million acre-feet per year)") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18))
#theme(text = element_text(size=20), legend.text=element_text(size=16)

ggsave("PowellTenYearRelease.png", width=9, height = 6.5, units="in")







#### 10-Year Difference between Release and 82.3 target

ggplot() +
  #  10-year sum
  geom_line(data=dfPowellHistAnnual,aes(x=DateAsValue,y=TenYearDiffRnd, color="10-Year"), size=2) +

  geom_line(data=dfPowellHistAnnual,aes(x=DateAsValue,y=NineYearDiffRnd, color="9-Year"), size=2) +
  
  theme_bw() +
  #coord_fixed() +
  labs(x="", y="Powell Release above Requirement\n(million acre-feet)") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18))
#theme(text = element_text(size=20), legend.text=element_text(size=16)

ggsave("PowellReleaseTargetDifference.png", width=9, height = 6.5, units="in")

# Report the most recent 9-year release, target, and difference 
sprintf("Nine year Lake Powell release (%s) of %s maf is %s maf above 74.1 maf requirement",dfPowellHistAnnual[nrow(dfPowellHistAnnual), "NineYearPeriod"], round(dfPowellHistAnnual[nrow(dfPowellHistAnnual), "NineYearRelease"],1), dfPowellHistAnnual[nrow(dfPowellHistAnnual), "NineYearDiffRnd"])

### Powell Annual Inflow

#### Powell Release over time - annual

# As double line plot
# ggplot() +
#   # Powell Annual Inflow
#   geom_line(data=dfPowellHistAnnual,aes(x=DateAsValue,y=OneYearInflow, color="Inflow", linetype="Inflow"), size=2) +
#   geom_line(data=dfPowellHistAnnual,aes(x=DateAsValue,y=OneYearRelease, color="Release", linetype="Release"), size=2) +
#   #geom_bar(data=dfPowellHistAnnual,aes(x=DateAsValue,y=OneYearRelease, color="1-Year Total")) +
#   
#   scale_color_manual(values=c("red","blue"), guide="legend") +
#   scale_linetype_manual(values=c("solid", "dashed")) +
#   guides(color=guide_legend(""), linetype=guide_legend("")) +
#   theme_bw() +
#   #coord_fixed() +
#   labs(x="", y="Water Volume\n(million acre-feet per year)") +
#   theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18))
#theme(text = element_text(size=20), legend.text=element_text(size=16)

# As mixed bar (inflow) line (release) plot

ggplot(dfPowellHistAnnual, aes(DateAsValue)) +
  geom_bar(aes(y=OneYearInflow, fill = "Inflow"), stat="identity") +
  geom_line(aes(y=OneYearRelease, group = 1, color="Release"), size=2) +
  scale_color_manual(" ", values = c("Inflow" = "grey50", "Release" = "blue")) +
  scale_fill_manual("", values="grey50") +
  labs(x="", y="Water Volume\n(million acre-feet per year)") +  
  
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank())

ggsave("PowellInflow.png", width=9, height = 6.5, units="in")

#Compare Powell USBR data evaporation to values from rate

ggplot(dfPowellHistAnnual, aes(DateAsValue)) +
  geom_bar(aes(y=OneYearEvap, fill = "Powell - USBR data"), stat="identity") +
  geom_line(aes(y=EvaporationAnnual, group = 1, color="Powell - Rate Annual"), size=2) +
  geom_line(aes(y=ByMonthEvap, group = 1, color="Powell - Rate Monthly"), size=2) +
  scale_color_manual(" ", values = c("Powell - USBR data" = "grey50", "Powell - Rate Annual" = "blue", "Powell - Rate Monthly" = "red")) +
  scale_fill_manual("", values="grey50") +
  labs(x="", y="Evaporation\n(million acre-feet per year)") +  
  
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank())

ggsave("PowellEvap.png", width=9, height = 6.5, units="in")

  
#Histogram -- frequency of annual inflow volumes
ggplot(dfPowellHistAnnual, aes(x=OneYearInflow)) +
  geom_histogram(color="darkmagenta", fill="magenta", binwidth = 2) +

  scale_x_continuous(limits = c(2,22), breaks = seq(2,22,by=2)) +
  scale_y_continuous(breaks = seq(0,11,by=2)) +
  
  labs(x="Powell Inflow\n(million acre feet per year)", y="Frequency\n(number of years)") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
      legend.key = element_blank())

ggsave("PowellInflowHistogram.png", width=9, height = 6.5, units="in")

#Histogram -- frequency of annual release volumes
ggplot(dfPowellHistAnnual, aes(x=OneYearRelease)) +
  geom_histogram(color="darkmagenta", fill="magenta", binwidth = 1) +
  
  scale_x_continuous(limits = c(2,22), breaks = seq(2,22,by=1)) +
  #scale_y_continuous(breaks = seq(0,11,by=1)) +
  
  labs(x="Powell Release\n(million acre feet per year)", y="Frequency\n(number of years)") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank())

ggsave("PowellInflowHistogram.png", width=9, height = 6.5, units="in")

print(paste0("Number of Years of Powell releases = ", max(dfPowellHistAnnual$Year) - min(dfPowellHistAnnual$Year) + 1))

# Sort the Powell releases by value and year to faciliate easy identify of years
dfPowellHistAnnualSort <- dfPowellHistAnnual[order(dfPowellHistAnnual$OneYearRelease,dfPowellHistAnnual$Year), c("OneYearRelease","Year")]
