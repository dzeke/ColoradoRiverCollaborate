# Powell10Year-AutoImport.r
#
# Plot 10-year running total release from Lake Powell
#
# This is a beginning R-programming effort! There could be lurking bugs or basic coding errors that I am not even aware of.
# Please report bugs/feedback to me (contact info below)
#
# Auto download daily data from Reclamation's HydroData portal - https://www.usbr.gov/uc/water/hydrodata/reservoir_data/site_map.html
#
# The data wrangling strategy is:
#   1. Auto download the release data from Reclamation's HydroData portal into CSV file format
#   2. Clean and covert Reclamation's date format to a format R understands
#   3. Calculate Water Year and sum daily values to annual and 10-year running sums.
#   4. Plot stuff.

# David E. Rosenberg
# January 13, 2026
# Utah State University
# david.rosenberg@usu.edu

rm(list = ls())  #Clear history

# Load required libraries in 1 go
# List of packages
load.lib <- c("tidyverse", "readxl", "RColorBrewer", "dplyr", "expss", "reshape2", "pracma", "lubridate", "directlabels", "plyr", "stringr", "ggplot2", "ggpubr", "ggrepel", "zoo", "here")
# Then we select only the packages that aren't currently installed.
install.lib <- load.lib[!load.lib %in% installed.packages()]
# And finally we install the missing packages, including their dependency.
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
# After the installation process completes, we load all packages.
sapply(load.lib,require,character=TRUE)

here::i_am("LakePowell10year/AsR/Powell10Year-AutoImportFromFunc.R")
        
## Read in functions to:
#     Auto load USBR data
#     Interpolate with NAs
#     Load Reservoir Bathymetry and Critical Elevations
source("../../AutoReadUSBRData/AutoReadUSBRData.r")

# Read in the Reclamation Hydro Data
#lResData <- fReadReclamationHydroData(FromHydroData = TRUE)
lResData <- fReadReclamationHydroData(FromHydroData = FALSE)



# Read in the Reservoir Bathymetry and Critical Elevations
dfTemp <- ReadBathymetryCritialElevations()


# Let's try plotting the annuall Lake Powell Release
dfResDataAnnual <- lResData$dfResAnnual

#Filter the Powell Release Volume
dfPowellAnnual <- dfResDataAnnual %>% filter(ResName == "Lake Powell",FieldName == "Release volume")

#10-year total release
dfPowellAnnual$TenYearRelease <- rollapply(dfPowellAnnual$AnnualValue, 10,sum, fill=NA, align="right")


#7.48 and 8.23 MAF annual targets
dfPowellAnnual$OneYearTarget <- 7.48  # Paria flow (0.02 maf per year adds 0.2 maf over 10 years)
dfPowellAnnual$OneYearTarget82 <- dfPowellAnnual$OneYearTarget + 0.75
dfPowellAnnual$TenYearTarget <- dfPowellAnnual$OneYearTarget * 10  # Paria flow (0.02 maf per year adds 0.2 maf over 10 years)
dfPowellAnnual$TenYearTarget82 <- dfPowellAnnual$TenYearTarget + 10*0.75



#Get the blue color bar
pBlues <- brewer.pal(9,"Blues")
pReds <- brewer.pal(9,"Reds")

#### Figure 1. Annual Powell Release compared to 7.5 and 8.23 targets
#Pivot the Wide format to Longer for Plotting with different colors
dfPowellAnnualLong <- pivot_longer(data = dfPowellAnnual, cols = c(AnnualValue, OneYearTarget, OneYearTarget82), names_to = "DataType", values_to = "Flow")
dfPowellAnnualLong$DataTypeFactor <- factor(dfPowellAnnualLong$DataType, levels = c("AnnualValue", "OneYearTarget82", "OneYearTarget") )

ggplot(data = dfPowellAnnualLong %>% filter(WaterYear >= 1995), aes(x = WaterYear, y = Flow , color = DataTypeFactor, linetype = DataTypeFactor)) +
  #Powell release 
  geom_line(size=2) +
  
  scale_color_manual(labels = c("Annual Release", "8.23 Target", "7.48 Target"), values = c("AnnualValue" = pBlues[8], "OneYearTarget82" =  pReds[7], "OneYearTarget" = pReds[4])) +
  scale_linetype_manual(labels = c("Annual Release", "8.23 Target", "7.48 Target"), values = c("AnnualValue" = "solid", "OneYearTarget82" =  "dashed", "OneYearTarget" = "twodash")) +
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

#Export to CSV
write.csv(dfPowellAnnual,"dfPowellAnnual.csv" )

#### Spit out the most recent Lake Powell and Lake Mead Pool elevations
dfResDataDaily <- lResData$dfResDaily

#filter to values for yesterday 
dYesterday <- today() - 1
dfResElevations <- dfResDataDaily %>% filter(FieldName == "Pool Elevation", DateValue == dYesterday)

print(dfResElevations)


################
#  Figure 3. Powell, Mead, and Combined storage plot
#

# Combined, Powell, and Mead on one plot

# Filter the Powell and Mead monthly storages

dfResDataMonthly <- lResData$dfResMonthly

#Filter the Powell and Mead storages
dfResStorage <- dfResDataMonthly %>% filter(ResName %in% c("Lake Powell", "Lake Mead"), FieldName == "Storage")
# Convert year and month 
# Calculate calendar year
dfResStorage$Year <- ifelse(dfResStorage$Month >= 10, dfResStorage$WaterYear - 1, dfResStorage$WaterYear)
#Calculate Date as Date Type
dfResStorage$Date <- as.Date(paste(dfResStorage$Year,"-",dfResStorage$Month, "-01", sep = ""), format = "%Y-%m-%d")

#Turn narrow into wide so separate columns for Lake Powell and Lake Mead
dfResStorageWide <- pivot_wider(  dfResStorage %>% group_by(ResName, Date) %>% select(ResName, Date, MonthlyValue),   names_from = ResName,   values_from = MonthlyValue)

ggplot() +
  #Powell storage
  geom_line(data=dfResStorageWide ,aes(x=Date, y=`Lake Powell`, color="Powell"), size=2) +
  #Mead Storage
  geom_line(data=dfResStorageWide ,aes(x=Date,y=`Lake Mead`, color="Mead"), size=2) +
  #Combined Storage
  geom_line(data=dfResStorageWide,aes(x=Date,y=`Lake Powell` + `Lake Mead`, color="Combined"), size=2) +
  scale_color_manual(values = c("purple","red","blue"), breaks=c("Combined", "Powell", "Mead")) +
  #geom_area(data=dfPlotData,aes(x=month,y=stor_maf, fill = variable), position='stack') +
  scale_y_continuous(breaks = seq(0,50,by=10),labels=seq(0,50,by=10)) +
  scale_x_date(limits= c(as.Date("1968-01-01"), as.Date("2030-01-01")),
               date_breaks = "10 years", # Major ticks every 10 years
               date_labels = "%Y") +
  
  
  #    scale_y_continuous(breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]),labels=c(0,5.98,9.6,12.2,dfMaxStor[2,2]),  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]), labels = c(895,1025,1075,1105,1218.8))) +
  #scale_x_discrete(breaks=cMonths, labels= cMonthsLabels) +
  #scale_x_continuous(breaks=seq(1960,2020,by=10), labels= seq(1960,2020,by=10)) +
  
  
  #scale_fill_manual(breaks=c(1:6),values = palBlues[2:7]) + #,labels = variable) + 
  theme_bw() +
  #coord_fixed() +
  labs(x="", y="Active Storage (MAF)", color = "Reservoir") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18))
#theme(text = element_text(size=20), legend.text=element_text(size=16)

