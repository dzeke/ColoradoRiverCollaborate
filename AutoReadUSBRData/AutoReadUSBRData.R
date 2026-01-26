# AutoReadUSBRData.r
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

###### Define a function that Reads in all the available reservoir data from Reclamations HydroData webportal.
# https://www.usbr.gov/uc/water/hydrodata/reservoir_data/site_map.html

fReadReclamationHydroData <- function(FromHydroData) {
    
    # IF FromHydroData is TRUE, then read from the hydroportal. Else just load the most recent CSV files
  
    if(FromHydroData != TRUE ) {
      dfResDataAnnual <- read.csv(file = "dfResDataAnnual.csv", header=TRUE,  sep=",")
      dfResDataMonthly <- read.csv(file = "dfResDataMonthly.csv", header=TRUE,  sep=",")
      dfResDataDaily <- read.csv(file = "dfResDataDaily.csv", header=TRUE,  sep=",")
      
      # return(list(first_df = dfResDataDaily, second_df = dfResDataMonthly, third_df = dfResDataAnnual))
      return(list(dfResDaily = dfResDataDaily, dfResMonthly = dfResDataMonthly, dfResAnnual = dfResDataAnnual))
      
    }

    #We will read from the HydroDataPortal
    sExcelMeta <- 'USBRWebPortalMetaData.xlsx'
    
    dfReservoirs <- read_excel(sExcelMeta, sheet = "Reservoirs")
    dfFields <- read_excel(sExcelMeta, sheet = "Fields")
    
    
    # Create a master data frame of reservoir data
    
    dfResData <- data.frame(datetime = 0, Value = 0, FieldID = 0, ResID = 0)
    
    # Loop over the reservoirs and fields
    
    for (iRes in dfReservoirs$ResID) {
    #for (iRes in 921) {  
      for (iField in dfFields$FieldID) {
        
         # Create the url call
        sResFieldURL <- paste0('https://www.usbr.gov/uc/water/hydrodata/reservoir_data/', iRes, '/csv/', iField,'.csv')

        # Log the reservoir name and field for use in error checking.
        dfResNameTemp <- dfReservoirs %>% filter(ResID == iRes) %>% select(ResName)
        dfFieldNameTemp <- dfFields %>% filter(FieldID == iField) %>% select(FieldName)
        
        # print(sResFieldURL)
        
        dfTemp <- try(read.csv(file=sResFieldURL, 
                               header=TRUE,
                               
                               stringsAsFactors=FALSE,
                               sep=","))
        if (!inherits(dfTemp, "try-error")) {
          # Proceed with data processing if no error occurred
          
          print(paste("Successfully read -", dfResNameTemp$ResName[1],":", dfFieldNameTemp$FieldName[1], " - ", sResFieldURL))
          #print(paste("Successfully read - ", sResFieldURL))
                    # Change the second column from the field name to value
          colnames(dfTemp)[2] <- "Value"
          # Add reservoir and field codes as new fields
          dfTemp$FieldID <- iField
          dfTemp$ResID <- iRes
          #
          #
           dfResData <- rbind(dfResData,  dfTemp)
        } else {
          print(paste("Skipping due to error -", dfResNameTemp$ResName[1],":", dfFieldNameTemp$FieldName[1], " - ", sResFieldURL))
          
          #print(paste("Skipping", sResFieldURL, "due to an error"))
        }
      }
    }
    
    # Join in field and reservoir names and associated data
    dfResData <- left_join(dfResData, dfReservoirs, by = "ResID")
    dfResData <- left_join(dfResData, dfFields, by = "FieldID")
    
    # Remove the first row
    dfResData <- dfResData %>% slice(-1)
    
    dDateInterval <- 365 # Days
    
    #Convert date text to date value
    # dfResData$DateValue <- as.POSIXct(dfResData$datetime)
    dfResData$DateValue <- ymd(dfResData$datetime)
    
    # Convert CFS to Acre-feet per month
    nCFStoAFMon <- 60.37
    nCFStoAFDay <- nCFStoAFMon/30.5
    
    nCFSToAF <- nCFStoAFDay
    
    # Calculate Day, Month, and Year
    dfResData$Year <- year(dfResData$DateValue)
    dfResData$Month <- month(dfResData$DateValue)
    dfResData$Day <- day(dfResData$DateValue)
    
    # Calculate Water Year
    dfResData$WaterYear <- ifelse(dfResData$Month >= 10, dfResData$Year + 1, dfResData$Year)
    
    # Save the Daily data frame
    dfResDataDaily <- dfResData
    
    dfTemp <- dfResData %>% filter(Year == 2000, Month == 10, Day ==1)
    
    ### Aggregate and filter to Monthly values
    # Aggregate To Monthly values for fields with AggregateToTimePeriod == Yes
    dfFieldsAggByTimePeriod <- dfFields %>% filter(AggregateByTimePeriod == "Yes")
    
    # For fields with AggregateToTimePeroid == Yes, we aggregate
    dfTempAgg <- dfResData %>% filter(AggregateByTimePeriod == "Yes") %>% 
                              select(WaterYear, Month, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits, Value) %>%
                               group_by(WaterYear, Month, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits) %>%
                                dplyr::summarize(MonthlyValue = sum(Value))           
    
    # For fields with AggregateToTimePeriod == No, we select the first day of the month
    dfTempDay1 <- dfResData %>% filter(AggregateByTimePeriod == "No", Day == 1) %>%
                                select(WaterYear, Month, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits, Value) %>%
                                dplyr::rename(MonthlyValue = Value)
    
    # Merge back into a single Monthly data frame
    dfResDataMonthly <- rbind(dfTempAgg, dfTempDay1)
    
    ### Convert acre-feet fields to Million Acre-Feet
    dfResDataMonthly$MonthlyValue <- ifelse(dfResDataMonthly$FieldUnits == "acre-feet", dfResDataMonthly$MonthlyValue/ 1e6, dfResDataMonthly$MonthlyValue )
    dfResDataMonthly$FieldUnits <- ifelse(dfResDataMonthly$FieldUnits == "acre-feet", "million acre-feet", dfResDataMonthly$FieldUnits )
    
    
    ### Aggregate and filter to Water Year values
    # Aggregate To WaterYear values for fields with AggregateToTimePeriod == Yes
    
    dfTempAgg <- dfResData %>% filter(AggregateByTimePeriod == "Yes") %>% 
      select(WaterYear, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits, Value) %>%
      group_by(WaterYear, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits) %>%
      dplyr::summarize(AnnualValue= sum(Value))           
    
    # For fields with AggregateToTimePeriod == No, we select the first day of the Water Year (October 1)
    dfTempDay1 <- dfResData %>% filter(AggregateByTimePeriod == "No", Month == 12, Day == 1) %>%
      select(WaterYear, ResID, ResName, ResNameAbbrev, FieldID, FieldName, FieldUnits, Value) %>%
      dplyr::rename(AnnualValue = Value)
    
    # Merge back into a single data frame
    dfResDataAnnual <- rbind(dfTempAgg, dfTempDay1)
    
    ### Convert acre-feet fields to Million Acre-Feet
    dfResDataAnnual$AnnualValue <- ifelse(dfResDataAnnual$FieldUnits == "acre-feet", dfResDataAnnual$AnnualValue/ 1e6, dfResDataAnnual$AnnualValue )
    dfResDataAnnual$FieldUnits <- ifelse(dfResDataAnnual$FieldUnits == "acre-feet", "million acre-feet", dfResDataAnnual$FieldUnits )
    
    #Trim first and last years with incomplete data
    nFirstYear <- min(dfResDataAnnual$WaterYear) + 1
    nLastYear <-  max(dfResDataAnnual$WaterYear)
    
    dfResDataAnnual <- dfResDataAnnual %>% filter(WaterYear > nFirstYear, WaterYear < nLastYear)
    ###########
    
    # Write the data frames to csv
    write.csv(dfResDataAnnual, "dfResDataAnnual.csv")
    write.csv(dfResDataMonthly, "dfResDataMonthly.csv")
    write.csv(dfResDataDaily, "dfResDataDaily.csv")
    
    return(list(dfResDaily = dfResDataDaily, dfResMonthly = dfResDataMonthly, dfResAnnual = dfResDataAnnual))
}


lResData <- fReadReclamationHydroData(FromHydroData = FALSE)

# Let's try plotting the annuall Lake Powell Release
dfResDataAnnual <- lResData$dfResAnnual

#Filter the Powell Release Volume
dfPowellAnnual <- dfResDataAnnual %>% filter(ResName == "Lake Powell",FieldName == "Release volume")

#10-year total release
dfPowellAnnual$TenYearRelease <- rollapply(dfPowellAnnual$AnnualRelease, 10,sum, fill=NA, align="right")


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