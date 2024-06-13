####################
#     Lake Mead Inflow
#
#     Estimate Lake Mead inflow using 4 methods:
#
#       1. Add U.S. Geological Service data from stream gages 
#
#             A. Colorado River nr Peach Springs [9404200; https://waterdata.usgs.gov/monitoring-location/09404200/#parameterCode=00065&timeSeriesId=6324&period=P7D] (1990 to present)
#             B. Virgin River at Littlefield [9415000; https://waterdata.usgs.gov/monitoring-location/09415000/#parameterCode=00065&period=P7D] (1930 to present)
#             C. Las Vegas Wash Below LAKE LAS VEGAS NR BOULDER CITY, NV [09419800; https://waterdata.usgs.gov/monitoring-location/09419800/] (2002 to present)
#
#             Mead Inflow = A + B + C
#
#             All data in USGSInterveningFlowData.xlsx
#
#       
#       2. Back calculate from Lake Mead storage, release, Nevada Diversion, and Lake Mead evaporation (2004 to present)
#
#             Use the HDB Data Service (usbr.gov) for all values - https://www.usbr.gov/lc/region/g4000/riverops/_HdbWebQuery.html
#
#                 API query - https://www.usbr.gov/pn-bin/hdb/hdb.pl?svr=lchdb&sdi=1776%2C2091%2C1721%2C1874&tstp=DY&t1=1990-01-01T00:00&t2=2023-08-28T00:00&table=R&mrid=0&format=csv
#
#                 In order to use this, you will need to know the region and Site Datatype ID (SDID). 
#                 The lake Mead data will be with the Lower Colorado Regional Offices HDB. For the different values you mentioned,
#                 the SDID's you will need are as follows: Evaporation (SDID=1776), Inflow (SDID=2091), Storage (SDID=1721), 
#                 and Release (SDID=1874). From there you can select the timestep you want,
#                  Instantaneous, Hourly, Daily, Monthly, as well as for what time span you want.
#
#                 as USBR-API-MeadData.json and USBR-API-MeadData.csv
#
#                 Lake Mead Inflow = [Change in Storage] + [Release] + [Nevada Diversion] + [Evaporation]
#
#       3. Back calculate from Lake Mead storage, release, Nevada Diversion, and Lake Mead evaporation (2004 to present)
#
#            Use the HDB Data Service (usbr.gov) for Storage, Release, and Nevada Diversion.
#
#                 API query same as for method 2.
#
#            Estimate evaporation from USBR storage volume and evaporation-storage relationship developed in prior work (see folder EvapCalcs)
#
#             More specifically, use the csv file dfMeadEvap.csv in EvapData subfolder. This dataframe is expored by the code in ../EvapCalcs folder.
#
#             Explanation of contents of dfMeadEvap.csv:
#               A. Elevation - Lake Mead elevation in feet
#               B. Live storage - storage volume above minimum power pool in acre-feet
#               C. Total storage - storage volume at minimum power pool + Live storage in acre-feet
#               D. Area - area of reservoir pool at live storage volume in acres
#               E. EvapVolMax - evaporated volume acre-feet estimated from mid point of evaporation rate
#               F. EvapVolMaxUp - evaporated volume in acre-feet estimated from upper bound of evaporation rate
#               G. EvapVolMaxLo - evaporated volume in acre-feet estimated from lower bound of evaporation rate
#
#             Note: E, F, and G represent a range of evaporated volumes estimated from a range of evaporation rates.

#
#       4. Lake Mead.Inflow slot from Colorado River Simulation System (CRSS) historical trace (1907 to present)
#
#             A. Read from SingleTraceOut.xlsx
#
#      
#
#       4. Wang / Schmidt - White Paper #5 [https://qcnr.usu.edu/coloradoriver/news/wp5] (2015 to 2020)
#
#             A. Supplementary_file-WangSchmidt.xlsx => Tables => S18:W18
#         
#             IGNORE because these values pull from the same gage data as #1 with different definitions of the year (March to February)

#
#     This code file modifies and adds to a prior coding effort -- Grand Canyon Intervening flow available at https://github.com/dzeke/ColoradoRiverCoding/tree/main/GrandCanyonInterveningFlow
#
#    
#
#     David E. Rosenberg
#     May 10, 2021
#     Updated August 18, 2023 to calculate Inflow to Lake Mead
#     david.rosenberg@usu.edu

#
#####


rm(list = ls())  #Clear history

#Load packages in one go
  #List of packages
  load.lib <- c("tidyverse", "readxl", "RColorBrewer", "dplyr", "expss", "reshape2", "pracma", "lubridate", "directlabels", "plyr", "stringr", "ggplot2", "ggpubr")
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
  
#Labels for each method to use in grouping and plotting
cMethods <- c("USGS Gages", "USBR Application Program Interface", "USBR with Evap from Table", "CRSS", "Wang-Schmidt")
cColors <- c("Blue", "Red", "Pink", "Purple", "Black")

# the Combined data frame will have the variables WaterYear, MeadInflow, Method


### Read in the Natural Flow data and convert it to annual flows
# Note used in calc of Mead Inflow. But keep anyway for backward compatibility
sExcelFileGrandCanyonFlow <- 'HistoricalNaturalFlow.xlsx'
dfGCFlows <- read_excel(sExcelFileGrandCanyonFlow, sheet = 'Total Natural Flow',  range = "U1:Z1324")
dfGCDates <- read_excel(sExcelFileGrandCanyonFlow, sheet = 'Total Natural Flow',  range = "A1:A1324")

#Merge and combine into one Data frame
dfGCFlows$Date <- dfGCDates$`Natural Flow And Salt Calc model Object.Slot`

#Calculate Grand Canyon Tributary flows as sum of Paria, Little Colorado River, Virgin, and intervening flows
#Just tribs (without intervening)
#dfGCFlows$Total <- dfGCFlows$`CoRivPowellToVirgin:PariaGains.LocalInflow` + dfGCFlows$`CoRivPowellToVirgin:LittleCoR.LocalInflow` + 
#                          dfGCFlows$VirginRiver.Inflow

#Tribs + Gains above Hoover
dfGCFlows$Total <- dfGCFlows$`CoRivPowellToVirgin:PariaGains.LocalInflow` + dfGCFlows$`CoRivPowellToVirgin:LittleCoR.LocalInflow` + 
  dfGCFlows$VirginRiver.Inflow + dfGCFlows$`CoRivVirginToMead:GainsAboveHoover.LocalInflow` - dfGCFlows$`CoRivPowellToVirgin:GainsAboveGC.LocalInflow`

dfGCFlows$Year <- year(dfGCFlows$Date)
dfGCFlows$Month <- month(as.Date(dfGCFlows$Date,"%Y-%m-%d"))
dfGCFlows$WaterYear <- ifelse(dfGCFlows$Month >= 10,dfGCFlows$Year,dfGCFlows$Year + 1)


#Convert to Water Year and sum by water year
dfGCFlowsByYear <- aggregate(dfGCFlows$Total, by=list(Category=dfGCFlows$WaterYear), FUN=sum)
dfLeeFerryByYear <- aggregate(dfGCFlows$`HistoricalNaturalFlow.AboveLeesFerry`, by=list(Category=dfGCFlows$WaterYear), FUN=sum)

#Change the Names
colnames(dfGCFlowsByYear) <- c("WaterYear","GCFlow")
colnames(dfLeeFerryByYear) <- c("WaterYear", "LeeFerryNaturalFlow")
dfGCFlowsByYear$LeeFerryNaturalFlow <- dfLeeFerryByYear$LeeFerryNaturalFlow

#Calculate Lake Mead Inflow as sum of GCFlow and Lee Ferry Natural Flow
dfGCFlowsByYear$MeadInflowNat <- dfGCFlowsByYear$GCFlow + dfGCFlowsByYear$LeeFerryNaturalFlow



##############################
### Inflow Calc Method #1. Add U.S. Geological Service data from stream gages
# Read in the USGS gaged data

sExcelFileUSGSFlow <- 'USGSInterveningFlowData.xlsx'
dfGCFlowsUSGS <- read_excel(sExcelFileUSGSFlow, sheet = 'Combined',  range = "A1:E34")
cColNames <- colnames(dfGCFlowsUSGS)
cColNames[1] <- "WaterYear"
cColNames[2] <- "LeeFerryFlow"
cColNames[5] <- "LasVegasWash"
colnames(dfGCFlowsUSGS) <- cColNames

#Remove rows with NaN
#dfGCFlowsUSGS <- na.omit(dfGCFlowsUSGS)

# Replace NAs with zeros
# Note calc less than 2002 assumes Las Vegas wash is zero 
dfGCFlowsUSGS <- dfGCFlowsUSGS %>% replace(is.na(.),0)

#Calculate the total
#Grand Canyon interveening flow
dfGCFlowsUSGS$GCFlow <- dfGCFlowsUSGS$`Colorado River near Peach Springs` - dfGCFlowsUSGS$LeeFerryFlow + dfGCFlowsUSGS$`Virgin River at Littlefield`
#Lake Mead inflow
dfGCFlowsUSGS$MeadInflow <- dfGCFlowsUSGS$`Colorado River near Peach Springs` + dfGCFlowsUSGS$`Virgin River at Littlefield` + dfGCFlowsUSGS$LasVegasWash
dfGCFlowsUSGS$Method <- cMethods[1]



#################################
#       Inflow Calc Method #2. Back calculate from Lake Mead storage, release, Nevada Diversion, and Lake Mead evaporation (2004 to present)
#
#             A. HDB Data Service (usbr.gov) - https://www.usbr.gov/lc/region/g4000/riverops/_HdbWebQuery.html
#
#                 API query - https://www.usbr.gov/pn-bin/hdb/hdb.pl?svr=lchdb&sdi=1776%2C2091%2C1721%2C1874&tstp=DY&t1=1990-01-01T00:00&t2=2023-08-28T00:00&table=R&mrid=0&format=csv
#
#                 In order to use this, you will need to know the region and Site Datatype ID (SDID). 
#                 The lake Mead data will be with the Lower Colorado Regional Offices HDB. For the different values you mentioned,
#                 the SDID's you will need are as follows: Evaporation (SDID=1776), Inflow (SDID=2091), Storage (SDID=1721), 
#                 and Release (SDID=1874). From there you can select the timestep you want,
#                  Instantaneous, Hourly, Daily, Monthly, as well as for what time span you want.
#
#                 as USBR-API-MeadData.json and USBR-API-MeadData.csv
#
#                 Note: this method ignores Southern Nevada Water Authority diversions and return flow. This net depletion
#                       is on the order of 0.1 to 0.2 million acre-feet per year.
#
#                 Lake Mead Inflow = [Change in Storage] + [Release] + [Nevada Diversion] + [Evaporation]


sExcelFileUSBRAPI <- "USBR-API-MeadData.csv"
dfUSBR_API<- read_csv(sExcelFileUSBRAPI, skip = 6) 

#Turn the SDID Code # into meaningful variable names
dfSDIDcode <- data.frame(code = c(1776, 2091, 1721, 1874),
                         Field = c("Evaporation", "Inflow", "Storage", "Release"),
                         Units = c("acre-feet", "??", "acre-feet", "cfs"))

cSDID <- colnames(dfUSBR_API)
cSDID[2:5] <-dfSDIDcode$Field
colnames(dfUSBR_API) <- cSDID

#Inflow is NaN for all time. So ignore the inflow variable
dfUSBR_API <- dfUSBR_API[, c(1:2,4:5)]

#Convert DATETIME to time series format R understands
dfUSBR_API$Date <- as.Date(dfUSBR_API$DATETIME, "%m/%d/%Y %h:%m")
dfUSBR_API$Date <- mdy_hm(dfUSBR_API$DATETIME)

#Add month, year, and day variables
dfUSBR_API$Month <- month(dfUSBR_API$Date)
dfUSBR_API$Year <- year(dfUSBR_API$Date)
dfUSBR_API$Day <- day(dfUSBR_API$Date)

#Convert cfs per day to million acre-feet per day
dfUSBR_API$Release <- dfUSBR_API$Release * 1.983 / 1e6
#Convert acre-feet to million acre-feet
dfUSBR_API$Evaporation <- dfUSBR_API$Evaporation / 1e6
dfUSBR_API$Storage <- dfUSBR_API$Storage / 1e6

#Calculate water year
dfUSBR_API$WaterYear <- ifelse(dfUSBR_API$Month >= 10, dfUSBR_API$Year + 1, dfUSBR_API$Year)

#Filter out rows with NaN - Evaporation at early months and years
dfUSBR_API2 <- na.omit(dfUSBR_API) # We will come back and fill in the evaporation using an evap rate and the storage-area curve 

#Filter out years < 2004 because some months have NAs for Evap
dfUSBR_API2 <- dfUSBR_API2 %>% filter(WaterYear > 2004)

#Pick the first day of October as day to take storage for the year
dfUSBR_Stor <- dfUSBR_API2 %>% select(WaterYear, Month, Day, Storage ) %>% filter(Month == 10, Day == 1)

#Calculate the difference
dfUSBR_Stor$DeltaStorage <- c(diff(dfUSBR_Stor$Storage),0)
#Aggregate to Month and Year
dfUSBR_API_Agg <- dfUSBR_API2 %>% dplyr::group_by(WaterYear) %>% dplyr::summarise(Evaporation = sum(Evaporation), Release = sum(Release))

#Aggregate to Month and Year
#dfUSBR_API_Agg <- dfUSBR_API2 %>% dplyr::group_by(WaterYear, Month) %>% dplyr::summarise(Evaporation = sum(Evaporation), Release = sum(Release))

#Join the annual delta storage to the annual release and evaporation data
dfUSBR_API_Agg <- left_join(dfUSBR_API_Agg, dfUSBR_Stor, by = c("WaterYear" = "WaterYear"))

#Now calculate the inflow from release, evaporation, and change in storage
# Lake Mead Inflow = [Change in Storage] + [Release] + [Nevada Diversion] + [Evaporation]
# Use API evaporation data
dfUSBR_API_Agg$MeadInflow <- dfUSBR_API_Agg$DeltaStorage +  dfUSBR_API_Agg$Release +  dfUSBR_API_Agg$Evaporation
dfUSBR_API_Agg$Method <- cMethods[2]

#Use Evaporation table look up from storage
#Create a new data frame
dfUSBR_FromEvapTable <- dfUSBR_API_Agg
dfUSBR_FromEvapTable$MeadInflow <- dfUSBR_FromEvapTable$DeltaStorage +  dfUSBR_FromEvapTable$Release +  dfUSBR_FromEvapTable$EvaporationFromTable
dfUSBR_FromEvapTable$Method <- cMethods[3]


##Compare API Evaporation to Evaporation vs Volume curve
#Read in the evaporation vs storage data from dfMeadEvap

dfMeadEvap <- read.csv(file = "EvapData/dfMeadEvap.csv", header = TRUE)
#Interpolate middle Evaporation from Mead Storage - Evap data
dfUSBR_API_Agg$EvaporationFromTable <- interpNA(xi = dfUSBR_API_Agg$Storage, x= dfMeadEvap$Total.Storage..ac.ft./1e6, y=dfMeadEvap$EvapVolMax/1e6)
#Interpolate range of Evap 
dfUSBR_API_Agg$EvaporationRange <- interpNA(xi = dfUSBR_API_Agg$Storage, x= dfMeadEvap$Total.Storage..ac.ft./1e6, y=dfMeadEvap$EvapVolMaxUp/1e6) - interpNA(xi = dfUSBR_API_Agg$Storage, x= dfMeadEvap$Total.Storage..ac.ft./1e6, y=dfMeadEvap$EvapVolMaxLo/1e6)

##############
###   FIGURE 1
###   Plot API Evaporation vs Table Look up
###############

ggplot() +
  
  geom_point(data = dfUSBR_API_Agg, aes(x= Evaporation, y = EvaporationFromTable),  size = 6) + #color=Method shape=Method, size=6) +
  
  #Add error bars to data points
  #Mead
  geom_errorbar(data=dfUSBR_API_Agg, aes(x=Evaporation,ymin=EvaporationFromTable - EvaporationRange/2, ymax=EvaporationFromTable + EvaporationRange/2), width=.005,
                position=position_dodge(0.2), color="black", show.legend = FALSE) +
  
  #Add 1:1 line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  
  #Add linear regression line
  geom_smooth(data = dfUSBR_API_Agg, aes(x= Evaporation, y = EvaporationFromTable),
              method = "lm",
              formula = y ~ x,
              geom = "smooth") + 
  #Add regression equation to plot
  stat_regline_equation(data = dfUSBR_API_Agg, aes(x= Evaporation, y = EvaporationFromTable),
                        label.x= mean(dfUSBR_API_Agg$Evaporation), label.y=mean(dfUSBR_API_Agg$EvaporationFromTable), size = 6) +
  
  #Make one combined legend
  guides(color = guide_legend("Dataset"), shape = guide_legend("Dataset")) +
  
  #facet_wrap( ~ Source) +
  labs(x="Evaporation USBR API\n(MAF per year)", y="Evaporation from Table\n(MAF per year)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  
  theme_bw() +  
  theme(text = element_text(size=20))


##############################
### Inflow Calc Method #3. Lake Mead.Inflow slot from Colorado River Simulation System (CRSS) historical trace (1907 to present)
#
#        A. file SingleTraceOut.xlsx
#

sExcelFileCRSS <- "SingleTraceOut.xlsx"
dfCRSSOutput<- read_excel(sExcelFileCRSS, sheet = 'RunTest') #  range = "A1:E32")

#Rename first column to Date
cCRSSColNames <- colnames(dfCRSSOutput)
cCRSSColNames[1] <- "CRSSDate"
colnames(dfCRSSOutput) <- cCRSSColNames

#Add a Water Year column
dfCRSSOutput$ModelYear <- year(dfCRSSOutput$CRSSDate)
dfCRSSOutput$Year <- dfCRSSOutput$ModelYear - 2022 + 1907
dfCRSSOutput$Month <- month(dfCRSSOutput$CRSSDate)
dfCRSSOutput$WaterYear <- ifelse(dfCRSSOutput$Month >= 10, dfCRSSOutput$Year + 1, dfCRSSOutput$Year)

# Aggregate to year
dfMeadInflowsCRSS <- dfCRSSOutput %>% dplyr::select(WaterYear, Month, Mead.Inflow) %>% dplyr::group_by(WaterYear) %>% dplyr::summarize(MeadInflow = sum(Mead.Inflow)/1e6)
dfMeadInflowsCRSS$Method <- cMethods[4]




##############################
### Inflow Calc Method #4. Wang / Schmidt - White Paper #5 [https://qcnr.usu.edu/coloradoriver/news/wp5] (2015 to 2020)
# Read in the Water Balance from the Supplemental spreadsheet => Tables => S18:X18
#
# IGNORE because year definitions are different and draw on same gage data
# 
sExcelFileWangSchmidt <- "Supplementary_file-WangSchmidt.xlsx"
dfMeadInflowsWSvals <- read_excel(sExcelFileWangSchmidt, sheet = 'Tables',  range = "S18:W18")
dfMeadInflowsWSyears <- read_excel(sExcelFileWangSchmidt, sheet = 'Tables',  range = "S4:W4")

#Read in values as headers. Reshape to long
cWSvalColNames <- colnames(dfMeadInflowsWSvals)
cWSyearsColNames <- colnames(dfMeadInflowsWSyears)
# Make a new dataframe
dfMeadInflowsWS <- data.frame(Year = cWSyearsColNames, MeadInflow = as.numeric(cWSvalColNames))
#Extract Water year from Year variable
dfMeadInflowsWS$WaterYear <- as.numeric(str_sub(dfMeadInflowsWS$Year,3,6)) + 1

dfMeadInflowsWS$Method <- cMethods[5]



#########
## Bind all the MeadInflow variables from the dataframes for the different methods
## This dataframe will have the structure WaterYear, MeadInflow, Method

#Methods 1 and 2
dfInflows <- rbind(dfGCFlowsUSGS %>% select(WaterYear, MeadInflow, Method), dfUSBR_API_Agg %>% select(WaterYear, MeadInflow, Method) )
#Add Method with evap from table
dfInflows <- rbind(dfInflows, dfUSBR_FromEvapTable %>% select(WaterYear, MeadInflow, Method))
#Add CRSS method
dfInflows <- rbind(dfInflows, dfMeadInflowsCRSS %>% select(WaterYear, MeadInflow, Method))

#Compare inflow values
dfInflowCompare <- dcast(dfInflows, WaterYear ~ Method, value.var = "MeadInflow")

# #Natural flow - Not used but preserve
# dfGCFDataToUse <- dfGCFlowsByYear
# dfGCFDataToUse$GCFlow <- dfGCFDataToUse$GCFlow/1e6
# dfGCFDataToUse$MeadInflowNat <- dfGCFDataToUse$MeadInflowNat/1e6
# dfGCFDataToUse$LeeFerryNaturalFlow <- dfGCFDataToUse$LeeFerryNaturalFlow/1e6
# #Rename the MeadInflowNat column to MeadInflow for later use with rbind
# cColNames <- colnames(dfGCFDataToUse)
# cColNames[4] <- "MeadInflow"
# colnames(dfGCFDataToUse) <- cColNames
# 
# dfGCFDataToUse$Source <- 'Natural Flow'
# 
# #USGS data
# #Pull in the correct columns
# dfGCFDataToUse2 <- as.data.frame(dfGCFlowsUSGS[,c(1,5,6)])
# #Rename the 6th column MeadInflow
# dfGCFDataToUse2 <- dfGCFDataToUse2 %>% dplyr::rename(MeadInflow = MeadInflowUSGS)
# #Assign the Lee Ferry Natural Flow by year
# dfGCFDataToUse2 <- left_join(dfGCFDataToUse2, dfGCFDataToUse[,c("WaterYear","LeeFerryNaturalFlow")], by=c("WaterYear" = "WaterYear"))
# #Sort smallest year to largest year
# dfGCFDataToUse2 <- dfGCFDataToUse2[order(dfGCFDataToUse2$`WaterYear`),]
# dfGCFDataToUse2$Source <- 'USGS'
# #Swap the order of MeadInflow and LeeFerryNaturalFlow
# dfGCFDataToUse2 <- dfGCFDataToUse2 %>% dplyr::select(WaterYear, GCFlow, LeeFerryNaturalFlow, MeadInflow, Source)
# 
# #Bind the two data sets together
# dfGCFDataToUse <- rbind(dfGCFDataToUse, dfGCFDataToUse2)


#Subset of methods to plot
cMethodsToPlot <- cMethods[1:3]
cColorsToPlot <- cColors[1:3]
dfInflowsToPlot <- dfInflows %>% filter(Method %in% cMethodsToPlot)

##### Compare ICS deposits to available water
#####
#Load in the ICS data
dfICSBalanceMelt <- read_csv(file = "dfICSBalanceMelt.csv", col_names = TRUE)
dfICSDeposit <- read_csv(file = "dfICSDeposit.csv", col_names = TRUE)
dfICSDepositMelt <- read_csv(file = "dfICSDepositMelt.csv", col_names = TRUE)


cColNames <- unique(dfICSBalanceMelt$variable) 
#Figure  - timeseries of bar plots of ICS balances
palBlues <- brewer.pal(9, "Blues")


##############
###   FIGURE 2
###   Plot ICS account balances over time
###############

ggplot() +
  
  geom_bar(data=dfICSBalanceMelt %>% filter(variable != "Mexico"), aes(fill=variable,y=value/1e6,x=Year),position="stack", stat="identity") +
  
  #geom_hline(yintercept = nMaxBalance$Total[2]/1e6, size = 2) +
  #geom_line(data=dfMaxBalance, aes(color="Max Balance", y=MaxBal/1e6,x=Year), size=2) +
  
  scale_fill_manual(name="Guide1",values = c(palBlues[3],palBlues[6],palBlues[9]),breaks=cColNames[1:3]) +
  scale_color_manual(name="Guide2", values=c("Black")) +
  
  #scale_x_continuous(breaks=seq(min(dfICSBalanceMelt$Year),max(dfICSBalanceMelt$Year),by=2),labels=seq(min(dfICSBalanceMelt$Year),max(dfICSBalanceMelt$Year),by=2)) +
  
  #Secondary scale with total max balance
  #scale_y_continuous(breaks=seq(0,3,by=1),labels=seq(0,3,by=1), sec.axis = sec_axis(~. +0, name = "", breaks = c(nMaxBalance$Total[2])/1e6, labels = c("Max Balance"))) +
  
  #Secondary scale with individual state max balances
  scale_y_continuous(breaks=seq(0,3,by=1),labels=seq(0,3,by=1)) + #, sec.axis = sec_axis(~. +0, name = "Maximum Balance", breaks = dfMaxBalanceCum$CumVal/1e6, labels = dfMaxBalanceCum$StateAsChar)) +
  
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1), color=FALSE) +
  
  
  theme_bw() +
  
  labs(x="", y="Intentionally Created Surplus\nAccount Balance\n(MAF)") +
  theme(text = element_text(size=20),  legend.title = element_blank(), 
        legend.text=element_text(size=18),
        legend.position= c(0.1,0.80))


##############
###   FIGURE 3
###   Plot ICS deposits over time
###############

ggplot() +
  
  geom_bar(data=dfICSDepositMelt, aes(fill=variable,y=value/1e6,x=Year),position="stack", stat="identity") +
  #geom_line(data=dfMaxAnnualAmounts, aes(y=MaxDeposit/1e6,x=Year), size=2) +
  #geom_line(data=dfMaxAnnualAmounts, aes(color="Max Withdrawal", y=-MaxWithdraw/1e6,x=Year), size=2) +
  
  scale_fill_manual(name="Guide1",values = c(palBlues[3],palBlues[6],palBlues[9]),breaks=cColNames[1:3]) +
  scale_color_manual(name="Guide2", values=c("Black","Black")) +
  
  scale_x_continuous(breaks=seq(min(dfICSDepositMelt$Year),max(dfICSDepositMelt$Year),by=2),labels=seq(min(dfICSDepositMelt$Year),max(dfICSDepositMelt$Year),by=2)) +
  #scale_y_continuous(sec.axis = sec_axis(~. +0, name = "", breaks = c(nMaxBalance$Total[1],-nMaxBalance$Total[3])/1e6, labels = c("Max Deposit","Max Withdraw"))) +
  
  #scale_x_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25), limits = c(0,as.numeric(dfMaxStor %>% filter(Reservoir %in% c("Mead")) %>% select(Volume))),
  #                  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfMeadPoolsPlot$stor_maf, labels = dfMeadPoolsPlot$label)) +
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1), color = FALSE) +
  
  
  theme_bw() +
  
  labs(x="", y="Deposit to Intentionally Created Surplus Account\n(MAF per year)") +
  theme(text = element_text(size=20),  legend.title = element_blank(), legend.text=element_text(size=18),
        legend.position= c(1.075,0.5))


##############
###   FIGURE 4
###   Plot Inflow by different methods as Time series
###############


ggplot() +
  #Data after 1989
  geom_line(data = dfInflowsToPlot, aes(x=WaterYear , y=MeadInflow, color=Method, linetype=Method), size=1.5) +
  theme_bw() +
  
  scale_color_manual(values = cColorsToPlot) +
  scale_linetype_manual(values = c("solid","dotdash", "longdash")) +
  
  #Make one combined legend
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="", y="Lake Mead Inflow\n(MAF per year)", color="") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20))


##############
###   FIGURE 5
###   Plot Inflow as histogram
###############


ggplot() +
  geom_histogram(data = dfInflowsToPlot %>% filter(Method %in% cMethodsToPlot[1]), aes(x = MeadInflow), binwidth = 1, color = "Black", fill = "Blue") +
                   
  
  theme_bw() +
  
  labs(x="Lake Mead Inflow\n(MAF per year)", y="Number of Years") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20), 
        legend.position = "none")



##############
###   FIGURE 6
###   Plot inflow, available water, and ICS deposits as area plot
###############

#Calculate Total ICS deposits each year (sum of positive values)

dfICSDeposit$TotalDeposit <- ifelse(dfICSDeposit$Arizona > 0, dfICSDeposit$Arizona, 0) +
  ifelse(dfICSDeposit$California > 0, dfICSDeposit$California, 0) +
  ifelse(dfICSDeposit$Nevada > 0, dfICSDeposit$Nevada, 0)

ggplot() +
  
  #Inflow
  geom_area(data = dfUSBR_API_Agg, aes(x= WaterYear, y = MeadInflow, color = "Inflow", fill="Inflow")) + #color=Method shape=Method, size=6) +
  
  geom_area(data = dfICSDeposit, aes(x=Year, y = TotalDeposit, color = "ICS Deposit", fill = "ICSDeposit"))
  #Available water = Inflow - evaporation
  geom_area(data = dfUSBR_API_Agg, aes(x= WaterYear, y = MeadInflow - Evaporation, color = "Available Water", fill="Available Water")) + #color=Method shape=Method, size=6) +
  
  #Add line for 9.0 maf
  geom_hline(yintercept = 9, color="red", linetype = "dashed") +
  #Add 1:1 line
  #geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  
  #Add linear regression line
  #geom_smooth(data = dfUSBR_API_Agg, aes(x= Evaporation, y = EvaporationFromTable),
  #           method = "lm",
  #           formula = y ~ x,
  #            geom = "smooth") + 
  #Add regression equation to plot
  #stat_regline_equation(data = dfUSBR_API_Agg, aes(x= Evaporation, y = EvaporationFromTable),
  #                      label.x= mean(dfUSBR_API_Agg$Evaporation), label.y=mean(dfUSBR_API_Agg$EvaporationFromTable), size = 6) +

  # Set x-axis limits
  xlim(min(dfUSBR_API_Agg$WaterYear),max(dfUSBR_API_Agg$WaterYear)) +
  
  #Make one combined legend
  guides(color = guide_legend("Dataset"), shape = guide_legend("Dataset")) +
  
  #facet_wrap( ~ Source) +
  labs(x="", y="Volume\n(MAF per year)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  
  theme_bw() +  
  theme(text = element_text(size=20))


##############
###   FIGURE 7
###   Plot Inflow, Evaporation Table Look up, and Available water
###############

ggplot() +
  
  #Evaporation
  geom_point(data = dfUSBR_API_Agg, aes(x= WaterYear, y = EvaporationFromTable),  size = 6) + #color=Method shape=Method, size=6) +
  
  #Add error bars to data points
  #Mead
  
  geom_errorbar(data=dfUSBR_API_Agg, aes(x=WaterYear, ymin=EvaporationFromTable - EvaporationRange/2, ymax=EvaporationFromTable + EvaporationRange/2), width=.005,
                position=position_dodge(0.2), color="black", show.legend = FALSE) +
  #Inflow
  geom_point(data = dfUSBR_API_Agg, aes(x= WaterYear, y = MeadInflow, color = "orange"), size = 6) + #color=Method shape=Method, size=6) +
  
  #Available water = Inflow - evaporation
  geom_point(data = dfUSBR_API_Agg, aes(x= WaterYear, y = MeadInflow - Evaporation, color = "red"),  size = 6) + #color=Method shape=Method, size=6) +
  
  #Add line for 9.0 maf
  geom_hline(yintercept = 9, color="red", linetype = "dashed") +
  #Add 1:1 line
  #geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  
  #Add linear regression line
  #geom_smooth(data = dfUSBR_API_Agg, aes(x= Evaporation, y = EvaporationFromTable),
   #           method = "lm",
   #           formula = y ~ x,
  #            geom = "smooth") + 
  #Add regression equation to plot
  #stat_regline_equation(data = dfUSBR_API_Agg, aes(x= Evaporation, y = EvaporationFromTable),
  #                      label.x= mean(dfUSBR_API_Agg$Evaporation), label.y=mean(dfUSBR_API_Agg$EvaporationFromTable), size = 6) +
  
  # Set x-axis limits
  xlim(min(dfUSBR_API_Agg$WaterYear),max(dfUSBR_API_Agg$WaterYear)) +

  #Make one combined legend
  guides(color = guide_legend("Dataset"), shape = guide_legend("Dataset")) +
  
  #facet_wrap( ~ Source) +
  labs(x="", y="Volume\n(MAF per year)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  
  theme_bw() +  
  theme(text = element_text(size=20))


#################
###   FIGURE 8
###   Plot Mead Inflow as a box-and-whisker by different methods
#################

ggplot() +
  geom_boxplot(data = dfInflowsToPlot, aes(x=Method , y=MeadInflow, fill=Method)) +
  theme_bw() +
  
  #Data before 1990
  #geom_boxplot(data = dfGCFDataToUse %>% filter(WaterYear < 1990), aes(x="Before 1990 Natural Flow" , y=MeadInflow, fill="Before 1990 Natural Flow")) +
  
  #scale_x_discrete(labels = c("Natural Flow" = "Natural Flow\n(1990 to 2016)", "Before 1990 Natural Flow" = "Natural Flow\n(1905 to 1989)", "USGS" = "USGS\n(1990 to 2016)") ) +
  scale_fill_manual(values = cColorsToPlot) +
  
  theme_bw() +
  
  labs(x="", y="Lake Mead Inflow\n(MAF per year)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20), 
        legend.position = "none")

#Reshape the data so Methods are in columns
dfInflowsWide <- dcast(dfInflowsToPlot, WaterYear ~ Method, value.var = "MeadInflow")
dfInflowsWide$Diff <-  dfInflowsWide$`USGS Gages` - dfInflowsWide$`USBR Application Program Interface`


#################
###   FIGURE 9
###   Show the correlation between USGS and USBR estimates of inflow
#################

ggplot() +
  
  geom_point(data = dfInflowsWide, aes(x= `USBR Application Program Interface`, y=`USGS Gages`),  size = 6) + #color=Method shape=Method, size=6) +
  
  #geom_point(data = dfInflowsWide, aes(x= WaterYear, y=Diff),  size = 6) + #color=Method shape=Method, size=6) +
  
  #geom_point(data = dfGCFDataToUse %>% filter(WaterYear < 1990), aes(x= LeeFerryNaturalFlow, y=MeadInflow, color="Natural Flow pre 1990", shape="Natural Flow pre 1990"), size=6) +
  
  #scale_shape_manual(values=c(17,16,16), breaks = c("USGS","Natural Flow","Natural Flow pre 1990"), labels  = c("USGS (after 1990)","Natural Flow (after 1990)","Natural Flow (before 1990)")) +
  
  #scale_color_manual(values=c("Blue","Red","Pink"), breaks = c("USGS","Natural Flow","Natural Flow pre 1990"), labels  = c("USGS (after 1990)","Natural Flow (after 1990)","Natural Flow (before 1990)")) +
  
  #Add 1:1 line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  
  #Add linear regression line
  geom_smooth(data = dfInflowsWide, aes(x= `USBR Application Program Interface`, y=`USGS Gages`),
              method = "lm",
              formula = y ~ x,
              geom = "smooth") + 
  #Add regression equation to plot
  stat_regline_equation(data = dfInflowsWide, aes(x= `USBR Application Program Interface`, y=`USGS Gages`),
                          label.x=9, label.y=13, size = 6) +
  
  #Make one combined legend
  guides(color = guide_legend("Dataset"), shape = guide_legend("Dataset")) +
  
  #facet_wrap( ~ Source) +
  labs(x="USBR Back Calculation\n(MAF per year)", y="USGS Gages\n(MAF per year)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  
  theme_bw() +  
  theme(text = element_text(size=20))


#################
###   FIGURE 10
###   Show the difference between USGS and USBR methods to estimate inflow
#################

ggplot() +
  #Points after 1990 in Blue and Red
  #geom_point(data = dfInflowsWide, aes(x= `USBR Application Program Interface`, y=`USGS Gages`),  size = 6) + #color=Method shape=Method, size=6) +
 
  geom_point(data = dfInflowsWide, aes(x= WaterYear, y=Diff),  size = 6) + #color=Method shape=Method, size=6) +
  
  #geom_point(data = dfGCFDataToUse %>% filter(WaterYear < 1990), aes(x= LeeFerryNaturalFlow, y=MeadInflow, color="Natural Flow pre 1990", shape="Natural Flow pre 1990"), size=6) +

  #scale_shape_manual(values=c(17,16,16), breaks = c("USGS","Natural Flow","Natural Flow pre 1990"), labels  = c("USGS (after 1990)","Natural Flow (after 1990)","Natural Flow (before 1990)")) +
  
  #scale_color_manual(values=c("Blue","Red","Pink"), breaks = c("USGS","Natural Flow","Natural Flow pre 1990"), labels  = c("USGS (after 1990)","Natural Flow (after 1990)","Natural Flow (before 1990)")) +
  
  #Add 1:1 line
  #geom_abline(intercept = 0, slope = 1, linetype = "dash", color = red, size = 1) +
  
  #Make one combined legend
  guides(color = guide_legend("Dataset"), shape = guide_legend("Dataset")) +
  
  #facet_wrap( ~ Source) +
  labs(x="", y="Difference in Lake Mead Inflow\n(MAF per year)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  
  theme_bw() +  
  theme(text = element_text(size=20))

## Show the correlation matrix
mCorr <- cor(as.data.frame(dfInflowsWide %>% filter(WaterYear >= 2005, WaterYear < 2023) %>%  select(`USGS Gages`,`USBR Application Program Interface`) ))
print(paste("Correlation = ",round(mCorr[1,2],2)))


# Not working beyond here


#### Figures 6 and 7. Show the sequence average plot using Salehabadi code for Natural Flow data set and USGS data

############################################################################################################
###### Sequence Average Plot (Dotty Plot)                                                             ######
######  - creat the Sequence-Average plot (blue dots: full period,  red dots: post-yr period)         ######                                      
######  - Add the long term average of the flow over the full and post-yr periods as horizontal lines ######
######                                                                                                ######
######  Homa Salehabadi                                                                               ###### 
############################################################################################################

# This may not be needed if already installed.
#install.packages("openxlsx") 
library(openxlsx)

#####==================================================================================================================
##### Inputs (change them if needed) ==================================================================================

## Set working directory if necessary e.g.
# setwd("H:/Homa/PhD/Research/Works/SeqAvePlots")

##### n.lowest function: find the nth lowest value (or index) in x ====================================================

n.lowest <- function(x,n,value=TRUE){
  s <- sort(x,index.return=TRUE)
  if(value==TRUE)  {s$x[n]}  else  {s$ix[n]}    ## TRUE: n.lowest=value   FALSE: n.lowest=index
} 


# Natural Flow Plot
## Input Files ------------------------------------------------------------------------------
#filename1 <- "R_InputData.xlsx"
#sheetname1 <-  "AnnualWYTotalNaturalFlow_LF2018"    ## Natural flow: "AnnualWYTotalNaturalFlow_LF2018"   ## Tree ring: "TR_Meko_2017-SK"

#A data frame to loop over
dfDataTypes <- data.frame(Source = c("USGS","Natural Flow"), minY = c(6,6), maxY = c(2,2))

for(iType in (1:nrow(dfDataTypes))) {
  
   # iType <- 2

    ### Pull the data into the data data frame for plotting
    data <- dfGCFDataToUse %>% filter(Source == dfDataTypes$Source[iType], WaterYear > 1906)
    
    ## Factor to change the current unit --------------------------------------------------------
    unit_factor <- 1 #10^(-6)   ## ac-ft to MAF
    
    ## Maximum length of sequence (sequences will be from 1 to seq_yr) --------------------------
    seq_yr <- 15 ## 25
    
    ## desired period ---------------------------------------------------------------------------
    #yr1 <- 1990   ## NF:1906   TR:1416       
    #yr2 <- 2016   ## NF:2018   TR:2015    
      
    ## A year to divide the period into two period.  --------------------------------------------
    post_year <- 2000   ## post-year will be distinguished in plot
    
    #desired period is the min and max water years
    yr1 <- min(data$WaterYear)
    yr2 <- max(data$WaterYear)
    
    
    #data <- read.xlsx(filename1, sheet=sheetname1, colNames=TRUE)
    #data <- read.csv(file = "GrandCanyonFlows.csv", header = TRUE, sep =",", strip.white = TRUE)
    years <- yr1:yr2
    n <- length(years)
    
    
    #### Sequence Average plot ###########################################################################################     
    ####   - creat the Sequence-Average plot                                                 
    ####   - add the long term average of the flow over the full and post-yr periods as horizontal lines
    ####
    #### >> Check Legend if needed  
    
    ## take the flow data ------------
    flow <- data[ which(data[,1]==yr1):which(data[,1]==yr2) ,(4)]
    
    ## define empty matrixes -------------
    Mean<- matrix(rep(NA), nrow=n , ncol=seq_yr)
    lowest <- matrix(rep(NA), nrow=n , ncol=seq_yr)
    lowest_index <- matrix(rep(NA), nrow=n , ncol=seq_yr)
    lowest_year <- matrix(rep(NA), nrow=n , ncol=seq_yr)
    
    ## calculate the averages over the sequences---------------
    ## Loop: over the defined sequences
    for (m_yr in 1:seq_yr){  
      
      print(m_yr)
      
      mean_m_yr <- rep(NA)
      sort <- rep(NA)
      
      for (m in 1:(n-(m_yr-1))){
        mean_m_yr[m] <- mean( flow[ m : (m+(m_yr-1)) ] )
        Mean[m ,m_yr] <- mean_m_yr[m]
        
        print(paste("m: ",m))
      }
      
      for (m in 1:(n-(m_yr-1))){
        lowest[m ,m_yr] <- n.lowest( mean_m_yr,m,value=TRUE)
        lowest_index[m ,m_yr] <- n.lowest(mean_m_yr,m,value=FALSE)   
        lowest_year[m ,m_yr] <- years[lowest_index[m ,m_yr]]
      }
      
    }
    
    
    ## change unit to MAF ----------------------
    lowest_MAF <- lowest*unit_factor  
    
    ###### Plot SeqAve (dotty plots) ==========================================================================
    
    ## the final dataframe that you want its dotty plot will be SeqAve
    SeqAve <- lowest_MAF
    
    ## will be used to plot with a better scale:
    #min <-  6 #floor(min(SeqAve, na.rm=TRUE))
    max <- ceiling(max(SeqAve, na.rm=TRUE))
    
    min <- dfDataTypes$minY[iType]
    #max <- dfDataTypes$maxY[iType]
    
    ##### plot -----------------------------------------------------------
    x <- c(1:seq_yr)
    par(mar=c(5, 4, 3, 2) + 0.2 , mgp=c(2.5, 1, 0) )
    
    ## 1- For natural flow run this:
    plot(x, SeqAve[1,], col="white", ylim=c(min, max) , xlim=c(1, seq_yr+1), xaxt="n" ,yaxt="n",
         pch=16, cex=0.6, xlab="Length of sequence (year)", ylab="Lake Mead Inflow\n(maf per year)", cex.lab=1.3, 
         main=paste0("Lake Mead Inflow),  Period: " ,yr1,"-",yr2,paste0("\n",dfDataTypes$Source[iType]," Data")) )  ## , cex.main=1.3
    
    ### axis of the plot -------
    axis(1, at=seq(1,seq_yr,1), cex.axis=1)
   # axis(2, at=seq((min-2),max,0.25), cex.axis=1, las=1)  ## las=1 to rotate the y lables
    axis(2, at=seq((min-2),max,1), cex.axis=1, las=1)  ## las=1 to rotate the y lables
    
    ### plot dots and seperate them to blue and red ones ---------
    
    ## full period
    for (j in 1:seq_yr){  
      for (i in 1:(n-(j-1))){  #1:n
        points(j, SeqAve[i,j], col= "lightskyblue2" ,pch=1, cex=0.5, lwd=1)
      }
    }
    
    ## specify post-yr period
    for (j in 1:seq_yr){  
      for (i in 1:(n-(j-1))){  #1:n
        
        if ( lowest_year[i,j]>=post_year) {
          points(j, SeqAve[i,j], col= "black" ,bg="red" ,pch=21, cex=0.7, lwd=0.2)
        }
      }
    }
    
    
    ### add a line representing the long-term average of flow during the full period -----------
    ave_all <- mean(flow)* unit_factor
    abline (ave_all, 0, col="steelblue2", lwd=1.2)
    
    ### add a line representing the long-term average of flow during the post-yr period 
    while(post_year<=yr2){
      ave_post <- mean(flow[(which(years==post_year) : which(years==yr2))] ) * unit_factor
      abline (ave_post, 0, col="red", lwd=1.2)
      break}
    
    
    ### lable the two lines of long-term average -----------
    if(post_year<=yr2){
      if(ave_all>ave_post){
        text((seq_yr+0.2),(ave_all+0.3), labels= paste(round(ave_all, digits=2)), pos = 4, cex=1, col="dodgerblue3", xpd=TRUE)  ##, font=2
        text((seq_yr+0.2),(ave_post-0.4), labels= paste(round(ave_post, digits=2)), pos = 4, cex=1, col="red", xpd=TRUE)
      }
      if(ave_all<ave_post){
        text((seq_yr+0.2),(ave_all-0.4), labels= paste(round(ave_all, digits=2)), pos = 4, cex=1, col="dodgerblue3", xpd=TRUE)  ##, font=2
        text((seq_yr+0.2),(ave_post+0.4), labels= paste(round(ave_post, digits=2)), pos = 4, cex=1, col="red", xpd=TRUE)
      }
    } else {
      text((seq_yr+0.2),(ave_all+0.3), labels= paste(round(ave_all, digits=2)), pos = 4, cex=1, col="dodgerblue3", xpd=TRUE)
    }
    
    
    ### lable the first and second lowest SeqAve ----------
    text(SeqAve[1,]~x, labels=lowest_year[1,], pos = 1, cex=0.6, col="black", srt=0) ## the lowest     (vertical text: srt=90)
    text(SeqAve[2,]~x, labels=lowest_year[2,], pos = 2, cex=0.5, col="gray47", srt=0)  ## the second lowest
    
    
    ### 1- Legend for natural flow 1906-2018 -----------
    legend("topright", legend=c(paste0("Full Period (",yr1,"-",yr2,")"),paste0("Post-",post_year,"(",post_year,"-",yr2,")"), paste0("Long term mean (",yr1,"-",yr2,")"),  paste0("Long term mean (",post_year,"-", yr2,")")),
           col=c("lightskyblue3","black","steelblue2","red"), pt.bg=c(NA,"red", NA,NA) , pch=c(1,21, NA, NA), pt.cex=c(0.6, 0.8),
           lwd=1,  lty=c(0,0,1,1), inset=c(0.05, 0.03), bty = "n")
}


  


