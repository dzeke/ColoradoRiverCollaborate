# HistoricalCurrentReservoirData.r
#
# This script harvests Daily, Monthly, and Annual Lake Powell and Lake Mead data
# from the Reclamation data portal and plots a variety of historical
# and current reservoir data.

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

here::i_am("HistoricalCurrentReservoirData/HistoricalCurrentReservoirData.r")
        
## Read in functions to:
#     Auto load USBR data
#     Interpolate with NAs
#     Load Reservoir Bathymetry and Critical Elevations
source("../AutoReadUSBRData/AutoReadUSBRData.r")

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

dMaxDay <- as.character(as.Date(max(dfResDataDaily$DateValue)) - 1)
#dfResElevations <- dfResDataDaily %>% filter(FieldName %in% c("Pool Elevation","Storage"), DateValue == dYesterday) %>% select(ResName, FieldName, Value)
dfResElevations <- dfResDataDaily %>% filter(FieldName %in% c("Pool Elevation","Storage"), DateValue == dMaxDay) %>% select(ResName, FieldName, Value)


# Make the Table easy to Read
dfResValues <- pivot_wider(  dfResElevations %>% group_by(ResName),  names_from = FieldName,   values_from = Value)
# Convert storage to million acre-feet
dfResValues$Storage <- dfResValues$Storage / 1e6
print(dfResValues)

################
#  Figure 3. Powell, Mead, and Combined storage plot
#

# Combined, Powell, and Mead on one plot

# Filter the Powell and Mead monthly storages

dfResDataMonthly <- lResData$dfResMonthly

#Filter the Powell and Mead storages
dfResStorage <- dfResDataMonthly %>% filter(ResName %in% c("Lake Powell", "Lake Mead"), FieldName == "Storage")

#Turn narrow into wide so separate columns for Lake Powell and Lake Mead
dfResStorageWide <- pivot_wider(  dfResStorage %>% group_by(ResName, Date, Year, Month) %>% select(ResName, Date, Year, Month, MonthlyValue),   names_from = ResName,   values_from = MonthlyValue)

#Filter to data after 1995
dfResStorageWide1995 <- dfResStorageWide %>% filter(Year >= 1995)

ggplot() +
  #Powell storage
  geom_line(data=dfResStorageWide1995 ,aes(x=as.Date(Date), y=`Lake Powell`, color="Powell"), size=2) +
  #Mead Storage
  geom_line(data=dfResStorageWide1995 ,aes(x=as.Date(Date), y=`Lake Mead`, color="Mead"), size=2) +
  #Combined Storage
  geom_line(data=dfResStorageWide1995,aes(x=as.Date(Date), y=`Lake Powell` + `Lake Mead`, color="Combined"), size=2) +
  scale_color_manual(values = c("purple","red","blue"), breaks=c("Combined", "Powell", "Mead")) +
  #geom_area(data=dfPlotData,aes(x=month,y=stor_maf, fill = variable), position='stack') +
  scale_y_continuous(breaks = seq(0,50,by=10),labels=seq(0,50,by=10)) +
  scale_x_date(limits= c(as.Date("1995-01-01"), as.Date("2030-01-01")),
               date_breaks = "5 years", # Major ticks every 10 years
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


################
# Figure 4. Powell on X and Mead on Y with 1:1

ggplot() +
  # Plot a 1:1 line to total storage
  geom_abline(intercept = 0, slope = 1, color="1:1 line (by volume)", size=2, linetype="longdash", show.legend = FALSE) +
    #Label the 1:1 line
  geom_text(aes(x=3.5,y=4.2,label="1:1 line"), color="black", angle = 45, size =6, show.legend = FALSE) +

  #Overplot Mead-Powell historical storage -- January of each year
  #Before guidelines in place in Red
  geom_path(data = dfResStorageWide %>% filter(Month == 1, Year < 2007), aes(x = `Lake Powell`, y = `Lake Mead`, color="Before Guidelines"), size=1.5, linetype=1, show.legend = TRUE) +
  #After guidelines in place in purple
  geom_path(data = dfResStorageWide %>% filter(Month == 1, Year >=  2007), aes(x = `Lake Powell`, y = `Lake Mead`, color="After Guidelines"), size=1.5, linetype=1, show.legend = TRUE) +

  #Label each January with it's year
  #geom_text_repel(data=dfResStorageWide %>% filter(Month == 1, Year %% 4 == 0), point.padding = NA, aes(x = `Lake Powell`, y = `Lake Mead`, label = Year, angle = 0, size = 2)) +
  #geom_text_repel(data=dfResStorageWide %>% filter(Month == 1, Year %% 4 == 0), aes(x = `Lake Powell`, y = `Lake Mead`, label = Year, angle = 0, size = 2)) +

  # All years in black
  #geom_text(data = dfResStorageWide %>% filter(Month == 1, Year %% 2 == 0), aes(x = `Lake Powell`, y = `Lake Mead`, label = Year, angle = 0, size = 1)) +
  
  #Before guidelines
  geom_text(data = dfResStorageWide %>% filter(Month == 1, Year %% 2 == 0, Year < 2007), aes(x = `Lake Powell`, y = `Lake Mead`, label = Year, angle = 0, size = 1, color = "Before Guidelines")) +
  # After guidelines
  geom_text(data = dfResStorageWide %>% filter(Month == 1, Year %% 2 == 0, Year >= 2007), aes(x = `Lake Powell`, y = `Lake Mead`, label = Year, angle = 0, size = 1, color = "After Guidelines")) +
  
  
    # Before guidelines
  #geom_text_repel(data=dfResStorageWide %>% filter(Month == 1, Year < 2007), point.padding = NA, aes(x = `Lake Powell`, y = `Lake Mead`, label = Year, color="Before Guidelines", angle = 0, size = 2, check_overlap = TRUE)) +
  # After guidelines
  #geom_text_repel(data=dfResStorageWide %>% filter(Month == 1, Year >= 2007), point.padding = NA, aes(x = `Lake Powell`, y = `Lake Mead`, label = Year, color="After Guidelines", angle = 0, size = 2, check_overlap = TRUE)) +
  
  
  
   #set colors for lines

   scale_color_manual(breaks = c("Before Guidelines", "After Guidelines"),
                          #All years in Black 
                          # values= c(pBlues[8], pBlues[5], "black"),
                            values= c(pBlues[6], pReds[6]),
                            labels = c("Before Guidelines", "With Guidelines")) +
  
  #Create secondary y axes for Mead Lake Level
  scale_y_continuous(limits = c(0,25), breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25),  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfTemp$dfMeadElevations$ActiveStorageMAF, labels = dfTemp$dfMeadElevations$Label)) +
  #Create secondary x axes for Powell Lake Level
  scale_x_continuous(limits = c(0,25), breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25), sec.axis = sec_axis(~. +0, name = "Powell Level (feet)", breaks = dfTemp$dfPowellElevations$ActiveStorageMAF , labels = dfTemp$dfPowellElevations$`Elevation (feet)`)) +
  
  theme_bw() +
  coord_fixed() +
  
  #guides(size = "none", colour = guide_legend("Historical volumes (year)"), fill="none") +
  guides(size = "none", colour = guide_legend(""), fill="none") +
  
  labs(x="Powell Active Storage (MAF)", y="Mead Active Storage (MAF)") +

       # theme(text = element_text(size=14), legend.text=element_text(size=10),
       #  panel.border = element_rect(colour = "black", fill="NA"),
       #  legend.background = element_blank(),
       #  legend.box.background = element_rect(colour = "black", fill="grey"),
       #  legend.position = c(1.13,0.620))
  theme(text = element_text(size=14))  # legend.position = "none")


##########
## Figure 5. Lake Powell storage/elevation over time

# Calculate volumes from current storage to elevations 3,525, 3,500, and 3490
dfPowellElevations <- dfTemp$dfPowellElevations
dfPowellElevations$CurrentStorage <- dfResElevations %>% filter(FieldName == "Storage", ResName == "Lake Powell") %>% pull(Value) / 1e6
dfPowellElevations$StorageAbove <- dfPowellElevations$CurrentStorage - dfPowellElevations$ActiveStorageMAF
dfPowellElevations$StorageAboveLabel <- paste0(round(dfPowellElevations$StorageAbove, digits = 1), " maf above elev. ", format(dfPowellElevations$`Elevation (feet)`,scientific = FALSE, big.mark = ",", trim = TRUE))

ggplot() +

  geom_line(data = dfResStorageWide, aes(x = as.Date(Date), y = `Lake Powell`), size = 1.5) +
  #geom_point(data = dfResStorageWide %>% filter(Date == max(Date)), aes(x = Date, y = `Lake Powell`), color = pReds[7] ) +
  
   geom_text(data = dfPowellElevations %>% filter(`Elevation (feet)` >= 3490, `Elevation (feet)` <= 3525), aes(x = as.Date("2029-01-01"), y = 2.5 + ActiveStorageMAF, label = StorageAboveLabel), color = pReds[7]) +
    scale_x_date(limits= c(as.Date("1995-01-01"), as.Date("2030-01-01")),
               date_breaks = "5 years", # Major ticks every 10 years
               date_labels = "%Y") +
  
  #Create secondary y axes for Powell Lake Level
   scale_y_continuous(limits = c(0,25), breaks = dfTemp$dfPowellElevations$ActiveStorageMAF,labels=round(dfTemp$dfPowellElevations$ActiveStorageMAF, digits = 1), sec.axis = sec_axis(~. +0, name = "Powell Elevation (feet)", breaks = dfTemp$dfPowellElevations$ActiveStorageMAF , labels = dfTemp$dfPowellElevations$Label)) +
  
  geom_hline(yintercept = dfTemp$dfPowellElevations$ActiveStorageMAF, color = pBlues[5], linetype = "dashed") +
  theme_bw() +
  
  labs(x="", y="Powell Active Storage (MAF)") +
  theme(text = element_text(size=14), legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

 
##########
## Figure 6. Lake Mead storage/elevation over time

ggplot() +
  
  geom_line(data = dfResStorageWide, aes(x = as.Date(Date), y = `Lake Mead`), size = 1.5) +
  
  scale_x_date(limits= c(as.Date("1995-01-01"), as.Date("2030-01-01")),
               date_breaks = "5 years", # Major ticks every 10 years
               date_labels = "%Y") +
  
  #Create secondary y axes for Mead Lake Level
  scale_y_continuous(limits = c(0,25), breaks = dfTemp$dfMeadElevations$ActiveStorageMAF,labels=round(dfTemp$dfMeadElevations$ActiveStorageMAF, digits = 1), sec.axis = sec_axis(~. +0, name = "Mead Elevation (feet)", breaks = dfTemp$dfMeadElevations$ActiveStorageMAF , labels = dfTemp$dfMeadElevations$Label)) +
  
  geom_hline(yintercept = dfTemp$dfMeadElevations$ActiveStorageMAF, color = pBlues[5], linetype = "dashed") +
  theme_bw() +
  
  labs(x="", y="Mead Active Storage (MAF)") +
  theme(text = element_text(size=14), legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


##################
## Figure 7

#Histogram -- frequency of annual Release volumes
ggplot(dfResDataAnnual %>% filter(ResName == "Lake Powell", FieldName == "Release volume"), aes(x=AnnualValue)) +
  geom_histogram(color="darkmagenta", fill="magenta", binwidth = 2) +
  
  scale_x_continuous(limits = c(2,22), breaks = seq(2,22,by=2)) +
  #scale_y_continuous(breaks = seq(0,11,by=2)) +
  
  labs(x="Powell Release\n(million acre feet per year)", y="Frequency\n(number of years)") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank())


#################
# Figure 8. Histogram -- frequency of annual release volumes
ggplot(dfResDataAnnual %>% filter(ResName == "Lake Powell", FieldName == "Inflow Volume"), aes(x=AnnualValue)) +
  geom_histogram(color="darkmagenta", fill="magenta", binwidth = 2) +
  
  scale_x_continuous(limits = c(2,22), breaks = seq(2,22,by=2)) +
  #scale_y_continuous(breaks = seq(0,11,by=2)) +
  
  labs(x="Powell Inflow\n(million acre feet per year)", y="Frequency\n(number of years)") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank())

##############
### Figure 9. As mixed bar (inflow) line (release) plot

ggplot() +
  #Bar graph of Powell Inflow
  geom_bar(data = dfResDataAnnual %>% filter(ResName == "Lake Powell", FieldName == "Inflow Volume"), aes(x = WaterYear, y=AnnualValue, fill = "Inflow"), stat="identity") +
  #Line graph of Powell Release
  geom_line(data = dfResDataAnnual %>% filter(ResName == "Lake Powell", FieldName == "Release volume"), aes(x = WaterYear, y=AnnualValue, group = 1, color="Release"), size=2) +
  scale_color_manual(" ", values = c("Inflow" = "grey50", "Release" = "blue")) +
  scale_fill_manual("", values="grey50") +
  labs(x="", y="Water Volume\n(million acre-feet per year)") +  
  
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
        panel.background = element_blank(), 
        legend.key = element_blank(),
        axis.line = element_line(colour = "black"))

#### Figure 10. Powell Annual Evaporation
ggplot(data = dfResDataAnnual %>% filter(ResName == "Lake Powell", FieldName == "Evaporation")) +
  geom_line(aes(x=WaterYear, y=`AnnualValue`, color=pBlues[7]), size=2) +

  #geom_area(data=dfPlotData,aes(x=month,y=stor_maf, fill = variable), position='stack') +
  #scale_y_continuous(breaks = seq(0,50,by=10),labels=seq(0,50,by=10)) +
  #scale_x_date(limits= c(as.Date("1995-01-01"), as.Date("2030-01-01")),
  #             date_breaks = "5 years", # Major ticks every 10 years
  #             date_labels = "%Y") +
  
  #scale_fill_manual(breaks=c(1:6),values = palBlues[2:7]) + #,labels = variable) + 
  theme_bw() +
  #coord_fixed() +
  labs(x="", y="Lake Powell Evaporation\n(MAF per year)") +
  theme(text = element_text(size=20), legend.position = "none")



##################
## ICS Data

lICSdata <- fReadICSData()

########################
# Figure 11. Timeseries of bar plots of ICS balances 
lFontSize <- 20

cColNamesICSBalance <- colnames(lICSdata$dfICSBalance)

ggplot() +
  
  geom_bar(data=lICSdata$dfICSBalanceNarrow %>% filter(variable != "Mexico"), aes(fill=variable,y=value/1e6,x=Year),position="stack", stat="identity") +
  
  #geom_hline(yintercept = nMaxBalance$Total[2]/1e6, size = 2) +
  geom_hline(yintercept = lICSdata$dfICSLimits$Total[2]/1e6, size = 2) +
  #geom_line(data=dfMaxBalance, aes(color="Max Balance", y=MaxBal/1e6,x=Year), size=2) +
  
  scale_fill_manual(name="Guide1",values = c(pBlues[3],pBlues[6],pBlues[9]),breaks=cColNamesICSBalance[2:4]) +
  scale_color_manual(name="Guide2", values=c("Black")) +
  
  #scale_x_continuous(breaks=seq(min(dfICSBalanceMelt$Year),max(dfICSBalanceMelt$Year),by=2),labels=seq(min(dfICSBalanceMelt$Year),max(dfICSBalanceMelt$Year),by=2)) +
  scale_x_continuous(breaks=seq(min(lICSdata$dfICSBalanceNarrow$Year),max(lICSdata$dfICSBalanceNarrow$Year),by=2),labels=seq(min(lICSdata$dfICSBalanceNarrow$Year),max(lICSdata$dfICSBalanceNarrow$Year),by=2)) +
  
  #Secondary scale with total max balance
  #scale_y_continuous(breaks=seq(0,3,by=1),labels=seq(0,3,by=1), sec.axis = sec_axis(~. +0, name = "", breaks = c(nMaxBalance$Total[2])/1e6, labels = c("Max Balance"))) +
  
  #Secondary scale with individual state max balances
  #scale_y_continuous(breaks=seq(0,3,by=1),labels=seq(0,3,by=1), sec.axis = sec_axis(~. +0, name = "Maximum Balance", breaks = dfMaxBalanceCum$CumVal/1e6, labels = dfMaxBalanceCum$StateAsChar)) +
  scale_y_continuous(breaks=seq(0,3,by=1),labels=seq(0,3,by=1), sec.axis = sec_axis(~. +0, name = "Maximum Balance", breaks = lICSdata$dfMaxBalanceCum$CumVal/1e6, labels = lICSdata$dfMaxBalanceCum$StateAsChar)) +
  
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1), color=FALSE) +
  
  
  theme_bw() +
  
  labs(x="", y="Lake Mead Water Conservation\nAccount Balance\n(MAF)") +
  theme(text = element_text(size=lFontSize),  legend.title = element_blank(), 
        legend.text=element_text(size=lFontSize - 2),
        legend.position= c(0.2,0.80))

###################
## Figure 12. ICS Deposits/Withdraws by Year

dfICSDepositsWithdraws <- lICSdata$dfICSDepositNarrow

ggplot() +
  
  geom_bar(data= dfICSDepositsWithdraws, aes(fill=variable,y=value/1e6,x=Year),position="stack", stat="identity") +

  #geom_line(data=dfMaxAnnualAmounts, aes(y=MaxDeposit/1e6,x=Year), size=2) +
  geom_line(data=lICSdata$dfMaxAnnualAmounts, aes(y=MaxDeposit/1e6,x=Year), size=2) +
  geom_line(data=lICSdata$dfMaxAnnualAmounts, aes(color="Max Withdrawal", y=-MaxWithdraw/1e6,x=Year), size=2) +
  
  scale_fill_manual(name="Guide1",values = c(pBlues[3],pBlues[6],pBlues[9]),breaks=cColNamesICSBalance[2:4]) +
  scale_color_manual(name="Guide2", values=c("Black","Black")) +
  
  scale_x_continuous(breaks=seq(min(lICSdata$dfICSDepositNarrow$Year),max(lICSdata$dfICSDepositNarrow$Year),by=2),labels=seq(min(lICSdata$dfICSDepositNarrow$Year),max(lICSdata$dfICSDepositNarrow$Year),by=2)) +
  scale_y_continuous(sec.axis = sec_axis(~. +0, name = "", breaks = c(lICSdata$dfICSLimits$Total[1],-lICSdata$dfICSLimits$Total[3])/1e6, labels = c("Max Credit","Max Debit"))) +
  
  #scale_x_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25), limits = c(0,as.numeric(dfMaxStor %>% filter(Reservoir %in% c("Mead")) %>% select(Volume))),
  #                  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfMeadPoolsPlot$stor_maf, labels = dfMeadPoolsPlot$label)) +
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1), color = FALSE) +
  
  
  theme_bw() +
  
  labs(x="", y="Credits (+) and Debits (-) to\nLake Mead Water Conservation Accounts\n(MAF per year)") +
  theme(text = element_text(size=lFontSize - 4),  
        axis.text.y = element_text(size = lFontSize - 4),
        legend.title = element_blank(),
        legend.text=element_text(size=lFontSize - 6),
        legend.position= c(1.075,0.5))