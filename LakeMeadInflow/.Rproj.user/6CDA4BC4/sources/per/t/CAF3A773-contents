####################
#     Lake Mead Inflow
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
#       2. Lake Mead.Inflow slot from Colorado River Simulation System (CRSS) historical trace (1907 to present)
#
#             A. 
#
#       3. Back calculate from Lake Mead storage, release, Nevada Diversion, and Lake Mead evaporation (2004 to present)
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
#                 Lake Mead Inflow = [Change in Storage] + [Release] + [Nevada Diversion] + [Evaporation]
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

# Load required libraies

if (!require(tidyverse)) { 
  install.packages("tidyverse", repos="http://cran.r-project.org") 
  library(tidyverse) 
}

if (!require(readxl)) { 
  install.packages("readxl", repos="http://cran.r-project.org") 
  library(readxl) 
}

if (!require(RColorBrewer)) { 
  install.packages("RColorBrewer",repos="http://cran.r-project.org") 
  library(RColorBrewer) # 
}

if (!require(dplyr)) { 
  install.packages("dplyr",repos="http://cran.r-project.org") 
  library(dplyr) # 
}

if (!require(expss)) { 
  install.packages("expss",repos="http://cran.r-project.org") 
  library(expss) # 
}

if (!require(reshape2)) { 
  install.packages("reshape2", repos="http://cran.r-project.org") 
  library(reshape2) 
}

if (!require(pracma)) { 
  install.packages("pracma", repos="http://cran.r-project.org") 
  library(pracma) 
}

if (!require(lubridate)) { 
  install.packages("lubridate", repos="http://cran.r-project.org") 
  library(lubridate) 
}

if (!require(directlabels)) { 
  install.packages("directlabels", repo="http://cran.r-project.org")
  library(directlabels) 
}

if (!require(plyr)) { 
  install.packages("plyr", repo="http://cran.r-project.org")
  library(plyr) 
}

if (!require(stringr)) { 
  install.packages("stringr", repo="http://cran.r-project.org")
  library(stringr) 
}


if (!require(ggplot2)) { 
  install.packages("ggplot2", repo="http://cran.r-project.org")
  library(ggplot2) 
}


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
dfGCFlowsUSGS <- read_excel(sExcelFileUSGSFlow, sheet = 'Combined',  range = "A1:E32")
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
dfGCFlowsUSGS$MeadInflowUSGS <- dfGCFlowsUSGS$`Colorado River near Peach Springs` + dfGCFlowsUSGS$`Virgin River at Littlefield` + dfGCFlowsUSGS$LasVegasWash
dfGCFlowsUSGS$Method <- "USGSgages"

##############################
### Inflow Calc Method #2. Lake Mead.Inflow slot from Colorado River Simulation System (CRSS) historical trace (1907 to present)
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
dfMeadInflowsCRSS <- dfCRSSOutput %>% dplyr::select(Year, Month, Mead.Inflow) %>% dplyr::group_by(Year) %>% dplyr::summarize(AnnualInflow = sum(Mead.Inflow))



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
dfMeadInflowsWS <- data.frame(Year = cWSyearsColNames, Inflow = as.numeric(cWSvalColNames))
#Extract Water year from Year variable
dfMeadInflowsWS$WaterYear <- as.numeric(str_sub(dfMeadInflowsWS$Year,3,6)) + 1


#Natural flow - Not used but preserve
dfGCFDataToUse <- dfGCFlowsByYear
dfGCFDataToUse$GCFlow <- dfGCFDataToUse$GCFlow/1e6
dfGCFDataToUse$MeadInflowNat <- dfGCFDataToUse$MeadInflowNat/1e6
dfGCFDataToUse$LeeFerryNaturalFlow <- dfGCFDataToUse$LeeFerryNaturalFlow/1e6
#Rename the MeadInflowNat column to MeadInflow for later use with rbind
cColNames <- colnames(dfGCFDataToUse)
cColNames[4] <- "MeadInflow"
colnames(dfGCFDataToUse) <- cColNames

dfGCFDataToUse$Source <- 'Natural Flow'

#USGS data
#Pull in the correct columns
dfGCFDataToUse2 <- as.data.frame(dfGCFlowsUSGS[,c(1,5,6)])
#Rename the 6th column MeadInflow
dfGCFDataToUse2 <- dfGCFDataToUse2 %>% dplyr::rename(MeadInflow = MeadInflowUSGS)
#Assign the Lee Ferry Natural Flow by year
dfGCFDataToUse2 <- left_join(dfGCFDataToUse2, dfGCFDataToUse[,c("WaterYear","LeeFerryNaturalFlow")], by=c("WaterYear" = "WaterYear"))
#Sort smallest year to largest year
dfGCFDataToUse2 <- dfGCFDataToUse2[order(dfGCFDataToUse2$`WaterYear`),]
dfGCFDataToUse2$Source <- 'USGS'
#Swap the order of MeadInflow and LeeFerryNaturalFlow
dfGCFDataToUse2 <- dfGCFDataToUse2 %>% dplyr::select(WaterYear, GCFlow, LeeFerryNaturalFlow, MeadInflow, Source)

#Bind the two data sets together
dfGCFDataToUse <- rbind(dfGCFDataToUse, dfGCFDataToUse2)

#### Figure 1 - Time series

ggplot() +
  #Data after 1989
  geom_line(data = dfGCFDataToUse, aes(x=WaterYear , y=MeadInflow, color=Source, linetype=Source), size=1.5) +
  theme_bw() +
  
  scale_color_manual(values = c("Red", "Blue")) +
  scale_linetype_manual(values = c("solid","longdash")) +
  
  #Make one combined legend
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="", y="Lake Mead Inflow(MAF per year)", color="") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20))




#### Figure 2 - Plot Mead Inflow as a box-and-whiskers
#Plot as a box-and whiskers

ggplot() +
  #Data after 1989
  geom_boxplot(data = dfGCFDataToUse %>% filter(WaterYear >= 1990), aes(x=Source , y=MeadInflow, fill=Source)) +
  theme_bw() +
  
  #Data before 1990
  geom_boxplot(data = dfGCFDataToUse %>% filter(WaterYear < 1990), aes(x="Before 1990 Natural Flow" , y=MeadInflow, fill="Before 1990 Natural Flow")) +
  
  scale_x_discrete(labels = c("Natural Flow" = "Natural Flow\n(1990 to 2016)", "Before 1990 Natural Flow" = "Natural Flow\n(1905 to 1989)", "USGS" = "USGS\n(1990 to 2016)") ) +
  scale_fill_manual(values = c("Pink", "Red", "Blue")) +
  
  theme_bw() +
  
  labs(x="", y="Lake Mead Inflow\n(MAF per year)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20), 
        legend.position = "none")


#### Figure 3. Show the correlation between Mead Inflow and Lee Ferry Flow
#
ggplot() +
  #Points after 1990 in Blue and Red
  geom_point(data = dfGCFDataToUse %>% filter(WaterYear >= 1990), aes(x= LeeFerryNaturalFlow, y=MeadInflow, color=Source, shape=Source), size=6) +
  
  geom_point(data = dfGCFDataToUse %>% filter(WaterYear < 1990), aes(x= LeeFerryNaturalFlow, y=MeadInflow, color="Natural Flow pre 1990", shape="Natural Flow pre 1990"), size=6) +

  scale_shape_manual(values=c(17,16,16), breaks = c("USGS","Natural Flow","Natural Flow pre 1990"), labels  = c("USGS (after 1990)","Natural Flow (after 1990)","Natural Flow (before 1990)")) +
  
  scale_color_manual(values=c("Blue","Red","Pink"), breaks = c("USGS","Natural Flow","Natural Flow pre 1990"), labels  = c("USGS (after 1990)","Natural Flow (after 1990)","Natural Flow (before 1990)")) +
  
  #Make one combined legend
  guides(color = guide_legend("Dataset"), shape = guide_legend("Dataset")) +
  
  #facet_wrap( ~ Source) +
  labs(x="Lake Mead Inflow\n(MAF per year)", y="Grand Canyon Intervening Flows\n(MAF per year)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  
  theme_bw() +  
  theme(text = element_text(size=20))

## Show the correlation matrix
mCorr <- cor(as.data.frame(dfGCFDataToUse %>% filter(WaterYear >= 1990, Source == "Natural Flow") %>% select(LeeFerryNaturalFlow,GCFlow)))
print(paste("Correlation = ",round(mCorr[1,2],2)))


#### Figures 4 and 5. Show the sequence average plot using Salehabadi code for Natural Flow data set and USGS data

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


  


