# InflowSplit.R
#
# Shows the split of Lake Powell natural flow from 0 to 18 million acre-feet per year among Colorado River Basin accounts.
#  1. Loads data/calculations from the ColoradoRiverBasinAccounts-v5.1.xlsx file.
#  2. Plots as a stacked area plot.
#  3. Labels each area as an account.
#
# This is a beginning R-programming effort! There could be lurking bugs or basic coding errors that I am not even aware of.
# Please report bugs/feedback to me (contact info below)
#
# David E. Rosenberg
# April 26, 2022
# Utah State University
# david.rosenberg@usu.edu

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

if (!require(ggrepel)) { 
  devtools::install_github("slowkow/ggrepel")
  library(ggrepel) 
}

if (!require(stringr)) { 
  install.packages("stringr", repo="http://cran.r-project.org")
  library(stringr) 
}



# Read in Lake Powell natural flow scenarios from Excel
sExcelFile <- 'ColoradoRiverBasinAccounts-v5.1.xlsx'
# Rows are share to Accounts and some other stuff
dfInflowSplits <- read_excel(sExcelFile, sheet = "SplitInflow",  range = "A28:L46")

#Sum all inflows
cInterveneFlow <- colSums(dfInflowSplits[1:2,3:12])

#Read the headers of Lake Powell inflow
cNamesInflowSplits <- colnames(dfInflowSplits)

#Clean the headers. Turn NAs to zeros
cNamesInflowSplits <- as.numeric(cNamesInflowSplits)
cNamesInflowSplits[is.na(cNamesInflowSplits)] <- 0

#Add for Total Basin Flow
cTotalInflow <- cNamesInflowSplits[3:12] + cInterveneFlow

#Remake headers as total flow
cNamesInflowSplits[1] <- "Item"
cNamesInflowSplits[2] <- "Blank"
cNamesInflowSplits[3:12] <- cTotalInflow

colnames(dfInflowSplits) <- cNamesInflowSplits


#Subset rows that have account splits in them
dfInflowSplitsAccounts <- dfInflowSplits[12:18,]


dfInflowSplitsAccounts$AccountName <- 0

#Remove the "To " in names
#Somehow is not working on dataframe column so use loop.
for(i in (1:nrow(dfInflowSplitsAccounts))) {
  dfInflowSplitsAccounts$AccountName[i] <- substr(dfInflowSplitsAccounts$Item[i], 4, nchar(dfInflowSplitsAccounts$Item[i]))
}

#Get column names
cNamesInflowSplits <- colnames(dfInflowSplitsAccounts)

#Melt the data into 2 columns
dfInflowSplitMelt <- melt(dfInflowSplitsAccounts, id.vars = cNamesInflowSplits[13], measure.vars = cNamesInflowSplits[3:12])

#Convert the into a numeric flow value. 
dfInflowSplitMelt$Flow <- as.numeric(as.character(dfInflowSplitMelt$variable)) 

#Merge the Havasu Parker and Lower Basin into one group
#dfInflowSplitMelt$AccountName[dfInflowSplitMelt$AccountName %in% dfInflowSplitsAccounts$AccountName[7]] <- dfInflowSplitsAccounts$AccountName[2]
#dfInflowSplitsAccounts$AccountName[7] <- dfInflowSplitsAccounts$AccountName[2]


#Order the accounts by allocation priority (stack order)
dfInflowSplitMelt$AccountName <- factor(dfInflowSplitMelt$AccountName, levels = c(dfInflowSplitsAccounts$AccountName[1:5],dfInflowSplitsAccounts$AccountName[7],dfInflowSplitsAccounts$AccountName[6]) )

nColInflowSplitsAccounts <- ncol(dfInflowSplitsAccounts)

#Build a data frame for the account labels inside the plot
dfAccountLabels <- data.frame(AccountName =  dfInflowSplitsAccounts$AccountName, xPos = c(15, rep(13,2),10, rep(7,3)), maxY = (dfInflowSplitsAccounts[, nColInflowSplitsAccounts-1]))
#Set the new column name to yPos
cNames <-colnames(dfAccountLabels)
cNames[3] <- "maxY"
colnames(dfAccountLabels) <- cNames


#Order the account names by priority
dfAccountLabels$AccountName <- factor(dfAccountLabels$AccountName, levels = c(dfInflowSplitsAccounts$AccountName[1:5],dfInflowSplitsAccounts$AccountName[7],dfInflowSplitsAccounts$AccountName[6]))
#Order the AccountLabels by the factors
dfAccountLabels <- dfAccountLabels[dfAccountLabels$AccountName,]

#calculate difference between accounts
dfAccountLabels$yCum <- rev(cumsum(rev(dfAccountLabels$maxY)))
#Calculate yPos as cumulative minus 1/2 max y
dfAccountLabels$yPos <- dfAccountLabels$yCum - 0.5 * dfAccountLabels$maxY

#Custom offset for Upper Basin
dfAccountLabels$yPos[1] <- 12.5




#Get the color palettes
#Get the blue color bar
pBlues <- brewer.pal(9,"Blues")


#Plot split of inflow - Customary units only on Flow Assignment (y-axis) and Basin Natural Flow (x-axis)
ggplot(dfInflowSplitMelt, aes(x = Flow , y = value, fill=AccountName)) +
  #Area plot with lines
  geom_area(color="Black", linetype = "solid", size = 0.25) +
  
  #Fill with Blues light to dark
  scale_fill_manual(values = pBlues[2:9]) +
  
  #Label each area
  geom_text(data = dfAccountLabels, aes(label = AccountName, x = xPos, y = yPos), size = 3.5, color = "black") +
  
  #Label the vertical line
  #geom_text(aes(x=12.2, y=5.5, label="2000 to 2020 average"), angle = 90, color = "Black", size=7) +
  
  #Define scales
  scale_x_continuous(limits = c(0,18), breaks = seq(0,18,by=2), minor_breaks = seq(0,18,by=2)) +
  scale_y_continuous(breaks = seq(0,18,by=2), minor_breaks = seq(0,18,by=2)) +
 
  labs(x="Whole Basin Inflow\n(million acre-feet per year)", y="Flow Assignment\n(maf per year)") +
  theme(text = element_text(size=25), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank(), legend.position = "none")

#Convert MAF to BCM
cMAFtoBCM <- 1.23

c_xLimitsMAF <- c(0,18) #maf
c_xBreaksMAF <- seq(0,18,by=2) #maf

c_xLimitsBCM <- c_xLimitsMAF * cMAFtoBCM
c_xBreaksBCM <- round(c_xBreaksMAF * cMAFtoBCM, digits = 1)

#Plot split of inflow - Customary units of MAF on left and bottom axes. SI units of BCM on top and right axes
ggplot(dfInflowSplitMelt, aes(x = Flow , y = value, fill=AccountName)) +
  #Area plot with lines
  geom_area(color="Black", linetype = "solid", size = 0.25) +
  
  #Fill with Blues light to dark
  scale_fill_manual(values = pBlues[2:9]) +
  
  #Label each area
  geom_text(data = dfAccountLabels, aes(label = AccountName, x = xPos, y = yPos), size = 3.5, color = "black") +
  
  #Label the vertical line
  #geom_text(aes(x=12.2, y=5.5, label="2000 to 2020 average"), angle = 90, color = "Black", size=7) +
  
  #Define scales
  scale_x_continuous(limits = c_xLimitsMAF, breaks = c_xBreaksMAF, minor_breaks = c_xBreaksMAF,
                     sec.axis = sec_axis(~ ., name="Whole Basin Inflow\n(bcm per year)", breaks = c_xBreaksMAF, labels = c_xBreaksBCM)) +
  scale_y_continuous(breaks = c_xBreaksMAF, minor_breaks = c_xBreaksMAF,
                     sec.axis = sec_axis(~ ., name="Flow Assignment\n(bcm per year)", breaks = c_xBreaksMAF, labels = c_xBreaksBCM)) +
  
  labs(x="Whole Basin Inflow\n(maf per year)", y="Flow Assignment\n(maf per year)") +
  theme(text = element_text(size=25), legend.title=element_blank(), legend.text=element_text(size=18),
        legend.key = element_blank(), legend.position = "none")