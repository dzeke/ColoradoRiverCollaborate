####################
#     PowellMonthlyRelease.R
#
#     The purpose of this code file is to load the Lake Powell Monthly release schedule from the Colorado River Simulation System
#     And plot the data by month of the year and annual release volume.
#
#     We want to visualize the high release months corresponding to maximum energy generation.
#
#     The file with the CRSS data is Powell-MonthlyReleaseSchedule.txt
#     This data comes form the Powell.MonthlyReleaseTable slot in CRSS.
#    
#
#     David E. Rosenberg
#     September 11, 2023
#     david.rosenberg@usu.edu

#
#####


rm(list = ls())  #Clear history

#Load packages in one go
  #List of packages
  load.lib <- c("tidyverse", "readxl", "RColorBrewer", "dplyr", "expss", "reshape2", "pracma", "lubridate", "directlabels", "plyr", "stringr", "ggplot2")
# Then we select only the packages that aren't currently installed.
  install.lib <- load.lib[!load.lib %in% installed.packages()]
# And finally we install the missing packages, including their dependency.
  for(lib in install.lib) install.packages(lib,dependencies=TRUE)
  # After the installation process completes, we load all packages.
  sapply(load.lib,require,character=TRUE)




### Read in the Monthly Release datas
# Note used in calc of Mead Inflow. But keep anyway for backward compatibility
sCRSSDataFile <- 'Powell-MonthlyReleaseSchedule.txt'
dfMonthlyRelease <- read.csv(file = sCRSSDataFile, header = FALSE, sep = " ", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "#")

# Rename the columns/variables to the annual flow amount

nAnnualVolumes <- c(7, 7.48, 8.23, seq(9.0, 17.5, 0.5), 18, 20, 30, 50, 75,100)
colnames(dfMonthlyRelease) <- as.character(nAnnualVolumes)
#Remove the 100 column
#dfMonthlyRelease <- subset(dfMonthlyRelease, select = -c("100"))

#Add column for numeric month
dfMonthlyRelease$Month <- c(10, 11, 12, seq(1,9,1))
#Add column for text month
dfMonthlyRelease$MonthTxt <- month.abb[dfMonthlyRelease$Month]

#Reshape wide format (annual releases as columns) to long
dfMonthlyReleaseLong <- reshape(data = dfMonthlyRelease, 
                                idvar = c("Month","MonthTxt"), 
                                varying = as.character(nAnnualVolumes), 
                                v.names = "Monthly Release", 
                                timevar = "Annual Release Volume",
                                times = as.character(nAnnualVolumes),
                                #new.row.names = 1:(length(nAnnualVolumes)*12)
                                direction="long")

#Convert Annual Release Volume to numeric
dfMonthlyReleaseLong$`Annual Release Volume` <- as.numeric(dfMonthlyReleaseLong$`Annual Release Volume`)
dfMonthlyReleaseLong$`Annual Release VolumeMAF` <- as.factor(as.numeric(dfMonthlyReleaseLong$`Annual Release Volume`))

#Plot by month


cColorsToPlot <- colorRampPalette((brewer.pal(9, "Blues")))(length(nAnnualVolumes))

#### Figure 1 - Lines as 

ggplot() +
  #Data after 1989
  geom_line(data = dfMonthlyReleaseLong %>% filter(`Annual Release Volume` < 20), aes(x=Month , y=`Monthly Release`/1e6, color=`Annual Release VolumeMAF`), size=1.5) +
  theme_bw() +
  
  scale_color_manual(values = cColorsToPlot) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  scale_x_continuous(1, 12, breaks = seq(1,12,1), labels = month.abb[seq(1,12,1)]) +
  
  #Make one combined legend
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="Month", y="Release Volume\n(MAF per mo nth)", color="Annual Release Volume") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20), legend.title = element_text("Annual Release\nMAF"), legend.text=element_text(size=14), axis.text.x = element_text(size=12))

  


