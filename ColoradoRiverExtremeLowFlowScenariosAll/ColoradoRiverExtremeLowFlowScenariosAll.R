####################
#     ColoradoRiverExtremeLowFlowScenarios.R
#
# This project compiles and compares scenarios of extreme low Coloardo River flows used in 6 different prior studies.
# The purpose is to compare the inflow scenarios across the studies that used river flow as an operations criteria.
#
# The data for this comparision are located in the Excel file **ColoradoRiverExtremeLowFlowFlowScenarios.xlsx**.
#
# A line segment plot shows the range of extreme low flows used in each study. The figure additionally shows the strategy
# used to stabilize reservoir storage for each study.

# The studies are:
#  A. Wang, J., and Rosenberg, D. E. (2023). "Adapting Colorado River Basin Depletions to Available Water to Live within Our Means." Journal of Water Resources Planning and Management, 149(7), 04023026. https://doi.org/10.1061/JWRMD5.WRENG-5555.
#  B. Rosenberg, D. E. (2024a). "Lessons from immersive online collaborative modeling to discuss more adaptive reservoir operations." Journal of Water Resources Planning and Management, 150(7). https://doi.org/10.1061/JWRMD5.WRENG-5893.
#  C. Rosenberg, D. E. (2022). "Adapt Lake Mead Releases to Inflow to Give Managers More Flexibility to Slow Reservoir Drawdown." Journal of Water Resources Planning and Management, 148(10), 02522006. https://doi.org/10.1061/(ASCE)WR.1943-5452.0001592.
#  D. Abualqumboz, M., Chamberlain, B., and Rosenberg, D. (2024). "Adaptively Managing Lake Powell Releases to Respond to Reservoir Inflow and Evaporation." Utah State University Digital Commons. https://digitalcommons.usu.edu/cee_stures/12/.
#  E. Rosenberg, D. E. (2024b). "Reclamation Web Tool - Minimum Glen Canyon Dam Annual Release to protect Lake Powell Minimum Power Pool." Github, https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/Post2026WebTool [Accessed on: July 3, 2025].
#  F. Myers, A. (2025). "Immersive Modeling for Lake Mead". https://github.com/Anabelle374/ImmersiveModelLakeMead"
#
#
#     David E. Rosenberg
#     August 2, 2025
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




### Read in the Extreme Flow values
# Excel version
sExtremeFlowFile <- 'ColoradoRiverExtremeLowFlowScenarios.xlsx'
dfExtremeFlows <- read_excel(sExtremeFlowFile, sheet = "ExtremeFlows")

# # CSV version
# sExtremeFlowFile <- 'ColoradoRiverExtremeLowFlowScenarios.csv'
# dfExtremeFlows <- read.csv(file = sExtremeFlowFile, header = FALSE, sep = ",", quote = "\"",
#          dec = ".", fill = TRUE, comment.char = "#")

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

  


