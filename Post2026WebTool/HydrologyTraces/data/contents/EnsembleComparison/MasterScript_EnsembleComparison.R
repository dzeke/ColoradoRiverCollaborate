##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
###### Last modified: 1/24/2024
##################################################################################################################################################################



##################################################################################################################################################################
###### This is the master Script for evaluating hydrology scenarios. This master script calls the functions from R_Files folder.
###### To run this script:
######   1- Specify desired scenarios to compare in the "ScenarioListForAnalyzing" tab of the "HydrologyScenarios.xlsx" file.
######   2- Specify desired metrics to calculate in the "MetricsForAnalysis" tab of the "HydrologyScenarios.xlsx" file.
###### Output will be:
######   1- A PDF file in the "Results" folder.



#setwd("..")

MainInputFile = "HydrologyScenarios.xlsx"
OutputName <- "ComparisonResults.pdf" 


##################################################################################################################################################################
###### Load and install required R packages 

# Install any required packages that may be missing
# The list below is what we added to a clean R installation.  If you run this and find a package missing, adding it to this list should be the first step in working to resolve.
list.of.packages <- c("openxlsx", "matrixStats","RColorBrewer","Rfast","ggplot2", "entropy", "dplyr", "pheatmap", "viridis", "Kendall")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("openxlsx")


##################################################################################################################################################################
###### Read Desired Hydrology Scenarios From HydrologyScenarios.xlsx ##########################################################################

#### List of required scenarios (desired hydrology scenarios to compare) =========================================

ScenarioList <- read.xlsx(MainInputFile, sheet="ScenarioListForAnalysis", colNames = TRUE)
ScenarioList
nScenarios <- nrow(ScenarioList)

#### Read required hydrology scenarios ==========================================================================

## Function to read scenarios
source("./R_Files/Func_read_required_hydrology_scenarios_from_xlsx.R")
Func_read_required_hydrology_scenarios_from_xlsx(ScenarioList)


##################################################################################################################################################################
###### Read Desired Metrics From HydrologyScenarios.xlsx ##########################################################################

#### List of required metrics ===============================================================================

MetricList <- read.xlsx(MainInputFile, sheet="MetricsForAnalysis", colNames = TRUE)
MetricList
nMetrics <- nrow(MetricList)


######################################################################################################################################
######################################################################################################################################

### create a pdf and plot all the results in it
if(!file.exists('Results'))dir.create('Results')
pdf(file=paste0("./Results/",OutputName), width = 11, height = 7, onefile=TRUE) ## open an empty pdf

for(iMetric in 1:nMetrics){
  print(MetricList$Metric[iMetric])
  
  
  ###### TimeSeries ############################################################################################################
  
  if("TimeSeries" %in% MetricList$Metric[iMetric]){
    source("./R_Files/Func_EnsembleTimeSeries.R")
    Func_EnsembleTimeSeries( ScenarioList)
  }
  
  

  ###### Mean  ############################################################################################################
  
  if("Mean" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_CM_Mean.R")
    Func_Mean(ScenarioList, duration=vars[1])
  }
  
  
  ###### Median #########################################################################################################
  
  if("Median" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_CM_Median.R")
    Func_Median(ScenarioList, duration=vars[1])
  }
  
  
  ###### Max ############################################################################################################
  
  if("Max" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_CM_Max.R")
    Func_Max(ScenarioList, duration=vars[1])
  }
  
  
  ###### Min ############################################################################################################
  
  if("Min" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_CM_Min.R")
    Func_Min(ScenarioList, duration=vars[1])
  }
  
  
  ###### Std ############################################################################################################
  
  if("Std" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_CM_Std.R")
    Func_Std(ScenarioList, duration=vars[1])
  }
  
  
  ###### Skewness ######################################################################################################
  
  if("Skew" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_CM_Skewness.R")
    Func_Skewness(ScenarioList, duration=vars[1])
  }
  
  
  
  ###### Annual ACF lags 1,2,3  ########################################################################################
  
  if("ACF" %in% MetricList$Metric[iMetric]){
    source("./R_Files/Func_Metrics_ACF_Lag123.R")
    Func_ACF_Lag123(ScenarioList)
  }
  
  
  ###### Annual PACF lags 1,2,3  #######################################################################################
  
  if("PACF" %in% MetricList$Metric[iMetric]){
    source("./R_Files/Func_Metrics_PACF_Lag123.R")
    Func_PACF_Lag123(ScenarioList)
  }
  
  
  ###### Count Below Threshold  ########################################################################################
  
  #### Average Count Below Threshold ============================================================
  if("ACBT" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_AverageCountBelowThreshold.R")
    Func_AverageCountBelowThreshold (ScenarioList , Threshold=vars[1] , Duration=vars[2])
  }
  
  
  #### Moving Count Below Threshold =============================================================================
  if("MCBT" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_MovingCountBelowThreshold.R")
    for (s in 1:nScenarios){
      Func_MovingCountBelowThreshold (data= eval(parse(text = ScenarioList[s,1])) , Threshold=vars[1] , Duration=vars[2], ScenarioName=ScenarioList[s,2])
    }
  }
  
  
  ###### Count Above Threshold  ########################################################################################
  
  #### Average Count Above Threshold ============================================================
  if("ACAT" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_AverageCountAboveThreshold.R")
    Func_AverageCountAboveThreshold (ScenarioList , Threshold=vars[1] , Duration=vars[2])
  }
  
  
  #### Moving Count Above Threshold =============================================================================
  if("MCAT" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_MovingCountAboveThreshold.R")
    for (s in 1:nScenarios){
      Func_MovingCountAboveThreshold (data= eval(parse(text = ScenarioList[s,1])) , Threshold=vars[1] , Duration=vars[2], ScenarioName=ScenarioList[s,2])
    }
  }
  
  
  ### Violin plot of duration-count =============================================================
  
  if("DCBT" %in% MetricList$Metric[iMetric]){
    vars= as.numeric( MetricList[iMetric,3:dim(MetricList)[2]] )
    source("./R_Files/Func_Metrics_CountBelowThreshold_ViolinPlot.R")
    for (s in 1:nScenarios){
      print(Func_CountBelowThreshold_ViolinPlot (scenario= eval(parse(text = ScenarioList[s,1])) , Threshold=vars[1] , Duration=vars[2:length(vars)], ScenarioName=ScenarioList[s,2]) )
      ### you needed to use "print" for this function because of ggplot is in a loop
    }
  }
  
  
  
  ###### Storage-Yield-Reliability  #####################################################################################
  
  if("SYR" %in% MetricList$Metric[iMetric]){
    vars= as.numeric( MetricList[iMetric,3:dim(MetricList)[2]] )
    source("./R_Files/Func_Metrics_StorageYieldReliability.R")
    Func_StorageYieldReliability (ScenarioList,
                                  YieldFraction=seq(10, 15, by=0.5)/15,
                                  ReferenceMean=vars[1],
                                  S0=0,
                                  DesiredYieldFraction = vars[2])
    
  }


  ######  Hurst   ######################################################################################################
  
  if("Hurst" %in% MetricList$Metric[iMetric]){
    source("./R_Files/Func_Metrics_Hurst.R")
    Func_Hurst(ScenarioList, RoverSPlot=F, HurstBoxplot=T)
  }
  
  
  if("RoverS" %in% MetricList$Metric[iMetric]){
    source("./R_Files/Func_Metrics_Hurst.R")
    Func_Hurst(ScenarioList, RoverSPlot=T, HurstBoxplot=F)
  }
  
  
  ###### Duration-Severity ######################################################################################################
  
  if("DS" %in% MetricList$Metric[iMetric]){
    
    source("./R_Files/Func_Metrics_MinDurationSeverity.R")
    MinDurationSeverity (ScenarioList, MaxDuration=25)

  }
  
  
  ## MedianDurSev ----
  
  if("DSMedian" %in% MetricList$Metric[iMetric]){
  
    source("./R_Files/Func_Metrics_MedianDurationSeverity.R")
    MinAndMedianDurationSeverity (ScenarioList, MaxDuration=25, addToPlot=F, addMinDurSev=F, addMedianDurSev=T) 
    
  }
  
    

  ###### Drought Event Statistics #####################################################################################################
  
  
  #### Drought Length =================================
  if("DES_L" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_DroughtEventStats.R")
    Func_DroughtEventStats (ScenarioList,
                            Threshold = vars[1],
                            nBreakYears = vars[2],
                            LMin = vars[3],
                            LMax = vars[4],
                            DMin = vars[5],
                            IMin = vars[6],
                            PlotL = T,
                            PlotD = F,
                            PlotI = F,
                            PlotT = F)
  }
  
  #### Drought Cumulative Deficit =================================
  if("DES_D" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_DroughtEventStats.R")
    Func_DroughtEventStats (ScenarioList,
                            Threshold = vars[1],
                            nBreakYears = vars[2],
                            LMin = vars[3],
                            LMax = vars[4],
                            DMin = vars[5],
                            IMin = vars[6],
                            PlotL = F,
                            PlotD = T,
                            PlotI = F,
                            PlotT = F)
  }
  
  #### Drought Intensity =================================
  if("DES_I" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_DroughtEventStats.R")
    Func_DroughtEventStats (ScenarioList,
                            Threshold = vars[1],
                            nBreakYears = vars[2],
                            LMin = vars[3],
                            LMax = vars[4],
                            DMin = vars[5],
                            IMin = vars[6],
                            PlotL = F,
                            PlotD = F,
                            PlotI = T,
                            PlotT = F)
  }
  
  #### Drought Interarrival time =================================
  if("DES_T" %in% MetricList$Metric[iMetric]){
    vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_DroughtEventStats.R")
    Func_DroughtEventStats (ScenarioList,
                            Threshold = vars[1],
                            nBreakYears = vars[2],
                            LMin = vars[3],
                            LMax = vars[4],
                            DMin = vars[5],
                            IMin = vars[6],
                            PlotL = F,
                            PlotD = F,
                            PlotI = F,
                            PlotT = T)
  }
  
  
  
  ###### Normalized Mutual Information #####################################################################################################
  
  if("NMI" %in% MetricList$Metric[iMetric]){
    # vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
    source("./R_Files/Func_Metrics_MutualInfo.R")
    Func_MutualInfo (ScenarioList)
  }
  
  

 ######################################################################################### 
  
} ### End of loop over the metric list
  
dev.off()  ### Close pdf



############################################################################################################################################################
###### Heatmap and Classification ##########################################################################################################

OutputName2 <- "Heatmap.pdf"
pdf(file=paste0("./Results/",OutputName2), width = 13, height = 9, onefile=TRUE) ## open an empty pdf

source("./R_Files/Func_Heatmap.R")

Func_Heatmap(ScenarioList, Quartile="Median", SYRReliability="Q90", PlotTitle="")

dev.off()



