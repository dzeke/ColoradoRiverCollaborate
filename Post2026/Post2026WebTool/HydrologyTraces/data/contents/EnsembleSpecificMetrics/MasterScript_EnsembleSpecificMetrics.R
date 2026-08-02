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
######   1- A PDF file for each ensemble stored in the "Results" folder.



#setwd("..")

MainInputFile = "HydrologyScenarios.xlsx"


##################################################################################################################################################################
###### Load and install required R packages 

# Install any required packages that may be missing
# The list below is what we added to a clean R installation.  If you run this and find a package missing, adding it to this list should be the first step in working to resolve.
list.of.packages <- c("openxlsx", "matrixStats","RColorBrewer","Rfast","ggplot2", "entropy", "dplyr", "pheatmap", "gridBase", "grid", "Kendall", "viridis")
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
##### Loop over each ensemble one by one and create one pdf for each ensemble separately
##### 

ScenarioFullList <- ScenarioList

for (s in 1:nScenarios){
  
  ### pick one scenario from the ScenarioList
  ScenarioList <- ScenarioFullList[s,]
  print (paste0(ScenarioList[2], " .................................."))
  

  OutputName <- gsub("%", "",  paste0(ScenarioList[2], ".pdf"))  ## ScenarioList$Name_On_Plots  and remove % from RCP names


  ### create a pdf and plot all the results in it
  if(!file.exists('Results'))dir.create('Results')
  pdf(file=paste0("./Results/",OutputName), width = 8.5, height = 11, onefile=TRUE) ## open an empty pdf

  ######################################################################################################################################
  ###### 1- First page of the pdf (for each ensemble) ############################################################################################################

  #### Write the cover sheet ==============================================================

  scenario <- eval(parse(text = ScenarioList[1]))
  nTraces <- ncol(scenario[,-1])
  StartYr <- min(scenario[,1])
  EndYr <- max(scenario[,1])

  par(mfrow=c(2,1), oma=c(3,1,1,1))


  if (StartYr==1){  ## for ensembles that are not for a specific time in the future (e.g. ISM ensembles)

    par(mar=c(4, 4, 4, 4))
    plot(1, type="n", xaxt="n", yaxt="n", xlab="", ylab="", col="white")
    box(col="white", lwd=3)
    title (main=paste0( " Attributes of Streamflow Ensembles in Colorado River Basin",
                        "\n ---------------------------------------------------",
                        "\n Ensemble:   ", ScenarioList[2],
                        "\n Number of Realizations:   ", nTraces,
                        "\n Planning Period:   Next ", EndYr, " Years"),
           cex.main=1.3,
           line=-10,
           adj = 0.5)

  } else {  ## for ensembles that are for a specific time in the future (e.g. CMIP ensembles)

    par(mar=c(4, 4, 4, 4))
    plot(1, type="n", xaxt="n", yaxt="n", xlab="", ylab="", col="white")
    box(col="white", lwd=3)
    title (main=paste0( " Attributes of Streamflow Ensembles in Colorado River Basin",
                        "\n ---------------------------------------------------",
                        "\n Ensemble:   ", ScenarioList[2],
                        "\n Number of Realizations:   ", nTraces,
                        "\n Planning Period:   ", StartYr, "-", EndYr),
           cex.main=1.3,
           line=-10,
           adj = 0.5)

  }


  ##### Plot Ensemble Time Series ==========================================================

  source("./R_Files/Func_EnsembleTimeSeries.R")
  Func_EnsembleTimeSeries(ScenarioList, ScenarioName=ScenarioList[2])


  ######################################################################################################################################
  ###### 2- Second page (common metrics, ACF, DES, ACBT, Hurst) ############################################################################################################

  ### Plot multiple figures of metrics along with each other ###

  par(mfrow=c(4,3), oma=c(2,1,2,1)) ## 4x3=12 plots
  # par(mfrow=c(1,1))
  SubFigLabel <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)", "(j)", "(k)", "(l)")
  iSubFig <- 0

  iPage2Metrics <- c(2:12, 15)
  
  for(iMetric in iPage2Metrics){  ## 1:nMetrics
    
    # if (MetricList$Metric[iMetric]=="TimeSeries"){next}

    # par(mar=c(6, 4, 3, 8) + 0.1 , mgp=c(2.7, 1, 0) )
    par(mar=c(1, 4.5, 4, 7) + 0.1 , mgp=c(2.7, 1, 0) )
    
    ###### Mean  ############################################################################################################


    if("Mean" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
      source("./R_Files/Func_Metrics_CM_Mean.R")
      Func_Mean(ScenarioList, duration=vars[1])
    }


      
    ###### Median #########################################################################################################

    if("Median" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
      source("./R_Files/Func_Metrics_CM_Median.R")
      Func_Median(ScenarioList, duration=vars[1])
    }

    
    ###### Max ############################################################################################################
    
    
    if("Max" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
      source("./R_Files/Func_Metrics_CM_Max.R")
      Func_Max(ScenarioList, duration=vars[1])
    }
    

    ###### Min ############################################################################################################

    
    if("Min" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
      source("./R_Files/Func_Metrics_CM_Min.R")
      Func_Min(ScenarioList, duration=vars[1])
    }
    

    ###### Std ############################################################################################################

    
    if("Std" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
      source("./R_Files/Func_Metrics_CM_Std.R")
      Func_Std(ScenarioList, duration=vars[1])
    }


    ###### Skewness ######################################################################################################
    
    
    if("Skew" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
      source("./R_Files/Func_Metrics_CM_Skewness.R")
      Func_Skewness(ScenarioList, duration=vars[1])
    }
    

    
    ###### Annual ACF lags 1,2,3  ########################################################################################
    

    if("ACF" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      source("./R_Files/Func_Metrics_ACF_Lag123.R")
      Func_ACF_Lag123(ScenarioList)
    }

    
    ###### Annual PACF lags 1,2,3  #######################################################################################

    if("PACF" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      source("./R_Files/Func_Metrics_PACF_Lag123.R")
      Func_PACF_Lag123(ScenarioList)
    }

    
    ###### Count Below Threshold  ########################################################################################
    
    #### Average Count Below Threshold ============================================================
    if("ACBT" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
      source("./R_Files/Func_Metrics_AverageCountBelowThreshold.R")
      Func_AverageCountBelowThreshold (ScenarioList , Threshold=vars[1] , Duration=vars[2])
    }
    
  
    ###### Count Above Threshold  ########################################################################################
    
    #### Average Count Above Threshold ============================================================
    if("ACAT" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
      source("./R_Files/Func_Metrics_AverageCountAboveThreshold.R")
      Func_AverageCountAboveThreshold (ScenarioList , Threshold=vars[1] , Duration=vars[2])
    }
    
    
    ###### Drought Event Statistics #####################################################################################################
    
    
    #### Drought Length =================================
    if("DES_L" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
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
      print(MetricList$Metric[iMetric])
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
      print(MetricList$Metric[iMetric])
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
      print(MetricList$Metric[iMetric])
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
      print(MetricList$Metric[iMetric])
      source("./R_Files/Func_Metrics_MutualInfo.R")
      Func_MutualInfo (ScenarioList)
    }
    
    
    
    ######################################################################################### 

    iSubFig <- iSubFig+1
    
    legend(x=(par("usr")[1]),
           y=(par("usr")[4]),
           legend=SubFigLabel[iSubFig],
           bty="n", xjust=1.3, yjust = -0.3, cex=1.9, xpd=T)
    
    box(col = "gray", which = "figure")
    
    # iSubFig <- iSubFig+1
    
    # text(SubFigLabel[iSubFig],
    #      x=((par("usr")[1]) *1),
    #      y=((par("usr")[4]) *(1.2)),
    #      xpd=T,
    #      pos=4,
    #      offset=-1,
    #      # adj=c(0,0),
    #      cex=1.7,
    #      font=2,
    #      col="green") 
    # 
    # print(paste0( par("usr")[1], " - " , par("usr")[4] ))
    

    
  } ### End of loop over the metric list in page 2


  
  
  ######################################################################################################################################
  ###### 3A- third page (Hurst & R/S, SYR) ############################################################################################################


  layout(matrix(c(1,2, 3,4 ), 2, 2, byrow = TRUE),
         widths=c(1,1), heights=c(1,2))

  # layout.show(4)

  for(iMetric in 1:nMetrics){

    ######  Hurst   ######################################################################################################

    # par(mar=c(1, 6, 7, 7) + 0.1 , mgp=c(2.7, 1, 0) )
    par(mar=c(1, 6, 6, 9) + 0.1 , mgp=c(2.7, 1, 0) )
    
    if("Hurst" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      source("./R_Files/Func_Metrics_Hurst.R")
      Func_Hurst(ScenarioList, RoverSPlot=F, HurstBoxplot=T)
    }


    # par(mar=c(1, 5, 7, 4) + 0.1 , mgp=c(2.7, 1, 0) )
    par(mar=c(2, 3.5, 6, 2) + 0.1 , mgp=c(2.7, 1, 0) )
    
    if("Hurst" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      source("./R_Files/Func_Metrics_Hurst.R")
      Func_Hurst(ScenarioList, RoverSPlot=T, HurstBoxplot=F)
    }


    ###### Storage-Yield-Reliability  #####################################################################################

    
    # par(mar=c(6, 3, 20, 5) + 0.1 , mgp=c(2.7, 1, 0) )
    
    if("SYR" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      vars= as.numeric( MetricList[iMetric,3:dim(MetricList)[2]] )
      source("./R_Files/Func_Metrics_StorageYieldReliability.R")
      Func_StorageYieldReliability (ScenarioList,
                                    YieldFraction=seq(10, 15, by=0.5)/15,
                                    ReferenceMean=vars[1],
                                    S0=0,
                                    DesiredYieldFraction = vars[2])

    }

  }

    ######################################################################################################################################
  ###### 3B- third page (MCBT, DCBT) ############################################################################################################
  
  par(mfrow=c(2,1) , oma=c(0,1,2,1))
  
  for(iMetric in 1:nMetrics){

    #### Moving Count Below Threshold =============================================================================
    if("MCBT" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
      source("./R_Files/Func_Metrics_MovingCountBelowThreshold.R")
      Func_MovingCountBelowThreshold (data= scenario , Threshold=vars[1] , Duration=vars[2], ScenarioName=ScenarioList[2])
    }

    #### Moving Count Above Threshold =============================================================================
    if("MCAT" %in% MetricList$Metric[iMetric]){
      print(MetricList$Metric[iMetric])
      vars=as.numeric(MetricList[iMetric,3:dim(MetricList)[2]])
      source("./R_Files/Func_Metrics_MovingCountAboveThreshold.R")
      Func_MovingCountAboveThreshold (data= scenario , Threshold=vars[1] , Duration=vars[2], ScenarioName=ScenarioList[2])
    }
    
  }
    

  ######################################################################################################################################
  ###### 4- Forth page (DS, SYR) ############################################################################################################
  
    layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE),
           widths=c(1,1), heights=c(1,1))
    
  # par(mfrow=c(2,1) , oma=c(0,1,2,1))
      
      for(iMetric in 1:nMetrics){
        
        
        ###### Duration-Severity ######################################################################################################
        
        
        if("DS" %in% MetricList$Metric[iMetric]){
          print(MetricList$Metric[iMetric])
          source("./R_Files/Func_Metrics_MinDurationSeverity.R")
          MinDurationSeverity (ScenarioList, MaxDuration=25)
          
        }
        
        
        ### Violin plot of duration-count =============================================================

        if("DCBT" %in% MetricList$Metric[iMetric]){

          ### to plot ggplot along with base plot in one page -----
          library(gridBase)
          library(grid)
          ## the last one is the current plot
          plot.new()              ## suggested by @Josh
          vps <- baseViewports()
          pushViewport(vps$figure) ##   I am in the space of the ggplot
          vp1 <-plotViewport(c(1.8,1,0,1)) ## create new vp with margins, you play with this values

          ### DCBT ------
          print(MetricList$Metric[iMetric])
          vars= as.numeric( MetricList[iMetric,3:dim(MetricList)[2]] )
          source("./R_Files/Func_Metrics_CountBelowThreshold_ViolinPlot.R")
          print(Func_CountBelowThreshold_ViolinPlot (scenario= scenario , Threshold=vars[1] , Duration=vars[2:length(vars)], ScenarioName=ScenarioList[2]),
                vp = vp1)
            ### you needed to use "print" for this function because of ggplot is in a loop
        }

  } 
  
  
  ######################################################################################################################################
  ###### End of writing on pdf ############################################################################################################
  
  dev.off()  ### Close pdf
  
  
}  ### end of loop over scenarios (one pdf for each scenario)




