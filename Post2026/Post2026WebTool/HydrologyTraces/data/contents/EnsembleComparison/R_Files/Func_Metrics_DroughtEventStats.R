##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


Func_DroughtEventStats <- function(ScenarioList,
                                   Threshold = 14.65,
                                   nBreakYears = 1,
                                   LMin = 2,
                                   LMax = 9999,
                                   DMin = 0,
                                   IMin = 0,
                                   PlotL = T,
                                   PlotD = T,
                                   PlotI = T,
                                   PlotT = T)  {
  

  
  nScenarios <- nrow(ScenarioList)
  ScenarioAveDroughtLength <- vector("list", nScenarios)
  ScenarioAveDroughtDeficit <- vector("list", nScenarios)
  ScenarioAveDroughtIntensity <- vector("list", nScenarios)
  ScenarioAveDroughtInterarrivalTime <- vector("list", nScenarios)
  
  for (s in 1:nScenarios){ ### Loop over scenarios ================================================================
    
    Scenario <- eval(parse(text = ScenarioList[s,1]))
    nTrace <- ncol(Scenario) -1
    nYr <- nrow(Scenario)
    
    
    #### Specify below threshold years (0 or 1) and calculate Deficit ----------------------
    
    ## create empty dataframes
    IsDrought <- Scenario
    Deficit <- Scenario
    
    ## Droughts and Deficits
    for (j in 1:nTrace){
      for (i in 1:nYr){
        if (Scenario[i,(j+1)]<Threshold){
          IsDrought[i,(j+1)] <- 1
          Deficit[i,(j+1)] <- Threshold - Scenario[i,(j+1)]
        }else{
          IsDrought[i,(j+1)] <- 0  
          Deficit[i,(j+1)] <- 0
        }
      }
    }
    
    
    ###### Calculate Drought Event Statistics -------------------------------------
    
    DrStatAllTraces <- data.frame("Trace"=1:nTrace, "AveLength"=NA, "AveDeficit"=NA, "AveIntensity"=NA, "AveInterarrivalTime"=NA)
    
    for (t in 1:nTrace){
      
      ### row numbers of years with below threshold flow
      DrYrIndices <- which(IsDrought[,(t+1)]==1)
      
      ### Group below threshold years into Drought periods
      DrYrGroups <- split(DrYrIndices, cumsum( c(1, diff(DrYrIndices) > (1+nBreakYears) ) ) )  ## Droughts <- split(DrYrIndices, cumsum( c(1, diff(DrYrIndices) != 1) ) )
      DrYrGroupsLength <- rep(NA, length(DrYrGroups))
      for(g in 1:length(DrYrGroups)){
        DrYrGroupsLength[g] <- DrYrGroups[[g]][lengths(DrYrGroups)[[g]]] - DrYrGroups[[g]][1] +1
      }
      
      ### Select droughts with length specified as inputs
      Droughts <- DrYrGroups[which( DrYrGroupsLength>=LMin & DrYrGroupsLength<=LMax)]
      
      # ### To check
      # Droughts
      # barplot(names.arg=IsDrought[,1], height=IsDrought[,(t+1)], cex.names=0.5)
      
      
      ### an empty dataframe to store drought stats
      nDr <- length(Droughts)
      DrStat <- data.frame("Id"=1:nDr, "Length"=NA, "Deficit"=NA, "Intensity"=NA, "IntArrTime"=NA)
      
      ### Calculate Duration, Deficit, and Intensity
      if(nDr>0){
        for (d in 1:nDr){
          DrStat$Length[d] <- Droughts[[d]][lengths(Droughts)[[d]]] - Droughts[[d]][1] +1  ## it also counts the break wet years (nBreakYears) during the drought.
          DrStat$Deficit[d] <- sum( Deficit[ Droughts[[d]], (t+1)] )
          DrStat$Intensity[d] <-  DrStat$Deficit[d] / DrStat$Length[d]
        }
      }
      
      
      ### Inter Arrival Time of droughts (the time between the start of two successive droughts)
      if(nDr>1){
        for (d in 1:(nDr-1)){
          DrStat$IntArrTime[d] <- Droughts[[d+1]][1] - Droughts[[d]][1]
        }
      }
      
      
      ### Select those droughts with specified Length, Deficit, and Intensity
      DrStatSubset <- subset(DrStat, subset = DrStat$Deficit>=DMin & DrStat$Intensity>=IMin)
      
      ### Average of the drought event statistics for each single trace ==> Scenario Drought Stats
      DrStatAllTraces$AveLength[t] <- mean(DrStatSubset$Length)
      DrStatAllTraces$AveDeficit[t] <- mean(DrStatSubset$Deficit)
      DrStatAllTraces$AveIntensity[t] <- mean(DrStatSubset$Intensity)
      DrStatAllTraces$AveInterarrivalTime[t] <- mean(DrStatSubset$IntArrTime, na.rm=TRUE)
      
    }
    
    
    ScenarioAveDroughtLength[[s]] <- DrStatAllTraces$AveLength
    ScenarioAveDroughtDeficit[[s]] <- DrStatAllTraces$AveDeficit
    ScenarioAveDroughtIntensity[[s]] <- DrStatAllTraces$AveIntensity
    ScenarioAveDroughtInterarrivalTime[[s]] <- DrStatAllTraces$AveInterarrivalTime
    
    
  } ### End of loop over scenarios ================================================================
  
  
  #####################################################################################################
  ##### Historical Values ========================================
  
  ## read Hist data ------
  
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  yr1 <- min(Hist[,1])
  yr2 <- max(Hist[,1])

  
  ## Drought event stats for historical flow
  
  Scenario <- Hist
  nTrace <- ncol(Scenario) -1
  nYr <- nrow(Scenario)
  
  
  #### Specify below threshold years (0 or 1) and calculate Deficit ----------------------
  
  ## create empty dataframes
  IsDrought <- Scenario
  Deficit <- Scenario
  
  ## Droughts and Deficits
  for (j in 1:nTrace){
    for (i in 1:nYr){
      if (Scenario[i,(j+1)]<Threshold){
        IsDrought[i,(j+1)] <- 1
        Deficit[i,(j+1)] <- Threshold - Scenario[i,(j+1)]
      }else{
        IsDrought[i,(j+1)] <- 0  
        Deficit[i,(j+1)] <- 0
      }
    }
  }
  
  
  ###### Calculate Drought Event Statistics -------------------------------------
  
  DrStatHist <- data.frame("Trace"=1:nTrace, "AveLength"=NA, "AveDeficit"=NA, "AveIntensity"=NA, "AveInterarrivalTime"=NA)
  
  for (t in 1:nTrace){
    
    ### row numbers of years with below threshold flow
    DrYrIndices <- which(IsDrought[,(t+1)]==1)
    
    ### Group below threshold years into Drought periods
    DrYrGroups <- split(DrYrIndices, cumsum( c(1, diff(DrYrIndices) > (1+nBreakYears) ) ) )  ## Droughts <- split(DrYrIndices, cumsum( c(1, diff(DrYrIndices) != 1) ) )
    DrYrGroupsLength <- rep(NA, length(DrYrGroups))
    for(g in 1:length(DrYrGroups)){
      DrYrGroupsLength[g] <- DrYrGroups[[g]][lengths(DrYrGroups)[[g]]] - DrYrGroups[[g]][1] +1
    }
    
    ### Select droughts with length specified as inputs
    Droughts <- DrYrGroups[which( DrYrGroupsLength>=LMin & DrYrGroupsLength<=LMax)]
    
    # ### To check
    # Droughts
    # barplot(names.arg=IsDrought[,1], height=IsDrought[,(t+1)], cex.names=0.5)
    
    
    ### an empty dataframe to store drought stats
    nDr <- length(Droughts)
    DrStat <- data.frame("Id"=1:nDr, "Length"=NA, "Deficit"=NA, "Intensity"=NA, "IntArrTime"=NA)
    
    ### Calculate Duration, Deficit, and Intensity
    for (d in 1:nDr){
      DrStat$Length[d] <- Droughts[[d]][lengths(Droughts)[[d]]] - Droughts[[d]][1] +1  ## it also counts the break wet years (nBreakYears) during the drought.
      DrStat$Deficit[d] <- sum( Deficit[ Droughts[[d]], (t+1)] )
      DrStat$Intensity[d] <-  DrStat$Deficit[d] / DrStat$Length[d]
    }
    
    ### Inter Arrival Time of droughts (the time between the start of two successive droughts)
    if(nDr>1){
      for (d in 1:(nDr-1)){
        DrStat$IntArrTime[d] <- Droughts[[d+1]][1] - Droughts[[d]][1]
      }
    }
    
    
    ### Select those droughts with specified Length, Deficit, and Intensity
    DrStatSubset <- subset(DrStat, subset = DrStat$Deficit>=DMin & DrStat$Intensity>=IMin)
    
    ### Average of the drought event statistics for each single trace ==> Scenario Drought Stats
    DrStatHist$AveLength[t] <- mean(DrStatSubset$Length)
    DrStatHist$AveDeficit[t] <- mean(DrStatSubset$Deficit)
    DrStatHist$AveIntensity[t] <- mean(DrStatSubset$Intensity)
    DrStatHist$AveInterarrivalTime[t] <- mean(DrStatSubset$IntArrTime, na.rm=TRUE)
    
  }
  
  
  
  
  
  
  #################################################################################################
  ##### plots #################################################################
  
  par(mar=c(6, 4.5, 3, 2) + 0.1 , mgp=c(3, 1, 0) )
  
  
  ### Drought Length ##################################################################################
  
  if (PlotL==T){
    
    ylim1=0
    ylim2=50
    dy=1
    
    boxplot(ScenarioAveDroughtLength, 
            names=NA, 
            las=1, 
            cex.axis=1.2, 
            ylab="Drought length (yrs)", 
            cex.lab=1.3,
            cex=0.5)
    
    abline(h=seq(ylim1, ylim2, dy), col="grey50", lty="dotted")
    
    boxplot(add= T, 
            ScenarioAveDroughtLength, 
            names=NA, 
            las=1, 
            cex.axis=1.2, 
            ylab="Drought length (yrs)", 
            cex.lab=1.3,
            cex=0.5)
    
    axis(2, at=seq(ylim1,ylim2, dy) , labels=NA, tck=0.015)
    axis(4, labels=NA, tck=-0.02)
    axis(4, at=seq(ylim1,ylim2, dy) , labels=NA, tck=0.015)
    title("Average Drought Length", line=1.7)
    text(x=(par("usr")[1] + par("usr")[2])/2,
         y=(par("usr")[4]),
         xpd=T,
         adj=c(0.5,-0.8),
         cex=1,
         paste0("Flow threshold = ", Threshold, " (maf/yr),    nWetYr= ", nBreakYears, ",   LMin = ", LMin, " (yrs),   LMax = ", LMax, " (yrs),   D0 = ", DMin, " (maf),   I0 = ", IMin, " (maf/yr)"))
    
    text(x=c(1:nScenarios), y=(par("usr")[3]), adj=c(1,2.5), labels=ScenarioList[,2], cex=0.9, srt=30,  xpd=T)
    abline(h= DrStatHist$AveLength, col="red", lwd=1.5)
    
    legend("topleft",
           legend=c("Historical Drought Length  " ),
           cex=0.9,
           col="red",
           lwd=1.5, lty=1,
           inset=c(0.02, 0.03),
           bty = "o",
           bg = "white",
           box.col = "grey",
           y.intersp=1.5,  ## vertical distance between two symbols
           x.intersp=1)
    
    ###### Heatmap values ======================================================================
    
    ###  Create an empty dataframe to get the final values that we need for heatmap 
    HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=9))
    colnames(HeatmapValues) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
    HeatmapValues$Scenario <- ScenarioList[,2]
    
    ## Store the values needed for the final heatmap 
    for (s in 1:nScenarios){
      HeatmapValues$Min[s] <- min(ScenarioAveDroughtLength[[s]])
      HeatmapValues$Q10[s] <- quantile(ScenarioAveDroughtLength[[s]], probs=0.10, na.rm = T)
      HeatmapValues$Q25[s] <- quantile(ScenarioAveDroughtLength[[s]], probs=0.25, na.rm = T)
      HeatmapValues$Median[s] <- median(ScenarioAveDroughtLength[[s]], na.rm = T)
      HeatmapValues$Q75[s] <- quantile(ScenarioAveDroughtLength[[s]], probs=0.75, na.rm = T)
      HeatmapValues$Q90[s] <- quantile(ScenarioAveDroughtLength[[s]], probs=0.90, na.rm = T)
      HeatmapValues$Max[s] <- max(ScenarioAveDroughtLength[[s]])
    }
    
    HeatmapValues$Hist <- DrStatHist$AveLength
    
    ##### add the heatmap dataframe output to Global Environment ==============================================
    
    assign("HM_DES_L", HeatmapValues, .GlobalEnv)
    
  }
  
  
  ### Deficit ##################################################################################
  
  if (PlotD==T){
    
    ylim1=0
    ylim2=500
    dy=10
    
    boxplot(ScenarioAveDroughtDeficit, 
            names=NA, 
            las=1, 
            cex.axis=1.2, 
            ylab="Deficit (maf)", 
            cex.lab=1.3,
            cex=0.5)
    
    abline(h=seq(ylim1, ylim2, dy), col="grey50", lty="dotted")
    
    boxplot(add=T,
            ScenarioAveDroughtDeficit, 
            names=NA, 
            las=1, 
            cex.axis=1.2, 
            ylab="Deficit (maf)", 
            cex.lab=1.3,
            cex=0.5)
    
    axis(2, at=seq(ylim1,ylim2, dy) , labels=NA, tck=0.015)
    axis(4, labels=NA, tck=-0.02)
    axis(4, at=seq(ylim1,ylim2, dy) , labels=NA, tck=0.015)
    title("Average Cumulative Deficit", line=1.7)
    text(x=(par("usr")[1] + par("usr")[2])/2,
         y=(par("usr")[4]),
         xpd=T,
         adj=c(0.5,-0.8),
         cex=1,
         paste0("Flow threshold = ", Threshold, " (maf/yr),    nWetYr= ", nBreakYears, ",   LMin = ", LMin, " (yrs),   LMax = ", LMax, " (yrs),   D0 = ", DMin, " (maf),   I0 = ", IMin, " (maf/yr)"))
    
    text(x=c(1:nScenarios), y=(par("usr")[3]), adj=c(1,2.5), labels=ScenarioList[,2], cex=0.9, srt=30,  xpd=T)
    abline(h= DrStatHist$AveDeficit, col="red", lwd=1.5)
    
    legend("topleft",
           legend=c("Historical Deficit  " ),
           cex=0.9,
           col="red",
           lwd=1.5, lty=1,
           inset=c(0.02, 0.03),
           bty = "o",
           bg = "white",
           box.col = "grey",
           y.intersp=1.5,  ## vertical distance between two symbols
           x.intersp=1)
    
    
    ###### Heatmap values ======================================================================
    
    ###  Create an empty dataframe to get the final values that we need for heatmap 
    HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=9))
    colnames(HeatmapValues) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
    HeatmapValues$Scenario <- ScenarioList[,2]
    
    ## Store the values needed for the final heatmap 
    for (s in 1:nScenarios){
      HeatmapValues$Min[s] <- min(ScenarioAveDroughtDeficit[[s]])
      HeatmapValues$Q10[s] <- quantile(ScenarioAveDroughtDeficit[[s]], probs=0.10, na.rm = T)
      HeatmapValues$Q25[s] <- quantile(ScenarioAveDroughtDeficit[[s]], probs=0.25, na.rm = T)
      HeatmapValues$Median[s] <- median(ScenarioAveDroughtDeficit[[s]], na.rm = T)
      HeatmapValues$Q75[s] <- quantile(ScenarioAveDroughtDeficit[[s]], probs=0.75, na.rm = T)
      HeatmapValues$Q90[s] <- quantile(ScenarioAveDroughtDeficit[[s]], probs=0.90, na.rm = T)
      HeatmapValues$Max[s] <- max(ScenarioAveDroughtDeficit[[s]])
    }
    
    HeatmapValues$Hist <- DrStatHist$AveDeficit
    
    ##### add the heatmap dataframe output to Global Environment ==============================================
    
    assign("HM_DES_D", HeatmapValues, .GlobalEnv)
    
    
  }
  
  
  ### Intensity ##################################################################################
  
  if (PlotI==T){
    
    ylim1=0
    ylim2=20
    dy=0.5
    
    boxplot(ScenarioAveDroughtIntensity, 
            names=NA, 
            las=1, 
            cex.axis=1.2, 
            ylab="Intensity (maf/yr)", 
            cex.lab=1.3,
            cex=0.5)
    
    abline(h=seq(ylim1, ylim2, dy), col="grey50", lty="dotted")
    
    boxplot(add=T,
            ScenarioAveDroughtIntensity, 
            names=NA, 
            las=1, 
            cex.axis=1.2, 
            ylab="Intensity (maf/yr)", 
            cex.lab=1.3,
            cex=0.5)
    
    axis(2, at=seq(ylim1,ylim2, dy) , labels=NA, tck=0.015)
    axis(4, labels=NA, tck=-0.02)
    axis(4, at=seq(ylim1,ylim2, dy) , labels=NA, tck=0.015)
    title("Average Drought Intensity", line=1.7)
    text(x=(par("usr")[1] + par("usr")[2])/2,
         y=(par("usr")[4]),
         xpd=T,
         adj=c(0.5,-0.8),
         cex=1,
         paste0("Flow threshold = ", Threshold, " (maf/yr),    nWetYr= ", nBreakYears, ",   LMin = ", LMin, " (yrs),   LMax = ", LMax, " (yrs),   D0 = ", DMin, " (maf),   I0 = ", IMin, " (maf/yr)"))
    
    text(x=c(1:nScenarios), y=(par("usr")[3]), adj=c(1,2.5), labels=ScenarioList[,2], cex=0.9, srt=30,  xpd=T)
    abline(h= DrStatHist$AveIntensity, col="red", lwd=1.5)
    
    legend("topleft",
           legend=c("Historical Drought Intensity  " ),
           cex=0.9,
           col="red",
           lwd=1.5, lty=1,
           inset=c(0.02, 0.03),
           bty = "o",
           bg = "white",
           box.col = "grey",
           y.intersp=1.5,  ## vertical distance between two symbols
           x.intersp=1)
    
    
    ###### Heatmap values ======================================================================
    
    ###  Create an empty dataframe to get the final values that we need for heatmap 
    HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=9))
    colnames(HeatmapValues) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
    HeatmapValues$Scenario <- ScenarioList[,2]
    
    ## Store the values needed for the final heatmap 
    for (s in 1:nScenarios){
      HeatmapValues$Min[s] <- min(ScenarioAveDroughtIntensity[[s]])
      HeatmapValues$Q10[s] <- quantile(ScenarioAveDroughtIntensity[[s]], probs=0.10, na.rm = T)
      HeatmapValues$Q25[s] <- quantile(ScenarioAveDroughtIntensity[[s]], probs=0.25, na.rm = T)
      HeatmapValues$Median[s] <- median(ScenarioAveDroughtIntensity[[s]], na.rm = T)
      HeatmapValues$Q75[s] <- quantile(ScenarioAveDroughtIntensity[[s]], probs=0.75, na.rm = T)
      HeatmapValues$Q90[s] <- quantile(ScenarioAveDroughtIntensity[[s]], probs=0.90, na.rm = T)
      HeatmapValues$Max[s] <- max(ScenarioAveDroughtIntensity[[s]])
    }
    
    HeatmapValues$Hist <- DrStatHist$AveIntensity
    
    ##### add the heatmap dataframe output to Global Environment ==============================================
    
    assign("HM_DES_I", HeatmapValues, .GlobalEnv)
    
  }
    
  
  ### Interarrival Time ##################################################################################
  
  if (PlotT==T){
    
    ylim1=0
    ylim2=100
    dy=1
    
    boxplot(ScenarioAveDroughtInterarrivalTime, 
            names=NA, 
            las=1, 
            cex.axis=1.2, 
            ylab="Interarrival Time (yrs)", 
            cex.lab=1.3,
            cex=0.5)
    
    abline(h=seq(ylim1, ylim2, dy), col="grey50", lty="dotted")
    
    boxplot(add=T,
            ScenarioAveDroughtInterarrivalTime, 
            names=NA, 
            las=1, 
            cex.axis=1.2, 
            ylab="Interarrival Time (yrs)", 
            cex.lab=1.3,
            cex=0.5)
    
    axis(2, at=seq(ylim1,ylim2, dy) , labels=NA, tck=0.015)
    axis(4, labels=NA, tck=-0.02)
    axis(4, at=seq(ylim1,ylim2, dy) , labels=NA, tck=0.015)
    title("Average Interarrival Time", line=1.7)
    text(x=(par("usr")[1] + par("usr")[2])/2,
         y=(par("usr")[4]),
         xpd=T,
         adj=c(0.5,-0.8),
         cex=1,
         paste0("Flow threshold = ", Threshold, " (maf/yr),    nWetYr= ", nBreakYears, ",   LMin = ", LMin, " (yrs),   LMax = ", LMax, " (yrs),   D0 = ", DMin, " (maf),   I0 = ", IMin, " (maf/yr)"))
    
    text(x=c(1:nScenarios), y=(par("usr")[3]), adj=c(1,2.5), labels=ScenarioList[,2], cex=0.9, srt=30,  xpd=T)
    abline(h= DrStatHist$AveInterarrivalTime, col="red", lwd=1.5)
    
    legend("topleft",
           legend=c("Historical Interarrival Time  " ),
           cex=0.9,
           col="red",
           lwd=1.5, lty=1,
           inset=c(0.02, 0.03),
           bty = "o",
           bg = "white",
           box.col = "grey",
           y.intersp=1.5,  ## vertical distance between two symbols
           x.intersp=1)
    
    ###### Heatmap values ======================================================================
    
    ###  Create an empty dataframe to get the final values that we need for heatmap 
    HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=9))
    colnames(HeatmapValues) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
    HeatmapValues$Scenario <- ScenarioList[,2]
    
    ## Store the values needed for the final heatmap 
    for (s in 1:nScenarios){
      HeatmapValues$Min[s] <- min(ScenarioAveDroughtInterarrivalTime[[s]])
      HeatmapValues$Q10[s] <- quantile(ScenarioAveDroughtInterarrivalTime[[s]], probs=0.10, na.rm = T)
      HeatmapValues$Q25[s] <- quantile(ScenarioAveDroughtInterarrivalTime[[s]], probs=0.25, na.rm = T)
      HeatmapValues$Median[s] <- median(ScenarioAveDroughtInterarrivalTime[[s]], na.rm = T)
      HeatmapValues$Q75[s] <- quantile(ScenarioAveDroughtInterarrivalTime[[s]], probs=0.75, na.rm = T)
      HeatmapValues$Q90[s] <- quantile(ScenarioAveDroughtInterarrivalTime[[s]], probs=0.90, na.rm = T)
      HeatmapValues$Max[s] <- max(ScenarioAveDroughtInterarrivalTime[[s]])
    }
    
    HeatmapValues$Hist <- DrStatHist$AveInterarrivalTime
    
    ##### add the heatmap dataframe output to Global Environment ==============================================
    
    assign("HM_DES_T", HeatmapValues, .GlobalEnv)
    
    
  }
    


  }  ### End of function

  
  









