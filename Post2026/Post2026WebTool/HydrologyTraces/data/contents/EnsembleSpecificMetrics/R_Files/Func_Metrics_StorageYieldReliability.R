##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


#########################################################################################################
### This function create both storage-yield and storage reliability plots 


Func_StorageYieldReliability <- function(ScenarioList, 
                                         YieldFraction=seq(7, 15, by=1)/15,   ## YieldFraction=seq(0.5, 0.95, by=0.05),
                                         ReferenceMean=15, 
                                         S0=0,    ## initial storage 
                                         DesiredYieldFraction = 1){
                               
  
  ####  Create an empty dataframe to get the final values that we need for heatmap #######################
  HeatmapValues_SY <- data.frame(matrix(nrow=nScenarios, ncol=7))
  colnames(HeatmapValues_SY) <- c("Scenario", "Q10", "Q25", "Median", "Q75", "Q90", "Hist")
  HeatmapValues_SY$Scenario <- ScenarioList[,2]
  
  HeatmapValues_SR <- data.frame(matrix(nrow=nScenarios, ncol=7))
  colnames(HeatmapValues_SR) <- c("Scenario", "Q10", "Q25", "Median", "Q75", "Q90", "Hist")
  HeatmapValues_SR$Scenario <- ScenarioList[,2]
  
  
  #########################################################################################################
  ############# Historical Storage Yield ###############################
  
  #### Read Historical NF data -------------------
  
  ## Function to read Hist data
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
   
  ## Input 
  scenario <-  Hist   
  
  ##
  ntraces_hist <- ncol(scenario)-1      
  nyr <- nrow(scenario)
  
  ## creat an empty dataframe for StorageCapacityMax_Hist -----------------------
  StorageCapacityMax_Hist <- data.frame(matrix( nrow=(length(YieldFraction)), ncol=(ntraces_hist+2) ))
  colnames(StorageCapacityMax_Hist)[1] <- "YieldFraction"
  colnames(StorageCapacityMax_Hist)[2] <- "YieldMAF"
  colnames(StorageCapacityMax_Hist)[-1:-2] <- "HistoricalStorage"
  
  StorageCapacityMax_Hist$YieldFraction <- YieldFraction
  YieldMAF <- ReferenceMean * YieldFraction
  StorageCapacityMax_Hist$YieldMAF <- YieldMAF
  
  ## creat StCap dataframe for storing storage capacity in each year of traces -----------------
  StCap <- scenario
  StCap [,-1] <- NA
  
  ## estimate storage capacity: S(t) = S(t-1) + Yield(t) - Inflow(t) ,  if S(t)<0 --> S(t)=0 ----------------------
  
  library("matrixStats")  ## to use colMaxs function
  
  for(Yield in StorageCapacityMax_Hist$YieldMAF){
    
    for(j in 2:(ntraces_hist+1)){
      
      ## for the first row
      if( (S0 + Yield - scenario[1,j])<0 ){
        StCap[1,j] <- 0
      }else{
        StCap[1,j] <- S0 + Yield - scenario[1,j]
      }
      
      ## for the other rows
      for(i in 2:nyr){
        
        if( (StCap[(i-1),j] + Yield - scenario[i,j])<0 ){
          StCap[i,j] <- 0
        }else{
          StCap[i,j] <- StCap[(i-1),j] + Yield - scenario[i,j]
        }
        
      }
    }
    
    r <- which(StorageCapacityMax_Hist$YieldMAF==Yield)
    StorageCapacityMax_Hist[r,-1:-2] <- colMaxs(as.matrix(StCap[,-1]))  ## storage capacity for each trace is max of calculated StCap
    
  }
  
  #### Store the historical metric in the heatmap df =======================
  ii <- which(StorageCapacityMax_Hist$YieldFraction==DesiredYieldFraction)
  HeatmapValues_SY$Hist <- StorageCapacityMax_Hist[ii,3]
  
  
  
  #########################################################################################################
  ############# Scenario Storage Yield ###############################
  
  
  ### Loop over Scenarios OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
  nScenarios <- nrow(ScenarioList)
  
  for (s in 1:nScenarios){
    
    ## Input ???????????????????????????????
    scenario <- eval(parse(text = ScenarioList[s,1]))   ## Drought_Mill_LF_TotWY # Scenario_ISM_Stress   ### Scenario_ISM_PluvialRemoved   ### Scenario_ISM_Full
    
    ## 
    ntraces <- ncol(scenario)-1     
    nyr <- nrow(scenario)
    
    ######## Calculate Storage Statistics #############################################################################
    
    YieldMAF <- ReferenceMean * YieldFraction
    
    ### estimate storage capacity for various yield fractions =======================================================
    
    ## creat an empty dataframe for StorageCapacityMax -----------------------
    StorageCapacityMax <- data.frame(matrix( nrow=(length(YieldFraction)), ncol=(ntraces+2) ))
    colnames(StorageCapacityMax)[1] <- "YieldFraction"
    colnames(StorageCapacityMax)[2] <- "YieldMAF"
    colnames(StorageCapacityMax)[-1:-2] <- colnames(scenario[-1])
    
    StorageCapacityMax$YieldFraction <- YieldFraction
    StorageCapacityMax$YieldMAF <- YieldMAF
    
    ## creat StCap dataframe for storing storage capacity in each year of traces -----------------
    StCap <- scenario
    StCap [,-1] <- NA
    
    ## estimate storage capacity: S(t) = S(t-1) + Yield(t) - Inflow(t) ,  if S(t)<0 --> S(t)=0 ----------------------
    
    library("matrixStats")  ## to use colMaxs function
    
    for(Yield in StorageCapacityMax$YieldMAF){
      
      for(j in 2:(ntraces+1)){
        
        ## for the first row
        if( (S0 + Yield - scenario[1,j])<0 ){
          StCap[1,j] <- 0
        }else{
          StCap[1,j] <- S0 + Yield - scenario[1,j]
        }
        
        ## for the other rows
        for(i in 2:nyr){
          
          if( (StCap[(i-1),j] + Yield - scenario[i,j])<0 ){
            StCap[i,j] <- 0
          }else{
            StCap[i,j] <- StCap[(i-1),j] + Yield - scenario[i,j]
          }
          
        }
      }
      
      r <- which(StorageCapacityMax$YieldMAF==Yield)
      StorageCapacityMax[r,-1:-2] <- colMaxs(as.matrix(StCap[,-1]))  ## storage capacity for each trace is max of calculated StCap
      
    }
    
    
    ### Store the values needed for the final heatmap =================================
    ii <- which(StorageCapacityMax$YieldFraction==DesiredYieldFraction)
    HeatmapValues_SY$Q10[s] <- quantile(as.numeric(StorageCapacityMax[ii,-1:-2]), probs=0.10)
    HeatmapValues_SY$Q25[s] <- quantile(as.numeric(StorageCapacityMax[ii,-1:-2]), probs=0.25)
    HeatmapValues_SY$Median[s] <- median(as.numeric(StorageCapacityMax[ii,-1:-2]))
    HeatmapValues_SY$Q75[s] <- quantile(as.numeric(StorageCapacityMax[ii,-1:-2]), probs=0.75)
    HeatmapValues_SY$Q90[s] <- quantile(as.numeric(StorageCapacityMax[ii,-1:-2]), probs=0.90)
    
    

    ########## PLOT ===========================================================================
    
    PlotYmax <- 300  ## 300
    
    ### Plot a number of figures along with each other --------------
    # par(mfrow=c(1,2), mar=c(5,4.5,5,4), oma=c(4,4,4,4)) ## 2x2=4 plots
    
    # par( mar=c(7,4.5,10,4))  ##par( mar=c(5,4.5,7,4))
    par(mar=c(7, 4.5, 10, 2)  , mgp=c(3.2, 1, 0) )   
    
    ### boxplot ------------------
    # bx <- t(StorageYield[,-2])
    # bx <- data.frame(bx)
    # bx$ExtraCol <- NA
    # bx$ExtraCol [1] <- 1
    
    if(ntraces!=1){  ### when the scenario has more than one trace
      
      bx <- t(StorageCapacityMax[,-1])
      boxplot(bx[-1, ], 
              horizontal=TRUE,
              # main = paste0("\n Reservoir Storage-Yield Analysis \n Reference mean flow: ",ReferenceMean," maf;  \n ", ScenarioList[s,2]),
              # # cex.main=1.1,
              xlab = "Storage (maf)",
              ylab = "Yield (maf)",
              ylim = c(0,PlotYmax),
              names = as.character(bx[1,]),
              cex.lab=1.3,
              cex.axis=1.1,
              las=1,
              cex=0.7)
      
      title(main = paste0("\n Reservoir Storage-Yield Analysis \n ", ScenarioList[s,2]),
            line=0.6, cex.main=1.1)

      
      points(x=StorageCapacityMax_Hist$HistoricalStorage, y=c(1:(length(YieldFraction))), col="red", pch=16)
      # lines(x=StorageCapacityMax_Hist$HistoricalStorage, y=c(1:(length(YieldFraction))), col="red")
      
      ## minor axis tick
      axis(1, at=seq(0,PlotYmax, 10), labels=NA, tck=0.01) 
      # axis(4, at=c(1:(length(YieldFraction))), labels= round(bx[1,]*ReferenceMean, digits=2) , cex.axis=1.1, las=1, xlab="HHH")
      # mtext("Yield (maf)", side=4, cex=1.2, line=4)
      # text(x=(par("usr")[2])*1.2, y=mean(par("usr")[3], par("usr")[4]) ,  labels="Yield (maf)", pos=4, cex=1.3, col="red", srt=180)
      
      # ## second axis lable---
      # xmin <- par("usr")[1]
      # xmax <- par("usr")[2]
      # ymin <- par("usr")[3]
      # ymax <- par("usr")[4]
      # text(x=xmax, y=(ymax+ymin)/5*3 ,  labels="Yield (maf)", pos=4, offset=3.8, col="black", cex=1.3, srt=-90,  xpd=T)
      
      
      
      
      legend("bottomright", legend="Historical ", cex=1, col="red", pch=16, inset=c(0.05, 0.05), bty="o")
      
    }
    
    
    if(ntraces==1){  ### when the scenario has only one trace
      plot(x=StorageCapacityMax[,3], y=StorageCapacityMax$YieldFraction,
           # main = paste0("\n Reservoir Storage-Yield Analysis \n Reference mean flow: ",ReferenceMean," maf;  \n ", ScenarioList[s,2]),
           # cex.main=1.1,
           xlab = "Storage (maf)",
           ylab = "Yield fraction",
           xlim = c(0,250),
           ylim=c(0.48,0.98),
           yaxt="n",
           cex.lab=1.3,
           cex.axis=1.1,
           las=1,
           pch=16)
      
      title(main = paste0("\n Reservoir Storage-Yield Analysis \n Reference mean flow: ",ReferenceMean," maf;  \n ", ScenarioList[s,2]),
            line=0.6, cex.main=1.1)
      
      axis(2, at=StorageCapacityMax$YieldFraction, las=1, cex.axis=1.1)
      points(x=StorageCapacityMax_Hist$HistoricalStorage, y=StorageCapacityMax_Hist$YieldFraction , col="red", pch=16)
      ## minor axis tick
      axis(1, at=seq(0,250, 10), labels=NA, tck=0.01) 
      axis(4, at=StorageCapacityMax$YieldFraction, labels= round(StorageCapacityMax$YieldMAF , digits=2) , cex.axis=1.1, las=1)
      mtext("Yield (maf)", side=4, cex=1.3, line=4)
      
      legend("bottomright", legend="Historical storage-yield", cex=1, col="red", pch=16, inset=c(0.05, 0.05), bty="o")
    }
    
    
    
    # assign("StorageYield" , StorageCapacityMax, .GlobalEnv)
    # assign("StorageYield_Hist" , StorageCapacityMax_Hist, .GlobalEnv)
    # assign("YieldFraction" , YieldFraction, .GlobalEnv)
    # assign("ReferenceMean" , ReferenceMean, .GlobalEnv)
    
    
    ##############################################################################################################
    ##### Reliability Analysis (CDF) ######################################################################
    
    ### Input ????????????????????????????????????????????????????
    DesiredYieldFraction <- DesiredYieldFraction  ## %
    
    r <- (which(StorageCapacityMax$YieldFraction==DesiredYieldFraction))
    NeededFullStorage <- StorageCapacityMax[r,]
    
    cdf <- data.frame("Trace"=colnames(NeededFullStorage[-1:-2]) ,
                      "storage"=as.numeric(NeededFullStorage[-1:-2]),
                      "sort"=NA,
                      "rank"=NA,
                      "cdf"=NA)
    
    cdf$sort <- sort(cdf$storage, decreasing = FALSE)
    cdf$rank <- c(1:(nrow(cdf)))
    cdf$cdf <- (cdf$rank) / (nrow(cdf)+1)
    
    #### plot --------------------------
    # dev.new()
    par(mar=c(7, 4.5, 10, 2), mgp=c(3, 1, 0))  ## marging for second plot
    
    plot(x=cdf$cdf, y=cdf$sort,
         # main = paste0("Reservoir Reliability Analysis \n" , ScenarioName, ", Yield Fraction = ", DesiredYieldFraction ),
         # main = paste0("\n Reservoir Reliability Analysis \n Reference mean flow: ",ReferenceMean," maf;  \n ", ScenarioList[s,2]),
         # cex.main=1.1,
         xlab="Probability of meeting yield",
         ylab="Storage (maf)",
         ylim=c(0,PlotYmax),
         cex.lab=1.3,
         cex.axis=1.1,
         las=1,
         pch=16,
         cex=0.8)
    
    title(main = paste0("\n Reservoir Storage Reliability \n ", ScenarioList[s,2]),
          line=0.6, cex.main=1.1)
    
    ## minor axis tick
    axis(1, at=seq(0,1, 0.05), labels=NA, tck=0.01)
    axis(2, at=seq(0,PlotYmax, 10), labels=NA, tck=0.01)
    axis(4, labels=NA)
    axis(4, at=seq(0,PlotYmax, 10), labels=NA, tck=0.01)
    
    # text(x=0.25, y=210, pos=4, paste0(" Yield fraction = ", DesiredYieldFraction))
    # text(x=0, y=210, pos=4, cex=1.4 , paste0(" Yield fraction = ", DesiredYieldFraction))
    legend("topleft", legend=paste0("Yield = ", round(ReferenceMean*DesiredYieldFraction, digits=2), " maf"),
           cex=1.2, bty="n", inset=c(0.02, 0.03))
    
    
    ### Store values needed for the final heatmap =================================
    
    # HeatmapValues_SR$Storage[s] <- approx(x=cdf$cdf, y=cdf$sort, method="linear", xout=0.9)$y  ## interpolation to estimate the storage associated with 90% of reliability
    
    HeatmapValues_SR$Q10[s] <- NA
    HeatmapValues_SR$Q25[s] <- NA
    HeatmapValues_SR$Median[s] <- approx(x=cdf$cdf, y=cdf$sort, method="linear", xout=0.9)$y  ## interpolation to estimate the storage associated with 90% of reliability
    HeatmapValues_SR$Q75[s] <- NA
    HeatmapValues_SR$Q90[s] <- NA
    
    
  }  ## end of loop over scenarios  OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
  
  
  # par(mfrow=c(1,1))  ## to get back to the default plot area
  
  ##### add the heatmap dataframe output to Global Environment ##################################################################################
  assign("HM_SYR", HeatmapValues_SY, .GlobalEnv)
  # assign("HM_SR", HeatmapValues_SR, .GlobalEnv)  %% no need to add SR
  
  
} ## end of function








