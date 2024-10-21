##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


############################################################################################################################
#### Function to calculate Minimum Duration-Severity 



MinDurationSeverity <- function (ScenarioList, MaxDuration=25, add=F,  
                                 col_in=rgb(r=255/255, g=228/255, b=181/255, alpha=1), 
                                 col_border=rgb(r=251/255, g=167/255, b=61/255, alpha=1), 
                                 borderlty="solid") {
    
  
  ##### nlowest function: find the nth lowest value (or index) in x =============
  
  nlowest <- function(x,n,value=TRUE){
    s <- sort(x,index.return=TRUE)
    if(value==TRUE)  {s$x[n]}  else  {s$ix[n]}    ## TRUE: nlowest=value   FALSE: nlowest=index
  } 
  
  ##### ==================================================== 
  nScenarios <- nrow(ScenarioList)
  
  ####  Create an empty dataframe to get the final values that we need for heatmap #######################
  HeatmapValues_Dur1to25 <- data.frame(matrix(nrow=nScenarios, ncol=9))
  colnames(HeatmapValues_Dur1to25) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
  HeatmapValues_Dur1to25$Scenario <- ScenarioList[,2]
  
  HeatmapValues_Dur1to5 <- data.frame(matrix(nrow=nScenarios, ncol=9))
  colnames(HeatmapValues_Dur1to5) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
  HeatmapValues_Dur1to5$Scenario <- ScenarioList[,2]
  
  HeatmapValues_Dur1to10 <- data.frame(matrix(nrow=nScenarios, ncol=9))
  colnames(HeatmapValues_Dur1to10) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
  HeatmapValues_Dur1to10$Scenario <- ScenarioList[,2]

  HeatmapValues_Dur20 <- data.frame(matrix(nrow=nScenarios, ncol=9))
  colnames(HeatmapValues_Dur20) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
  HeatmapValues_Dur20$Scenario <- ScenarioList[,2]
  
  HeatmapValues_Dur10 <- data.frame(matrix(nrow=nScenarios, ncol=9))
  colnames(HeatmapValues_Dur10) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
  HeatmapValues_Dur10$Scenario <- ScenarioList[,2]
  
  HeatmapValues_Dur5 <- data.frame(matrix(nrow=nScenarios, ncol=9))
  colnames(HeatmapValues_Dur5) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
  HeatmapValues_Dur5$Scenario <- ScenarioList[,2]
  
  ##### Historical Average Duration-Severity (for heatmap) ===================================
  #### Read Historical NF data and calculate DurSev 
  
  ## read Hist data ------
  
  Hist <- read.xlsx(MainInputFile, sheet="Hist")

  data_Hist <- Hist
  yr1_Hist <- min(data_Hist[,1])
  yr2_Hist <- max(data_Hist[,1])
  
  ### calculate DurSev -------------
  source("./R_Files/Func_DurSev_df.R")
  Func_DurSev_df(data=data_Hist, MaxDuration=MaxDuration, yr1=yr1_Hist, yr2=yr2_Hist, unit_factor=1)
  DurSev_Hist <- DurSev
  DurSev_yr_Hist <- DurSev_yr
  MinDurSev_Hist <- DurSev_Hist[1,]
  
  ### Store historical value for heatmap -----------
  HeatmapValues_Dur1to25$Hist <- mean(MinDurSev_Hist)
  HeatmapValues_Dur1to5$Hist <- mean(MinDurSev_Hist[1:5])
  HeatmapValues_Dur1to10$Hist <- mean(MinDurSev_Hist[1:10])
  HeatmapValues_Dur20$Hist <- (MinDurSev_Hist[20])
  HeatmapValues_Dur10$Hist <- (MinDurSev_Hist[10])
  HeatmapValues_Dur5$Hist <- (MinDurSev_Hist[5])

  ####################################################################################################################
  for (s in 1:nScenarios){  ### Loop over scenarios oooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    
    print(paste0("Scenario #", s))
    
    scenario <- eval(parse(text = ScenarioList[s,1]))   
    nyrs <- nrow(scenario)
    ntraces <- ncol(scenario)-1
    
    ##### MinDurSev of all series  ========================================
    
    MinDurSev <-  matrix(rep(NA), nrow=ntraces , ncol=MaxDuration)
    
    ## Loop: calculate Duration-Severity for all traces and then take the MinDurSev of each one
    for (t in 1:ntraces){
      
      flow <- scenario[ ,(1+t)]
      ## empty matrixes:
      Mean <- matrix(rep(NA), nrow=nyrs , ncol=MaxDuration)
      lowest <- matrix(rep(NA), nrow=nyrs , ncol=MaxDuration)
      lowest_index <- matrix(rep(NA), nrow=nyrs , ncol=MaxDuration)
      lowest_year <- matrix(rep(NA), nrow=nyrs , ncol=MaxDuration)
      
      ## Loop: Sequence Average over each ntraces individually
      for (duration in 1:MaxDuration){  

        mean_duration <- rep(NA)
        sort <- rep(NA)
        
        for (m in 1:(nyrs-(duration-1))){
          mean_duration[m] <- mean( flow[ m : (m+(duration-1)) ] )
          Mean[m ,duration] <- mean_duration[m]
        }
        
        for (m in 1:(nyrs-(duration-1))){
          lowest[m ,duration] <- nlowest( mean_duration, m, value=TRUE)
          lowest_index[m ,duration] <- nlowest(mean_duration, m, value=FALSE)   
          lowest_year[m ,duration] <- scenario[lowest_index[m ,duration], 1]
        }
        
        if (duration==nyrs) {break} ## for scenarios with length less than MaxDuration=25
      }
      
      MinDurSev [t,] <-  lowest [1,]  ## just take the lowest sequence average
      
    }
    
    colnames(MinDurSev) <- paste0("dur",1:MaxDuration)
    
    ## change MinDurSev to a data frame and add series' name 
    MinDurSev_df <- data.frame(MinDurSev)
    MinDurSev_df$Trace <- paste0("trace", c(1:ntraces))
    MinDurSev_df <- MinDurSev_df[c(ncol(MinDurSev_df), 1:MaxDuration)]
    
    
    
    ## calculate the band (range of the min duration-severity of all traces) ====================
    
    library(matrixStats)
    band_Min <- colMins(MinDurSev)
    band_Max <- colMaxs(MinDurSev)
    band_Median <- colMedians(MinDurSev)
    band_Q10 <- colQuantiles(MinDurSev, probs=0.1)
    band_Q90 <- colQuantiles(MinDurSev, probs=0.9)
    band_Q25 <- colQuantiles(MinDurSev, probs=0.25)
    band_Q75 <- colQuantiles(MinDurSev, probs=0.75)
    
    MinDurSev_Band <- data.frame("duration"=1:MaxDuration, "Min"=band_Min, "Max"=band_Max, "Median"=band_Median,
                                 "Q10"=band_Q10, "Q90"=band_Q90, "Q25"=band_Q25, "Q75"=band_Q75)
    
    
    ### Store the values needed for the final heatmap =================================
    
    HeatmapValues_Dur1to25$Min[s] <- mean(MinDurSev_Band$Min)
    HeatmapValues_Dur1to25$Q10[s] <- mean(MinDurSev_Band$Q10)
    HeatmapValues_Dur1to25$Q25[s] <- mean(MinDurSev_Band$Q25)
    HeatmapValues_Dur1to25$Median[s] <- mean(MinDurSev_Band$Median)
    HeatmapValues_Dur1to25$Q75[s] <- mean(MinDurSev_Band$Q75)
    HeatmapValues_Dur1to25$Q90[s] <- mean(MinDurSev_Band$Q90)
    HeatmapValues_Dur1to25$Max[s] <- mean(MinDurSev_Band$Max)

    HeatmapValues_Dur1to5$Min[s] <- mean(MinDurSev_Band$Min[1:5])
    HeatmapValues_Dur1to5$Q10[s] <- mean(MinDurSev_Band$Q10[1:5])
    HeatmapValues_Dur1to5$Q25[s] <- mean(MinDurSev_Band$Q25[1:5])
    HeatmapValues_Dur1to5$Median[s] <- mean(MinDurSev_Band$Median[1:5])
    HeatmapValues_Dur1to5$Q75[s] <- mean(MinDurSev_Band$Q75[1:5])
    HeatmapValues_Dur1to5$Q90[s] <- mean(MinDurSev_Band$Q90[1:5])
    HeatmapValues_Dur1to5$Max[s] <- mean(MinDurSev_Band$Max[1:5])
    
    HeatmapValues_Dur1to10$Min[s] <- mean(MinDurSev_Band$Min[1:10])
    HeatmapValues_Dur1to10$Q10[s] <- mean(MinDurSev_Band$Q10[1:10])
    HeatmapValues_Dur1to10$Q25[s] <- mean(MinDurSev_Band$Q25[1:10])
    HeatmapValues_Dur1to10$Median[s] <- mean(MinDurSev_Band$Median[1:10])
    HeatmapValues_Dur1to10$Q75[s] <- mean(MinDurSev_Band$Q75[1:10])
    HeatmapValues_Dur1to10$Q90[s] <- mean(MinDurSev_Band$Q90[1:10])
    HeatmapValues_Dur1to10$Max[s] <- mean(MinDurSev_Band$Max[1:10])
    
    HeatmapValues_Dur20$Min[s] <- (MinDurSev_Band$Min[20])
    HeatmapValues_Dur20$Q10[s] <- (MinDurSev_Band$Q10[20])
    HeatmapValues_Dur20$Q25[s] <- (MinDurSev_Band$Q25[20])
    HeatmapValues_Dur20$Median[s] <- (MinDurSev_Band$Median[20])
    HeatmapValues_Dur20$Q75[s] <- (MinDurSev_Band$Q75[20])
    HeatmapValues_Dur20$Q90[s] <- (MinDurSev_Band$Q90[20])
    HeatmapValues_Dur20$Max[s] <- (MinDurSev_Band$Max[20])
    
    HeatmapValues_Dur10$Min[s] <- (MinDurSev_Band$Min[10])
    HeatmapValues_Dur10$Q10[s] <- (MinDurSev_Band$Q10[10])
    HeatmapValues_Dur10$Q25[s] <- (MinDurSev_Band$Q25[10])
    HeatmapValues_Dur10$Median[s] <- (MinDurSev_Band$Median[10])
    HeatmapValues_Dur10$Q75[s] <- (MinDurSev_Band$Q75[10])
    HeatmapValues_Dur10$Q90[s] <- (MinDurSev_Band$Q90[10])
    HeatmapValues_Dur10$Max[s] <- (MinDurSev_Band$Max[10])
    
    HeatmapValues_Dur5$Min[s] <- (MinDurSev_Band$Min[5])
    HeatmapValues_Dur5$Q10[s] <- (MinDurSev_Band$Q10[5])
    HeatmapValues_Dur5$Q25[s] <- (MinDurSev_Band$Q25[5])
    HeatmapValues_Dur5$Median[s] <- (MinDurSev_Band$Median[5])
    HeatmapValues_Dur5$Q75[s] <- (MinDurSev_Band$Q75[5])
    HeatmapValues_Dur5$Q90[s] <- (MinDurSev_Band$Q90[5])
    HeatmapValues_Dur5$Max[s] <- (MinDurSev_Band$Max[5])
    
    #### Plot the band ==========================================
    
    if (add==F){
      
      ymin=3
      ymax=25
      
      par(mar=c(4.5,5,2.5,2))  ### margins for plot
      
      x <- c(1:MaxDuration)
      plot(x, x, col="white", ylim=c(ymin,ymax) , xlim=c(1, MaxDuration+1),
           xaxt="n" ,yaxt="n", pch=16, cex=0.6, xlab="Duration (yrs)", ylab="Mean flow (maf/yr)", cex.lab=1.4,
           main= paste0("Duration-Severity Analysis,  Ensemble: " , ScenarioList[s,2]) )
      
      axis(1, at=seq(1, MaxDuration, 1), cex.axis=1.1)
      axis(2, at=seq(ymin, ymax, 2), cex.axis=1.1, las=1)  
      axis(2, at=seq(ymin, ymax, 1), labels=NA, tck=0.01) 
      
      axis(4, at=seq(ymin, ymax, 2), labels=NA)  
      axis(4, at=seq(ymin, ymax, 1), labels=NA, tck=0.01)  
      
      jj <- which(!is.na(MinDurSev_Band$Q10))
      
      polygon (x=c(MinDurSev_Band$duration[jj], rev(MinDurSev_Band$duration[jj])), y=c(MinDurSev_Band$Q10[jj], rev(MinDurSev_Band$Q90[jj])), col= col_in , border=NA, lwd=1.5, lty=borderlty)
      lines(x=MinDurSev_Band$duration[jj], y=MinDurSev_Band$Q10[jj], col=col_border, lwd=2)
      lines(x=MinDurSev_Band$duration[jj], y=MinDurSev_Band$Q90[jj], col=col_border, lwd=2)
      
      lines(x=MinDurSev_Band$duration[jj], y=MinDurSev_Band$Min[jj], col=col_border, lwd=2)
      lines(x=MinDurSev_Band$duration[jj], y=MinDurSev_Band$Median[jj], col=col_border, lwd=2)
      lines(x=MinDurSev_Band$duration[jj], y=MinDurSev_Band$Max[jj], col=col_border, lwd=2)
      
      
      # polygon (x=c(MinDurSev_Band$duration[jj], rev(MinDurSev_Band$duration[jj])), y=c(MinDurSev_Band$Q10[jj], rev(MinDurSev_Band$Q90[jj])), col= col_in , border=col_border, lwd=1.5, lty=borderlty)
      # lines(x=MinDurSev_Band$duration[jj], y=MinDurSev_Band$Min[jj], col=col_border, lwd=1.5)
      # lines(x=MinDurSev_Band$duration[jj], y=MinDurSev_Band$Median[jj], col=col_border, lwd=1.5)
      # lines(x=MinDurSev_Band$duration[jj], y=MinDurSev_Band$Max[jj], col=col_border, lwd=1.5)
      
    }
    
    
    if (add==T){
      
      jj <- which(!is.na(MinDurSev_Band$Q10))
      polygon (x=c(MinDurSev_Band$duration[jj], rev(MinDurSev_Band$duration[jj])), y=c(MinDurSev_Band$Q10[jj], rev(MinDurSev_Band$Q90[jj])), col= col_in , border=col_border, lwd=1, lty=borderlty)
      lines(x=MinDurSev_Band$duration[jj], y=MinDurSev_Band$Min[jj], col=col_border, lwd=1.5)
      lines(x=MinDurSev_Band$duration[jj], y=MinDurSev_Band$Median[jj], col=col_border, lwd=1.5)
      lines(x=MinDurSev_Band$duration[jj], y=MinDurSev_Band$Max[jj], col=col_border, lwd=1.5)
    }
    
    
    
    # if (plot_mins==TRUE){   ## Plot MinDurSev of all series -------------------
    #   
    #   for (t in 1:ntraces){
    #     points ( c(1:MaxDuration), MinDurSev[t,], pch=16, cex=0.5, col="grey50")
    #     lines ( c(1:MaxDuration), MinDurSev[t,], col="grey50", lwd=0.2)
    #   }
    #   
    # }
    
    
    ### add dotty plots ===============================================
    
    source("./R_Files/Func_DottyPlots_Dots.R")
    
    
    #### Read Historical NF data and add dots to the plot -------------------
    
    ## read Hist data ------
    
    Hist <- read.xlsx(MainInputFile, sheet="Hist")

    data_Hist <- Hist
    yr1_Hist <- min(data_Hist[,1])
    yr2_Hist <- max(data_Hist[,1])

    # data_Hist <- read.xlsx("./Raw_Data/LeesFerryAnnualWYTotalNF.xlsx", sheet="AnnWYTotNFLF")
    
     ColObsDots <- rgb(r=118/255, g=228/255, b=250/255)
    DurationSeverity_Dots (data=data_Hist, MaxDuration=25, yr1=yr1_Hist, yr2=yr2_Hist, unit_factor=1, post_yr=2000, 
                           gap=0.12, col1="deepskyblue2", col2="red", DotBg=ColObsDots, DotType=21, DotSize=0.6, DotLwd=0.2,lable_min=F, ltm=F, line_minNF=F)  #"lightskyblue"
    
    # DurationSeverity_Dots (data=data_Hist, MaxDuration=25, yr1=yr1_Hist, yr2=yr2_Hist, unit_factor=1, post_yr=2000, 
    #                        gap=0.12, col1="deepskyblue2", col2="red", DotBg=NA, DotType=1, DotSize=0.5, DotLwd=1, lable_min=F, ltm=F, line_minNF=F)  #"lightskyblue"
    # 
    
    ltm_Hist <- mean(data_Hist[,2])
    abline ( ltm_Hist, 0, col= "deepskyblue2", lwd=1.2)  ##steelblue1
    text((MaxDuration+0.2),(ltm_Hist+0.4), labels= paste(round(ltm_Hist, digits=2)), pos = 4, cex=1, col="deepskyblue2", xpd=TRUE)  ##, font=2  "steelblue2"
    
    #### Read tree ring data and add dots to the plot -----------------
    
    ## Function to read TreeRing data
    
    data_TR <- read.xlsx(MainInputFile, sheet="TreeRingFlows")
    yr1_TR <- min(data_TR[,1])
    yr2_TR <- max(data_TR[,1])

        
    DurationSeverity_Dots(data=data_TR, MaxDuration=25, yr1=yr1_TR, yr2=yr2_TR, unit_factor=1, post_yr=2000, 
                          gap= -0.12, col1="deepskyblue4", col2="yellow", DotBg="deepskyblue3", DotType=21, DotSize=0.6, DotLwd=0.2, lable_min=F, ltm=F, line_minNF=F)  ##skyblue3
    
    
    # DurationSeverity_Dots(data=data_TR, MaxDuration=25, yr1=yr1_TR, yr2=yr2_TR, unit_factor=1, post_yr=2000, 
    #                       gap= -0.12, col1="deepskyblue4", col2="yellow", DotBg=NA, DotType=1, DotSize=0.5, DotLwd=1, lable_min=F, ltm=F, line_minNF=F)  ##skyblue3
    
    ltm_TR <- mean(data_TR[,2])
    abline ( ltm_TR, 0, col= "deepskyblue4", lwd=1.2)  ## skyblue4
    text((MaxDuration+0.2),(ltm_TR-0.5), labels= paste(round(ltm_TR, digits=2)), pos = 4, cex=1, col="deepskyblue4", xpd=TRUE)  ##, font=2
    
    
    #### Legend ==============================================
    
    legend("topright", title = "Observed natural flow", title.adj = 0,
           legend=c( paste0("Full period (", yr1_Hist,"-", yr2_Hist, ")"), 
                     paste0("Post-2000 (2000-", yr2_Hist, ")") , 
                     paste0("Long-term mean (", yr1_Hist,"-", yr2_Hist, ")")   ),
           col=c("deepskyblue2", "black" , "deepskyblue2"),
           pt.bg=c(ColObsDots,"red", NA) , pch=c(21,21, NA), pt.cex=c(0.7, 0.7,NA),
           lwd=1,  lty=c(0,0,1), inset=c(0.02, 0.03), bty = "n", cex=0.9) 
    
    legend("top", title = "Tree-ring-reconstructed natural flow", title.adj = 0,
           legend=c( paste0("Full period (", yr1_TR,"-", yr2_TR, ")"), 
                     paste0("Post-2000 (2000-", yr2_TR, ")") , 
                     paste0("Long-term mean (", yr1_TR,"-", yr2_TR, ")")   ),
           col=c("deepskyblue4", "black", "deepskyblue4"),
           pt.bg=c("deepskyblue3", "yellow", NA) , pch=c(21, 21, NA), pt.cex=c(0.7, 0.7, NA),
           lwd=1,  lty=c(0, 0, 1), inset=c(0.05, 0.03), bty = "n", cex=0.9) 
    
    legend("bottomright", title = "Minimum Duration-Severity of the Ensemble", title.adj = 0,
           legend=c("10% to 90%", "Minimum, 10%, Median, 90%, Maximum"),
           col=c(col_in, col_border),
           pt.bg=c(col_in, NA) , pch=c(15, NA), pt.cex=c(2, NA),
           lwd=2,  lty=c( 0, 1), inset=c(0.05, 0.03), bty = "n") 
    

    
  }  ### end of loop over scenarios oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
  
 
  ##### add the heatmap dataframe output to Global Environment ##################################################################################
  assign("HM_DS_1to25yr", HeatmapValues_Dur1to25, .GlobalEnv)
  assign("HM_DS_1to5yr", HeatmapValues_Dur1to5, .GlobalEnv)
  assign("HM_DS_1to10yr", HeatmapValues_Dur1to10, .GlobalEnv)
  assign("HM_DS_20yr", HeatmapValues_Dur20, .GlobalEnv)
  assign("HM_DS_10yr", HeatmapValues_Dur10, .GlobalEnv)
  assign("HM_DS_5yr", HeatmapValues_Dur5, .GlobalEnv)

} # function






