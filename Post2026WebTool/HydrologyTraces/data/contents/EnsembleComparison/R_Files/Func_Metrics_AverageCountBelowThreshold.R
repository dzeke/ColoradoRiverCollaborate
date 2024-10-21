##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################

 
#########################################################################################################
###### Function to calculate the average count below threshold for the selected scenarios
###### and then create the boxplot of the distribution of all traces of each scenario

Func_AverageCountBelowThreshold <- function(ScenarioList , Threshold=14.78 , Duration=10){
  
  
  ### Function to calculate the Average Count Below Threshold =========================================
  
  Func_AveCountBT <- function(data=scenario , Threshold=Threshold , Duration=Duration){
    
    n <- nrow(data)
    nTraces <- ncol(data)-1
    
    ### is data below the threshold or not (loop over all traces) -------
    
    IsBelowThreshold <- data
    IsBelowThreshold [,-1] <- NA
    
    for (j in 1:nTraces){
      for (i in 1:n){
        if (data[i,(1+j)]<Threshold) {
          IsBelowThreshold[i,(1+j)] <- 1
        }else{
          IsBelowThreshold[i,(1+j)] <- 0
        }
      }
    }
    
    
    ### Count below threshold over duration (all the traces) ---------------------
    
    nBlock <- (n-Duration+1)
    CountBelowThreshold <- data.frame(matrix( nrow=nBlock , ncol=(ncol(data)+1)  ))
    colnames(CountBelowThreshold)[1:2] <- c("StartYr", "EndYr")
    colnames(CountBelowThreshold)[3:(nTraces+2)] <- colnames(data)[-1]
    
    
    for (j in 1:nTraces){
      
      for (b in 1:nBlock){
        CountBelowThreshold$StartYr[b] <- data[b,1]
        CountBelowThreshold$EndYr[b] <- data[(b-1+Duration) , 1]
        CountBelowThreshold[b, (j+2)] <- sum(IsBelowThreshold[b:(b-1+Duration), (j+1)])
      }
    }
    
    
    if (nTraces>1){
      AveCount <- colMeans(CountBelowThreshold[,-1:-2])
    }else{
      AveCount <- mean(CountBelowThreshold[,-1:-2])
    }

    
    return(AveCount)
  } ## end of Func_AveCountBT function 
  
  
  #### Historical Average Count Below Threshold (will be added as points in boxplot) =================================================
  
  ## read Hist data ------
  
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  yr1 <- min(Hist[,1])
  yr2 <- max(Hist[,1])
  
  AveCountBT_hist <- Func_AveCountBT(data=Hist, Threshold , Duration)
  
  
  ####  Create an empty dataframe to get the final values that we need for heatmap ======================
  
  HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=9))
  colnames(HeatmapValues) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
  HeatmapValues$Scenario <- ScenarioList[,2]
  
  ## store the historical value of the metric in the heatmap dataframe
  HeatmapValues$Hist <- AveCountBT_hist
  
  ### boxplot of Average Count ========================================================================
  
  par(mar=c(6, 4, 3, 4.5) + 0.1 , mgp=c(2.7, 1, 0) )
  
  AveCountBT_Scenario1 <- Func_AveCountBT ( data = eval(parse(text = ScenarioList[1,1])) , Threshold , Duration)

  n_Scenario <- nrow(ScenarioList)
  
  nx <- n_Scenario+1
  boxplot(AveCountBT_Scenario1, xlim=c(0,nx),
          # main=paste0("Average Count Below Threshold (Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr)" ),
          ylab="Count below threshold (yrs)",
          cex.lab=1.3,
          cex.axis=1.2,
          ylim=c(0,Duration),
          # xaxt="n",
          las=1,
          cex=0.5)
  title(main=paste0("Average Count Below Threshold (Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr)" ), adj=0.5, line=1)
  
  abline(h=seq(0,Duration,1), col="gray50", lty="dotted")

  axis(1, at=c(1:n_Scenario), labels=NA) 
  axis(2, at=seq(0, Duration, 1), labels=NA, tck=0.015)  

  axis(4, at=seq(0, Duration, length.out=11), labels= seq(0, 100, 10) , cex.axis=1.2, las=1 )
  axis(4, at=seq(0, Duration, length.out=21), labels=NA, tck=0.015)
 
  # axis(4, at=seq(0, Duration, length.out=11), labels= seq(0, 1, 0.1) , cex.axis=1.2, las=1 )
  # axis(4, at=seq(0, Duration, length.out=21), labels=NA, tck=0.015)
  
  ## second axis lable---
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  text(x=xmax, y=(ymax+ymin)/10*9 ,  labels="Count below threshold (% of Duration)", pos=4, offset=3.5, col="black", cex=1.3, srt=-90, adj=0.75, xpd=T)
  
  
    ### add each boxplot to the current plot ----------
  
  ymin <- par("usr")[3]
  
  for (s in 1:n_Scenario){
    AveCountBT_Scenario <- Func_AveCountBT ( data = eval(parse(text = ScenarioList[s,1])), Threshold , Duration  )
    
    ### Store the values needed for the final heatmap 
    HeatmapValues$Min[s] <- min(AveCountBT_Scenario)
    HeatmapValues$Q10[s] <- quantile(AveCountBT_Scenario, probs=0.10)
    HeatmapValues$Q25[s] <- quantile(AveCountBT_Scenario, probs=0.25)
    HeatmapValues$Median[s] <- median(AveCountBT_Scenario)
    HeatmapValues$Q75[s] <- quantile(AveCountBT_Scenario, probs=0.75)
    HeatmapValues$Q90[s] <- quantile(AveCountBT_Scenario, probs=0.90)
    HeatmapValues$Max[s] <- max(AveCountBT_Scenario)
    
    i <- s
    boxplot(AveCountBT_Scenario, add=T, at=s, yaxt="n", xaxt="n" , cex=0.5)

    text(x=(i+0.5), y=ymin*2, ScenarioList[s,2], srt=30, pos=2, offset=0.3 , cex=0.9, xpd=T)
    
      }
  
  ### historical averge count -----------
  abline(h=AveCountBT_hist, col="red")  
  legend("bottomleft", legend="Historical count below threshold  ", 
         cex=1, 
         col="red", 
         lty=1,
         bty = "o",
         bg = "white",
         inset=c(0.03, 0.03))
  
  # ### save the current plot as pdf ========================================================================
  # 
  # pdfName <- paste0("./Results/CountBelowThreshold/AverageCountBelowThreshold_", Duration, "yr.pdf")
  # dev.copy2pdf(file=pdfName, width = 10, height = 5)
  # 
  
  
  ##### add the heatmap dataframe output to Global Environment ##################################################################################
  
  assign(paste0("HM_ACBT",Threshold), HeatmapValues, .GlobalEnv)
  
  
}  ## End of the main function 
#########################################################################################################







