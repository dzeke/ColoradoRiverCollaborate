##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


#########################################################################################################
###### Function to calculate the average count above threshold for the selected scenarios
###### and then create the boxplot of the distribution of all traces of each scenario

Func_MovingCountAboveThreshold <- function(data , Threshold=20 , Duration=10, ScenarioName="ISM_Full"){
  
  
  ### Function to calculate the Average Count Above Threshold =========================================
  
  Func_AveCountAT <- function(data=scenario , Threshold=Threshold , Duration=Duration){
    
    n <- nrow(data)
    nTraces <- ncol(data)-1
    
    ### is data above the threshold or not (loop over all traces) -------
    
    IsAboveThreshold <- data
    IsAboveThreshold [,-1] <- NA
    
    for (j in 1:nTraces){
      for (i in 1:n){
        if (data[i,(1+j)]>Threshold) {
          IsAboveThreshold[i,(1+j)] <- 1
        }else{
          IsAboveThreshold[i,(1+j)] <- 0
        }
      }
    }
    
    
    ### Count above threshold over duration (all the traces) ---------------------
    
    nBlock <- (n-Duration+1)
    CountAboveThreshold <- data.frame(matrix( nrow=nBlock , ncol=(ncol(data)+1)  ))
    colnames(CountAboveThreshold)[1:2] <- c("StartYr", "EndYr")
    colnames(CountAboveThreshold)[3:(nTraces+2)] <- colnames(data)[-1]
    
    
    for (j in 1:nTraces){
      
      for (b in 1:nBlock){
        CountAboveThreshold$StartYr[b] <- data[b,1]
        CountAboveThreshold$EndYr[b] <- data[(b-1+Duration) , 1]
        CountAboveThreshold[b, (j+2)] <- sum(IsAboveThreshold[b:(b-1+Duration), (j+1)])
      }
    }
    
    
    if (nTraces>1){
      AveCountAT <- colMeans(CountAboveThreshold[,-1:-2])
    }else{
      AveCountAT <- mean(CountAboveThreshold[,-1:-2])
    }

    
    assign("CountAboveThreshold", CountAboveThreshold, .GlobalEnv)
    return(AveCountAT)
    
  } ## end of Func_AveCountAT function 
  
  
  
  #### Historical Average Count Above Threshold (will be added as points in boxplot) =================================================
  
  ### Read historical data from HydrologyScenarioso.xlsx 

  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  yr1 <- min(Hist[,1])
  yr2 <- max(Hist[,1])
  
  HistData <- Hist
  
  AveCountAT_hist <- Func_AveCountAT(data=HistData, Threshold , Duration)
  # CountAboveThreshold_hist <- CountAboveThreshold
  
  #### Scenario ======================================================================================
  
  AveCountAT_Scenario <- Func_AveCountAT(data, Threshold , Duration)
  
  
  #### Plot of Moving Count ========================================================================
  
  par(mar=c(6, 4, 3, 4.5) + 0.1 , mgp=c(2.7, 1, 0) )
  
  ### If only one trace -----------------
  nTraces <- ncol(data)-1
  if(nTraces==1){
    
    plot(x=CountAboveThreshold$EndYr , y=CountAboveThreshold[,3],  pch=19, cex=0.7, las=1, 
         ylim=c(0,Duration), xlim=c(min(data[,1]), max(data[,1])) ,
         xlab="Last year of duration", ylab="Count above threshold (yrs)", cex.lab=1.3, xaxt="n")
    lines(x=CountAboveThreshold$EndYr , y=CountAboveThreshold[,3])
    ## minor axis tick
    axis(1, at=seq(min(data[,1]), max(data[,1]), 1), labels=NA, tck=0.015)  
    axis(1, at=seq(min(data[,1]), max(data[,1]), 5), tck=-0.015)  
    axis(2, at=seq(0, 50, 1), labels=NA, tck=0.015)  
    axis(4, las=1, labels=NA)
    axis(4, at=seq(0, 50, 1), labels=NA, tck=0.015)   
    title(main=paste0("\n Moving count above threshold (Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr) \n Ensemble: ", ScenarioName))
    
  }else{  ### More than one trace --> Boxplot ----------------------------
    
    boxplot( t(CountAboveThreshold[,-1:-2]) ,
             # main=paste0("\n Moving count above threshold (Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr) \n Scenario: ", ScenarioName),
             ylab="Count above threshold (yrs)",
             xlim=c( min(data[,1]), max(data[,1]) ), 
             ylim=c(0,Duration),
             at=CountAboveThreshold$EndYr,
             xlab="Last year of duration", 
             cex.lab=1.3,
             cex.axis=1.2,
             xaxt="n",
             las=1,
             cex=0.5)
    
    title(main=paste0("Moving count above threshold (Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr) \n Ensemble: ", ScenarioName),
          adj=0.5, line=0.5)
    
    abline(h=seq(0,Duration,1), col="gray50", lty="dotted")
    
    boxplot( t(CountAboveThreshold[,-1:-2]) ,
             # main=paste0("\n Moving count above threshold (Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr) \n Scenario: ", ScenarioName),
             add=T,
             ylab="Count above threshold (yrs)",
             xlim=c( min(data[,1]), max(data[,1]) ), 
             ylim=c(0,Duration),
             at=CountAboveThreshold$EndYr,
             xlab="Last year of duration", 
             cex.lab=1.3,
             cex.axis=1.2,
             xaxt="n",
             las=1,
             cex=0.5)
    
    axis(1, at=seq(min(data[,1]), max(data[,1]), 1), labels=NA, tck=0.015)  
    axis(1, at=seq(min(data[,1]), max(data[,1]), 5), tck=-0.015, cex.axis=1.2)  
    axis(2, at=seq(0, Duration, 1), labels=NA, tck=0.015)  
    
    axis(4, at=seq(0, Duration, length.out=11), labels= seq(0, 100, 10) , cex.axis=1.2, las=1 )
    axis(4, at=seq(0, Duration, length.out=21), labels=NA, tck=0.015)

    
    ## second axis lable---
    xmin <- par("usr")[1]
    xmax <- par("usr")[2]
    ymin <- par("usr")[3]
    ymax <- par("usr")[4]
    text(x=xmax, y=(ymax+ymin)/10*9 ,  labels="Count above threshold (% of Duration)", pos=4, offset=3.5, col="black", cex=1.3, srt=-90, adj=0.75, xpd=T)
    

    
    abline(h=AveCountAT_hist, col="red")
    legend("topleft", legend="Historical ", 
           cex=0.9, 
           col="red", 
           lty=1,
           bty = "o",
           bg = "white",
           inset=c(0, 0))
    
  }
  
  

  
}  ## End of the main function 
#########################################################################################################







