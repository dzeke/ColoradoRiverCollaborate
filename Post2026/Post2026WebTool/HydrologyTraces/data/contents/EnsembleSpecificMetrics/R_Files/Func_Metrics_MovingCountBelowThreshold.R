##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


#########################################################################################################
###### Function to calculate the average count below threshold for the selected scenarios
###### and then create the boxplot of the distribution of all traces of each scenario

Func_MovingCountBelowThreshold <- function(data , Threshold=14.65 , Duration=10, ScenarioName="ISM_Full"){
  
  
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
    

    assign("CountBelowThreshold", CountBelowThreshold, .GlobalEnv)
    return(AveCount)
    
  } ## end of Func_AveCountBT function 
  
  
  
  #### Historical Average Count Below Threshold (will be added as points in boxplot) =================================================
  
  ## read Hist data
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  yr1 <- min(Hist[,1])
  yr2 <- max(Hist[,1])

  
  AveCountBT_hist <- Func_AveCountBT(data=Hist, Threshold , Duration)
  # CountBelowThreshold_hist <- CountBelowThreshold
  
  #### Scenario ======================================================================================
  
  AveCountBT_Scenario <- Func_AveCountBT(data, Threshold , Duration)
  
  
  #### Plot of Moving Count ========================================================================
  
  # par(mar=c(6, 4, 3, 4.5) + 0.1 , mgp=c(2.7, 1, 0) )
  par(mar=c(6, 5, 4, 5) + 0.1 , mgp=c(2.7, 1, 0) )
  
  ### If only one trace -----------------
  nTraces <- ncol(data)-1
  if(nTraces==1){
    
    plot(x=CountBelowThreshold$EndYr , y=CountBelowThreshold[,3],  pch=19, cex=0.7, las=1, 
         ylim=c(0,Duration), xlim=c(min(data[,1]), max(data[,1])) ,
         xlab="Last year of duration", ylab="Count below threshold (yrs)", cex.lab=1.3, xaxt="n")
    lines(x=CountBelowThreshold$EndYr , y=CountBelowThreshold[,3])
    ## minor axis tick
    axis(1, at=seq(min(data[,1]), max(data[,1]), 1), labels=NA, tck=0.015)  
    axis(1, at=seq(min(data[,1]), max(data[,1]), 5), tck=-0.015)  
    axis(2, at=seq(0, 50, 1), labels=NA, tck=0.015)  
    axis(4, las=1, labels=NA)
    axis(4, at=seq(0, 50, 1), labels=NA, tck=0.015)   
    title(main=paste0("\n Moving Count Below Threshold (Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr) \n ", ScenarioName))
    
  }else{  ### More than one trace --> Boxplot ----------------------------
    
    boxplot( t(CountBelowThreshold[,-1:-2]) ,
             # main=paste0("\n Moving count below threshold (Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr) \n Ensemble: ", ScenarioName),
             ylab="Count below threshold (yrs)",
             xlim=c( min(data[,1]), max(data[,1]) ), 
             ylim=c(0,Duration),
             at=CountBelowThreshold$EndYr,
             xlab="Last year of duration", 
             cex.lab=1.2,
             cex.axis=1.1,
             xaxt="n",
             las=1,
             cex=0.5)
    
    title(main=paste0("Moving count below threshold (Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr) \n ", ScenarioName),
          adj=0.5, line=0.5, cex.main=1.1 )
    
    abline(h=seq(0,Duration,1), col="gray50", lty="dotted")
    
    boxplot( t(CountBelowThreshold[,-1:-2]) ,
             # main=paste0("\n Moving count below threshold (Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr) \n Ensemble: ", ScenarioName),
             add=T,
             ylab="Count below threshold (yrs)",
             xlim=c( min(data[,1]), max(data[,1]) ), 
             ylim=c(0,Duration),
             at=CountBelowThreshold$EndYr,
             xlab="Last year of duration", 
             cex.lab=1.2,
             cex.axis=1.1,
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
    text(x=xmax, y=ymax ,  labels="Count below threshold (% of Duration)", pos=4, offset=3.5, col="black", adj = c(0,0), cex=1.2, srt=-90, xpd=T)
    
    
    # for (j in 2:nTraces){
    #   points(x=CountBelowThreshold$EndYr , y=CountBelowThreshold[,(2+j)],  pch=19, cex=0.7)
    #   lines(x=CountBelowThreshold$EndYr , y=CountBelowThreshold[,(2+j)])
    # }
    
    abline(h=AveCountBT_hist, col="red")
    legend("topleft", legend="Historical ", 
           cex=0.8, 
           col="red", 
           lty=1,
           # seg.len=1,
           bty = "n",
           bg = "white",
           inset=c(0, 0))
    
  }
  
  

  
}  ## End of the main function 
#########################################################################################################







