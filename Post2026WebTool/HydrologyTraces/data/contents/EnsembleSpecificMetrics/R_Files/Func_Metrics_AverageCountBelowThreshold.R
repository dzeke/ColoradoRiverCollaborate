##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


#########################################################################################################
###### Function to calculate the average count below threshold for the selected scenarios
###### and then create the boxplot of the distribution of all traces of each scenario

Func_AverageCountBelowThreshold <- function(ScenarioList , Threshold=14.65 , Duration=10){
  
  
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
  

  ## read Hist data
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  yr1 <- min(Hist[,1])
  yr2 <- max(Hist[,1])

  AveCountBT_hist <- Func_AveCountBT(data=Hist, Threshold , Duration)
  
  ####  Create an empty dataframe to get the final values that we need for heatmap ======================
  
  HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=7))
  colnames(HeatmapValues) <- c("Scenario", "Q10", "Q25", "Median", "Q75", "Q90", "Hist")
  HeatmapValues$Scenario <- ScenarioList[,2]
  
  ## store the historical value of the metric in the heatmap dataframe
  HeatmapValues$Hist <- AveCountBT_hist
  
  ### boxplot of Average Count ========================================================================
  
  # par(mar=c(6, 4, 3, 4.5) + 0.1 , mgp=c(2.7, 1, 0) )
  
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
  # title(main=paste0("Average Count Below Threshold \n (Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr)" ), adj=0.5, line=0.8)
  
  title(main="Count Below Threshold" , adj=0.5, line=2)
  # title(main=paste0("Duration: ", Duration, " yrs; Threshold: ", Threshold, " maf/yr" ), adj=0.5, line=0.6, cex.main=0.5 )
  text(paste0(ScenarioList[2]),
       x=(par("usr")[1] + par("usr")[2])/2,
       y=(par("usr")[4]),
       xpd=T,
       adj=c(0.5,-1.2),
       cex=1)
  
  
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  
  ### write variables at the rigth side of the plot
  legend(x=(par("usr")[2]), y=(par("usr")[4]),
         legend=paste0("Threshold = \n ", Threshold, " maf/yr \n",  "Duration = ", Duration, " yrs") ,
         horiz = F, bty="n", xjust=-0.05, yjust = 1, x.intersp=0.5 , y.intersp=2, cex=0.8, xpd=T)
  
  
  
  # abline(h=seq(0,Duration,1), col="gray50", lty="dotted")

  # axis(1, at=c(1:n_Scenario), labels=NA) 
  axis(2, at=seq(0, Duration, 1), labels=NA, tck=0.015)  
  axis(4, at=seq(0, Duration, 1), labels=NA, tck=0.015) 
  axis(4, at=seq(0, Duration, 2), labels=NA) 


    ### add each boxplot to the current plot ----------
  
  ymin <- par("usr")[3]
  
  for (s in 1:n_Scenario){
    AveCountBT_Scenario <- Func_AveCountBT ( data = eval(parse(text = ScenarioList[s,1])), Threshold , Duration  )
    
    ### Store the values needed for the final heatmap 
    HeatmapValues$Q10[s] <- quantile(AveCountBT_Scenario, probs=0.10)
    HeatmapValues$Q25[s] <- quantile(AveCountBT_Scenario, probs=0.25)
    HeatmapValues$Median[s] <- median(AveCountBT_Scenario)
    HeatmapValues$Q75[s] <- quantile(AveCountBT_Scenario, probs=0.75)
    HeatmapValues$Q90[s] <- quantile(AveCountBT_Scenario, probs=0.90)
    
    i <- s
    boxplot(AveCountBT_Scenario, add=T, at=s, yaxt="n", xaxt="n" , cex=0.5)
    # points(x=s, y=AveCountBT_hist,
    #        pch=16, cex=0.8 , col="red")
    
    # if(s!=n_Scenario){
    #   abline(v = (i+3) , col="gray70")
    # }
    
    # text(x=(i), y=-(Duration/10), ScenarioList[s,2], srt=30, pos=2, offset=0.1 , cex=0.8, xpd=T)
    # text(x=(i+0.5), y=ymin*2, ScenarioList[s,2], srt=30, pos=2, offset=0.3 , cex=0.9, xpd=T)
    
      }
  
  ### historical averge count -----------
  abline(h=AveCountBT_hist, col="red")  

  legend(x=xmax, y=(ymax+ymin)/2, xjust=-0.05, yjust = 1, xpd=T,
         legend=paste0("Historical \n(", yr1, "-", yr2, "): \n", round(AveCountBT_hist, digits=2)," yrs  "),
         cex=0.8,
         col=c("red" ),
         lwd=1.5, lty="solid", 
         # seg.len=1,
         # inset=c(0.02, 0.03), 
         bty = "n",
         bg = "white",
         box.col = "grey",
         y.intersp=1.8,  ## vertical distance between two symbols
         x.intersp=0.5)
  
  
  # ### save the current plot as pdf ========================================================================
  # 
  # pdfName <- paste0("./Results/CountBelowThreshold/AverageCountBelowThreshold_", Duration, "yr.pdf")
  # dev.copy2pdf(file=pdfName, width = 10, height = 5)
  # 
  
  
  ##### add the heatmap dataframe output to Global Environment ##################################################################################
  
  assign("HM_ACBT", HeatmapValues, .GlobalEnv)
  
  
}  ## End of the main function 
#########################################################################################################







