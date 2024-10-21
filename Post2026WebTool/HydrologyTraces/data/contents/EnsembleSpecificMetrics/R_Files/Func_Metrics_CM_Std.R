##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


###########################################################################################################################################################
####### Function to plot Std ranges of scenarios (decadal or any desired duration)



Func_Std <- function(ScenarioList, duration=10){
  
  
  nScenarios <- nrow(ScenarioList)
  
  #### How many boxplots do we have in our plot? ##########################################################################################################
  #### To specify the number of boxplots in the plot 
  
  nboxplot <- 0
  nxAll <- NA
  for (s in 1:nScenarios){
    
    scenario <- eval(parse(text = ScenarioList[s,1]))
    nyrs <- nrow(scenario)
    
    #### specify the number of blocks (or duration or moving period) that we want to see the statistics for (e.g. decadal)
    if( (nyrs%%duration)==0 ){
      nblock <- (nyrs/duration)
    } else if ( (nyrs%%duration)<=(duration%/%2) ){   ## when the remaining years in the last block are less than duration/2
      nblock <- (nyrs%/%duration)
    }else if ( (nyrs%%duration)>(duration%/%2) ) {   ## when the the remaining years in the last block are more than duration/2 ==> take it as another block
      nblock <- (nyrs%/%duration) + 1
    }
    
    nxAll[s] <- nblock
    nboxplot <- nboxplot + nblock
    
  }
  
  nx_max <- max(nxAll) +1  ## max number of boxplots for a scenario 
  nboxplot <- nboxplot + nScenarios   ## +nScenarios to add boxplots of full periods
  
  
  
  ### Function to calculate the metric for duration such as decadal #######################################################################################
  
  Ranges_ComMetric <- function(scenario){  
    
    ntraces <- ncol(scenario)-1    
    nyrs <- nrow(scenario)
    
    #### specify the length and the number of blocks (or duration or moving period) that we want to see the statistics for (e.g. decadal)
    if( (nyrs%%duration)==0 ){
      nblock <- (nyrs/duration)
      LastBlockLen <- duration
    } else if ( (nyrs%%duration)<=(duration%/%2) ){   ## when the remaining years in the last block are less than duration/2
      nblock <- (nyrs%/%duration)
      LastBlockLen <- duration + (nyrs%%duration)
    }else if ( (nyrs%%duration)>(duration%/%2) ) {   ## when the the remaining years in the last block are more than duration/2 ==> take it as another block
      nblock <- (nyrs%/%duration) + 1
      LastBlockLen <- nyrs%%duration
    }
    
    ### define an empty dataframe for the outputs ----------------
    Dist_ComMetric <- data.frame(matrix(nrow=ntraces, ncol=(nblock+2)))
    colnames(Dist_ComMetric) <- c("trace", paste0("d",1:nblock), "full")
    Dist_ComMetric$trace <- 1:ntraces
    
    ## calculate the metric for each period ----------------------
    
    library("matrixStats")  ## to calculate statistics over columns
    
    for(b in 1:nblock){
      
      if(b<nblock){
        scenario_sub <- scenario[ ((b-1)*duration+1) : (b*duration) , ]
        Dist_ComMetric[,(b+1)] <- colSds(as.matrix(scenario_sub[,-1]))
        
      }else if (b==nblock){  ## last block length may be different 
        scenario_sub <- scenario[ ((b-1)*duration+1) : nrow(scenario) , ]
        Dist_ComMetric[,(b+1)] <- colSds(as.matrix(scenario_sub[,-1]))
      }
      
    }
    
    ## calculate the metric for the full period -------
    Dist_ComMetric$full <- colSds(as.matrix(scenario[,-1]))
    return(Dist_ComMetric)
    
  } ### end of the Ranges_ComMetric function 
  
  
  
  ####  Create an empty dataframe to get the final values that we need for heatmap ======================
  
  HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=7))
  colnames(HeatmapValues) <- c("Scenario", "Q10", "Q25", "Median", "Q75", "Q90", "Hist")
  HeatmapValues$Scenario <- ScenarioList[,2]
  
  ##### Plot ##############################################################################################################################################
  
  ### determine historical metric -----
  
  ## read Hist data
  
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  yr1 <- min(Hist[,1])
  yr2 <- max(Hist[,1])
  
 
  ## historical metric
  HistMetric <- round(sd(Hist[,2]), digits=2)
  
  # y1 <- 0
  # y2 <- 12
  # dymax <- 2
  # dymin <- 1
  ScenarioNameSize <- 0.92
  
  
  
  ### Color Palette -------------------------------------------------------------
  
  # library(RColorBrewer)  # display.brewer.all()
  # brewer.pal(n=9, name="Greys")
  
  if (nx_max<=9){
    # color <- RColorBrewer::brewer.pal(n=9, name="YlGnBu")
    color <- viridis::viridis_pal(alpha = 0.5, direction = -1, option = "D")(9)
  }else if(nx_max>9 & nx_max<22){
    # color <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#A8DDB5", "#7FCDBB",
    #            "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5",
    #            "#6A51A3", "#807DBA", "#9E9AC8", "#BCBDDC",
    #            "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FCA537FF", "#FB9F3AFF", "#F79342FF", "#F48849FF", rep("#969696", times=300) )
    
    color <- viridis::viridis_pal(alpha = 0.5, direction = -1, option = "D")(20)
    
  }else if(nx_max>=22){
    color <- viridis::viridis_pal(alpha = 0.5, direction = -1, option = "D")(50)
    
  }
  
  colorLastBox <- "#FA9FB5"
  
  
  
  #### Boxplot for decadal and full period ===================================================================
  
  if (nx_max>2){  ## it means that want metric over a duration along with the full period
    
    ### Plot area and plot the first scenario -------------------------------------
    
    ### calculate the metric for the first scenario
    scenario <- eval(parse(text = ScenarioList[1,1]))
    Dist_ComMetric <- Ranges_ComMetric(scenario)
    
    ### Store the values needed for the final heatmap 
    HeatmapValues$Q10[1] <- quantile(Dist_ComMetric$full, probs=0.10)
    HeatmapValues$Q25[1] <- quantile(Dist_ComMetric$full, probs=0.25)
    HeatmapValues$Median[1] <- median(Dist_ComMetric$full)
    HeatmapValues$Q75[1] <- quantile(Dist_ComMetric$full, probs=0.75)
    HeatmapValues$Q90[1] <- quantile(Dist_ComMetric$full, probs=0.90)
    
    
    ## color ---------
    
    nx <- ncol(Dist_ComMetric[,-1])
    
    col <- NA
    colMedian <- NA
    for(x in 1:(nx-1)){
      col[x] <- color[x]
      if (   ( fivenum(Dist_ComMetric[,(x+1)])[4] - fivenum(Dist_ComMetric[,(x+1)])[2] ) < 0.00001 ) {
        colMedian[x] <- col[x]
      }else{
        colMedian[x] <- "black"
      }
    }
    
    col[nx] <- colorLastBox
    
    if (   ( fivenum(Dist_ComMetric[,(nx+1)])[4] - fivenum(Dist_ComMetric[,(nx+1)])[2]  ) < 0.00001 ) {
      colMedian[nx] <- colorLastBox
    }else{
      colMedian[nx] <- "black"
    }
    
    
    ## plot --------
    
    ## plot properties ..........
    y1 <- floor(min(Dist_ComMetric[,-1], HistMetric)) 
    y2 <- ceiling(max(Dist_ComMetric[,-1], HistMetric))  
    dymax <- ceiling((y2-y1)/6)  
    if(dymax%%2==0) {dymin <- dymax/2} else if (dymax==1) {dymin=0.5} else {dymin=1}
    
    nx <- nboxplot + (nScenarios -1)
    boxplot(Dist_ComMetric[,-1], xlim=c(0.5,nx+0.5),
            # main="Standard Deviation (Std)",
            ylab="Standard Deviation (maf/yr)",
            ylim=c(y1,y2),
            cex.lab=1.3,
            yaxt="n",
            xaxt="n",
            las=1,
            col=col,
            medcol=colMedian,
            border="black",
            pch=1,
            cex=0.4,
            boxwex=0.5)
    
    title(main="Standard Deviation", adj=0.5, line=2)
    text(paste0(ScenarioList[2]),
         x=(par("usr")[1] + par("usr")[2])/2,
         y=(par("usr")[4]),
         xpd=T,
         adj=c(0.5,-1.2),
         cex=1)
    
    x2 <- ncol(Dist_ComMetric[,-1])
    abline(v = (x2+1) , col="gray70")
    
    axis(2, at=seq(y1, y2, dymax),  tck=-0.03, las=1, cex.axis=1.2)
    axis(2, at=seq(y1, y2, dymin),  labels=NA, tck=0.015, las=1)
    axis(4, at=seq(y1, y2, dymax),  labels=NA, tck=-0.03, las=1, cex.axis=1.2)
    axis(4, at=seq(y1, y2, dymin),  labels=NA, tck=0.015, las=1)
    
    xmin <- par("usr")[1]
    xmax <- par("usr")[2]
    ymin <- par("usr")[3]
    ymax <- par("usr")[4]
    
    # text(y=(ymin-(y1-ymin)/2), x=((1+x2)/2+0.5), ScenarioList[1,2], srt=30, pos=2, xpd=T, cex=ScenarioNameSize, offset=0)
    
    
    ### add other scenarios to the current plot ----------------------------------------
    
    if(nScenarios>1){
      
      for (s in 2:nScenarios){
        
        ### calculate the metric for the scenario
        scenario <- eval(parse(text = ScenarioList[s,1]))
        Dist_ComMetric <- Ranges_ComMetric(scenario)
        
        ### Store the values needed for the final heatmap 
        HeatmapValues$Q10[s] <- quantile(Dist_ComMetric$full, probs=0.10)
        HeatmapValues$Q25[s] <- quantile(Dist_ComMetric$full, probs=0.25)
        HeatmapValues$Median[s] <- median(Dist_ComMetric$full)
        HeatmapValues$Q75[s] <- quantile(Dist_ComMetric$full, probs=0.75)
        HeatmapValues$Q90[s] <- quantile(Dist_ComMetric$full, probs=0.90)
        
        ## colors --------
        x1 <- x2+2
        x2 <- x1 + ncol(Dist_ComMetric[,-1]) -1
        nx <- ncol(Dist_ComMetric[,-1])
        
        col <- NA
        colMedian <- NA
        for(x in 1:(nx-1)){
          col[x] <- color[x]
          if ( ( fivenum(Dist_ComMetric[,(x+1)])[4] - fivenum(Dist_ComMetric[,(x+1)])[2] ) < 0.00001 ) {
            colMedian[x] <- col[x]
          }else{
            colMedian[x] <- "black"
          }
        }
        
        col[nx] <- colorLastBox
        
        if (  ( fivenum(Dist_ComMetric[,(nx+1)])[4] - fivenum(Dist_ComMetric[,(nx+1)])[2]  ) < 0.00001  ) {
          colMedian[nx] <- colorLastBox
        }else{
          colMedian[nx] <- "black"
        }
        
        ## plot --------
        
        boxplot(Dist_ComMetric[,-1], add=T, at=c(x1:x2), yaxt="n", xaxt="n", col=col, border="black", medcol=colMedian,pch=1,cex=0.4, boxwex=0.5)
        # text(y=(ymin-(y1-ymin)/2), x=((x2+x1)/2+1), ScenarioList[s,2], srt=30, pos=2, xpd=T, cex=ScenarioNameSize, offset=0)
        
        if(s!=nScenarios){
          abline(v = (x2+1) , col="gray70")
        }
        
        
      } ## end of loop on scenarios
      
    } ## end of is nScenario>1
    
    
    #### add legend --------------------------------------------------------
    
    xmin <- par("usr")[1]
    xmax <- par("usr")[2]
    ymin <- par("usr")[3]
    ymax <- par("usr")[4]
    
    
    #### Only for decadal periods --------------
    if (nx_max<=4){ ## 3 decades + Full
      legend(x=xmax, y=ymax, legend=c("1st decade", "2nd decade", "3rd decade", "Full planning period") ,
             fill=c(color[1:(nx_max-1)], colorLastBox), horiz = F, bty="n", xjust=0, yjust = 1, x.intersp=0.4 , y.intersp=1.2, cex=0.8, xpd=T)
    }else if (nx_max>=5 & nx_max<=9){ ## 4 to 8 decades + Full
      legend(x=xmax, y=ymax, legend=c("1st decade", "2nd decade", "3rd decade", paste0(c(4:(nx_max-1)) , "th decade"  ) ,"Full planning period") ,
             fill=c(color[1:(nx_max-1)], colorLastBox), horiz = F, bty="n", xjust=0, yjust = 1,x.intersp=0.4 , y.intersp=1.2, cex=0.8, xpd=T)
    }else if(nx_max>=10){ ## more than 8 decades + Full
      legend(x=xmax, y=ymax, legend=c("1st decade", "2nd decade", "3rd decade", paste0(c(4:5) , "th decade"  ) ,"etc.","Full planning period") ,
             fill=c(color[1:5], "white",colorLastBox), horiz = F, bty="n", xjust=0, yjust = 1,x.intersp=0.4 , y.intersp=1.2, cex=0.8, xpd=T)
    }
    
    
  }  ## end of if (nx_max>2)
  
  
  
  
  ###### If we want only the boxplot for the full period ===================================================================
  
  if (nx_max<3){
    

    ### Plot area and plot the first scenario -------------------------------------
    
    scenario <- eval(parse(text = ScenarioList[1,1]))
    Dist_ComMetric <- Ranges_ComMetric(scenario)
    
    ### Store the values needed for the final heatmap 
    HeatmapValues$Q10[1] <- quantile(Dist_ComMetric$full, probs=0.10)
    HeatmapValues$Q25[1] <- quantile(Dist_ComMetric$full, probs=0.25)
    HeatmapValues$Median[1] <- median(Dist_ComMetric$full)
    HeatmapValues$Q75[1] <- quantile(Dist_ComMetric$full, probs=0.75)
    HeatmapValues$Q90[1] <- quantile(Dist_ComMetric$full, probs=0.90)
    
    ## color ---------
  
    col <- colorLastBox
    
    if (  ( max(Dist_ComMetric$full) - min(Dist_ComMetric$full ) < 0.001 ) ) {
      colborder <- colorLastBox
    }else{
      colborder <- "black"
    }
    
    
    ## plot --------
    
    ## plot properties ..........
    y1 <- floor(min(Dist_ComMetric[,-1], HistMetric)) 
    y2 <- ceiling(max(Dist_ComMetric[,-1], HistMetric))  
    dymax <- ceiling((y2-y1)/6)  
    if(dymax%%2==0) {dymin <- dymax/2} else if (dymax==1) {dymin=0.5} else {dymin=1}
    
    nx <- nScenarios
    boxplot(Dist_ComMetric$full , xlim=c(0.5,nx+0.5),
            # main="Standard Deviation",
            ylab="Std (maf/yr)",
            ylim=c(y1, y2),
            cex.lab=1.3,
            yaxt="n",
            xaxt="n",
            las=1,
            col=col,
            border=colborder,
            boxwex=0.5)
    
    title(main="Standard Deviation", adj=0.5, line=2)
    text(paste0(ScenarioList[2]),
         x=(par("usr")[1] + par("usr")[2])/2,
         y=(par("usr")[4]),
         xpd=T,
         adj=c(0.5,-1.2),
         cex=1)   

    axis(2, at=seq(y1, y2, dymax),  tck=-0.03, las=1, cex.axis=1.2)
    axis(2, at=seq(y1, y2, dymin),  labels=NA, tck=0.015, las=1)
    axis(4, at=seq(y1, y2, dymax),  labels=NA, tck=-0.03, las=1, cex.axis=1.2)
    axis(4, at=seq(y1, y2, dymin),  labels=NA, tck=0.015, las=1)
    # axis(1, at=seq(1, nx, 1),  labels=NA, tck=-0.015, las=1, cex.axis=1.2)

    ymin <- par("usr")[3]
    # text(y=(ymin-(y1-ymin)), x=1.5, ScenarioList[1,2], srt=30, pos=2, xpd=T)
    
    
    ### add other scenarios to the current plot ----------------------------------------
    
    if(nScenarios>1){
      
      for (s in 2:nScenarios){
        
        scenario <- eval(parse(text = ScenarioList[s,1]))
        Dist_ComMetric <- Ranges_ComMetric(scenario)
        
        ### Store the values needed for the final heatmap 
        HeatmapValues$Q10[s] <- quantile(Dist_ComMetric$full, probs=0.10)
        HeatmapValues$Q25[s] <- quantile(Dist_ComMetric$full, probs=0.25)
        HeatmapValues$Median[s] <- median(Dist_ComMetric$full)
        HeatmapValues$Q75[s] <- quantile(Dist_ComMetric$full, probs=0.75)
        HeatmapValues$Q90[1] <- quantile(Dist_ComMetric$full, probs=0.90)
        
        ## colors --------

        col <- colorLastBox
        
        if (   ( max(Dist_ComMetric$full) - min(Dist_ComMetric$full ) < 0.001 ) ) {
          colborder <- colorLastBox
        }else{
          colborder <- "black"
        }
        
        ## plot --------
        
        boxplot(Dist_ComMetric$full , add=T, at=s, yaxt="n", xaxt="n", col=col, border=colborder, boxwex=0.5)
        # text(y=(ymin-(y1-ymin)), x=s+0.5, ScenarioList[s,2], srt=30, pos=2, xpd=T)
        

      } ## end of loop on scenarios
      
    } ## end of is nScenario>1
    
    
    #### add legend --------------------------------------------------------
    
    xmin <- par("usr")[1]
    xmax <- par("usr")[2]
    ymin <- par("usr")[3]
    ymax <- par("usr")[4]
    
    legend(x=(xmin+xmax)/2, y=ymax+(ymax-y2), legend= "Full planning period" ,
           fill=col, horiz = T, bty="n", xjust=0.5, yjust = 0.5, xpd=T)
    
    
  }  ## end of if (nx_max<3) ====================================================================
  
  
  ##########################################################################################################
  #### Add line for the historical Std =======================
  
  ## read Hist data
  
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  yr1 <- min(Hist[,1])
  yr2 <- max(Hist[,1])
  
 
  ## historical std
  HistMetric <- round(sd(Hist[,2]), digits=2)
  
  ## add line
  abline(h=HistMetric, col="red", lwd=2, lty="solid")  ## 1906-2021: 4.31 maf/yr
  
 
  ## add legend on the left and outside the plot -----------------
  
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  
  legend(x=xmax, y=(ymax+ymin)/2, xjust=0, yjust = 1.3, xpd=T,
         legend=paste0("Historical \n(", yr1, "-", yr2, "): \n", HistMetric, " maf/yr  " ),
         cex=0.8,
         col="red",
         lwd=1.5, lty="solid", 
         # seg.len=1.5,
         # inset=c(0.02, 0.03), 
         bty = "n",
         bg = "white",
         box.col = "grey",
         y.intersp=1.8,  ## vertical distance between two symbols
         x.intersp=0.5)
  
  
  
  ## store the historical value of the metric in the heatmap dataframe
  HeatmapValues$Hist <- HistMetric
  
  
  
  ##### add the heatmap dataframe output to Global Environment ##################################################################################
  
  assign("HM_Std", HeatmapValues, .GlobalEnv)
  
} ## end of the main function: Func_Std   ################################################################################################################








