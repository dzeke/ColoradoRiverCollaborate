##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


###########################################################################################################################################################
####### Function to plot Median ranges of scenarios (decadal or any desired duration)



Func_Median <- function(ScenarioList, duration=10){
  
  
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
        Dist_ComMetric[,(b+1)] <- colMedians(as.matrix(scenario_sub[,-1]))
        
      }else if (b==nblock){  ## last block length may be different 
        scenario_sub <- scenario[ ((b-1)*duration+1) : nrow(scenario) , ]
        Dist_ComMetric[,(b+1)] <- colMedians(as.matrix(scenario_sub[,-1]))
      }
      
    }
    
    ## calculate the metric for the full period -------
    Dist_ComMetric$full <- colMedians(as.matrix(scenario[,-1]))
    return(Dist_ComMetric)
    
  } ### end of the Ranges_ComMetric function 
  
  
  
  ####  Create an empty dataframe to get the final values that we need for heatmap ======================
  
  HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=9))
  colnames(HeatmapValues) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
  HeatmapValues$Scenario <- ScenarioList[,2]
  
  ##### Plot ##############################################################################################################################################
  
  y1 <- 6
  y2 <- 25
  dymax <- 2
  dymin <- 1
  ScenarioNameSize <- 0.92
  par(mar=c(6, 4, 3, 2) + 0.1 , mgp=c(2.7, 1, 0) )
  
  
  ### Color Palette -------------------------------------------------------------
  
  library(RColorBrewer)  # display.brewer.all()
  
  if (nx_max<=9){
    color <- brewer.pal(n=9, name="YlGnBu")
  }else if(nx_max>9){
    color <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#A8DDB5", "#7FCDBB", 
               "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5",
               "#6A51A3", "#807DBA", "#9E9AC8", "#BCBDDC", 
               "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", rep("#969696", times=300) )
  }
  
  colorLastBox <- "#FA9FB5"
  
  
  
  #### Boxplot for decadal and full period ===================================================================
  
  if (nx_max>2){  ## it means that want metric over a duration along with the full period
    
    ### Plot area and plot the first scenario -------------------------------------
    
    ### calculate the metric for the first scenario
    scenario <- eval(parse(text = ScenarioList[1,1]))
    Dist_ComMetric <- Ranges_ComMetric(scenario)
    
    ### Store the values needed for the final heatmap 
    HeatmapValues$Min[1] <- min(Dist_ComMetric$full)
    HeatmapValues$Q10[1] <- quantile(Dist_ComMetric$full, probs=0.10)
    HeatmapValues$Q25[1] <- quantile(Dist_ComMetric$full, probs=0.25)
    HeatmapValues$Median[1] <- median(Dist_ComMetric$full)
    HeatmapValues$Q75[1] <- quantile(Dist_ComMetric$full, probs=0.75)
    HeatmapValues$Q90[1] <- quantile(Dist_ComMetric$full, probs=0.90)
    HeatmapValues$Max[1] <- max(Dist_ComMetric$full)
    
    
    ## color ---------
    
    nx <- ncol(Dist_ComMetric[,-1])
    
    col <- NA
    colMedian <- NA
    for(x in 1:(nx-1)){
      col[x] <- color[x] 
      if (   fivenum(Dist_ComMetric[,(x+1)])[2] == fivenum(Dist_ComMetric[,(x+1)])[4]  ) {
        colMedian[x] <- col[x]
      }else{
        colMedian[x] <- "black"
      }
    }
    
    col[nx] <- colorLastBox
    
    if (   fivenum(Dist_ComMetric[,(nx+1)])[2] == fivenum(Dist_ComMetric[,(nx+1)])[4]  ) {
      colMedian[nx] <- colorLastBox
    }else{
      colMedian[nx] <- "black"
    }
    
    
    ## plot --------
    
    nx <- nboxplot + (nScenarios -1)
    boxplot(Dist_ComMetric[,-1], xlim=c(0.5,nx+0.5),
            # main="Median",
            ylab="Median (maf/yr)",
            ylim=c(y1,y2),
            cex.lab=1.3,
            yaxt="n",
            xaxt="n",
            las=1,
            col=col,
            medcol=colMedian,
            border="black",
            pch=1,
            cex=0.4)
    
    title(main="Median", adj=0.5, line=1.75)
    
    x2 <- ncol(Dist_ComMetric[,-1])
    abline(v = (x2+1) , col="gray70")
    
    axis(2, at=seq(y1, y2, dymax),  tck=-0.015, las=1, cex.axis=1.2)
    axis(2, at=seq(y1, y2, dymin),  labels=NA, tck=0.015, las=1)
    axis(4, at=seq(y1, y2, dymax),  labels=NA, tck=-0.015, las=1, cex.axis=1.2)
    axis(4, at=seq(y1, y2, dymin),  labels=NA, tck=0.015, las=1)
    
    xmin <- par("usr")[1]
    xmax <- par("usr")[2]
    ymin <- par("usr")[3]
    ymax <- par("usr")[4]
    
    text(y=(ymin-(y1-ymin)/2), x=((1+x2)/2+0.5), ScenarioList[1,2], srt=30, pos=2, xpd=T, cex=ScenarioNameSize, offset=0)
    
    
    ### add other scenarios to the current plot ----------------------------------------
    
    if(nScenarios>1){
      
      for (s in 2:nScenarios){
        
        ### calculate the metric for the scenario
        scenario <- eval(parse(text = ScenarioList[s,1]))
        Dist_ComMetric <- Ranges_ComMetric(scenario)
        
        ### Store the values needed for the final heatmap 
        HeatmapValues$Min[s] <- min(Dist_ComMetric$full)
        HeatmapValues$Q10[s] <- quantile(Dist_ComMetric$full, probs=0.10)
        HeatmapValues$Q25[s] <- quantile(Dist_ComMetric$full, probs=0.25)
        HeatmapValues$Median[s] <- median(Dist_ComMetric$full)
        HeatmapValues$Q75[s] <- quantile(Dist_ComMetric$full, probs=0.75)
        HeatmapValues$Q90[s] <- quantile(Dist_ComMetric$full, probs=0.90)
        HeatmapValues$Max[s] <- max(Dist_ComMetric$full)
        
        ## colors --------
        x1 <- x2+2
        x2 <- x1 + ncol(Dist_ComMetric[,-1]) -1
        nx <- ncol(Dist_ComMetric[,-1])
        
        col <- NA
        colMedian <- NA
        for(x in 1:(nx-1)){
          col[x] <- color[x]
          if (   fivenum(Dist_ComMetric[,(x+1)])[2] == fivenum(Dist_ComMetric[,(x+1)])[4]  ) {
            colMedian[x] <- col[x]
          }else{
            colMedian[x] <- "black"
          }
        }
        
        col[nx] <- colorLastBox
        
        if (   fivenum(Dist_ComMetric[,(nx+1)])[2] == fivenum(Dist_ComMetric[,(nx+1)])[4]  ) {
          colMedian[nx] <- colorLastBox
        }else{
          colMedian[nx] <- "black"
        }
        
        ## plot --------
        
        boxplot(Dist_ComMetric[,-1], add=T, at=c(x1:x2), yaxt="n", xaxt="n", col=col, border="black", medcol=colMedian, pch=1,cex=0.4)
        text(y=(ymin-(y1-ymin)/2), x=((x2+x1)/2+1), ScenarioList[s,2], srt=30, pos=2, xpd=T, cex=ScenarioNameSize, offset=0)
        
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
    
    ### Legend: 1st decade, etc.
    if (nx_max<7){
      legend(x=(xmin+xmax)/2, y=ymax+(ymax-y2), 
             legend=c("1st decade", "2nd decade", "3rd decade", paste0(c(4:(nx_max-1)) , "th decade"  ) ,"Full planning period") ,
             fill=c(color[1:(nx_max-1)], colorLastBox), horiz = T, bty="n", xjust=0.5, yjust = 0.5, xpd=T ,x.intersp=0.6, cex=1.1,
             text.width=c(c(strwidth("1st decade  "), strwidth("2nd decade  "), strwidth("3rd decade  "), strwidth("4th decade  "), strwidth("5th decade  "), strwidth("Full planning period"))) )
    }else if (nx_max>=7){
      legend(x=(xmin+xmax)/2, y=ymax+(ymax-y2), legend=c("1st decade", "2nd decade", "3rd decade", paste0(c(4:(nx_max-1)) , "th decade"  ) ,"etc." ,"Full planning period") ,
             fill=c(color[1:5], "white",colorLastBox), horiz = T, bty="n", xjust=0.5, yjust = 0.5, xpd=T,x.intersp=0.6, cex=1.1,
             text.width=c(c(strwidth("1st decade  "), strwidth("2nd decade  "), strwidth("3rd decade  "), strwidth("4th decade  "), strwidth("5th decade  "), strwidth("etc.  "), strwidth("Full planning period"))) )
    }
    
    ### Legend: 10-yr Period 1
    # if (nx_max<7){
    #   legend(x=(xmin+xmax)/2, y=ymax+(ymax-y2), legend=c(paste0(duration, "-yr Period ", c(1:(nx_max-1))) , "Full period") ,
    #          fill=c(color[1:(nx_max-1)], colorLastBox), horiz = T, bty="n", xjust=0.5, yjust = 0.5, xpd=T)
    # }else if (nx_max>=7){  ### add etc. instead of puting all the legends
    #   legend(x=(xmin+xmax)/2, y=ymax+(ymax-y2), legend=c(paste0(duration, "-yr period ", c(1:5) ),"etc.", "Full period") ,
    #          fill=c(color[1:5], "white",colorLastBox), horiz = T, bty="n", xjust=0.5, yjust = 0.5, xpd=T)
    # }
    
    
  }  ## end of if (nx_max>2)
  
  
  
  
  ###### If we want only the boxplot for the full period ===================================================================
  
  if (nx_max<3){
    

    ### Plot area and plot the first scenario -------------------------------------
    
    scenario <- eval(parse(text = ScenarioList[1,1]))
    Dist_ComMetric <- Ranges_ComMetric(scenario)
    
    ### Store the values needed for the final heatmap 
    HeatmapValues$Min[1] <- min(Dist_ComMetric$full)
    HeatmapValues$Q10[1] <- quantile(Dist_ComMetric$full, probs=0.10)
    HeatmapValues$Q25[1] <- quantile(Dist_ComMetric$full, probs=0.25)
    HeatmapValues$Median[1] <- median(Dist_ComMetric$full)
    HeatmapValues$Q75[1] <- quantile(Dist_ComMetric$full, probs=0.75)
    HeatmapValues$Q90[1] <- quantile(Dist_ComMetric$full, probs=0.90)
    HeatmapValues$Max[1] <- max(Dist_ComMetric$full)
    
    ## color ---------
  
    col <- colorLastBox
    
    if (   max(Dist_ComMetric$full) == min(Dist_ComMetric$full ) ) {
      colborder <- colorLastBox
    }else{
      colborder <- "black"
    }
    
    
    ## plot --------
    
    nx <- nScenarios
    boxplot(Dist_ComMetric$full , xlim=c(0.5,nx+0.5),
            main="Median",
            ylab="Median (maf/yr)",
            ylim=c(y1, y2),
            cex.lab=1.3,
            yaxt="n",
            xaxt="n",
            las=1,
            col=col,
            border=colborder)
    

    axis(2, at=seq(y1, y2, dymax),  tck=-0.015, las=1, cex.axis=1.2)
    axis(2, at=seq(y1, y2, dymin),  labels=NA, tck=0.015, las=1)
    axis(4, at=seq(y1, y2, dymax),  labels=NA, tck=-0.015, las=1, cex.axis=1.2)
    axis(4, at=seq(y1, y2, dymin),  labels=NA, tck=0.015, las=1)
    axis(1, at=seq(1, nx, 1),  labels=NA, tck=-0.015, las=1, cex.axis=1.2)

    ymin <- par("usr")[3]
    text(y=(ymin-(y1-ymin)), x=1.5, ScenarioList[1,2], srt=30, pos=2, xpd=T)
    
    
    ### add other scenarios to the current plot ----------------------------------------
    
    if(nScenarios>1){
      
      for (s in 2:nScenarios){
        
        scenario <- eval(parse(text = ScenarioList[s,1]))
        Dist_ComMetric <- Ranges_ComMetric(scenario)
        
        ### Store the values needed for the final heatmap 
        HeatmapValues$Min[s] <- min(Dist_ComMetric$full)
        HeatmapValues$Q10[s] <- quantile(Dist_ComMetric$full, probs=0.10)
        HeatmapValues$Q25[s] <- quantile(Dist_ComMetric$full, probs=0.25)
        HeatmapValues$Median[s] <- median(Dist_ComMetric$full)
        HeatmapValues$Q75[s] <- quantile(Dist_ComMetric$full, probs=0.75)
        HeatmapValues$Q90[s] <- quantile(Dist_ComMetric$full, probs=0.90)
        HeatmapValues$Max[s] <- max(Dist_ComMetric$full)
        
        ## colors --------

        col <- colorLastBox
        
        if (   max(Dist_ComMetric$full) == min(Dist_ComMetric$full ) ) {
          colborder <- colorLastBox
        }else{
          colborder <- "black"
        }
        
        ## plot --------
        
        boxplot(Dist_ComMetric$full , add=T, at=s, yaxt="n", xaxt="n", col=col, border=colborder)
        text(y=(ymin-(y1-ymin)), x=s+0.5, ScenarioList[s,2], srt=30, pos=2, xpd=T)
        

      } ## end of loop on scenarios
      
    } ## end of is nScenario>1
    
    
    #### add legend --------------------------------------------------------
    
    xmin <- par("usr")[1]
    xmax <- par("usr")[2]
    ymin <- par("usr")[3]
    ymax <- par("usr")[4]
    
    legend(x=(xmin+xmax)/2, y=ymax+(ymax-y2), legend= "Full period" ,
           fill=col, horiz = T, bty="n", xjust=0.5, yjust = 0.5, xpd=T)
    
    
  }  ## end of if (nx_max<3) ====================================================================
  
  
  
  ##########################################################################################################
  #### Add lines for the Historical median ===.====================
  
  ## read Hist data ------
  
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  yr1 <- min(Hist[,1])
  yr2 <- max(Hist[,1])
  
  # yr1 <- 1906
  # yr2 <- 2020
  # HistInfo <- data.frame("ExcelSheet"="Hist", "Name_On_Plots"="Historical", "StartYr"=yr1, "EndYr"=yr2)
  # source("./R_Files/Func_read_required_hydrology_scenarios_from_xlsx.R")
  # Func_read_required_hydrology_scenarios_from_xlsx(HistInfo)
  
  ## long term mean and millennium drought mean
  HistMetric <- round(median(Hist[,2]), digits=2)
  MillMetric <- round( median( Hist[which( Hist[,1]>=2000 & Hist[,1]<=yr2) ,2]), digits=2)
  
  abline(h=HistMetric, col="red", lwd=2)  ## 1906-2021(WY): 14.36 maf/yr
  abline(h=MillMetric, col="red", lwd=2, lty="longdash")  ## 2000-2021(WY): 12.08 maf/yr   2000-2019: 12.7 maf/yr
  
  legend("topright",
         legend=c(paste0("Historical long-term median  \n (", yr1, "-", yr2, "): ", HistMetric," maf/yr  "), 
                  paste0("Millennium drought median \n  (2000-", yr2, "): ", MillMetric," maf/yr  " ) ),
         cex=0.9,
         col=c("red", "red" ),
         lwd=1.5, lty=c(1, 5), 
         inset=c(0.02, 0.03), 
         bty = "o",
         bg = "white",
         box.col = "grey",
         y.intersp=1.5,  ## vertical distance between two symbols
         x.intersp=1)
  
  
  ## store the historical value of the metric in the heatmap dataframe
  HeatmapValues$Hist <- HistMetric
  
  
  ##### add the heatmap dataframe output to Global Environment ##################################################################################
  
  assign("HM_Median", HeatmapValues, .GlobalEnv)

} ## end of the main function: Func_Median   ################################################################################################################








