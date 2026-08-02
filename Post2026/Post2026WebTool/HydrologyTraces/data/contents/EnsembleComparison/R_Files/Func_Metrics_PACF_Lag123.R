##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################

##### This function takes the hydrology scenario dataframe in which the first column is year and the others are flow traces.
##### Then calculate PACF lag 1, 2, and 3 for all the traces in a hydrology scenario (a dataframe with 3 columns: lag1, lag2, and lag3, and each row corresponds to one trace). 
##### The output is a boxplot showing the distribution of PACF for the required hydrology scenarios


Func_PACF_Lag123 <- function (ScenarioList){
  
  
  ####  Create an empty dataframe to get the final values that we need for heatmap ======================
  
  HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=9))
  colnames(HeatmapValues) <- c("Scenario", "Min", "Q10", "Q25", "Median", "Q75", "Q90", "Max", "Hist")
  HeatmapValues$Scenario <- ScenarioList[,2]
  
  
  #### Historical PACF lag 1,2,3 (will be added as points in boxplot) =================================================
  
  ## read Hist data ------
  
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  yr1 <- min(Hist[,1])
  yr2 <- max(Hist[,1])

  
  PACF_hist <- pacf(Hist[,2], plot=F)
  PACF1_hist <- PACF_hist$acf[which(PACF_hist$lag==1)]
  PACF2_hist <- PACF_hist$acf[which(PACF_hist$lag==2)]
  PACF3_hist <- PACF_hist$acf[which(PACF_hist$lag==3)]
  
  HeatmapValues$Hist <- PACF1_hist
  
  #### Compare scenarios, boxplots of PACF lag 1,2,3 ===================================================================
  
  ### Function to calculate PACF lag 1,2,3 for all the traces of a hydrology scenario ---------------
  
  PACF123 <- function (scenario){
    
    ntraces <- ncol(scenario)-1      
    
    PACF <- data.frame(matrix(nrow=ntraces, ncol=3))
    colnames(PACF) <- c("lag1", "lag2", "lag3")
    rownames(PACF) <- colnames(scenario[-1])
    
    ## Lag 1,2,3
    for(t in 1:ntraces){
      pacf_list <- pacf(scenario[,(t+1)], plot=FALSE)
      PACF$lag1[t] <- pacf_list$acf[which(pacf_list$lag==1)]   ## or: pacf_list$acf[2] 
      PACF$lag2[t] <- pacf_list$acf[which(pacf_list$lag==2)] 
      PACF$lag3[t] <- pacf_list$acf[which(pacf_list$lag==3)] 
    }
    
    return(PACF)
  }
  
  
  ### boxplot --------------------------------------------------------------------------------------
  PACF_Scenario1 <- PACF123( scenario = eval(parse(text = ScenarioList[1,1])) )
  
  n_Scenario <- nrow(ScenarioList)
  
  nx <- 3*n_Scenario + (n_Scenario -1)
  boxplot(PACF_Scenario1, xlim=c(0.5, nx+0.5),
          # main="Partial Autocorrelation Function (PACF) at lags 1 to 3",
          ylab="PACF",
          cex.lab=1.3,
          cex.axis=1.2,
          ylim=c(-0.5,0.7),
          xaxt="n",
          las=1,
          cex=0.5,
          col="white",
          border="white")
  
  title(main="Partial Autocorrelation Function (PACF) at lags 1 to 3", adj=0.5, line=1.75)
  
  
  axis(2, at=seq(-10, 10, 0.1), labels=NA, tck=0.015)
  axis(4, las=1, labels=NA)
  axis(4, at=seq(-10, 10, 0.1), labels=NA, tck=0.015)
  
  
  
  ### add each boxplot to the current plot
  
  col_box <- c(rgb(r=1, b=0, g=0, alpha=0.2) , rgb(r=0, b=1, g=0, alpha=0.2) , rgb(r=0, b=0, g=1, alpha=0.2) )
  
  for (s in 1:n_Scenario){
    
    PACF_Scenario <- PACF123( scenario = eval(parse(text = ScenarioList[s,1]))  )
    
    ### Store the values needed for the final heatmap 
    HeatmapValues$Min[s] <- min(PACF_Scenario$lag1)
    HeatmapValues$Q10[s] <- quantile(PACF_Scenario$lag1, probs=0.10)
    HeatmapValues$Q25[s] <- quantile(PACF_Scenario$lag1, probs=0.25)
    HeatmapValues$Median[s] <- median(PACF_Scenario$lag1)
    HeatmapValues$Q75[s] <- quantile(PACF_Scenario$lag1, probs=0.75)
    HeatmapValues$Q90[s] <- quantile(PACF_Scenario$lag1, probs=0.90)
    HeatmapValues$Max[s] <- max(PACF_Scenario$lag1)
    
    i <- 4*s-3
    boxplot(PACF_Scenario, add=T, at=c(i, i+1, i+2), yaxt="n", xaxt="n", cex=0.5,
            col=col_box)
    
    points(x=c(i, i+1, i+2), y=c(PACF1_hist, PACF2_hist, PACF3_hist),
           pch=16, cex=0.8 , col=c("red","blue", "green"))
    
    # axis(1, at=(i+1), labels=NA, tck=-0.025)
    
    if(s!=n_Scenario){
      abline(v = (i+3) , col="gray70")
    }
    
    text(x=(i+1), y=-0.57, ScenarioList[s,2], srt=30, pos=2, offset=0 , cex=0.9, xpd=T)
    
  }
  
  #### add legend -----------------
  
   
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  
  
  legend(x=(xmin+xmax)/4, y=ymax*1.06, legend= c( "Lag 1", "Lag 2", "Lag 3") ,
         cex=1.1,
         pch=22, pt.bg=col_box, pt.cex=1.5, horiz = T, bty="n",
         xjust=0.5, yjust = 0.5, xpd=T)
  text("Ensemble: ", x= (xmin+xmax)/8, y=ymax*1.05, pos=2, cex=1.1, xpd=T)
  
  
  legend(x=(xmin+xmax)*5/6, y=ymax*1.06, legend= c( "Lag 1", "Lag 2", "Lag 3") , cex=1.1,
         pch=16, col=c( "red","blue", "green"), pt.cex=1, horiz = T, bty="n",
         xjust=0.5, yjust = 0.5, xpd=T)
  text("Historical: ", x= (xmin+xmax)/5*3, y=ymax*1.05, pos=4, cex=1.1, xpd=T)
  
  
  
  # ### save the current plot as pdf ========================================================================
  # 
  # pdfName <- paste0("./Results/PACF/PACF.pdf")
  # dev.copy2pdf(file=pdfName, width = 12, height = 5)
  
  ##### add the heatmap dataframe output to Global Environment ##################################################################################
  
  assign("HM_PACF", HeatmapValues, .GlobalEnv)
  
}  ## End of Function











