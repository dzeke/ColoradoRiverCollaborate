##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################

##### This function takes the hydrology scenario dataframe in which the first column is year and the others are flow traces.
##### Then calculate ACF lag 1, 2, and 3 for all the traces in a hydrology scenario (a dataframe with 3 columns: lag1, lag2, and lag3, and each row corresponds to one trace). 
##### The output is a boxplot showing the distribution of ACF for the required hydrology scenarios


Func_ACF_Lag123 <- function (ScenarioList){
  
  
  ####  Create an empty dataframe to get the final values that we need for heatmap ======================
  
  HeatmapValues <- data.frame(matrix(nrow=nScenarios, ncol=7))
  colnames(HeatmapValues) <- c("Scenario", "Q10", "Q25", "Median", "Q75", "Q90", "Hist")
  HeatmapValues$Scenario <- ScenarioList[,2]
  
  
  #### Historical ACF lag 1,2,3 (will be added as points in boxplot) =================================================
  
  ## read Hist data
  
  Hist <- read.xlsx(MainInputFile, sheet="Hist")
  yr1 <- min(Hist[,1])
  yr2 <- max(Hist[,1])
  

  ACF_hist <- acf(Hist[,2], plot=F)
  ACF1_hist <- ACF_hist$acf[which(ACF_hist$lag==1)]
  ACF2_hist <- ACF_hist$acf[which(ACF_hist$lag==2)]
  ACF3_hist <- ACF_hist$acf[which(ACF_hist$lag==3)]
  
  HeatmapValues$Hist <- ACF1_hist
  
  #### Compare scenarios, boxplots of ACF lag 1,2,3 ===================================================================
  
  ### Function to calculate ACF lag 1,2,3 for all the traces of a hydrology scenario ---------------
  
  ACF123 <- function (scenario){
    
    ntraces <- ncol(scenario)-1      
    
    ACF <- data.frame(matrix(nrow=ntraces, ncol=3))
    colnames(ACF) <- c("lag1", "lag2", "lag3")
    rownames(ACF) <- colnames(scenario[-1])
    
    ## Lag 1,2,3
    for(t in 1:ntraces){
      acf_list <- acf(scenario[,(t+1)], plot=FALSE)
      ACF$lag1[t] <- acf_list$acf[which(acf_list$lag==1)]   ## or: acf_list$acf[2] 
      ACF$lag2[t] <- acf_list$acf[which(acf_list$lag==2)] 
      ACF$lag3[t] <- acf_list$acf[which(acf_list$lag==3)] 
    }
    
    return(ACF)
  }
  

  ### boxplot --------------------------------------------------------------------------------------
  
  ACF_Scenario1 <- ACF123( scenario = eval(parse(text = ScenarioList[1,1])) )
  
  n_Scenario <- nrow(ScenarioList)
  
  nx <- 3*n_Scenario + (n_Scenario -1)
  boxplot(ACF_Scenario1, xlim=c(0.5, nx+0.5),
          # main="Autocorrelation Function (ACF) at lags 1 to 3",
          ylab="ACF",
          cex.lab=1.3,
          cex.axis=1.2,
          # ylim=c(-0.5,0.7),
          xaxt="n",
          las=1,
          pch=1,
          cex=0.5,
          col="white",
          border="white",
          boxwex=0.5)
  
  title(main="ACF at lags 1 to 3", adj=0.5, line=2)
  text(paste0(ScenarioList[2]),
       x=(par("usr")[1] + par("usr")[2])/2,
       y=(par("usr")[4]),
       xpd=T,
       adj=c(0.5,-1.2),
       cex=1)
  
  axis(2, at=seq(-10, 10, 0.1), labels=NA, tck=0.015)
  axis(4, las=1, labels=NA)
  axis(4, at=seq(-10, 10, 0.1), labels=NA, tck=0.015)
  
  
  ### add each boxplot to the current plot
  
  col_box <- c(rgb(r=1, b=0, g=0, alpha=0.2) , rgb(r=0, b=1, g=0, alpha=0.2) , rgb(r=0, b=0, g=1, alpha=0.2) )
  
  for (s in 1:n_Scenario){
    
    ACF_Scenario <- ACF123( scenario = eval(parse(text = ScenarioList[s,1]))  )
    
    ### Store the values needed for the final heatmap 
    HeatmapValues$Q10[s] <- quantile(ACF_Scenario$lag1, probs=0.10)
    HeatmapValues$Q25[s] <- quantile(ACF_Scenario$lag1, probs=0.25)
    HeatmapValues$Median[s] <- median(ACF_Scenario$lag1)
    HeatmapValues$Q75[s] <- quantile(ACF_Scenario$lag1, probs=0.75)
    HeatmapValues$Q90[s] <- quantile(ACF_Scenario$lag1, probs=0.90)
    
    i <- 4*s-3
    boxplot(ACF_Scenario, add=T, at=c(i, i+1, i+2), yaxt="n", xaxt="n", cex=0.5,
            col=col_box, boxwex=0.5)
    
    points(x=c(i, i+1, i+2), y=c(ACF1_hist, ACF2_hist, ACF3_hist),
           pch=16, cex=0.8 , col=c("red","blue", "green"))
    
    # axis(1, at=(i+1), labels=NA, tck=-0.025)
    
    if(s!=n_Scenario){
      abline(v = (i+3) , col="gray70")
    }
    
    # text(x=(i+1), y=-0.57, ScenarioList[s,2], srt=30, pos=2, offset=0 , cex=0.9, xpd=T)
  }
  
  #### add legend -----------------
  
  
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]


  legend(x=xmax, y=ymax, legend= c( "Lag 1", "Lag 2", "Lag 3") , title="Ensemble ACF",
         cex=0.8,
         pch=22, pt.bg=col_box, pt.cex=1.5, horiz = F, bty="n",
         xjust= -0.2, yjust = 1, xpd=T)
  
  
  legend(x=xmax, y=(ymax/2), 
         legend= c( paste0("Lag 1 = ", round(ACF1_hist, digits=2)), paste0("Lag 2 = ", round(ACF2_hist, digits=2)), paste0("Lag 3 = ", round(ACF3_hist, digits=2))) , 
         title="Historical ACF", title.adj = 0,
         cex=0.8,
         pch=16, col=c( "red","blue", "green"), pt.cex=1, horiz = F, bty="n",
         xjust=-0.2, yjust = 1, xpd=T)
  

  # ### save the current plot as pdf ========================================================================
  # 
  # pdfName <- paste0("./Results/ACF/ACF.pdf")
  # dev.copy2pdf(file=pdfName, width = 12, height = 5)
  
  
  ##### add the heatmap dataframe output to Global Environment ##################################################################################
  
  assign("HM_ACF", HeatmapValues, .GlobalEnv)
  
  
}  ## End of Function











