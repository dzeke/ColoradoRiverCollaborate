
##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


##### Heatmap function

Func_Heatmap <- function(ScenarioList, Quartile="Median", SYRReliability="Q90", PlotTitle="..."){  
  ## Quartile: specify which value should be used (Q10, Q25, Median, Q75, Q90, IQR1090, IQR2575)

  ### read  metric names for heatmap from Excel file -----------
  library("openxlsx")
  HMInputs <- read.xlsx(MainInputFile, sheet="Heatmap")
  nHMMetrics <- nrow(HMInputs)
  
  
  #### calculate metric uncertainty and add columns of IQR2575 and IQR1090 totje HM_... dadaframes
  
  for (m in 1:nHMMetrics){

      eval(str2expression( paste0("HM_", HMInputs[m ,1], "$IQR1090 <- HM_", HMInputs[m ,1], "$Q90 - HM_", HMInputs[m ,1], "$Q10" )    ))
      eval(str2expression( paste0("HM_", HMInputs[m ,1], "$IQR2575 <- HM_", HMInputs[m ,1], "$Q75 - HM_", HMInputs[m ,1], "$Q25" )    ))
  }
  
  
  
  #### retrieve the specified "Quartile" heatmap values for all metrics =============================
  
  ### create an empty dataframe to store heatmap values for all scenarios and all metrics ---------
  HMdf <- matrix(nrow=(nScenarios+1), ncol=nHMMetrics)
  rownames(HMdf) <- c(ScenarioList[,2] , "Historical")
  colnames(HMdf) <- HMInputs[,1]  ## These will be used as metric names on heatmap
  
  ### read and store heatmap values for all metrics and scenarios from the HM_... dataframes already exist in the Global Environment ---------

  for (m in colnames(HMdf)){
    for (Ens in ScenarioList[,2] ){
      
      HMdf[(which(rownames(HMdf)==Ens)) , (which(colnames(HMdf)==m))] <- eval(str2expression( paste0("HM_", m, "$", Quartile, "[which(HM_", m, "[,1]=='",Ens,"')]") ))
      HMdf[(nScenarios+1),(which(colnames(HMdf)==m))] <- eval(str2expression( paste0("HM_", m, "$Hist[1]") ))
        
    }
  }
  
  
  ### to read the 90% of SYR (instead of median)
  colInd_SYR <- which(grepl("SYR", colnames(HMdf)) == TRUE)
  colName_SYR <- colnames(HMdf)[colInd_SYR]
  
  for (ii in colName_SYR){
    for (Ens in ScenarioList[,2] ){
      
      HMdf[(which(rownames(HMdf)==Ens)) , (which(colnames(HMdf)==ii)) ] <- eval(str2expression( paste0("HM_", ii, "$", SYRReliability, "[which(HM_", ii, "[,1]=='",Ens,"')]") ))
      
    }
  }
  
    
  #### Hist value ranges = 0 -------
  
  if (Quartile == "IQR2575" ){
    HMdf[(nScenarios+1), ] <- 0
  }
  
  if (Quartile == "IQR1090" ){
    HMdf[(nScenarios+1), ] <- 0
  }
  
  
  ### Plot heatmap =============================================


  library("RColorBrewer")
  
  pheatmap::pheatmap(mat=HMdf, 
                     color = colorRampPalette(brewer.pal(n = 7, name = "RdYlBu"))(16),
                     # border_color = NA,
                     display_numbers=round(HMdf, digits=2), ## display original values (from the main df, not normalized values)
                     # display_numbers=T,   ## display actual values used to create the heatmat (e.g. normalized)
                     # fontsize_number=9,
                     number_color="gray20",
                     cellwidth=30,
                     scale="column",  ## this will normalize the values. (it works similar to these functions: 1- scale (HMdf) or 2- HMdf[,j]-(mean(HMdf[,j])))/sd(HMdf[,j])
                     legend_breaks=seq(-4,4, by=1),  ##c(10:17)
                     main=PlotTitle,
                     labels_col=HMInputs[,2],
                     angle_col=45,
                     
                     # na_col="white",
                     
                     # cluster_rows=F, cluster_cols=F,  ## to not cluster (keep the orders)
                     cluster_rows=T,
                     # cluster_cols=T,
                     clustering_method = "ward.D2",  ## centroid  ward.D2  complete
                     
                     cutree_rows = 4,
                     # cutree_cols = 15,
                     
                     treeheight_row = 150,  ## 0 to turn off tree lines
                     treeheight_col = 80
                     )  
  
  
  ##### add the master heatmap dataframe to Global Environment ======================
  
  assign(paste0("HM_MasterDF_", Quartile), HMdf, .GlobalEnv)
  
}
  
  



  
  