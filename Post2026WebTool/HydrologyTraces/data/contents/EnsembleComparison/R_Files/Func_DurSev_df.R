##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


##### Function to calculate duration-severity of a time series and 
##### return the DurSev and DurSev_yr dataframes into the Global Environment


Func_DurSev_df <- function (data, MaxDuration=25, yr1=1906, yr2=2019, unit_factor=1 ) {
  
  ##### DurSev calculations (dataframe) ====================================================
  
  ## nlowest function: find the nth lowest value (or index) in x --------------
  
  nlowest <- function(x,n,value=TRUE){
    s <- sort(x,index.return=TRUE)
    if(value==TRUE)  {s$x[n]}  else  {s$ix[n]}    ## TRUE: nlowest=value   FALSE: nlowest=index
  } 
  
  ## ---------------------
  
  years <- yr1:yr2
  n <- length(years)
  
  s=1
  ## take the flow data related to the s series ------------
  flow <- data[ which(data[,1]==yr1):which(data[,1]==yr2) ,(1+s)]
  
  ## empty matrixes -------------
  Mean <- matrix(rep(NA), nrow=n , ncol=MaxDuration)
  DurSev <- matrix(rep(NA), nrow=n , ncol=MaxDuration)
  DurSev_index <- matrix(rep(NA), nrow=n , ncol=MaxDuration)
  DurSev_yr <- matrix(rep(NA), nrow=n , ncol=MaxDuration)
  
  ## Loop: over different durations ----------
  for (duration in 1:MaxDuration){  
    
    mean_duration <- rep(NA)
    # sort <- rep(NA)
    
    for (m in 1:(n-(duration-1))){
      mean_duration[m] <- mean( flow[ m : (m+(duration-1)) ] )
      Mean[m ,duration] <- mean_duration[m]
    }
    
    for (m in 1:(n-(duration-1))){
      DurSev[m ,duration] <- nlowest( mean_duration,m,value=TRUE)
      DurSev_index[m ,duration] <- nlowest(mean_duration,m,value=FALSE)   
      DurSev_yr[m ,duration] <- years[DurSev_index[m ,duration]]
    }
    
  }
  
  ## change unit ----------------------
  DurSev_unit <- DurSev*unit_factor  
  
  
  ## the final data frames that you can use for dotty plots
  DurSev <- DurSev_unit
  DurSev_yr <- DurSev_yr
  
  
  ## return the SeqAve dataframe in hte Global Environment
  assign("DurSev", DurSev, .GlobalEnv)
  assign("DurSev_yr", DurSev_yr, .GlobalEnv)
  
}