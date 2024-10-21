##################################################################################################################################################################
###### Authors: Homa Salehabadi (homa.salehabadi@gmail.com), David Tarboton (david.tarboton@usu.edu)
###### Institution: Utah State University and Utah Water Research Laboratory
###### Project Title: Cataloguing and Generating Hydrology Scenarios in the Colorado River Basin.
###### Funding Agency: U.S. Bureau of Reclamation
##################################################################################################################################################################


###### Function to read scenarios during the desired planning period (StartYr, EndYr) from xlsx file ------------


Func_read_required_hydrology_scenarios_from_xlsx <- function(ScenarioList=ScenarioList){
  
  nScenarios <- nrow(ScenarioList)
  
  for (s in 1:nScenarios){
    data <- read.xlsx(MainInputFile, sheet=ScenarioList[s,1])
    yr1 <- ScenarioList[s,3]
    yr2 <- ScenarioList[s,4]
    data <- subset(data, subset= data[,1]>=yr1 & data[,1]<=yr2)
    assign( paste0(ScenarioList[s,1]), data, .GlobalEnv)
  }
  

} ## End of Function








