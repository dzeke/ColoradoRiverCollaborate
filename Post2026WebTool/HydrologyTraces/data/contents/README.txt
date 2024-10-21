R Scripts for Evaluating Annual Streamflow Ensemble Metrics and data and results from their application in the Colorado River Basin

Organization of this Resource
Ensemble Comparison folder (EnsembleComparison).
- MasterScript_EnsembleComparison.R. R script that computes cross ensemble comparison metrics. 
- HydrologyScenarios.xlsx. Input file for the R script. Tabs in this file hold the data for each ensemble. There is a Readme tab describing the content and a tab where users can control the ensembles and metrics that are to be computed by the R script.
- R_Files. Folder with R functions for evaluating the metrics. These are called by the master script.
- Results. Folder where a PDF file with results is output.

Ensemble Specific Metrics folder (EnsembleSpecificMetrics).
- MasterScript_EnsembleSpecificMetrics.R. R script that computes ensemble specific metrics. 
- HydrologyScenarios.xlsx. Input file for the R script. Tabs in this file hold the data for each ensemble. There is a Readme tab describing the content and a tab where users can control the ensembles and metrics that are to be computed by the R script. 
- R_Files. Folder with R functions for evaluating the metrics. These are called by the master script.
- Results. Folder were multiple PDF files, one file for each ensemble, are output.

To reproduce these results you need R software available from https://www.R-project.org/.
R Core Team (2023). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/

The master scripts above are programmed to download and install the needed R packages and should be run using R. 

R Studio https://posit.co/download/rstudio-desktop/ provides a convenient environment for running R.

Results here were obtained using R version 4.3.1 (2023-06-16 ucrt) -- "Beagle Scouts" running in R Studio version 2023.06.0+421 "Mountain Hydrangea" Release (583b465ecc45e60ee9de085148cd2f9741cc5214, 2023-06-05) for windows.

R is available as Free Software under the terms of the Free Software Foundation’s GNU General Public License.

RStudio is available from Posit Software under the terms of version 3 of the GNU Affero General Public License. 

