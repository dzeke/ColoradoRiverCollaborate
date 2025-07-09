# Import Colorado River reservoir data from Reclamation
David E. Rosenberg
February 27, 2024
Updated: July 9, 2025

## Overview

These codes read daily Reclamation data for Colorado River reservoirs from U.S. Bureau of Reclamation's (Reclamation) web portal https://www.usbr.gov/uc/water/hydrodata/reservoir_data/919/csv/25.csv). Here, as an example, 919 is the code for Lake Powell. And 25 is the code for reservoir evaporation.

## Files

1. **USBRDataImport.ipynb** - Jupyter notebook file. Loads in selected reservoirs and fields.
1. **PowellDataImport.ipynb** - Jupyter notebook file. Loads in fields for Lake Powell.
1. **PowellEvap.ipynb** - Juputer notebook file. Loads in Evaporation Data for Lake Powell.
1. **ReclamationMetaData.csv** - All reservoirs, all reservoir fields, units of measurement, etc.
1. **ReservoirsAndFields.csv** - Cross-tabulation of the *ReclamationMetaData.csv* file showing selected reservoirs (rows) and fields of measurement for the reservoir (columns). This file is output by the notebook. 

## Data wrangling strategy
First the code reads in the Metadata for all available reservoirs and data fields from the file 'https://raw.githubusercontent.com/dzeke/ColoradoRiverCollaborate/main/Powell10year/AsPython/ReclamationMetaData.csv.' This metadata includes reservoir name, site id, field name, field ids, units of measurement, and so forth.

Second, the code then iterates over all specified reservoirs and reads in the associated data fields.

Third, the code combines data for all the fields into a database (Pandas dataframe). The database has the columns: Date_Raw, Value, Reservoir_ID, Reservoir_Name, Field_ID, Field_Name, Units.

Fourth, the code then aggregates data to yearly (water year starting Oct 1).

Fifth, the code plots data for different reservoirs and fields. For example:

    Evaporation
    Total evaporation for each period of consecutative 10 years

## Directions to Open, Edit, and Run (Windows)

    Download all the files into a new local folder on your machine or storage device (e.g., c:\users\myfolder).
    Open a command prompt (c:).
    Install the Juptyer server. At the command prompt type:

            pip install jupyter

    Navigate to the folder where this file is stored. For example:

            cd C:\users\myfolder

    Type the command:

            jupyter notebook

    A new webpage will open in your browser with the url http://localhost:8888/tree.
    Select this file from the list -- PowellDataImport.ipynb.
    This page will display.
    Select the first cell -- e.g., block [19] to run or edit. You should see the follow output:

            A list of numbers from 912 to 923. The reservoir codes. A list of reservoir names and numbers to the right. e.g. Blue Mesa Reservoir 16. Here, 16 indicates the number of data fields for the reservoir. e.g, Inflow, Storage, Evaporation, etc.

    Run each successive cell.

## Requested Citation
David E. Rosenberg (2024), “Import Lake Powell Data from Reclamation.” Utah State University. Logan, Utah. https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/Powell10year/AsPython.

Pandas cheat sheet at https://pandas.pydata.org/docs/user_guide/10min.html.