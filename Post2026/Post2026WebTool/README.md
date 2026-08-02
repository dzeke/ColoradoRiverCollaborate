# Reclamation Web Tool - Minimum Glen Canyon Dam Annual Release to protect Lake Powell Minimum Power Pool

This repository shares results that summarize analysis in Reclamations Post-2026 Operations Exploration Tool with the goal to identify 
minimum Glen Canyon Dam annual releases needed to protect Lake Powell from drawdown below elevation 3,490 feet (Minimum power pool). More specifically, we are looking for reservoir performance during extreme conditions of low reservoir storage and low inflows.
This analysis includes results across the 6 hydrologic ensembles available in the webtool. The web tool is also call a decision making under deep uncertainty (DMDU) tool.

Link to Reclamation's Website for the tool - https://tool.crbpost2026dmdu.org/.

The 5 storyline hydrologic ensembles are:
1. 11 maf, infrequent high flows (8 traces).
1. Decreasing Trend from 12 to 10 maf (8 traces).
1. Drought through 2028 then 11.5 maf (8 traces).
1. Early High Flows, then 12 maf (8 traces).
1. Late 20230s Drought (8 traces).

A 6th ensemble includes a grouping of other previously used ensembles (Stress test, CMIP5-LOCA kNN, Post-Pluvial NPC Temp-Adjusted, Paleo Drought Resampled, CMIP3 NPC)
1. Default optimization (33 + 64 + 100 + 50 + 153 = 290 traces).
 
## Key Findings and Insights
1. Need Glen Canyon Dam annual release from 4.0 to 6.0 million acre-feet per year to protect Lake Powell drawdown below elevation 3,490 feet across the 5 storyline ensembles.
1. Only two alternatives within the database are able to prevent drawdown below elevation 3,490 feet across **all** ensembles.
1. Both alternatives have a Lake Powell release paradigm of *Running Average Inflow*. These alternatives will have unsatisfactory operations above Lake Powell elevation 3,525 feet.

## View Results
1. Open the file **[DMDU-Performance.xlsx](DMDU-Performance.xlsx)**
1. Navigate to the worksheet **PivotTable**. This table will appear:

![PivotTable results](https://github.com/dzeke/ColoradoRiverCollaborate/blob/main/Post2026WebTool/ResultsScreenCapture.png "Key Results")

## Requirements to Run
* A web browser that can support the DMDU Tool.
* License for Excel 360.

## Directions to Reproduce Results
Note - there are a lot of "by-hand" instructions because I did not see a way to automate steps in the Web tool.

**Start-up**
1. Navigate to the website https://tool.crbpost2026dmdu.org/.
1. If needed, create a user name and password.
1. Log into the website using credentials created in Step #2.
1. Select the **Welcome** Tab from the left menu. Read the documentation plus instructions to start 
(Tool 101, Resource Center, Video, New User, Learn About Paradigms, Explore Operational Strategies).
Pay particular attention to the file on the **Welcome** => **Resource Center** (top, right) => **Technical Information** => **[What Hydrologies are included](https://tool.crbpost2026dmdu.org/www/pdf/en/powell/web_tool_hydrology.pdf)**. This document describes the hydrology ensembles used.
1. When ready, click the **Upload Session File** on the top.
1. Select **Browse** and navigate the the folder on your harddrive where you downloaded this repository.
1. Select the file **dmdu-2024-10-11.json**. This file loads the session with the most recent settings used to generate results. This file specifies filters for the database of 700+ alternatives simulated over all the hydrologies used in this analysis.
1. Click the **Operational Strategies** on the left menu. Read the documentation for the strategies (Overview, Operational Paradigms, Lake Powell Release Concepts, Lower Basin Delivery Concepts).
1. Click the **Operational Strategies Suite** tab (top). A table with 5 selected strategies (associated with the session loaded in step 7) will appear in a pop-out window. The strategies are:
1. STGY85558, 49068, 89269, 98230, 95580, 26992.
1. These strategies were choosen because they were either a (i) 'Running Average Inflow' Lake Powell release concept with a low number of years of lookback (1 or 3 years) anticipated to keep Lake Powell above minimum power pool, or (ii) had zero months of drawdown below Powell Elevation 3,490 feet in one
one of the hydrology ensembles explored in the performance tab (next step). The *Running Average Inflow* paradigm limits release to the average of inflow in the prior number of specified years. An additional 
*Run of river* operation kicks in and limits release to be equal or less than inflow.
1. The table was also downloaded and appears in **DMDU-Performance.xlsx** on the **Strategies** worksheet.
1. These strategies will appear in highlighted colors through the next steps.

**Performance**
1. Click **Performance** on the left menu.
1. On the **Choose metrics for the axes** note that **Glen Canyon Dam Water Year Release | Minimum** and **Powell Monthly Pool Elevations Less Than 3,490 feet | Percent of Months** are selected.
1. Notice also that the **Choose Hydrology Set** is set to *Default/Optimization*.
1. Click the **Update Plot** button.
1. In the parallel axis plot notice the choosen strategies are highlighted. See also the table below.
1. Copy results shown in the table -- *Strategy ID, Powell < 3,490 | Pct of Mos (%)* and *GCD WY Rel | Min (maf)* into the file **DMDU-Performance.xlsx** => **Data** worksheet.
1. Go back up to the **Choose Hydrology Set** dropdown. Select the next ensemble **Late 2030s Drought**. 
1. Repeat Performance Steps 3 to 6 for each hydrology ensemble.
1. Notice that as you cycle through the ensembles, that one or more of the highlighted strategies (dark colors) cross the Powell < 3,490 axis at 0% and appear as flat (or near flat) lines across the top of the parallel axis plot to the second GLD WY Rel | Min axes.
These are the scenarios of most interest because they keep Lake Powell above 3,490 feet in **all** traces *and* they have a relatively high minimum Glen Canyon Dam water year release.
1. Last, in the **DMDU-Performance.xlsx** file, select the worksheet **PivotTable**. In the **PivotTable Analyze** Tab, select the **Data** => **Refresh** button to load the newly copied data in the table.
1. Screen captures of the parallel axis plots and tables for each hydrology ensemblem also are saved in the file **DMDUResultsFigures.docx**.

**Robustness**
1. Click **Robustness** on the left menu.
1. Notice there are two conditions of acceptability selected: 1)  Powell Monthly Pool Elevation stays above 3,490 Feet in 100% (360 months) or more of the next 30 years AND 2) the Glen Canyon Dam Water Year Release stays above 6 Million Acre-Feet in 100% (30 years) or more of the next 30 years.
1. Click the button **Calculate Robustness**.
1. Notice on the ranked ordered bar plot that the largest percent of acceptable futures is ~ 80%. This result means there ar **NO** scenarios where a Glen Canyon Dam annual release above 6 million acre-feet per year will keep the Lake Powell elevation above 3,490 feet in all years.

**Vulnerability**
1. Click the **Vulnerability** Tab on the left menu.
1. Set unacceptability Condition 1 to "Powell Monthly Pool Elevation" *exceeds* 3,490 feet Threshold value 100% of the time over a 30-year time horizon. Note, here we are switching
the direction of "acceptability" because it is not possible in the web tool to specify a Powell Monthly Pool Elevation below 3,490 feet in 0% months over 30-year time horizon (lowest allowable selection is ~1% of months).
1. Set unacceptability Condition 2 to "Glen Canyon Dam Water Year Release exceeds Threshold of 6 million acre-feet in 100%." Again, we use the converse because it is not possible to specify a Glen Canyon Dam water year release
falls below 6 maf in 0% of months over the 30-year time horizon (can only specify in ~ 1% of months.
1. Click **Calculate Vulnerability**.
1. In the ranked order bar plot note that none of alternatives have an acceptability of 100% (i.e., unacceptable in some futures).

## Explanation of Contents
1. **DMDU-Performance.xlsx** - Excel file with values of 'Lake Powell elevation below 3,490 feet (percent of months)' and 'Glen Canyon Dam Minimum Water Year Release' for each alternative in each hydrology ensemble. These values were copied
from the **Performance** Tab within the webtool.
1. **DMDU-ResultsFigures.docx** - Word file with screen captures of the parallel axis plots and tables for each hydrology ensemble.
1. **ResultsScreenCapture.png** - Screen capture of the worksheet **PivotTable** in the Excel File **DMDU-Performance.xlsx**.
1. **dmdu-2024-10-11.json** - File with webtool session saved data on October 11, 2024. This file stores data such as selected alternatives and selected performance metrics. This file is selected in Step 7 above.
1. **HydrologyTraces** - Folder with meta set of hydrology ensembles and traces reported by Salehabadi et al. (2024).
1. **OldSessionFiles** - Folder with session files of prior earlier model sessions from September and October 2024.

## Requested Citation
David E. Rosenberg (2024), “Reclamation Web Tool - Minimum Glen Canyon Dam Annual Release to protect Lake Powell minimum power pool.” Utah State University. Logan, Utah.
https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/Post2026WebTool.

## References

1. Salehabadi, H., Tarboton, D. G., Wheeler, K. G., Smith, R., and Baker, S. (2024). "Quantifying and Classifying Streamflow Ensembles Using a Broad Range of Metrics for an Evidence-Based Analysis: Colorado River Case Study." Water Resources Research, 60(7), e2024WR037225. https://doi.org/10.1029/2024WR037225. 
