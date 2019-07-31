# Hurricanes_and_Migration

This repository contains county-to-county migration data from the Internal Revenue Service (IRS) Statistics of Income (SOI) division. I've assembled two general outflows and inflows files containing county-to-county migration flows for the period 1998-2015, there are the ```outflow_csv``` and the ```inflow.csv``` files in the ```outflows``` and ```inflows``` folders respectively. Starting from these aggregated files, I build inflows and outflows matrices for each year in the period 1998-2015. The are organised with each row indicating the sending county for outflows and the receiving county for inflows and each row the destination for outflows and the origin for inflows. These matrices can be found in the ```outflows``` and ```inflows``` folders, respectively. The IRS releases migration files each year covering a two year period such as 2015-2016. Given the methodology adopted by the IRS, for the generic year1-year2 file, the migration flows mostly refer to movements in year1. Following this convention, I've named the matrices with a two-digits code for year1 and year2 followed by 'in' for inflows matrices and 'out' for outflows matrices. For example, the inflow matrix for the 2001-2002 period is named ```0101in.csv```.

In addition to the datafiles, I've also uploaded five Python notebooks which I have used to analyse the impact of Hurricanes Katrina and Sandy on the migration system of the affected counties. There are, for each Hurricane, an inflow analysis and an outflow analysis notebook. To have more details on the analysis, refer to the article uploaded in the ```articles``` folder. Finally, I've added the Python notebook I've used to construct the dataset I then employed in the regression analysis (refer again to the article).

I've added also the Stata do-file I've used to perform the regression analysis on Katrina and Sandy. In the ```scripts``` forlder there are now both a do-file ```regression_analysis.do``` and a notebook version of it ```regression_analysis.ipynb```.

I've added the ```hurricanes.json``` file to the ```county_groups``` folder. This file contains a list of all the hurricanes that hit the US in the 1990-2018 period such that the Federal Emergency Management Agency (FEMA) granted individual assistance to at least one county. For every such hurricane the file containes the name and fip codes of all counties which received individual assistance divided by state. For those hurricanes for which the information was available, I also reported the total cost of the hurricane, the number of deaths it caused, when it started, and when it ended (using the dataset available here ```https://www.ncdc.noaa.gov/billions/events/US/1980-2019```). Given that the FEMA website is not very user-friendly and does not provide accurate search options, I might have overlooked some event which instead should be included. Feel free to point them out.
