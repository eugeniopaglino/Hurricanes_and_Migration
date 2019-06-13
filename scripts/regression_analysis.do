
*** REGRESSION ANALYSIS ***

* This do-file analyses the impact of Hurricanes Katrina and Sandy
* on the migration systems of the affected area by using first a
* modified gravity model, that compares variation in inflows to 
* disaster affected and nearby counties after the Hurricanes, and then
* a difference-in-differences model with fixed-effects for sending-receiving
* county pairs, to understand whether there is evidence of recovery
* migration when comparing the nearby and the disaster-affected
* counties.

set more off, permanently
set matsize 11000

* Here you should add the directory where the gravity_data.csv file
* produced by the regression_analysis notebook is located.

cd "your/directory"

clear all

import delimited "gravity_data.csv"

* We generate a numeric version of the year variable. The year string variable
* containes pairs of years like year1-year2, the numeric variable extracts only
* year1, which is the year to which most of the migration flows refer to.

generate year_num = substr(year,1,4)
destring year_num, replace

* We generate dummy variables for time (before and after the Hurricane) and
* treatment (being in disaster affected counties for the Hurricane) for each
* Hurricane.

generate time_sandy = 0
replace time_sandy = 1 if inlist(year, "2012-2013", "2013-2014")
								 
generate time_kat = 0
replace time_kat = 1 if inlist(year, "2005-2006", "2006-2007", "2007-2008")									 
				
generate treatment_sandy = 0
replace treatment_sandy = 1 if sandy_group_dest == "disaster"

generate treatment_kat = 0
replace treatment_kat = 1 if kat_group_dest == "disaster"

* We also generate the time*treatment interaction to denote the flows which
* refer to disaster-affected counties after the Hurricane. This is our treatment
* group.

generate treated_kat = 0
replace treated_kat = 1 if (treatment_kat == 1 & time_kat == 1)

generate treated_sandy = 0
replace treated_sandy = 1 if (treatment_sandy == 1 & time_sandy == 1)

* We generate a dummy for each sending-receiving county pair
				
egen sending_receiving = group(destination origin)

* We apply a log transformation to the following variables:

*	- return_num: which is an estimate of the number of households
*				  that moved from one county to the other;
*	- exmpt_num: which is an estimate of the number of individuals
*				  that moved from one county to the other;
*	- pop_destination_hh: which is an estimate of the number of households
*						  at the destination;
*	- pop_origin_hh: which is an estimate of the number of households
*						  at the origin;
*	- pop_destination_in: which is an estimate of the number of individuals
*						  at the destination;
*	- pop_origin_in: which is an estimate of the number of individuals
*						  at the origin;
				
replace return_num = log(return_num)
replace exmpt_num = log(exmpt_num)
replace pop_destination_hh = log(pop_destination_hh)
replace pop_origin_hh = log(pop_origin_hh)
replace pop_destination_in = log(pop_destination_in)
replace pop_origin_in = log(pop_origin_in)

* We sort the dataset and remove duplicates

sort sending_receiving year_num
quietly by sending_receiving year_num:  gen dup = cond(_N==1,0,_n)
drop if dup>1

* We set sending_receiving as the panel variable and year_num as the 
* time variable

xtset sending_receiving year_num

* We can now proceed with the analysis. First I will perform the analsysis
* for Katrina, then the one for Sandy. The regression results are written to
* .csv file.
		   
*** KATRINA REPLICATION ANALYSIS ***

*** GRAVITY MODEL ***

*** DISASTER-ALL ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(kat_group_dest == "disaster")), ///
			vce(robust) fe

*** DISASTER-DISASTER ***		
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "disaster")) & ///
			(kat_group_origin == "disaster")), ///
			vce(robust) fe
			
*** DISASTER-NEARBY ***				
			
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "disaster")) & ///
			(kat_group_origin == "nearby")), ///
			vce(robust) fe
			
*** DISASTER-DISTANT ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "disaster")) & ///
			(kat_group_origin == "distant")), ///
			vce(robust) fe
			
*** DISASTER-ALL URBAN ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "disaster")) & ///
			(urban_origin == "urban")), ///
			vce(robust) fe			
			
*** DISASTER-DISASTER URBAN ***		   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "disaster")) & ///
			(urban_origin == "urban") & ///
			(kat_group_origin == "disaster")), ///
			vce(robust) fe
			
*** DISASTER-NEARBY URBAN ***				
			
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "disaster")) & ///
			(urban_origin == "urban") & ///
			(kat_group_origin == "nearby")), ///
			vce(robust) fe
			
*** DISASTER-DISTANT URBAN ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "disaster")) & ///
			(urban_origin == "urban") & ///
			(kat_group_origin == "distant")), ///
			vce(robust) fe

*** STORING THE ESTIMATES ***			
			
esttab using "gravity_disaster_kat.csv", ///
		replace ar2 csv noomitted nolines label se
	
eststo clear

*** NEARBY-ALL ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(kat_group_dest == "nearby")), ///
			vce(robust) fe

*** NEARBY-DISASTER ***		
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "nearby")) & ///
			(kat_group_origin == "disaster")), ///
			vce(robust) fe
			
*** NEARBY-NEARBY ***				
			
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "nearby")) & ///
			(kat_group_origin == "nearby")), ///
			vce(robust) fe
			
*** NEARBY-DISTANT ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "nearby")) & ///
			(kat_group_origin == "distant")), ///
			vce(robust) fe
			
*** NEARBY-ALL URBAN ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "nearby")) & ///
			(urban_origin == "urban")), ///
			vce(robust) fe			
			
*** NEARBY-DISASTER URBAN ***		   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "nearby")) & ///
			(urban_origin == "urban") & ///
			(kat_group_origin == "disaster")), ///
			vce(robust) fe
			
*** NEARBY-NEARBY URBAN ***				
			
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "nearby")) & ///
			(urban_origin == "urban") & ///
			(kat_group_origin == "nearby")), ///
			vce(robust) fe
			
*** NEARBY-DISTANT URBAN ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat ///
			if ((inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							  "2002-2003", "2003-2004", "2004-2005", ///
							  "2007-2008", "2008-2009", "2009-2010")) & ///
			(inlist(kat_group_dest, "nearby")) & ///
			(urban_origin == "urban") & ///
			(kat_group_origin == "distant")), ///
			vce(robust) fe

*** STORING THE ESTIMATES ***			
			
esttab using "gravity_nearby_kat.csv", ///
		replace ar2 csv noomitted nolines label se
	
eststo clear
			
*** DIFF-IN-DIFF MODEL ***

*** ALL ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat i.treated_kat ///
		   if (inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							"2002-2003", "2003-2004", "2004-2005", ///
							"2005-2006", "2006-2007", "2007-2008") & ///
		   inlist(kat_group_dest, "disaster", "nearby")), ///
		   vce(robust) fe

*** DISASTER ***				
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat i.treated_kat ///
		   if (inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							"2002-2003", "2003-2004", "2004-2005", ///
							"2005-2006", "2006-2007", "2007-2008") & ///
		   inlist(kat_group_dest, "disaster", "nearby") & ///
		   (kat_group_origin == "disaster")), ///
		   vce(robust) fe
		   
*** NEARBY ***			   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat i.treated_kat ///
		   if (inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							"2002-2003", "2003-2004", "2004-2005", ///
							"2005-2006", "2006-2007", "2007-2008") & ///
		   inlist(kat_group_dest, "disaster", "nearby") & ///
		   (urban_origin == "urban") & ///
		   (kat_group_origin == "nearby")), ///
		   vce(robust) fe
		   
*** DISTANT ***			   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat i.treated_kat ///
		   if (inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							"2002-2003", "2003-2004", "2004-2005", ///
							"2005-2006", "2006-2007", "2007-2008") & ///
		   inlist(kat_group_dest, "disaster", "nearby") & ///
		   (urban_origin == "urban") & ///
		   (kat_group_origin == "distant")), ///
		   vce(robust) fe
		     
*** ALL URBAN ***				
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat i.treated_kat ///
		   if (inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							"2002-2003", "2003-2004", "2004-2005", ///
							"2005-2006", "2006-2007", "2007-2008") & ///
		   inlist(kat_group_dest, "disaster", "nearby") & ///
		   (urban_origin == "urban")), ///
		   vce(robust) fe		   
		   
*** DISASTER URBAN ***				
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat i.treated_kat ///
		   if (inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							"2002-2003", "2003-2004", "2004-2005", ///
							"2005-2006", "2006-2007", "2007-2008") & ///
		   inlist(kat_group_dest, "disaster", "nearby") & ///
		   urban_origin == "urban" & ///
		   (kat_group_origin == "disaster")), ///
		   vce(robust) fe
		   
*** NEARBY URBAN ***			   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat i.treated_kat ///
			if (inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							 "2002-2003", "2003-2004", "2004-2005", ///
							 "2005-2006", "2006-2007", "2007-2008") & ///
		   inlist(kat_group_dest, "disaster", "nearby") & ///
		   urban_origin == "urban" & ///
		   (kat_group_origin == "nearby")), ///
		   vce(robust) fe
		   
*** DISTANT URBAN ***			   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_kat i.treated_kat ///
			if (inlist(year, "1999-2000", "2000-2001", "2001-2002", ///
							 "2002-2003", "2003-2004", "2004-2005", ///
							 "2005-2006", "2006-2007", "2007-2008") & ///
		   inlist(kat_group_dest, "disaster", "nearby") & ///
		   urban_origin == "urban" & ///
		   (kat_group_origin == "distant")), ///
		   vce(robust) fe

*** STORING THE ESTIMATES ***		  	   

esttab using "diffindiff_results_kat.csv", ///
	replace ar2 csv noomitted nolines label se

eststo clear	

*** SANDY ANALYSIS ***

*** GRAVITY MODEL ***

*** DISASTER-ALL ***		   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if (inlist(year, "2010-2011", "2011-2012", ///
							 "2012-2013", "2013-2014") & ///
			inlist(sandy_group_dest, "disaster")), ///
			vce(robust) fe

*** DISASTER-DISASTER ***		   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if (inlist(year, "2010-2011", "2011-2012", ///
							 "2012-2013", "2013-2014") & ///
			inlist(sandy_group_dest, "disaster") & ///
			(sandy_group_origin == "disaster")), ///
			vce(robust) fe
								
*** DISASTER-NEARBY ***				
			
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "disaster")) & ///
			(sandy_group_origin == "nearby")), ///
			vce(robust) fe
			
*** DISASTER-DISTANT ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "disaster")) & ///
			(sandy_group_origin == "distant")), ///
			vce(robust) fe
			
*** DISASTER-ALL URBAN ***		   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "disaster")) & ///
			(urban_origin == "urban")), ///
			vce(robust) fe			
			
*** DISASTER-DISASTER URBAN ***		   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "disaster")) & ///
			(urban_origin == "urban") & ///
			(sandy_group_origin == "disaster")), ///
			vce(robust) fe
			
*** DISASTER-NEARBY URBAN ***				
			
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "disaster")) & ///
			(urban_origin == "urban") & ///
			(sandy_group_origin == "nearby")), ///
			vce(robust) fe
			
*** DISASTER-DISTANT URBAN ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "disaster")) & ///
			(urban_origin == "urban") & ///
			(sandy_group_origin == "distant")), ///
			vce(robust) fe
			
*** STORING THE ESTIMATES ***			
			
esttab using "gravity_disaster_sandy.csv", ///
	replace ar2 csv noomitted nolines label se
	
eststo clear	

*** NEARBY-ALL ***		   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if (inlist(year, "2010-2011", "2011-2012", ///
							 "2012-2013", "2013-2014") & ///
			inlist(sandy_group_dest, "nearby")), ///
			vce(robust) fe

*** NEARBY-DISASTER ***		   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if (inlist(year, "2010-2011", "2011-2012", ///
							 "2012-2013", "2013-2014") & ///
			inlist(sandy_group_dest, "nearby") & ///
			(sandy_group_origin == "disaster")), ///
			vce(robust) fe
								
*** NEARBY-NEARBY ***				
			
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "nearby")) & ///
			(sandy_group_origin == "nearby")), ///
			vce(robust) fe
			
*** NEARBY-DISTANT ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "nearby")) & ///
			(sandy_group_origin == "distant")), ///
			vce(robust) fe
			
*** NEARBY-ALL URBAN ***		   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "nearby")) & ///
			(urban_origin == "urban")), ///
			vce(robust) fe			
			
*** NEARBY-DISASTER URBAN ***		   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "nearby")) & ///
			(urban_origin == "urban") & ///
			(sandy_group_origin == "disaster")), ///
			vce(robust) fe
			
*** NEARBY-NEARBY URBAN ***				
			
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "nearby")) & ///
			(urban_origin == "urban") & ///
			(sandy_group_origin == "nearby")), ///
			vce(robust) fe
			
*** NEARBY-DISTANT URBAN ***

eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy ///
			if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
			(inlist(sandy_group_dest, "nearby")) & ///
			(urban_origin == "urban") & ///
			(sandy_group_origin == "distant")), ///
			vce(robust) fe
			
*** STORING THE ESTIMATES ***			
			
esttab using "gravity_nearby_sandy.csv", ///
	replace ar2 csv noomitted nolines label se
	
eststo clear	
			
*** DIFF-IN-DIFF MODEL ***

*** ALL ***				
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy i.treated_sandy ///
		   if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
		   inlist(sandy_group_dest, "disaster", "nearby")), ///
		   vce(robust) fe

*** DISASTER ***				
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy i.treated_sandy ///
		   if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
		   inlist(sandy_group_dest, "disaster", "nearby") & ///
		   (sandy_group_origin == "disaster")), ///
		   vce(robust) fe
		   
*** NEARBY ***			   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy i.treated_sandy ///
		   if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
		   inlist(sandy_group_dest, "disaster", "nearby") & ///
		   (sandy_group_origin == "nearby")), ///
		   vce(robust) fe
						      
		   
*** DISTANT ***			   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy i.treated_sandy ///
		   if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
		   inlist(sandy_group_dest, "disaster", "nearby") & ///
		   (sandy_group_origin == "distant")), ///
		   vce(robust) fe
		   
*** ALL URBAN ***				
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy i.treated_sandy ///
		   if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
		   inlist(sandy_group_dest, "disaster", "nearby") & ///
		   (urban_origin == "urban")), ///
		   vce(robust) fe		   
		   
*** DISASTER URBAN ***				
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy i.treated_sandy ///
		   if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
		   inlist(sandy_group_dest, "disaster", "nearby") & ///
		   urban_origin == "urban" & ///
		   sandy_group_origin == "disaster"), ///
		   vce(robust) fe
		   
*** NEARBY URBAN ***			   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy i.treated_sandy ///
		   if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
		   inlist(sandy_group_dest, "disaster", "nearby") & ///
		   urban_origin == "urban" & ///
		   sandy_group_origin == "nearby"), ///
		   vce(robust) fe
		   
*** DISTANT URBAN ***			   
		   
eststo: xtreg exmpt_num pop_origin_in pop_destination_in i.time_sandy i.treated_sandy ///
		   if ((inlist(year, "2010-2011", "2011-2012", ///
							  "2012-2013", "2013-2014")) & ///
		   inlist(sandy_group_dest, "disaster", "nearby") & ///
		   urban_origin == "urban" & ///
		   sandy_group_origin == "distant"), ///
		   vce(robust) fe
		  
*** STORING THE ESTIMATES ***		  
		      
esttab using "diffindiff_results_sandy.csv", ///
	replace ar2 csv noomitted nolines label se

eststo clear
