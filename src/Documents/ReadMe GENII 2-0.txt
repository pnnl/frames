GENII Version 2.10.2
20 February 2017

Version 2.10.2 is a minor update to Version 2.10.1 to correct some minor difficulties found in the updates of operating systems from Windows XP and Windows 7 to Windows 8 and Windows 10.
1) Logic was repaired for connection of the User Defined to Chronic Exposure modules
2) The default parameter for tritium gas (HT) in the database was changed from ?V? (vapor) to ?G? (gas)
3) Kd values from the database are now available in the Exposure Module/Soil Leaching tab
4) User Defined Surface Water is now allowed as an input to the Acute Exposure Module
5) The Known EPF module was revised to allow input of soil, ground, water external inputs
6) The VB coding for the meteorological data processors to convert joint frequency and CEAM data to GENII format were recompiled for later OS
7) The Sensitivity capability for the surface water module was enhanced
8) An inappropriate adjustment of irrigation rate was corrected in the Acute water module
9) Typographical errors in the SDD Tables 8.2 ? 8.5 were corrected.
10) The ATO output mirror in the air report has been corrected to list all 16 directions
11) The decay chain for Mo99 has been corrected to include Tc99m
12) Output of the GENII puff model has been expanded to up to a 41 by 41 grid; the reporting cutoff for external dose in the puff models has been decreased 
13) The acute-period aquatic pathway names in the report generator have been corrected (the chronic period names were being used)
NOTE: Windows 8 and Windows 10 users should turn off the touch screen capabilities for use with FRAMES
NOTE: Non-US users must adjust the code page to be compliant with US standards for True/False and dot-as-decimal use.


GENII Version 2.10.1
9 September 2012

Version 2.10.1 was prepared in response to DOE Report No. HSS-CR-2011-01, Software Evaluation of GENII V2.10 with FRAMES V1.7 for Inclusion in the DOE Safety Software Central Registry.  This report described several QA-related steps necessary for complete acceptance of GENII.  These included some changes in GENII management oversight at PNNL, some additional review and testing, and other changes.  As a result of the subsequent detailed reviews, some minor changes were made to Version 2.10.  These included:
1) Correction of the C14 contribution in the air report generator;
2) Addition of the population file input to air Report EPA file;
3) Change of air module error writes to an err file, not to the DOS screen;
4) Trap for zero mixing heights in met file; some imported met files do not meet specifications – so there was a need to gracefully detect potential error conditions; Added additional error traps;
5) Substantial suggestions for text revisions in numerous sections of the SDD and Users Manual - Revision 4 of each is now included in the distribution package;
6) Submersion doses calculated using multi-year meteorological files were not annual; external doses accumulated for as many hours as occured in the met data; this has been changed to annual-average values in the Puff and Plume chronic modules;
7) Logic correction has been made to allow multiple User Defined inputs in one Site for the Chronic Exposure Module;
8) Capability had inadvertently been lost in earlier updates of the aability to turn off the ATO mirror in the air Report Generator. This was repaired;
9) Additional input parameters are now output in all Report Generators for QA checking, along with formatting improvements;
10) An error in the Surface Water report generator if all zero doses, causing an indicator to not be set, was corrected;
11) Multiple particle sizes were not getting appropriate settling velocities in all plume models, this was repaired;
12) Added direction randomization for calms, as in chronic plume air module, to the 95% air module because results reported in 10-degree increments would have been over-reported in the cardinal directions;
13) Errors occurred in the Biota module under certain inputs were the code was using old defaults if zero values were input, giving odd results.  Also, some cases allowed an unreported overflow condition that was prevented;
14) Updated the default suggestions in HEI help file for plutonium to suggest "M" for nitrates;
15) Increased length of file name for population file in the air report generator to allow for deeper directory structures.


GENII Version 2.10
1 June 2010

Version 2.10 was released on an expedited basis to correct two significant errors found in the water and air report generators.  It also includes several less-significant upgrades to version 2.09 functionality.  The significant errors are:

1)	The user input of population and food production grids in the Atmospheric Pathway Report Generator, introduced in Version 2.09, was found to be mis-labeled by one sector.  Existing files were imported and functioned correctly if prepared as described in the Users Guide, but the labeling in the User Interface was incorrect.  The sectors begin in the first sector clockwise from north (e.g., 22.5 degrees if 16 sectors are used or 10 degrees if 36 sectors are used) and the final sector is north (0/360 degrees).  Labels have been corrected to prevent user misinterpretation; additional discussion of the intermediate files has been added to the Users Guide, and
2)	The summary output of the Surface Water Report Generator was not including Drinking Water because of a difference in labeling between the User Interface (i.e. “Drinking Water”, “ingestion”) and the calculation module (i.e., “Water”, “ingestion”).  The calculation module has been revised to accept either or both as equivalent.  The population was not correctly used in the estimation of population cancer incidence; this has also been corrected.

Other changes include:
3)	Increasing the allowable volume of an impoundment in the Surface Water User Interface to 1E10 (independent of units);
4)	Revision of the Surface Water module so that the time integration of the source is performed using the times input in the Source Term module rather than the “Duration of the Release” input parameter;
5)	Revision of the label of the surface contamination in the Acute Puff module to “Total Deposition”.  This has no user impact; it makes future conversion from FRAMES 1.7 to FRAMES 2.x possible;
6)	Revision of the Chronic and Acute Exposure modules to include drinking water consumption by farm animals in the tritium/carbon-14 model from surface water sources even if irrigation is not turned on;
7)	A small addition to the Users Guide, Appendix B, indicating to meteorologists that, in the absence of all precipitation data, a value of “0” rather than “>6” be used for the precipitation code.  If all precipitation data are indicated as being missing, the code would cycle through all hours without producing an answer;
8)	A related change in all GENII atmospheric dispersion modules to trap errors where the precipitation code input is set to zero but a non-zero precipitation rate is entered.  The precipitation code is assumed to be dominant; if it is zero then precipitation rates and types are also assumed to be zero;
9)	A revision to FRAMES and the Contaminant Database module to allow users to add references while in standard use mode, rather than only in stand-alone mode;
10)	The Surface Water viewer address/location fields were expanded, the food production summary table was cleaned up to eliminate unneeded zeros;
11)	A correction to FRAMES and DoAll to eliminate occasional annoyance closings of the code;
12)	Revision of incorrect labels in the database for ICRP-30 ingestion dose factors; soluble and insoluble labels were interchanged.  This has no calculation effect because the code currently selects the larger of these regardless of label.


GENII Version 2.09 
1 December 2009

Significant new functionality was added under contract to US Nuclear Regulatory Commission.
1)The GENII Surface Water module was enhanced to add consideration of impoundments for SW releases prior to 
  discharge into rivers or lakes.  This involved new Fortran coding as well as additional inputs to the Surface Water UI
2)A new Biota Dose module was added.  This module uses the environmental parameters developed for the human 
  Chronic Exposure module, with much of the same logic for accumulation and loss in the environment.  A limited
  number of reference organisms are provided.  This required new Fortran coding, a new Biota Dose UI, and 
  additions to the GENII.mdb database
3)Significant revisions were made to the Atmospheric Release Report Generator, including the capability to 
  implement a food-distribution network for population doses, reporting by nuclide by pathway, and user selection
  of output units.  This required new Fortran coding and updates to the Air Report Generator UI.
4)A new Surface Water Report Generator, parallel in function to the Air Release Report Generator, was added.
  The combination includes a new Surface Water Report Generator UI.
  These new capabilities are described in updates to the GENII Software Design Document and Users Manual; 
  a new biota example case has been added.

5)Enhancements were made to the GENII database, so that default solubility classes are now available in the 
  Health Impacts UI.

6)Corrections were made to the FRAMES XLSChart capability, which provides viewers for intermediate data.  
  This module was not handling cases in which non-decay-progeny nuclides had zero release and therefore all zero consequences.

7)The water immersion dose factor in GENII.mdb had a units correction; prior versions were low by a factor 
  of 1000 due to incorrect units.

8)In earlier versions, the factor of 2 difference between swimming and boating was missing - this has been added.

9)The atmospheric Puff model UI interfaces were updated so that intermittent loss of input data upon re-entry was avoided.

10)Incorrect handling of user-selected units in the Health Impacts UI, Method Parameters tab has been corrected.

11)Minor updates were made to labels in the DES files for Exposure and Surface Water modules.

12)Corrections have been made to handling of the error reporting files; stops due to errors should be more graceful 
   in many instances now.



GENII Version 2.08 Updates
Rev. 21 August 2008

A series of very minor updates and corrections to 2.07, including:

1) Correcting a flag in GENII Chronic Exposure module to allow output of aquatic plants; correct 
   logic for attaching more than one User Defined sources to Chronic Exposure Module
2) Correcting a data statement in GENII Acute Exposure module to allow calculation of elemental tritium
3) Elimination of an old 16-bit numeric underflow trap in GENII Surface Water module, near-shore lake model 
   to eliminate reporting instabilities under certain conditions
4) Correction of SDD Equation 5.32 (text change only)
5) Correction of SDD Equation 11.11 units (text change only)
6) REvision to GENII Acute Exposure Module to put default season change dates into external file SEASON.DAT; 
   complimentary revision to Report Generator to print these dates for acute air cases
7) Update of all files to 2.08; rerun of all example cases.

GENII Version 2.08a updates
Rev. 19 September 2008

Only one significant change, involving FRAMES components only, was made.  The SUM3 uncertainty/sensitivity 
driver was not correctly picking up the effective or organ doses for selections made by pathway (effective 
dose summed over all pathways was working properly) – reported values were all equal to the initial dose 
on the organ list, adrenals for the FGR13 option.  (Proper effective selected for ICRP-30 option).  
1)	Updated frames.dll included in GENII v.2.08a install package
2)	Item 7 above for SEASON.DAT was slightly modified to obtain the file from the default directory 
        rather than explicitly FRAMES directory (code would not run if installed in other than \FRAMES).
3)	Update of all reports to 2.08a, rerun of all example cases.


GENII Version 2.07 Updates
Rev. 18 June 2008

Changes made recently include:

1) Revision of the plume models to accomodate the problem identified in:
Droppo JG, Jr, and BA Napier. "Wind Direction Bias in Generating Wind Roses 
and Conducting Sector-Based Air-Dispersion Modeling."  Journal of the Air 
and Waste Management Association. 2008.
This consists of addition of a small random component to the reported wind 
direction to ensure that binning errors are minimized.  There will be a ~30% reduction
in X/Q for N/E/S/W directions, and smaller increases in the other directions for 
some meteorological input files.

2) A correction of the definition of the "z" parameter for the 
GENII Lake Model in the surface water module in the 
UI input description, help file, and SDD.

3) Correction of the reported start time in the EPF file for the second
time period for acute releases for non-atmospheric cases. (It was reported
as zero, while it should have been a small positive value.)

4) Elimination of the "NOT FOUND" in the release rate summary of the
Report Generator in decay chains (caused by a change in handling of decay
chains in FRAMES).

5) Correction of a recent FRAMES-induced connectivity issue between 
User Defined and the GENII Chronic Exposure modules.

6) Addition of a new 95th percentile atmospheric dispersion module.
This is a variant of the older acute plume model.  95% is defined using the
air concentration; corresponding values of dry and wet deposition and direct
finite plume exposure are maintained. The change includes updates to the SDD,
Users Manual, and example problems.

7) A problem of dropping of significant figures for values in the range of
0.001 to 0.0099 in the SUM3 user interface has been resolved.  Numbers are no
longer converted to exponential notation for very large and very small values.


