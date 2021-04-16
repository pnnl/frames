Well, while trying to figure out how to deal with the missing data, another new error message I hadn't seen occurred.


 ERROR in subr. CGAMMA -- bottom of sounding is missing -- TSAA =     999.9000     TSBB =    999.9000     Date of AA sounding (YYYYDDDHH) =     200706100 Date of BB sounding (YYYYDDDHH) =    200706112


KL



Hello Kathy:
 
This error is very subtle, but again has to do with the boundary conditions CALMET imposes on the UP1.dat file.  Basically CALMET requires two complete sounding levels--the first sounding level and one sounding level above the the last ZFACE height (which is 6000 m in BWIC).  This is the minimum requirement to be able to interpolate values to all levels in the domain.  In the sample file you sent me, the first level is missing temperature (999.9) for all three time periods.
 
Another issue I noted has to do with the height of the first two pressure levels in you files.  Specifically, the height of the first pressure surface is higher than the height of the second pressure surface for each time period.  For example, for the first sounding time period, the 992.0 mb pressure level occurs at 609 meters above sea level (ASL).  The next pressure level--969.0 mb-- occurs at 178 meter ASL.  Basically, pressure is increasing with height in the first two layers.  Are you getting the upper air data from FSL (I forget their new name!)?  Are you using mandatory levels only?  Or are you including significant levels too?
 
In any event, I have attached two upper air files.  The first file (up1.dat) just removes your first sounding level (for all time periods), such that the first level is complete and that makes CALMET happy.  The second file (up1_2_levels_only.dat) is to illustrate the fact that CALMET only needs two complete levels (that bracket the model domain) in order to run successfully.  If you can meet that requirement, then these errors with the upper air data should go away.
 
I hope this helps and let us know if there's anything else.
 
thanks,
Jeremy