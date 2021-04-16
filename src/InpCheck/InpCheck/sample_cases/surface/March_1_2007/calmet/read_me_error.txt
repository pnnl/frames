Well, things were looking ok for a few hours, until 13:10 CST.  There happened to be no surface stations that had a pressure level, so calmet failed.  However, now for the rest of the day, it continues to fail because of that single problematic hour.  In the previous version, if it failed for an hour, that was it.  Now, since we run it from 5am most of the time, any problematic hour will cause a major outage for the rest of the day.


I've attached the calmet input set to show you the trouble, but I don't see any way around this, short of having to do a lot of faking of data to get this robust enough to run on real-time observations.


KL




Hello Kathy:
 
Your are correct...CALMET will need at least one value per hour for each parameter to run successfully.
 
When you were running in the "single-hour" mode, it did not depend on the quality of the data from previous hours.
 
However, now you are running CALMET such that simulations with a start time occurring after 5 a.m. will require complete data from 5 a.m. to the simulation start time.  You will need to verify that you have at least one value for each parameter for each hour.   You can do this by:
1.  Persisting the last good hour's value
2.  Interpolating between two known hourly values
3.  Setting a default.
 
Any of the methods would work, but one method might be better for different parameters (e.g., defaulting pressure and interpolating temperature).
 
I know running CALMET in this mode requires a lot more consideration (like the above), but it is necessary to produce the full variable set that is required by MEDOC/HPAC.  The previous implementation of CALMET (the single-hour simulation) really only calculated a diagnostic wind fields; it did not calculate the evolution of the boundary layer (e.g., surface heat flux, mixed layer height) which is what HPAC (via the MEDOC file) expects.
 
Let us know if we can help.
 
thanks,
Jeremy