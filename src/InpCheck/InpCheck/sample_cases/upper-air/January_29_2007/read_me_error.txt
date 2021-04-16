well, it's back to complaining about the cell face height....  I've attached a piz file containing the 3 files I"m creating.  I don't see where there it's getting the Top Sounding height of 5142.  It's not in the data file.  And why does it say 21 are the number of sounding levels?  There are 13 in the file...


anl.bwic.modelserver.ModelWSException: Cannot parse Calmet output:
ERROR IN SUBR. VERTAV -- cell face is above top sounding level     cell face height =     6000.0     No. sounding levels =    21     Top sounding height =     5142.0



Hello Kathy:
 
This helps a lot...the last height in the vertical sounding (UP.dat) needs to be greater than 6000 meters (above ground level), which is the last vertical level in the CALMET domain.  The reason it needs to be above 6000 meters is because CALMET requires values to either side to do the interpolation.
 
Why is the last domain height 6000 meters?  Two reasons:
1.  To accommodate all the possible locations BWIC will be implemented
2.  To accommodate the vertical coordinate transformation for the MEDOC format.
 
In short, you'll want to be sure that your UP.dat files extend beyond 6000 m.
 
Let me know if this helps,
thanks,
Jeremy
