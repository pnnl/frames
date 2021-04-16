I'm back to trying to get calmet running the "new" way.  I've attached the input files I'm creating that output all the data from 5am til current and I'm getting an error that it is attempting to read past the end of the up1.dat file.  Could you please check the BWICCALMET.inp and up1.dat files that I'm creating to see if they are consistent?  We used to run the duration of the calmet run as 1 hour, but now I'm calculating it from the end time and starttime, since I have to run from 5am...   I don't know why the upper air reader would want to read 4 records.   I'm already persisting the last reading ahead 12 hours to bound the run time, so I'm not sure what else to do here and would appreciate your input.


thanks, 
KL


09:22:52,189 INFO  [CalmetModelWrapper] Executing command: /apps/bwic/sandboxes/../models/calmet/CalmetWrapper from directory: /apps/bwic/sandboxes/automatedServerRuns/calmet
09:22:52,473 INFO  [STDOUT] FORTRAN STOP
09:22:54,103 INFO  [STDOUT] FORTRAN STOP
09:22:54,599 INFO  [STDOUT] FORTRAN STOP
09:22:54,955 INFO  [STDOUT] PGFIO-F-231/formatted read/unit=30/error on data conversion.
09:22:54,955 INFO  [STDOUT]  File name = up1.dat    formatted, sequential access   record = 4
09:22:54,955 INFO  [STDOUT]  In source file CALMET.f, at line number 11801
09:22:54,969 INFO  [STDOUT] PGFIO-F-217/unformatted read/unit=7/attempt to read past end of file.
09:22:54,969 INFO  [STDOUT]  File name = ./calmet/calbwic.dat    unformatted, sequential access   record = 14
09:22:54,970 INFO  [STDOUT]  In source file CALMET2HPAC.f, at line number 539
09:22:54,971 INFO  [STDOUT]                                                                                                                         Starting TERREL Processor
09:22:54,971 INFO  [STDOUT] TERREL Processor Finished
09:22:54,971 INFO  [STDOUT] Starting CTGPROC Processor
09:22:54,971 INFO  [STDOUT] CTGPROC Processor Finished
09:22:54,971 INFO  [STDOUT] Starting MAKEGEO Processor
09:22:54,971 INFO  [STDOUT] MAKEGEO Processor Finished
09:22:54,971 INFO  [STDOUT] Starting CALMET Model
09:22:54,971 INFO  [STDOUT] CALMET Model Finished
09:22:54,971 INFO  [STDOUT] Processing Calmet.lst File
09:22:54,971 INFO  [STDOUT] Processing Calmet.lst File Finished
09:22:54,971 INFO  [STDOUT] Processing Calmet.dat File
09:22:54,971 INFO  [STDOUT] Processing Calmet.dat File Finished




Hello Kathy:
 
Sorry for the delay in my reply.
 
The up1.dat file appears to be in error.  The general data order for the up.dat file is:
Pressure(mb) / Height(m) /Temperature(K) / Wind Direction(degrees)/ Wind Speed(m/s)
 
With that in mind, look at some of the data you have for the pressure surfaces in your up1.dat file (first data block):
   1000.0/   98/10273.1/99999/99999    990.0/  178/277.1/190/ 31    976.0/  294/280.5/99999/99999    975.0/  304/10273.1/200/ 67
    944.0/  568/282.1/99999/99999    939.0/  609/10273.1/230/113    925.0/  738/281.1/235/113    905.0/  914/10273.1/245/123
    872.0/ 1219/10273.1/255/139    862.0/ 1313/277.4/99999/99999    850.0/ 1430/277.8/265/154    809.0/ 1828/10273.1/275/144
    805.0/ 1870/276.8/99999/99999    779.0/ 2133/10273.1/280/144    750.0/ 2438/10273.1/290/154    742.0/ 2522/271.5/99999/99999
    722.0/ 2743/10273.1/290/175    700.0/ 2988/268.9/290/180    646.0/ 3613/264.6/99999/99999    642.0/ 3657/10273.1/280/201
    601.0/ 4168/262.3/99999/99999    593.0/ 4267/10273.1/290/242    547.0/ 4876/10273.1/285/267    545.0/ 4908/256.3/99999/99999
    500.0/ 5560/252.9/285/303    472.0/ 5983/250.3/99999/99999    465.0/ 6096/10273.1/285/324    427.0/ 6705/10273.1/285/329
    422.0/ 6790/243.3/99999/99999    400.0/ 7170/240.1/285/386
 
The 1000 mb pressure surface has a temp of 10273.1 K
The 400 mb pressure surface has a temp a Wind Speed of 386 m/s.
 
Other pressure surfaces display similar characteristics.  Verify the format of your upper-air data file writer or the data that it's reading and see if that corrects the problem.
 
thanks,
Jeremy