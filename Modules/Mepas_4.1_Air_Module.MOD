Variable Count,Description,Name,Privilege,Version,Updated,Template
40,Module Properties,Mepas 4.1 Air Module,0,2,1,Module
Class,Module class type,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,Model
ConSchemeDic,Consumed dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
0
DatabaseID,Identification number for an online database,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,
Description,Module description lines,1,STRING,FALSE,FALSE,0,4096,,,FALSE,
40,MODULE DESCRIPTION,MEPAS 4.1 Air Module,The MEPAS air module estimates constituent air,concentrations and deposition rates using standard,Gaussian dispersion models.  By using local,"climatological and site characteristics data, this air",module accounts for local and regional influences.,A complex-terrain option can account for the influence,of local nocturnal wind channeling near the source.,"Typically, this air module is used to calculate",long-term (i.e. annual or longer) regional constituent,air concentrations and deposition rates.  The module,has relatively short run times making it useful in,"uncertainty and sensitivity studies, or in other cases",where model run time is an issue.,Some Key Assumptions:,Assumes sector-average straight-line Gaussian,transport from source.,Considers surface area releases.  Accounts for,initial transverse and lateral area dilution effects,Considers elevated stack/vent releases including,plume rise and plume impingement on terrain,Uses climatological wind-stability frequency tables.,Computes plume depletion and surface deposition by,wet and dry deposition. Uses dry deposition similarity,model that allows removal rates to vary with,"meteorological, surface, and constituent properties.",Three particle size classes are considered.,Uses a limited generic precipitation-scavenging model.,Assumes no decay or chemical reactions during time of,airborne transport.,Computes concentrations at distances of 100m or greater,from source center.,"Assumes that the particle sizes are 7.5um for particle 1,","3.0um for particle  2, and 0.3um for particle 3.",This model DOES NOT currently read in different,particle sizes.,The local channelling and complex terrain options for,this model have been turned off for this version,because they have not been fully tested.
DescriptionCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,40
Dictionary,Id(name) and path of input dictionary,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,MepAIR
DiskSpace,Minimum required disk space,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,4
Icon,Name and path of display icon,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\Icons\air.ico
Login,Login for model server,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,
ModelCmdLine,Model command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
0
ModelExe,Name and path of Model executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,
ModelURL,Remote model server URL,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,
Name,Module name,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,Mepas 4.1 Air Module
OperatingSystem,Native operating system,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,WIN 95 / NT
Password,Password for model server,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,
POCAddress1,Point of contact first address,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,PO Box 999
POCAddress2,Point of contact second address,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,
POCCity,Point of contact city,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,Richland
POCCompany,Point of contact company name,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Pacific Northwset National Laboratory
POCContact,Point of contact name,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,John Buck
POCCountry,Point of contact country,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,USA
POCEmail,Point of contact email address,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,john.w.buck@pnl.gov
POCFax,Point of contact fax telephone number,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,
POCPerson,Point of contact person,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,John Buck
POCPhone,Point of contact telephone number,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,
POCState,Point of contact state,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,WA
POCUrl,Point of contact web address,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,http://mepas.pnl.gov/earth/req/airreq.html
POCZip,Point of contact zip code,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,99352
Processor,Minumum processor required,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Pentium
ProSchemeDic,Produced dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
0
RAM,Minimum required memory,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,16
Reference,Module reference lines,1,STRING,FALSE,FALSE,1,4096,,,FALSE,
9,MODULE REFERENCES,A detailed description of the assumptions and theoretical,foundations can be found in the formulation document:,"Droppo, JG and JW Buck. 1996. Multimedia Environmental",Pollutant Assessment System (MEPAS): Atmospheric,Pathway Formulations.,Other related sites:,http://mepas.pnl.gov/earth,http://mepas.pnl.gov/FRAMESV1
ReferenceCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,9
Scheme,The name of a connection scheme,1,STRING,FALSE,FALSE,0,0,,,FALSE,
0
SystemUpdate,Internal flag tracking if a module has been updated,0,LOGICAL,FALSE,TRUE,0,0,,,FALSE,
1,1
SystemVersion,Internal version of a module used by the system,0,INTEGER,FALSE,TRUE,0,200000,,,FALSE,
1,2
Tool,Launch from Tool menu if true,0,LOGICAL,FALSE,TRUE,0,1,,,FALSE,
1,0
UICmdLine,UI command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,/UI ""C:\FRAMESv1\"" ""MepAIR.des""
UIExe,Name and path of UI executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\Mepas Modules\Run1XMod.Exe
Version,Module version description,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,Version 2.1
