Variable Count,Description,Name,Privilege,Version,Updated,Template
40,Module Properties,StochIter,0,2,1,Module
Class,Module class type,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,System
ConSchemeDic,Consumed dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
2
0
1,NormalDist
DatabaseID,Identification number for an online database,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,
Description,Module description lines,1,STRING,FALSE,FALSE,0,4096,,,FALSE,
3,FRAMES 2.0 Sensitivity Uncertainty Module.  ,This is an example of a module that manipulates the inputs,of other modules as a part of a stochastic (random) assessment.
DescriptionCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,3
Dictionary,Id(name) and path of input dictionary,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,StochIter
DiskSpace,Minimum required disk space,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,1
Icon,Name and path of display icon,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\dice.ico
Login,Login for model server,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,
ModelCmdLine,Model command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,
ModelExe,Name and path of Model executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\SUMod.exe
ModelURL,Remote model server URL,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,
Name,Module name,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,StochIter
OperatingSystem,Native operating system,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Windows 95 or better
Password,Password for model server,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,
POCAddress1,Point of contact first address,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,P.O. Box 999
POCAddress2,Point of contact second address,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,
POCCity,Point of contact city,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,Richland
POCCompany,Point of contact company name,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,PNNL
POCContact,Point of contact name,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Karl Castleton
POCCountry,Point of contact country,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,USA
POCEmail,Point of contact email address,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Karl.Castleton@pnl.gov
POCFax,Point of contact fax telephone number,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,(970) 248-1324
POCPerson,Point of contact person,0,STRING,FALSE,TRUE,0,64,,,FALSE,
0
POCPhone,Point of contact telephone number,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,(970) 248-1837
POCState,Point of contact state,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,WA
POCUrl,Point of contact web address,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,http://mepas.pnl.gov:2080
POCZip,Point of contact zip code,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,99352
Processor,Minumum processor required,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,
ProSchemeDic,Produced dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
2
4,SampledValues,Seed,SummaryValues,Iteration
4,Iteration,Seed,SampledValues,SummaryValues
RAM,Minimum required memory,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,32
Reference,Module reference lines,1,STRING,FALSE,FALSE,1,4096,,,FALSE,
0
ReferenceCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,0
Scheme,The name of a connection scheme,1,STRING,FALSE,FALSE,0,0,,,FALSE,
2,NoDB,typical
SystemUpdate,Internal flag tracking if a module has been updated,0,LOGICAL,FALSE,TRUE,0,0,,,FALSE,
1,1
SystemVersion,Internal version of a module used by the system,0,INTEGER,FALSE,TRUE,0,200000,,,FALSE,
0
Tool,Launch from Tool menu if true,0,LOGICAL,FALSE,TRUE,0,1,,,FALSE,
0
UICmdLine,UI command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,
UIExe,Name and path of UI executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\ConTest.exe
Version,Module version description,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,
