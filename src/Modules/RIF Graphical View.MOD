Variable Count,Description,Name,Privilege,Version,Updated,Template
40,Module Properties,RIF Graphical View,0,2,1,Module
Class,Module class type,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,Model
ConSchemeDic,Consumed dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
1
18,Receptor Intakes,Location,AgeGroups,ChemDermalIntake,ChemList,ChemPhysicalProperties,ChemIngestIntake,ChemInhalIntake,ChemInhalExpConc,RadExtDos,RadList,RadPhysicalProperties,RadExtSoilConc,RadExtAirConc,RadExtLiqConc,RadIngestIntake,RadDermalIntake,RadInhalIntake
DatabaseID,Identification number for an online database,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,
Description,Module description lines,1,STRING,FALSE,FALSE,0,4096,,,FALSE,
4,MODULE DESCRIPTION,"This module will display, in spreadsheet and a graphical chart, the contents of the section in an RIF file specified by the commandline passed from FRAMES to this viewer.",IMPORTANT NOTE:,Requires Microsoft Excel to be installed.
DescriptionCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,4
Dictionary,Id(name) and path of input dictionary,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,
DiskSpace,Minimum required disk space,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,4
Icon,Name and path of display icon,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\Icons\vwr.ico
Login,Login for model server,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,
ModelCmdLine,Model command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,/EXE ""C:\FRAMESv1\"" ""RIFchart.des""
ModelExe,Name and path of Model executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\Mepas Modules\Run1XMod.Exe
ModelURL,Remote model server URL,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,
Name,Module name,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,RIF Graphical View
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
1,Pacific Northwest National Laboratory
POCContact,Point of contact name,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Bonnie Hoopes
POCCountry,Point of contact country,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,USA
POCEmail,Point of contact email address,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,bonnie.hoopes@pnl.gov
POCFax,Point of contact fax telephone number,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,
POCPerson,Point of contact person,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Bonnie Hoopes
POCPhone,Point of contact telephone number,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,
POCState,Point of contact state,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,WA
POCUrl,Point of contact web address,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,http://mepas.pnl.gov/FRAMESV1
POCZip,Point of contact zip code,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,99352
Processor,Minumum processor required,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Pentium
ProSchemeDic,Produced dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
0
RAM,Minimum required memory,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,4
Reference,Module reference lines,1,STRING,FALSE,FALSE,1,4096,,,FALSE,
3,MODULE REFERENCES,Other related sites:,http://mepas.pnl.gov/earth
ReferenceCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,3
Scheme,The name of a connection scheme,1,STRING,FALSE,FALSE,0,0,,,FALSE,
1,Receptor Intakes
SystemUpdate,Internal flag tracking if a module has been updated,0,LOGICAL,FALSE,TRUE,0,0,,,FALSE,
1,1
SystemVersion,Internal version of a module used by the system,0,INTEGER,FALSE,TRUE,0,200000,,,FALSE,
1,2
Tool,Launch from Tool menu if true,0,LOGICAL,FALSE,TRUE,0,1,,,FALSE,
1,0
UICmdLine,UI command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,
UIExe,Name and path of UI executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,
Version,Module version description,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,Version 2.1
