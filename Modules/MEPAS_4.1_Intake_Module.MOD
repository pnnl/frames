Variable Count,Description,Name,Privilege,Version,Updated,Template
40,Module Properties,MEPAS 4.1 Intake Module,0,2,1,Module
Class,Module class type,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,Model
ConSchemeDic,Consumed dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
1
41,ChemPhysicalProperties,ChemList,ChemPartitioning,ChemDiffusionCoefficient,ChemThermodynamics,ChemExposureFactors,ChemBCF,Organism,ChemSoiltoPlantTF,PlantType,ChemEnvironmentalRates,ChemAirToPlantTF,RadPhysicalProperties,RadList,RadPartitioning,RadDiffusionCoefficient,RadThermodynamics,RadExposureFactors,RadDosimetry,RadBCF,RadFeedToAnimalTF,AnimalType,RadSoilToPlantTF,RadEnvironmentalRates,Exposure Pathways Concentration,Location,ChemDermalGasConc,ChemDermalLiqConc,ChemDermalSolidConc,ChemIngestLiqConc,ChemIngestSolidConc,ChemInhalConc,RadDermalGasAct,RadDermalLiqAct,RadDermalSolidAct,RadIngestLiqAct,RadIngestSolidAct,RadInhalAct,RadExtDos,ChemToxicity,RadToxicity
DatabaseID,Identification number for an online database,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,
Description,Module description lines,1,STRING,FALSE,FALSE,0,4096,,,FALSE,
21,MODULE DESCRIPTION,MEPAS 4.1 Intake Module,The MEPAS intake module evaluates the,intake or exposure of an individual from consumption,"or contact with contaminated media, or exposure to",radiation in contaminated media.  Standard EPA,methods are used to evaluate the average daily,intake rate of chemical pollutants for each exposure,"pathway, based on user defined consumption/contact",rates and body weight. The module includes,"consideration of domestic water use, farm product","consumption, aquatic food consumption, surface water","recreational activities, soil contact exposure, and",air exposures.  Both chemical and radioactive,pollutants may be evaluated.  EPA models are used,to evaluate dermal contact with soil and water.,The module evaluates intakes for one age group per,receptor definition and for all input exposure routes,defined in the EPF file and recognized by MEPAS.,Limitations:,None
DescriptionCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,21
Dictionary,Id(name) and path of input dictionary,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,MepRCP
DiskSpace,Minimum required disk space,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,4
Icon,Name and path of display icon,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\Icons\rcp.ico
Login,Login for model server,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,
ModelCmdLine,Model command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,/EXE ""C:\FRAMESv1\"" ""MepRCP.des""
ModelExe,Name and path of Model executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\Mepas Modules\run1xmod.exe
ModelURL,Remote model server URL,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,
Name,Module name,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,MEPAS 4.1 Intake Module
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
1,Dennis Strenge
POCCountry,Point of contact country,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,USA
POCEmail,Point of contact email address,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,dennis.strenge@pnl.gov
POCFax,Point of contact fax telephone number,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,
POCPerson,Point of contact person,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Dennis Strenge
POCPhone,Point of contact telephone number,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,
POCState,Point of contact state,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,WA
POCUrl,Point of contact web address,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,http://mepas.pnl.gov/earth/req/intakreq.htm
POCZip,Point of contact zip code,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,99352
Processor,Minumum processor required,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Pentium
ProSchemeDic,Produced dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
1
14,Receptor Intakes,Location,AgeGroups,ChemDermalIntake,ChemIngestIntake,ChemInhalIntake,ChemInhalExpConc,RadExtDos,RadExtSoilConc,RadExtAirConc,RadExtLiqConc,RadIngestIntake,RadDermalIntake,RadInhalIntake
RAM,Minimum required memory,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,16
Reference,Module reference lines,1,STRING,FALSE,FALSE,1,4096,,,FALSE,
8,MODULE REFERENCES,"Strenge, D.L., and P.J. Chamberlain.  1995.",Multimedia Environmental Pollutant Assessment System (MEPAS):,Exposure Pathway and Human Health Impact Assessment Models.,"PNL-10523.  Pacific Northwest Laboratory, Richland, WA.",Other related sites:,http://mepas.pnl.gov/earth,http://mepas.pnl.gov/FRAMESV1
ReferenceCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,8
Scheme,The name of a connection scheme,1,STRING,FALSE,FALSE,0,0,,,FALSE,
1,Receptor Intakes
SystemUpdate,Internal flag tracking if a module has been updated,0,LOGICAL,FALSE,TRUE,0,0,,,FALSE,
1,1
SystemVersion,Internal version of a module used by the system,0,INTEGER,FALSE,TRUE,0,200000,,,FALSE,
1,2
Tool,Launch from Tool menu if true,0,LOGICAL,FALSE,TRUE,0,1,,,FALSE,
1,0
UICmdLine,UI command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,/UI ""C:\FRAMESv1\"" ""MepRCP.des""
UIExe,Name and path of UI executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\MEPAS Modules\run1xmod.exe
Version,Module version description,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,Version 2.1
