Variable Count,Description,Name,Privilege,Version,Updated,Template
40,Module Properties,MEPAS 4.1 River Module,0,2,1,Module
Class,Module class type,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,Model
ConSchemeDic,Consumed dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
2
33,Aquifer,Location,ChemFlux,ChemList,ChemPhysicalProperties,RadFlux,RadList,RadPhysicalProperties,WaterFlux,ChemPhysicalProperties,ChemPartitioning,ChemDiffusionCoefficient,ChemThermodynamics,ChemExposureFactors,ChemBCF,Organism,ChemSoiltoPlantTF,PlantType,ChemEnvironmentalRates,ChemAirToPlantTF,RadPhysicalProperties,RadPartitioning,RadDiffusionCoefficient,RadThermodynamics,RadExposureFactors,RadDosimetry,RadBCF,RadFeedToAnimalTF,AnimalType,RadSoilToPlantTF,RadEnvironmentalRates,ChemToxicity,RadToxicity
33,Surface Water,Location,ChemFlux,ChemList,ChemPhysicalProperties,RadFlux,RadList,RadPhysicalProperties,WaterFlux,ChemPhysicalProperties,ChemPartitioning,ChemDiffusionCoefficient,ChemThermodynamics,ChemExposureFactors,ChemBCF,Organism,ChemSoiltoPlantTF,PlantType,ChemEnvironmentalRates,ChemAirToPlantTF,RadPhysicalProperties,RadPartitioning,RadDiffusionCoefficient,RadThermodynamics,RadExposureFactors,RadDosimetry,RadBCF,RadFeedToAnimalTF,AnimalType,RadSoilToPlantTF,RadEnvironmentalRates,ChemToxicity,RadToxicity
DatabaseID,Identification number for an online database,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,
Description,Module description lines,1,STRING,FALSE,FALSE,0,4096,,,FALSE,
29,MODULE DESCRIPTION,MEPAS 4.1 River Module,The MEPAS River module simulates the movement,of radionuclides and chemicals through,non-tidal rivers.  The migration and fate of,constituents through the riverine environment are,"described by the one-dimensional advective,",one-dimensional dispersive (lateral) equation for,solute transport.,Some Key Assumptions:,1. Flow in the river is steady and uniform in the,longitudinal direction.,2. Constituent adsorption to sediment particles in,"the water column or river bed is negligible, so",all constituents travel at the same speed as the,river flow.,3. Constituent releases to the river are long-term,relative to the travel time in the river.,"Therefore, the steady-state solution to the",advective-dispersive equation is used.,4. The river geometry can be represented by a,rectangular cross-section.,5. Advection dominates dispersion in the,longitudinal direction and constituents are,"assumed to be fully mixed vertically, so",dispersion is considered only in the lateral,direction.,6. Degradation/decay for all constituents is,first-order.
DescriptionCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,29
Dictionary,Id(name) and path of input dictionary,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,MepSW
DiskSpace,Minimum required disk space,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,4
Icon,Name and path of display icon,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\Icons\riv.ico
Login,Login for model server,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,
ModelCmdLine,Model command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,/EXE ""C:\FRAMESv1\"" ""MepSW.des""
ModelExe,Name and path of Model executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\Mepas Modules\Run1XMod.Exe
ModelURL,Remote model server URL,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,
Name,Module name,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,MEPAS 4.1 River Module
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
1,Gene Whelan
POCCountry,Point of contact country,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,USA
POCEmail,Point of contact email address,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,gene.whelan@pnl.gov
POCFax,Point of contact fax telephone number,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,
POCPerson,Point of contact person,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Gene Whelan
POCPhone,Point of contact telephone number,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,
POCState,Point of contact state,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,WA
POCUrl,Point of contact web address,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,http://mepas.pnl.gov/earth/req/riverreq.html
POCZip,Point of contact zip code,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,99352
Processor,Minumum processor required,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Pentium
ProSchemeDic,Produced dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
2
4,Surface Water,Location,ChemWaterConc,RadWaterAct
4,Surface Water,Location,ChemWaterConc,RadWaterAct
RAM,Minimum required memory,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,16
Reference,Module reference lines,1,STRING,FALSE,FALSE,1,4096,,,FALSE,
12,MODULE REFERENCES,A more complete list of key assumptions and the,associated discussion of them can be found in the,formulation document.,"Whelan, G. and J. P. McDonald.  1996.  The",Multimedia Environmental Pollutant Assessment,System (MEPAS)(R): Riverine Pathway,"Formulations.  PNNL-11176, Pacific Northwest","National Laboratory, Richland, Washington.",Other related sites:,http://mepas.pnl.gov/earth,http://mepas.pnl.gov/FRAMESV1
ReferenceCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,12
Scheme,The name of a connection scheme,1,STRING,FALSE,FALSE,0,0,,,FALSE,
2,Surface Water (1),Surface Water (2)
SystemUpdate,Internal flag tracking if a module has been updated,0,LOGICAL,FALSE,TRUE,0,0,,,FALSE,
1,1
SystemVersion,Internal version of a module used by the system,0,INTEGER,FALSE,TRUE,0,200000,,,FALSE,
1,2
Tool,Launch from Tool menu if true,0,LOGICAL,FALSE,TRUE,0,1,,,FALSE,
1,0
UICmdLine,UI command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,/UI ""C:\FRAMESv1\"" ""MepSW.des""
UIExe,Name and path of UI executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\Mepas Modules\Run1XMod.Exe
Version,Module version description,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,Version 2.1
