Variable Count,Description,Name,Privilege,Version,Updated,Template
40,Module Properties,MEPAS 4.1 Source in Soil,0,2,1,Module
Class,Module class type,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,Model
ConSchemeDic,Consumed dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
1
26,ChemPhysicalProperties,ChemList,ChemPartitioning,ChemDiffusionCoefficient,ChemThermodynamics,ChemExposureFactors,ChemBCF,Organism,ChemSoiltoPlantTF,PlantType,ChemEnvironmentalRates,ChemAirToPlantTF,RadPhysicalProperties,RadList,RadPartitioning,RadDiffusionCoefficient,RadThermodynamics,RadExposureFactors,RadDosimetry,RadBCF,RadFeedToAnimalTF,AnimalType,RadSoilToPlantTF,RadEnvironmentalRates,ChemToxicity,RadToxicity
DatabaseID,Identification number for an online database,0,STRING,FALSE,TRUE,0,0,,,FALSE,
1,
Description,Module description lines,1,STRING,FALSE,FALSE,0,4096,,,FALSE,
91,MODULE DESCRIPTION,MEPAS 4.1 Computed Source Term in Soil,The MEPAS Computed Source Term Release Module (CSTRM) is a,constantly stirred tank reactor (CSTR) model that,assumes that the contamination is instantly,distributed homogeneously throughout the defined,source volume. The CSTRM assumes that the,contaminants may be present in multiple phases,"(i.e., in aqueous solution, sorbed to solid","particles, in vapor-filled pore space, or in a",separate non-aqueous-phase liquid [NAPL] that is,immiscible with water and air).,The CSTRM can be used to compute the environmental,"release rates (i.e., water infiltration, wind and",sediment erosion) and contaminant emissions of,radionuclides and chemicals from surface water,"impoundments, unsaturated soil (i.e., vadose zone),","and saturated soil (i.e., aquifer) sources.",CONTAMINANT LOSS MECHANISMS,This version of the MEPAS CSTRM allows for loss of,contaminant mass from unremediated sites via the,following mass loss routes depending on the source,medium:,"- Leaching to groundwater,","- Overland to surface water by water erosion,","- Suspension by wind erosion to the atmosphere,","- Volatilization to the atmosphere,","- Decay/degradation, and",- External inflow/outflow,These contaminant loss routes can be used in,combinations or individually.  The wind/sediment,erosion and water infiltration rates can be,calculated by the model or they can be input,directly by the user.,MASS BALANCE AND PARTITIONING,The CSTRM calculates mass balance for the different,loss routes for each contaminant as a function,of time.  These mass loss routes describe the,simultaneous loss of contaminants from the source,"zone to the environmental media (i.e., vadose zone,","saturated zone, and surface water).  The mass of each",contaminant is recomputed and redistributed to each,of the four phase for each time until all mass is,gone or the defined simulation end time is reached.,In the case where the simulation period terminates the,evaluation before all the mass is release from the,"source, mass balance is preserve if the remaining mass",at the source is taken into account. When multiple loss,"processes are assumed to occur simultaneously, the",STRM accounts for the mass partitioning to the,"different environmental media, while maintaning mass",balance.,"In some cases, the user may know the contaminant",emissions for thr set of contaminants for one or more,mass loss route but not for all routes.  This module,will allow the user to enter know contaminant emissions,for one or more mass loss route and will compute,contaminant emissions for the other mass loss routes,"based on initial mass, environmental release rates",and contaminant physical/chemical properties.  This,module will maintain mass balance for these cases and,conduct mass partitioning to the different media based,on the user input and computed results.  In order to,"maintain mass balance, the model may not provide the",same time-varying contaminant emission curves as,input by the user because other losses are computed,besides the user input losses.  User-defined masses,will always be maintained by the model but the duration,of the contaminant emissions may be shorter than the,user input to maintain mass balance for the entire,system.,It should be noted that this is a semi-analytical model,that is meant to be used in screening level or,preliminary assessments.  Testing has indicated that,it may overestimate mass released from the source by,as much as 4.6%.,PATH DEPENDENCE,This MEPAS CSTRM calculates each mass loss route,individually and all contaminants evaluated for,that route must have the same type of input data,"(i.e., known contaminant emissions or known initial",mass and physical/chemical properties).  Information,for different mass loss routes can vary for a given,source.,DECAY PRODUCTS,This MEPAS CSTRM evaluates the ingrowth of progeny,products via decay from the parent contaminants,and are released using the same physical/chemical,processes which govern the release of the parent,contaminants.  The initial mass of progeny is,assumed to be zero.
DescriptionCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,91
Dictionary,Id(name) and path of input dictionary,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,MepSrcSl
DiskSpace,Minimum required disk space,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,4
Icon,Name and path of display icon,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\Icons\src.ico
Login,Login for model server,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,
ModelCmdLine,Model command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,/EXE ""C:\FRAMESv1\"" ""MepSrcSl.des""
ModelExe,Name and path of Model executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\Mepas Modules\Run1XMOd.exe
ModelURL,Remote model server URL,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,
Name,Module name,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,MEPAS 4.1 Source in Soil
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
1,http://mepas.pnl.gov/earth/req/srcreq.html
POCZip,Point of contact zip code,0,STRING,FALSE,TRUE,0,16,,,FALSE,
1,99352
Processor,Minumum processor required,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,Pentium
ProSchemeDic,Produced dictionary names,2,STRING,FALSE,FALSE,0,0,,,FALSE,,Module.Scheme
1
12,Vadose,Location,ChemFlux,RadFlux,WaterFlux,Surface Water,Air Emission Rates,Temperature,Soil,ChemSoilConc,RadSoilAct,SoilFlux
RAM,Minimum required memory,0,INTEGER,FALSE,TRUE,0,512,,,FALSE,
1,16
Reference,Module reference lines,1,STRING,FALSE,FALSE,1,4096,,,FALSE,
14,MODULE REFERENCES,A complete description of the assumptions and,theoretical foundations used in this model can be,found in:,"Streile, G.P., K.D. Shields, J.L. Stroh,","L.M. Bagaasen, G. Whelan, J.P. McDonald,","J.G. Droppo, and J.W. Buck.  The Multimedia",Environmental Pollutant Assessment System,"(MEPAS): Source-Term Release Formulations,","PNNL-11248, November 1996, Pacific Northwest","National Laboratory, Richland, WA.",Other related sites:,http://mepas.pnl.gov/earth,http://mepas.pnl.gov/FRAMESV1
ReferenceCount,,0,INTEGER,FALSE,TRUE,0,0,,,FALSE,
1,14
Scheme,The name of a connection scheme,1,STRING,FALSE,FALSE,0,0,,,FALSE,
1,Source in Soil
SystemUpdate,Internal flag tracking if a module has been updated,0,LOGICAL,FALSE,TRUE,0,0,,,FALSE,
1,1
SystemVersion,Internal version of a module used by the system,0,INTEGER,FALSE,TRUE,0,200000,,,FALSE,
1,2
Tool,Launch from Tool menu if true,0,LOGICAL,FALSE,TRUE,0,1,,,FALSE,
1,0
UICmdLine,UI command line switches,0,STRING,FALSE,TRUE,0,64,,,FALSE,
1,/UI ""C:\FRAMESv1\"" ""MepSrcSl.des""
UIExe,Name and path of UI executable,0,STRING,FALSE,TRUE,0,512,,,FALSE,
1,C:\FRAMESV2\MEPAS Modules\Run1XMod.exe
Version,Module version description,0,STRING,FALSE,TRUE,0,32,,,FALSE,
1,Version 2.1
