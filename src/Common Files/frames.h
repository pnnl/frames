#ifndef FRAMES_H
#define FRAMES_H

#ifndef BUILD_DLL
#ifndef DIRECTACCESS
  #define FRAMES_API __declspec(dllimport) __stdcall
#else
  #define FRAMES_API
#endif
#else
  #define FRAMES_API __declspec(dllexport) __stdcall
#endif

#ifdef __cplusplus
extern "C" {
#endif

//Series.cpp
    void FRAMES_API AddSeries(int Num1, double *Times1, double *Values1,
                                int Num2, double *Times2, double *Values2,
                                int *NumRes, double *TimesRes, double *ValuesRes);
     int FRAMES_API GetProbability(int zeroes, int resolution, int count,
                                     double *intimes, double *invalues,
                                     double *minY, double *maxY,
                                     int *xBegIdx, int *xEndIdx,
                                     double *sprob, double *svalue);
    void FRAMES_API HasSections(int val);
    void FRAMES_API StatSort(int n, int *idx, double *val, double *mean, double *min, double *max, double *median, double *sd);

//Sum3.cpp
     int FRAMES_API Initialize(char *argv0, char *argv1, char *argv2, char *argv3, char *argv4, char *argv5);
     int FRAMES_API Iterate(int iteration);
     int FRAMES_API Finalize();

// 1 based, all indices start at 1 for SCF/WFF/WCF/AFF/BBF
// 0 based, all indices start at 0 for ATO/EPF/RIF/HIF

    //SCFClass.cpp
    void FRAMES_API scfOpen(char *fuiname, char *modId);
    void FRAMES_API scfClose();
     int FRAMES_API scfGetNumRunInfo();
     int FRAMES_API scfGetRunInfo(int Idx, char *info);
     int FRAMES_API scfGetNumSets();
     int FRAMES_API scfGetSetInfo(int dsIdx, char *name, char *type);
//     int FRAMES_API scfGetLocation(int dsIdx, double *x, double *y, double *z);
     int FRAMES_API scfGetDimensions(int dsIdx, double *width, double *length, double *depth);
     int FRAMES_API scfGetNumContam(int dsIdx);
     int FRAMES_API scfGetNumProgeny(int dsIdx, int conIdx);
     int FRAMES_API scfGetContamName(int dsIdx, int conIdx, int progIdx, char *name, char *cas);
     int FRAMES_API scfGetSeriesProperties(int dsIdx, int conIdx, int progIdx, char *vUnits, char *tUnits);
     int FRAMES_API scfGetSeriesValues(int dsIdx, int conIdx, int progIdx, int count, double *times, double *values);
    void FRAMES_API scfAggregate(char *filename, char* casList);
    void FRAMES_API scfInsert(char *filename, char *casList, char *nameList, char *degList, char *secList, int branching);

    //AFFClass.cpp
    void FRAMES_API affOpen(char *fuiname, char *modId);
    void FRAMES_API affClose();
     int FRAMES_API affGetNumRunInfo();
     int FRAMES_API affGetRunInfo(int Idx, char *info);
     int FRAMES_API affGetNumSets();
     int FRAMES_API affGetSetInfo(int dsIdx, char *name, char *type);
//     int FRAMES_API affGetLocation(int dsIdx, double *x, double *y, double *z);
     int FRAMES_API affGetDimensions(int dsIdx, double *area, double *eht, double *aht, double *vel, double *etmp, double *atmp);
     int FRAMES_API affGetNumFluxTypes(int dsIdx);
     int FRAMES_API affGetFluxType(int dsIdx, int fluxIdx, char *type, double *size, char *sunt, double *density, char *dunt);
     int FRAMES_API affGetNumContam(int dsIdx);
     int FRAMES_API affGetNumProgeny(int dsIdx, int conIdx);
     int FRAMES_API affGetSeriesValues(int dsIdx, int conIdx, int progIdx, int fluxIdx, int count, double *times, double *values);
     int FRAMES_API affGetContamName(int dsIdx, int conIdx, int progIdx, char *name, char *cas);
     int FRAMES_API affGetSeriesProperties(int dsIdx, int conIdx, int progIdx, char *vUnits, char *tUnits);
    void FRAMES_API affAggregate(char *filename, char *casList);
    void FRAMES_API affInsert(char *filename, char *casList, char *nameList, char *degList, char *secList, int branching);

    //WFFClass.cpp
    void FRAMES_API wffOpen(char *fuiname, char *modId);
    void FRAMES_API wffClose();
     int FRAMES_API wffGetNumRunInfo();
     int FRAMES_API wffGetRunInfo(int Idx, char *info);
     int FRAMES_API wffGetNumSets();
     int FRAMES_API wffGetSetInfo(int dsIdx, char *name, char *type);
//     int FRAMES_API wffGetLocation(int dsIdx, double *x, double *y, double *z);
     int FRAMES_API wffGetDimensions(int dsIdx, double *width, double *length, double *dist, double *recharge);
     int FRAMES_API wffGetNumFluxTypes(int dsIdx);
     int FRAMES_API wffGetWaterFluxSeries(int dsIdx, int count, double *times, double *values);
     int FRAMES_API wffGetNumContam(int dsIdx);
     int FRAMES_API wffGetNumProgeny(int dsIdx, int conIdx);
     int FRAMES_API wffGetSeriesValues(int dsIdx, int conIdx, int progIdx, int fluxIdx, int count, double *times, double *values);
     int FRAMES_API wffGetSeriesValuesSum(int dsIdx, int conIdx, int progIdx, int count, double *times, double *values);
     int FRAMES_API wffGetContamName(int dsIdx, int conIdx, int progIdx, char *name, char *cas);
     int FRAMES_API wffGetSeriesProperties(int dsIdx, int conIdx, int progIdx, char *vUnits, char *tUnits);
    void FRAMES_API wffAggregate(char *filename, char *casList);
    void FRAMES_API wffInsert(char *filename, char *casList, char *nameList, char *degList, char *secList, int branching);

    //WCFClass.cpp
    void FRAMES_API wcfOpen(char *fuiname, char *modId);
    void FRAMES_API wcfClose();
     int FRAMES_API wcfGetNumRunInfo();
     int FRAMES_API wcfGetRunInfo(int Idx, char *info);
     int FRAMES_API wcfGetNumSets();
     int FRAMES_API wcfGetSetInfo(int dsIdx, char *name, char *type);
     int FRAMES_API wcfGetLocation(int dsIdx, double *x, double *y, double *z);
     int FRAMES_API wcfGetNumContam(int dsIdx);
     int FRAMES_API wcfGetNumProgeny(int dsIdx, int conIdx);
     int FRAMES_API wcfGetContamName(int dsIdx, int conIdx, int progIdx, char *name, char *cas);
     int FRAMES_API wcfGetSeriesValues(int dsIdx, int conIdx, int progIdx, int count, double *times, double *values);
     int FRAMES_API wcfGetSeriesProperties(int dsIdx, int conIdx, int progIdx, char *vUnits, char *tUnits);
    void FRAMES_API wcfAggregate(char *filename, char *casList);
    void FRAMES_API wcfInsert(char *filename, char *casList, char *nameList, char *degList, char *secList, int branching);

    //ATOClass.cpp
    void FRAMES_API atoOpen(char *fuiname, char *modId);
    void FRAMES_API atoClose() ;
     int FRAMES_API atoGetNumRunInfo();
     int FRAMES_API atoGetRunInfo(int Idx, char *info);
     int FRAMES_API atoGetDatasetCount();
     int FRAMES_API atoGetConstituentCount(int dsIdx);
     int FRAMES_API atoGetNumFluxCount(int dsIdx);
    void FRAMES_API atoGetDatasetName(int dsIdx, char *name);
    void FRAMES_API atoGetDatasetModname(int dsIdx, char *name);
    void FRAMES_API atoGetCoordinateType(int dsIdx, char *name);
    void FRAMES_API atoGetReleaseType(int dsIdx, char *name);
    void FRAMES_API atoGetSpatialType(int dsIdx, char *name);
         //---------- ATO flux calls -------------------
    void FRAMES_API atoGetFluxType(int dsIdx, int fluxIdx, char *name);
  double FRAMES_API atoGetFluxRadius(int dsIdx, int fluxIdx);
  double FRAMES_API atoGetFluxDensity(int dsIdx, int fluxIdx);
    void FRAMES_API atoGetFluxRadiusUnits(int dsIdx, int fluxIdx, char *name);
    void FRAMES_API atoGetFluxDensityUnits(int dsIdx, int fluxIdx, char *name);
         //---------- ATO constituent (and Progeny) calls --------------
    void FRAMES_API atoGetCasID(int dsIdx, int conIdx, int progIdx, char *name);
    void FRAMES_API atoGetChemName(int dsIdx, int conIdx, int progIdx, char *name);
    void FRAMES_API atoGetParentCasID(int dsIdx, int conIdx, int progIdx, char *name);
    void FRAMES_API atoGetParentChemName(int dsIdx, int conIdx, int progIdx, char *name);
     int FRAMES_API atoGetProgenyCount(int dsIdx, int conIdx);
         //---------- ATO time period calls ------------------
     int FRAMES_API atoGetTimePeriodCount(int dsIdx, int conIdx, int progIdx);
     int FRAMES_API atoGetTimePeriodOutputCount(int dsIdx, int conIdx, int progIdx, int timeIdx);
    void FRAMES_API atoGetTimePeriodUnit(int dsIdx, int conIdx, int progIdx, int timeIdx, char *name);
  double FRAMES_API atoGetTimePeriodTime(int dsIdx, int conIdx, int progIdx, int timeIdx);
         //---------- ATO grid calls -------------------------
    void FRAMES_API atoGetGridAxis1Unit(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID, char *name);
    void FRAMES_API atoGetGridAxis2Unit(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID, char *name);
    void FRAMES_API atoGetGridDataUnit(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID, char *name);
    void FRAMES_API atoGetGridMoist(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID, char *name);
     int FRAMES_API atoGetGridFluxIndex(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID);
    void FRAMES_API atoGetGridOutputType(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID, char *name);
  double FRAMES_API atoGetGridMax(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID, double& xCoord, double& yCoord);
  double FRAMES_API atoGetGridMin(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID, double& xCoord, double& yCoord);
  double FRAMES_API atoGetGridValue(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID, int axis1, int axis2);
     int FRAMES_API atoGetGridAxis1Count(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID);
     int FRAMES_API atoGetGridAxis2Count(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID);
  double FRAMES_API atoGetGridAxis1Value(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID, int Idx);
  double FRAMES_API atoGetGridAxis2Value(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridID, int Idx);
         //---------- ATO timeseries calls-------------------------
     int FRAMES_API atoGetTimeSeries(int dsIdx, int conIdx, int progIdx, char *product, char *flux, char *moist, double x, double y);
  double FRAMES_API atoGetTimeSeriesTime(int dsIdx, int conIdx, int progIdx, int timeIdx);
  double FRAMES_API atoGetTimeSeriesValue(int dsIdx, int conIdx, int progIdx, int timeIdx);
    void FRAMES_API atoGetTimeSeriesXAxisUnit(int dsIdx, int conIdx, int progIdx, char *unit);
    void FRAMES_API atoGetTimeSeriesYAxisUnit(int dsIdx, int conIdx, int progIdx, char *unit);
    void FRAMES_API atoAggregate(char *filename, char *casList);
    void FRAMES_API atoInsert(char *filename, char *casList, char *nameList, char *degList, char *secList, int branching);

    //BBFClass
    void FRAMES_API bbfOpen(char *fuiname, char *modId);
    void FRAMES_API bbfClose();
     int FRAMES_API bbfGetNumRunInfo();
     int FRAMES_API bbfGetRunInfo(int Idx, char *info);
     int FRAMES_API bbfGetNumSets();
     int FRAMES_API bbfGetSetInfo(int dsIdx, char *name, char *type);
     int FRAMES_API bbfGetNumVULevels(int dsIdx);
     int FRAMES_API bbfGetVULevel(int dsIdx, int vuIdx, char *variability, char *uncertainty);
     int FRAMES_API bbfGetNumOrganism(int dsIdx);
     int FRAMES_API bbfGetOrganismName(int dsIdx, int orgIdx, char *name, char *id);
     int FRAMES_API bbfGetNumContam(int dsIdx, int orgIdx);
     int FRAMES_API bbfGetNumProgeny(int dsIdx, int orgIdx, int conIdx);
     int FRAMES_API bbfGetContamName(int dsIdx, int orgIdx, int conIdx, int progIdx, char *name, char *cas);
     int FRAMES_API bbfGetSeriesProperties(int dsIdx, int orgIdx, int conIdx, int progIdx, char *concunits, char *timeunits);
     int FRAMES_API bbfGetSeriesValues(int dsIdx, int orgIdx, int conIdx, int progIdx, int vuIdx, int count, double *times, double *values);

//EPFRead.cpp
    bool FRAMES_API epfChecksum();
     int FRAMES_API epfOpen(char *fuiname, char *modId);
    void FRAMES_API epfClose();
     int FRAMES_API epfGetNumRunInfo();
     int FRAMES_API epfGetRunInfo(int Idx, char *info);
     int FRAMES_API epfGetSetInfo(int dsIdx, int *numLoc, char *typ, char *med, char *exp);
     int FRAMES_API epfGetExposurePoint(int dsIdx, int locIdx, double *xp, double *yp);
     int FRAMES_API epfGetTimeCount(int dsIdx, char *parentCAS);
  double FRAMES_API epfGetTime(int dsIdx, char *parentCAS, int timeIdx);
     int FRAMES_API epfLoadRoutesAndPathwaysByTime(int dsIdx, char *cas, char *parentCAS, int timeIdx);
    void FRAMES_API epfGetRouteAndPathway(int dsIdx, int expIdx, char *path, char *route, char *unit);
     int FRAMES_API epfLoadTimeSeries(int dsIdx, int locIdx, char *cas, char *parentCAS, char *path, char *route);
    void FRAMES_API epfGetTimeSeries(int dsIdx, double *stime, double *svalue);
    void FRAMES_API epfGetTimeAndValue(int dsIdx, int idx, double *stime, double *svalue);
  double FRAMES_API epfGetDuration(int dsIdx);
    void FRAMES_API epfAggregate(char *filename, char *casList);

//RIFRead.cpp
    bool FRAMES_API rifChecksum();
     int FRAMES_API rifOpen(char *fuiname, char *modId);
    void FRAMES_API rifClose();
     int FRAMES_API rifGetNumRunInfo();
     int FRAMES_API rifGetRunInfo(int Idx, char *info);
     int FRAMES_API rifGetSetInfo(int dsIdx, int *numLoc, int *numAge, char *typ, char *med, char *exp);
     int FRAMES_API rifGetExposurePoint(int dsIdx, int locIdx, double *xp, double *yp);
     int FRAMES_API rifGetTimeCount(int dsIdx, int ageIdx, char *parentCAS);
  double FRAMES_API rifGetTime(int dsIdx, int ageIdx, char *parentCAS, int timeIdx);
     int FRAMES_API rifGetAgeGroup(int dsIdx, int ageIdx, double *min, double *max);
     int FRAMES_API rifLoadRoutesAndPathwaysByTime(int dsIdx, int ageIdx, char *cas, char *parentCAS, int timeIdx);
    void FRAMES_API rifGetRouteAndPathway(int dsIdx, int expIdx, char *path, char *route, char *measure, char *unit);
     int FRAMES_API rifLoadTimeSeries(int dsIdx, int locIdx, int ageIdx, char *cas, char *parentCAS, char *path, char *route, char *measure, char *unit);
    void FRAMES_API rifGetTimeSeries(int dsIdx, double *stime, double *svalue);
    void FRAMES_API rifGetTimeAndValue(int dsIdx, int timeIdx, double *stime, double *svalue);
  double FRAMES_API rifGetDuration(int dsIdx);
  double FRAMES_API rifGetPopulation(int dsIdx);
    void FRAMES_API rifAggregate(char *filename, char *casList);

//HIFClass.cpp
    bool FRAMES_API hifChecksum();
     int FRAMES_API hifOpen(char *fuiname, char *modId);
    void FRAMES_API hifClose();
     int FRAMES_API hifGetNumRunInfo();
     int FRAMES_API hifGetRunInfo(int Idx, char *info);
     int FRAMES_API hifGetSetInfo(int dsIdx, int *numLoc, int *numAge, int *numCancerOrg, int *numDoseOrg, char *typ, char *med, char *exp);
     int FRAMES_API hifGetExposurePoint(int dsIdx, int locIdx, double *xp, double *yp);
     int FRAMES_API hifGetTimeCount(int dsIdx, int ageIdx, char *parentCAS);
  double FRAMES_API hifGetTime(int dsIdx, int ageIdx, char *parentCAS, int timeIdx);
     int FRAMES_API hifGetAgeGroup(int dsIdx, int ageIdx, double *min, double *max);
    void FRAMES_API hifGetCancerOrgan(int dsIdx, int orgIdx, char *cstr);
    void FRAMES_API hifGetDoseOrgan(int dsIdx, int orgIdx, char *cstr);
     int FRAMES_API hifLoadRoutesAndPathwaysByTime(int dsIdx, int ageIdx, char *cas, char *parentCAS, int timeIdx);
    void FRAMES_API hifGetRouteAndPathway(int dsIdx, int expIdx, char *path, char *route, char *measure, char *unit);
     int FRAMES_API hifLoadTimeSeries(int dsIdx, int locIdx, int ageIdx, int orgIdx, char *cas, char *parentCAS, char *path, char *route, char *measure, char *unit);
    void FRAMES_API hifGetTimeSeries(int dsIdx, double *stime, double *svalue);
    void FRAMES_API hifGetTimeAndValue(int dsIdx, int timeIdx, double *stime, double *svalue);
  double FRAMES_API hifGetDuration(int dsIdx);
  double FRAMES_API hifGetPopulation(int dsIdx);
    void FRAMES_API hifAggregate(char *filename, char *casList);

// EXFClass.cpp
    void FRAMES_API exfOpen(char *fuiname, char *modId);
    void FRAMES_API exfClose();
     int FRAMES_API exfGetNumRunInfo();
     int FRAMES_API exfGetRunInfo(int Idx, char *info);
     int FRAMES_API exfGetNumSets();
     int FRAMES_API exfGetSetInfo(int dsIdx, char *name, char *type);
     int FRAMES_API exfGetNumLocation(int dsIdx);
     int FRAMES_API exfGetLocationName(int dsIdx, int locIdx, char *name, char *id);
     int FRAMES_API exfGetNumOrganism(int dsIdx);
     int FRAMES_API exfGetOrganismName(int dsIdx, int orgIdx, char *name, char *id);
     int FRAMES_API exfGetNumContam(int dsIdx, int orgIdx);
     int FRAMES_API exfGetContamName(int dsIdx, int orgIdx, int conIdx, char *name, char *cas);
     int FRAMES_API exfGetNumEffects(int dsIdx, int orgIdx, int conIdx);
     int FRAMES_API exfGetSeriesProperties(int dsIdx, int orgIdx, int conIdx, int efxIdx, char *efxDes, char *efxunits, char *timeunits);
     int FRAMES_API exfGetSeriesValues(int dsIdx, int orgIdx, int conIdx, int efxIdx, int count, double *times, double *values);

#ifdef __cplusplus
}
#endif

#endif
