#include "SCFv2.h"
#include "WFFv2.h"
#include "WCFv2.h"
#include "AFFv2.h"
#include "ATOv2.h"
#include "EPFv2.h"
#include "RIFv2.h"
#include "HIFv2.h"

//unit constants for mappings
char *UNIT;
char *NA = "n/a";
char *HOUR = "hr";
char *YEAR = "yr";
char *M = "m";
char *KM = "km";
char *UM = "um";
char *G = "g";
char *PCI = "pCi";
char *MYR = "m/yr";
char *GYR = "g/yr";
char *GHR = "g/hr";
char *GML = "g/mL";
char *MGKG = "mg/kg";
char *MGL = "mg/L";
char *GCM3 = "g/cm^3";
char *PCIKG = "pCi/kg";
char *PCIL = "pCi/L";
char *PCIML = "pCi/mL";
char *PCIYR = "pCi/yr";
char *PCIHR = "pCi/hr";
char *FRACTION = "fraction";

// measure
char *cmWCF ="Mass/Volume";
char *rmWCF ="Activity/Volume";
char *cmSCF ="Mass/Mass";
char *rmSCF ="Activity/Mass";
char *cmFF ="Mass/Time";
char *rmFF ="Activity/Time";

//Dictionary Name constants for mappings
char *_GID = "gid";
char *_MSG = "msg";
char *_WRN = "wrn";
char *_ERR = "err";
char *_AFF = "aff";
char *_WFF = "wff";
char *_WCF = "wcf";
char *_SCF = "scf";
char *_ATO = "ato";
char *_EPF = "epf";
char *_RIF = "rif";
char *_HIF = "hif";
char *_EPA = "epa";
char *_BBF = "bbf";
char *_TWI = "twi";
char *_HQF = "hqf";
char *_EXF = "exf";
char *_TXT = "txt";
char *_FLX = "flx";
char *_AC2 = "ac2";
char *_RDF = "rdf";
char *_DOT = ".";

char *FILETYPE = "FileType";
char *DICLIST = "DicList";
char *FEATURE = "Feature";
char *FEATUREPTS = "FeaturePts";
char *ATTRIBUTES = "Attributes";
char *SETATTRIBUTES = "SetAttributes";
char *ATTRIBUTEDES = "AttributeDes";
char *SETATTRIBUTEDES = "SetAttributeDes";
char *MEDIA = "Media";
char *MEDIASET = "MediaSet";
char *POPFEATURE = "PopBlockFeature";

//GIS Feature type constants
char *PTS = "Points";
char *POLYS = "Polygons";
char *VOLUMES = "Volumes";

//point styles
char *GRID = "Grid";

//defined in atoClass
//char *POLAR = "Polar";
//char *CARTESIAN = "Cartesian";

//specific GIS feature types
char *MEDIUM;
char *AIR = "Air";
char *SOIL = "Soil";
char *VADOSE = "Vadose";
char *AQUIFER = "Aquifer";
char *SURFACEWATER = "SurfaceWater";
char *SURFACE_WATER = "Surface Water";
char *SEDIMENT = "Sediment";
char *ACUTEEXP = "AcuteExposure";
char *CHRONICEXP = "ChronicExposure";
char *POPULATION = "Population";

//Time
char *TIME;
char *ACUTE = "Acute";
char *CHRONIC = "Chronic";
char *DURATION = "Duration";
char *DUR = "Dur";

//Constituent
char *LIST = "List";
char *CASID = "CASID";
char *NAME = "Name";
char *CHEM = "Chem";
char *RAD = "Rad";

//Measurements & Modifiers
char *MODIFIER;
char *TIMEPTS = "TimePts";
char *FLUX = "Flux";
char *CONC = "Conc";
char *DEP = "Dep";
char *DOSE = "Dose";
char *INTAKE = "Intake";
char *WATERFLUX = "WaterFlux";
char *TOTALFLUX = "TotalFlux";
char *ADSORBEDFLUX = "AdsorbedFlux";
char *DISSOLVEDFLUX = "DissolvedFlux";
char *TOTALCONC = "TotalConc";
char *ADSORBEDCONC = "AdsorbedConc";
char *DISSOLVEDCONC = "DissolvedConc";
char *EXTERNALDOSE = "ExternalDose";

char *MIDSIZE = "Size";
char *DENSITY = "Density";
char *REACTIVE = "Reactive";

char *ALL = "All";     // Reactive fractions
char *GAS = "Gas";     // Reactive fractions
char *SOLID = "Solid";   // particle range what are given are mid points
char *LIQUID = "Liquid";  // vapor

char *WET = "Wet";     // Deposition types
char *DRY = "Dry";
char *TOTAL = "Total";

char *GASEMISSION = "GasEmission";     // Reactive fractions
char *SOLEMISSION = "SolidEmission";   // particle range what are given are mid points
char *LIQEMISSION = "LiquidEmission";  // vapor

char *PATHWAY = "Pathway";
char *AGEGROUP = "AgeGroups";
char *STARTAGE = "StartAge";
char *ENDAGE = "EndAge";

char Media[SMALLSTRING];
char MediaSet[SMALLSTRING];
char setAgeGroup[SMALLSTRING];
char setPopulation[SMALLSTRING];
char varPopFeature[SMALLSTRING];
int  population = 0;
int  popFeatureIdx = 0;
double duration = 0.0;

Glyph::Glyph(char *simname, char *modid)
{
  int dim;
  int scope;
  int ErrorCode;
  int idx[MMF_MAX_DIM] = {0};
  char cmdline[MAXSTRING];
  char filename[MAXSTRING];
  CStringParser args;

  ID1x = NULL;
  ID2x = NULL;
  Label = NULL;
  Model = NULL;
  des = NULL;
  vwrobject = NULL;
  PDCFglyph = NULL;
  x = 0.0;
  y = 0.0;
  z = 0.0;
  sx = 0.0;
  sy = 0.0;
  State = 0;
  siteidx = 1;
  glyphidx = 1;
  rstrcpy(ftype, blank);
  rstrcpy(fqual, blank);
  rstrcpy(uiset, blank);
  rstrcpy(uidic, blank);

  ID1x = strdup(modid); TrackLocation(ID1x);
  ID2x = strdup(modid); TrackLocation(ID2x);

  VarLookUp(pid, simname, "ModID",  ID2x, idx);
  ErrorCode = GetString(pid, simname, "ModLabel", blank, idx, dummy);
  Label = strdup(dummy); TrackLocation(Label);

  ErrorCode = GetInteger(pid, simname, "ModScope", blank, idx, &scope);
  ErrorCode = GetInteger(pid, simname, "ModState", blank, idx, &State);

  ErrorCode = GetInteger(pid, simname, "ModPosX", blank, idx, &sx);
  ErrorCode = GetInteger(pid, simname, "ModPosY", blank, idx, &sy);
  if(scope == 1)
  {  sx = sx + 75;  sy = sy + 75; }

  if(State>0)
  {
    GetString(pid, simname, "ModName", blank, idx, dummy);
    Model = strdup(dummy); TrackLocation(Model);
    if(0<strlen(Model))
    {
      cmdline[0] = 0;
      ErrorCode = GetModDimSize(pid, Model, "UICmdLine", SetIdx(), &dim);
      if(ErrorCode == 0 && dim>0)
        ErrorCode = GetModString(pid, Model, "UICmdLine", blank, SetIdx(), cmdline);
      if(0 == strlen(cmdline))
      {
        ErrorCode = GetModDimSize(pid, Model, "ModelCmdLine", SetIdx(), &dim);
        if(ErrorCode == 0 && dim>0)
          ErrorCode = GetModString(pid, Model, "ModelCmdLine", blank, SetIdx(), cmdline);
      }
      if(0<strlen(cmdline))
      {
        args.Parse(cmdline, poCmdLine);
        if(args.GetCount()>= 3)
        {
          sprintf(filename, "%s\\%s.des", args.GetAt(args.GetCount()-2), args.GetAt(args.GetCount()-1));
          if(0 == access(filename, 0))
          {
            des = ReadDes(filename);
            TrackDelete(ID1x);
            ID1x = strdup(rstrcpy(dummy,Prefix.c_str(),&modid[3]));
            TrackLocation(ID1x);
          }
        }
      }
    }
  }
  
  ErrorCode = GetIconUISet(pid, ID2x, uiset);
  // Expected -7005 List index error if no UI dataset
  // underlying call uses an index of 1 for the first dataset
  if (ErrorCode == -7005)  ClearErrors(pid); 
  if(strlen(uiset))
  {
    long dichandle = DataSetGetHandle(pid, uiset);
    ErrorCode = DataSetGetDictionaryName(pid, dichandle, uidic);
    if(des) ReadUIDataset();
  }
  ErrorCode = GetInputModuleList();
  ErrorCode = GetOutputModuleList();
}

Glyph::~Glyph()
{
  if(ID2x!= NULL && ID2x!= blank)   TrackDelete(ID2x);
  if(ID1x!= NULL && ID1x!= blank)   TrackDelete(ID1x);
  if(Label!= NULL && Label!= blank) TrackDelete(Label);
  if(Model!= NULL && Model!= blank) TrackDelete(Model);
  gid.ParamsClear();
  imodlist.clear();
  omodlist.clear();
  isetlist.clear();
  osetlist.clear();
  sinks.clear();
  sources.clear();
  vwrobjects.clear();
  Reads.clear();
  Writes.clear();
}

int Glyph::ReadDes(const char *path)
{
  int i, k;
  char cbuf[MAXSTRING];
  char ftype[SMALLSTRING];
  char fqual[SMALLSTRING];
  CStringParser args;

  icsv *ifle = new icsv((char *)path);  TrackLocation(ifle);
  if(ifle->ok())
  {
    DesPath = string(path);

    *ifle >> NewLn;
    *ifle >> cbuf;

    args.Parse(cbuf, CParseOptions(':'));

    Class = string(args.GetAt(0));
    Group = string(args.GetAt(1));
    Prefix = string(args.GetAt(2));

    *ifle >> cbuf; // name
    *ifle >> cbuf;
    UIExe = string(cbuf);
    *ifle >> cbuf;
    ModelExe = string(cbuf);
    *ifle >> NewLn;

    int q = fgetc(ifle->fptr);
    int c = fgetc(ifle->fptr);
    while(c!= q && c!= EOF) c = fgetc(ifle->fptr);
    if((!ifle->eof() && c == q && q == 34))
    {
      if(!ifle->eof() && c == q && q == 34)
      {
        *ifle >> NewLn ;
        *ifle >> numreads >> NewLn;
        for(i = 0; i<numreads; i++)
          *ifle >> NewLn;
        *ifle >> numwrites >> NewLn;
        for(k = 0; k<numwrites; k++)
        {
          *ifle >> ftype >> fqual >> NewLn;
          sprintf(cbuf, "%s.%s", ftype, fqual);
          Writes.push_back(string(cbuf));
        }
      }
    }
    TrackDelete(ifle);
    return 1;
  }
  TrackDelete(ifle);
  return 0;
}

bool Glyph::Source(char *modid)
{
  STR_VEC_MAP_MAP_ITR md = imodlist.find(string(modid));
  if(md != imodlist.end()) return true;
  return false;
}

bool Glyph::Sink(char *modid)
{
  STR_VEC_MAP_MAP_ITR md = omodlist.find(string(modid));
  if(md != omodlist.end()) return true;
  return false;
}

bool Glyph::FindWriteType(char *type, char *qual)
{
  char cbuf[SMALLSTRING];
  char cbuf2[SMALLSTRING];
  sprintf(cbuf, "%s.%s", type, qual);
  for(STR_VEC_ITR it = Writes.begin(); it!= Writes.end(); ++it)
  {
    string w = (*it);
    rstrcpy(cbuf2, w.c_str());
    if(0 == rstrcmpi(cbuf, cbuf2)) return true;
  }
  return false;
}

bool Glyph::HaveFileType(char *ftype, char *fqual, STR_VEC_MAP setlist)
{
  bool NoRequiredDic;
  char name[SMALLSTRING];
  STR_VEC_ITR iv;
  STR_VEC_MAP_ITR is;

  rstrcpy(name,ftype,_DOT,fqual);
  FILETYPE_PTR_MAP_ITR it = mapFileType.find(_strupr(name));
  if(it != mapFileType.end())
  {
    FileType *ft = (*it).second;
    if(0<ft->dicreq.size() || 0<ft->dicopt.size())
    {
      STR_VEC vc = ft->dicreq;
      NoRequiredDic = !vc.size();
      for(iv = vc.begin(); iv!= vc.end(); ++iv)
      {
          is = setlist.find(iv->c_str());
          if(is == setlist.end())
          { return false; }
      }
      // if no required dics we must have at least one of the optionals
      if(NoRequiredDic)
      {
        vc = ft->dicopt;
        for(iv = vc.begin(); iv!= vc.end(); ++iv)
        {
            is = setlist.find(iv->c_str());
            if(is != setlist.end())
              return true;
        }
        return false;
      }
      return true;
    }
  }
  return false;
}

int Glyph::GetInputModuleList()
{
  char modid[SMALLSTRING];
  char setid[SMALLSTRING];
  char dicid[SMALLSTRING];
  int i, j, nummod, numset, ErrorCode;
  STR_VEC_MAP_ITR it;

  imodlist.clear();
  isetlist.clear(); // isetlist is map of all input datasets

  ErrorCode = NumIMod(pid, ID2x, &nummod);
  for(i = 0; i<nummod; i++)
  {
    ErrorCode = GetIModId(pid, ID2x, i+1, modid);
    ErrorCode = NumIModSet(pid, ID2x, modid, &numset);
    STR_VEC_MAP setlist;
    STR_VEC vecset;
    for(j = 0; j<numset; j++)
    {
      ErrorCode = GetIDataSet(pid, ID2x, modid, j+1, setid);
      ErrorCode = GetDicId(pid, setid, dicid);
      it = setlist.find(dicid);
      if(it == setlist.end())
      {
        STR_VEC v = vecset;
        v.push_back(setid);
        setlist[dicid] = v;
      }
      else
        (*it).second.push_back(setid);
    }
    STR_VEC_MAP imodsets = setlist; // make a copy
    imodlist[modid] = imodsets;

    // add these datasets to complete set list
    for(it = imodsets.begin(); it!= imodsets.end(); ++it)
    {
      STR_VEC_MAP_ITR is = isetlist.find((*it).first);
      if(is == isetlist.end())
        isetlist[(*it).first] = (*it).second;
      else
        for(STR_VEC_ITR iv = (*it).second.begin(); iv!= (*it).second.end(); ++iv)
          (*is).second.push_back((*iv));
    }
    setlist.clear();
  }
  return 0;
}

int Glyph::GetOutputModuleList()
{
  char cbuf1[SMALLSTRING];
  char cbuf2[SMALLSTRING];
  char cbuf3[SMALLSTRING];
  int i, j, nummod, numset, ErrorCode;
  STR_VEC_MAP_ITR it;

  omodlist.clear();
  osetlist.clear();

  ErrorCode = NumOMod(pid, ID2x, &nummod);
  for(i = 0; i<nummod; i++)
  {
    ErrorCode = GetOModId(pid, ID2x, i+1, cbuf1);
    // sink as a module is OK, the setlist is the complete non-duplicate output sets
    // which is what we want here, thus a module with no downstream connections will still
    // produce(for viewers, etc.)
    ErrorCode = NumOModSet(pid, ID2x, cbuf1, &numset);
    STR_VEC_MAP setlist;
    STR_VEC vecset;
    for(j = 0; j<numset; j++)
    {
      ErrorCode = GetODataSet(pid, ID2x, cbuf1, j+1, cbuf2);
      ErrorCode = GetDicId(pid, cbuf2, cbuf3);
      it = setlist.find(cbuf3);
      if(it == setlist.end())
      {
        STR_VEC v = vecset;
        v.push_back(cbuf2);
        setlist[cbuf3] = v;
      }
      else
        (*it).second.push_back(cbuf2);
    }
    STR_VEC_MAP omodsets = setlist;
    omodlist[cbuf1] = omodsets;

    // add these datasets to complete set list
    for(it = setlist.begin(); it!= setlist.end(); ++it)
    {
      STR_VEC_MAP_ITR is = osetlist.find((*it).first);
      if(is == osetlist.end())
        osetlist[(*it).first] = (*it).second;
      else
        for(STR_VEC_ITR iv = (*it).second.begin(); iv!= (*it).second.end(); ++iv)
          (*is).second.push_back((*iv));
    }
    setlist.clear();
  }
  return 0;
}

int Glyph::GetSourcesAndSinks(GLYPH_PTR_MAP glyphlist)
{
  int ErrorCode = 0;
  Glyph *src = NULL;
  Glyph *snk = NULL;
  STR_VEC_MAP setlist;
  STR_VEC_MAP_ITR is;
  int idx[MMF_MAX_DIM];

  for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  if(GetSetName("GeoReference", setname, blank, isetlist))
  {
    ErrorCode = VarLookUp(pid, setname, "PointID", ID2x, idx);
    if (idx[0] > 0) 
    {
      ErrorCode = GetFloat(pid, setname, "Coordinates", M, SetIdx(idx[0], 1), &x);
      ErrorCode = GetFloat(pid, setname, "Coordinates", M, SetIdx(idx[0], 2), &y);
      ErrorCode = GetFloat(pid, setname, "Coordinates", M, SetIdx(idx[0], 3), &z);
    }
  }

  for(STR_VEC_MAP_MAP_ITR it = imodlist.begin(); it!= imodlist.end(); ++it)
  {
    // if chemical properties are supplied by multiple modules then
    // this logic will not work.  Need to look at the dictionaries
    // being supplied by the incoming modules to get the right profile
    src = NULL;
    string key = (*it).first;
    GLYPH_PTR_MAP_ITR ig = glyphlist.find(key);
    if(ig!= glyphlist.end())
      src = (*ig).second;
    if(src != NULL)
      sources[src->ID2x] = src;
  }

  for(it = omodlist.begin(); it != omodlist.end(); ++it)
  {
    snk = NULL;
    string key = (*it).first;
    // sink object in 2x for datasets that don't get consumed
    if(rstrcmpi((char *)key.c_str(), "sink"))
    {
      GLYPH_PTR_MAP_ITR ig = glyphlist.find(key);
      if(ig!= glyphlist.end())
        snk = (*ig).second;
      if(snk != NULL)
      {
        if(snk->des)
          if(!rstrcmpi((char *)snk->Prefix.c_str(), "vwr"))
            snk->sources[ID2x] = this;
        sinks[snk->ID2x] = snk;
      }
    }
  }
  return 0;
}

int Glyph::GetFileTypeAndQualifier(STR_VEC_MAP setlist, char *ctyp, char *cqual)
{
  bool required;
  bool optional;
  char ttyp[SMALLSTRING];
  STR_VEC_ITR iv;
  STR_VEC_MAP_ITR is;

  rstrcpy(ctyp, blank);
  rstrcpy(cqual, blank);

  // look for exact match
  for(FILETYPE_PTR_MAP_ITR it = mapFileType.begin(); it!= mapFileType.end(); ++it)
  {
    FileType *ft = (*it).second;
    if(0<ft->dicreq.size() || 0<ft->dicopt.size())
    {
      STR_VEC vc = ft->dicreq;
      required = (vc.size()>0);
      for(iv = vc.begin(); iv!= vc.end(); ++iv)
      {
        is = setlist.find(iv->c_str());
        if(is == setlist.end())
        { 
          required = false; 
          break; 
        }
      }
      if(required)
      {
        optional = true;
        vc = ft->dicopt;
        for(iv = vc.begin(); iv!= vc.end(); ++iv)
        {
          is = setlist.find(iv->c_str());
          if(is == setlist.end())
          { 
            optional = false; 
            break; 
          }
        }
        if(optional)
        {
          rstrcpy(cqual, ft->fqual.c_str());
          rstrcpy(ctyp, ft->ftype.c_str());
          if(0 == rstrcmpi(cqual, "<none>")) rstrcpy(cqual, blank);
          return 0;
        }
      }
    }
  }

  // try a different approach
  int nf = 0;
  nf = -1;
  for(it = mapFileType.begin(); it!= mapFileType.end(); ++it)
  {
    nf++;
    FileType *t = NULL;
    FileType *ft = (*it).second;
    if(0<ft->dicreq.size() || 0<ft->dicopt.size())
    {
      if(0 == rstrcmpi((char *)ft->ftype.c_str(), "con"))
        int z = 0;
      STR_VEC vc = ft->dicreq;
      bool required = true;
      for(iv = vc.begin(); iv!= vc.end(); ++iv)
      {
        is = setlist.find(iv->c_str());
        if(is == setlist.end())
          required = false;
      }
      if(required)
      {
        rstrcpy(ttyp, ft->ftype.c_str());
        vc = ft->dicopt;
        for(iv = vc.begin(); iv!= vc.end(); ++iv)
        {
          is = setlist.find(iv->c_str());
          if(is!= setlist.end())
          {
            rstrcpy(ctyp, ttyp);
            rstrcpy(cqual, ft->fqual.c_str());
            if(0 == rstrcmpi(cqual, "<none>")) rstrcpy(cqual, blank);
            return 0;
          }
        }
        if(0 == rstrcmpi(ttyp, "con") || 0 == rstrcmpi(ttyp, "GIS"))
        {
          rstrcpy(ctyp, ttyp);
          return 0;
        }
      }
    }
  }
  return 1;
}

int Glyph::WriteFUI(oGid *gid, char *prefix, int siteIdx, int glyphIdx)
{
  int srcNum = 0;
  int rcpNum = 0;
  char ft[SMALLSTRING];
  char fq[SMALLSTRING];

  WriteProperties(gid, prefix, siteIdx, glyphIdx);
  for(GLYPH_PTR_MAP_ITR it = sources.begin(); it!= sources.end(); ++it)
  {
    Glyph *src = (*it).second;
    STR_VEC_MAP_MAP_ITR im = imodlist.find(src->ID2x);
    if(im!= imodlist.end())
    {
      STR_VEC_MAP setlist = (*im).second;
      if(src->des)
      {
        if(rstrcmpi((char *)src->Class.c_str(),"Database"))
        {
          srcNum++;
          gid->PutValue(concat(prefix, "SrcName"), SetIdx(siteIdx, glyphIdx, srcNum), 0, NA, NA, src->ID1x);
          gid->PutValue(concat(prefix, "Type"), SetIdx(siteIdx, glyphIdx, srcNum), 0, NA, NA, src->Group);
        }
      }
      else
      {
        STR_VEC_MAP setlist = (*im).second;
        GetFileTypeAndQualifier(setlist, ft, fq);
        if (!rstrcmpi(ft,"GIS")) continue;
        if (src->Group.length()>0) src->Group = "User Defined";
        srcNum++;
        gid->PutValue(concat(prefix, "SrcName"), SetIdx(siteIdx, glyphIdx, srcNum), 0, NA, NA, src->ID1x);
        gid->PutValue(concat(prefix, "Type"), SetIdx(siteIdx, glyphIdx, srcNum), 0, NA, NA, src->Group);
      }
    }
  }
  gid->PutValue(concat(prefix, "SrcNum"), SetIdx(siteIdx, glyphIdx), 0, NA, NA, srcNum);
  gid->PutValue(concat(prefix, "TypeNum"), SetIdx(siteIdx, glyphIdx), 0, NA, NA, srcNum);
  return 0;
}

int Glyph::WriteCSM(oGid *gid, char *prefix, int siteIdx, int glyphIdx, char *modid)
{
  int numsrc = 0;
  int numsnk = 0;
  char ft[SMALLSTRING];
  char fq[SMALLSTRING];
  GLYPH_PTR_MAP_ITR it;

  WriteProperties(gid, prefix, siteIdx, glyphIdx);

  // Write source info
  // examine what this glyph is consuming(scheme) from the source module
  // map that dictionary list to the filetype and qualifer in importdes.ini
  for(it = sources.begin(); it != sources.end(); ++it)
  {
    Glyph *g = (*it).second;
    if(g->des)
    {
      if(!rstrcmpi((char *)g->Class.c_str(), "Viewer"))
        continue;
    }
    else
    {
      // this excludes the GIS file type because it is unknown in Frames 1.X
      STR_VEC_MAP_MAP_ITR im = imodlist.find(g->ID2x);
      if(im != imodlist.end())
      {
        STR_VEC_MAP setlist = (*im).second;
        GetFileTypeAndQualifier(setlist, ft, fq);
        if (!rstrcmpi(ft,"GIS")) continue;
      }
      else continue;
    }
    numsrc++;
    gid->PutValue(concat(prefix, "SrcId"), SetIdx(siteIdx, glyphIdx, numsrc), 0, NA, NA, g->ID1x);
    gid->PutValue(concat(prefix, "SrcLabel"), SetIdx(siteIdx, glyphIdx, numsrc), 0, NA, NA, g->Label);
    STR_VEC_MAP_MAP_ITR im = imodlist.find(g->ID2x);
    if(im != imodlist.end())
    {
      STR_VEC_MAP setlist = (*im).second;
      GetFileTypeAndQualifier(setlist, ft, fq);
      gid->PutValue(concat(prefix, "SrcType"), SetIdx(siteIdx, glyphIdx, numsrc), 0, NA, NA, ft);
      gid->PutValue(concat(prefix, "SrcQual"), SetIdx(siteIdx, glyphIdx, numsrc), 0, NA, NA, fq);
    }
  }

  // Write sink info
  for(it = sinks.begin(); it != sinks.end(); ++it)
  {
    Glyph *g = (*it).second;
    if(g->des)
      if(!rstrcmpi((char *)g->Class.c_str(),"Viewer"))
        continue;

    numsnk++;
    gid->PutValue(concat(prefix, "SinkId"), SetIdx(siteIdx, glyphIdx, numsnk), 0, NA, NA, g->ID1x);
    gid->PutValue(concat(prefix, "SinkLabel"), SetIdx(siteIdx, glyphIdx, numsnk), 0, NA, NA, g->Label);
    STR_VEC_MAP_MAP_ITR im = omodlist.find(g->ID2x);
    if(im!= omodlist.end())
    {
      STR_VEC_MAP setlist = (*im).second;
      GetFileTypeAndQualifier(setlist, ft, fq);
      gid->PutValue(concat(prefix, "SinkType"), SetIdx(siteIdx, glyphIdx, numsnk), 0, NA, NA, ft);
      gid->PutValue(concat(prefix, "SinkQual"), SetIdx(siteIdx, glyphIdx, numsnk), 0, NA, NA, fq);
    }
  }
  gid->PutValue(concat(prefix, "SrcNum"), SetIdx(siteIdx, glyphIdx), 0, NA, NA, numsrc);
  gid->PutValue(concat(prefix, "SinkNum"), SetIdx(siteIdx, glyphIdx), 0, NA, NA, numsnk);
  return 0;
}

int Glyph::WriteProperties(oGid *gid, char *prefix, int siteIdx, int glyphIdx)
{

  this->siteidx = siteIdx;
  if(0!= strcmpi(prefix, "Mod"))
    this->glyphidx = glyphIdx;

  int idx[MMF_MAX_DIM] = {siteIdx, glyphIdx, 0, 0, 0, 0} ; // idx = SetIdx(siteIdx, glyphIdx);

  if(0 == strcmpi(prefix, "Mod"))
    gid->PutValue(concat(prefix, "Id"), idx, 0, NA, NA, ID1x); // only required for CSM(same as Name)
  else
    gid->PutValue(concat(prefix, NAME), idx, 0, NA, NA, ID1x); // only required for FUI(same as Id)
  gid->PutValue(concat(prefix, "Label"), idx, 0, NA, NA, Label);
  if(Model)
    gid->PutValue(concat(prefix, "Model"), idx, 0, NA, NA, Model);
  if(des)
    gid->PutValue(concat(prefix, "DesPath"), idx, 0, NA, NA, DesPath);
  if(0 == strcmpi(prefix, "Mod"))
  {
    gid->PutValue(concat(prefix, "LocX"), idx, 0, KM, KM, x);
    gid->PutValue(concat(prefix, "LocY"), idx, 0, KM, KM, y);
    gid->PutValue(concat(prefix, "LocZ"), idx, 0, KM, KM, z);
  }
  else
  {
    gid->PutValue(concat(prefix, "X"), idx, 0, KM, KM, x);
    gid->PutValue(concat(prefix, "Y"), idx, 0, KM, KM, y);
    gid->PutValue(concat(prefix, "Z"), idx, 0, KM, KM, z);
  }
  gid->PutValue(concat(prefix, "ScrX"), idx, 0, NA, NA, sx);
  gid->PutValue(concat(prefix, "ScrY"), idx, 0, NA, NA, sy);
  if(0 == strcmpi(prefix, "Mod"))
    gid->PutValue(concat(prefix, "State"), idx, 0, NA, NA, State-1);
  else
    gid->PutValue(concat(prefix, "ModelStat"), idx, 0, NA, NA, State-1);
  return 0;
}

int Glyph::ReadUIDataset()
{
  int ct, ErrorCode = 0;
  char varname[SMALLSTRING];
  int dim = 0;
  int iv, vdim;

  sprintf(dummy,"Loading %s",uiset); Log(dummy);
  gid.ParamsClear();
  ErrorCode += GetVarDimSize(pid, uiset, "zVAR", SetIdx(), &ct);
  for(iv = 1; iv<= ct; iv++)
  {
    vdim = 0;
    ErrorCode += GetString(pid, uiset, "zVAR", blank, SetIdx(iv), varname);
    ErrorCode += GetVarDimSize(pid, uiset, varname, SetIdx(), &vdim);
    Param *prop = gid.ParamsFind(varname);
    if(!prop)
      prop = gid.ParamsAdd(uidic, varname);
    if(vdim>0)
      ErrorCode += prop->ReadValues(uiset, iv);
    if (ErrorCode != 0)
      ErrorCode = 0;
  }
  for(PARAM_PTR_VEC_ITR it = gid.params.begin(); it!= gid.params.end(); ++it)
  {
    Param *p = (*it);
    for(VALUE_VEC_ITR iv = p->values.begin(); iv!= p->values.end(); ++iv)
      ParamValue *pv = *iv;
  }
  return 0;
}

int Glyph::WriteGIDSection(oGid *ogid, char *section)
{
  int i;
  int j;
  int ct = 0;
  long pos;
  long end;

  for(PARAM_PTR_VEC_ITR it = gid.params.begin(); it!= gid.params.end(); ++it)
    ct += (*it)->values.size();
  if(ct == 0) return 0;

  // get position if header else skip
  if(0 == rstrcmpi(section, ID1x))
  {
    *ogid->fp << ID1x;
    pos = ogid->fp->getpos();
    ogid->fp->smartQuote();
    *ogid->fp << "          " << NewLn;
    ogid->fp->alwaysQuote();
    ogid->nrec = 0;
  }

  for (i=1; i<=ct; i++)
  {
    j=0;
    for(it = gid.params.begin(); it!= gid.params.end(); ++it)
    {
      Param *p = (*it);
      j+=p->PutValues(ogid,i);
    }
    if (j==0) ct++;
  }

  // fix up if header
  if(0 == rstrcmpi(section, ID1x))
  {
    // get last position
    end = ogid->fp->getpos();
    // return to first position
    ogid->fp->setpos(pos);
    // update the count
    ogid->fp->delim = ' ';
    *ogid->fp << ogid->nrec;
    ogid->fp->delim = ',';
    // return to last position
    ogid->fp->setpos(end);
  }
  return SUCCESS;
}

int Glyph::WriteDatasetsFromPDCF(char *outpath, STR_VEC_PTR_VEC conList)
{
  int i, ct, ErrorCode;
  int idx[MMF_MAX_DIM] = {0, 0, 0};
  char dicname[SMALLSTRING];
  char vname[MAXSTRING];

  Log("WriteDatasetsFromPDCF");
  // == == == == == == == = Clear produced datasets
  for(STR_VEC_MAP_ITR it = osetlist.begin(); it!= osetlist.end(); ++it)
  {
    rstrcpy(dicname,(*it).first.c_str());
    for(int v = 0; v<(*it).second.size(); v++)
    {
      rstrcpy(setname,(*it).second[v].c_str());
      ErrorCode = GetVarCount(pid, dicname, &ct);
      for(i = 1; i<= ct; i++)
      {
        ErrorCode = GetVarName(pid, dicname, i, vname);
        if(ErrorCode == SUCCESS)
          ErrorCode = ClearVariable(pid, setname, vname, idx);
      }
    }
  }

  // == == == == == == = Write produced datasets
  // no external keys
/*
  if(0 == access(AddExtension(outpath, "tos"), 0))
  {
    GID_v2 *tos = new GID_v2(); TrackLocation(tos);
    tos->WriteDataset(pid, outpath, this);
    TrackDelete(tos);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, "aos"), 0))
  {
    GID_v2 *aos = new GID_v2(); TrackLocation(aos);
    aos->WriteDataset(pid, outpath, this);
    TrackDelete(aos);
    CheckErrors();
  }

  //two external keys - conlist and toslist
  if(0 == access(AddExtension(outpath, "cct"), 0))
  {
    GID_v2 *cct = new GID2_v2(); TrackLocation(cct);
    cct->WriteDataset(pid, outpath, this, &conList, &tosList);
    TrackDelete(cct);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, "ebf"), 0))
  {
    GID_v2 *ebf = new GID2_v2(); TrackLocation(ebf);
    ebf->WriteDataset(pid, outpath, this, &conList, &aosList);
    TrackDelete(ebf);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, "bbf"), 0))
  {
    GID2_v *bbf = new GID2_v2(); TrackLocation(bbf);
    bbf->WriteDataset(pid, outpath, this, &conList, &aosList);
    TrackDelete(bbf);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, "twi"), 0))
  {
    GID2_v *twi = new GID2_v2(); TrackLocation(twi);
    twi->WriteDataset(pid, outpath, this, &conList, &tosList);
    TrackDelete(twi);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, "exf"), 0))
  {
    GID2_v *exf = new GID2_v2(); TrackLocation(exf);
    twi->WriteDataset(pid, outpath, this, &conList, &aosList);
    TrackDelete(exf);
    CheckErrors();
  }
*/
  // one external key - conList
  if(0 == access(AddExtension(outpath, _ATO), 0))
  {
    ATO_v2 *ato = new ATO_v2(); TrackLocation(ato);
    ato->WriteDataset(pid, outpath, this, &conList);
    TrackDelete(ato);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _SCF), 0))
  {
    if (doBench) SetWarning(pid, " Benchmark_Start");
    SCF_v2 *scf = new SCF_v2(); TrackLocation(scf);
    scf->WriteDataset(pid, outpath, this, &conList);
    if (doBench) {
      SetWarning(pid, " Benchmark_End");
      SetWarning(pid, "");
    }
    TrackDelete(scf);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _WFF), 0))
  {
    WFF_v2 *wff = new WFF_v2(); TrackLocation(wff);
    wff->WriteDataset(pid, outpath, this, &conList);
    TrackDelete(wff);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _AFF), 0))
  {
    AFF_v2 *aff = new AFF_v2(); TrackLocation(aff);
    aff->WriteDataset(pid, outpath, this, &conList);
    TrackDelete(aff);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _WCF), 0))
  {
    if (doBench) SetWarning(pid, " Benchmark_Start");
    WCF_v2 *wcf = new WCF_v2(); TrackLocation(wcf);
    wcf->WriteDataset(pid, outpath, this, &conList);
    if (doBench) {
      SetWarning(pid, " Benchmark_End");
      SetWarning(pid, "");
    }
    TrackDelete(wcf);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _EPF), 0))
  {
    EPF_v2 *epf = new EPF_v2(); TrackLocation(epf);
    epf->WriteDataset(pid, outpath, this, &conList);
    TrackDelete(epf);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _RIF), 0))
  {
    RIF_v2 *rif = new RIF_v2();  TrackLocation(rif);
    rif->WriteDataset(pid, outpath, this, &conList);
    TrackDelete(rif);
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _HIF), 0))
  {
    HIF_v2 *hif = new HIF_v2(); TrackLocation(hif);
    hif->WriteDataset(pid, outpath, this, &conList);
    TrackDelete(hif);
    CheckErrors();
  }
  // easy conversions
  if(0 == access(AddExtension(outpath, _TXT), 0))
  {
    WriteToFileType(outpath, _TXT, "EMConfigFile", "Text");
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _FLX), 0))
  {
    WriteToFileType(outpath, _FLX, "SoilFile", "Text");
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _AC2), 0))
  {
    WriteToFileType(outpath, _AC2, "CMSDataFile", "Text");
    CheckErrors();
  }
//  if(0 == access(AddExtension(outpath, _SUF), 0))
//  {
//    WriteToFileType(outpath, _SUF, "Sensitivity", "Text");
//    CheckErrors();
//  }
  if(0 == access(AddExtension(outpath, _RDF), 0))
  {
    WriteToFileType(outpath, _RDF, "RecoveryDataFile", "Text");
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _MSG), 0))
  {
    WriteToFileType(outpath, _MSG, "Status", "Messages");
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _WRN), 0))
  {
    WriteToFileType(outpath, _WRN, "Status", "Warnings");
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _ERR), 0))
  {
    WriteToFileType(outpath, _ERR, "Status", "Errors");
    CheckErrors();
  }
  if(0 == access(AddExtension(outpath, _EPA), 0))
  {
    WriteToFileType(outpath, _EPA, "BiotaDoseComplianceFile", "Text");
    WriteToFileType(outpath, _EPA, "AirDoseComplianceFile", "Text");
    WriteToFileType(outpath, _EPA, "SurfaceWaterDoseComplianceFile", "Text");
    CheckErrors();  
  }
  if(0 == access(AddExtension(outpath, _HQF), 0))
  {
    WriteToFileType(outpath, _HQF, "HazardQuotients", "Text");
    CheckErrors();  
  }
  if(0 == access(AddExtension(outpath, _TWI), 0))
  {
    WriteToFileType(outpath, _TWI, "TerrestrialWildlifeIntake", "Text");
    CheckErrors();  
  }
  if(0 == access(AddExtension(outpath, _BBF), 0))
  {
    WriteToFileType(outpath, _BBF, "EcoBodyBurdensSUF", "Text");
    CheckErrors();  
  }
  if(0 == access(AddExtension(outpath, _EXF), 0))
  {
    WriteToFileType(outpath, _EXF, "AquaticOrganismEffects", "Text");
    CheckErrors();  
  }
  // Not supported
  if(0 == access(AddExtension(outpath, "saf"), 0))
  {    CheckErrors();  }
  if(0 == access(AddExtension(outpath, "xls"), 0))
  {    CheckErrors();  }
  return SUCCESS;
}

int Glyph::WritePDCFFromDatasets(char *outPath, STR_VEC_PTR_VEC conList, Glyph *g)
{
  sprintf(dummy, "Writing output files for %s", ID1x);  Log(dummy);
  PDCFglyph = g;

  if (g->State != 3)
    return 0;
  if(HaveFileType(_WFF, VADOSE, isetlist) &&
     HaveFileType(_WFF, VADOSE, g->osetlist))
  {
    WFF_v2 *wff = new WFF_v2();
    wff->WriteFile(pid, outPath, this, &conList, VADOSE, g->ID1x, g->ID2x);
    delete wff;
    vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_WFF, AQUIFER, isetlist) &&
     HaveFileType(_WFF, AQUIFER, g->osetlist))
  {
    WFF_v2 *wff = new WFF_v2();
    wff->WriteFile(pid, outPath, this, &conList, AQUIFER, g->ID1x, g->ID2x);
    delete wff;
    vwrobject = g;
    CheckErrors();
  }

  if(HaveFileType(_WFF, SURFACE_WATER, isetlist) &&
     HaveFileType(_WFF, SURFACE_WATER, g->osetlist))
  {
    WFF_v2 *wff = new WFF_v2();
    wff->WriteFile(pid, outPath, this, &conList, SURFACE_WATER, g->ID1x, g->ID2x);
    delete wff;
    vwrobject = g;
    CheckErrors();
  }

  if(HaveFileType(_AFF, AIR, isetlist) &&
     HaveFileType(_AFF, AIR, g->osetlist))
  {
    AFF_v2 *aff = new AFF_v2();
    aff->WriteFile(pid, outPath, this, &conList, AIR, g->ID1x, g->ID2x);
    delete aff;
    vwrobject = g;
    CheckErrors();
  }

  if(HaveFileType(_WCF, AQUIFER, isetlist) &&
     HaveFileType(_WCF, AQUIFER, g->osetlist))
  {
    WCF_v2 *wcf = new WCF_v2();
    wcf->WriteFile(pid, outPath, this, &conList, AQUIFER, g->ID1x, g->ID2x);
    delete wcf;
    vwrobject = g;
    CheckErrors();
  }
  if((HaveFileType(_WCF, SURFACE_WATER, isetlist) &&
     HaveFileType(_WCF, SURFACE_WATER, g->osetlist)) ||
    (HaveFileType(_WCF, "SurfaceWater-Dissolved", isetlist) &&
     HaveFileType(_WCF, "SurfaceWater-Dissolved", g->osetlist)))
  {
    WCF_v2 *wcf = new WCF_v2();
    wcf->WriteFile(pid, outPath, this, &conList, SURFACE_WATER, g->ID1x, g->ID2x);
    delete wcf;
    vwrobject = g;
    CheckErrors();
  }
  if((HaveFileType(_SCF, SOIL, isetlist) &&
     HaveFileType(_SCF, SOIL, g->osetlist)) ||
    (HaveFileType(_SCF, "Soil-Dissolved", isetlist) &&
     HaveFileType(_SCF, "Soil-Dissolved", g->osetlist)))
  {
    SCF_v2 *scf = new SCF_v2();
    scf->WriteFile(pid, outPath, this, &conList, SOIL, g->ID1x, g->ID2x);
    delete scf;
    vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_SCF, SEDIMENT, isetlist) &&
     HaveFileType(_SCF, SEDIMENT, g->osetlist))
  {
    SCF_v2 *scf = new SCF_v2();
    scf->WriteFile(pid, outPath, this, &conList, SEDIMENT, g->ID1x, g->ID2x);
    delete scf;
    vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_ATO, AIR, isetlist) &&
     HaveFileType(_ATO, AIR, g->osetlist))
  {
    ATO_v2 *ato = new ATO_v2();
    ato->WriteFile(pid, outPath, this, &conList, AIR, g->ID1x, g->ID2x);
    delete ato;
    vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_ATO, "Polar Air", isetlist) &&
     HaveFileType(_ATO, "Polar Air", g->osetlist))
  {
    ATO_v2 *ato = new ATO_v2();
    ato->WriteFile(pid, outPath, this, &conList, "Polar Air", g->ID1x, g->ID2x);
    delete ato;
    vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_ATO, "Cartesian Air", isetlist) &&
     HaveFileType(_ATO, "Cartesian Air", g->osetlist))
  {
    ATO_v2 *ato = new ATO_v2();
    ato->WriteFile(pid, outPath, this, &conList, "Cartesian Air", g->ID1x, g->ID2x);
    delete ato;
    vwrobject = g;
    CheckErrors();
  }

  if(HaveFileType(_ATO, "Acute Air", isetlist) &&
     HaveFileType(_ATO, "Acute Air", g->osetlist))
  {
    ATO_v2 *ato = new ATO_v2();
    ato->WriteFile(pid, outPath, this, &conList, "Acute Air", g->ID1x, g->ID2x);
    delete ato;
    vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_ATO, "Acute Polar Air", isetlist) &&
     HaveFileType(_ATO, "Acute Polar Air", g->osetlist))
  {
    ATO_v2 *ato = new ATO_v2();
    ato->WriteFile(pid, outPath, this, &conList, "Acute Polar Air", g->ID1x, g->ID2x);
    delete ato;
    vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_ATO, "Acute Cartesian Air", isetlist) &&
     HaveFileType(_ATO, "Acute Cartesian Air", g->osetlist))
  {
    ATO_v2 *ato = new ATO_v2();
    ato->WriteFile(pid, outPath, this, &conList, "Acute Cartesian Air", g->ID1x, g->ID2x);
    delete ato;
    vwrobject = g;
    CheckErrors();
  }
  if((HaveFileType(_EPF, "Exposure Pathways", isetlist) &&
      HaveFileType(_EPF, "Exposure Pathways", g->osetlist)) ||
     (HaveFileType(_EPF, "Environmental Protection", isetlist) &&
      HaveFileType(_EPF, "Environmental Protection", g->osetlist)))
  {
    EPF_v2 *epf = new EPF_v2();
    epf->WriteFile(pid, outPath, this, &conList, g->ID1x, g->ID2x);
    delete epf;
    vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_EPF, "Acute Exposure Pathways", isetlist) &&
     HaveFileType(_EPF, "Acute Exposure Pathways", g->osetlist))
  {
    EPF_v2 *epf = new EPF_v2();
    epf->WriteFile(pid, outPath, this, &conList, g->ID1x, g->ID2x);
    delete epf;
    vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_RIF, "Receptor Intakes", isetlist) &&
     HaveFileType(_RIF, "Receptor Intakes", g->osetlist))
  {
    RIF_v2 *rif = new RIF_v2();
    rif->WriteFile(pid, outPath, this, &conList, g->ID1x, g->ID2x);
    delete rif;
    vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_HIF, "Health Impacts", isetlist) &&
     HaveFileType(_HIF, "Health Impacts", g->osetlist))
  {
    HIF_v2 *hif = new HIF_v2();
    hif->WriteFile(pid, outPath, this, &conList, g->ID1x, g->ID2x);
    delete hif;
    vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_MSG, "Messages", isetlist) &&
     HaveFileType(_MSG, "Messages", g->osetlist))
  {
    if (WriteFromFileType(outPath, _MSG, "Status", "Messages", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
//  if(HaveFileType(_SUF, "Sensitivity/Uncertainty", isetlist) &&
//     HaveFileType(_SUF, "Sensitivity/Uncertainty", g->osetlist))
//  {
//    if (WriteFromFileType(outPath, _SUF, "Sensitivity", "Text", g->ID1x, g->ID2x))
//      vwrobject = g;
//    CheckErrors();
//  }
  if(HaveFileType(_EPA, "Air Dose Compliance File", isetlist) &&
     HaveFileType(_EPA, "Air Dose Compliance File", g->osetlist))
  {
    if (WriteFromFileType(outPath, _EPA, "AirDoseComplianceFile", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_EPA, "Surface Water Dose Compliance File", isetlist) &&
     HaveFileType(_EPA, "Surface Water Dose Compliance File", g->osetlist))
  {
    if (WriteFromFileType(outPath, _EPA, "SurfaceWaterDoseComplianceFile", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_EPA, "Biota Dose Compliance File", isetlist) &&
     HaveFileType(_EPA, "Biota Dose Compliance File", g->osetlist))
  {
    if (WriteFromFileType(outPath, _EPA, "BiotaDoseComplianceFile", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_FLX, "Soil", isetlist) &&
     HaveFileType(_FLX, "Soil", g->osetlist))
  {
    if (WriteFromFileType(outPath, _FLX, "SoilFile", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_AC2, "CMS Data", isetlist) &&
     HaveFileType(_AC2, "CMS Data", g->osetlist))
  {
    if (WriteFromFileType(outPath, _AC2, "CMSDataFile", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_RDF, "Recovery Data", isetlist) &&
     HaveFileType(_RDF, "Recovery Data", g->osetlist))
  {
    if (WriteFromFileType(outPath, _RDF, "RecoveryDataFile", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  if((HaveFileType(_TXT, "Sensor Config", isetlist) &&
     HaveFileType(_TXT, "Sensor Config", g->osetlist)) ||
    (HaveFileType(_TXT, "Source Config", isetlist) &&
     HaveFileType(_TXT, "Source Config", g->osetlist)))
  {
    if (WriteFromFileType(outPath, _TXT, "EMConfigFile", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  if((HaveFileType(_HQF, "Aquatic HQ", isetlist) &&
     HaveFileType(_HQF, "Aquatic HQ", g->osetlist)) ||
    (HaveFileType(_HQF, "Terrestrial HQ", isetlist) &&
     HaveFileType(_HQF, "Terrestrial HQ", g->osetlist)) ||
    (HaveFileType(_HQF, "Aquatic Organism HQ", isetlist) &&
     HaveFileType(_HQF, "Aquatic Organism HQ", g->osetlist)) ||
    (HaveFileType(_HQF, "Terrestrial Organism Intake HQ", isetlist) &&
     HaveFileType(_HQF, "Terrestrial Organism Intake HQ", g->osetlist)))
  {
    if (WriteFromFileType(outPath, _HQF, "HazardQuotients", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_BBF, "Eco Body Burdens SUF", isetlist) &&
     HaveFileType(_BBF, "Eco Body Burdens SUF", g->osetlist))
  {
    if (WriteFromFileType(outPath, _HQF, "HazardQuotients", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_BBF, "Eco Body Burdens SUF", isetlist) &&
     HaveFileType(_BBF, "Eco Body Burdens SUF", g->osetlist))
  {
    if (WriteFromFileType(outPath, _HQF, "EcoBodyBurdensSUF", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_TWI, "Terrestrial Wildlife Intake", isetlist) &&
     HaveFileType(_TWI, "Terrestrial Wildlife Intake", g->osetlist))
  {
    if (WriteFromFileType(outPath, _TWI, "TerrestrialWildlifeIntake", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  if(HaveFileType(_EXF, "Aquatic Organism Effects", isetlist) &&
     HaveFileType(_EXF, "Aquatic Organism Effects", g->osetlist))
  {
    if (WriteFromFileType(outPath, _HQF, "AquaticOrganismEffects", "Text", g->ID1x, g->ID2x))
      vwrobject = g;
    CheckErrors();
  }
  return 0;
}

int Glyph::WriteDatasetsFromGID(char *outpath)
{
  int i;
  int ct;
  int recct;
  int ErrorCode = 0;
  char vname[SMALLSTRING];
  char cbuf[MAXSTRING];

  recct = gid.ReadGID(outpath, uidic);
  if(recct < 0) return 0;
  if(0 == gid.params.size()) return 0;
  if(recct > 0 && 0!= strcmpi(uiset, blank))
  {
    // clear UI dataset if there is one
    ErrorCode = GetVarCount(pid, uidic, &ct);
    int idx[MMF_MAX_DIM] = {0};
    for(i = 1; i<= ct; i++)
    {
      ErrorCode = GetVarName(pid, uidic, i, vname);
      ErrorCode = ClearVariable(pid, uiset, vname, idx);
    }
    i = 0;
    for(PARAM_PTR_VEC_ITR iv = gid.params.begin(); iv!= gid.params.end(); ++iv)
    {
      i++;
      Param *p = *iv;
      rstrcpy(vname, p->vname.c_str());
      ErrorCode = GetVarUnit(pid, uidic, vname, cbuf);
      p->vunit = string(cbuf);
      ErrorCode = GetVarType(pid, uidic, vname, cbuf);
      p->vtype = string(cbuf);
      PutString(pid, uiset, "zVar", blank, SetIdx(i), vname);
      ErrorCode = p->PutValues(uiset, i);
    }
  }
  return 0;
}

void Glyph::WriteToFileType(char *outpath, char *ext, char *dname, char *vname)
{
  int buflen = MAXSTRING;
  int idx[MMF_MAX_DIM]; for(int i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;

  GetSetName(dname, setname, ID2x, osetlist);
  if(0 == strlen(setname)) return;

  /* Open file in text mode: */
  FILE *stream = fopen(AddExtension(outpath, ext), "r+t");
  if(stream == NULL) return;


  long hset = DataSetGetHandle(pid, setname);
  long hvar = VariableGetHandleByDataSet(pid, hset, vname);
  while(fgets(dummy, buflen, stream)!= NULL)
  {
    if(dummy[0])
    {
      idx[0]++;
      VariableSetString(pid, hset, hvar, blank, idx, dummy);
    }
  }
  fclose(stream);
}

bool Glyph::WriteFromFileType(char *outpath, char *ext, char *dname, char *vname, char *ID1x, char *ID2x)
{
  int i, ErrorCode;
  ocsv *outf;
  int idx[MMF_MAX_DIM]; for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;

  newGetSetName(ID2x, dname, isetlist, setname);
  if(0 == strlen(setname)) return 0;

  /* Open file in text mode: */
  if(0 == access(AddExtension(outpath, ext), 0))
    outf = new ocsv(AddExtension(outpath, ext), '"', ',', _APPEND_);
  else
    outf = new ocsv(AddExtension(outpath, ext), '"', ',', _CREATE_);
  if(!outf->ok()) return 0;

  int numlines = 0;
  ErrorCode = GetVarDimSize(pid, setname, vname, SetIdx(), &numlines);

  *outf << ID1x << numlines << NewLn;
  for (i=1; i<=numlines; i++)    
  {
    idx[0] = i;
    if (!GetString(pid, setname, vname, blank, idx, dummy))
      fprintf(outf->fptr,"%s",dummy);
  }
  delete outf;
  return 1;
}
