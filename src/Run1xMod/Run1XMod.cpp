// Run1XMod.cpp : Defines the entry point for the console application.

#include <direct.h>
#include "CONv2.h"
#include "OSv2.h"
#include "TBv2.h"
#include "pugixml.hpp"

/*
<?xml version="1.0"  encoding='ISO-8859-1'?>

<!ELEMENT TREECSBenchmarkInfo (Medium*)>
<!ELEMENT Medium (Benchmark*)>
<!ELEMENT Benchmark (#PCDATA)>

<!ATTLIST TREECSBenchmarkInfo>

<!ATTLIST Medium
  type CDATA #REQUIRED
  applicability CDATA #REQUIRED>

<!ATTLIST Benchmark 
  casname CDATA #REQUIRED
  casrn CDATA #REQUIRED
  unit CDATA #REQUIRED>
*/

bool doBench = true;
pugi::xml_document dom;

// commandline parser and variables
CStringParser args;
char modid[SMALLSTRING];
char simname[LARGESTRING];
char simdir[LARGESTRING];
char moddes[LARGESTRING];
char moddir[LARGESTRING];
  char envPTH[MAXSTRING];

// frames 2x variables
long pid;
int cancel = 0;

// other control variables
bool doUI;
bool doEXE;
bool verbose = false;
bool ignoreIOerrors = false;

// 1x file mapping dictionary and dataset
char inidic[SMALLSTRING];
char iniset[SMALLSTRING];
FILETYPE_PTR_MAP mapFileType;

// variables for reading and write from 2x IO
char setname[SMALLSTRING];
int _Idx[8]  =  {0,0,0,0,0,0,0,0};

int *SetIdx(int _1,int _2, int _3, int _4, int _5, int _6, int _7, int _8)
{
  _Idx[0]  = _1;
  _Idx[1]  = _2;
  _Idx[2]  = _3;
  _Idx[3]  = _4;
  _Idx[4]  = _5;
  _Idx[5]  = _6;
  _Idx[6]  = _7;
  _Idx[7]  = _8;
  return _Idx;
}

FileType::FileType(char *type, char *qual, int ix1, int ix2)
{
  int req = 0;
  int numDic = 0;
  int ErrorCode = 0;
  char dummy[SMALLSTRING];

  ftype = type;
  fqual = qual;

  ErrorCode = GetVarDimSize(pid, iniset, DICLIST, SetIdx(ix1, ix2), &numDic);
  for(int k = 0; k<numDic; k++)
  {
    ErrorCode = GetString(pid, iniset, DICLIST, blank, SetIdx(ix1, ix2, k+1), dummy);
    ErrorCode = GetInteger(pid, iniset, "DicRequired", blank, SetIdx(ix1, ix2, k+1), &req);
    if(req == 1) dicreq.push_back(dummy);
    if(req == 0) dicopt.push_back(dummy);
  }
}

void FindBenchmarks(char *type, char *cas, Series_v2 *conc)
{
  pugi::xml_node medium_nodes = dom.child("TREECSBenchmarkInfo").child("Medium");
  for (pugi::xml_node medium_node = medium_nodes; 
       medium_node; medium_node = medium_node.next_sibling())
  {
    if (!rstrcmpi(type,medium_node.attribute("type").value()))
    {
      pugi::xml_node benchmark_nodes = medium_node.child("Benchmark");
      for (pugi::xml_node benchmark_node = benchmark_nodes; 
           benchmark_node; benchmark_node = benchmark_node.next_sibling())
      {
        if (!rstrcmpi(cas,benchmark_node.attribute("casrn").value()))
        {
          char unit[SMALLSTRING];
          rstrcpy(unit, benchmark_node.attribute("unit").value());

          if (!rstrcmpi("Human", medium_node.attribute("applicability").value()))  
          {
            conc->man = sGetConversion(conc->measure, benchmark_node.text().as_double(), unit, conc->yUnits);
            rstrcpy(conc->msgman, " Human benchmark (", benchmark_node.text().get(), unit, ") has been exceeded for ");
            rstrcat(conc->msgman, benchmark_node.attribute("casname").value()," in ", type);
          }
          if (!rstrcmpi("Eco", medium_node.attribute("applicability").value()))  
          {
            conc->eco = sGetConversion(conc->measure, benchmark_node.text().as_double(), unit, conc->yUnits);
            rstrcpy(conc->msgeco, " Eco benchmark (", benchmark_node.text().get(), unit, ") has been exceeded for ");
            rstrcat(conc->msgeco, benchmark_node.attribute("casname").value()," in ", type);
          }
        }
      }
    }
  }
}


class Run1XMod:public Test
{
public:
  int ErrorCode;

  STR_SET dicList;    // set of all dics in system
  GLYPH_PTR_MAP glyphlist;

  Run1XMod()
  {
    Name = "Run1XMod";
    Description = "Test Run1XMod";
    ErrorCode = SUCCESS;
  }

  int GetMappingOfFileTypeAndQualifier()
  {
    string ftype, fqual, typequal;
    int numFileType = 0;
    int numFileQual = 0;
    char tstr[SMALLSTRING];

    ErrorCode = GetVarDimSize(pid, iniset, FILETYPE, SetIdx(), &numFileType);
    if(ErrorCode!= SUCCESS) Log("ERROR GetMappingOfFileTypeAndQualifier:GetVarDimSize:");
    for(int i = 0; i<numFileType; i++)
    {
      ErrorCode = GetString(pid, iniset, FILETYPE, blank, SetIdx(i+1), tstr);
      ftype = string(_strupr(tstr));
      numFileQual = 0;
      GetVarDimSize(pid, iniset, "FileQual", SetIdx(i+1), &numFileQual);
      for(int j = 0; j<numFileQual; j++)
      {
        ErrorCode = GetString(pid, iniset, "FileQual", blank, SetIdx(i+1, j+1), tstr);
        fqual = string(_strupr(tstr));
        typequal = ftype + _DOT + fqual;

        FileType *ft = new FileType((char *)ftype.c_str(),(char *)fqual.c_str(), i+1, j+1); TrackLocation(ft);
        mapFileType[typequal] = ft;
      }
    }
    return 0;
  }

  void GlyphListClear()
  {
    for(GLYPH_PTR_MAP_ITR it = glyphlist.begin(); it!= glyphlist.end(); ++it)
    {
      Glyph *g = (*it).second;
      TrackDelete(g);
    }
    glyphlist.clear();
  }

  void FileMapClear()
  {
    for(FILETYPE_PTR_MAP_ITR it = mapFileType.begin(); it!= mapFileType.end(); ++it)
    {
      FileType *ft = (*it).second;
      TrackDelete(ft);
    }
    mapFileType.clear();
  }

  int MakeFUI(oGid *gid, char *modid, Glyph *con)
  {
    int i, j;
    int siteIdx = 1;  //always only one site
    int ErrorCode;
    int numClass;
    int numGroup;
    int numGlyph;
    char prefix[8];
    string key;

    gid->nrec = 0;
    *gid->fp << "FUI";
    long pos = gid->fp->getpos();
    gid->fp->smartQuote();
    *gid->fp << "          " << NewLn;
    gid->fp->alwaysQuote();

    numClass = 0;
    gid->PutValue("Version", SetIdx(), 0, NA, NA, 1.6);
    gid->PutValue("Sites", SetIdx(), 0, NA, NA, 1);
    gid->PutValue("SiteName", SetIdx(1), 0, NA, NA, "FRAMES 2x Simulated Site");
    ErrorCode = GetVarDimSize(pid, iniset, "ModClass", SetIdx(), &numClass);
    for(i = 0; i<numClass; i++)
    {
      numGroup = 0;
      ErrorCode = GetVarDimSize(pid, iniset, "ModGroup", SetIdx(i+1), &numGroup);
      for(j = 0; j<numGroup; j++)
      {
        ErrorCode = GetString(pid, iniset, "ModPrefix", blank, SetIdx(i+1, j+1), prefix);
        numGlyph = 0;
        for(GLYPH_PTR_MAP_ITR it = glyphlist.begin(); it!= glyphlist.end(); ++it)
        {
          key = (*it).first;
          if(0>key.compare("sink"))
          {
            Glyph *g = (*it).second;
            if(g->des)
              if(0 == g->Prefix.compare(prefix))
              {
                if((0!= strcmpi(prefix, "vwr")) || (0 == strcmpi(g->ID2x, modid)))
                {
                  numGlyph++;
                  g->WriteFUI(gid, prefix, siteIdx, numGlyph);
                  g->glyphidx = numGlyph;
                }
              }
            else
            {
              //do something
            }
          }
        }
        if(numGlyph>0)
        {
          rstrcpy(dummy, prefix, "Num");
          gid->PutValue(dummy, SetIdx(siteIdx), 0, NA, NA, numGlyph);
        }
      }
    }
    if(con!= NULL) con->WriteGIDSection(gid, "FUI");

    long end = gid->fp->getpos();
    gid->fp->setpos(pos);
    gid->fp->delim = ' ';
    *gid->fp << gid->nrec;
    gid->fp->delim = ',';
    gid->fp->setpos(end);
    return 0;
  }

  int MakeCSM(oGid *gid, char *modid)
  {
    int numGlyph = 0;
    int glyphIdx = 0;
    int siteIdx = 1;
    char *prefix = "Mod";
    STR_VEC_MAP_MAP_ITR im;
    string key;


    gid->nrec = 0;
    *gid->fp << "CSM";
    long pos = gid->fp->getpos();
    gid->fp->smartQuote();
    *gid->fp << "          " << NewLn;
    gid->fp->alwaysQuote();

    gid->PutValue("Version", SetIdx(), 0, NA, NA, 1.6);
    gid->PutValue("Sites", SetIdx(), 0, NA, NA, 1);
    gid->PutValue("SiteName", SetIdx(1), 0, NA, NA, "FRAMES 2x Simulated Site");

    int nmod = 0;

    for(GLYPH_PTR_MAP_ITR it = glyphlist.begin(); it!= glyphlist.end(); ++it)
    {
      Glyph *g = (*it).second;
      if((0 == rstrcmpi(g->ID2x, modid)) ||
        ((0!= rstrcmpi(g->ftype, "vwr")) && (0!= rstrcmpi(g->ftype, "gis"))) )
      {
        nmod++;
        glyphIdx++;
        g->WriteCSM(gid, prefix, siteIdx, glyphIdx, modid);
      }
    }
    gid->PutValue("NumMod", SetIdx(siteIdx), 0, NA, NA,  nmod);

    long end = gid->fp->getpos();
    gid->fp->setpos(pos);
    gid->fp->delim = ' ';
    *gid->fp << gid->nrec;
    gid->fp->delim = ',';
    gid->fp->setpos(end);
    return 0;
  }

  int LaunchGlyph(Glyph *modGlyph, char *workdir)
  {
    string exe;
    string cmdline;
    CStringParser exeargs;

    if(doUI) exe = modGlyph->UIExe;
    if(doEXE) exe = modGlyph->ModelExe;

    // ============  construct command line arguments ============================ =
    exeargs.Parse(exe.c_str(), CParseOptions(' '));
    cmdline = moddir;
    cmdline = cmdline + "\\" + exeargs.GetAt(0);
    cmdline = "\"" + cmdline + "\"";
    for(int i = 1; i<exeargs.GetCount(); i++)
      cmdline = cmdline + " " + exeargs.GetAt(i);

    cmdline = cmdline + " " + workdir + "\\~tmp~";
    cmdline = cmdline + " " + workdir + "\\(tmp)";

    if(0 == modGlyph->Prefix.compare("vwr"))
    {
      Glyph *src = modGlyph->vwrobject;
      if(src)
      {
        cmdline = cmdline + " " + itoa(src->siteidx, dummy, 10);
        cmdline = cmdline + " " + itoa(src->glyphidx, dummy, 10);
        cmdline = cmdline + " " + src->ID1x;
      }
      else
        cmdline = blank;
    }
    else
    {
      cmdline = cmdline + " " + itoa(modGlyph->siteidx, dummy, 10);
      cmdline = cmdline + " " + itoa(modGlyph->glyphidx, dummy, 10);
      cmdline = cmdline + " " + modGlyph->ID1x;
    }

    chdir(moddir);
//    WritePrivateProfileString("App Path", "FUI", concat(moddir,"\\"), "FramesUI.ini");

    // ============  launch the executable ============================ =
    return _Launcher(moddir, (char *)cmdline.c_str(), 1);
  }

  virtual bool doTest(ofstream &status, ofstream &doc)
  {
    int i;
    int ct;
    int dicCt;
    long hnd;
    long done;
    bool found;
    char tmpId[SMALLSTRING];
    char outPath[MAXPATH];
    char appPath[MAXPATH];
    char workdir[MAXPATH];
    struct _finddata_t ff;
    Glyph *g;
    Glyph *conGlyph;
    Glyph *aosGlyph;
    Glyph *tosGlyph;
    Glyph *cctGlyph;
    Glyph *sslGlyph;
    Glyph *modGlyph;
    STR_SET_ITR id;

    // Set import dictionary and dictionary mapping simulation names
    rstrcpy(inidic, "Import");
    rstrcpy(iniset, "MaintainImporter.Sim.Mod1.Import");

    // Get application path directory for Frames2x
    if(GetModString(pid, "Startup", "AppPath", blank, SetIdx(), appPath)) return false;

    // Check for/create temporary directory for Frames1x directory workspace
    rstrcpy(workdir, moddir, "\\V2Temp");
    if(0!= _access(workdir, 0))
      if(0!= _mkdir(workdir))
      {
        rstrcpy(dummy,"Unable use directory workspace: ", workdir);
        Log(dummy);
        return false;
      }

      // ================   cleanup working directory ====================
      //  leftover(tmp) files seems to cause problems if testing ._GID with FUI
      rstrcpy(outPath, workdir, "\\(tmp).*");
      hnd = _findfirst(outPath, &ff);
      done = hnd;
      while(done != -1)
      {
        rstrcpy(outPath, workdir, "\\", ff.name);
        unlink(outPath);
        done = _findnext(hnd, &ff);
      }
      _findclose(hnd);

      rstrcpy(outPath, workdir, "\\~tmp~.*");
      hnd = _findfirst(outPath, &ff);
      done = hnd;
      while(done != -1)
      {
        rstrcpy(outPath, workdir, "\\", ff.name);
        unlink(outPath);
        done = _findnext(hnd, &ff);
      }
      _findclose(hnd);

      // ================   GETTING STARTUP DICTIONARY LIST ====================
      Log("GETTING DICTIONARY LIST");
      found = false;
      dicCt = 0;
      ErrorCode = GetModDimSize(pid, "Startup", "Dictionaries", SetIdx(), &dicCt);
      for(i = 1; i<= dicCt; i++)
      {
        ErrorCode = GetModString(pid, "Startup", "Dictionaries", blank, SetIdx(i), outPath);
        fnSetPath(outPath);
        dicList.insert(fnfile);
        if (rstrcmpi(fnfile, inidic) == 0)
          found = true;
      }
//      CheckErrors();

      Log("LOADING MAPPING IMPORT FILE");
      if (!found)
      {
//        rstrcpy(outPath, appPath, "\\1xImport\\", inidic, ".dic");
        rstrcpy(outPath, moddir, "\\2xExport\\", inidic, ".dic");
        ErrorCode = AddOpenDictionary(pid, outPath, inidic);
      }
      if(DataSetGetHandle(pid, iniset)<= 0)
      {
//        rstrcpy(outPath, appPath, "\\1xImport\\", iniset);
        rstrcpy(outPath, moddir, "\\2xExport\\", iniset);
        ErrorCode = AddOpenDataSet(pid, inidic, outPath, iniset);
      }
      ErrorCode = DataSetGetHandle(pid, iniset);
      CheckErrors();

  // ============== = PREPARE the mappings ==============================
      Log("GET MAPPING OF FILETYPES AND QUALIFIERS");
      GetMappingOfFileTypeAndQualifier();

  // ====================== PREPARE the glyphs ==========================
  //
  // Must include CON in FUI section every time if available
  // May or may not be able to determine the des file. May have modules in
  // v2 that are not in v1 therefore you must compensate by creation of a
  // sink or source that would supply the expected file types, the question
  // then arises how do we know how many of these to create for the module
  // to respond properly. does it matter?

  // Support for one sink and one source for each filetype in 1x
  // for connections to 2x only modules.  The actual module id will
  // be changed to to fit the 1x theme (ID1x).

  // If a des is found the _GID and output files will be supported.
  // Note there is limited support for connections other than
  // those of the executed module.

  // If a glyph doesn't consume or produce to the executed module and
  // does not have a des file it won't be include in the simulated _GID.

      Log("CREATING GLYPH OBJECTS");
      CON_v2 *con = NULL;
      OS_v2 *aos = NULL;
      OS_v2 *tos = NULL;
//      TB_v2 *cct = NULL;
//      TB_v2 *ssl = NULL;
      modGlyph = NULL;
      conGlyph = NULL;
      aosGlyph = NULL;
      tosGlyph = NULL;
      cctGlyph = NULL;
      sslGlyph = NULL;
      GlyphListClear();
      CheckErrors();

      ct = 0;
      ErrorCode = GetVarDimSize(pid, simname, "ModID", SetIdx(), &ct);
      for(i = 1; i<= ct; i++)
      {
        ErrorCode = GetString(pid, simname, "ModID", blank, SetIdx(i), tmpId);
        g = new Glyph(simname, tmpId);
        TrackLocation(g);
        if(g)
          if(g->des)
          {
            if(g->FindWriteType("con",blank)) conGlyph = g;
            if(g->FindWriteType("aos","Aquatic Organism")) aosGlyph = g;
            if(g->FindWriteType("tos","Terrestrial Organism")) tosGlyph = g;
            if(g->FindWriteType("cct","Chemical Terrestrial TRVs")) cctGlyph = g;
            if(g->FindWriteType("ssl","Chemical SSLs")) sslGlyph = g;
          }
          else
          {
          }

        //add only sinks and sources to glyphlist
        if(g->Source(modid))                   glyphlist[g->ID2x] = g;   //source
        else if(g->Sink(modid))                glyphlist[g->ID2x] = g;   //sink
        else if(!rstrcmpi(tmpId,modid))        glyphlist[g->ID2x] = g;   //self
        else if(!rstrncmpi(g->ID1x,"exp",3))   glyphlist[g->ID2x] = g;   //always add exp for reach back to media sources
//        else if(!rstrncmpi(g->ID1x,"wcf",3))   glyphlist[g->ID2x] = g;   //always add wcf for reach back to treecs tier2 sources
//        else if(!rstrncmpi(g->ID1x,"scf",3))   glyphlist[g->ID2x] = g;   //always add scf for reach back to treecs tier2 sources
        else if(!rstrncmpi(g->ID1x,"con",3))   glyphlist[g->ID2x] = g;   //always add con list
        else if(!rstrncmpi(g->ID1x,"aos",3))   glyphlist[g->ID2x] = g;   //always add aos list
        else if(!rstrncmpi(g->ID1x,"tos",3))   glyphlist[g->ID2x] = g;   //always add tos list
        else { TrackDelete(g);}
        CheckErrors();
      }

      // now we have only connected modules, map those to 1x CSM
      Log("GET SOURCES AND SINKS");
      for(GLYPH_PTR_MAP_ITR ig = glyphlist.begin(); ig!= glyphlist.end(); ++ig)
      {
        g = (*ig).second;
        g->GetSourcesAndSinks(glyphlist);
      }
      CheckErrors();

      // load database info
      // known primary key list
      modGlyph = glyphlist[modid];
      if (conGlyph!=NULL)
      {
        Log("MAP AND LOAD CON PARAMETERS");
        con = new CON_v2(dicList, "1xcon"); TrackLocation(con);
        if(modGlyph == conGlyph)
          con->GetConstituentList(modGlyph->osetlist, modGlyph->ID2x);
        else
          con->GetConstituentList(modGlyph->isetlist, conGlyph->ID2x);
        con->ReadConstituentDatasets(conGlyph);
        CheckErrors();
      }

      if (aosGlyph!=NULL)
      {
        Log("MAP AND LOAD AOS PARAMETERS");
        aos = new OS_v2(dicList, "Organism", "Aquatic", "1xaos"); TrackLocation(aos);
        if(modGlyph != aosGlyph)
          aos->GetKeyList(modGlyph->isetlist, aosGlyph->ID2x);
        CheckErrors();
      }

      if (tosGlyph!=NULL)
      {
        Log("MAP AND LOAD TOS PARAMETERS");
        tos = new OS_v2(dicList, "Organism", "Terrestrial", "1xtos"); TrackLocation(tos);
        if(modGlyph != tosGlyph)
          tos->GetKeyList(modGlyph->isetlist, tosGlyph->ID2x);
        CheckErrors();
      }
/*
      if (cctGlyph!=NULL)
      {
        Log("MAP AND LOAD CCT PARAMETERS");
        cct = new TB_v2(dicList, "CCT", "Chem", "1xcct"); TrackLocation(cct);
        if(modGlyph != cctGlyph)
          cct->GetKeyList(modGlyph->isetlist, cctGlyph->ID2x);
        CheckErrors();
      }

      if (sslGlyph!=NULL)
      {
        Log("MAP AND LOAD SSL PARAMETERS");
        ssl = new TB_v2(dicList, "SSL", "Chem", "1xssl"); TrackLocation(ssl);
        if(modGlyph != sslGlyph)
          ssl->GetKeyList(modGlyph->isetlist, sslGlyph->ID2x);
        CheckErrors();
      }
*/
      // prepare the _GID file =============================

      rstrcpy(outPath, workdir, "\\~tmp~.gid");
      oGid *gid = new oGid(outPath); TrackLocation(gid);

      // =================  make the UI sections ==========
      if(ErrorCode == 0)
      {
        for(GLYPH_PTR_MAP_ITR it = glyphlist.begin(); it!= glyphlist.end(); ++it)
        {
          g = (*it).second;
          ErrorCode = g->WriteGIDSection(gid, g->ID1x);
        }
      }
      // ================ =  make the FUI section ==========
      Log("MAKE FUI");
      if(ErrorCode == 0)
        ErrorCode = MakeFUI(gid, modid, conGlyph); // why pass modid
      CheckErrors();

      // ================ =  make the CSM section ==========
      Log("MAKE CSM");
      if(ErrorCode == 0)
        ErrorCode = MakeCSM(gid, modid);
      CheckErrors();

      TrackDelete(gid);

      //write the all possible boundary files from datasets
      Log("MAKE PDCFs");
      if(ErrorCode == 0 && doEXE)
      {
        rstrcpy(outPath, workdir, "\\~tmp~");
        for(GLYPH_PTR_MAP_ITR ig = glyphlist.begin(); ig!= glyphlist.end(); ++ig)
        {
          ErrorCode = modGlyph->WritePDCFFromDatasets(outPath, con->conList, (*ig).second);
          Log((char *)(*ig).first.c_str());
          CheckErrors();
        }
      }
      CheckErrors();

      cancel = 0;
      Log("LAUNCHING MODEL");
      CheckErrors();

      if (LaunchGlyph(modGlyph, workdir) == 0)
      {
        // =====================  setup benckmark file =======================
        rstrcpy(outPath, workdir, "\\benchmarks.xml");
        if (!dom.load_file(outPath))
        {
          SetWarning(pid, " Benchmark_Start");
          SetWarning(pid, " No bench mark file");
          SetWarning(pid, " Benchmark_End");
          SetWarning(pid, "");
          doBench = false;
        }

        // =====================  if no errors then write datasets =======================
        int errfile = 0;
        int wrnfile = 0;
        int gidfile = 0;

        rstrcpy(outPath, workdir, "\\(tmp).", _ERR);
        if(0 == access(outPath, 0))
          errfile = 1;
        rstrcpy(outPath, workdir, "\\(tmp).", _WRN);
        if(0 == access(outPath, 0))
          wrnfile = 1;
        rstrcpy(outPath, workdir, "\\(tmp).", _GID);
        if(0 == access(outPath, 0)) gidfile = 1;

        // ====================== save ui inputs even if errors ==========================
        Log("MAKE DATASETS");
        rstrcpy(outPath, workdir, "\\(tmp)");
        if(doUI)
        {

          if(gidfile != 0)
          {
            modGlyph->WriteDatasetsFromGID(outPath);
            // special handling of the seven database types
            if(modGlyph==conGlyph)
              ErrorCode = con->WriteConstituentDatasets(modGlyph);
            if(modGlyph==aosGlyph)
              ErrorCode = aos->WriteDatasets(modGlyph);
            if(modGlyph==tosGlyph)
              ErrorCode = tos->WriteDatasets(modGlyph);
//            if(modGlyph==sslGlyph)
//            {
//              ssl = new TB_v2(dicList, "ssl", CHEM, "1xssl"); TrackLocation(tos);
//              ErrorCode = ssl->WriteDatasets(modGlyph, con->conList);
//            }
//            if(modGlyph==cctGlyph)
//            {
//              cct = new TB_v2(dicList, "cct", CHEM, "1xcct"); TrackLocation(tos);
//              ErrorCode = cct->WriteDatasets(modGlyph);
//              ErrorCode = cct->WriteDatasets(modGlyph, con->conList, tos->keyList);
//            }

//            if(modGlyph==bsaGlyph)
//            {
//              bsa = new OS_v2(dicList, "Organism", "Terrestrial", "1xbsa"); TrackLocation(tos);
//              ErrorCode = bsa->WriteDatasets(modGlyph, con->conList, aos->keyList);
//            }
//            if(modGlyph==ebfGlyph)
//            {
//              ebf = new OS_v2(dicList, "Organism", "Terrestrial", "1xebf"); TrackLocation(tos);
//              ErrorCode = ebf->WriteDatasets(modGlyph, con->conList, aos->keyList);
//            }

            CheckErrors();
          }
          if(gidfile == 0 && errfile == 0)
          {
            // exit with no changes
            cancel = 1;
            // it would also work here to clear errors and warnings
            // since the models don't write to datasets directly
            // but that may not forever be the case
          }
        }

        // ======================   dont save datasets if model errors ==================== =
        if((!errfile) && doEXE)
          ErrorCode = modGlyph->WriteDatasetsFromPDCF(outPath, con->conList);
 
        if(cancel == 0)
        {
          int wrnct = 0;
          int errct = 0;
          ErrorCode = GetCountOfWarningsAndErrors(pid, &wrnct, &errct);

          if(errfile == 1)
          {
            rstrcpy(outPath, workdir, "\\(tmp).", _ERR);
            ifstream fin;
            fin.open(outPath, ios::in);
            //what is maxsize of string now in v2?
            while(fin.getline(dummy, sizeof(dummy), '\n'))
              Log(dummy);
            fin.close();
          }
          else
          {
            if(errct>0)
            {
              sprintf(outPath, "%s\\%s.%s.err", simdir, simname, modid);
              WriteErrorFile(pid, outPath);
            }
          }

          if(wrnfile == 1)
          {
            rstrcpy(outPath, workdir, "\\(tmp).", _WRN);
            ifstream fin;
            fin.open(outPath, ios::in);
            while(fin.getline(dummy, sizeof(dummy), '\n'))
              SetWarning(pid, dummy);
            fin.close();
          }
          else
          {
            if(wrnct>0)
            {
              sprintf(outPath, "%s\\%s.%s.wrn", simdir, simname, modid);
              WriteWarningFile(pid, outPath);
            }
          }
        }
      }
      else
      {
        Log("Run1XMod.exe: Module Launch FAILED!");
        cancel = 0;
      }
      CheckErrors();

      if (con) {  con->ConListClear();      TrackDelete(con);  }
      if (aos) {  aos->KeyListClear();      TrackDelete(aos);  }
      if (tos) {  tos->KeyListClear();      TrackDelete(tos);  }
//      if (cct) {  TrackDelete(cct);  }
//      if (ssl) {  TrackDelete(ssl);  }
      dicList.clear();
      GlyphListClear();
      FileMapClear();
      DelDataSet(pid, iniset);
      dom.reset();
      return true;
    }
};

//-----------------------------------------------------------------------------------
char *CheckUnit(long pid, char *setname, char *var, char *vunit)
{
  char unitname[SMALLSTRING];
  char dicname[MAXSTRING];

  // check the units
  int ErrorCode = GetDicId(pid, setname, dicname);

  rstrcpy(dummy,blank);
  ErrorCode = GetVarMeasure(pid, dicname, var, unitname);
  int im = GetMeasureIdx(unitname);
  if (im>=0)
  {
    int iu = GetUnitAbbrIdx(im,vunit);
    if (iu<0)
    {
      GetUnitAbbr(im,1,unitname);
      return rstrcpy(vunit,unitname);
    }
  }
  return vunit;
}
//-----------------------------------------------------------------------------------
// index would be the index of the multiple setnames having the same dictionary
// important to note when models accept multiples of a particular dictionary
int GetSetName(char *dicname, char *setname, char *ID, STR_VEC_MAP setlist)
{
  rstrcpy(setname, blank);
  STR_VEC_MAP_ITR it = setlist.find(dicname);
  if(it == setlist.end()) return 0;
  STR_VEC sv = (*it).second;
  if(sv.size() == 0) return 0;
  if (ID==blank)
  {
    rstrcpy(setname, sv[0].c_str());
    return strlen(setname);
  }
  else
  {
    int len = strlen(ID);
    for (int i=0; i<sv.size(); i++)
    {
      rstrcpy(setname, sv[i].c_str());
      if (rstrncmpi(setname,ID,len)== 0)
        return strlen(setname);
    }
    rstrcpy(setname, blank);
  }
  return strlen(setname);
}
//-----------------------------------------------------------------------------------
// index would be the index of the multiple setnames having the same dictionary
// important to note when models accept multiple dictionary it is important to match modId
int newGetSetName(char *modId, char *dicname, STR_VEC_MAP setlist, char *setname)
{
  int len = strlen(modId);
  rstrcpy(setname, blank);
  STR_VEC_MAP_ITR it = setlist.find(dicname);
  if(it == setlist.end()) return 0;
  for(STR_VEC_ITR is = (*it).second.begin(); is!=(*it).second.end(); ++is)
  {
    if(!rstrncmpi(modId, (char *)(*is).c_str(), len))
    {
      rstrcpy(setname, (*is).c_str());
      return strlen(setname);
    }
  }
  return 0;
}

//-----------------------------------------------------------------------------------
void CheckErrors()
{
  bool unexpectedError = false;
  long enumHandle = _EnumOpen(pid);
  int count = _EnumProcessErrors(enumHandle);
  for(int e = 1; e<= count; e++)
  {
    long ecode = _EnumGetHandleAtIndex(enumHandle, e);
    long slen = _EnumGetStringAtIndex(enumHandle, e, dummy);
    switch(ecode)
    {
    // These errors are all expected and are OK
    case -1010:  // undefined value
    case -1009:  // invalid indices
    case -3009:  // thrown by DataSetGetHandle if not found
    case -49998: // user defined, usually for logging
      if(verbose)        cout << e << " " << ecode << " " << dummy <<  endl;
      break;

    // These rest are all unexpected and are not OK
    default:
      if(verbose)
        cout << e << " " << ecode << " " << dummy <<  endl;
      unexpectedError = true;
      break;
    }
  }
  _EnumClose(enumHandle);
  if(ignoreIOerrors)
    ClearErrors(pid);
  else if(!unexpectedError)
    ClearErrors(pid);
}

//-----------------------------------------------------------------------------------
void Log(char *s)
{
  if(verbose) SetError(pid, concat("\t", s));
}

//-----------------------------------------------------------------------------------
int MyReportHook( int reportType, char *message, int *returnValue )
{
  debugMessages << message ;
  return *returnValue;
}

//-----------------------------------------------------------------------------------
void usage()
{
  cout << "Usage: " << endl;
  cout << "   Helpful options, order non specific, but must precede all others" << endl;
  cout << "       <Ignore errors>         ex  /I" << endl;
  cout << "       <Verbose>               ex  /V" << endl;
  cout << "   Frames1x required, order specific" << endl;
  cout << "       </UI or /EXE from DES>  ex  /UI" << endl;
  cout << "       <Frames1X directory>    ex: \"C:\\Frames1x\"" << endl;
  cout << "       <Module DES file>       ex: \"MepSrcSl.des\"" << endl;
  cout << "   Frames 2x supplied, order specific" << endl;
  cout << "       <sim directory>         ex: \"C:\\FramesV2\\MakeGid\"" << endl;
  cout << "       <sim name>              ex: \"ImportDes.sim\"" << endl;
  cout << "       <module id>             ex: Mod1" << endl;
  cout << endl;
}

//-----------------------------------------------------------------------------------
bool StartUp()
{
  int i;
  int ct;
  char buffer[MAXSTRING];
  char envPID[SMALLSTRING];
  char envMOD[SMALLSTRING];


  args.Parse(::GetCommandLine(), poCmdLine);
  if(args.GetCount()<7)
  {
    cout << endl << "ERROR: Too few arguments" << endl << endl;
    usage();
    return false;
  }

  ct = args.GetCount();
  rstrcpy(modid, args.GetAt(ct-1));
  rstrcpy(simname, args.GetAt(ct-2));
  rstrcpy(simdir, args.GetAt(ct-3));
  rstrcpy(moddes, args.GetAt(ct-4));
  rstrcpy(moddir, args.GetAt(ct-5));
  string modexe = args.GetAt(ct-6);
  doUI = (0 == modexe.compare("/UI"));
  doEXE = (0 == modexe.compare("/EXE"));

  if(!(doUI || doEXE))
  {
    cout << "ERROR:  Nothing to do - '/UI' or '/EXE' are missing or mispelled or in the w." << endl;
    return false;
  }

//  always on for ide testing
//  ignoreIOerrors = true;
  verbose = true;
  if(ct > 7)
    for(i = 1; i<ct; i++)
    {
      if(0 == ((string)args.GetAt(i)).compare("/I")) ignoreIOerrors = true;
      if(0 == ((string)args.GetAt(i)).compare("/V")) verbose = true;
    }
  if(verbose)
  {
    usage();
    for(i = 1; i<ct; i++)
      cout << " Arg " << i << " " << args.GetAt(i) << endl;
  }

//  struct HINSTANCE__ * hLibModule = LoadLibrary("c:\\program files\\framesv2\\systemio.dll");
  pid = OpenIO(simdir, simname, modid);
  if(pid<=0)
  {
    //assume at least a connection to the dll with next call
    ReadError(pid, pid, dummy);
    cout << endl << "ERROR:  OpenIO failed.( " << pid << " ) " << endl;
    cout << endl << "  " <<  dummy << endl << endl;
    cout << "  Path:       " << simdir << endl;
    cout << "  Simulation: " << simname << endl;
    cout << "  Module:     " << modid << endl;
    cout << endl;
    return false;
  }

  sprintf(envPTH,"fPTH=%s",_getcwd(buffer, MAXSTRING));
  putenv(envPTH);  
  sprintf(envPID,"fPID=%d",pid);
  putenv(envPID);  
  sprintf(envMOD,"fMOD=%s",modid);
  putenv(envMOD);

  ClearWarnings(pid);
  ClearErrors(pid);
  return true;
}

//-----------------------------------------------------------------------------------
void CleanUp()
{
  CloseIO(pid, cancel);
}

//-----------------------------------------------------------------------------------
void main(int argc, char* argv[])
{
  if(StartUp())
  {
    tests.AddTest(new GrabHeapState());
    tests.AddTest(new Run1XMod());
    tests.AddTest(new CheckHeapForLeaks());
    tests.doTests();
    CleanUp();
  }
}
