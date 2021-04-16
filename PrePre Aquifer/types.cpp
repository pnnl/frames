/*______________________________________________________________________________

   Date:       February 2001
   Programmer: Mitch Pelton
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
   Client:     DOE
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "types.h"

/*____________________________________________________________________________*/

// Helper Variables

char unit1[STRING64];                      // for temporary unit reads
char unit2[STRING64];                      // for temporary unit reads
char unit3[STRING64];                      // for temporary unit reads
char *EXT[9] = {"SCF","AFF","ATO","WFF","WCF","EPF","RIF","HIF","GFF"};

/*______________________________________________________________________________

class Locales
 public:
  int readok;
  int count;                         //number of locations
  double *x;                         //x coordinates
  double *y;                         //y coordinates
  double *z;                         //elevations
  static char *xU = {"km"};                  //kilometers
  static char *yU = {"km"};                  //kilometers
  static char *zU = {"m"};                   //meters
*/

  char *Locales::xU;
  char *Locales::yU;
  char *Locales::zU;

  Locales::Locales(int count)
  {
    readok = 0;
    xU = "km";                  //kilometers
    yU = "km";                  //kilometers
    zU = "m";                   //meters
    this->count = count;
    x = new double[count];
    y = new double[count];
  }

  Locales::~Locales()
  {
    delete[] x;
    delete[] y;
  }

  int Locales::Read(icsv *ifile)
  {
    int i;

    if (ifile!=NULL && !readok)
    {
      for (i=0; i<count; i++)
      {
        *ifile >> x[i] >> unit1 >> y[i] >> unit2 >> NewLn;
        if (strcmpi(unit1,xU)!=0) return 0;
        if (strcmpi(unit2,yU)!=0) return 0;
      }
      readok = 1;
    }
    return readok;
  }

  void Locales::TestRead(ocsv *ofile)
  {
    int i;

    for (i=0; i<count; i++)
      *ofile << x[i] << xU << y[i] << yU << NewLn;
  }

  void Locales::Write(ocsv *ofile)
  {}

/*______________________________________________________________________________

class FluxType
  int readok;
  char name[STRING32];
  double radius;
  double density;
  double fraction;
  static char radiusU  = {"um"};            //micrometers
  static char densityU = {"g/cm^3"};        //grams per cubic centimeter
  static char masfracU = {"fraction"};      //fraction of the whole
*/

  char *FluxType::radiusU;
  char *FluxType::densityU;
  char *FluxType::massfracU;

  FluxType::FluxType()
  {
    readok = 0;
    radiusU  = "um";            //micrometers
    densityU = "g/cm^3";        //grams per cubic centimeter
    massfracU = "fraction";     //fraction of the whole
  }

  int FluxType::Read(icsv *ifile)
  {
    if (ifile != NULL && !readok)
    {
      *ifile >> name;
      if (strncmpi(name,"gas",3) == 0)
      {
        *ifile >> fraction >> unit1;
        if (strcmpi(unit1,massfracU)) return 0;
      }
      else
      {
        *ifile >> radius >> unit1;
        if (strcmpi(unit1,radiusU)) return 0;
      }
      *ifile >> density >> unit1 >> NewLn;
      if (strcmpi(unit1,densityU)) return 0;
      readok = 1;
    }
    return readok;
  }

  void FluxType::TestRead(ocsv *ofile)
  {
    Write(ofile);
  }

  void FluxType::Write(ocsv *ofile)
  {
    *ofile << name;
    if (strncmpi(name,"gas",3) == 0)
      *ofile << fraction << massfracU;
    else
      *ofile << radius << radiusU;
    *ofile << density << densityU << NewLn;
  }

/*______________________________________________________________________________

class Contam
 private:
 public:
  icsv *ifile;
  ocsv *ofile;
  fpos_t fpos;
  fpos_t *time;
  int readok;
  int numprog;
  int numtime;
  char cas[STRING16];
  char name[STRING64];
  Contam **prog;
*/

  Contam::Contam()
  {
    readok = 0;
    numtime = 0;
    numprog = 0;
    contype = 0;
    time = NULL;
    prog = NULL;
  }

  Contam::~Contam()
  {}

  int Contam::Read(icsv *ifile, int isparent)
  {
    return 0;
  }

  void Contam::TestRead(ocsv *ofile, char *pName, char *pCas)
  {}

  void Contam::Write(ocsv *ofile)
  {}

/*______________________________________________________________________________

class DataSet
 private:
 public:
  int readok;
  int capacity;
  icsv *ifile;
  ocsv *ofile;
  fpos_t fpos;
  char ID[STRING16];
  int numcon;
  Contam **con;
*/

  DataSet::DataSet(icsv *ifile, char *ID)
  {
    readok = 0;
    ofile = NULL;
    numcon = 0;
    capacity = 0;
    rstrcpy(this->ID,ID);
    this->ifile = ifile;
    fpos = ifile->getpos();
  }

  DataSet::~DataSet()
  {}

  int DataSet::Read()
  {
    return 0;
  }

  void DataSet::TestRead(ocsv *ofile)
  {}

  void DataSet::Write()
  {}

/*______________________________________________________________________________

class Module
 private:
  int readok;
  int capacity;
  icsv *ifile;
  ocsv *ofile;
  f_tpos fpos;
 public:
  int numset;
  DataSet **dataset;
  char ID[STRING16];
*/

  Module::Module(icsv *ifile, char *ID)
  {
    readok = 0;
    numset = 0;
    capacity = 0;
    ofile = NULL;
    dataset = NULL;
    rstrcpy(this->ID,ID);
    this->ifile = ifile;
    fpos = ifile->getpos();
  }
  Module::Module(icsv *ifile, char *ID,int numline)
  {
    readok = 0;
    numset = 0;
    capacity = 0;
    ofile = NULL;
    dataset = NULL;
    numlines = numline;
    rstrcpy(this->ID,ID);
    this->ifile = ifile;
    fpos = ifile->getpos();
  }

  Module::~Module()
  {}

  void Module::TestRead()
  {}

  void Module::Add(DataSet *set)
  {
    int i;

    DataSet **temp;
    if (numset < capacity)
    {
      dataset[numset] = set;
      numset++;
    }
    else
    {
      capacity+=10;
      temp = dataset;
      dataset = new DataSet*[capacity];
      for (i=0; i<numset; i++)
        dataset[i] = temp[i];
      dataset[numset] = set;
      delete[] temp;
      numset++;
    }
  }

  DataSet *Module::GetDataSet(char *ID)
  {
    int i;

    for (i=0; i<numset; i++)
      if (!strcmpi(dataset[i]->ID, ID))
        return dataset[i];
    return NULL;
  }

  int Module::Read()
  {
    return 0;
  }

  void Module::Write()
  {}

/*______________________________________________________________________________

class FRAMESFile
 private:
  int capacity;
  icsv *ifile;
  ocsv *ofile;
 public
  int kind;
  int nummod;
  Module *module;
  char filename[STRING512];
*/
  FRAMESFile::FRAMESFile(int kind)
  {
    ifile = NULL;
    ofile = NULL;
    module = NULL;
    nummod = 0;
    capacity = 0;
    this->kind = kind;
  };

  FRAMESFile::~FRAMESFile()
  {
    int i;

    if (ifile) delete ifile;
    if (ofile) delete ofile;
    if (nummod)
      for (i=0; i<nummod; i++)
        delete module[i];
    delete[] module;
  }

  void FRAMESFile::CloseInputFile()
  {
        if(ifile) delete ifile;
        ifile = NULL;
  }
  void FRAMESFile::CloseOutputFile()
  {
        if(ofile) delete ofile;
        ofile = NULL;
  }

  void FRAMESFile::Add(Module *mod)
  {
    int i;

    Module **temp;
    if (nummod < capacity)
    {
      module[nummod] = mod;
      nummod++;
    }
    else
    {
      capacity+=10;
      temp = module;
      module = new Module*[capacity];
      for (i=0; i<nummod; i++)
        module[i] = temp[i];
      module[nummod] = mod;
      delete[] temp;
      nummod++;
    }
  }

  Module *FRAMESFile::GetModule(char *ID)
  {
    int i;

    for (i=0; i<nummod; i++)
      if (strcmpi(module[i]->ID,ID) == 0)
        return module[i];
    return NULL;
  }

  int FRAMESFile::Read(char *fuiname)
  {
    int num;
    char ID[STRING16];
    Module *mod;

    sprintf(filename,"%s.%s",fuiname,EXT[kind]);
    ifile = new icsv(filename,'\"');
    if (!ifile->ok())
      return 0;

    do
    {
      *ifile >> ID >> num >> NewLn;
      if (strlen(ID) < 4) break;
      try
      {
        switch (kind)
        {
        case AFF:
          mod = (Module *)new AFFModule(ifile,ID);
          break;
        case ATO:
          mod = (Module *)new ATOModule(ifile,ID);
          break;
        case SCF:
          mod = (Module *)new SCFModule(ifile,ID);
          break;
        case GFF:
          mod = (Module *)new GFFModule(ifile,ID,num);
          break;
        case WFF:
          mod = (Module *)new WFFModule(ifile,ID,num);
          break;
        case WCF:
          mod = (Module *)new WCFModule(ifile,ID);
          break;
        case EPF:
          mod = (Module *)new EPFModule(ifile,ID);
          break;
        case RIF:
          mod = (Module *)new RIFModule(ifile,ID);
          break;
        case HIF:
          mod = (Module *)new HIFModule(ifile,ID);
          break;
        }
        Add(mod);
      }
      catch (...)
      {
        printf("What the hell happened?\n");
      }
      ifile->Skip(num);
    }
    while (!ifile->eof());
    return true;
  }

  void FRAMESFile::Write(char *fuiname)
  {}


