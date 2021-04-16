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

#ifndef typesH
#define typesH

#include "csv.h"

#define STRING4 4
#define STRING8 8
#define STRING16 16
#define STRING32 32
#define STRING64 64
#define STRING128 128
#define STRING256 256
#define STRING512 512
#define STRING1024 1024

typedef enum OLDFRAMES{SCF,AFF,ATO,WFF,WCF,EPF,RIF,HIF,GFF}filetype;

extern char unit1[STRING64];
extern char unit2[STRING64];
extern char unit3[STRING64];
extern char *EXT[9];

class Locales
{
 public:
  int readok;
  int count;                         //number of locations
  double *x;                         //x coordinates
  double *y;                         //y coordinates
  double *z;                         //elevations
  static char *xU; // = {"km"}       //kilometers
  static char *yU; // = {"km"}       //kilometers
  static char *zU; // = {"m"}        //meters
  Locales(int count);
  ~Locales();
  int Read(icsv *ifile);                 //different for some type of files
  void TestRead(ocsv *ofile);
  void Write(ocsv *ofile);
};

class FluxType
{
 public:
  int readok;
  char name[STRING32];    // Gas1, Particle1 Particle2, Particle3
  double radius;
  double density;
  double fraction;
  static char *radiusU;   // = {"um"}            //micrometers
  static char *densityU;  // = {"g/cm^3"}        //grams per cubic centimeter
  static char *massfracU; // = {"fraction"};     //fraction of the whole

  FluxType();
  int Read(icsv *ifile);
  void TestRead(ocsv *ofile);
  void Write(ocsv *ofile);
};

class Contam
{
 private:
 public:
  int readok;
  icsv *ifile;
  ocsv *ofile;
  long fpos;
  long *time;
  int contype;
  int numprog;
  int numtime;
  char cas[STRING16];
  char name[STRING64];
  Contam **prog;

  Contam();
  ~Contam();
  virtual int Read(icsv *ifile,int isparent);
  virtual void TestRead(ocsv *ofile,char *pName, char *pCas);
  virtual void Write(ocsv *ofile);
};

class DataSet
{
 private:
 public:
  int readok;
  int capacity;
  icsv *ifile;
  ocsv *ofile;
  long fpos;

  char ID[STRING16];
  int numcon;
  Contam **con;

  DataSet();
  DataSet(icsv *ifile, char *ID);
  ~DataSet();
  virtual int Read();
  virtual void TestRead(ocsv *ofile);
  virtual void Write();
};

class Module
{
 private:
 public:
  int readok;
  int capacity;
  int numlines;
  icsv *ifile;
  ocsv *ofile;
  long fpos;
  int numset;
  DataSet **dataset;
  char ID[STRING16];

  Module();
  Module(icsv *ifile,char *ID,int numline);
  Module(icsv *ifile,char *ID);
  ~Module();
  void Add(DataSet *set);
  virtual int Read();
  virtual void TestRead();
  virtual void Write();
  DataSet *GetDataSet(char *ID);
};

class WFFModule:public Module
{
 public:
  int numheaders;
  char **header;
  WFFModule(icsv *ifile,char *ID,int numline);
  ~WFFModule();
  int Read();
  void TestRead();
  void Write();
};

class GFFModule:public Module
{
 public:
  int numheaders;
  char **header;
  GFFModule(icsv *ifile,char *ID,int numline);
  ~GFFModule();
  int Read();
  void TestRead();
  void Write();
};
class WCFModule:public Module
{
 public:
  WCFModule(icsv *ifile,char *ID);
  ~WCFModule();
  int Read();
  void TestRead();
  void Write();
};

class SCFModule:public Module
{
 public:
  SCFModule(icsv *ifile,char *ID);
  ~SCFModule();
  int Read();
  void TestRead();
  void Write();
};

class AFFModule:public Module
{
 public:
  AFFModule(icsv *ifile,char *ID);
  ~AFFModule();
  int Read();
  void TestRead();
  void Write();
};

class ATOModule:public Module
{
 public:
  ATOModule(icsv *ifile,char *ID);
  ~ATOModule();
  int Read();
  void TestRead();
  void Write();
};

class EPFModule:public Module
{
 public:
  EPFModule(icsv *ifile,char *ID);
  ~EPFModule();
  int Read();
  void TestRead();
  void Write();
};

class RIFModule:public Module
{
 public:
  RIFModule(icsv *ifile,char *ID);
  ~RIFModule();
  int Read();
  void TestRead();
  void Write();
};

class HIFModule:public Module
{
 public:
  HIFModule(icsv *ifile,char *ID);
  ~HIFModule();
  int Read();
  void TestRead();
  void Write();
};

class FRAMESFile
{
 private:
  int capacity;
  icsv *ifile;
  ocsv *ofile;

 public:
  int kind;
  int nummod;
  Module **module;
  char filename[STRING512];

  FRAMESFile(int kind);
  ~FRAMESFile();
  void CloseInputFile();
  void CloseOutputFile();
  void Add(Module *mod);
  Module *GetModule(char *moduleId);

  int Read(char *fuiname);
  void Write(char *fuiname);
};

#endif
