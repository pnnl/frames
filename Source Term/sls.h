//---------------------------------------------------------------------------
#ifndef slsH
#define slsH

#include "fcsv.h"

class SLS
{
  fcsv fp;
  public:
  SLS(fcsv &err, char *run);
  ~SLS();
  void analysis();
  void contaminants();
  void all_gone(int contam, float time);
  void BlaneyCriddle();
  void writeln(char *desc, float val);
  void write(char *desc);
  void writeln(char *desc);
  void writeln();
  void write(int i);
  void write(float f);

};



//---------------------------------------------------------------------------
#endif
